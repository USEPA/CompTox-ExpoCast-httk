#' convert_units
#' 
#' This function is designed to accept input units, output units, and the 
#' molecular weight (MW) of a substance of interest to then use a table lookup
#' to return a scaling factor that can be readily applied for the intended
#' conversion. It can also take chemical identifiers in the place of a 
#' specified molecular weight value to retrieve that value for its own use.
#' 
#' If input or output units not contained in the table are queried,
#' it gives a corresponding error message. It gives a warning message about the
#' handling of 'ppmv,' as the function is only set up to convert between ppmv 
#' and mass-based units (like 
#' mg/\ifelse{html}{\out{m<sup>3</sup>}}{\eqn{m^3}} or umol/L) 
#' in the context of ideal gases.
#' 
#' convert_units is not directly configured to accept and convert units based
#' on BW, like mg/kg. For this purpose, see \code{\link{scale_dosing}}.
#' 
#' The function supports a limited set of most relevant units across
#' toxicological models, currently including umol, uM, mg, mg/L, 
#' mg/\ifelse{html}{\out{m<sup>3</sup>}}{\eqn{m^3}} or umol/L), and
#' in the context of gases assumed to be ideal, ppmv. 
#' 
#' \emph{Andersen and Clewell's Rules of PBPK Modeling:}
#' \itemize{
#'  \item{1}{Check Your Units}
#'  \item{2}{\strong{Check Your Units}}
#'  \item{3}{Check Mass Balance}
#' }
#'
#' @param input.units Assigned input units of interest
#'
#' @param output.units Desired output units
#'
#' @param MW Molecular weight of substance of interest in g/mole 
#'
#' @param vol Volume for the target tissue of interest in liters (L).
#' NOTE: Volume should not be in units of per BW, i.e. "kg".
#'
#' @param chem.name Either the chemical name, CAS number, or the parameters
#' must be specified.
#'
#' @param chem.cas Either the chemical name, CAS number, or the parameters must
#' be specified.
#'
#' @param dtxsid EPA's DSSTox Structure ID 
#' (\url{http://comptox.epa.gov/dashboard}) the chemical must be identified by
#' either CAS, name, or DTXSIDs
#'
#' @param parameters A set of model parameters, especially a set that
#' includes MW (molecular weight) for our conversions
#'
#' @param temp Temperature for conversions (default = 25 degreees C)
#'
#' @param state Chemical state (gas or default liquid)
#' 
#' @author Mark Sfeir, John Wambaugh, and Sarah E. Davidson
#'
#' @examples
#' 
#' # MW BPA is 228.29 g/mol
#' # 1 mg/L -> 1/228.29*1000 = 4.38 uM
#' convert_units("mg/L","uM",chem.cas="80-05-7")
#'
#' # MW Diclofenac is 296.148 g/mol
#' # 1 uM -> 296.148/1000 =  0.296
#' convert_units("uM","mg/L",chem.name="diclofenac")
#'
#' convert_units("uM","ppmv",chem.name="styrene")
#'
#' # Compare with https://www3.epa.gov/ceampubl/learn2model/part-two/onsite/ia_unit_conversion.html
#' # 1 ug/L Toluene -> 0.263 ppmv
#' convert_units("ug/L","ppmv",chem.name="toluene")
#' # 1 pppmv Toluene, 0.0038 mg/L
#' convert_units("ppmv","mg/L",chem.name="toluene")
#'
#' MW_pyrene <- get_physchem_param(param='MW', chem.name='pyrene')
#' conversion_factor <- convert_units(input.units='mg/L', output.units ='uM',
#'   MW=MW_pyrene)
#' 
#' @export convert_units
convert_units <- function(input.units = NULL, 
                          output.units = NULL, 
                          MW = NULL,
                          vol = NULL,
                          chem.cas = NULL,
                          chem.name = NULL,
                          dtxsid = NULL,
                          parameters = NULL,
                          temp = 25,
                          liquid.density = 1.0, # g/mL
                          state="liquid")
{
# The volume of an ideal gas at this temperature (L/mol)
  volidealgas <- (273.15 + temp)*0.08205

  #Take the lower case form of the units requested
  input.units <- tolower(input.units)
  output.units <- tolower(output.units)
  
  if ("unitless" %in% c(input.units,output.units)){
    if(input.units!=output.units){
      stop(
        "User specified 'unitless' for a model compartment that has units, ",
        "or alternatively specified units compartment that is 'unitless'. "
      )
    }else{
      conversion_factor <- 1
      return(set_httk_precision(conversion_factor))
    }
  }
  
  #Conditional block that checks if enough info is available to retrieve
  #MW value and use in determining conversion factor:
if (is.null(MW)) {
  if (is.null(chem.cas) & is.null(chem.name) & is.null(dtxsid) &
      is.null(parameters))
    {
    stop('User must specify either MW (molecular weight), or give chemical
identifying information like chem.cas, chem.name, or dtxsid
so that httk can retrieve a molecular weight value in determining
the unit conversion factor.')
  } else if (is.null(chem.cas) & is.null(chem.name) & is.null(dtxsid)) {
      if (is.null(parameters['MW'])) {
        stop("If describing chemical exclusively with a set of parameters,
the parameters must include an entry 'MW' (molecular weight).")
      } else {
        #Support parameters objects of the different classes currently employed
        #by httk, namely compound data.table/data.frame objects and lists.
          if (is.data.table(parameters)){
            MW <- parameters[1, MW] #should be same MW value in each row
            #of data.table of parameters from Monte Carlo functions
          } else if (is(parameters,'list')){
            MW <- parameters[['MW']]
          } else stop('httk only supports parameters objects of class
compound data.table/data.frame or list.')
        } 
  } else {
    MW <- get_physchem_param(param = 'MW', chem.cas=chem.cas,
                             chem.name=chem.name, dtxsid=dtxsid)
          }
  }
  
  #Check if input.units or output.units contain a time component indicating
  #AUC value for conversion
  if(all(grepl(c(input.units,output.units),pattern = "[*].+"))){
    #obtain vector of split amount/concentration and time scale portions
    #of unit string
    split.input.units  <- unlist(stringr::str_split(input.units,pattern = "[*]"))
    split.output.units <- unlist(stringr::str_split(output.units,pattern = "[*]"))
    
    #Check if input and output unit time scales are NOT the same
    if(split.input.units[2]!=split.output.units[2]){
      stop(
        "Input and output units are in different time scales, namely:\n    ",
        "Input units:", input.units,"\n    ",
        "Output units:", output.units,"\n    ",
        "This functionality is currently not supported."
      )
    }else{
      #Update input.units and output.units to only the amount/concentration
      #and proceed with normal conversions
      input.units  <- split.input.units[1]
      output.units <- split.output.units[1]
    }
  }else if(sum(grepl(c(input.units,output.units),pattern = "[*].+"))==1){
    #If one of the units has a time scale and the other does not,
    #then stop code and notify the user both need to have time scales or no
    #time scales
    stop(
      "If unit conversions are for area under the curve (AUC), then both the
      input and output units need to specify time scales. Input units are in ",
      input.units,
      " and output units are in ",
      output.units,".  Add or remove time scales where appropriate."
    )
  }
  
  #initialize a data.frame that determines conversion factors between key 
  #units corresponding to extrinsic quantities
  amounts_units_conversion_frame <- data.frame(mg = c(1, MW/10^3), 
                                               umol = c(10^3/MW, 1))
  row.names(amounts_units_conversion_frame) <- c('mg','umol')
  
  #initialize a data.frame that determines conversion factors between key
  #units corresponding to intrinsic quantities, set official names manually
  
  conc_units_conversion_frame <- data.frame()
  # First entry (row) is input unit, second entry (column) is output unit:
  # Where 24.45 L is the volume of an ideal gas under standardized temp/pressure
  # conditions, according to the Environmental Science and Technology Briefs for 
  # Citizens Issue 2 in 2006 from the Center for Hazardous Substances Research.
  # So an ideal gas will occupy 24.45 L/mol at 1 atm and 25 degrees C. 
  # MW has units of g/mol or ug/umol
  # So MW/24.45 has units of g/L
  # density of water is 1 g/mL = 1000 g/L = 10^6 mg/L
  # density of air is 1.225 kg/m^3 = 0.001225 kg/L = 1.225 g/L
#
# Note that inverse unit conversions are calculated in a later step, so only
# need to specify one direction (for example unit A to unit B, but not B to A):
#
# density (weight per volume) to molar (that is, moles per volume):
  conc_units_conversion_frame["mg/l","um"] <- 10^3/MW 
  conc_units_conversion_frame["ug/ml","um"] <- 10^3/MW
  conc_units_conversion_frame["mcg/ml","um"] <- 10^3/MW
  conc_units_conversion_frame["ug/l","um"] <- 1/MW 
  conc_units_conversion_frame["ug/dl","um"] <- 1/10^2*10^3/MW
  conc_units_conversion_frame["mg/l","m"] <- 1/MW/10^3 
  conc_units_conversion_frame["mg/l","mm"] <- 1/MW 
  conc_units_conversion_frame["mg/l","nm"] <- 10^6/MW 
# density to density:
  conc_units_conversion_frame["ug/ml","mg/l"] <- 1
  conc_units_conversion_frame["mcg/ml","mg/l"] <- 1
  conc_units_conversion_frame["ug/l","mg/l"] <- 1/10^3
  conc_units_conversion_frame["ug/dl","mg/l"] <- 1/10^2
  conc_units_conversion_frame["ng/l","mg/l"] <- 1/10^6
  conc_units_conversion_frame["ng/ml","mg/l"] <- 1/10^3
# molar to molar:
  conc_units_conversion_frame["umol/l","um"] <- 1
  conc_units_conversion_frame["nmol/l","um"] <- 1/10^3
  conc_units_conversion_frame["nm","um"] <- 1/10^3
  conc_units_conversion_frame["nmol/l","nm"] <- 1
# molar to density:
  conc_units_conversion_frame["umol/l","mg/l"] <- MW/10^3
  conc_units_conversion_frame["nmol/l","mg/l"] <- MW/10^6
# Weight per weight and volume per volume conversion depends on state of matter:
  if (state == "liquid")
  {
    # Liquid solvent has density liquid.density g / mL:
# density and volume per volume:
    conc_units_conversion_frame["mg/l","ppmv"] <- 1/liquid.density  
    conc_units_conversion_frame["mg/l","ppbv"] <- 10^3/liquid.density  
    conc_units_conversion_frame["mg/m3","ppmv"] <- 1/liquid.density/10^3  
    conc_units_conversion_frame["ug/l","ppmv"] <- 1/10^3/liquid.density
    conc_units_conversion_frame["ug/ml","ppmv"] <- 1/liquid.density
# molar and volume per volume:
    conc_units_conversion_frame["um","ppmv"] <- MW*10^-3/liquid.density  
    conc_units_conversion_frame["um","ppbv"] <- MW*10^0/liquid.density  
    conc_units_conversion_frame["nm","ppmv"] <- MW*10^-6/liquid.density  
    conc_units_conversion_frame["nm","ppbv"] <- MW*10^-3/liquid.density  
    conc_units_conversion_frame["m","ppmv"] <- MW*10^3/liquid.density  
    conc_units_conversion_frame["m","ppbv"] <- MW*10^6/liquid.density  
    conc_units_conversion_frame["mm","ppmv"] <- MW*10^0/liquid.density  
    conc_units_conversion_frame["mm","ppbv"] <- MW*10^3/liquid.density  
# molar and weight per weight:
    conc_units_conversion_frame["ug/g","um"] <- 10^3/MW*liquid.density
    conc_units_conversion_frame["ppmw","um"] <- 10^3/MW*liquid.density     
# density and weight per weight:
    conc_units_conversion_frame["mg/kg","mg/l"] <- liquid.density 
    conc_units_conversion_frame["ug/g","mg/l"] <- liquid.density 
    conc_units_conversion_frame["ug/g","ppmw"] <- liquid.density
  } else if (state == "gas") {
  # Gas has density volidealgas L / mol:
# density and volume per volume:
    conc_units_conversion_frame["mg/l","ppmv"] <- 10^3/MW*volidealgas  
    conc_units_conversion_frame["mg/l","ppbv"] <- 10^6/MW*volidealgas  
    conc_units_conversion_frame["ug/l","ppmv"] <- 1/MW*volidealgas
    conc_units_conversion_frame["ug/ml","ppmv"] <- 10^3/MW*volidealgas
    conc_units_conversion_frame["mcg/ml","ppmv"] <- 10^3/MW*volidealgas
    conc_units_conversion_frame["ug/dl","ppmv"] <- 1/10^2*10^3*MW*volidealgas
    conc_units_conversion_frame["mg/m3","ppmv"] <- 1/MW*volidealgas  
# molar and volume per volume:
    conc_units_conversion_frame["um","ppmv"] <- 1*volidealgas 
    conc_units_conversion_frame["umol/l","ppmv"] <- 1*volidealgas
    conc_units_conversion_frame["nmol/l","ppmv"] <- 1/10^3*volidealgas
# molar and weight per weight:
    # ug/g -> uL/L for air not water    CHECK    
# density and weight per weight:
    conc_units_conversion_frame["ug/g","ppmv"] <- 1.225/(MW/volidealgas*10^6) 
    conc_units_conversion_frame["mg/kg","ppmv"] <- 1.225/(MW/volidealgas*10^6) 
    # weight per weight and volume per volume:
    conc_units_conversion_frame["ppmw","ppmv"] <- 1.225/(MW/volidealgas*10^6) 
  } 
  
  # Get a master list of all units:
  conc_units <- sort(unique(c(rownames(conc_units_conversion_frame),
    colnames(conc_units_conversion_frame))))
  
  # Define undefined relationships as NA and make
  # symmetric entries inverses:
  for (i in conc_units)
    for (j in conc_units)
    {      
      # Identity:
      if (i==j) 
      {
        conc_units_conversion_frame[i,j] <- 1
      } else if (!is.null(conc_units_conversion_frame[i,j]))
      { 
        if (!is.na(conc_units_conversion_frame[i,j]))
        {
          conc_units_conversion_frame[j,i] <- 1/conc_units_conversion_frame[i,j]
        } else conc_units_conversion_frame[i,j] <- NA
      } else conc_units_conversion_frame[i,j] <- NA
    }
         
  # Make sure there is a row and column for each unit and that they are in the same order:
  conc_units_conversion_frame <- conc_units_conversion_frame[conc_units,   
    conc_units]

  #initialize a data.frame that determines conversion factors between key
  #amount units and concentration units, set official names manually
  #Check if volume is provided to complete the conversion table.
  if(!is.null(vol)){
    # rows = concentrations; columns = amount (input, output -- respectively)
    # if amount to concentration is needed use the inverse
    conc2amount_units_conversion_frame <- 
      conc_units_conversion_frame[,c("mg/l", "um")]*vol
    
    colnames(conc2amount_units_conversion_frame) <- c("mg","umol")
  }
  
  #initialize master list of names of chemical amounts/concentration-based
  #units supported in httk, excluding those scaled to body weight 
  httk_dose_units_list <- sort(unique(c(rownames(conc_units_conversion_frame),
    rownames(amounts_units_conversion_frame))))
  
  #Now check to see if our compiled information can appropriately support
  #the requested units conversion, and if so, provide the conversion factor.
  if (any(!c(input.units,output.units)%in%httk_dose_units_list))
  {
    stop(paste("Requested units",
      paste(unique(c(input.units,output.units))[!(
      unique(c(input.units,output.units)) %in% httk_dose_units_list)],
      collapse=", "), "
not supported for unit conversion. 
Extrinsic amounts are supported in units of \'mg\' and \'umol\', and intrinsic 
concentrations are supported in \'mg/L\', \'uM\', and, in the case of gas models 
where the gas is assumed ideal, \'ppmv\'.")) 
  }

  conversion_factor <- NA
  if(all(c(input.units,output.units)%in%names(amounts_units_conversion_frame))){
    conversion_factor <-
      amounts_units_conversion_frame[input.units, output.units]
    
  }else if(all(c(input.units,output.units)%in%names(conc_units_conversion_frame))){
    conversion_factor <-
      conc_units_conversion_frame[input.units, output.units]
    
  }else if(input.units%in%names(amounts_units_conversion_frame) &
           output.units%in% names(conc_units_conversion_frame) &
           is.null(vol)==FALSE){
    # if we need to switch between an amount and concentration;
    # volume must be provided
    # ** amount to concentration (use inverse from
    #    'conc2amount_units_conversion_frame') **
    conversion_factor <-
      1/conc2amount_units_conversion_frame[output.units, input.units]
    
  }else if(input.units%in%names(conc_units_conversion_frame) &
           output.units%in%names(amounts_units_conversion_frame) &
           is.null(vol)==FALSE){
    # if we need to switch between an amount and concentration;
    # volume must be provided
    # ** concentration to amount (use straight from
    #    'conc2amount_units_conversion_frame') **
    conversion_factor <- 
      conc2amount_units_conversion_frame[input.units,output.units]
  }
  
  if (is.na(conversion_factor)) stop(
    paste('Conversion from', input.units, 'to', output.units, 'is not
  supported for', state, '. Supported extrinsic amount units include mg and
  umol, and supported intrinsic concentration units include
  mg/L, uM, and in the case of gas models where the gas is
  assumed ideal, ppmv. If converting between amount and
  concentration, user must specify volume (vol).'))

  return(set_httk_precision(conversion_factor))
}

