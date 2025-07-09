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
#' \enumerate{
#' \item Check Your Units
#' \item \strong{Check Your Units}
#' \item Check Mass Balance
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
#' (\url{https://comptox.epa.gov/dashboard}) the chemical must be identified by
#' either CAS, name, or DTXSIDs
#'
#' @param parameters A set of model parameters, especially a set that
#' includes MW (molecular weight) for our conversions
#'
#' @param temp Temperature for conversions (default = 25 degreees C)
#' 
#' @param liquid.density Density of the specified chemical in liquid state,
#' numeric value, (default 1.0 g/mL).
#'
#' @param state Chemical state (gas or default liquid).
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
#' # ppmv only works for gasses:
#' try(convert_units("uM","ppmv",chem.name="styrene"))
#' convert_units("uM","ppmv",chem.name="styrene",state="gas")
#'
#' # Compare with https://www3.epa.gov/ceampubl/learn2model/part-two/onsite/ia_unit_conversion.html
#' # 1 ug/L Toluene -> 0.263 ppmv
#' convert_units("ug/L","ppmv",chem.name="toluene",state="gas")
#' # 1 pppmv Toluene, 0.0038 mg/L
#' convert_units("ppmv","mg/L",chem.name="toluene",state="gas")
#'
#' MW_pyrene <- get_physchem_param(param='MW', chem.name='pyrene')
#' conversion_factor <- convert_units(input.units='mg/L', output.units ='uM',
#'   MW=MW_pyrene)
#'
#' calc_mc_oral_equiv(15, parameters=p)
#' @importFrom data.table is.data.table 
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
  
  #create list of variables that require MW
  requires_MW <- c("mg/l","ug/ml","mcg/ml","ug/l","ug/dl","umol/l","nmol/l","um","nm","m",  "mm","ug/g",
    "ppmw","ng/l","ng/ml","mg/m3","mg/kg","ppbw","ppmv","ppbv")
  
  if (is.null(MW)) {
    
    if (input.units %in% requires_MW | output.units %in% requires_MW){
    
      if (is.null(chem.cas) & is.null(chem.name) & is.null(dtxsid) &
          is.null(parameters))
      {
        stop('User must specify either MW (molecular weight), or give chemical
    identifying information like chem.cas, chem.name, or dtxsid
    so that httk can retrieve a molecular weight value in determining
    the unit conversion factor.')
      } else if (is.null(chem.cas) & is.null(chem.name) & is.null(dtxsid)) {
        #Support parameters objects of the different classes currently employed
        #by httk, namely compound data.table/data.frame objects and lists.
        if (data.table::is.data.table(parameters))
        {
          if (!("MW" %in% colnames(parameters)))
          {
            stop("If describing chemical with a table of parameters,
    the parameters must include a column 'MW' (molecular weight).")
          }
          MW <- parameters[, MW]
        } else if (is(parameters,'list')) {
          if (is.null(parameters['MW'])) 
          {
            stop("If describing chemical exclusively with a set of parameters,
    the parameters must include an entry 'MW' (molecular weight).")
          }
          MW <- parameters[['MW']]
        } else stop('httk only supports parameters objects of class
    compound data.table/data.frame or list.')
      } else {
        MW <- get_physchem_param(param = 'MW', chem.cas=chem.cas,
                                 chem.name=chem.name, dtxsid=dtxsid)
      }
    }else {MW<-0}
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
  
  ### Input units
  input_units <- c(
    #volume to volume
    "ul",
    "ul",
    "ml",
    "l",
    #weight to weight
    "mg",
    "mg",
    "mg",
    # density (weight per volume) to molar (that is, moles per volume):
    "mg/l",
    "ug/ml",
    "mcg/ml",
    "ug/l",
    "ug/dl",
    "mg/l",
    "mg/l",
    "mg/l",
    # density to density:
    "ug/ml",
    "mcg/ml",
    "ug/l",
    "ug/dl",
    "ng/l",
    "ng/ml",
    # molar to molar:
    "mol",
    "umol/l",
    "nmol/l",
    "nm",
    "nmol/l",
    # molar to density:
    "umol/l",
    "nmol/l",
    #######
    #liquid state
    ##########
    # Liquid solvent has density liquid.density g / mL:
    # density and weight per weight:
    "mg/l",
    "mg/l",
    "mg/m3",
    "ug/l",
    "ug/ml",
    # molar and weight per weight:
    "um",
    "um",
    "nm",
    "nm",
    "m",  
    "m",
    "mm",
    "mm",
    # molar and weight per weight:
    "ug/g",
    "ppmw",
    # density and weight per weight:
    "mg/kg",
    "ug/g",
    "ug/g",
    #######
    # gas state
    #######
    # Gas has density volidealgas L / mol:
    # volume per volume to volume per volume:
    "ppbv",
    "pptv",
    # density and volume per volume:
    "mg/l",
    "mg/l", 
    "ug/l",
    "ng/l",
    "ug/ml",
    "ng/ml",
    "mcg/ml",
    "ug/dl",
    "mg/m3",
    # molar and volume per volume:
    "um",
    "umol/l",
    "nmol/l",
    # molar and weight per weight:
    # ug/g -> uL/L for air not water    CHECK    
    # density and weight per weight:
    "ug/g",
    "mg/kg",
    # weight per weight and volume per volume:
    "ppmw")
  
  ##### Output units
  output_units <- c(
    #volume to volume
    "l",
    "m3",
    "l",
    "m3",
    #weight to weight
    "ng",
    "kg",
    "g",
    # density (weight per volume) to molar (that is, moles per volume):
    "um",
    "um",
    "um",
    "um",
    "um",
    "m",
    "mm",
    "nm",
    # density to density:
    "mg/l",
    "mg/l",
    "mg/l",
    "mg/l",
    "mg/l",
    "mg/l",
    # molar to molar:
    "umol",
    "um",
    "um",
    "um",
    "nm",
    # molar to density:
    "mg/l",
    "mg/l",
    #######
    #liquid state
    ##########
    # Liquid solvent has density liquid.density g / mL:
    # density and weight per weight:
    "ppmw",  
    "ppbw", 
    "ppmw", 
    "ppmw",
    "ppmw",
    # molar and weight per weight:
    "ppmw",
    "ppbw",
    "ppmw",
    "ppbw",
    "ppmw",
    "ppbw",
    "ppmw",
    "ppbw",
    # molar and weight per weight:
    "um",
    "um",
    # density and weight per weight:
    "mg/l",
    "mg/l",
    "ppmw",
    #######
    # gas state
    #######
    # Gas has density volidealgas L / mol:
    # volume per volume to volume per volume:
    "ppmv",
    "ppmv",
    # density and volume per volume:
    "ppmv",
    "ppbv",
    "ppmv",
    "ppmv",
    "ppmv",
    "ppmv",
    "ppmv",
    "ppmv",
    "ppmv",
    # molar and volume per volume:
    "ppmv",
    "ppmv",
    "ppmv",
    # molar and weight per weight:
    # ug/g -> uL/L for air not water    CHECK    
    # density and weight per weight:
    "ppmv",
    "ppmv",
    # weight per weight and volume per volume:
    "ppmv")
  
  
  
  # Loop over multiple MW values:
  conversion_factors <- NA
  for (this.MW in unique(MW))
  {
    #initialize a data.frame that determines conversion factors between key 
    #units corresponding to extrinsic quantities
    amounts_units_conversion_mat <- cbind(mg = c(1, this.MW/10^3), 
                                          umol = c(10^3/this.MW, 1))
    row.names(amounts_units_conversion_mat) <- c('mg','umol')
    
    #initialize a data.frame that determines conversion factors between key
    #units corresponding to intrinsic quantities, set official names manually
    
    #pre-slug a complete lookup table of conversion units
    #rows are input units, columns are output units
    
    
    # Conversions
    #correspond to input_units and output_units
    conversions <- c(
      1/10^6,
      1/10^9,
      1/10^3,
      1/10^3,
      10^6,
      1/10^6,
      1/10^3,
      10^3/this.MW,
      10^3/this.MW,
      10^3/this.MW,
      1/this.MW,
      1/10^2*10^3/this.MW,
      1/this.MW/10^3,
      1/this.MW,
      10^6/this.MW,
      # density to density:
      1,
      1,
      1/10^3,
      1/10^2,
      1/10^6,
      1/10^3,
      # molar to molar:
      10^6,
      1,
      1/10^3,
      1/10^3,
      1,
      # molar to density:
      this.MW/10^3,
      this.MW/10^6,
      #######
      #liquid state
      ##########
      # Liquid solvent has density liquid.density g / mL:
      # density and weight per weight:
      1/liquid.density,
      10^3/liquid.density,
      1/liquid.density/10^3,
      1/10^3/liquid.density,
      1/liquid.density,
      # molar and weight per weight:
      this.MW*10^-3/liquid.density, 
      this.MW*10^0/liquid.density,
      this.MW*10^-6/liquid.density, 
      this.MW*10^-3/liquid.density,
      this.MW*10^3/liquid.density,
      this.MW*10^6/liquid.density,
      this.MW*10^0/liquid.density,
      this.MW*10^3/liquid.density, 
      # molar and weight per weight:
      10^3/this.MW*liquid.density,
      10^3/this.MW*liquid.density,  
      # density and weight per weight:
      liquid.density,
      liquid.density,
      liquid.density,
      #######
      # gas state
      #######
      # Gas has density volidealgas L / mol:
      # volume per volume to volume per volume:
      10^-3,
      10^-6,
      # density and volume per volume:
      10^3/this.MW*volidealgas,
      10^6/this.MW*volidealgas,
      10^0/this.MW*volidealgas,
      10^-3/this.MW*volidealgas,
      10^3/this.MW*volidealgas,
      10^0/this.MW*volidealgas,
      10^3/this.MW*volidealgas,
      1/10^2*10^3*this.MW*volidealgas,
      1/this.MW*volidealgas,
      # molar and volume per volume:
      1*volidealgas,
      1*volidealgas,
      1/10^3*volidealgas,
      # molar and weight per weight:
      # ug/g -> uL/L for air not water    CHECK    
      # density and weight per weight:
      1.225/(this.MW/volidealgas*10^6),
      1.225/(this.MW/volidealgas*10^6),
      # weight per weight and volume per volume:
      1.225/(this.MW/volidealgas*10^6)
    )
    
    
    
    #matrix of conversion factors:
    #rows are input units, columns are output units
    #but include all units in both row & column, so we can do inverse conversions
    
    
    all_units <- union(input_units,output_units)
    #create all-NA matrix to begin with
    conc_units_conversion_mat <- matrix(nrow = length(all_units),
                                        ncol = length(all_units))
    
    #name rows and columns 
    rownames(conc_units_conversion_mat) <- all_units
    colnames(conc_units_conversion_mat) <- all_units
    
    #diagonal 1: each unit to itself has conversion factor 1
    diag(conc_units_conversion_mat) <- 1
    
    #fill matrix with conversion factors for input units to output units
    conc_units_conversion_mat[cbind(input_units,
                                    output_units)] <- conversions
    #and the inverse -- output units to input units
    conc_units_conversion_mat[cbind(output_units,
                                    input_units)] <- 1/conversions
    #all other conversion factors remain NA
    
    #initialize a data.frame that determines conversion factors between key
    #amount units and concentration units, set official names manually
    #Check if volume is provided to complete the conversion table.
    if(!is.null(vol)){
      # rows = concentrations; columns = amount (input, output -- respectively)
      # if amount to concentration is needed use the inverse
      conc2amount_units_conversion_mat <- 
        conc_units_conversion_mat[,c("mg/l", "um")]*vol
      
      colnames(conc2amount_units_conversion_mat) <- c("mg","umol")
    }
    
    #initialize master list of names of chemical amounts/concentration-based
    #units supported in httk, excluding those scaled to body weight 
    httk_dose_units_list <- sort(unique(c(rownames(conc_units_conversion_mat),
                                          rownames(amounts_units_conversion_mat))))
    
    #Now check to see if our compiled information can appropriately support
    #the requested units conversion, and if so, provide the conversion factor.
    if (any(!c(input.units,output.units) %in% httk_dose_units_list))
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
    if(all(c(input.units,output.units) %in% rownames(amounts_units_conversion_mat))){
      conversion_factor <-
        amounts_units_conversion_mat[cbind(input.units, output.units)]
      
    }else if(all(c(input.units,output.units) %in% rownames(conc_units_conversion_mat))){
      conversion_factor <-
        conc_units_conversion_mat[cbind(input.units, output.units)]
      
    }else if(input.units %in% rownames(amounts_units_conversion_mat) &
             output.units %in% rownames(conc_units_conversion_mat) &
             is.null(vol)==FALSE){
      # if we need to switch between an amount and concentration;
      # volume must be provided
      # ** amount to concentration (use inverse from
      #    'conc2amount_units_conversion_mat') **
      conversion_factor <-
        1/conc2amount_units_conversion_mat[cbind(output.units, input.units)]
      
    }else if(input.units %in% rownames(conc_units_conversion_mat) &
             output.units %in% rownames(amounts_units_conversion_mat) &
             is.null(vol)==FALSE){
      # if we need to switch between an amount and concentration;
      # volume must be provided
      # ** concentration to amount (use straight from
      #    'conc2amount_units_conversion_mat') **
      conversion_factor <- 
        conc2amount_units_conversion_mat[cbind(input.units,output.units)]
    }
    conversion_factors[MW==this.MW] <- conversion_factor
  }
  
  if (any(is.na(conversion_factors))) stop(
    paste('Conversion from', input.units, 'to', output.units, 'is not
  supported for', state, '. Supported extrinsic amount units include mg and
  umol, and supported intrinsic concentration units include
  mg/L, uM, and in the case of gas models where the gas is
  assumed ideal, ppmv. If converting between amount and
  concentration, user must specify volume (vol).'))
  
  return(set_httk_precision(conversion_factors))
}

