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
#' and mass-based units (like mg/m^3 or umol/L) in the context of ideal gases.
#' 
#' The function supports a limited set of most relevant units across
#' toxicological models, currently include umol, uM, mg, mg/L, mg/m^3, and, 
#' in the context of ideal gases, ppmv. 
#' 
#' @param input.units Queried input units of interest
#' @param output.units Desired output units
#' @param MW Molecular weight of substance of interest in g/mole 
#' @param chem.name Either the chemical name, CAS number, or the parameters
#' must be specified.
#' @param chem.cas Either the chemical name, CAS number, or the parameters must
#' be specified.
#' @param dtxsid EPA's DSSTox Structure ID 
#' (\url{http://comptox.epa.gov/dashboard}) the chemical must be identified by
#' either CAS, name, or DTXSIDs
#' @param parameters A set of model parameters, especially a set that
#' includes MW (molecular weight) for our conversions
#' 
#' @author Mark Sfeir and John Wambaugh
#' @examples
#' 
#' MW_pyrene <- get_physchem_param(param <- 'MW', chem.name <- 'pyrene')
#' conversion_factor <- convert_units(input.units <- 'mg/L', output.units <- 'uM',
#' MW <- MW_pyrene)
#' 
#' @export convert_units
convert_units <- function(input.units = NULL, 
                          output.units = NULL, 
                          MW = NULL,
                          chem.cas = NULL,
                          chem.name = NULL,
                          dtxsid = NULL,
                          parameters = NULL)
{
  #Take the lower case form of the units requested
  input.units <- tolower(input.units)
  output.units <- tolower(output.units)
  
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
          if (any(class(parameters)) == 'data.table'){
            MW <- parameters[1, MW] #should be same MW value in each row
            #of data.table of parameters from Monte Carlo functions
          } else if (class(parameters) == 'list'){
            MW <- parameters['MW']
          } else stop('httk only supports parameters objects of class
                      compound data.table/data.frame or list.')
        } 
  } else {
    MW <- get_physchem_param(param = 'MW', chem.cas=chem.cas,
                             chem.name=chem.name, dtxsid=dtxsid)
          }
  }
  
  #initialize a data.frame that determines conversion factors between key 
  #units corresponding to extrinsic quantities
  amounts_units_conversion_frame <- data.frame(mg = c(1, MW/10^3), 
                                               umol = c(10^3/MW, 1))
  row.names(amounts_units_conversion_frame) <- c('mg','umol')
  
  
  #initialize a data.frame that determines conversion factors between key
  #units corresponding to intrinsic quantities, set official names manually
  conc_units_conversion_frame <- 
              data.frame(mg_per_L = c(1, MW/10^3, MW/(10^3*24.45)),
                         #^^ temporarily different column name as variable
                         #name of 'mg/L' not supported for assignment
                         um = c(10^3/MW, 1, 1/24.45),
                         ppmv = c(10^3*24.45/MW, 24.45, 1))
  colnames(conc_units_conversion_frame) <- c('mg/l', 'um', 'ppmv')
  row.names(conc_units_conversion_frame) <- c('mg/l', 'um', 'ppmv')
  
  #initialize master list of names of chemical amounts/concentration-based
  #units supported in httk
  httk_units_list <- c('mg','umol','mg/l','um','ppmv')
  
#Now check to see if our compiled information can appropriately support
#the requested units conversion, and if so, provide the conversion factor.
if (!(input.units %in% httk_units_list) |
    !(output.units %in% httk_units_list)) {
  stop("Requested units not directly supported in httk. Extrinsic
        amounts are supported in units of 'mg' and 'umol', and
        intrinsic concentrations are supported in 'mg/L', 'uM', and, 
        in the case of gas models where the gas is assumed ideal,
        'ppmv'.")
} else {
  if (input.units %in% names(amounts_units_conversion_frame)){
    if (output.units %in% names(amounts_units_conversion_frame)) {
       conversion_factor <- 
         amounts_units_conversion_frame[input.units, output.units]
    } else stop('Conversion from ', input.units, ' to ', output.units, 'is not
                 supported.')
  } else if (input.units %in% names(conc_units_conversion_frame)) {
    if (output.units %in% names(conc_units_conversion_frame)) {
      conversion_factor <- 
        conc_units_conversion_frame[input.units, output.units]
    } else stop('Conversion from ', input.units, ' to ', output.units, 'is not
                 supported.'))
  }
}
  
return(conversion_factor)
}

