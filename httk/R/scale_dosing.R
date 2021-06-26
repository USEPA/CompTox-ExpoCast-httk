#' Scale mg/kg body weight doses according to body weight and units
#' 
#' This function transforms the dose (in mg/kg) into the appropriate units. It
#' handles single doses, matrices of doses, or daily repeated doses at varying
#' intervals. Gut absorption is also factored in through the parameter Fgutabs,
#' and scaling is currently avoided in the inhalation exposure case with a 
#' scale factor of 1
#' 
#' @param dosing List of dosing metrics used in simulation, which must include
#' the general entries with names "initial.dose", "doses.per.day", 
#' "daily.dose", and "dosing.matrix". The "dosing.matrix" is used for more
#' precise dose regimen specification, and is a matrix consisting of two
#' columns or rows named "time" and "dose" containing the time and amount,
#' in mg/kg BW, of each dose. The minimal usage case involves all entries but
#' "initial.dose" set to NULL in value.
#' @param parameters  Chemical parameters from parameterize_pbtk function,
#' overrides chem.name and chem.cas.
#' @param route String specification of route of exposure for simulation:
#' "oral", "iv", "inhalation", ...
#' @param output.units Desired units (either "mg/L", "mg", "umol", or default
#' "uM").
#' 
#' @return
#' A list of numeric values for doses converted to output.units, potentially
#' (depending on argument dosing) including:
#' \item{initial.dose}{The first dose given}
#' \item{dosing.matrix}{A 2xN matrix where the first column is dose time and
#' the second is dose amount for N doses}
#' \item{daily.dose}{The total cumulative daily dose}
#'
#' @author John Wambaugh
#'
#' @keywords Dynamic
scale_dosing <- function(dosing,parameters,route,output.units="uM")
{
  if (!all(c("BW","MW","Fgutabs")%in%names(parameters))) 
    stop("Argument \"parameters\" must specify, and MW, and Fgutabs.")

    BW <- as.numeric(parameters[["BW"]])
    MW <- as.numeric(parameters[["MW"]])

# If we are working in molar units then we need to convert parameters:
  if (tolower(output.units)=='um' | tolower(output.units) == 'umol')
  {
    scale.factor <- 
      BW /  # mg/kg BW -> mg
      1e3 / # mg -> g
      MW *  # g -> mol
      1e6   # mol -> umol
  } else if (tolower(output.units) == 'mg/l' | tolower(output.units) == 'mg')
  {
    scale.factor <- BW
  } else stop('Output.units can only be uM, umol, mg, or mg/L.')

# We currently model absorption processes as just diminishing overall dose:
  if (route=="oral")
  {
    scale.factor <- scale.factor*as.numeric(parameters[['Fgutabs']])
  } else if (route == "inhalation"){ #Added 9-25-19 MWL, obviously needs touching up, but for now, takes uM inputs and returns uM outputs
    scale.factor <- 1
  }
  
  if (!is.null(dosing$initial.dose)) dosing$initial.dose <- 
    as.numeric(dosing$initial.dose) * scale.factor                 
  if (!is.null(dosing$dosing.matrix)) dosing$dosing.matrix[,"dose"] <- 
    as.numeric(dosing$dosing.matrix[,"dose"]) * scale.factor
  if (!is.null(dosing$daily.dose)) dosing$daily.dose <- 
    as.numeric(dosing$daily.dose) * scale.factor 
    
  return(dosing)
}