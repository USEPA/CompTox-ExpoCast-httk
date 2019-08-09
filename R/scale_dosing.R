#' Scale mg/kg body weight doses according to body weight and units
#' 
#' This function transforms the dose (in mg/kg) into the appropriate units. It
#' handles single doses, matrices of doses, or daily repeated doses (at varying
#' intervals. Absorption is also factored in through the parameter Fgutabs
#' 
#' @author John Wambaugh
#' @param dosing
#' @param parameters
#' @param route
#' @param output.units
#' @return 
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
  }
  
  if (!is.null(dosing$initial.dose)) dosing$initial.dose <- 
    as.numeric(dosing$initial.dose) * scale.factor                 
  if (!is.null(dosing$dosing.matrix)) dosing$dosing.matrix[,"dose"] <- 
    as.numeric(dosing$dosing.matrix[,"dose"]) * scale.factor
  if (!is.null(dosing$daily.dose)) dosing$daily.dose <- 
    as.numeric(dosing$daily.dose) * scale.factor 
    
  return(dosing)
}