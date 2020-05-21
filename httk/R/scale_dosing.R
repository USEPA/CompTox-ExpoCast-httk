#' Scale mg/kg body weight doses according to body weight and units
#' 
#' This function scales doses (in mg/kg) by body weight. It handles single 
#' doses, matrices of doses, or daily repeated doses at varying intervals. 
#' Gut absorption is also factored in through the parameter Fgutabs,
#' and scaling is currently avoided in the inhalation exposure case with a 
#' scale factor of 1
#' 
#' @param dosing List of dosing metrics used in simulation, which includes
#' the namesake entries of a model's associated dosing.params. In the case
#' of most httk models, these should include "initial.dose", "doses.per.day", 
#' "daily.dose", and "dosing.matrix". The "dosing.matrix" is used for more
#' precise dose regimen specification, and is a matrix consisting of two
#' columns or rows named "time" and "dose" containing the time and amount,
#' in mg/kg BW, of each dose. 
#' @param parameters  Chemical parameters, either from parameterization 
#' function, or otherwise specified. Must contain a BW entry.
#' @param route String specification of route of exposure for simulation:
#' "oral", "iv", "inhalation", ...
#' 
#' @author John Wambaugh, Matt Linakis, and Mark Sfeir
#'
#' @keywords Dynamic
scale_dosing <- function(dosing,parameters,route)
{
  if (!all(c("BW","Fgutabs")%in%names(parameters))) 
    stop("Argument \"parameters\" must specify, and MW, and Fgutabs.")

    BW <- as.numeric(parameters[["BW"]])

# We currently model absorption processes as just diminishing overall dose:
  if (route=="oral")
  {
    scale.factor <- BW*as.numeric(parameters[['Fgutabs']])
  } else if (route == "iv")
    {
    scale.factor <- BW
  } else if (route == "inhalation")
    { #Added 9-25-19 MWL, obviously needs touching up, but for now,
      #takes uM inputs and returns uM outputs
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