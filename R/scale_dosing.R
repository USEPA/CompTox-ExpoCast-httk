#' Scale mg/kg body weight doses according to body weight and units
#' 
#' This function transforms the dose (in mg/kg) into the appropriate units. It
#' handles single doses, matrices of doses, or daily repeated doses (at varying
#' intervals. Gut absorption is also factored in through the parameter Fgutabs,
#' and scaling is currently avoided in the inhalation exposure case with a 
#' scale factor of 1
#' 
#' @author John Wambaugh and Mark Sfeir
#' @param dosing List of dosing metrics to be scaled, possibly including
#' "initial.dose", "dosing.matrix", and/or "daily.dose".
#' @param parameters List of chemical parameters, as output by 
#' parameterize_pbtk function. Overrides chem.name and chem.cas.
#' @param route String specification of route of exposure for simulation: 
#' "oral", "iv", ...
#' @param output.units String specification of desired output units, "uM",
#' "umol", "mg", or "mg/L"
#' @return Modified dosing list scaled with entries scaled according to desired
#' output units, body weight, and route of exposure. 
#' @keywords Dynamic
scale_dosing <- function(dosing,parameters,route,output.units="uM")
{
  if (!all(c("BW","MW","Fgutabs")%in%names(parameters))) 
    stop("Argument \"parameters\" must specify BW, MW, and Fgutabs.")

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
  } else stop('Only supported output.units values are uM, umol, mg, or mg/L.')

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