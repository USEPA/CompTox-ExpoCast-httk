scale_dosing <- function(dosing,parameters,output.units="uM")
{
  if (!all(c("BW","MW")%in%names(parameters))) 
    stop("Argument \"parameters\" must specify BW and MW.")

# If we are working in molar units then we need to convert parameters:
  if (tolower(output.units)=='um' | tolower(output.units) == 'umol')
  {
    if (!is.null(dosing$initial.dose)) dosing$initial.dose <- 
      as.numeric(dosing$initial.dose * 
      parameters[["BW"]] / # mg/kg BW -> mg
      1e3 /                # mg -> g
      parameters[["MW"]] * # g -> mol
      1e6)                 # mol -> umol
    if (!is.null(dosing$dosing.matrix)) dosing$dosing.matrix[,"dose"] <- 
      as.numeric(dosing$dosing.matrix[,"dose"] * 
      parameters[["BW"]] / # mg/kg BW -> mg
      1e3/                 # mg -> g
      parameters[["MW"]] * # g -> mol
      1e6)                 # mol -> umol
    if (!is.null(dosing$daily.dose)) dosing$daily.dose <- 
      as.numeric(dosing$daily.dose * 
      parameters[["BW"]] / # mg/kg BW -> mg
      1e3/                 # mg -> g
      parameters[["MW"]] * # g -> mol
      1e6)                 # mol -> umol
    
  } else if (tolower(output.units) == 'mg/l' | tolower(output.units) == 'mg')
  {
    if (!is.null(dosing$initial.dose)) dosing$initial.dose <- 
      dosing$initial.dose * parameters[['BW']]
    if (!is.null(dosing$dosing.matrix)) dosing$dosing.matrix[,"dose"] <- 
      dosing$dosing.matrix[,"dose"] * parameters[['BW']]
    if (!is.null(dosing$daily.dose)) dosing$daily.dose <- 
      dosing$daily.dose * parameters[['BW']]
  } else stop('Output.units can only be uM, umol, mg, or mg/L.')
  
  return(dosing)
}