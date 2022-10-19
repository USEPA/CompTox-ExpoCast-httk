#' Scale mg/kg body weight doses according to body weight and units
#' 
#' This function transforms the dose (in mg/kg) into the appropriate units. It
#' handles single doses, matrices of doses, or daily repeated doses at varying
#' intervals. Gut absorption is also factored in through the parameter Fabsgut,
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
#' @param input.units Units of the dose values being scaled. (Default is NULL.) 
#' Currently supported units "mg/L", "ug/L","ug/mL", "uM", "umol/L", "ug/dL",
#' "ug/g", "nmol/L", "nM", and "ppmw" (supported input.units subject to change).
#' @param output.units Desired units (either "mg/L", "mg", "umol", or default
#' "uM").
#' @param vol Volume for the target tissue of interest.
#' NOTE: Volume should not be in units of per BW, i.e. "kg".
#' 
#' @return
#' A list of numeric values for doses converted to output.units, potentially
#' (depending on argument dosing) including:
#' \item{initial.dose}{The first dose given}
#' \item{dosing.matrix}{A 2xN matrix where the first column is dose time and
#' the second is dose amount for N doses}
#' \item{daily.dose}{The total cumulative daily dose}
#'
#' @author John Wambaugh and Sarah E. Davidson
#'
#' @keywords Dynamic
scale_dosing <- function(
  dosing,
  parameters,                   
  route,
  input.units=NULL,
  output.units="uM",
  vol = NULL)# add volume conversion update for amount to conc (or vice versa)
             # then update solve_model
{
  if (!all(c("BW","MW","Fabsgut")%in%names(parameters))) 
    stop("Argument \"parameters\" must specify BW, MW, and Fabsgut.")

  BW <- as.numeric(parameters[["BW"]]) # kg
  MW <- as.numeric(parameters[["MW"]]) # mol/g

  if (is.null(input.units)) stop("Input dose units must be specified.")

  # Convert_units doesn't do bodyweight scaling so we handle that here:
  if (regexpr("/kg",input.units)!=-1) 
  {
    scale.factor <- BW
    input.units <- gsub("/kg","",input.units)
  }
  else scale.factor <- 1
 
  scale.factor <- scale.factor * 
    convert_units(
      input.units = input.units, 
      output.units = output.units, 
      MW =MW,
      vol=vol) # Should NOT be in '/kg' if applicable

# We currently model absorption processes as just diminishing overall dose:
  if (route=="oral")
  {
<<<<<<< HEAD
    scale.factor <- scale.factor*as.numeric(parameters[['Fabsgut']])
  } else if (route == "inhalation"){ #Added 9-25-19 MWL, obviously needs touching up, but for now, takes uM inputs and returns uM outputs
    scale.factor <- 1
=======
    if (!("Fgutabs"%in%names(parameters))) 
      stop(
"Argument \"parameters\" to scale_dosing must specify Fgutabs for oral route.")
    scale.factor <- scale.factor*as.numeric(parameters[['Fgutabs']])
>>>>>>> feature/otherrepos
  }
  
  if (!is.null(dosing$initial.dose)) dosing$initial.dose <- 
    as.numeric(dosing$initial.dose) * scale.factor                 
  if (!is.null(dosing$dosing.matrix)) dosing$dosing.matrix[,"dose"] <- 
    as.numeric(dosing$dosing.matrix[,"dose"]) * scale.factor
  if (!is.null(dosing$daily.dose)) dosing$daily.dose <- 
    as.numeric(dosing$daily.dose) * scale.factor
  if (!is.null(dosing$forcings)) dosing$forcings[,"forcing_values"] <- 
    as.numeric(dosing$forcings[,"forcing_values"]) * scale.factor
    
  return(dosing)
}