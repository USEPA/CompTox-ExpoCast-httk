#' Solve_fetal_PBTK
#' 
#' This function solves for the amounts or concentrations in uM of a chemical
#' in different tissues of a maternofetal system as functions of time based on
#' the dose and dosing frequency.
#' 
#' The stage of pregnancy simulated here begins by default at the 13th week due
#' to a relative lack of data to support parameterization prior, in line with 
#' the recommendations of Kapraun et al. 2019 ("Empirical models for anatomical
#' and physiological..."), and ends at the 40th week of pregnancy.
#' 
#' Note that the model parameters have units of hours while the model output is
#' in days. Dose is in mg, not scaled for body weight.
#' 
#' Default NULL value for doses.per.day solves for a single dose.
#' 
#' The maternal compartments used in this model are the gut lumen, gut, liver, 
#' venous blood, arterial blood, lung, adipose tissue, kidney, thyroid, 
#' and rest of body. A placenta is modeled as a joint organ shared by mother
#' and fetus, through which chemical exchange can occur with the fetus. Fetal
#' compartments include arterial blood, venous blood, kidney, thyroid, liver,
#' lung, gut, brain, and rest of body. 
#' 
#' The extra compartments include the amounts or concentrations metabolized by
#' the liver and excreted by the kidneys through the tubules.
#' 
#' AUC is the area under the curve of the plasma concentration.
#' 
#' This gestational model is only parameterized for humans.
#' 
#' Because this model does not simulate exhalation, inhalation, and other 
#' processes relevant to volatile chemicals, this model is by default 
#' restricted to chemicals with a logHenry's Law Constant less than that of 
#' Acetone, a known volatile chemical. That is, chemicals with logHLC > -4.5 
#' (Log10 atm-m3/mole) are excluded. Volatility is not purely determined by the 
#' Henry's Law Constant, therefore this chemical exclusion may be turned off 
#' with the argument "physchem.exclude = FALSE". Similarly, per- and 
#' polyfluoroalkyl substances (PFAS) are excluded by default because the 
#' transporters that often drive PFAS toxicokinetics are not included in this 
#' model. However, PFAS chemicals can be included with the argument 
#' "class.exclude = FALSE".
#' 
#' @param chem.name Either the chemical name, CAS number, or the parameters
#' must be specified.
#' 
#' @param chem.cas Either the chemical name, CAS number, or the parameters must
#' be specified.
#' 
#' @param dtxsid EPA's DSSTox Structure ID (\url{https://comptox.epa.gov/dashboard})  
#' the chemical must be identified by either CAS, name, or DTXSIDs
#' 
#' @param times Optional time sequence in days. Dosing sequence begins at the
#' beginning of times. Default is from 13th week of pregnancy to 40th due to 
#' data constraints. 
#' 
#' @param parameters Chemical parameters from parameterize_fetal_pbtk function,
#' overrides chem.name and chem.cas.
#' 
#' @param days Length of the simulation.
#' 
#' @param species Included for compatibility with other functions, but the model
#' will not run for non-human species (default "Human").
#' 
#' @param tsteps The number time steps per hour. Default of 4. 
#' 
#' @param daily.dose Total daily dose, mg/kg BW.
#' 
#' @param dose Amount of a single, initial oral dose in mg/kg BW.
#' 
#' @param doses.per.day Number of doses per day.
#' 
#' @param initial.values Vector containing the initial concentrations or
#' amounts of the chemical in specified tissues with units corresponding to
#' compartment.units.  Defaults are zero.
#' 
#' @param plots Plots all outputs if true.
#' 
#' @param suppress.messages Whether or not the output message is suppressed.
#' 
#' @param iv.dose Simulates a single i.v. dose if true.
#' 
#' @param input.units Input units of interest assigned to dosing, defaults to
#' mg/kg BW
#' 
#' @param output.units A named vector of output units expected for the model
#' results. Default, NULL, returns model results in units specified in the
#' 'modelinfo' file. See table below for details.
#' 
#' @param class.exclude Exclude chemical classes identified as outside of 
#' domain of applicability by relevant modelinfo_[MODEL] file (default TRUE).
#' 
#' @param physchem.exclude Exclude chemicals on the basis of physico-chemical
#' properties (currently only Henry's law constant) as specified by 
#' the relevant modelinfo_[MODEL] file (default TRUE).
#' 
#' @param recalc.blood2plasma Recalculates the ratio of the amount of chemical
#' in the blood to plasma using the input parameters, calculated with
#' hematocrit, Funbound.plasma, and Krbc2pu.
#' 
#' @param recalc.clearance Recalculates the the hepatic clearance
#' (Clmetabolism) with new million.cells.per.gliver parameter.
#' 
#' @param dosing.matrix A matrix of either one column (or row) with a set of
#' dosing times or with two columns (or rows) correspondingly named "dose" and
#' "time" containing the time and amount, in mg/kg BW, of each dose.
#'
#' @param adjusted.Funbound.plasma Uses adjusted Funbound.plasma when set to
#' TRUE along with partition coefficients calculated with this value.
#' 
#' @param regression Whether or not to use the regressions in calculating
#' partition coefficients.
#' 
#' @param restrictive.clearance Protein binding not taken into account (set to
#' 1) in liver clearance if FALSE.
#' 
#' @param minimum.Funbound.plasma Monte Carlo draws less than this value are set 
#' equal to this value (default is 0.0001 -- half the lowest measured Fup in our
#' dataset).
#' 
#' @param monitor.vars Which variables to track by default 
#' 
#' @param rtol Relative tolerance used by integrator (deSolve) to determine 
#' numerical precision -- defaults to 1e-8.
#' 
#' @param atol Absolute tolerance used by integrator (deSolve) to determine
#' numerical precision-- defaults to 1e-8.
#' 
#' @param Caco2.options A list of options to use when working with Caco2 apical 
#' to basolateral data \code{Caco2.Pab}, default is Caco2.options = 
#' list(Caco2.Pab.default = 1.6, Caco2.Fabs = TRUE, Caco2.Fgut = TRUE, 
#' overwrite.invivo = FALSE, keepit100 = FALSE). Caco2.Pab.default sets the 
#' default value for Caco2.Pab if Caco2.Pab is unavailable. Caco2.Fabs = TRUE 
#' uses Caco2.Pab to calculate fabs.oral, otherwise fabs.oral = \code{Fabs}. 
#' Caco2.Fgut = TRUE uses Caco2.Pab to calculate 
#' fgut.oral, otherwise fgut.oral = \code{Fgut}. overwrite.invivo = TRUE 
#' overwrites Fabs and Fgut in vivo values from literature with 
#' Caco2 derived values if available. keepit100 = TRUE overwrites Fabs and Fgut 
#' with 1 (i.e. 100 percent) regardless of other settings.
#' See \code{\link{get_fbio}} for further details.
#'
#' @param ... Additional arguments passed to the integrator.
#' 
#' @return A matrix of class deSolve with a column for time(in days), each
#' compartment, the area under the curve, and plasma concentration and a row
#' for each time point.
#' 
#' @author John Wambaugh, Mark Sfeir, and Dustin Kapraun
#'
#' @references 
#' \insertRef{kapraun2022fetalmodel}{httk}
#'
#' @keywords Solve
#'
#' @seealso \code{\link{solve_model}}
#'
#' @seealso \code{\link{parameterize_fetal_pbtk}}
#'
#' @examples
#' 
#' \donttest{
#' out = solve_fetal_pbtk(chem.name = 'bisphenol a', daily.dose = 1,
#' doses.per.day = 3)
#'
#' # With adjustement to fraction unbound plasma for fetus:
#' fetal_parms_fup_adjusted <- 
#'   parameterize_fetal_pbtk(chem.name = "triclosan")
#' head(solve_fetal_pbtk(parameters = fetal_parms_fup_adjusted))
#'  
#' # Without adjustement to fraction unbound plasma for fetus:
#' fetal_parms_fup_unadjusted <-  
#'   parameterize_fetal_pbtk(chem.name = "triclosan",
#'                           fetal_fup_adjustment = FALSE)
#' head(solve_fetal_pbtk(parameters = fetal_parms_fup_unadjusted))
#' 
#' # The following will not work because Diquat dibromide monohydrate's 
#' # Henry's Law Constant (-3.912) is higher than that of Acetone (~-4.5):
#' try(head(solve_fetal_pbtk(chem.cas = "6385-62-2")))
#' # However, we can turn off checking for phys-chem properties, since we know
#' # that  Diquat dibromide monohydrate is not too volatile:
#' head(solve_fetal_pbtk(chem.cas = "6385-62-2", physchem.exclude = FALSE))
#'
#' # Try different ways to call the function:
#' head(solve_fetal_pbtk(chem.cas="80-05-7"))
#' head(solve_fetal_pbtk(parameters=parameterize_fetal_pbtk(chem.cas="80-05-7")))
#' }
#'
#' @export solve_fetal_pbtk
#'
#' @import deSolve
solve_fetal_pbtk <- function(chem.name = NULL,
                             chem.cas = NULL,
                             dtxsid = NULL,
                             times= seq(13*7,40*7,1), #from 13th week to 40th
                             parameters=NULL,
                             days=NULL,
                             species="human",
                             tsteps = 1, # tsteps is number of steps per hour
                             dose = NULL, 
                             dosing.matrix=NULL,
                             daily.dose = NULL,
                             doses.per.day=NULL,
                             initial.values=NULL,
                             plots=FALSE,
                             suppress.messages=FALSE,
                             iv.dose=FALSE,
                             input.units='mg/kg',
                             output.units=NULL,
                             physchem.exclude = TRUE,
                             class.exclude = TRUE,
                             recalc.blood2plasma=FALSE,
                             recalc.clearance=FALSE,
                             adjusted.Funbound.plasma=TRUE,
                             regression=TRUE,
                             restrictive.clearance = TRUE,
                             minimum.Funbound.plasma = 0.0001,
                             monitor.vars = NULL,
                             Caco2.options = list(),
                             atol=1e-6,
                             rtol=1e-6,
                             ...)
{
  #Screen any 'times' input
  if (times[1] < 13*7) stop('Existing data does not support simulation
with current parameterization scheme prior to the 13th week of pregnancy. It is
recommended to set \"times\" to begin at or after day 91.')
    
  if (species!="human") stop("The time-varying parameters for this model only 
describe human gestation.")
  
  out <- solve_model(
    chem.name = chem.name,
    chem.cas = chem.cas,
    dtxsid=dtxsid,
    times=times,
    parameters=parameters,
    model="fetal_pbtk",
    route=ifelse(iv.dose,"iv","oral"),
    dosing=list(
      initial.dose=dose,
      dosing.matrix=dosing.matrix,
      daily.dose=daily.dose,
      doses.per.day=doses.per.day
    ),
    days=days,
    tsteps = tsteps, # tsteps is number of steps per hour
    initial.values=initial.values,
    plots=plots,
    monitor.vars=monitor.vars,
    suppress.messages=suppress.messages,
    species='Human', #other species not (yet) supported by solve_fetal_pbtk
    input.units=input.units,
    output.units=output.units,
    recalc.blood2plasma=recalc.blood2plasma,
    recalc.clearance=recalc.clearance,
    adjusted.Funbound.plasma=adjusted.Funbound.plasma,
    minimum.Funbound.plasma=minimum.Funbound.plasma,
    parameterize.args.list =list(
                  restrictive.clearance = restrictive.clearance,
                  regression=regression,
                  Caco2.options=Caco2.options,
                  physchem.exclude = physchem.exclude,
                  class.exclude = class.exclude),
    atol=atol,
    rtol=rtol,
    ...)
  
  return(out) 
}
