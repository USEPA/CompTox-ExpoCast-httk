#' Solve_fetal_PBTK
#' 
#' This function solves for the amounts or concentrations in uM of a chemical
#' in different tissues of a maternofetal system as functions of time based on
#' the dose and dosing frequency.
#' In this PBTK formulation. \eqn{C_{tissue}} is the concentration in tissue at 
#' time t. Since the perfusion limited partition coefficients describe 
#' instantaneous equilibrium between the tissue and the free fraction in 
#' plasma, the whole plasma concentration is 
#' \eqn{C_{tissue,plasma} = \frac{1}{f_{up}*K_{tissue2fup}}*C_{tissue}}. 
#' Note that we use a single, 
#' constant value of \eqn{f_{up}} across all tissues. Corespondingly the free 
#' plasma 
#' concentration is modeled as 
#' \eqn{C_{tissue,free plasma} = \frac{1}{K_{tissue2fup}}*C_tissue}. 
#' The amount of blood flowing from tissue x is \eqn{Q_{tissue}} (L/h) at a 
#' concentration 
#' \eqn{C_{x,blood} = \frac{R_{b2p}}{f_{up}*K_{tissue2fup}}*C_{tissue}}, where 
#' we use a 
#' single \eqn{R_{b2p}} value throughout the body.
#' Metabolic clearance is modelled as being from the total plasma 
#' concentration here, though it is restricted to the free fraction in 
#' \code{\link{calc_hep_clearance}} by default. Renal clearance via 
#' glomerulsr filtration is from the free plasma concentration.
#' The maternal compartments used in this model are the gut lumen, gut, liver, 
#' venous blood, arterial blood, lung, adipose tissue, kidney, thyroid, 
#' and rest of body. A placenta is modeled as a joint organ shared by mother
#' and fetus, through which chemical exchange can occur with the fetus. Fetal
#' compartments include arterial blood, venous blood, kidney, thyroid, liver,
#' lung, gut, brain, and rest of body. 
#' The extra compartments include the amounts or concentrations metabolized by
#' the liver and excreted by the kidneys through the tubules.
#' AUC is the area under the curve of the plasma concentration.
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
#' This gestational model is only parameterized for humans.
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
#' @param default.to.human Substitutes missing animal values with human values
#' if true (hepatic intrinsic clearance or fraction of unbound plasma).
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
#' @param ... Additional arguments passed to the integrator.
#' 
#' @return A matrix of class deSolve with a column for time(in days), each
#' compartment, the area under the curve, and plasma concentration and a row
#' for each time point.
#' 
#' @author John Wambaugh, Mark Sfeir, and Dustin Kapraun
#'
#' @keywords Solve
#'
#' @seealso \code{\link{solve_model}}
#'
#' @seealso \code{\link{parameterize_fetal_pbtk}}
#'
#' @examples
#' 
#' out = solve_fetal_pbtk(chem.name = 'bisphenol a', daily.dose = 1,
#' doses.per.day = 3)
#'
#' # With adjustement to fraction unbound plasma for fetus:
#' fetal_parms_fup_adjusted <- 
#'   parameterize_fetal_pbtk(chem.name = 'perfluorooctane sulfonic acid')
#' head(solve_fetal_pbtk(parameters = fetal_parms_fup_adjusted))
#' 
#' # Without adjustement to fraction unbound plasma for fetus:
#' fetal_parms_fup_unadjusted <-  
#'   parameterize_fetal_pbtk(chem.name = 'perfluorooctane sulfonic acid',
#'                                 fetal_fup_adjustment = FALSE)
#' head(solve_fetal_pbtk(parameters = fetal_parms_fup_unadjusted))
#' 
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
                             tsteps = 4, # tsteps is number of steps per hour
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
                             default.to.human=FALSE,
                             recalc.blood2plasma=FALSE,
                             recalc.clearance=FALSE,
                             adjusted.Funbound.plasma=TRUE,
                             regression=TRUE,
                             restrictive.clearance = TRUE,
                             minimum.Funbound.plasma = 0.0001,
                             monitor.vars = NULL,
                             atol=1e-8,
                             rtol=1e-8,
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
    default.to.human=default.to.human,
    recalc.blood2plasma=recalc.blood2plasma,
    recalc.clearance=recalc.clearance,
    adjusted.Funbound.plasma=adjusted.Funbound.plasma,
    regression=regression,
    restrictive.clearance = restrictive.clearance,
    minimum.Funbound.plasma=minimum.Funbound.plasma,
    atol=atol,
    rotl=rtol,
    ...)
  
  return(out) 
}
