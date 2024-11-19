#' Solve_1tri_PBTK
#' 
#' This function solves for the amounts (in umol) or concentrations (in uM) of a chemical
#' in different tissues of a pregnant woman (and her conceptus, i.e., products of conception)
#' as functions of time based on the dose and dosing frequency.
#' 
#' The model begins by default at non-pregnancy (0th week) and ends at the 13th 
#' week of pregnancy, thereby simulating the 1st trimester. This is meant to 
#' augment the fetal_pbtk model (Kapraun et al. 2022) which is limited to the 
#' 13th to 40th week window. 
#' 
#' Note that the model parameters have units of hours while the model output is
#' in days. Dose is in mg, not scaled for body weight.
#' 
#' Default NULL value for doses.per.day solves for a single dose.
#' 
#' The maternal compartments used in this model are the gut lumen, gut, liver, 
#' venous blood, arterial blood, lung, adipose tissue, kidney, thyroid, 
#' and rest of body. The "conceptus" compartment models an early developing fetus 
#' along with the products of conception (i.e. placenta, amniotic fluid) through 
#' which chemical exchange can occur with the maternal blood. 
#'
#' The extra compartments include the amounts or concentrations metabolized by
#' the liver and excreted by the kidneys through the tubules.
#' 
#' AUC is the area under the curve of the plasma concentration.
#' 
#' This gestational model is only parameterized for humans.
#' 
#' @param chem.name Either the chemical name, CAS number, or the parameters
#' must be specified.
#' @param chem.cas Either the chemical name, CAS number, or the parameters must
#' be specified.
#' @param dtxsid EPA's DSSTox Structure ID (\url{http://comptox.epa.gov/dashboard})  
#' the chemical must be identified by either CAS, name, or DTXSIDs
#' @param times Optional time sequence in days. Dosing sequence begins at the
#' beginning of times. Default is from 0th week of pregnancy to 13th due to 
#' model representation. 
#' @param parameters Chemical parameters from parameterize_fetal_pbtk function,
#' overrides chem.name and chem.cas.
#' @param days Length of the simulation.
#' @param species Included for compatibility with other functions, but the model
#' will not run for non-human species (default "Human").
#' @param tsteps The number time steps per hour. Default of 4. 
#' @param daily.dose Total daily dose, mg/kg BW.
#' @param dose Amount of a single, initial oral dose in mg/kg BW.  
#' @param doses.per.day Number of doses per day.
#' @param initial.values Vector containing the initial concentrations or
#' amounts of the chemical in specified tissues with units corresponding to
#' compartment.units.  Defaults are zero.
#' @param plots Plots all outputs if true.
#' @param suppress.messages Whether or not the output message is suppressed.
#' @param iv.dose Simulates a single i.v. dose if true.
#' @param input.units Input units of interest assigned to dosing, defaults to
#' mg/kg BW
#' @param output.units A named vector of output units expected for the model
#' results. Default, NULL, returns model results in units specified in the
#' 'modelinfo' file. See table below for details.
#' @param method Method used by integrator (deSolve).
#' @param rtol Argument passed to integrator (deSolve).
#' @param atol Argument passed to integrator (deSolve).
#' @param default.to.human Substitutes missing animal values with human values
#' if true (hepatic intrinsic clearance or fraction of unbound plasma).
#' @param recalc.blood2plasma Recalculates the ratio of the amount of chemical
#' in the blood to plasma using the input parameters, calculated with
#' hematocrit, Funbound.plasma, and Krbc2pu.
#' @param recalc.clearance Recalculates the the hepatic clearance
#' (Clmetabolism) with new million.cells.per.gliver parameter.
#' @param dosing.matrix A matrix of either one column (or row) with a set of
#' dosing times or with two columns (or rows) correspondingly named "dose" and
#' "time" containing the time and amount, in mg/kg BW, of each dose.
#' @param adjusted.Funbound.plasma Uses adjusted Funbound.plasma when set to
#' TRUE along with partition coefficients calculated with this value.
#' @param regression Whether or not to use the regressions in calculating
#' partition coefficients.
#' @param restrictive.clearance Protein binding not taken into account (set to
#' 1) in liver clearance if FALSE.
#' @param minimum.Funbound.plasma Monte Carlo draws less than this value are set 
#' equal to this value (default is 0.0001 -- half the lowest measured Fup in our
#' dataset).
#' @param monitor.vars Which variables to track by default 
#' @param ... Additional arguments passed to the integrator.
#' @return A matrix of class deSolve with a column for time(in days), each
#' compartment, the area under the curve, and plasma concentration and a row
#' for each time point.
#' 
#' @author Kimberly Truong, John Wambaugh, Mark Sfeir, Dustin Kapraun
#'
#' @keywords Solve
#'
#' @seealso \code{\link{solve_model}}
#'
#' @seealso \code{\link{parameterize_1tri_pbtk}}
#'
#' @examples
#' 
#' out = solve_1tri_pbtk(chem.name = 'Bisphenol-A', daily.dose = 1, 
#' doses.per.day = 3)
#' 
#' 
#' @export solve_1tri_pbtk
#'
#' @import deSolve
solve_1tri_pbtk <- function(chem.name = NULL,
                             chem.cas = NULL,
                             dtxsid = NULL,
                             times= seq(0, 13*7,1), #from 0th week to end of 13th week
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
                             method="lsoda",rtol=1e-8,atol=1e-12, #begin.css = FALSE,
                             default.to.human=FALSE,
                             recalc.blood2plasma=FALSE,
                             recalc.clearance=FALSE,
                             adjusted.Funbound.plasma=TRUE,
                             regression=TRUE,
                             restrictive.clearance = TRUE,
                             minimum.Funbound.plasma = 0.0001,
                             monitor.vars = NULL,
                             ...)
{
  #Screen any 'times' input
  if (times[length(times)] > 13*7) stop('This model is meant to simulate the 1st
trimester of pregnancy. It is recommended to set \"times\" to begin at day 0 
and end at day 91.')
    
  if (species!="human") stop("The time-varying parameters for this model only 
describe human gestation.")
  
  out <- solve_model(
    chem.name = chem.name,
    chem.cas = chem.cas,
    dtxsid=dtxsid,
    times=times,
    parameters=parameters,
    model="1tri_pbtk",
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
    method=method,rtol=rtol,atol=atol,
    default.to.human=default.to.human,
    recalc.blood2plasma=recalc.blood2plasma,
    recalc.clearance=recalc.clearance,
    adjusted.Funbound.plasma=adjusted.Funbound.plasma,
    regression=regression,
    restrictive.clearance = restrictive.clearance,
    minimum.Funbound.plasma=minimum.Funbound.plasma,
    ...)
  
  return(out) 
}
