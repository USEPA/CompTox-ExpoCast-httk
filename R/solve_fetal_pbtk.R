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
#' The compartments used in this model are the gutlumen, gut, liver, kidneys,
#' veins, arteries, lungs, and the rest of the body, as well as **
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
#' @param times Optional time sequence in days. Dosing sequence begins at the
#' beginning of times. Default is from 13th week of pregnancy to 40th due to 
#' data constraints. 
#' @param parameters Chemical parameters from parameterize_fetal_pbtk function,
#' overrides chem.name and chem.cas.
#' @param days Length of the simulation.
#' @param tsteps The number time steps per hour. Default of 4. 
#' @param daily.dose Total daily dose, mg.
#' @param dose Amount of a single dose, mg.  Overwrites daily.dose.
#' @param doses.per.day Number of doses per day.
#' @param initial.values Vector containing the initial concentrations or
#' amounts of the chemical in specified tissues with units corresponding to
#' output.units.  Defaults are zero.
#' @param plots Plots all outputs if true.
#' @param suppress.messages Whether or not the output message is suppressed.
#' @param iv.dose Simulates a single i.v. dose if true.
#' @param output.units Desired units (either "mg/L", "mg", "umol", or default
#' "uM").
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
#' @param begin.css Begin at steady state concentration in mother.
#' @param restrictive.clearance Protein binding not taken into account (set to
#' 1) in liver clearance if FALSE.
#' @param minimum.Funbound.plasma Monte Carlo draws less than this value are set 
#' equal to this value (default is 0.0001 -- half the lowest measured Fup in our
#' dataset).
#' @monitor.vars Which variables to track by default 
#' @param ... Additional arguments passed to the integrator.
#' @return A matrix of class deSolve with a column for time(in days), each
#' compartment, the area under the curve, and plasma concentration and a row
#' for each time point.
#' 
#' @author John Wambaugh, Mark Sfeir, and Dustin Kapraun
#' @keywords Solve
#' @examples
#' 
#' 
#' 
#' @export solve_fetal_pbtk
#' @useDynLib httk
#' @import deSolve
solve_fetal_pbtk <- function(chem.name = NULL,
                             chem.cas = NULL,
                             times= seq(13*7,40*7,1), #from 13th week to 40th
                             parameters=NULL,
                             days=10,
                             tsteps = 4, # tsteps is number of steps per hour
                             dose = 1, # Assume dose is mg, consistent with output units
                             dosing.matrix=NULL,
                             daily.dose = NULL,
                             doses.per.day=NULL,
                             initial.values=NULL,
                             plots=F,
                             suppress.messages=F,
                             iv.dose=F,
                             output.units='uM',
                             method="lsoda",rtol=1e-8,atol=1e-12, #begin.css=F,
                             default.to.human=F,
                             recalc.blood2plasma=F,
                             recalc.clearance=F,
                             adjusted.Funbound.plasma=T,
                             regression=T,
                             restrictive.clearance = T,
                             minimum.Funbound.plasma = 0.0001,
                             monitor.vars = NULL,
                             ...)
{
  #Screen any 'times' input
  if (times[1] < 13*7) stop('Existing data does not support simulation
with current parameterization scheme prior to the 13th week of pregnancy. It is
recommended to set \"times\" to begin at or after day 91.')
    
  
  out <- solve_model(
    chem.name = chem.name,
    chem.cas = chem.cas,
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
    species='Human', #optionality of species not supported by solve_fetal_pbtk
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
