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
#' @param dtxsid EPA's DSSTox Structure ID (\url{http://comptox.epa.gov/dashboard})  
#' the chemical must be identified by either CAS, name, or DTXSIDs
#' 
#' @param times Optional time sequence in days. Dosing sequence begins at the
#' beginning of times. Default is from 0th week of pregnancy to 13th due to 
#' model representation. 
#' 
#' @param parameters Chemical parameters from parameterize_1tri_pbtk function,
#' overrides chem.name and chem.cas.
#' 
#' @param days Length of the simulation.
#' 
#' @param species Included for compatibility with other functions, but the model
#' will not run for non-human species (default "Human").
#' 
#' @param tsteps The number time steps per hour. Default of 4. 
#' 
#' @param dose Amount of a single, initial oral dose in mg/kg BW. 
#' 
#' @param dosing.matrix A matrix of either one column (or row) with a set of
#' dosing times or with two columns (or rows) correspondingly named "dose" and
#' "time" containing the time and amount, in mg/kg BW, of each dose.
#' 
#' @param daily.dose Total daily dose, mg/kg BW.
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
#' @param physchem.exclude Exclude chemicals on the basis of physico-chemical
#' properties (currently only Henry's law constant) as specified by 
#' the relevant modelinfo_[MODEL] file (default TRUE).
#' 
#' @param class.exclude Exclude chemical classes identified as outside of 
#' domain of applicability by relevant modelinfo_[MODEL] file (default TRUE).
#' 
#' @param recalc.blood2plasma Recalculates the ratio of the amount of chemical
#' in the blood to plasma using the input parameters, calculated with
#' hematocrit, Funbound.plasma, and Krbc2pu.
#' 
#' @param recalc.clearance Recalculates the the hepatic clearance
#' (Clmetabolism) with new million.cells.per.gliver parameter.
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
#' @param atol Argument passed to integrator (deSolve).
#' 
#' @param rtol Argument passed to integrator (deSolve).
#' 
#' @param ... Additional arguments passed to the integrator.
#' 
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
                             times= seq(0,13*7,1), #from 0th week to end of 13th week
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
                             physchem.exclude=TRUE, 
                             class.exclude=TRUE,
                             recalc.blood2plasma=FALSE,
                             recalc.clearance=FALSE,
                             adjusted.Funbound.plasma=TRUE,
                             regression=TRUE,
                             restrictive.clearance = TRUE,
                             minimum.Funbound.plasma = 0.0001,
                             monitor.vars = NULL,
                             Caco2.options = list(),
                             atol = 1e-8,
                             rtol = 1e-8,
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
    species='Human', #other species not (yet) supported by solve_1tri_pbtk
    input.units=input.units,
    output.units=output.units,
    recalc.blood2plasma=recalc.blood2plasma,
    recalc.clearance=recalc.clearance,
    adjusted.Funbound.plasma=adjusted.Funbound.plasma,
    minimum.Funbound.plasma=minimum.Funbound.plasma,
    parameterize.args.list=list(
                  restrictive.clearance = restrictive.clearance,
                  regression = regression,
                  Caco2.options = Caco2.options,
                  physchem.exclude = physchem.exclude,
                  class.exclude = class.exclude), 
    atol=atol, 
    rtol=rtol,
    ...)
  
  return(out) 
}
