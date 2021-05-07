#' Solve one compartment TK model
#' 
#' This function solves for the amount or concentration of a chemical in plasma
#' for a one compartment model as a function of time based on the dose and
#' dosing frequency. 
#' 
#' Note that the model parameters have units of hours while the model output is
#' in days.
#' 
#' Default value of NULL for doses.per.day solves for a single dose.
#' 
#' When species is specified as rabbit, dog, or mouse, the function uses the
#' appropriate physiological data(volumes and flows) but substitues human
#' fraction unbound, partition coefficients, and intrinsic hepatic clearance.
#' 
#' AUC is area under plasma concentration curve.
#' 
#' Model Figure 
#' \if{html}{\figure{1comp.png}{options: width="60\%" alt="Figure: One
#' Compartment Model Schematic"}}
#' \if{latex}{\figure{1comp.pdf}{options: width=12cm alt="Figure: One
#' Compartment Model Schematic"}}
#' 
#' @param chem.name Either the chemical name, CAS number, or the parameters
#' must be specified.
#' @param chem.cas Either the chemical name, CAS number, or the parameters must
#' be specified.
#' @param dtxsid EPA's 'DSSTox Structure ID (\url{https://comptox.epa.gov/dashboard})  
#' the chemical must be identified by either CAS, name, or DTXSIDs
#' @param times Optional time sequence for specified number of days.
#' @param parameters Chemical parameters from parameterize_1comp function,
#' overrides chem.name and chem.cas.
#' @param days Length of the simulation.
#' @param tsteps The number time steps per hour.
#' @param daily.dose Total daily dose, mg/kg BW.
#' @param dose Amount of a single dose, mg/kg BW. 
#' @param doses.per.day Number of doses per day.
#' @param species Species desired (either "Rat", "Rabbit", "Dog", or default
#' "Human").
#' @param iv.dose Simulates a single i.v. dose if true.
#' @param output.units Desired units (either "mg/L", "mg", "umol", or default
#' "uM").
#' @param initial.values Vector containing the initial concentrations or
#' amounts of the chemical in specified tissues with units corresponding to
#' output.units.  Defaults are zero.
#' @param suppress.messages Whether or not the output message is suppressed.
#' @param plots Plots all outputs if true.
#' @param method Method used by integrator (deSolve).
#' @param rtol Argument passed to integrator (deSolve).
#' @param atol Argument passed to integrator (deSolve).
#' @param default.to.human Substitutes missing rat values with human values if
#' true.
#' @param dosing.matrix Vector of dosing times or a matrix consisting of two
#' columns or rows named "dose" and "time" containing the time and amount, in
#' mg/kg BW, of each dose.
#' @param recalc.clearance Whether or not to recalculate the elimination
#' rate.
#' @param recalc.blood2plasma Whether or not to recalculate the blood:plasma
#' chemical concentrationr ratio
#' @param adjusted.Funbound.plasma Uses adjusted Funbound.plasma when set to
#' TRUE along with volume of distribution calculated with this value.
#' @param regression Whether or not to use the regressions in calculating
#' partition coefficients in volume of distribution calculation.
#' @param restrictive.clearance In calculating elimination rate, protein
#' binding is not taken into account (set to 1) in liver clearance if FALSE.
#' @param minimum.Funbound.plasma Monte Carlo draws less than this value are set 
#' equal to this value (default is 0.0001 -- half the lowest measured Fup in our
#' dataset).
#' @param monitor.vars Which variables are returned as a function of time. 
#' Defaults value of NULL provides "Agutlumen", "Ccompartment", "Ametabolized",
#' "AUC"
#' @param ... Additional arguments passed to the integrator.
#'
#' @return A matrix with a column for time(in days) and a column for the
#' compartment and the area under the curve (concentration only).
#'
#' @author Robert Pearce
#'
#' @references Pearce, Robert G., et al. "Httk: R package for high-throughput
#' toxicokinetics." Journal of statistical software 79.4 (2017): 1.
#'
#' @keywords Solve 1compartment
#'
#' @examples
#' 
#' solve_1comp(chem.name='Bisphenol-A',days=1)
#' params <- parameterize_1comp(chem.cas="80-05-7")
#' solve_1comp(parameters=params)
#'
#' @export solve_1comp
#' @useDynLib httk
solve_1comp <- function(chem.name = NULL,
                    chem.cas = NULL,
                    dtxsid = NULL,
                    times=NULL,
                    parameters=NULL,
                    days=10,
                    tsteps = 4, # tsteps is number of steps per hour
                    daily.dose = NULL,
                    dose = NULL,  
                    doses.per.day=NULL,
                    initial.values=NULL,
                    plots=FALSE,
                    suppress.messages=FALSE,
                    species="Human",
                    iv.dose=FALSE,
                    output.units='uM',
                    method="lsoda",rtol=1e-8,atol=1e-12,
                    default.to.human=FALSE,
                    recalc.blood2plasma=FALSE,
                    recalc.clearance=FALSE,
                    dosing.matrix=NULL,
                    adjusted.Funbound.plasma=TRUE,
                    regression=TRUE,
                    restrictive.clearance = T,
                    minimum.Funbound.plasma=0.0001,
                    monitor.vars=NULL,
                    ...)
{
  out <- solve_model(
    chem.name = chem.name,
    chem.cas = chem.cas,
    dtxsid = dtxsid,
    times=times,
    parameters=parameters,
    model="1compartment",
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
    species=species,
    output.units=output.units,
    method=method,rtol=rtol,atol=atol,
    recalc.blood2plasma=recalc.blood2plasma,
    recalc.clearance=recalc.clearance,
    adjusted.Funbound.plasma=adjusted.Funbound.plasma,
    minimum.Funbound.plasma=minimum.Funbound.plasma,
    parameterize.arg.list=list(
                      default.to.human=default.to.human,
                      clint.pvalue.threshold=0.05,
                      restrictive.clearance = restrictive.clearance,
                      regression=regression),
    ...)
  
  return(out) 
}
