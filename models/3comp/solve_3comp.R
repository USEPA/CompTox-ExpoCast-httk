#' Solve_3comp
#' 
#' This function solves for the amounts or concentrations of a chemical in
#' the blood of three different compartments representing the body.
#' The volumes of the three compartments are chemical specific, determined from
#' the true tissue volumes multipled by the partition coefficients:
#' \deqn{V_{pv} = V_{gut}}
#' \deqn{V_{liv} = \frac{K_{liv}*f_{up}}{R_{b:p}}V_{liver}}
#' \deqn{V_{sc} = \frac{K_{sc}*f_{up}}{R_{b:p}}V_{rest}}
#' where "pv" is the portal vein, "liv" is the liver, and "sc" is the systemic
#' compartment; V_gut, V_liver, and V_rest are physiological tissue volumes;
#' K_x are chemical- and tissue-specific equlibrium partition coefficients 
#' between tissue and free chemcial concentration in plasma;
#' f_up is the chemical-specific fraction unbound in plasma; and R_b:p is the 
#' chemical specific ratio of concentrations in blood:plasma.
#' The blood concentrations evolve according to:
#' \deqn{\frac{d C_{pv}}{dt} = \frac{1}{V_{pv}}\left(k_{abs}A_{si} + Q_{pv}C_{sc}-Q_{pv}C_{pv}\right)}
#' \deqn{\frac{d C_{liv}}{dt} = \frac{1}{V_{liv}}\left(Q_{pv}C_{pv} + Q_{ha}C_{sc}-\left(Q_{pv} + Q{ha}\right)C_{liv}-\frac{1}{R_{b:p}}Cl_{h}C_{liv}\right)}
#' \deqn{\frac{d C_{sc}}{dt} = \frac{1}{V_{sc}}\left(\left(Q_{pv} + Q_{ha}\right)C_{liv} - \left(Q_{pv} + Q_{ha}\right)C_{sc} - \frac{f_{up}}{R_{b:p}}*Q_{GFR}*C_{sc}\right)}
#' where "ha" is the hepatic artery, Q's are flows, "GFR" is the glomerular
#' filtration rate in the kidney, 
# and Cl_h is the chemical-specific whole liver metabolism 
#' clearance (scaled up from intrinsic clearance, which does not depend on flow).
#' Plasma concentration in compartment x is given by 
#' \eqn{C_{x,plasma} = \frac{C_{x}}{R_{b2p}}} for a tissue independent value of 
#' \eqn{R_{b2p}}.
#' 
#' Note that the timescales for the model parameters have units of hours while 
#' the model output is in days.
#' 
#' Default of NULL for doses.per.day solves for a single dose.
#' 
#' The compartments used in this model are the gutlumen, gut, liver, and
#' rest-of-body, with the plasma related to the concentration in the blood
#' in the systemic compartment by the blood:plasma ratio.
#' 
#' Model Figure 
#' \if{html}{\figure{3comp.jpg}{options: width="60\%" alt="Figure: Three
#' Compartment Model Schematic"}} 
#' \if{latex}{\figure{3comp.pdf}{options: width=12cm alt="Figure: Three Compartment
#' Model Schematic"}}
#' 
#' When species is specified as rabbit, dog, or mouse, the function uses the
#' appropriate physiological data(volumes and flows) but substitues human
#' fraction unbound, partition coefficients, and intrinsic hepatic clearance.
#' 
#' @param chem.name Either the chemical name, CAS number, or the parameters
#' must be specified.
#' @param chem.cas Either the chemical name, CAS number, or the parameters must
#' be specified.
#' @param dtxsid EPA's 'DSSTox Structure ID (\url{https://comptox.epa.gov/dashboard})  
#' the chemical must be identified by either CAS, name, or DTXSIDs
#' @param times Optional time sequence for specified number of days.  The
#' dosing sequence begins at the beginning of times.
#' @param parameters Chemical parameters from parameterize_3comp function,
#' overrides chem.name and chem.cas.
#' @param days Length of the simulation.
#' @param tsteps The number time steps per hour.
#' @param daily.dose Total daily dose, mg/kg BW.
#' @param dose Amount of a single dose, mg/kg BW. 
#' @param doses.per.day Number of doses per day.
#' @param initial.values Vector containing the initial concentrations or
#' amounts of the chemical in specified tissues with units corresponding to
#' output.units.  Defaults are zero.
#' @param plots Plots all outputs if true.
#' @param suppress.messages Whether or not the output message is suppressed.
#' @param species Species desired (either "Rat", "Rabbit", "Dog", "Mouse", or
#' default "Human").
#' @param iv.dose Simulates a single i.v. dose if true.
#' @param input.units Input units of interest assigned to dosing,
#' defaults to mg/kg BW
#' @param output.units A named vector of output units expected for the model
#' results. Default, NULL, returns model results in units specified in the
#' 'modelinfo' file. See table below for details.
#' @param default.to.human Substitutes missing animal values with human values
#' if true (hepatic intrinsic clearance or fraction of unbound plasma).
#' @param recalc.blood2plasma Recalculates the ratio of the amount of chemical
#' in the blood to plasma using the input parameters, calculated with
#' hematocrit, Funbound.plasma, and Krbc2pu.
#' @param clint.pvalue.threshold Hepatic clearances with clearance assays
#' having p-values greater than the threshold are set to zero.
#' @param recalc.clearance Recalculates the the hepatic clearance
#' (Clmetabolism) with new million.cells.per.gliver parameter.
#' @param dosing.matrix Vector of dosing times or a matrix consisting of two
#' columns or rows named "dose" and "time" containing the time and amount, in
#' mg/kg BW, of each dose.
#' @param adjusted.Funbound.plasma Uses adjusted Funbound.plasma when set to
#' TRUE along with partition coefficients calculated with this value.
#' @param regression Whether or not to use the regressions in calculating
#' partition coefficients.
#' @param restrictive.clearance Protein binding not taken into account (set to
#' 1) in liver clearance if FALSE.
#' @param minimum.Funbound.plasma Monte Carlo draws less than this value are set 
#' equal to this value (default is 0.0001 -- half the lowest measured Fup in our
#' dataset).
#' @param Caco2.options A list of options to use when working with Caco2 apical to
#' basolateral data \code{Caco2.Pab}, default is Caco2.options = list(Caco2.Pab.default = 1.6,
#' Caco2.Fabs = TRUE, Caco2.Fgut = TRUE, overwrite.invivo = FALSE, keepit100 = FALSE). Caco2.Pab.default sets the default value for 
#' Caco2.Pab if Caco2.Pab is unavailable. Caco2.Fabs = TRUE uses Caco2.Pab to calculate
#' fabs.oral, otherwise fabs.oral = \code{Fabs}. Caco2.Fgut = TRUE uses Caco2.Pab to calculate 
#' fgut.oral, otherwise fgut.oral = \code{Fgut}. overwrite.invivo = TRUE overwrites Fabs and Fgut in vivo values from literature with 
#' Caco2 derived values if available. keepit100 = TRUE overwrites Fabs and Fgut with 1 (i.e. 100 percent) regardless of other settings.
#' See \code{\link{get_fbio}} for further details.
#' 
#' @param monitor.vars Which variables are returned as a function of time. 
#' Defaults value of NULL provides "Cliver", "Csyscomp", "Atubules", 
#' "Ametabolized", "AUC"
#' @param ... Additional arguments passed to the integrator (deSolve).
#'
#' @return A matrix of class deSolve with a column for time(in days) and each
#' compartment, the plasma concentration, area under the curve, and a row for
#' each time point.
#'
#' @author John Wambaugh and Robert Pearce
#'
#' @references 
#' \insertRef{pearce2017httk}{httk}
#'
#' @keywords Solve 3compartment
#'
#' @examples
#' 
#' solve_3comp(chem.name='Bisphenol-A', 
#'             doses.per.day=2, 
#'             daily.dose=.5,
#'             days=1,
#'             tsteps=2)
#'
#' # By storing the model parameters in a vector first, you can potentially
#' # edit them before using the model:
#' params <-parameterize_3comp(chem.cas="80-05-7")
#' solve_3comp(parameters=params, days=1)
#' 
#' head(solve_3comp(chem.name="Terbufos", daily.dose=NULL, dose=1, days=1))
#' head(solve_3comp(chem.name="Terbufos", daily.dose=NULL, dose=1, 
#'                  days=1, iv.dose=TRUE))
#' 
#' # A dose matrix specifies times and magnitudes of doses:
#' dm <- matrix(c(0,1,2,5,5,5),nrow=3)
#' colnames(dm) <- c("time","dose")
#' solve_3comp(chem.name="Methenamine", dosing.matrix=dm,
#'             dose=NULL, daily.dose=NULL,
#'             days=2.5)
#' 
#' solve_3comp(chem.name="Besonprodil",
#'             daily.dose=1, dose=NULL,
#'             days=2.5, doses.per.day=4)
#'
#' @seealso \code{\link{solve_model}}
#'
#' @seealso \code{\link{parameterize_3comp}}
#'
#' @seealso \code{\link{calc_analytic_css_3comp}}
#'
#' @export solve_3comp
#'
#' @useDynLib httk
solve_3comp <- function(chem.name = NULL,
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
                    input.units='mg/kg',
                    # output.units='uM',
                    output.units=NULL,
                    default.to.human=FALSE,
                    recalc.blood2plasma=FALSE,
                    recalc.clearance=FALSE,
                    clint.pvalue.threshold=0.05,
                    dosing.matrix=NULL,
                    adjusted.Funbound.plasma=TRUE,
                    regression=TRUE,
                    restrictive.clearance = TRUE,
                    minimum.Funbound.plasma=0.0001,
                    Caco2.options = list(),
                    monitor.vars=NULL,
                    ...)
{
  out <- solve_model(
    chem.name = chem.name,
    chem.cas = chem.cas,
    dtxsid = dtxsid,
    times=times,
    parameters=parameters,
    model="3compartment",
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
    input.units=input.units,
    output.units=output.units,
    recalc.blood2plasma=recalc.blood2plasma,
    recalc.clearance=recalc.clearance,
    adjusted.Funbound.plasma=adjusted.Funbound.plasma,
    minimum.Funbound.plasma=minimum.Funbound.plasma,
    parameterize.arg.list=list(
                      default.to.human=default.to.human,
                      clint.pvalue.threshold=clint.pvalue.threshold,
                      restrictive.clearance = restrictive.clearance,
                      regression=regression,
                      Caco2.options=Caco2.options),
    ...)
  
  return(out) 
}
