#' Solve one compartment TK model
#' 
#' This function solves for the amount or concentration of a chemical in plasma
#' for a one compartment model as a function of time based on the dose and
#' dosing frequency. 
#' The model describes blood concentrations in a single compartment. 
#' The volume of distribution
#' depends on the physical volume of each tissue and the predicted chemical 
#' partitioning into those volumes. 
#' Plasma concentration in compartment x is given by 
#' \eqn{C_{plasma} = \frac{C_{blood}}{R_{b2p}}} for a tissue independent value of 
#' \eqn{R_{b2p}}.
#' 
#' Model Figure 
#' \if{html}{\figure{1comp.png}{options: width="60\%" alt="Figure: One
#' Compartment Model Schematic"}}
#' \if{latex}{\figure{1comp.pdf}{options: width=12cm alt="Figure: One
#' Compartment Model Schematic"}}
#' Note that the timescales for the model parameters have units of hours while 
#' the model output is in days.
#' 
#' Default value of NULL for doses.per.day solves for a single dose.
#' 
#' When species is specified as rabbit, dog, or mouse, the function uses the
#' appropriate physiological data(volumes and flows) but substitutes human
#' fraction unbound, partition coefficients, and intrinsic hepatic clearance.
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
#' @param dtxsid EPA's 'DSSTox Structure ID (\url{https://comptox.epa.gov/dashboard})  
#' the chemical must be identified by either CAS, name, or DTXSIDs
#' 
#' @param times Optional time sequence for specified number of days.
#' 
#' @param parameters Chemical parameters from parameterize_1comp function,
#' overrides chem.name and chem.cas.
#' 
#' @param days Length of the simulation.
#' 
#' @param tsteps The number time steps per hour.
#' 
#' @param daily.dose Total daily dose, default is mg/kg BW.
#' 
#' @param dose Amount of a single dose, default is mg/kg BW. 
#' 
#' @param doses.per.day Number of doses per day.
#' 
#' @param species Species desired (either "Rat", "Rabbit", "Dog", or default
#' "Human").
#' 
#' @param iv.dose Simulates a single i.v. dose if true.
#' 
#' @param input.units Input units of interest assigned to dosing, defaults to
#' "mg/kg" BW. 
#' 
#' @param output.units A named vector of output units expected for the model
#' results. Default, NULL, returns model results in units specified in the
#' 'modelinfo' file. See table below for details.
#' 
#' @param initial.values Vector containing the initial concentrations or
#' amounts of the chemical in specified tissues with units corresponding to
#' output.units.  Defaults are zero.
#' 
#' @param suppress.messages Whether or not the output message is suppressed.
#' 
#' @param plots Plots all outputs if true.
#' 
#' @param default.to.human Substitutes missing rat values with human values if
#' true.
#' 
#' @param class.exclude Exclude chemical classes identified as outside of 
#' domain of applicability by relevant modelinfo_[MODEL] file (default TRUE).
#' 
#' @param physchem.exclude Exclude chemicals on the basis of physico-chemical
#' properties (currently only Henry's law constant) as specified by 
#' the relevant modelinfo_[MODEL] file (default TRUE).
#' 
#' @param dosing.matrix Vector of dosing times or a matrix consisting of two
#' columns or rows named "dose" and "time" containing the time and amount, in
#' mg/kg BW by default, of each dose.
#' 
#' @param recalc.clearance Whether or not to recalculate the elimination
#' rate.
#' 
#' @param recalc.blood2plasma Whether or not to recalculate the blood:plasma
#' chemical concentrationr ratio
#' 
#' @param adjusted.Funbound.plasma Uses adjusted Funbound.plasma when set to
#' TRUE along with volume of distribution calculated with this value.
#' 
#' @param regression Whether or not to use the regressions in calculating
#' partition coefficients in volume of distribution calculation.
#' 
#' @param restrictive.clearance In calculating elimination rate, protein
#' binding is not taken into account (set to 1) in liver clearance if FALSE.
#' 
#' @param minimum.Funbound.plasma Monte Carlo draws less than this value are set 
#' equal to this value (default is 0.0001 -- half the lowest measured Fup in our
#' dataset).
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
#' @param monitor.vars Which variables are returned as a function of time. 
#' Defaults value of NULL provides "Agutlumen", "Ccompartment", "Ametabolized",
#' "AUC"
#' 
#' @param ... Additional arguments passed to the integrator (deSolve).
#'
#' @return A matrix with a column for time(in days) and a column for the
#' compartment and the area under the curve (concentration only).
#'
#' @author Robert Pearce
#'
#' @references 
#' \insertRef{pearce2017httk}{httk} 
#'
#' @keywords Solve 1compartment
#'
#' @examples
#' 
#' solve_1comp(chem.name='Bisphenol-A', days=1)
#'
#' \donttest{
#' # By storing the model parameters in a vector first, you can potentially
#' # edit them before using the model:
#' params <- parameterize_1comp(chem.cas="80-05-7")
#' solve_1comp(parameters=params, days=1)
#'
#' head(solve_1comp(chem.name="Terbufos", daily.dose=NULL, dose=1, days=1))
#' head(solve_1comp(chem.name="Terbufos", daily.dose=NULL,
#'                  dose=1,days=1, iv.dose=TRUE))
#' 
#' # A dose matrix specifies times and magnitudes of doses:
#' dm <- matrix(c(0,1,2,5,5,5),nrow=3)
#' colnames(dm) <- c("time","dose")
#' solve_1comp(chem.name="Methenamine", dosing.matrix=dm,
#'             days=2.5, dose=NULL,daily.dose=NULL)
#' 
#' solve_1comp(chem.name="Besonprodil", daily.dose=1, dose=NULL,
#'             days=2.5, doses.per.day=4)
#' 
#' # The following will not work because Diquat dibromide monohydrate's 
#' # Henry's Law Constant (-3.912) is higher than that of Acetone (~-4.5):
#' try(head(solve_1comp(chem.cas = "6385-62-2")))
#' # However, we can turn off checking for phys-chem properties, since we know
#' # that  Diquat dibromide monohydrate is not too volatile:
#' head(solve_1comp(chem.cas = "6385-62-2", physchem.exclude = FALSE))
#' }
#'
#' @seealso \code{\link{solve_model}}
#'
#' @seealso \code{\link{parameterize_1comp}}
#'
#' @seealso \code{\link{calc_analytic_css_1comp}}
#'
#' @export solve_1comp
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
                    input.units='mg/kg',
                    # output.units='uM',
                    output.units=NULL,
                    default.to.human=FALSE,
                    class.exclude = TRUE,
                    physchem.exclude = TRUE,
                    recalc.blood2plasma=FALSE,
                    recalc.clearance=FALSE,
                    dosing.matrix=NULL,
                    adjusted.Funbound.plasma=TRUE,
                    regression=TRUE,
                    restrictive.clearance = TRUE,
                    minimum.Funbound.plasma=0.0001,
                    monitor.vars=NULL,
                    Caco2.options = list(),
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
    input.units=input.units,
    output.units=output.units,
    recalc.blood2plasma=recalc.blood2plasma,
    recalc.clearance=recalc.clearance,
    adjusted.Funbound.plasma=adjusted.Funbound.plasma,
    minimum.Funbound.plasma=minimum.Funbound.plasma,
    parameterize.args.list =list(
                      default.to.human=default.to.human,
                      clint.pvalue.threshold=0.05,
                      restrictive.clearance = restrictive.clearance,
                      regression=regression,
                      Caco2.options=Caco2.options,
                      class.exclude = class.exclude,
                      physchem.exclude = physchem.exclude),
    ...)
  
  return(out) 
}
