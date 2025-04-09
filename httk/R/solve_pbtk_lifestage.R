#' Solve the \code{pbtk_lifestage} model, which has time-dependent parameters
#' 
#' This function solves for the amounts or concentrations in uM of a chemical
#' in different tissues as functions of time based on the dose and dosing
#' frequency. 
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
#' The compartments used in this model are the gutlumen, gut, liver, kidneys,
#' veins, arteries, lungs, and the rest of the body.
#' The extra compartments include the amounts or concentrations metabolized by
#' the liver and excreted by the kidneys through the tubules.
#' AUC is the area under the curve of the plasma concentration.
#' 
#' Note that the model parameters have units of hours while the model output is
#' in days.
#' 
#' Default NULL value for doses.per.day solves for a single dose.
#' 
#' Model Figure 
#' \if{html}{\figure{pbtk.jpg}{options: width="60\%" alt="Figure: PBTK Model
#' Schematic"}}
#' \if{latex}{\figure{pbtk.pdf}{options: width=12cm alt="Figure: PBTK Model
#' Schematic"}}
#' 
#' When species is specified as rabbit, dog, or mouse, the function uses the
#' appropriate physiological data(volumes and flows) but substitutes human
#' fraction unbound, partition coefficients, and intrinsic hepatic clearance.
#' 
#' 
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
#' @param times Optional time sequence for specified number of days.  Dosing
#' sequence begins at the beginning of times.
#' 
#' @param parameters Chemical parameters from parameterize_pbtk function,
#' overrides chem.name and chem.cas.
#' 
#' @param days Length of the simulation.
#' 
#' @param tsteps The number of time steps per hour.
#' 
#' @param daily.dose Total daily dose, defaults to mg/kg BW.
#' 
#' @param dose Amount of a single, initial oral dose in mg/kg BW. 
#' 
#' @param doses.per.day Number of doses per day.
#' 
#' @param initial.values Vector containing the initial concentrations or
#' amounts of the chemical in specified tissues with units corresponding to
#' output.units.  Defaults are zero.
#' 
#' @param plots Plots all outputs if true.
#' 
#' @param suppress.messages Whether or not the output message is suppressed.
#' 
#' @param species Species desired (either "Rat", "Rabbit", "Dog", "Mouse", or
#' default "Human").
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
#' @param dosing.matrix Vector of dosing times or a matrix consisting of two
#' columns or rows named "dose" and "time" containing the time and amount, in
#' mg/kg BW, of each dose.
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
#' The default value of NULL provides "Cgut", "Cliver", "Cven", "Clung", "Cart", 
#' "Crest", "Ckidney", "Cplasma", "Atubules", "Ametabolized", and "AUC"
#' 
#' @param time.varying.params Whether or not to allow parameters to vary in time 
#' according to the nonparametric regression determined by \code{\link{get_input_param_timeseries}}. 
#' Default is TRUE.
#' 
#' @param start.age The age of the individual in months at the beginning of the
#' simulation. Default 360.
#'
#'The next four parameters play the same role here as in \code{\link{httkpop_generate}}:
#' the user may restrict the data available to generate parameter evolution by
#' specifying demographics. 
#' 
#' @param gender Optional: The gender categories to include in the popualtion; 
#'  default \code{c("Female", "Male")}.
#' @param weight_category Optional: The weight categories to include in the
#'   population. Default is \code{c('Underweight', 'Normal', 'Overweight',
#'   'Obese')}. User-supplied vector must contain one or more of these strings.
#' @param gfr_category The kidney function categories to include in the
#'   population. Default is \code{c('Normal','Kidney Disease', 'Kidney
#'   Failure')} to include all kidney function levels.
#' @param reths Optional: a character vector giving the races/ethnicities to
#'   include in the population. Default is \code{c('Mexican American','Other
#'   Hispanic','Non-Hispanic White','Non-Hispanic Black','Other')}, to include
#'   all races and ethnicities in their proportions in the NHANES data.
#'   User-supplied vector must contain one or more of these strings.
#'
#' @param input.param.dir The path to the \code{input_params_data_files} directory,
#' which is used to store all \code{input_param} data files. If \code{input_params_data_files}
#' does not exist, this function will create it in the specified path. Default \code{NULL}, 
#' in which case the present working directory is used as default.
#'
#' @param ... Additional arguments passed to the integrator (deSolve).
#'
#' @return A matrix of class deSolve with a column for time(in days), each
#' compartment, the area under the curve, and plasma concentration and a row
#' for each time point.
#'
#' @author Colin Thomson
#'
#' @seealso \code{\link{solve_model}}
#'
#' @seealso \code{\link{parameterize_pbtk}}
#'
#' @seealso \code{\link{get_input_param_timeseries}}
#'
#' @keywords Solve pbtk, lifestage
#'
#' @examples
#' \donttest{
#' 
#' params <- parameterize_pbtk(chem.name = 'Bisphenol A')
#' out <- solve_pbtk_lifestage(chem.name = 'Bisphenol A',
#'                             parameters = params,
#'                             days = 365,
#'                             start.age = 600, # age fifty
#'                             weight_category=c('Underweight',
#'                                               'Normal',
#'                                               'Overweight')
#'                             doses.per.day = 3,
#'                             daily.dose = 1)
#' 
#' }
#' 
#' @export solve_pbtk_lifestage
#'
#' @useDynLib httk
solve_pbtk_lifestage <- function(chem.name = NULL,
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
                    class.exclude=TRUE,
                    recalc.blood2plasma=FALSE,
                    recalc.clearance=FALSE,
                    dosing.matrix=NULL,
                    adjusted.Funbound.plasma=TRUE,
                    regression=TRUE,
                    restrictive.clearance = TRUE,
                    minimum.Funbound.plasma=0.0001,
                    Caco2.options = list(),
                    monitor.vars=NULL,
                    time.varying.params=TRUE,
                    start.age = 360,
                    gender=c('Male',
                             'Female'),
                    weight_category=c('Underweight',
                                      'Normal',
                                      'Overweight',
                                      'Obese'), 
                    gfr_category=c('Normal',
                                   'Kidney Disease', 
                                   'Kidney Failure'),
                    reths=c('Mexican American',
                            'Other Hispanic',
                            'Non-Hispanic White',
                            'Non-Hispanic Black',
                            'Other'),
                    input.param.dir = NULL,
                    ...)
{
  
  if (time.varying.params) {
    if (species != "Human") {
      warning(paste("Time-varying parameters only available for human subjects:",
                    "time.varying.params set to FALSE.",
                    sep = "\n")
      )
      time.varying.parms <- FALSE
    }
  }
  
  timeseries.list <- list()
  if (time.varying.params) {
    if (start.age < 0 | start.age >= 959) {
      warning(paste("Start age should be between zero and 959 months.",
                    "Start age set to default.",
                    sep = " ")
      )
      start.age <- 360
    }
    
    timeseries.list <- get_input_param_timeseries(model = "pbtk_lifestage",
                                                  chem.name = chem.name,
                                                  chem.cas = chem.cas,
                                                  dtxsid = dtxsid,
                                                  initial.params = parameters,
                                                  start.age = start.age,
                                                  days = days,
                                                  gender = gender,
                                                  weight_category =
                                                    weight_category,
                                                  gfr_category = gfr_category,
                                                  reths=reths,
                                                  input.param.dir = 
                                                    input.param.dir)

  } else { # forcing is repeated zero
    for (param in model.list[["pbtk_lifestage"]]$input.var.names) {
      timeseries.list[[param]] <- rbind(c(0,0), c(days, 0))
    }
  }
  
  # scale_dosing in solve_model.R, line 785, multiplies doses by BW-- which is
  #   the initial value for BW. This dosing matrix ensures doses are based on
  #   dynamic body weight, and overrides other given dosing parameters. 
  # initial.dose does not need to be altered.
  
  if (!is.null(doses.per.day) && !is.null(daily.dose)) {
    dosing.times <- seq(0, days, 1/doses.per.day)
    dosing.matrix <- matrix(data = c(dosing.times,
                                     rep(daily.dose/doses.per.day, 
                                         length(dosing.times))),
                            ncol = 2)
    colnames(dosing.matrix) <- c("time", "dose")
  }
  if (!is.null(dosing.matrix)) {
    dosing.matrix[,"dose"] <- dosing.matrix[,"dose"]/parameters[["BW"]] * (
      parameters[["BW"]] +  approx(x = timeseries.list$d_BW[,1],
                                   y = timeseries.list$d_BW[,2],
                                   xout = dosing.matrix[,"time"])$y )
  }
  
  out <- solve_model(
    chem.name = chem.name,
    chem.cas = chem.cas,
    dtxsid = dtxsid,
    times=times,
    parameters=parameters,
    model="pbtk_lifestage",
    route= ifelse(iv.dose,"iv","oral"),
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
                      clint.pvalue.threshold=0.05,
                      restrictive.clearance = restrictive.clearance,
                      regression=regression,
                      Caco2.options=Caco2.options,
                      class.exclude=class.exclude),
    forcings=timeseries.list,
    ...)
  
  return(out) 
}
