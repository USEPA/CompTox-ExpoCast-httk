#' Solve \code{1comp_lifestage} model, which has time-dependent parameters
#' 
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
#' chemical concentration ratio
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
#' 
#' @param start.age The age of the individual in months at the beginning of the
#' simulation. Default 360.
#'
#' @param ref.pop.dt The output of \code{\link{httkpop_generate}} containing physiology
#' of the population used in determining timeseries of parameters. Ignored if \code{ref.params}
#' is given.
#' 
#' @param httkpop.generate.arg.list If \code{ref.pop.dt} is \code{NULL}, these arguments
#' are used as input to \code{\link{httkpop_generate}} for generating physiology of 
#' a reference population. 
#' 
#' @param ref.params Model parameters of a reference population used in determining
#' timeseries. Recommended column binding ages in months (as \code{age_months}) to
#' the output of \code{\link{create_mc_samples}}.
#' 
#' @param ... Additional arguments passed to the integrator.
#'
#' @return A matrix with a column for time(in days) and a column for the
#' compartment and the area under the curve (concentration only).
#'
#' @author Colin Thomson
#'
#' @keywords Solve 1compartment, lifestage
#'
#' @examples
#' 
#' \donttest{
#' 
#' params <- parameterize_1comp(chem.name = 'Bisphenol A')
#' 
#' pop.phys <- httkpop_generate(method = 'virtual individuals',
#'                              nsamp = 25000,
#'                              agelim_years = c(18, 79),
#'                              weight_category = c("Normal"))
#' pop.params <- create_mc_samples(chem.name = 'Bisphenol A',
#'                                 model = '1compartment',
#'                                 httkpop.dt = pop.phys)
#' ref.params <- cbind(pop.params,
#'                     age_months = pop.phys$age_months)
#' out <- solve_1comp_lifestage(chem.name = 'Bisphenol A',
#'                              parameters = params,
#'                              days = 365,
#'                              start.age = 600, # age fifty
#'                              ref.params = ref.params,
#'                              doses.per.day = 3,
#'                              daily.dose = 1)
#'                               
#' }
#'
#' @export solve_1comp_lifestage
#' 
#' @useDynLib httk
solve_1comp_lifestage <- function(chem.name = NULL,
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
                    time.varying.params=TRUE,
                    start.age = 360, # age at beginning of simulation (months)
                    ref.pop.dt = NULL,
                    httkpop.generate.arg.list = list(
                      method = 'virtual individuals',
                      nsamp = 25000),
                    ref.params = NULL,
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
  
  # deSolve uses linear interpolation for timesteps in between forcing times,
  # so month-valued timeseries is a worthy tradeoff on speed in calculating 
  # timeseries values (expensive) and accuracy
  # timeseries times = seq(0,days*12/365+1) 
  timeseries.list <- list()
  if (time.varying.params) {
    if (start.age <0 | start.age >= 959) {
      warning(paste("Start age should be between zero and 959 months.",
                    "Start age set to default.",
                    sep = " ")
      )
      start.age <- 360
    }
    
    if (is.null(ref.params)) {
      if (is.null(ref.pop.dt)) {
        ref.pop.dt <- do.call(httkpop_generate, httkpop.generate.arg.list)
      }
      ref.params <- create_mc_samples(chem.cas = chem.cas,
                                      chem.name = chem.name,
                                      dtxsid = dtxsid,
                                      model = "pbtk",
                                      httkpop.dt = ref.pop.dt,
                                      samples = dim(ref.pop.dt)[1],
                                      suppress.messages = TRUE)
      
      ref.params <- cbind(ref.params,
                          age_years = ref.pop.dt$age_years, 
                          age_months = ref.pop.dt$age_months)
    }
    
    timeseries.list <- get_input_param_timeseries(model = 
                                                    "1compartment_lifestage", 
                                                  chem.name = chem.name,
                                                  chem.cas = chem.cas,
                                                  dtxsid = dtxsid,
                                                  initial.params = parameters,
                                                  start.age = start.age,
                                                  days = days,
                                                  ref.params = ref.params)
    # forcing is current - initial
  } else { # forcing is repeated zero
    for (param in model.list[["1compartment_lifestage"]]$input.var.names) {
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
    model="1compartment_lifestage",
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
    forcings = timeseries.list,
    ...)
  
  return(out) 
}
