#' solve_gas_pbtk
#' 
#' This function solves for the amounts or concentrations of a chemical
#' in different tissues as functions of time as a result of inhalation 
#' exposure. 
#' 
#' The default dosing scheme involves specifying the start time
#' of exposure, the concentration of gas inhaled, the period of a given 
#' assumed cycle of exposure, and the duration of the exposure during that 
#' period. Together, these arguments determine the forcings passed to the 
#' ODE integrator. The "forcings" can also be specified manually, or 
#' effectively turned off by setting exposure concentration to zero, if the
#' user prefers to simulate dosing by other means. 
#' 
#' 
#' This function solves for the amounts or concentrations in uM of a chemical
#' in different tissues as functions of time based on the dose and dosing
#' frequency. 
#' 
#' Note that the model parameters have units of hours while the model output is
#' in days.
#' 
#' Default NULL value for doses.per.day solves for a single dose.
#' 
#' The compartments used in this model are the gut lumen, gut, liver, kidneys,
#' veins, arteries, lungs, and the rest of the body.
#' 
#' The extra compartments include the amounts or concentrations metabolized by
#' the liver and excreted by the kidneys through the tubules.
#' 
#' AUC is the area under the curve of the plasma concentration.
#' 
#' Model parameters are named according to the following convention:\tabular{lrrrr}{
#' prefix \tab suffic \tab Meaning \tab units \cr
#' K \tab \tab Partition coefficient for tissue to free plasma \ tab unitless \cr
#' V \tab \tab Volume \tab L \cr
#' Q \tab \tab Flow \tab L/h \cr
#' k \tab \tab Rate \tab 1/h \cr
#' \tab c \tab Parameter is proportional to body weight \tab 1 / kg for volumes
#' and 1/kg^(3/4) for flows \cr}
#'
#' When species is specified but chemical-specific in vitro data are not
#' available, the function uses the appropriate physiological data (volumes and 
#' flows) but default.to.human = TRUE must be used to substitute human
#' fraction unbound, partition coefficients, and intrinsic hepatic clearance.
#'  
#' 
#' @param chem.name Either the chemical name, CAS number, or the parameters
#' must be specified.
#' @param chem.cas Either the chemical name, CAS number, or the parameters must
#' be specified.
#' @param dtxsid EPA's DSSTox Structure ID (\url{https://comptox.epa.gov/dashboard})  
#' the chemical must be identified by either CAS, name, or DTXSIDs
#' @param parameters Chemical parameters from parameterize_gas_pbtk (or other
#' bespoke) function, overrides chem.name and chem.cas.
#' @param times Optional time sequence for specified number of days.  Dosing
#' sequence begins at the beginning of times.
#' @param days Length of the simulation.
#' @param tsteps The number of time steps per hour.
#' @param daily.dose Total daily dose, mg/kg BW.
#' @param doses.per.day Number of doses per day.
#' @param dose Amount of a single dose, mg/kg BW. 
#' @param dosing.matrix Vector of dosing times or a matrix consisting of two
#' columns or rows named "dose" and "time" containing the time and amount, in
#' mg/kg BW, of each dose. With the gas pbtk model, dosing.matrix is set to 
#' specify forcing concentrations to the integrator, either in combination 
#' with eventdata or on its own. 
#' @param forcings Manual input of "forcings" data series argument for ode
#' integrator, defaults to NULL
#' @param exp.start.time Start time in specifying forcing exposure series,
#' default 0. 
#' @param exp.conc Specified inhalation exposure concentration for use in 
#' assembling "forcings" data series argument for integrator. Defaults to
#' uM, in line with output.units
#' @param period For use in assembling forcing function data series "forcings"
#' argument, specified in hours
#' @param exp.duration For use in assembling forcing function data 
#' series 'forcings' argument, specified in hours
#' @param fcontrol List of arguments for finetuning inhalation forcing function
#' in conjunction with existing ode integrator methods
#' @param initial.values Vector containing the initial concentrations or
#' amounts of the chemical in specified tissues with units corresponding to
#' output.units.  Defaults are zero.
#' @param plots Plots all outputs if true.
#' @param suppress.messages Whether or not the output message is suppressed.
#' @param species Species desired (either "Rat", "Rabbit", "Dog", "Mouse", or
#' default "Human").
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
#' @param recalc.clearance Recalculates the hepatic clearance
#' (Clmetabolism) with new million.cells.per.gliver parameter.
#' @param adjusted.Funbound.plasma Uses adjusted Funbound.plasma when set to
#' TRUE along with partition coefficients calculated with this value.
#' @param regression Whether or not to use the regressions in calculating
#' partition coefficients.
#' @param restrictive.clearance Protein binding not taken into account (set to
#' 1) in liver clearance if FALSE.
#' @param minimum.Funbound.plasma Monte Carlo draws less than this value are set 
#' equal to this value (default is 0.0001 -- half the lowest measured Fup in our
#' dataset).
#' @param monitor.vars Which variables are returned as a function of time. 
#' Defaults value of NULL provides "Cgut", "Cliver", "Cven", "Clung", "Cart",
#' "Crest", "Ckidney", "Cplasma", "Calv", "Cendexh", "Cmixexh", "Cmuc", 
#' "Atubules", "Ametabolized", "AUC"
#' @param vmax Michaelis-Menten vmax value in reactions/min
#' @param km Michaelis-Menten concentration of half-maximal reaction velocity
#' in desired output concentration units. 
#' @param exercise Logical indicator of whether to simulate an exercise-induced
#' heightened respiration rate
#' @param fR Respiratory frequency (breaths/minute), used especially to adjust
#' breathing rate in the case of exercise. This parameter, along with VT and VD
#' (below) gives another option for calculating Qalv (Alveolar ventilation) 
#' in case pulmonary ventilation rate is not known 
#' @param VT Tidal volume (L), to be modulated especially as part of simulating
#' the state of exercise
#' @param VD Anatomical dead space (L), to be modulated especially as part of
#' simulating the state of exercise
#' @param ... Additional arguments passed to the integrator.
#'
#' @return A matrix of class deSolve with a column for time(in days), each
#' compartment, the area under the curve, and plasma concentration and a row
#' for each time point.
#'
#' @author Matt Linakis, John Wambaugh, and Mark Sfeir
#'
#' @references 
#' Linakis, Matthew W., et al. "Development and Evaluation of a High Throughput 
#' Inhalation Model for Organic Chemicals", submitted
#' 
#' Pearce, Robert G., et al. "Httk: R package for high-throughput
#' toxicokinetics." Journal of statistical software 79.4 (2017): 1.
#'
#' @keywords Solve
#'
#' @examples
#' 
#' solve_gas_pbtk(chem.name='Pyrene',dose=.5,days = 3,tsteps=2)
#' 
#' \donttest{
#' out <- solve_gas_pbtk(chem.name='pyrene',exp.conc = 0, doses.per.day = 2,
#' daily.dose = 3, plots=TRUE,initial.values=c(Aven=20))
#' 
#' out <- solve_gas_pbtk(chem.name = 'pyrene',exp.conc = 3, period = 24,
#' exp.duration = 6, exercise = TRUE)
#'                   
#' params <- parameterize_gas_pbtk(chem.cas="80-05-7")
#' solve_gas_pbtk(parameters=params)
#' }
#' 
#' @export solve_gas_pbtk
#' @useDynLib httk
#' @import deSolve
solve_gas_pbtk <- function(chem.name = NULL,
                           chem.cas = NULL,
                           dtxsid = NULL,
                           parameters=NULL,
                           times=NULL,
                           days=10,
                           tsteps = 4, #tsteps is number of steps per hour
                           daily.dose = NULL,
                           doses.per.day = NULL,
                           dose = NULL, #Assume single dose is in mg/kg BW/day
                           dosing.matrix = NULL,
                           forcings = NULL,
                           exp.start.time = 0, #default starting time in specifying forcing exposure
                           exp.conc = 1, #default exposure concentration for forcing data series
                           period = 24, 
                           exp.duration = 12,
                           fcontrol = list(method='constant',rule=2,f=0), 
                           initial.values=NULL,
                           plots=FALSE,
                           suppress.messages=FALSE,
                           species="Human",
                           output.units='uM',
                           method="lsoda",rtol=1e-8,atol=1e-12,
                           default.to.human=FALSE,
                           recalc.blood2plasma=FALSE,
                           recalc.clearance=FALSE,
                           adjusted.Funbound.plasma=TRUE,
                           regression=TRUE,
                           restrictive.clearance = T,
                           minimum.Funbound.plasma=0.0001,
                           monitor.vars=NULL,
                           vmax = 0,
                           km = 1,
                           exercise = F,
                           fR = 12,
                           VT = 0.75,
                           VD = 0.15,
                           ...)
{
  
  #Screen against error in user's specification of forcing function timing
  if (exp.duration > period){
  stop("Argument 'exp.duration' should be smaller than its subsuming argument,
       'period', which together are set to specify a simple cyclic pattern of 
       inhalation exposure and rest in the default case.")
  }
  
  #Screen against case in which forcing function is specified, but output.units
  #are specified as other than 'uM'. Units of forcing function exposure 
  #concentration are only supported as 'uM' for now.
  #if ((!is.null(dosing.matrix) | exp.conc > 0) & tolower(output.units) != 'um') {
    #stop('Forcings exposure data series not yet supported 
      #   in units other than uM.')
  #}
  
  #Look up the chemical name/CAS to get some info about the chemical in
  #question and screen it for relevance of its logHenry value. Should not
  #be necessary if user manually specifies 'parameters'
  if (is.null(parameters)){
  out <- get_chem_id(
    chem.cas=chem.cas,
    chem.name=chem.name,
    dtxsid=dtxsid)
  chem.cas <- out$chem.cas
  chem.name <- out$chem.name                                
  dtxsid <- out$dtxsid
  
  
  #If value of Henry's law constant associated with queried chemical is smaller
  #than that of glycerol, generally considered non-volatile, issue warning
  #message:
 
    #get associated logHenry value and compare against glycerol's value, obtained
    #from EPA dashboard
    logHenry = chem.physical_and_invitro.data[chem.cas,'logHenry']
    if (is.na(logHenry)) stop (
"Henry's constant is not available for this compound")
    glycerol_logHenry = -7.80388
    if (logHenry <= glycerol_logHenry){ 
    warning("Henry's constant, as a measure of volatility, is smaller for the
    queried chemical than for glycerol, a chemical generally considered
    nonvolatile. Please proceed after having considered whether the inhalation
    exposure route is nonetheless relevant.")
    }
  }
  
    #Screen for compatible input that goes on to specify forcing function data series. 
  if(is.null(forcings)) {
  
  
    if (exp.duration > period){
      stop('If not specifying \'dose.matrix\' data series explicitly, additional arguments are needed
      to generate a \'dose.matrix\' argument with a cyclic exposure pattern across the simulation:
      exp.conc, period, exp.start.time, exp.duration, and days simulated.')
    }
    period <- period/24 #convert time period in hours to days
    exp.duration <- exp.duration/24 #convert exposure duration in hours to days
    
    #Assemble function for initializing 'forcings' argument data series with
    #certain periodicity and exposure concentration in default case, used if 
    #the 'forcings' argument is not otherwise specified.
    forcing <- function(exp.conc, period, exp.start.time, exp.duration, days) {
      #Provide for case in which forcing functionality is effectively turned off
      if (exp.conc == 0) {
        conc.matrix = NULL
      } else {
      Nrep <- ceiling((days - exp.start.time)/period) 
      times <- rep(c(exp.start.time, exp.duration), Nrep) + rep(period * (0:(Nrep - 1)), rep(2, Nrep))
      y  <- rep(c(exp.conc,0), Nrep)
      conc.matrix = cbind(times,y)
      }
      return(conc.matrix)
    }
    forcings = forcing(exp.conc, period, exp.start.time = 0, exp.duration, days) 
  }
      
      #Comment out tentative alternate scheme to forcings for now
      ###
    #Nrep <- ceiling((days - exp.start.time)/period)
# We want the start and stop timeS:
    #time <- sort(c(period * (0:(Nrep - 1)), # Start times
      #period * (0:(Nrep - 1))+exp.duration)) # End times
    #dose  <- rep(c(exp.conc,0), Nrep)
    #dosing.matrix = cbind(dose,time)
      ###
  
  
  #Now make call to solve_model with gas model specific arguments configured 
  out <- solve_model(
    chem.name = chem.name,
    chem.cas = chem.cas,
    dtxsid=dtxsid,
    times=times,
    parameters=parameters,
    model="gas_pbtk",
    route='inhalation',
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
    parameterize.arg.list = list(
      regression=regression,
      default.to.human=default.to.human,
      restrictive.clearance = restrictive.clearance,
      exercise = exercise,
      vmax = vmax,
      km = km,
      fR = fR,
      VT = VT,
      VD = VD),
    minimum.Funbound.plasma=minimum.Funbound.plasma,
    fcontrol = fcontrol,
    forcings = forcings,
    ...)
  
  return(out)
}

