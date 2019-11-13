#' solve_gas_pbtk
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
#' and 1/kg^(3/4) for florws \cr}
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
#' @param times Optional time sequence for specified number of days.  Dosing
#' sequence begins at the beginning of times.
#' @param parameters Chemical parameters from parameterize_pbtk function,
#' overrides chem.name and chem.cas.
#' @param days Length of the simulation.
#' @param tsteps The number of time steps per hour.
#' @param daily.dose Total daily dose, mg/kg BW.
#' @param dose Amount of a single dose, mg/kg BW. Overwrites daily.dose.
#' @param doses.per.day Number of doses per day.
#' @param dosing.matrix Vector of dosing times or a matrix consisting of two
#' columns or rows named "dose" and "time" containing the time and amount, in
#' mg/kg BW, of each dose. With the gas pbtk model, dosing.matrix is set to 
#' specify forcing concentrations to the integrator, either in combination 
#' with eventdata or on its own. 
#' @param forcings Manual input of 'forcings' data series argument for ode
#' integrator, defaults to NULL
#' @param conc Specified inhalation exposure concentration for use in assembling
#' 'forcings' data series argument for integrator. Defaults to uM/L **?
#' @param period For use in assembling forcing function data series 'forcings'
#' argument, specified in hours
#' @param exposure.duration For use in assembling forcing function data 
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
#' @param adjusted.Funbound.plasma Uses adjusted Funbound.plasma when set to
#' TRUE along with partition coefficients calculated with this value.
#' @param regression Whether or not to use the regressions in calculating
#' partition coefficients.
#' @param restrictive.clearance Protein binding not taken into account (set to
#' 1) in liver clearance if FALSE.
#' @param ... Additional arguments passed to the integrator.
#' @return A matrix of class deSolve with a column for time(in days), each
#' compartment, the area under the curve, and plasma concentration and a row
#' for each time point.
#' @author John Wambaugh, Matt Linakis, and Mark Sfeir
#' @references Pearce, Robert G., et al. "Httk: R package for high-throughput
#' toxicokinetics." Journal of statistical software 79.4 (2017): 1.
#' @keywords Solve
#' @examples
#' 
#' 
#' solve_pbtk(chem.name='Bisphenol-A',dose=.5,days=1,doses.per.day=2,tsteps=2)
#' out <- solve_pbtk(chem.name='bisphenola',dose=0,output.units='mg', 
#'                   plots=TRUE,initial.values=c(Agut=200))
#' params <- parameterize_pbtk(chem.cas="80-05-7")
#' solve_pbtk(parameters=params)
#'                   
#' \dontrun{
#' parameters <- parameterize_pbtk(chem.name = "triclosan", species = "rat")
#' parameters["Funbound.plasma"] <- 0.1
#' out <- solve_pbtk(parameters=parameters)
#' 
#' library("ggplot2")
#' out <- solve_pbtk(chem.name = "Bisphenol A", days = 50, doses.per.day = 3)
#' plot.data <- as.data.frame(out)
#' css <- calc_analytic_css(chem.name = "Bisphenol A")
#' c.vs.t <- ggplot(plot.data,aes(time, Cplasma)) + geom_line() +
#' geom_hline(yintercept = css) + ylab("Plasma Concentration (uM)") +
#' xlab("Day") + theme(axis.text = element_text(size = 16), axis.title =
#' element_text(size = 16), plot.title = element_text(size = 17)) +
#' ggtitle("Bisphenol A")
#' print(c.vs.t)
#' }
#' 
#' @export solve_gas_pbtk
#' @useDynLib httk
#' @import deSolve
solve_gas_pbtk <- function(chem.name = NULL,
                           chem.cas = NULL,
                           times=NULL,
                           parameters=NULL,
                           days=10,
                           tsteps = 4, #tsteps is number of steps per hour
                           daily.dose = NULL,
                           doses.per.day = NULL,
                           dose = NULL, #Assume single dose is in mg/kg BW/day
                           dosing.matrix = NULL,
                           forcings = NULL, 
                           conc = 1, #default exposure concentration for forcing data series
                           period = 24, 
                           exp.duration = 12,
                           fcontrol = list(method='constant',rule=2,f=0), 
                           initial.values=NULL,
                           plots=F,
                           suppress.messages=F,
                           species="Human",
                           output.units='uM',
                           method="lsoda",rtol=1e-8,atol=1e-12,
                           default.to.human=F,
                           recalc.blood2plasma=F,
                           recalc.clearance=F,
                           adjusted.Funbound.plasma=T,
                           regression=T,
                           restrictive.clearance = T,
                           minimum.Funbound.plasma=0.0001,
                           monitor.vars=NULL,
                           ...)


{
  
  
  #Assemble function for initializing 'forcings' argument data series with
  #certain periodicity and exposure concentration in default case, used if 
  #the 'forcings' argument is not otherwise specified.
  if(is.null(forcings)) {
    if (exp.duration > period){
      stop('If not specifying \'forcings\' data series explicitly, additional arguments are needed
      to generate a \'forcings\' argument with a cyclic exposure pattern across the simulation:
      conc, period, start.time, exp.duration, conc, and days simulated.')
    }
    period <- period/24 #convert time period in hours to days
    exp.duration <- exp.duration/24 #convert exposure duration in hours to days
    forcing <- function(conc, period, start.time, exp.duration, days) {
      Nrep <- ceiling(days/period) 
      times <- rep(c(start.time, exp.duration), Nrep) + rep(period * (0:(Nrep - 1)), rep(2, Nrep))
      y  <- rep(c(conc,0), Nrep)
      conc.matrix = cbind(times,y)
      return(conc.matrix)
    }
    forcings = forcing(conc, period, start.time = 0, exp.duration, days) 
  }
  
  out <- solve_model(
  chem.name = chem.name,
  chem.cas = chem.cas,
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
  default.to.human=default.to.human,
  recalc.blood2plasma=recalc.blood2plasma,
  recalc.clearance=recalc.clearance,
  adjusted.Funbound.plasma=adjusted.Funbound.plasma,
  regression=regression,
  restrictive.clearance = restrictive.clearance,
  minimum.Funbound.plasma=minimum.Funbound.plasma,
  fcontrol = fcontrol,
  forcings = forcings,
  ...)
  
  return(out)
}













