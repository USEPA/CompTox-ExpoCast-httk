#' Solve_dermal_PBTK
#' 
#' This function solves for the amounts or concentrations of a chemical in
#' different tissues as functions of time based on the dose and dosing
#' frequency. 
#' 
#' Model units are the same as media concentration, units/L or units when
#' use.amounts=T.
#' 
#' New doses replace rather than add to previous doses. A concentration of 0 in
#' dosing.matrix switches off the dosing/diffusion between the media and
#' exposed skin.
#' 
#' Note that the model parameters have units of hours while the model output is
#' in days.
#' 
#' Default NULL value for doses.per.day solves for a single dose.
#' 
#' The compartments used in this model are the gutlumen, gut, liver, kidneys,
#' veins, arteries, lungs, skin, exposed skin, media, and the rest of the body.
#' 
#' The extra compartments include the amounts or concentrations metabolized by
#' the liver and excreted by the kidneys through the tubules.
#' 
#' AUC is the area under the curve of the plasma concentration.
#' 
#' When species is specified as rabbit, dog, or mouse, the function uses the
#' appropriate physiological data(volumes and flows) but substitues human
#' fraction unbound, partition coefficients, and intrinsic hepatic clearance.
#' 
#' @param chem.name Either the chemical name, CAS number, or the parameters
#' must be specified.
#' @param chem.cas Either the chemical name, CAS number, or the parameters must
#' be specified.
#' @param dtxsid EPA's DSSTox Structure ID (\url{http://comptox.epa.gov/dashboard})  
#' the chemical must be identified by either CAS, name, or DTXSIDs
#' @param times Optional time sequence for specified number of days.  Dosing
#' sequence begins at the beginning of times.
#' @param parameters Chemical parameters from parameterize_pbtk function,
#' overrides chem.name and chem.cas.
#' @param days Length of the simulation.
#' @param tsteps The number time steps per hour.
#' @param concentration Concentration of media at dosing times, units/L.
#' @param doses.per.day Number of doses per day.
#' @param initial.values Vector containing the initial concentrations or
#' amounts of the chemical in specified tissues with units corresponding to
#' output.units.  Defaults are zero.
#' @param initial.value.units Vector of character strings containing the units
#' corresponding to 'initial.values' specified for the model outputs.
#' Default is assuming the units match expected compartment units for the model.
#' @param plots Plots all outputs if true.
#' @param suppress.messages Whether or not the output message is suppressed.
#' @param species Species desired (either "Rat", "Rabbit", "Dog", "Mouse", or
#' default "Human").
#' @param use.amounts Return outputs as amounts instead of concentrations.
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
#' @param dosing.matrix Matrix consisting of three columns named
#' "concentration", "Vmedia", and "time" containing the dosing times, days,
#' with the applied concentration, units/L, and the volume of the applied
#' media, L.
#' @param adjusted.Funbound.plasma Uses adjusted Funbound.plasma when set to
#' TRUE along with partition coefficients calculated with this value.
#' @param regression Whether or not to use the regressions in calculating
#' partition coefficients.
#' @param restrictive.clearance Protein binding not taken into account (set to
#' 1) in liver clearance if FALSE.
#' @param skin_depth skin_depth of skin, cm, used in calculating Kp.
#' @param skin.pH pH of dermis/skin, used in calculating Kp and Kskin2media.
#' @param vmax.km Whether or not to use Michaelis-Menten kinetics
#' @param ... Additional arguments passed to the integrator.
#' @return A matrix of class deSolve with a column for time(in days), each
#' compartment, the area under the curve, and plasma concentration and a row
#' for each time point.
#' 
#' @author John Wambaugh and Robert Pearce
#' @keywords Solve
#' @examples
#' 
#' Vmedia <- c(.1,.2,.3)
#' time <- c(0,.5,3)
#' concentration <- c(2,0,3)
#' dosing.matrix <- cbind(time,concentration,Vmedia)
#' out <- solve_dermal_pbtk(chem.name='bisphenola',dosing.matrix=dosing.matrix,plots=T)
#' 
#' parameters <- parameterize_dermal_pbtk(chem.name='bisphenola',skin_depth=1)
#' parameters$Fskinexposed <- 0.25
#' parameters$Kp <- 2
#' parameters$Fdermabs <- 0.5
#' parameters$Vmedia <- 1
#' out <- solve_dermal_pbtk(parameters=parameters,concentration=100,plots=T)
#' 
#' @export solve_dermal_pbtk
#' @useDynLib httk
#' @import deSolve
solve_dermal_pbtk <- function(chem.name = NULL,
                    chem.cas = NULL,
                    dtxsid = NULL, 
                    model.type = "dermal", #can also be "dermal_1subcomp"
                    times=NULL,
                    parameters=NULL,
                    days=10,
                    species="Human",
                    tsteps = 4, # tsteps is number of steps per hour
                    dose = NULL, #changed from concentration = 1
                    dosing.matrix=NULL,
                    forcings=NULL, # added by AEM, 2/1/2022, copying solve_gas_pbtk
                    exp.start.time = 0, #default starting time in specifying forcing exposure, AEM 2/1/2022
                    exp.conc = 1, #default exposure concentration for forcing data series, AEM 2/1/2022
                    exp.duration = 12, #for forcing functions, AEM, 2/1/2022
                    period = 24, #for forcing functions, AEM, 2/1/2022
                    daily.dose=NULL, #added
                    doses.per.day=NULL,
                    initial.values=NULL,
                    initial.value.units=NULL,
                    plots=F,
                    suppress.messages=F,
                    #use.amounts=F, #added
                    input.units="mg/kg", #added
                    output.units=NULL, #added
                    method="lsoda",rtol=1e-8,atol=1e-12,
                    default.to.human=F,
                    recalc.blood2plasma=F,
                    recalc.clearance=F,
                    adjusted.Funbound.plasma=T,
                    regression=T,
                    restrictive.clearance = T,
                    minimum.Funbound.plasma=0.0001, #added
                    monitor.vars=NULL, #added
                    skin_depth=0.3,skin.pH=7,
                    vmax.km=F,
                    ...)
{

  # COPIED BY AEM, 2/1/2022 from solve_gas_pbtk
  #Only generate the forcings if other dosing metrics are null; they're not
  #designed to work together in a very meaningful way
  if (is.null(dosing.matrix) & is.null(doses.per.day) & is.null(forcings))
  {
    if (exp.duration > period){
      stop('If not specifying \'dose.matrix\' data series explicitly, 
      additional arguments are needed to generate a \'dose.matrix\' argument
      with a cyclic exposure pattern across the simulation:
      exp.conc, period, exp.start.time, exp.duration, and days simulated.')
    }
    period <- period/24 #convert time period in hours to days
    exp.duration <- exp.duration/24 #convert exposure duration in hours to days
    
    #Assemble function for initializing 'forcings' argument data series with
    #certain periodicity and exposure concentration in default case, used if 
    #the 'forcings' argument is not otherwise specified.
    forcings_gen <- function(exp.conc, period, exp.start.time, exp.duration, days) {
      #Provide for case in which forcing functionality is effectively turned off
      if (exp.conc == 0) {
        conc.matrix = NULL
      } else {
        Nrep <- ceiling((days - exp.start.time)/period) 
        times <- rep(c(exp.start.time, exp.duration), Nrep) + rep(period * (0:(Nrep - 1)), rep(2, Nrep))
        #times <- sort(c(times,times)) # two forcings, so two times AEM, 2/1/2022
        forcing_values  <- rep(c(exp.conc,0), Nrep) #exp.conc, 0 -forcing; 0,0 -switch, AEM 2/1/2022
        conc.matrix = cbind(times,forcing_values)
      }
      return(conc.matrix)
    }
    
    forcings = list(forcings_gen(exp.conc, period, exp.start.time = 0, exp.duration, days), #forcing
                    forcings_gen(exp.conc,period,exp.start.time=0,exp.duration,days)) #switch
    #default forcings are 1 and 1
    forcings = list(cbind(times=exp.start.time,forcing_values=1), #forcing
                    cbind(times=exp.start.time,forcing_values=1)) #switch
  }
  
  if (model.type=="dermal"){
    model.forsolver="dermal"
  } else if (model.type=="dermal_1subcomp"){
    model.forsolver="dermal_1subcomp"
  } else { stop("Input of model.type is incorrect. Must either by 'dermal' (default) or 'dermal_1subcomp'.")}
  
  out <- solve_model(
    chem.name = chem.name,
    chem.cas = chem.cas,
    dtxsid=dtxsid,
    times=times,
    parameters=parameters,
    model=model.forsolver,
    route='dermal',
    dosing=list(
      initial.dose=dose,
      dosing.matrix=dosing.matrix,
      daily.dose=daily.dose,
      doses.per.day=doses.per.day,
      forcings=forcings #added by AEM, 2/1/2022, copying solve_gas_pbtk
    ),
    days=days,
    tsteps = tsteps, # tsteps is number of steps per hour
    initial.values=initial.values,
    initial.value.units=initial.value.units,
    plots=plots,
    monitor.vars=monitor.vars,
    suppress.messages=suppress.messages,
    species=species, #other species not (yet) supported by solve_fetal_pbtk
    #input.units=input.units,
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
