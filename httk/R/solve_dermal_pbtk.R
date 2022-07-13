#' Solve_dermal_PBTK
#' 
#' This function solves for the amounts or concentrations of a chemical in
#' different tissues as functions of time based on the dose and dosing
#' frequency. 
#' 
#' Model units are the same as vehicle concentration, units/L or units when
#' use.amounts=T.
#' 
#' New doses replace rather than add to previous doses. A concentration of 0 in
#' dosing.matrix switches off the dosing/diffusion between the vehicle and
#' exposed skin.
#' 
#' Note that the model parameters have units of hours while the model output is
#' in days.
#' 
#' Default NULL value for doses.per.day solves for a single dose.
#' 
#' The compartments used in this model are the gutlumen, gut, liver, kidneys,
#' veins, arteries, lungs, unexposed stratum corneum and viable epidermis, exposed 
#' stratum corneum and viable epidermis, vehicle, and the rest of the body. When model.type 
#' = "dermal_1subcomp", the stratum corneum and viable epidermis are replaced with 
#' one compartment: skin.
#' 
#' The extra compartments include the amounts or concentrations metabolized by
#' the liver and excreted by the kidneys through the tubules.
#' 
#' AUC is the area under the curve of the plasma concentration.
#' 
#' When species is specified as rabbit, dog, or mouse, the function uses the
#' appropriate physiological data(volumes and flows) but substitutes human
#' fraction unbound, partition coefficients, and intrinsic hepatic clearance.
#' 
#' @param chem.name Either the chemical name, CAS number, or the parameters
#' must be specified.
#' @param chem.cas Either the chemical name, CAS number, or the parameters must
#' be specified.
#' @param dtxsid EPA's DSSTox Structure ID (\url{http://comptox.epa.gov/dashboard})  
#' the chemical must be identified by either CAS, name, or DTXSIDs.
#' @param model.type Choice of dermal model, either the default "dermal" for the 
#' model with 2 sub compartments (stratum corneum and viable epidermis) for skin,
#' or "dermal_1subcomp" for the model with 1 compartment for the skin.
#' @param times Optional time sequence for specified number of days.  Dosing
#' sequence begins at the beginning of times.
#' @param parameters Chemical parameters from parameterize_dermal_pbtk function,
#' overrides chem.name and chem.cas.
#' @param days Length of the simulation.
#' @param tsteps The number time steps per hour.
#' @param plots Plots all outputs if true.
#' @param monitor.vars Which variables are returned as a function of time. 
#' Default values of NULL looks up variables specified in modelinfo_MODEL.R
#' @param suppress.messages Whether or not the output message is suppressed.
#' @param species Species desired (either "Rat", "Rabbit", "Dog", "Mouse", or
#' default "Human").
#' @param method Method used by integrator (deSolve).
#' @param rtol Argument passed to integrator (deSolve).
#' @param atol Argument passed to integrator (deSolve).
#' @param recalc.blood2plasma Recalculates the ratio of the amount of chemical
#' in the blood to plasma using the input parameters, calculated with
#' hematocrit, Funbound.plasma, and Krbc2pu.
#' @param recalc.clearance Recalculates the the hepatic clearance
#' (Clmetabolism) with new million.cells.per.gliver parameter.
#' @param adjusted.Funbound.plasma Uses adjusted Funbound.plasma when set to
#' TRUE along with partition coefficients calculated with this value.
#' @param parameterize.arg.list Additional parameterized passed to the model 
#' parameterization function, "parameterize_dermal_pbtk".
#' @param BW Body weight, kg.
#' @param Vvehicle Volume of vehicle applied to skin in L, defaults to 0.01 L.
#' @param initial.dose Concentration
#' @param dermal.dosing Matrix consisting of three columns named
#' "concentration", "Vvehicle", and "time" containing the dosing times, days,
#' with the applied concentration, units/L, and the volume of the applied
#' vehicle, L.
#' @param ... Additional arguments passed to the integrator.
#' @return A matrix of class deSolve with a column for time (in days), each
#' compartment, the area under the curve, and plasma concentration and a row
#' for each time point.
#' 
#' @author Annabel Meade, John Wambaugh, and Robert Pearce
#' @keywords Solve
#' @examples
#' 
#' Vvehicle <- c(.1,.2,.3)
#' time <- c(0,.5,3)
#' concentration <- c(2,0,3)
#' dermal.dosing <- cbind(time,concentration,Vvehicle)
#' out <- solve_dermal_pbtk(chem.name='bisphenola',dermal.dosing=dermal.dosing,plots=T)
#' 
#' parameters <- parameterize_dermal_pbtk(chem.name='bisphenola',skin_depth=1)
#' parameters$Fskin_exposed <- 0.25
#' parameters$Vvehicle <- 1
#' out <- solve_dermal_pbtk(parameters=parameters,plots=T)
#' 
#' @export solve_dermal_pbtk
#' @useDynLib httk
#' @import deSolve
solve_dermal_pbtk <- function(chem.name = NULL, #solve_model
                    chem.cas = NULL, #solve_model
                    dtxsid = NULL,#solve_model
                    model.type = "dermal", #can also be "dermal_1subcomp"
                    times=NULL, #solve_model
                    parameters=NULL, #solve_model
                    days=10, #solve_model
                    tsteps = 4, # solve_model
                    #initial.values=NULL, #solve_model
                    #initial.value.units=NULL, #solve_model
                    plots = FALSE, #solve_model
                    monitor.vars=NULL, #solve_model
                    suppress.messages=F, #solve_model
                    species = "Human", #solve_model
                    #output.units=NULL, #solve_model DOSING
                    method="lsoda",rtol=1e-8,atol=1e-12, #solve_model
                    recalc.blood2plasma=FALSE, #solve_model
                    recalc.clearance=FALSE, #solve_model
                    adjusted.Funbound.plasma=TRUE, #solve_model
                    minimum.Funbound.plasma=1e-4, #solve_model
                    parameterize.arg.list = list(
                      default.to.human=FALSE,
                      clint.pvalue.threshold=0.05,
                      restrictive.clearance = TRUE,
                      regression=TRUE),
                    route = NULL, #DERMAL
                    Vvehicle = NULL, #DERMAL
                    initial.dose = NULL, #DERMAL - DOSING
                    input.units="mg/kg", #DERMAL - DOSING
                    dosing.dermal = NULL, #DERMAL
                    #doses.per.day = NULL,
                    #daily.dose = NULL,
                    dosing.matrix = NULL, #DERMAL - DOSING
                    ...)
{
  # DOSING
  # Set start.time for dosing
  if (is.null(times)) {
    start.time = 0
    } else {start.time = times[1]}
  
  # Check for exposure route
  if (is.null(route)) { route <- 'dermal'; warning(
    "If route is not chosen, it is set to dermal by default.")}
  
  # Create forcing function for Vvehicle - single value
  if (is.null(dosing.dermal)) {
    if (is.null(Vvehicle)) {
      Vvehicle <- 0.1; #only affects model if route=dermal
      if (route=='dermal') warning(paste("Vvehicle not specified, so set to", Vvehicle, "L."))
      if (is.null(initial.dose)){ initial.dose = 1; warning(paste(
        "Initial dose not specified, so automatrically set to 1 mg/kg BW."))}
    } 
    if (length(Vvehicle)!=1) stop(
      "Vvehicle input must be one value. To change the volume of the vehicle over time, 
      use the dosing.dermal input.") 
    if ((Vvehicle<=0) & (route=="dermal")) { stop(
      "Vvehicle must be positive and non-zero if the initial dose is dermal.")
    }
    forcings = cbind(times = start.time, forcing_values = Vvehicle)
  }
  
  # Account for dosing.dermal
  if (!is.null(dosing.dermal)){ 
    if (!is.null(dosing.matrix)) stop(
      "Either dosing.matrix or dosing.dermal can be used, but not both. One must be null.")
    if (route!='dermal') stop(
      "Route must be set to dermal in order to use dosing.dermal input.")
    if (any(is.na(dosing.dermal))) stop(#check for NaNs
      "Dosing matrix dosing.dermal cannot contain NA values.")
    if (dim(dosing.dermal)[2]!=3) stop( #check for dimensions
    "dosing.dermal should be matrix with three named columns: time (days), 
    concentration (uM), and Vvehicle (L).")
    
    dose.times <- dosing.dermal[,"time"]
    dose.Vvehicle <- dosing.dermal[,"Vvehicle"]
    dose.conc <- dosing.dermal[,"concentration"]
    
    # Set dosing.matrix based on amount
    dose.vec <- dose.Vvehicle*dose.conc #Avehicle inputs
    dosing.matrix <- cbind(time=dose.times, dose=dose.vec)
    if (dose.times[1]==start.time){ #if dermal.dosing starts at beginning of time
      initial.dose <- dose.vec[1];
      input.units <- "umol";
      dosing.matrix <- dosing.matrix[-1,];
    }
    dosing.matrix<-matrix(dosing.matrix,ncol=2,dimnames=list(NULL,c("time","dose"))) #if only one dose
    
    #Reset forcing function for Vvehicle
    forcings = cbind(times = dose.times, forcing_values = dose.Vvehicle)
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
    route=route,
    dosing=list(
      initial.dose=initial.dose,
      dosing.matrix=dosing.matrix,
      forcings=forcings 
    ),
    days=days,
    tsteps = tsteps, # tsteps is number of steps per hour
    #initial.values=initial.values,
    #initial.value.units=initial.value.units,
    plots=plots,
    monitor.vars=monitor.vars,
    suppress.messages=suppress.messages,
    species=species, #other species not (yet) supported by solve_fetal_pbtk
    input.units=input.units,
    #output.units=output.units,
    method=method,rtol=rtol,atol=atol,
    recalc.blood2plasma=recalc.blood2plasma,
    recalc.clearance=recalc.clearance,
    adjusted.Funbound.plasma=adjusted.Funbound.plasma,
    parameterize.arg.list = c(
      model.type = model.type,
      parameterize.arg.list
    ),
    ...)
  
  return(out) 
}
