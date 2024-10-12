#' Solve_dermal_PBTK
#' 
#' This function solves for the amounts or concentrations in uM of a chemical in
#' different tissues as functions of time after dermal exposure. The user can input 
#' dermal doses via one of three options:
#' \itemize{
#'   \item{dose.duration: }{User can input the length of exposure time for one dermal
#'   dose before wash-off occurs. Note that initial.dose can be used to change the
#'   initial dose used along with this option.}
#'   \item{dosing.dermal: }{With this option, users can input multiple doses over
#'   time as a matrix with columns for time, the volume of vehicle administered,
#'   and the concentration of the vehicle administered. Note that the the parameter
#'   washoff can be used to specify whether chemical is washed off in between doses.}
#'   \item{dosing.matrix: }{This option is also used to describe multiple exposure doses
#'   over time, and is described in the help file of solve_model. 
#'   Note that unlike dosing.dermal, Vvehicle cannot be changed with this option.}
#' }
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
#' The compartments used in this model are the gutlumen, gut, liver, kidneys,
#' veins, arteries, lungs, unexposed skin, exposed skin, vehicle, and the rest of the body. When model.type 
#' = "dermal", a 2-compartment model is used where skin
#' is divided into the stratum corneum, SC, and the combined viable epidermis and
#' dermis, ED.
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
#' @param model.type Choice of dermal model, either the default "dermal_1subcomp" for
#' the model with 1 compartment for the skin; or "dermal" for the 
#' model with 2 sub compartments for skin: the stratum corneum (SC) and the combined
#' viable epidermis and dermis (ED).
#' @param method.permeability For "dermal_1subcomp" model, method of calculating 
#' the permeability coefficient, P, either "Potts-Guy" or "UK-Surrey". Default
#' is "UK-Surrey" (Sawyer et al., 2016 and Chen et al., 2015), which uses Fick's
#' law of diffusion to calculate P. For "dermal" model, this parameter is ignored.
#' @param Kvehicle2water Partition coefficient for the vehicle (sometimes called the 
#' media) carrying the chemical to water. Default is "water", which assumes the vehicle is water.
#' Other optional inputs are "octanol" and "olive oil".
#' @param times Optional time sequence for specified number of days.  Dosing
#' sequence begins at the beginning of times.
#' @param parameters Chemical parameters from parameterize_dermal_pbtk function,
#' overrides chem.name and chem.cas.
#' @param days Length of the simulation.If "times" input is used, this is ignored.
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
#' parameterization function, "parameterize_dermal_pbtk". The inputs "model.type",
#' "method.permeability", and "Kvehicle2water" are not passed through this.
#' @param route Route of exposure, can be "oral" OR "iv" OR "dermal" (default).
#' @param Vvehicle Volume of vehicle applied to skin in L, defaults to 0.1 L. If 
#' InfiniteDose=TRUE, this parameter is ignored and set = 1.
#' @param initial.dose Initial exposure dose. If InfiniteDose=TRUE, this is a concentration, 
#' otherwise, this is an amount.
#' @param input.units Exposure units applied to initial.dose and/or dosing.dermal.
#' If InfiniteDose=TRUE, must be a concentration, e.g., "mg/kg/L" (default), otherwise,
#' must be an amount, e.g., "mg/kg" (default).
#' @param dose.duration Amount of time dermal dose is on skin before being washed off.
#' Note that when dose.duration is used, washoff=TRUE.
#' @param dose.duration.units Units for dose.duration, can be "minutes" OR "hours" 
#' OR "days" (default).
#' @param dosing.dermal Matrix consisting of three columns named
#' "Cvehicle", "Vvehicle", and "time" containing the dosing times, days,
#' with the applied amount in the vehicle, and the volume of the applied
#' vehicle, L. Note that the units of Cvehicle are controlled by input.units. **If
#' InfiniteDose=TRUE, the Vvehicle column of dosing.dermal is ignored.**
#' @param washoff If TRUE, any chemical left on the skin is assumed to be replaced 
#' by new dose  (i.e., wash-off occurs before new dose is administered). If FALSE 
#' (default), any chemical left on the skin is added to the new dose.
#' @param InitialDose If TRUE, we assume infinite dosing (i.e., a constant unchanging concentration
#' of chemical in the vehicle is considered) and Cvehicle is a constant. If
#' FALSE (default), dosing is finite and Cvehicle changes over time.
#' @param ... Additional arguments passed to the integrator.
#' @return A matrix of class deSolve with a column for time (in days), each
#' compartment, the area under the curve, and plasma concentration and a row
#' for each time point.
#' 
#' @author Annabel Meade, John Wambaugh, and Robert Pearce
#' @keywords Solve
#' @examples
#' 
#' # Dermal exposure to default dose
#' out <- solve_dermal_pbtk(chem.name="bisphenola",plots=TRUE)
#' 
#' # Dermal exposure to 20 mg/L in 0.01 L of octanol with wash-off after 8 hours
#' # Since skin permeability happens quickly for bisphenol A, let's only look at 3 days.
#' dose.conc <- 2 #mg/L
#' Vvehicle <- 0.01 #L
#' initial.dose <- dose.conc*Vvehicle
#' out <- solve_dermal_pbtk(chem.name="bisphenola", initial.dose=initial.dose, input.units="mg",
#' Vvehicle=0.01, Kskin2vehicle="octanol", dose.duration=8, dose.duration.units="hr", days=3, plots=TRUE)
#' 
#' # Now, try this again with an infinite dose.
#' out <- solve_dermal_pbtk(chem.name="bisphenola", initial.dose=dose.conc, input.units="mg/L",
#' Vvehicle=0.01, Kskin2vehicle="octanol", dose.duration=8, dose.duration.units="hr", 
#' days=3, InfiniteDose=TRUE, plots=TRUE)
#' 
#' # Now, try a scenario where 2 mg of chemical in 1 mL of water is applied and washed off 8 hours later every day for 5 days
#' num.days <- 5;
#' time <- c(0:(num.days-1),(0:(num.days-1)) + 8/24); time <- sort(time) #in days
#' Vvehicle <- rep(1e-3,length(time)) #convert mL to L
#' Cvehicle <- rep(c(2,0),num.days)/Vvehicle # convert 2 mg to mg/L
#' dosing.dermal <- cbind(time,Cvehicle,Vvehicle)
#' out <- solve_dermal_pbtk(chem.name='bisphenola',dosing.dermal=dosing.dermal,plots=T)
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
                    model.type = "dermal_1subcomp", #can also be "dermal"
                    method.permeability = "UK-Surrey",
                    Kvehicle2water = NULL,
                    times=NULL, #solve_model
                    parameters=NULL, #solve_model
                    days=10, #solve_model
                    tsteps = 4, # solve_model
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
                    daily.dose = NULL, #ORAL - DOSING
                    doses.per.day = NULL, #ORAL - DOSING
                    input.units=NULL, #DERMAL - DOSING
                    dose.duration = NULL,
                    dose.duration.units = NULL,
                    dosing.dermal = NULL, #DERMAL
                    #doses.per.day = NULL,
                    #daily.dose = NULL,
                    dosing.matrix = NULL, #DERMAL - DOSING
                    washoff = FALSE,
                    InfiniteDose = FALSE,
                    ...)
{

# DON'T LET MODEL-SPECIFIC THINGS MAKE AN ERROR IN SOLVE_MODEL (put stop in solve_dermal_pbtk)
  
  # Set start.time for dosing
  if (is.null(times)) {
    start.time = 0
    } else {start.time = times[1]}
  
  # Check for exposure route 
  #check that it is iv OR dermal OR oral (if dermal.washoff, put ERROR stop())
  if (is.null(route)) { route <- 'dermal'; if(!suppress.messages) warning(
    "If route is not chosen, it is set to dermal by default.")}
  if (!(route %in% c("iv","oral","dermal"))){ stop(
    'route must either be "iv", "oral", or "dermal". To allow wash off to occur,
    set route="dermal" and washoff=TRUE. To allow infinite dosing, set route="dermal"
    and InfiniteDose=TRUE.')}
  
  # Check InfiniteDose is logical or = 1 or 0
  if (InfiniteDose==1) {
    InfiniteDose=TRUE
  } else if (InfiniteDose==0){
    InfiniteDose=FALSE
  } else if (typeof(InfiniteDose)!="logical"){
    stop("InfiniteDose must be either be equal to FALSE (i.e., 0) (default) or TRUE (i.e., 1).")
  }  
  # Check InfiniteDose in parameters
  if (!is.null(parameters)){
    if(InfiniteDose!=parameters$InfiniteDose){
      InfiniteDose = parameters$InfiniteDose
      if(!suppress.messages) warning("InfiniteDose in parameters overrides InfiniteDose input into solve_dermal_pbtk.")
    }
  }

  
  # ROUTE - IV or ORAL -------------------------------------------------
  if (route %in% c("iv","oral")){
    if (!all(simplify2array(lapply(list(Vvehicle,
                                       dosing.dermal,
                                       dose.duration,
                                       dose.duration.units),is.null))) | washoff | InfiniteDose){
      if(!suppress.messages) warning("When route is not set to 'dermal', inputs Vvehicle, dosing.dermal,
              dose.duration, dose.duration.units, washoff, and InfiniteDose are ignored.
              Vvehicle is set to 0.")
    }
    forcings = cbind(times = start.time, forcing_values = 0)
  }
  
  # ROUTE - DERMAL -------------------------------------------------------
  if (route=="dermal"){
    
    # INFINITE DOSING
    if (InfiniteDose){
      #Vvehicle and forcings set
      if (!is.null(Vvehicle) | washoff){
        if(!suppress.messages) warning("When InfiniteDose = T, washoff is ignored, and Vvehicle is set to 0 L and ignored.")
      }
      forcings = cbind(times=start.time, forcing_values = 0)
      
      # DOSING.DERMAL
      if (!is.null(dosing.dermal)){
        if (!is.null(dosing.matrix) | !is.null(dose.duration)) stop("Either dosing.matrix, dose.duration, or dosing.dermal can be used. Only one cannot be null.")
        if (any(is.na(dosing.dermal))) stop("Dosing matrix dosing.dermal cannot contain NA values.")
        if (dim(dosing.dermal)[2]==3){ if(!suppress.messages) warning("Vvehicle column of dosing.dermal ignored for when InfiniteDose=TRUE.")
        } else if (dim(dosing.dermal)[2]!=2){ stop("dosing.dermal should be matrix with two named columns: time (days), 
                                           Cvehicle (same units as input.units).")
        }
        dose.times <- dosing.dermal[,"time"]
        dose.Cvehicle <- dosing.dermal[,"Cvehicle"]
        # Set dosing.matrix based on amount
        dose.vec <- dose.Cvehicle
        dosing.matrix <- cbind(time=dose.times, dose=dose.vec)
        # Check if start.time is in dosing.dermal and account for initial.dose
        if (start.time %in% dose.times){
          if (!is.null(initial.dose)) {stop("Initial dose is specified in both initial.dose and dosing.dermal.")}
          initial.dose <- dose.vec[1];
          dosing.matrix <- dosing.matrix[-1,];
        }
        dosing.matrix<-matrix(dosing.matrix,ncol=2,dimnames=list(NULL,c("time","dose"))) #if only one dose
      }
      
    # FINITE DOSING
    } else {
      # Set Vvehicle and Forcings
      if (is.null(Vvehicle)) {
        Vvehicle <- 0.1; #only affects model if route=dermal
        if (is.null(dosing.dermal)) if(!suppress.messages) warning(paste("Vvehicle not specified, so set to", Vvehicle, "L."))
      }
      if (length(Vvehicle)!=1) stop("Vvehicle input must be one value. 
                                    To change the volume of the vehicle over time, use the dosing.dermal input.") 
      if ((Vvehicle<=0)) stop("Vvehicle must be positive and non-zero if the route is dermal.")
      forcings = cbind(times = start.time, forcing_values = Vvehicle)
      
      # DOSING.DERMAL
      if (!is.null(dosing.dermal)){
        if (!is.null(dosing.matrix) | !is.null(dose.duration)) stop("Either dosing.matrix, dose.duration, or dosing.dermal can be used. Only one cannot be null.")
        if (any(is.na(dosing.dermal))) stop("Dosing matrix dosing.dermal cannot contain NA values.")
        if (dim(dosing.dermal)[2]!=3) stop("dosing.dermal should be matrix with three named columns: time (days), 
                                           Cvehicle (same units as input.units / L, e.g., if input.units = 'mg/kg', then
                                           Cvehicle will be in 'mg/kg/L'), and Vvehicle (L).")
        dose.times <- dosing.dermal[,"time"]
        dose.Vvehicle <- dosing.dermal[,"Vvehicle"]
        dose.Cvehicle <- dosing.dermal[,"Cvehicle"]
        # Set dosing.matrix based on amount
        dose.vec <- dose.Vvehicle*dose.Cvehicle #Cvehicle inputs
        dosing.matrix <- cbind(time=dose.times, dose=dose.vec)
        # Set forcings
        forcings = cbind(times = c(start.time,dose.times), forcing_values = c(Vvehicle,dose.Vvehicle))
        # Check if start.time is in dosing.dermal and account for initial.dose
        if (start.time %in% dose.times){
          if (!is.null(initial.dose)) {stop("Initial dose is specified in both initial.dose and dosing.dermal.")}
          initial.dose <- dose.vec[1];
          dosing.matrix <- dosing.matrix[-1,];
          #Reset forcings
          forcings = cbind(times = dose.times, forcing_values = dose.Vvehicle)
        }
        dosing.matrix<-matrix(dosing.matrix,ncol=2,dimnames=list(NULL,c("time","dose"))) #if only one dose
      }
    }
    
    # DOSE.DURATION
    if(!is.null(dose.duration)){
      if(is.null(dose.duration.units)){ dose.duration.units="days"; if(!suppress.messages) warning("The dose.duration.units are automatically set to \"days\".\nTo change units, set dose.duration.units= \"hours\", \"mins\", or \"days\".")}
      if (length(dose.duration)!=1 | length(dose.duration.units)!=1){ stop("The dose.duration and dose.duration.units should only be one input. \nFor multiple dosing changes over time, use dosing.dermal.")}
      if (is.null(dose.duration.units) | (tolower(dose.duration.units) %in% c("days","day","d"))){
        dose.duration.days <- dose.duration;
      } else if (tolower(dose.duration.units) %in% c("hr","hrs","hours","h","hour")){
        dose.duration.days <- dose.duration/24
      } else if (tolower(dose.duration.units) %in% c("min","mins")){
        dose.duration.days <- dose.duration/24/60
      } else stop("dose.duration.units not in recognizable format. Set equal to 'hours' or 'mins' or 'days'.")
      
      dosing.matrix <- matrix(c(dose.duration.days,0),ncol=2,dimnames=list(NULL,c("time","dose")))
    } 
  }
  
  # INITIAL.DOSE and DOSE.UNITS
  if (InfiniteDose){input.units.default="mg/kg/L"} else {input.units.default="mg/kg"}
  
  if (is.null(input.units)){
    input.units <- input.units.default
    if (is.null(dosing.dermal)){
      if (is.null(initial.dose)){
        initial.dose <- 1;  
        if(!suppress.messages) warning(paste0("The initial.dose is automatically set to 1", input.units.default,"."))
      } else {
        if(!suppress.messages) warning(paste0("The input.units are not specified, so initial dose is set automatically to ",initial.dose," ",input.units.default,"."))
      }
    } else {
      if(is.null(initial.dose)){ stop("The initial.dose must be specified along with input.units.")}
    }
  }

  if (model.type=="dermal"){
    model.forsolver="dermal"
    #DERMAL DOES NOT WORK RIGHT NOW
    #stop('model.type="dermal" does not work right now. Please set model.type="dermal_1subcomp" or leave model.type blank.')
    
  } else if (model.type=="dermal_1subcomp"){
    model.forsolver="dermal_1subcomp"
  } else { stop("Input of model.type is incorrect. Must either by 'dermal' (default) or 'dermal_1subcomp'.")}
  
  # WASHOFF 
  if (washoff){
    if (route=="dermal"){
      route="dermal.washoff"
    } else if(!suppress.messages) warning(paste0('Since route is ',route,'washoff does not apply and is ignored.'))
  } else if (!is.null(dose.duration)){ 
    route="dermal.washoff"
    if(!suppress.messages) warning("Washoff occurs automatically if dose.duration is used.")
    }
  
  #Check for ppm
  if (!is.null(input.units)){
    if(input.units=="ppm"){
      input.units="ppmw"
      if (!(is.null(Kvehicle2water) || Kvehicle2water==1 || Kvehicle2water=="water")){
        if(!suppress.messages) warning("It is assumed that the dosing units are in ppm for water so that 1ppm = 1 mg/L.\n For concentrations in ppm in air assuming 25 degrees C, set input.units=\"ppmv\" or see help file for convert_units.")
      }
    }
  }

  
  #INFINITEDOSE
  if(InfiniteDose){
    route="dermal.InfiniteDose"
  }
  
  # Check that parameterize.arg.list contains restrictive.clearance for solve_model
  if (!("restrictive.clearance" %in% names(parameterize.arg.list))){
    paramterize.arg.list <- c(parameterize.arg.list,
                              restrictive.clearance = TRUE)
  }
  # par.arg.requirements <- list(default.to.human=FALSE,
  #                              clint.pvalue.threshold=0.05,
  #                              restrictive.clearance = TRUE,
  #                              regression=TRUE)
  # update.these <- !(names(par.arg.requirements) %in% names(parameterize.arg.list))
  # parameterize.arg.list <- c(parameterlize.arg.list,
  #                            par.arg.requirements[update.these])
  
  # Check that model.type, method.permeability, and Kvehicle2water is not in parameterize.arg.list
  outside.par.list <- c("method.permeability","model.type","Kvehicle2water","InfiniteDose")
  if (any(outside.par.list %in% names(parameterize.arg.list))){
    stop("model.type, method.permeability, Kvehicle2water, and InfiniteDose cannot appear in 
         parameterize.arg.list for solve_dermal_pbtk(). To change these options,
         assign them directly in the solve_dermal_pbtk function.")
  }
  
# Put everything into dosing at first:
  dosing <- list(
      initial.dose=initial.dose,
      dosing.matrix=dosing.matrix,
      forcings=forcings,
      daily.dose = daily.dose,
      doses.per.day = doses.per.day
    )
# But only keep the dosing parameters you need:
  dosing <- dosing[model.list[[model.type ]]$routes[[route]][["dosing.params"]]]

  out <- solve_model(
    chem.name = chem.name,
    chem.cas = chem.cas,
    dtxsid=dtxsid,
    times=times,
    parameters=parameters,
    model=model.forsolver,
    route=route,
    dosing=dosing,
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
      method.permeability = method.permeability,
      Kvehicle2water = Kvehicle2water,
      InfiniteDose = InfiniteDose,
      parameterize.arg.list
    ),
    ...)
  
  return(out) 
}
