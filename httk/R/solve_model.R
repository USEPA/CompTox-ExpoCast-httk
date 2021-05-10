#' Solve_model
#' 
#' solve_model's arguments prepare an ode system for numerical solution over
#' time of the amounts or concentrations (uM) of chemical in the different
#' bodily compartments of a given available species (either "Rat", "Rabbit",
#' "Dog", "Mouse", or default "Human").
#' 
#' The minimal usage case requires input that includes a chemical identifier
#' (whether name, CAS number, or other chemical parameterization) and a model
#' system of interest ("pbtk", "3compartment", "3compartmentss", "1compartment",
#' "schmitt", ...).
#' 
#' The 'dosing' argument includes all parameters needed to describe exposure
#' in terms of route of administration, frequency, and quantity short of 
#' scenarios that require use of a more precise forcing function. If the dosing
#' argument's namesake entries are left NULL, solve_model defaults to a
#' single-time dose of 1 mg/kg BW according to the given dosing route and 
#' associated type (either add/multiply, e.g. typically adds dose to gut lumen 
#' when oral route is specified).
#' 
#' AUC is the area under the curve of the plasma concentration.
#' 
#' Model parameters are named according to the following convention:
#' \tabular{lrrrr}{
#' prefix \tab suffix \tab Meaning \tab units \cr
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
#' @param chem.name Either the chemical name, CAS number, or the parameters
#' must be specified.
#' @param chem.cas Either the chemical name, CAS number, or the parameters must
#' be specified.
#' @param dtxsid EPA's DSSTox Structure ID (\url{https://comptox.epa.gov/dashboard})  
#' the chemical must be identified by either CAS, name, or DTXSIDs
#' @param times Optional time sequence for specified number of days. Dosing
#' sequence begins at the beginning of times.
#' @param parameters List of chemical parameters, as output by 
#' parameterize_pbtk function. Overrides chem.name and chem.cas.
#' @param model Specified model to use in simulation: "pbtk", "3compartment",
#' "3compartmentss", "1compartment", "schmitt", ...
#' @param route String specification of route of exposure for simulation:
#' "oral", "iv", "inhalation", ...
#' @param dosing List of dosing metrics passed to solver for a given model,
#' which must at least include entries with names "initial.dose", 
#' "doses.per.day", "daily.dose", and "dosing.matrix". The "dosing.matrix" can 
#' be used for more precise dose regimen specification, and is a matrix
#' consisting of two columns or rows named "time" and "dose" which contain the
#' time and amount, in mg/kg BW, of each dose. If none of the namesake entries 
#' of the dosing list is set to a non-NULL value, solve_model uses a default
#' dose of 1 mg/kg BW along with the dose type (add/multiply) specified for 
#' a given route (e.g. add the dose to gut lumen for oral route)
#' @param days Simulated period. Default 10 days. 
#' @param tsteps The number of time steps per hour. Default of 4. 
#' @param initial.values Vector containing the initial concentrations or
#' amounts of the chemical in specified tissues with units corresponding to
#' output.units.  Defaults are zero.
#' @param plots Plots all outputs if true.
#' @param suppress.messages Whether or not the output message is suppressed.
#' @param species Species desired (models have been designed to be
#' parameterized for some subset of the following species: "Rat", "Rabbit", 
#' "Dog", "Mouse", or default "Human").
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
#' @param monitor.vars Which variables are returned as a function of time. 
#' Default values of NULL looks up variables specified in modelinfo_MODEL.R
#' @param minimum.Funbound.plasma Monte Carlo draws less than this value are set 
#' equal to this value (default is 0.0001 -- half the lowest measured Fup in our
#' dataset)
#' @param parameterize.arg.list Additional parameterized passed to the model
#' parameterization function.
#' 
#' @return A matrix of class deSolve with a column for time(in days), each
#' compartment, the area under the curve, and plasma concentration and a row
#' for each time point.
#' 
#' @author John Wambaugh and Robert Pearce
#' 
#' @references Pearce, Robert G., et al. "Httk: R package for high-throughput
#' toxicokinetics." Journal of statistical software 79.4 (2017): 1.
#' 
#' @keywords Solve
#' 
#' @export solve_model
#'
#' @useDynLib httk
#'
#' @importFrom deSolve ode
solve_model <- function(chem.name = NULL,
                    chem.cas = NULL,
                    dtxsid = NULL,
                    times=NULL,
                    parameters=NULL,
                    model=NULL,
                    route="oral",
                    dosing=NULL,
                    days=10,
                    tsteps = 4, # tsteps is number of steps per hour
                    initial.values=NULL,
                    plots=FALSE,
                    monitor.vars=NULL,
                    suppress.messages=FALSE,
                    species="Human",
                    output.units='uM',
                    method="lsoda",rtol=1e-8,atol=1e-12,
                    recalc.blood2plasma=FALSE,
                    recalc.clearance=FALSE,
                    adjusted.Funbound.plasma=TRUE,
                    minimum.Funbound.plasma=0.0001,
                    parameterize.arg.list=list(
                      default.to.human=FALSE,
                      clint.pvalue.threshold=0.05,
                      restrictive.clearance = T,
                      regression=TRUE),
                    ...)
{
# Handy string manipulation functions for processing variable names that adhere
# to our naming conventions:
  lastchar <- function(x){substr(x, nchar(x), nchar(x))}
  firstchar <- function(x){substr(x, 1,1)}
  

# We need to describe the chemical to be simulated one way or another:
  if (is.null(chem.cas) & 
      is.null(chem.name) & 
      is.null(dtxsid) &
      is.null(parameters)) 
    stop('Parameters, chem.name, chem.cas, or dtxsid must be specified.')
  
  if (is.null(model)) stop("Model must be specified.")


# We need to know model-specific information (from modelinfo_[MODEL].R]) 
# to set up the solver:
  if (is.null(model)) stop("Model must be specified.")
  model <- tolower(model)
  if (!(model %in% names(model.list)))            
  {
    stop(paste("Model",model,"not available. Please select from:",
      paste(names(model.list),collapse=", ")))
  } else {
    #Set more convenient names for various model-related variables (e.g. route
    #of exposure) stored in the list of lists, model.list, compiled in the
    #various models' associated "modelinfo_xxx.R" files:
    
# Model parameter names:
    param_names <- model.list[[model]]$param.names
# Available exposure routes:
    model_routes <- model.list[[model]]$routes
# name of function that generates the model parameters:
    parameterize_function <- model.list[[model]]$parameterize.func
# allowable names of units for the model that are based on amounts (e.g., umol, mg)
    allowed_units <- model.list[[model]]$allowed.units
# the names of the state variables of the model (so far, always in units of 
# amounts)
    state.vars <- model.list[[model]]$state.vars
 # name of function that initializes the compiled model code:
    initialize_compiled_function <- model.list[[model]]$compiled.init.func
# name(s)s of the R parameters needed to initialize the compiled model params:
    Rtosolvermap <- model.list[[model]]$Rtosolvermap
# Function that transforms the parameters to those needed by the solver:
    compiled_parameters_init <- model.list[[model]]$compiled.parameters.init
# name(s) of the compiled model parameters that control the solver:
    compiled_param_names <- model.list[[model]]$compiled.param.names
# name of the function that calculates the derivative:
    derivative_function <- model.list[[model]]$derivative.func
# ordered names of the outputs from the derivative function:
    derivative_output_names <- model.list[[model]]$derivative.output.names
# calculate the number of outputs from the derivative function:
    num_outputs <- length(derivative_output_names)    
# Which variables to track by default (should be able to build this from
# state vars and outputs):
    default.monitor.vars <- model.list[[model]]$default.monitor.vars
# If using forcing function for dosing, specify name of this function as 
# it appears in model's associated .c file for passing to integrator
    initforc <- model.list[[model]]$initforc
  }
  

### ERROR CHECKING

  # Make sure the model is dynamic:
  if (is.null(derivative_function)) stop(paste("Model",model,"is not solvable \
  (Derivative function is not defined)"))

  # Make sure that the dose route is sufficiently described:
  if (is.null(route))
  {
    stop ("Route must be specified")
  } else if (!route %in% model_routes)
  {
    stop(paste("Model",model,"dose not have route",route))
  } else {
    dose.var <- model.list[[model]]$dose.variable[[route]]
    # We need to know which compartment gets the dose and how it receives it
    # (deSolve allows add, replace, or multiply:
    if (is.null(dose.var))
    {
      stop(paste("Must specify variable to receive dose for model",model,"and route",
        route))
    } else {
      dose.type <- model.list[[model]]$dose.type[[route]]
      if (is.null(dose.type))
      {
        stop(paste("Must specify how the variable is changed for model",model,"and route",
          route))
      }
    }
  }
  
  # Check the units that we want the solver to use:   
  if (!(tolower(output.units) %in% allowed_units)) stop(
    paste("Units",output.units,"unavailable for model",model))
  
### MODEL PARAMETERS FOR R

# Make sure we have all the parameters necessary to describe the chemical (we don't
# necessarily need all parameters associated with a given model to do this:)
  if (is.null(parameters))
  {
    parameters <- do.call(parameterize_function,c(list(
      chem.cas=chem.cas,
      chem.name=chem.name,
      dtxsid=dtxsid,
      species=species,
      suppress.messages=suppress.messages,
      adjusted.Funbound.plasma=adjusted.Funbound.plasma,
      minimum.Funbound.plasma=minimum.Funbound.plasma),parameterize.arg.list)) 
  } else {
    if (!all(param_names %in% names(parameters)))
    {
      stop(paste("Missing parameters:",
        paste(compiled_param_names[which(!param_names %in% 
        names(parameters))],collapse=', '),
        ". Use parameters from ",parameterize_function,".",sep="")) 
    }
  }
  
  # Rblood2plasma depends on hematocrit, Krbc2pu, and Funbound.plasma. If those
  # have been updated by Monte Carlo, it may be that Rblood2plasma needs to be
  # recalculated:
  if (recalc.blood2plasma) parameters[['Rblood2plasma']] <- 
    1 - 
    parameters[['hematocrit']] + 
    parameters[['hematocrit']] * parameters[['Krbc2pu']] * 
    parameters[['Funbound.plasma']]
  Rblood2plasma <- parameters[["Rblood2plasma"]]
  
  # The unscaled hepatic clearance depends on many parameters that could be
  # changed by Monte Carlo simulation. This code recalculates it if needed:
  if (recalc.clearance)
  {
  # Do we have all the parameters we need:
    if (!all(param_names%in%names(parameters)))
    {
      if (is.null(chem.name) & is.null(chem.cas)) 
        stop('Chemical name or CAS must be specified to recalculate hepatic clearance.')
      ss.params <- parameterize_steadystate(chem.name=chem.name,chem.cas=chem.cas)
    }
    ss.params[[names(ss.params) %in% names(parameters)]] <- 
      parameters[[names(ss.params) %in% names(parameters)]]
    parameters[['Clmetabolismc']] <- calc_hep_clearance(parameters=ss.params,
      hepatic.model='unscaled',
      suppress.messages=TRUE)
  }
  
  # If the hepatic metabolism is not slowed by plasma protein binding (non-
  # restrictive clearance)  
  if (!parameterize.arg.list$restrictive.clearance) parameters$Clmetabolismc <- 
    parameters$Clmetabolismc / parameters$Funbound.plasma
  
  # If there is not an explicit liver we need to include a factor for first-
  # pass metabolism:
  if (!is.null(model.list[[model]]$do.first.pass))
    if (model.list[[model]]$do.first.pass)
  {
    parameters$Fgutabs <- parameters$Fgutabs * parameters$hepatic.bioavailability
  }

### STATE VECTOR

# create the default initial state vector:
  state <- rep(0,length(state.vars))
  names(state) <- state.vars
  
# Address case where initial conditions provided using argument initial.values:
  if (!is.null(initial.values)){
    for (this.compartment in names(initial.values))
    {
      # Are we doing concentrations?
      if (firstchar(this.compartment)=="C")
      {
        tissue <- substring(this.compartment, 2)
        state[paste("A",tissue,sep="")] <-
                            initial.values[[this.compartment]] *
                            parameters[[paste("V",tissue,"c",sep="")]] *
                            parameters[["BW"]]
      # Or amounts?
      } else if (firstchar(this.compartment)=="A")
      {
        state[this.compartment] <- initial.values[[this.compartment]]
      } else stop("Initital values must begin with \"C\" or \"A\" 
                  to denote concentrations or amounts, respectively.")
    }
  }
### SIMULATION TIME

# Small time delta for plotting changes:
  SMALL.TIME <- 1e-5  
  
# We need to let the solver know which time points we want:
  if (is.null(times)) times <- round(seq(0, days, 1/(24*tsteps)),8)
  times <- sort(times)
  start.time <- times[1]
  end.time <- times[length(times)]

  # We add a time point 1e-5 later than the beginning to make the plots look
  # better. Use 'unique' function to remove redundant times that may have
  # been generated using 'round'
  times <- sort(unique(c(times,start.time+SMALL.TIME)))

### DOSING

  # Parse the dosing parameter into recognized values:
  if (!all(unique(c("initial.dose","dosing.matrix","daily.dose","doses.per.day",
    model.list[[model]]$dosing.params)) %in% 
    names(dosing))) stop("Dosing descriptor(s) missing")
  
  #Capture forcings argument from args passed to solve_model in ellipsis form,
  #in case a model is set to make use of the 'forcings' argument to the ode
  #function in dosing (which should in turn be passed with a name of
  # "forcings"):
  forcings <- list(...)$forcings #NULL if forcings not specified
  
  #Provide default, somewhat arbitrary, single-time dosing case of
  #1 mg/kg BW for when no dosing is specified by user.
  if (all(unlist(lapply(dosing, is.null))) & is.null(forcings)) 
    dosing$initial.dose <- 1 #mg/kg BW

  #Scale dose into intended units
  dosing <- scale_dosing(dosing,parameters,route,output.units)
  initial.dose <- dosing$initial.dose
  dosing.matrix <- dosing$dosing.matrix
  daily.dose <- dosing$daily.dose
  doses.per.day <- dosing$doses.per.day

# Add the first dose:
  if (!is.null(initial.dose))
  {
     if (!dose.type %in% c("add","replace","multiply"))
       stop("Dose type must be \"add\", \"replace\", or \"multiply\".")
       
     if (dose.type=="add") state[dose.var] <- state[dose.var] + initial.dose
     else if (dose.type=="multiply") state[dose.var] <- state[dose.var] *
       initial.dose
     else state[dose.var] <- initial.dose
  }

# eventdata is the deSolve object specifying "events" where the simulation 
# stops and variables are potentially changed. We use this object to perform 
# any dosings beyond the initial dosing. 
  if (is.null(dosing.matrix) & is.null(doses.per.day) & is.null(daily.dose))
  {
# If we are simulating a single dose then we don't need eventdata:
    eventdata <- NULL
  } else {
# Either we are doing dosing at a constant interval:
    if (is.null(dosing.matrix))
    {
      if (is.null(daily.dose)) {stop("Must specify total
\"daily.dose\" when \"doses.per.day\" is not set to NULL.")
      }else if (is.null(doses.per.day)) {stop("Must specify total
          \"doses.per.day\" when \"daily.dose\" is not set to NULL.")
      }
      
      dose.times <- seq(start.time,
                        end.time-1/doses.per.day,
                        1/doses.per.day)
      dose.vec <- rep(daily.dose/doses.per.day, length(dose.times))
# Or a matrix of doses (first col time, second col dose) has been specified:
    } else { #Do some screening on the dosing.matrix's contents first.
      if (any(is.na(dosing.matrix))) stop("Dosing matrix cannot contain NA values")
      if (dim(dosing.matrix)[2]!=2) stop("Dosing matrix should be a matrix 
with two columns (time, dose).")
      dose.times <- dosing.matrix[,"time"]
      dose.vec <- dosing.matrix[,"dose"]
    }
    num.doses <- length(dose.times)
    eventdata <- data.frame(var=rep(dose.var,num.doses),
                            time = round(dose.times,8),
                            value = dose.vec, 
                            method = rep(dose.type,num.doses))
    #Update our times vector to include times of provided dosing events, as well as
    #the times of dosing events incremented by SMALL.TIME for visualization.
    times <- sort(unique(c(times,
    eventdata$time,
    eventdata$time+SMALL.TIME)))
  }  
  
### MODEL PARAMETERS FOR DESOLVE

  # Map the R parameters onto the names for the C code:
  for (this.param in names(Rtosolvermap)[!(names(Rtosolvermap) 
    %in% names(parameters))])
  {
    if (Rtosolvermap[[this.param]] %in% names(parameters))
      parameters[[this.param]] <- parameters[[Rtosolvermap[[this.param]]]]
    else stop(paste("Failed to find R parameter",Rtosolvermap[[this.param]],
      "to initialize parameter",this.param,"in the C code."))
  }
  
  # These parameters are presumably initialized by the compiled code, such as
  # parameters scaled from other parameters:
  for (this.param in compiled_param_names)
    if (!(this.param %in% names(parameters)))
      parameters[[this.param]] <- 0
  
  # Here we remove model parameters that are not needed by the C solver (via
# only passing those parameters in compiled_param_names) and add in any
# additional parameters calculated by the C code (such as body weight scaling):
  parameters <- .C(
    getDLLRegisteredRoutines("httk")[[".C"]][[compiled_parameters_init]]$address,
    as.double(parameters[compiled_param_names]),
    out=double(length(parameters[compiled_param_names])),
    as.integer(length(parameters[compiled_param_names])))$out
  names(parameters) <- compiled_param_names

### RUNNING DESOLVE
  
# We use the events argument with deSolve to do multiple doses:
  out <- ode(y = state, 
    times = times, 
    func=derivative_function,
    parms = parameters,
    method=method,
    rtol=rtol,
    atol=atol,
    dllname="httk",
    initfunc=initialize_compiled_function,
    nout=num_outputs,
    outnames=derivative_output_names,
    events=list(data=eventdata),
    initforc = initforc,
    ...)

# Cannot guarantee arbitrary precision for deSolve:
  out <- set_httk_precision(out)
  
### MODEL OUTPUT
  
# The monitored variables can be altered by the user:
  if (is.null(monitor.vars))
  {
    monitor.vars <- default.monitor.vars
  }
# However, we always include whatever compartment received the dose:  
  monitor.vars <- unique(c(dose.var,monitor.vars))
  if (any(!(monitor.vars%in%colnames(out))))
    stop("Some of the requested variables to monitor (monitor.vars) are not in the derivative_output_names.")
 
#Initialize string variable, 'out.amount', to represent units of amount in
#accordance with the concentration units stored in 'output.units'. This is useful
#for plotting and other warning messages sent to the user. 
  if (tolower(output.units) == 'um')
  {
    out.amount <- 'umol'
  } else out.amount <- 'mg'
  
# Make a plot if asked for it (not the default behavior):
  if (plots==TRUE)
  {
    
    #assemble a y-axis units vector to correspond to each entry in monitor.vars
    vars_monitored = length(monitor.vars)
    plot_units_vector = rep(NA, vars_monitored)
    
    for (var in 1:vars_monitored) {
      if (firstchar(monitor.vars[var]) == 'A') {
        if (substr(monitor.vars[var], start=1, stop=3) == 'AUC') {
          plot_units_vector[var] = paste(output.units,'* days')
        } else plot_units_vector[var] = out.amount
      } else if (firstchar(monitor.vars[var]) == 'C') {
        plot_units_vector[var] = output.units
      } else stop("State and output variables to be monitored must begin with
                  \"C\", \"A\", or \"AUC\" to denote concentrations, amounts,
                  or areas under the curve, respectively.")
    }
    
   
    graphics::plot(out, select=unique(c(monitor.vars,names(initial.values))),ylab = plot_units_vector, xlab = 'time (days)')
  } 
               
# Downselect to only the desired parameters:
  out <- out[,unique(c("time",monitor.vars,names(initial.values)))]
  class(out) <- c('matrix','deSolve')

# Document the values produced by the simulation:  
  if(!suppress.messages)
  {
    if (is.null(chem.cas) & is.null(chem.name))
    {
      
      cat("Amounts returned in",out.amount," and concentration returned in",output.units,"units.\n")
# If only a parameter vector is given it's good to warn people that they
# need to make sure that these values have been appropriately recalculated:
      if (!recalc.blood2plasma) warning("Rblood2plasma not recalculated. \
Set recalc.blood2plasma to TRUE if desired.") 
      if (!recalc.clearance) warning("Clearance not recalculated. \
Set recalc.clearance to TRUE if desired.") 
    } else {
      cat(paste(toupper(substr(species,1,1)),
        substr(species,2,nchar(species)),sep=""),
        "amounts returned in", out.amount,
        "and concentration returned in", output.units,
        "units.\n")
    }
    if (tolower(output.units) == 'mg/l')
    {
      cat("AUC is area under plasma concentration in mg/L * days units with \
Rblood2plasma = ",signif(Rblood2plasma,3),".\n",sep="")
    } else if(tolower(output.units) == 'um')
    {
      cat("AUC is area under plasma concentration in uM * days units with \
Rblood2plasma = ",signif(Rblood2plasma,3),".\n",sep="")
    } else cat("AUC is area under plasma concentration curve in ",output.units,
      " * days units with Rblood2plasma = ",signif(Rblood2plasma,3),".\n",sep="")
  }
    
  return(out) 
}