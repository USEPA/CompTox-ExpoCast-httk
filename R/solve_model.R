#' Solve_model
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
#' The compartments used in this model are the gutlumen, gut, liver, kidneys,
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
#' @param dose Amount of a single dose, mg/kg BW.  Overwrites daily.dose.
#' @param doses.per.day Number of doses per day.
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
#' @param dosing.matrix Vector of dosing times or a matrix consisting of two
#' columns or rows named "dose" and "time" containing the time and amount, in
#' mg/kg BW, of each dose.
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
#' @author John Wambaugh and Robert Pearce
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
#' @export solve_model
#' @useDynLib httk
#' @import deSolve
solve_model <- function(chem.name = NULL,
                    chem.cas = NULL,
                    times=NULL,
                    parameters=NULL,
                    model=NULL,
                    route="oral",
                    dosing=NULL,
                    days=10,
                    tsteps = 4, # tsteps is number of steps per hour
                    initial.values=NULL,
                    plots=F,
                    monitor.vars=c("Ametabolized","Atubules","Cplasma","AUC"),
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
                    ...)
{
  Aart <- Agut <- Agutlumen <- Alung <- Aliver <- Aven <- Arest <- NULL
  Akidney <- Cgut <- Vgut <- Cliver <- Vliver <- Cven <- Vven <- Clung <- NULL
  Vlung <- Cart <- Vart <- Crest <- Vrest <- Ckidney <- Vkidney <- NULL

# set up some local functions for easier string manipulation:
  lastchar <- function(x){substr(x, nchar(x), nchar(x))}
  firstchar <- function(x){substr(x, 1,1)}
  
# We need to describe the chemical to be simulated one way or another:
  if (is.null(chem.cas) & is.null(chem.name) & is.null(parameters)) 
    stop('Parameters, chem.name, or chem.cas must be specified.')
  
  if (is.null(model)) stop("Model must be specified.")

# We need to know model-specific information to set up the solver:
  if (!(model %in% names(model.list)))            
  {
    stop(paste("Model",model,"not available. Please select from:",
      paste(names(model.list),collapse=", ")))
  } else {
# name of function that generates the model parameters:
    parameterize_function <- model.list[[model]]$parameterize.func
# name(s)s of the parameters that control the solver:
    solver_param_names <- model.list[[model]]$solver.param.names
# name of the function that calculates the derivative:
    derivative_function <- model.list[[model]]$derivative.func
# ordered names of the outputs from the derivative function:
    derivative_output_names <- model.list[[model]]$derivative.output.names
# calculate the number of outputs from the derivitive function:
    num_outputs <- length(derivative_output_names)    
# allowable names of units for the model that are based on amounts (e.g., umol, mg)
    amount_units <- model.list[[model]]$amount.units
# allowable names of units for the model that are concentrations (e.g, uM, mg/L)
    conc_units <- model.list[[model]]$conc.units
# the names of the compartments in the model that are always in units of 
# amounts (e.g., gut)
    amount_compartments <- model.list[[model]]$amount.compartments
# the names of the compartments in the model that can be solved for in either
# units of concentration or amount:
    other_compartments <- model.list[[model]]$other.compartments
    initialize_R_function <- model.list[["pbtk"]]$R.init.func
    initialize_compiled_function <- model.list[["pbtk"]]$compiled.init.func
    model_routes <- model.list[["pbtk"]]$routes
  }

  # Make sure the model is dynamic:
  if (is.null(derivative_function)) stop(paste("Model",model,"is not solvable \
  (Derivative function is not defined)"))

  if (!all(unique(c("initial.dose","dosing.matrix","daily.dose","doses.per.day",
    model.list[[model]]$dosing.params)) %in% 
    names(dosing))) stop("Dosing descriptor(s) missing")
  initial.dose <- dosing$initial.dose
  dosing.matrix <- dosing$dosing.matrix
  daily.dose <- dosing$daily.dose
  doses.per.day <- dosing$doses.per.day

  # Make sure that the dose route is sufficiently described:
  if (is.null(route))
  {
    stop ("Route must be specified")
  } else if (!route %in% model_routes)
  {
    stop(paste("Model",model,"dose not have route",route))
  } else {
    dose.var <- model.list[["pbtk"]]$dose.variable[[route]]
    # We need to know which compartment gets the dose and how it receives it
    # (deSolve allows add, replace, or multiply:
    if (is.null(dose.var))
    {
      stop(paste("Must specify variable to receive dose for model",model,"and route",
        route))
    } else {
      dose.type <- model.list[["pbtk"]]$dose.type[[route]]
      if (is.null(dose.type))
      {
        stop(paste("Must specify how the variable is changed for model",model,"and route",
          route))
      }
    }
  }

# Make sure we have all the parameters necessary to describe the chemical (we don't
# necessarily need all parameters associated with a given model to do this:)
  if (is.null(parameters))
  {
    parameters <- do.call(parameterize_function,list(
      chem.cas=chem.cas,
      chem.name=chem.name,
      species=species,
      default.to.human=default.to.human,
      suppress.messages=suppress.messages,
      adjusted.Funbound.plasma=adjusted.Funbound.plasma,
      regression=regression,
      minimum.Funbound.plasma=minimum.Funbound.plasma)) 
  } else {
    if (!all(solver_param_names %in% names(parameters)))
    {
      stop(paste("Missing parameters:",
        paste(solver_param_names[which(!solve_param_names %in% 
        names(parameters))],collapse=', '),
        ".  Use parameters from",parameterize_function,".",sep="")) 
    }
  }
  
  if (recalc.blood2plasma) parameters[['Rblood2plasma']] <- 
    1 - 
    parameters[['hematocrit']] + 
    parameters[['hematocrit']] * parameters[['Krbc2pu']] * 
    parameters[['Funbound.plasma']]
  
  if (recalc.clearance)
  {
    if (is.null(chem.name) & is.null(chem.cas)) 
      stop('Chemical name or CAS must be specified to recalculate hepatic clearance.')
    ss.params <- parameterize_steadystate(chem.name=chem.name,chem.cas=chem.cas)
    ss.params[['million.cells.per.gliver']] <- parameters[['million.cells.per.gliver']]
    parameters[['Clmetabolismc']] <- calc_hepatic_clearance(parameters=ss.params,
      hepatic.model='unscaled',
      suppress.messages=T)
  } 
  
  if (!restrictive.clearance) parameters$Clmetabolismc <- 
    parameters$Clmetabolismc / parameters$Funbound.plasma
  
  parameters[['Fraction_unbound_plasma']] <- parameters[['Funbound.plasma']]


# We need to let the solver know which time points we want:
  if (is.null(times)) times <- round(seq(0, days, 1/(24*tsteps)),8)
  times <- sort(times)
  start.time <- times[1]
  end.time <- times[length(times)]
  
# Figure out what units we want the solver to use:   
# Check to see if we are going to work in concentrations:
  if (tolower(output.units) %in% conc_units)
    use.amounts <- F
# or if we are going to use amounts:
  else if (tolower(output.units) %in% amount_units)
      use.amounts <- T
# or if we need to throw an error:
  else stop(paste("Units",output.units,"unavailable for model",model))

  dosing <- scale_dosing(dosing,parameters,output.units)
 
  # Volume parameters that need to be scaled (linearly) by body weight are those
  # volume parameters that are provided on a L/kg body weght scale start with
  # "V" and end with "c":       ]
  scaled.volumes <- names(parameters)[firstchar(names(parameters))=="V" & 
    lastchar(names(parameters))=="c"]
  # Multiply scaled volumes by body weight:      
  for (this.vol in scaled.volumes)
  {
    eval(parse(text=paste(substr(this.vol,1,nchar(this.vol)-1), 
      '<-', 
      parameters[[this.vol]],'*', parameters[["BW"]]))) # L/kg BW -> L 
  }
  
# Generate the list of compartments to be used:
  StateVecNames <- c(amount_compartments, 
                     sapply(other_compartments,
                     function(x) paste("A",x,sep="")))
  if (use.amounts)
  {
    CompartmentsToInitialize <- StateVecNames
  } else {
    CompartmentsToInitialize <-c(StateVecNames,
                                 sapply(other_compartments,
                                 function(x) paste("C",x,sep="")))
  }

# create the state vector:
  state <- rep(0,length(StateVecNames))
  names(state) <- StateVecNames
  
# Set the initial conditions based on argument initial.values
  for (this.compartment in names(initial.values))
  {
    if (firstchar(this.compartment)=="C")
    {
      tissue <- substring(this.compartment, 2)
      state[paste("A",tissue,sep="")] <-
                            initial.values[[this.compartment]] *
                            parameters[[paste("V",tissue,sep="")]]
    } else if (firstchar(this.compartment)=="A")
    {
      state[this.compartment] <- initial.values[[this.compartment]]
    } else stop("Initital values must begin with \"C\" or \"A\".")
  }
  
# Add the first dose:
  if (!is.null(dosing$initial.dose))
  {
     if (!dose.type %in% c("add","replace","multiply"))
       stop("Dose type must be \"add\", \"replace\", or \"multiply\".")
       
     if (dose.type=="add") state[dose.var] <- state[dose.var] + 
       dosing$initial.dose
     else if (dose.type=="multiply") state[dose.var] <- state[dose.var] *
       dosing$initial.dose
     else state[dose.var] <-dosing$initial.dose
  }

#  state <- do.call(initialize_R_function,list(initial.state=state, 
#             dosing=c(list(route=route),dosing), 
#             use.amounts=use.amounts))
             
#  parameters <- initparms(parameters[param.names.pbtk.solver])
#  state <-initState(parameters,state)

  times <- sort(unique(c(times,start.time,start.time + 1e-8,end.time)))

# If we are simulating a single dose:
  if (!is.null(initial.dose))
  {
    eventdata <- NULL
  } else {
# Either we are doing dosing at a constant interval:
    if (is.null(dosing.matrix))
    {
      if (is.null(daily.dose)) stop("Must specifiy total \"daily.dose\" when \
\"doses.per.day\" is not set to NULL.")
      else if (!is.null(daily.dose)) 
      {
        stop("Must specifiy total \"doses.per.day\" when \"daily.dose\" is not set to NULL.")
      } 

      dose.times <- seq(start.time + 1/doses.per.day,
                        end.time-1/doses.per.day,
                        1/doses.per.day)
      each.dose <- daily.dose/doses.per.day
      eventdata <- data.frame(var=rep(dose.var,num.doses),
                              time = round(dose.times,8),
                              value = rep(each.dose,num.doses), 
                              method = rep(dose.type,num.doses))
    } else {
      if (any(is.na(dosing.matrix))) stop("Dosing mstrix cannot contain NA values")
      if (dim(dosing.matrix)[2]!=2) stop("Dosing matrix should be a matrix \
with two columns (time, dose).")
    
# Or a matrix of doses (first col time, second col dose) has been specified:
      dose.times <- dosing.matrix[,1]
      num.doses <- length(dose.times)
      eventdata <- data.frame(var=rep(dose.var,num.doses),
                              time = dosing.times,
                              value = dosing.matrix[,2],
                              method = rep(dose.types,num.doses))
    }    
    times <- sort(unique(times,dose.times))
  }  
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
    ...)
  
  if (plots==T)
  {
    plot(out, select=unique(c(monitor.vars,names(initial.values))))
  }              

  out <- out[,unique(c("time",monitor.vars,names(initial.values)))]
  class(out) <- c('matrix','deSolve')
  
  if(!suppress.messages)
  {
    if(is.null(chem.cas) & is.null(chem.name))
    {
      cat("Values returned in",output.units,"units.\n")
      if (!recalc.blood2plasma) warning('Rblood2plasma not recalculated.  Set recalc.blood2plasma to TRUE if desired.') 
      if (!recalc.clearance) warning('Clearance not recalculated.  Set recalc.clearance to TRUE if desired.') 
    } else cat(paste(toupper(substr(species,1,1)),substr(species,2,nchar(species)),sep=''),"values returned in",output.units,"units.\n")
    if (tolower(output.units) == 'mg')
    {
      cat("AUC is area under plasma concentration in mg/L * days units with Rblood2plasma =",parameters[['Rblood2plasma']],".\n")
    } else if(tolower(output.units) == 'umol')
    {
      cat("AUC is area under plasma concentration in uM * days units with Rblood2plasma =",parameters[['Rblood2plasma']],".\n")
    } else cat("AUC is area under plasma concentration curve in",output.units,"* days units with Rblood2plasma =",parameters[['Rblood2plasma']],".\n")
  }
    
  return(out) 
}
