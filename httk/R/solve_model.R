#' Solve_model
#' 
#' solve_model is designed to accept systematized metadata (provided by the
#' model.list defined in the modelinfo files) for a given 
#' toxicokinetic model, including names of variables, parameterization
#' functions, and key units, and use it along with chemical information
#' to prepare an ode system for numerical solution over time of the amounts
#' or concentrations of chemical in different bodily compartments of a given
#' species (either "Rat", "Rabbit", "Dog", "Mouse", or default "Human").
#' 
#' Dosing values with certain acceptable associated input.units (like mg/kg BW)
#' are configured to undergo a unit conversion. All model simulations are 
#' intended to run with units as specifed by "compartment.units" in the 
#' model.list (as defined by the modelinfo files).
#' 
#' The 'dosing' argument includes all parameters needed to describe exposure
#' in terms of route of administration, frequency, and quantity short of 
#' scenarios that require use of a more precise forcing function. If the dosing
#' argument's namesake entries are left NULL, solve_model defaults to a
#' single-time dose of 1 mg/kg BW according to the given dosing route and 
#' associated type (either add/multiply, for example we typically add a dose to 
#' gut lumen 
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
#' (NOTE: The 'default.to.human' specification should be included as part of the
#' arguments listed in 'parameterize.arg.list'.)
#'  
#' For both plotting purposes and helping the numerical equation solver, it is
#' helpful to specify that time points shortly before and after dosing are 
#' included. This function automatically add these points, and they are returned
#' to the user unless the times argument is used, in which case only the time
#' points specified by that argument are provided.
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
#' @param times Optional time sequence for specified number of output times (in days) to be returned by the function.
#' The model is solved explicitly at the time sequence specified. Dosing sequence begins at the first time provided.
#' 
#' @param parameters List of chemical parameters, as output by 
#' parameterize_pbtk function. Overrides chem.name and chem.cas.
#' 
#' @param model Specified model to use in simulation: "pbtk", "3compartment",
#' "3compartmentss", "1compartment", "schmitt", ...
#' 
#' @param route String specification of route of exposure for simulation:
#' "oral", "iv", "inhalation", ...
#' 
#' @param dosing List of dosing metrics used in simulation, which includes
#' the namesake entries of a model's associated dosing.params. In the case
#' of most httk models, these should include "initial.dose", "doses.per.day", 
#' "daily.dose", and "dosing.matrix". The "dosing.matrix" is used for more
#' precise dose regimen specification, and is a matrix consisting of two
#' columns or rows named "time" and "dose" containing the time and amount of 
#' each dose. If none of the namesake entries of the dosing list is set to a
#' non-NULL value, solve_model uses a default initial dose of 1 mg/kg BW along with the 
#' dose type (add/multiply) specified for a given route (for example, add the dose to gut
#' lumen for oral route)
#' 
#' @param days Simulated period. Default 10 days. 
#' 
#' @param tsteps The number of time steps per hour. Default of 4. 
#' 
#' @param initial.values Vector of numeric values containing the initial
#' concentrations or amounts of the chemical in specified tissues with units
#' corresponding to those specified for the model outputs. Default values are zero.
#' 
#' @param initial.value.units Vector of character strings containing the units
#' corresponding to 'initial.values' specified for the model outputs.
#' Default is assuming the units match expected compartment units for the model.
#' 
#' @param plots Plots all outputs if true.
#' 
#' @param suppress.messages Whether or not the output messages are suppressed.
#' 
#' @param species Species desired (models have been designed to be
#' parameterized for some subset of the following species: "Rat", "Rabbit", 
#' "Dog", "Mouse", or default "Human").
#' 
#' @param input.units Input units of interest assigned to dosing. Defaults
#' to mg/kg BW, in line with the default dosing scheme of a one-time dose of
#' 1 mg/kg in which no other dosing parameters are specified.
#' 
#' @param output.units Output units of interest for the compiled components.
#' Defaults to NULL, and will provide values in model units if unspecified.
#' 
#' @param method Method used by integrator (deSolve).
#' 
#' @param rtol Argument passed to integrator (deSolve).
#' 
#' @param atol Argument passed to integrator (deSolve).
#' 
#' @param recalc.blood2plasma Recalculates the ratio of the amount of chemical
#' in the blood to plasma using the input parameters, calculated with
#' hematocrit, Funbound.plasma, and Krbc2pu.
#' 
#' @param recalc.clearance Recalculates the the hepatic clearance
#' (Clmetabolism) with new million.cells.per.gliver parameter.
#' 
#' @param adjusted.Funbound.plasma Uses adjusted Funbound.plasma when set to
#' TRUE along with partition coefficients calculated with this value.
#' 
#' @param restrictive.clearance Protein binding not taken into account (set to
#' 1) in liver clearance if FALSE.
#' 
#' @param ... Additional arguments passed to the integrator.
#' 
#' @param monitor.vars Which variables are returned as a function of time. 
#' Default values of NULL looks up variables specified in modelinfo_MODEL.R
#' 
#' @param minimum.Funbound.plasma Monte Carlo draws less than this value are set 
#' equal to this value (default is 0.0001 -- half the lowest measured Fup in our
#' dataset)
#' 
#' @param parameterize.arg.list Additional parameterized passed to the model
#' parameterization function.
#' 
#' @return A matrix of class deSolve with a column for time(in days), each
#' compartment, the area under the curve, and plasma concentration and a row
#' for each time point.
#' 
#' @author John Wambaugh, Robert Pearce, Miyuki Breen, Mark Sfeir, and
#' Sarah E. Davidson
#' 
#' @references
#' \insertRef{pearce2017httk}{httk}
#' 
#' @keywords Solve
#'
#' @examples
#' # The various "solve_x" functions are wrappers for solve_model:
#' head(solve_pbtk(chem.name="Terbufos", days=1))
#'
#' head(solve_model(chem.name="Terbufos",model="pbtk",
#'                  days=1,
#'                  dosing=list(
#'                    initial.dose = 1, # Assume dose is in mg/kg BW/day  
#'                    doses.per.day=NULL,
#'                    dosing.matrix = NULL,
#'                    daily.dose = NULL)))
#'
#' # A dose matrix specifies times and magnitudes of doses:
#' dm <- matrix(c(0,1,2,5,5,5),nrow=3)
#' colnames(dm) <- c("time","dose")
#'
#' solve_pbtk(chem.name="Methenamine",
#'            dosing.matrix=dm,
#'            dose=NULL,
#'            days=2.5,
#'            daily.dose=NULL)
#' 
#' solve_model(chem.name="Methenamine",
#'             model="pbtk",
#'             days=2.5,
#'             dosing=list(
#'               initial.dose =NULL,
#'               doses.per.day=NULL,
#'               daily.dose=NULL,
#'               dosing.matrix=dm))
#' 
#' solve_model(chem.name="Besonprodil",
#'             model="pbtk",
#'             days=2.5,
#'             dosing=list(
#'               initial.dose=NULL,
#'               doses.per.day=4,
#'               daily.dose=1,
#'               dosing.matrix=NULL))
#'   
#' solve_pbtk(chem.name="Besonprodil",
#'            daily.dose=1,
#'            dose=NULL,
#'            doses.per.day=4,
#'            days=2.5)
#' 
#' @export solve_model
#'
#' @useDynLib httk
#'
#' @importFrom deSolve ode
#' @importFrom purrr compact 
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
                    initial.values=NULL, #vector of initial values (numeric)
                    initial.value.units=NULL, # vector of units for initial values (character)
                    plots=FALSE,
                    monitor.vars=NULL,
                    suppress.messages=FALSE,
                    species="Human",
                    input.units="mg/kg", # units for 'dosing'
                    output.units=NULL, # needs to be a named list or named vector for desired units corresponding to compartments
                    method=NULL,
                    rtol=1e-5,
                    atol=1e-5,
                    recalc.blood2plasma=FALSE,
                    recalc.clearance=FALSE,
                    restrictive.clearance=TRUE,
                    adjusted.Funbound.plasma=TRUE,
                    minimum.Funbound.plasma=0.0001,
                    parameterize.arg.list=list(),
                    small.time = 1e-4, 
                    ...)
{
#R CMD CHECK throws notes about "no visible binding for global variable", for
  #each time a data.table column name is used without quotes. To appease R CMD
  #CHECK, a variable has to be created for each of these column names and set to
  #NULL. Note that within the data.table, these variables will not be NULL! Yes,
  #this is pointless and annoying.
  Cinhppmv <- NULL
  #End R CMD CHECK appeasement.
  
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
    model_routes <- names(model.list[[model]]$routes)
# name of function that generates the model parameters:
    parameterize_function <- model.list[[model]]$parameterize.func
# allowable names of output units for the model for a given route that are
# based on or derived from amounts (e.g., umol, mg, ppmv)
    allowed_units_output <- model.list[[model]]$allowed.units.output[[route]]
# Default set of units assigned to correspond to each of the "outputs" of 
# the model system  
    compartment_units <- model.list[[model]]$compartment.units
    if (is.null(compartment_units)) stop(
"Units must be set for each compartment in the modelinfo__[MODEL].R file")
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
# Input variables (used with forcings):
    input.vars <- model.list[[model]]$input.var.names
# If using forcing function for dosing, specify name of this function as 
# it appears in model's associated .c file for passing to integrator
    initforc <- model.list[[model]]$forcings.materials[["initforc"]]
# Using a forcings series also requires specifying fcontrol argument
    fcontrol <- model.list[[model]]$forcings.materials[["fcontrol"]]
    # State of Compound in Various Compartments
    compartment_state <- model.list[[model]]$compartment.state
    # Set the ODE solver:
    if (is.null(method))
    {
      if (!is.null(model.list[[model]]$default.solver.method))
      {
        method <- model.list[[model]]$default.solver.method
      } else method <- "lsoda"
    }
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
    # We need to know which compartment gets the dose and 
    dose.var <- model.list[[model]]$routes[[route]][["entry.compartment"]]
    if (!(dose.var %in% names(compartment_units))) stop(paste("Compartment",
      dose.var, "specified as entry.compartment for route", route, "
is not among those listed in compartment.units in modelinfo file for model",
      model))
    # The dose should be in whatever units the model actually uses:
    dose.units <- compartment_units[dose.var]
    if (is.null(dose.var))
    {
      stop(paste("Must specify variable to receive dose for model",model,"and route",
        route))
    } else if (is.null(dose.units))
    {
      stop(paste("Must specify target dose units for model",model,"compartment",
        dose.var))
    } else {
      # We need to know how the compartment dose.var receives the dose
      # (deSolve allows add, replace, or multiply:
      dose.type <- model.list[[model]]$routes[[route]][["dose.type"]]
      if (is.null(dose.type))
      {
        stop(paste("Must specify how the variable is changed for model",model,"and route",
          route))
      }
    }
    # The names of the supported dosing parameters, the names of which must be 
    # specified to solve_model, even if corresponding to NULL values
    dosing_params <- model.list[[model]]$routes[[route]][["dosing.params"]]
  }
  
  #Make basic checks for variable name convention observance in objects of
  #interest:
#  if (any(!firstchar(names(compartment_units)) %in% c("A","C"))) {
#    stop("The names of compartment_units for model, ", model, " must begin with
#          \"A\" for amounts or areas under the curve (AUC), or \"C\" for
#          concentrations.")
#  } else if (any(!firstchar(names(state.vars)) %in% c("A","C"))) {
#    stop("The names of state.vars for model, ", model, " must begin with
#          \"A\" for amounts or areas under the curve (AUC), or \"C\" for
#          concentrations.")
#  } 
  #The compartment_units entries should correspond to the names of some entry in
  #the derivative_output_names and/or state.vars
  if (any(!names(compartment_units) %in% c(derivative_output_names,
                                           state.vars,
                                           input.vars))) 
  {
    print(paste("Compartment(s)",
      paste(names(compartment_units)[!names(compartment_units) %in% 
      c(derivative_output_names, state.vars, input.vars)],collapse=", "),
      "not found."))
    stop("
The names of the compartments in compartment.units must be among those named
in state.vars, derivative.output.names, and input.var.names in the modelinfo
file for model ", model)
  } else if (!all(derivative_output_names %in% names(compartment_units))){
    stop("
Each entry in derivative_output_names should have a corresponding units 
specification in compartment_units for model ", model)
  }
  
### MODEL PARAMETERS FOR R

# Make sure we have all the parameters necessary to describe the chemical (we don't
# necessarily need all parameters associated with a given model to do this:)
  if (is.null(parameters))
  {
    parameters <- do.call(parameterize_function, args=purrr::compact(c(list(
      chem.cas=chem.cas,
      chem.name=chem.name,
      dtxsid=dtxsid,
      species=species,
      suppress.messages=suppress.messages,
      adjusted.Funbound.plasma=adjusted.Funbound.plasma,
      minimum.Funbound.plasma=minimum.Funbound.plasma),parameterize.arg.list))) 
  } else {
    if (!all(param_names %in% names(parameters)))
    {
      stop(paste("Missing parameters:",
        paste(param_names[which(!param_names %in% 
        names(parameters))],collapse=', '),
        ". Use parameters from ",parameterize_function,".",sep="")) 
    }
  }

  # Molecular weight for general use:
  MW <- parameters[["MW"]]
  
  if (any(!tolower(compartment_units)
                 %in% tolower(allowed_units_output))) {
    stop("The compartment.units list specified for the model outputs contains
          units not allowed per allowed.units.output for model ", model)
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
      restrictive.clearance = restrictive.clearance,
      suppress.messages=TRUE)
  }
  
  # If the hepatic metabolism is not slowed by plasma protein binding (non-
  # restrictive clearance)  
# This should be handled in calc_hep_clearance above
#  if (!restrictive.clearance) parameters$Clmetabolismc <- 
#    parameters$Clmetabolismc / parameters$Funbound.plasma
  
  # If there is not an explicit liver we need to include a factor for first-
  # pass metabolism:
  if (!is.null(model.list[[model]]$do.first.pass))
    if (model.list[[model]]$do.first.pass)
  {
    parameters$Fabsgut <- parameters$Fabsgut * parameters$hepatic.bioavailability
  }

### STATE VECTOR

# create the default initial state vector:
  state <- rep(0,length(state.vars))
  names(state) <- state.vars
  
# Address case where initial conditions provided using argument initial.values:
  if (!is.null(initial.values))
  {
    #-- Initial Values Checks --#
    # Check if 'initial.values' is a named vector linking values with
    # model components
    if(is.null(names(initial.values))){
      stop("The initial.values for the model, ",
           model,
           ", should correspond to model state variable compartments.\n    ",
           "Allowed compartments include: ",
           paste(state.vars,collapse = ", "),".\n    ",
           "These should be amounts not concentrations.")
    }
    
    # Check if 'initial.values' is a numeric vector
    if(!is.numeric(initial.values)){
      stop("The initial.values argument is of class '",class(initial.values),
           "' and should be 'numeric'.")
    }
    
    # Check if 'initial.values' contains a subset of state vars.
    # If the initial.values is not a subset of state vars reply with an error.
    if(all(!(names(initial.values)%in%state.vars))){
      stop("The initial.values for the model, ",
           model,
           ", should correspond to model state variable compartments.\n    ",
           "Allowed compartments include: ",
           paste(state.vars,collapse = ", "),".\n    ",
           "These should be amounts not concentrations.")
    }
    
    # Obtain any 'initial.values' that are not state.vars
    non.state.vars <- which(!(names(initial.values)%in%state.vars))
    # Check if there are any non.state.vars
    if(length(non.state.vars)!=0){
      if (!suppress.messages)
        warning("Additional unnecessary elements were included in the ",
              "initial.values -- namely ",
              paste(names(initial.values)[non.state.vars],collapse = ", "),
              ".\n    ",
              "These variables were removed from the initial conditions.")
      
      # Subset to only state.vars
      initial.values <- initial.values[-non.state.vars]
    }
    
    #-- Initial Value Units --#
    #Check if there are units specified for the initial.values provided
    #if no units are specified assume the user is giving initial values
    #in default units expected by the model and provide a warning message
    if(is.null(initial.value.units)){
      #units for only compartments specified in the initial value
      #(assume default) 
      initial.value.units <- compartment_units[names(initial.values)]
      
      #provide warning of assumption
      if (!suppress.messages)
        warning("No units were specified for the provided initial values. ",
              "Assume initial values are in default model units:\n    ",
              paste(paste(
                names(initial.value.units),
                initial.value.units,
                sep = " = "
              ),collapse = "\n    "))
    }else{
      #Check if 'initial.value.units' is a named vector linking units with
      #model components
      if(is.null(names(initial.value.units))){
        stop("The initial.value.units for the model, ",
             model,
             ", should correspond to model state variable compartments.\n    ",
             "Allowed compartments include: ",
             paste(state.vars,collapse = ", "),".\n    ",
             "These should be amounts not concentrations.")
      }
      
      #Check if 'initial.value.units' is a "character" vector
      if(!is.character(initial.value.units)){
        stop("The initial.value.units argument is of class '",
             class(initial.value.units),
             "' and should be 'character'.")
      }
      
      # Check if 'initial.value.units' contains units for all components
      # in 'initial.values'.
      # If the 'initial.value' vector is not a subset of 'initial.value.units'
      # reply with an error.
      if(any(!(names(initial.values)%in%names(initial.value.units)))){
        stop("The initial.value.units for the model, ",
             model,
             ", should correspond to model initial.values.\n    ",
             "Allowed compartments include: ",
             paste(state.vars,collapse = ", "),".\n    ",
             "These should be amounts not concentrations.")
      }else{
        #Obtain any 'initial.value.units' that are not in 'initial.values'
        non.state.vars <- which(!(names(initial.value.units)%in%names(initial.values)))
        
        #Check if there are any non.state.vars
        if(length(non.state.vars)!=0){
          if (!suppress.messages)
             warning("Additional unnecessary elements were included in the",
                  "initial.values -- namely ",
                  paste(names(initial.value.units)[non.state.vars],collapse = ", "),
                  ".\n    ",
                  "These variables were removed from the initial conditions.")
        }
        
        #Subset to only components in 'initial.values' 
        initial.value.units <- initial.value.units[names(initial.values)]
      }
    }
    
    #Obtain the scaling factor for each of the provided initial conditions
    #even if trivially 1.
    #It is assumed the units of this.compartment in initial.values is
    #a simple "tissue" volume scaling away from the units specified for
    #the amount in compartment_units.
    units_conversion_factor <- sapply(names(initial.values),function(this.compartment){
      #Obtain units for conversion
      # provided initial value units
      given.units <- initial.value.units[this.compartment]
      # required model value units
      model.units <- compartment_units[this.compartment]
      # compartment state for unit conversion
      #
      # First check if value "all" is used for a state (that is, all 
      # compartments are the same state of matter):
      if (all(tolower(compartment_state[[1]])%in%"all"))
      {
        model.compartment.state <- names(compartment_state)[1]
      } else {
        model.compartment.state.check <- unlist(lapply(compartment_state,function(x){this.compartment%in%x}))
        if(any(model.compartment.state.check)){
          model.compartment.state <- names(compartment_state)[which(model.compartment.state.check==TRUE)]
        }else{
          warning(paste0(this.compartment,
                         " state is not specified in the model.list for ",model,
                         ". Will assume the default, i.e. 'liquid'."))
          model.compartment.state <- "liquid"
        }
      }
      
      # # (1) get all tissues with volume from param_names
      # all.tissue.vols <- param_names[grep(param_names,pattern = "^V.+c$")]
      # # (2) get the potential volume tissue string with string update
      # tissue.string <- paste0(stringr::str_replace(this.compartment,
      #                                              pattern = "^A|^C",
      #                                              replacement = "V"),"c")
      # # (3) Check if the tissue string is in all.tissue.vols,
      # #     yes then get the provided parameter
      # #     no then return null value
      # #   (a) Check if the length of tissues with a volume in the parameter names is 0.
      # if(length(all.tissue.vols)==0){
      #   tissue.vol <- NULL
      # }else if(tissue.string%in%all.tissue.vols){
      #   tissue.vol <- parameters[[tissue.string]]*parameters[["BW"]]
      # }else{
      #   tissue.vol <- NULL
      # }
      
      #Use the units for this compartment from compartment_units to
      #get a units conversion factor, even if trivially 1
      out.unit.conversion <- convert_units(
        input.units=given.units,
        output.units=model.units,
        MW = MW,
        state = model.compartment.state) # ,
        # vol = tissue.vol)
      
      return(out.unit.conversion)
    })
    
    #...and apply units conversion factor to get the state value in 
    #umol-based units.
    state[names(initial.values)] <- initial.values*units_conversion_factor
    # state <- initial.values * units_conversion_factor
  }
  
### SIMULATION TIME

  # Save the requested times so that we only return those:
  requested.times <- times

  # We need to let the solver know which time points we want:
  if (is.null(times)) times <- seq(0, days, 1/(24*tsteps))
  times <- sort(times)
  start.time <- times[1]
  end.time <- times[length(times)]

  # We add a time point 1e-4 later than the beginning to make the plots look
  # better. Use 'unique' function to remove redundant times that may have
  # been generated using 'round'
  times <- sort(unique(c(times,start.time+small.time)))

### DOSING

  #Sanitize the input.units
  input.units <- tolower(input.units)
  
  # Set default dose:
  if (is.null(dosing)) dosing=list(
                                   initial.dose = 1,
                                   doses.per.day=NULL,
                                   dosing.matrix = NULL,
                                   daily.dose = NULL
                                   )
  
  # Make sure we have all specified dosing parameters for the model
  # accounted for
  if (!all(unique(dosing_params) %in% names(dosing)))
    stop(paste("Dosing descriptor(s) missing for route",
               route, "in model",
               model, ":",
               unique(dosing_params)[!(unique(dosing_params) 
                                       %in% names(dosing))]))
 
  # Warn if unnecessary dosing_param provided:
  if (any(!(names(dosing) %in% dosing_params)))
    warning(paste("The following dosing descriptor(s) ignored for route",
               route, "in model",
               model, ":",
               names(dosing)[!(names(dosing) 
                                       %in% dosing_params)]))
   
  #Provide default, somewhat arbitrary, single-time dosing case of
  #1 mg/kg BW for when no dosing is specified by user.
  if (all(as.logical(lapply(dosing, is.null)))) dosing$initial.dose <- 1 

  #### NEED TO CHECK/POSSIBLY UPDATE!!! ####
  # volume for the entry tissue
  # (1) get all tissues with volume from param_names
  all.tissue.vols <- param_names[grep(param_names,pattern = "^V.+c$")]
  # (2) get the potential volume tissue string with string update
  entry.tissue.string <- paste0(stringr::str_replace(dose.var,
                                                     pattern = "^A|^C",
                                                     replacement = "V"),"c")  

  # Check if value "all" is used for a state (that is, all compartments are
  # the same state of matter):
  if (all(tolower(compartment_state[[1]])%in%"all"))
  {
    entry.compartment.state <- names(compartment_state)[1]
  } else {
    # Check whether the dose.var is in the compartment.state list & if so which
    entry.state.check <- unlist(lapply(compartment_state,function(x){dose.var%in%x}))
    if(any(entry.state.check)){
      entry.compartment.state <- names(compartment_state)[which(entry.state.check==TRUE)]
    }else{
      stop(paste0("Entry compartment state is not specified in the model.list for ",model,"."))
    }
  }
  
  # (3) Check if the tissue string is in all.tissue.vols,
  #     yes then get the provided parameter
  #     no then return null value
  #   (a) Check if the length of tissues with a volume in the parameter names is 0.
  if(length(all.tissue.vols)==0){
    entry.tissue.vol <- NULL
  }else if(entry.tissue.string%in%all.tissue.vols){
    entry.tissue.vol <- parameters[[entry.tissue.string]]*parameters[["BW"]]
  }else{
    entry.tissue.vol <- NULL
  }
  
  #Assign input.units to new key units variable, intended as post-scaling
  #units that are ready for any necessary conversion
  dosing.units <- input.units
  
  # Scale dose if input.units is measured in (mg/kg) 
  # Oral absorption (Fabsgut) is handled in this functon:
  dosing <- scale_dosing(
    dosing,
    parameters,
    route,
    input.units = input.units,
    output.units = dose.units,
    vol = entry.tissue.vol,
    state = entry.compartment.state)
  dosing.units <- dose.units  #redefine the dosing units if scaling occurs
  
  #Extract our dosing parameters for use
  initial.dose <- dosing$initial.dose 
  dosing.matrix <- dosing$dosing.matrix
  daily.dose <- dosing$daily.dose
  doses.per.day <- dosing$doses.per.day
  forcings <- dosing$forcings

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
      if (any(is.na(dosing.matrix))) stop(
"Dosing matrix cannot contain NA values")
      if (dim(dosing.matrix)[2]!=2) stop(
"Dosing matrix should be a matrix with two columns (time, dose).")
      dose.times <- dosing.matrix[,"time"]
      dose.vec <- dosing.matrix[,"dose"]
    }
    num.doses <- length(dose.times)
    eventdata <- data.frame(
      var=rep(dose.var,num.doses),
      time = round(dose.times,8),
      value = dose.vec, 
      method = rep(dose.type,num.doses))
    #Update our times vector to include times of provided dosing events, as well as
    #the times of dosing events incremented by small.time for visualization.
    times <- sort(unique(c(times,
      sapply(eventdata$time-small.time, function(x) max(x,0)),
      eventdata$time,
      eventdata$time+small.time)))
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
  out <- ode(
    y = state, 
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
    forcings = forcings,
    fcontrol = fcontrol,
    ...)

# only give the requested times:
 if (!is.null(requested.times)) out <- out[out[,"time"] %in% requested.times, ]

### MODEL OUTPUT

  
# The monitored variables can be altered by the user:
  if (is.null(monitor.vars))
  {
    monitor.vars <- default.monitor.vars
  }
# However, we always include whatever compartment received the dose: 
  if (dose.var %in% colnames(out)) monitor.vars <- 
    unique(c(dose.var,monitor.vars))
  
  if (any(!(monitor.vars%in%colnames(out))))
    stop("Some of the requested variables to monitor (monitor.vars) are not in
          the columns of the deSolve output object. These variables should
          belong to either the states or outputs of the model.")
  
# Convert output to desired units
#   (Do not show conversion factors of 1 - i.e. verbose = FALSE.)
  cu.out <- convert_solve_x(model.output.mat = out,
                            model = model,
                            output.units = output.units,
                            MW = MW,
                            chem.cas = chem.cas,
                            chem.name = chem.name,
                            dtxsid = dtxsid,
                            parameters = parameters,
                            monitor.vars = monitor.vars,
                            suppress.messages=suppress.messages)
  # Re-assign 'out' with the new output from 'cu.out'
  out <- cu.out[['new.ouput.matrix']]

# Cannot guarantee arbitrary precision for deSolve:
  out[,colnames(out)!="time"] <- set_httk_precision(out[,colnames(out)!="time"])
  
# Make a plot if asked for it (not the default behavior):
  if (plots==TRUE)
  {
    #assemble a y-axis units vector to correspond to each entry in monitor.vars
    n_monitor_vars = length(monitor.vars)
    plot_units_vector = rep(NA, n_monitor_vars) 
    
    for (var in 1:n_monitor_vars) {
      if (monitor.vars[var] %in% names(compartment_units)) {
        plot_units_vector[var] = compartment_units[[monitor.vars[var]]]
      } else if (firstchar(monitor.vars[var]) == 'A') {
        #other variables that start with 'A' should all be amounts
        plot_units_vector[var] = "umol"
      } else if (firstchar(monitor.vars[var]) == 'C') {
        plot_units_vector[var] = "uM"
      }
    }
    
    graphics::plot(out, select=unique(c(monitor.vars,names(initial.values))),
                   ylab = plot_units_vector, xlab = 'time (days)')
  } 
               
# Down-select to only the desired parameters:
  out <- out[,unique(c("time",monitor.vars,names(initial.values)))]
  class(out) <- c('matrix','deSolve')
  

# Document the values produced by the simulation:  
  if(!suppress.messages)
  {
    if (is.null(chem.cas) & is.null(chem.name) & is.null(dtxsid))
    {
# If only a parameter vector is given it's good to warn people that they
# need to make sure that these values have been appropriately recalculated:
      if (!recalc.blood2plasma) warning("Rblood2plasma not recalculated. \
Set recalc.blood2plasma to TRUE if desired.") 
      if (!recalc.clearance) warning("Clearance not recalculated. \
Set recalc.clearance to TRUE if desired.") 
    }
    
    if (!is.null(Rblood2plasma))
    {
      if(model!="fetal_pbtk"){
        cat("AUC is area under the plasma concentration curve in ",
            compartment_units[["AUC"]], " units with Rblood2plasma = ",
            signif(Rblood2plasma,3),".\n",sep="")
      }else{
        cat("AUC is area under the maternal plasma concentration curve in ",
            compartment_units[["AUC"]], " units with Rblood2plasma = ",
            signif(Rblood2plasma,3),".\n",
            "fAUC is area under the fetal plasma concentration curve in ",
            compartment_units[["fAUC"]], " units with Rfblood2plasma = ",
            signif(parameters[["Rfblood2plasma"]],3),".\n",
            sep="")
      }
    } else {
      if(model!="fetal_pbtk"){
        cat("AUC is area under the plasma concentration curve in ",
            compartment_units[["AUC"]], " units.\n",sep="")
      }else{
        cat("AUC is area under the maternal plasma concentration curve in ",
            compartment_units[["AUC"]], " units.\n",
            "fAUC is area under the fetal plasma concentration curve in ",
            compartment_units[["fAUC"]], " units.\n",
            sep="")
      }
    }
    
    # Units for output message
    out.units <- cu.out[['output.units.vector']]
    # Cut down to only the desired parameters
    out.units <- out.units[unique(c(monitor.vars,names(initial.values)))]
    # Message to report the output units
    cat("The model outputs are provided in the following units:\n")
      for(u in unique(out.units)){
        cat(
          paste0(
            "\t",u,": ",
            paste(names(out.units)[which(out.units==u)],collapse = ", ")
            ),sep = "\n"
          )
      }
    cat("\n")
  }
    
  return(out) 
}
