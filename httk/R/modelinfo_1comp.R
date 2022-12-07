# Add the 1compartment model (Pearce et al., 2017) to the list of models:
#
# Pearce, Robert G., et al. "Httk: R package for high-throughput 
# toxicokinetics." Journal of statistical software 79.4 (2017): 1.

# Analytic expression for steady-state plasma concentration.
model.list[["1compartment"]]$analytic.css.func <- "calc_analytic_css_1comp"

# When calculating steady-state, which compartment do we test? 
# ("C" is preprended):
model.list[["1compartment"]]$steady.state.compartment <- "compartment"

# What units does the analytic function return:
model.list[["1compartment"]]$steady.state.units <- "mg/L"

# Function used for generating model parameters:
model.list[["1compartment"]]$parameterize.func <- "parameterize_1comp"

# Function called for running the model:
model.list[["1compartment"]]$solve.func <- "solve_1comp"

# Here are the tissues from tissue.data that are considered:
model.list[["1compartment"]]$alltissues=c(
  "adipose",
  "bone",            
  "brain",           
  "gut",            
  "heart",           
  "kidney",          
  "liver",           
  "lung",           
  "muscle", 
  "skin",            
  "spleen",          
  "red blood cells",
  "rest")
  
# 1compartment model lumps everything, so list of compartments is empty.
model.list[['1compartment']]$tissuelist <- NULL

# These are all the parameters returned by the R model parameterization function.
# Some of these parameters are not directly used to solve the model, but describe
# how other parameters were calculated:
model.list[["1compartment"]]$param.names <- c(
  "BW",
  "Clint",
  "Clint.dist",
  "Fgutabs",
  "Fhep.assay.correction",
  "Funbound.plasma",
  "Funbound.plasma.dist",
  "Funbound.plasma.adjustment",
  "hepatic.bioavailability",
  "hematocrit",
  "kelim",
  "kgutabs",
  "liver.density",
  "million.cells.per.gliver",
  "MA",
  "MW",
  "Rblood2plasma",
  "Pow",
  "pKa_Donor",
  "pKa_Accept",
  "Vdist")

# This subset of R parameters are needed to initially parametrize the compiled
# code for the solver: (must match ORDER under "parameters" in C code)
model.list[["1compartment"]]$Rtosolvermap <- list(
  vdist="Vdist",
  ke="kelim",
  kgutabs="kgutabs",
  BW="BW")

# If the model does not include an explicit gut-liver link before systemic
# circulation, then we want to decrease the absorbed dose by the first past
# hepatic extraction factor:
model.list[["1compartment"]]$do.first.pass <- TRUE

# This function translates the R model parameters into the compiled model
# parameters:
model.list[["1compartment"]]$compiled.parameters.init <- "getParms1comp"

# This is the ORDERED full list of parameters used by the compiled code to 
# calculate the derivative of the system of equations describing the model 
model.list[["1compartment"]]$compiled.param.names <- c(
  "vdist",
  "ke",
  "kgutabs",
  "BW")

# This function initializes the state vector for the compiled model:
model.list[["1compartment"]]$compiled.init.func <- "initmod1comp"

# This is the function that calculates the derviative of the model as a function
# of time, state, and parameters:
model.list[["1compartment"]]$derivative.func <- "derivs1comp"

# This is the ORDERED list of variables returned by the derivative function:
model.list[["1compartment"]]$derivative.output.names <- c(
  "Ccompartment")

model.list[["1compartment"]]$default.monitor.vars <- c(
  "Agutlumen",
  "Ccompartment",
  "Ametabolized",
  "AUC")


# Allowable units assigned to dosing input:
model.list[["1compartment"]]$allowed.units.input <- list(
      "oral" = c('umol','mg','mg/kg'),
       "iv" = c('umol','mg','mg/kg'))

# Allowable units assigned to entries in the output columns of the ode system
model.list[["1compartment"]]$allowed.units.output <- list(
       "oral" = c('umol','uM','mg/L','uM*days','mg/L*days'),
       "iv" = c('umol','uM','mg/L','uM*days','mg/L*days'))

# Default set of units assigned to correspond to each of the time dependent
# variables of the model system including state variables and any transformed
# outputs (for example, concentrations calculated from amounts.)
# AUC values should also be included.
model.list[["1compartment"]]$compartment.units <- c(
    "Agutlumen"="umol",
    "Acompartment"="umol",
    "Ametabolized"="umol", 
    "Ccompartment"="uM",
    "AUC" = "uM*days")

# These parameters specific the exposure scenario simulated by the model:
model.list[["1compartment"]]$dosing.params <- c(
  "daily.dose",
  "initial.dose",
  "doses.per.day",
  "dosing.matrix")

model.list[["1compartment"]]$routes <- list(
  "oral" = list(
# We need to know which compartment gets the dose 
    "entry.compartment" = "Agutlumen",
# desolve events can take the values "add" to add dose C1 <- C1 + dose,
# "replace" to change the value C1 <- dose
# or "multiply" to change the value to C1 <- C1*dose
    "dose.type" = "add"),
  "iv" = list(
    "entry.compartment" = "Acompartment",
    "dose.type" = "add")
  )

# ORDERED LIST of state variables (must match Model variables: 
# States in C code, each of which is associated with a differential equation),
# mostly calculated in amounts, though AUC (area under plasma concentration
# curve) also appears here: 
model.list[["1compartment"]]$state.vars <- c(
    "Agutlumen",
    "Acompartment",
    "Ametabolized", 
    "AUC")

#Parameters needed to make a prediction (this is used by get_cheminfo):
model.list[["1compartment"]]$required.params <- c(
  "Clint", 
  "Funbound.plasma",
  "Pow",
  "pKa_Donor",
  "pKa_Accept",
  "MW"
   )
   
# Function for calculating Clmetabolismc after Clint is varied:
model.list[["1compartment"]]$propagateuv.func <- "propagate_invitrouv_1comp"

# If httk-pop is enabled:
# Function for converting httk-pop physiology to model parameters:
model.list[["1compartment"]]$convert.httkpop.func <- NULL
# We want all the standard physiological calculations performed:
model.list[["1compartment"]]$calc.standard.httkpop2httk <- TRUE
# These are the model parameters that are impacted by httk-pop:
model.list[["1compartment"]]$httkpop.params <- c(
  "BW",
  "Fgutabs",
  "hepatic.bioavailability",
  "hematocrit",
  "liver.density",
  "million.cells.per.gliver",
  "Rblood2plasma",
  "Vdist")

# Do we need to recalculate partition coefficients when doing Monte Carlo?
model.list[["1compartment"]]$calcpc <- TRUE


# Do we need to recalculate first pass metabolism when doing Monte Carlo?
model.list[["1compartment"]]$firstpass <- TRUE

# Do we ignore the Fups where the value was below the limit of detection?
model.list[["1compartment"]]$exclude.fup.zero <- TRUE

# These are the parameter names needed to describe steady-state dosing:
model.list[["1compartment"]]$css.dosing.params <- c("hourly.dose")

# Filter out volatile compounds with Henry's Law Constant Threshold
model.list[["1compartment"]]$log.henry.threshold <- c(-4.5)

# Filter out compounds belonging to select chemical classes
model.list[["1compartment"]]$chem.class.filt <- c("PFAS")


