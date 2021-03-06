# Add the 1compartment model (Pearce et al., 2017) to the list of models:
#
# Pearce, Robert G., et al. "Httk: R package for high-throughput 
# toxicokinetics." Journal of statistical software 79.4 (2017): 1.

# Analytic expression for steady-state plasma concentration.
model.list[["1compartment"]]$analytic.css.func <- "calc_analytic_css_1comp"

# Function used for generating model parameters:
model.list[["1compartment"]]$parameterize.func <- "parameterize_1comp"

# Function called for running the model:
model.list[["1compartment"]]$solve.func <- "solve_1comp"

# How the tissues from tissue.table are lumped together to form the model:
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
model.list[["1compartment"]]$do.first.pass <- T

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

# Allowable units:
model.list[["1compartment"]]$allowed.units <- c('um', 'mg/l')

# These parameters specific the exposure scenario simulated by the model:
model.list[["1compartment"]]$dosing.params <- c("daily.dose",
  "initial.dose",
  "doses.per.day",
  "dosing.matrix")
model.list[["1compartment"]]$routes <- c("oral","iv")
# We need to know which compartment gets the dose 
model.list[["1compartment"]]$dose.variable <- list(oral="Agutlumen",
  iv="Acompartment")
# Can take the values "add" to add dose C1 <- C1 + dose,
#"replace" to change the value C1 <- dose
#or "multiply" to change the value to C1 <- C1*dose
model.list[["1compartment"]]$dose.type <- list(oral="add",
  iv="add")

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

#Governs how tissues are lumped:
model.list[["1compartment"]]$tissue.list <- NULL

# Do we need to recalculate partition coefficients when doing Monte Carlo?
model.list[["1compartment"]]$calcpc <- TRUE


# Do we need to recalculate first pass metabolism when doing Monte Carlo?
model.list[["1compartment"]]$firstpass <- TRUE

# Do we ignore the Fups where the value was below the limit of detection?
model.list[["1compartment"]]$exclude.fup.zero <- T