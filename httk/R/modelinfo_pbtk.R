# Add the default physiologically-base toxicokinetic (PBTK) model 
# (Pearce et al., 2017) to the list of models:
#
# Pearce, Robert G., et al. "Httk: R package for high-throughput 
# toxicokinetics." Journal of statistical software 79.4 (2017): 1.

# Model identifier for the model.list:
THIS.MODEL <- "pbtk" 

# Analytic expression for steady-state plasma concentration to be used by
# calc_analytic_css:
model.list[[THIS.MODEL]]$analytic.css.func <- "calc_analytic_css_pbtk"

# What units does the analytic function return in calc_analytic_css:
model.list[[THIS.MODEL]]$steady.state.units <- "mg/L"

# When calculating steady-state with calc_css, which compartment do we test? 
# ("C" is preprended):
model.list[[THIS.MODEL]]$steady.state.compartment <- "plasma"

# Function used for generating model parameters:
model.list[[THIS.MODEL]]$parameterize.func <- "parameterize_pbtk"

# Function called for running the model:
model.list[[THIS.MODEL]]$solve.func <- "solve_pbtk"

# Here are the tissues from tissue.data that are considered (for example,
# do we include placenta or not? Here, yes we do). They should correspond
# in name to the names present in the tissue.data object, if the parameters
# necessary for describing the tissue/compartment aren't going to be provided
# otherwise.
model.list[[THIS.MODEL]]$alltissues=c(
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

# How the tissues from tissue.data are lumped together to form the model:
# PBTK model has liver, kidney, gut, and lung compartments that draw info
# from tissue.data; everything else from alltissues should be lumped.
model.list[[THIS.MODEL]]$tissuelist=list(
  liver=c("liver"),
  kidney=c("kidney"),
  lung=c("lung"),
  gut=c("gut"))

# These are all the parameters returned by the R model parameterization function.
# Some of these parameters are not directly used to solve the model, but describe
# how other parameters were calculated:
model.list[[THIS.MODEL]]$param.names <- c(
  "BW",
  "Caco2.Pab",
  "Caco2.Pab.dist",
  "Clint",
  "Clint.dist",
  "Clmetabolismc",
  "Funbound.plasma",
  "Funbound.plasma.dist",
  "Funbound.plasma.adjustment",
  "Fabsgut",
  "Fhep.assay.correction",
  "hematocrit",
  "Kgut2pu",
  "kgutabs",
  "Kkidney2pu",
  "Kliver2pu",
  "Klung2pu",
  "Krbc2pu",
  "Krest2pu",
  "liver.density",
  "million.cells.per.gliver",
  "MW",
  "Pow",
  "pKa_Donor",
  "pKa_Accept",
  "MA",
  "Qcardiacc",
  "Qgfrc",
  "Qgutf",
  "Qkidneyf",
  "Qliverf",
  "Rblood2plasma",
  "Vartc",
  "Vgutc",
  "Vkidneyc",
  "Vliverc",
  "Vlungc",
  "Vrestc",
  "Vvenc")
                    
# This subset of R parameters are needed to initially parameterize the compiled
# code for the solver: (must match ORDER under "parameters" in C code)
model.list[[THIS.MODEL]]$Rtosolvermap <- list(
  BW="BW",
  Clmetabolismc = "Clmetabolismc",
  hematocrit = "hematocrit",
  kgutabs = "kgutabs",
  Kkidney2pu = "Kkidney2pu",
  Kliver2pu = "Kliver2pu",
  Krest2pu = "Krest2pu",
  Kgut2pu = "Kgut2pu",
  Klung2pu = "Klung2pu",
  Qcardiacc = "Qcardiacc",
  Qgfrc = "Qgfrc",
  Qgutf = "Qgutf",
  Qkidneyf = "Qkidneyf",
  Qliverf = "Qliverf",
  Vartc = "Vartc",
  Vgutc = "Vgutc",
  Vkidneyc = "Vkidneyc",
  Vliverc = "Vliverc",
  Vlungc = "Vlungc",
  Vrestc = "Vrestc",
  Vvenc = "Vvenc",
  Fraction_unbound_plasma = "Funbound.plasma",
  Rblood2plasma = "Rblood2plasma"
)

# This function translates the R model parameters into the compiled model
# parameters:
model.list[[THIS.MODEL]]$compiled.parameters.init <- "getParmspbtk"

# This needs to be a global variable so that R CMD check --as-cran can test
# the code (the HTTK package does not use this):
compiled_parameters_init <- "getParmspbtk"

# This is the ORDERED full list of parameters used by the compiled code to 
# calculate the derivative of the system of equations describing the model 
model.list[[THIS.MODEL]]$compiled.param.names <- c(
  "BW",
  "Clmetabolismc",
  "hematocrit",
  "kgutabs",
  "Kkidney2pu",
  "Kliver2pu",
  "Krest2pu",
  "Kgut2pu",
  "Klung2pu",
  "Qcardiacc",
  "Qgfrc",
  "Qgutf",
  "Qkidneyf",
  "Qliverf",
  "Vartc",
  "Vgutc",
  "Vkidneyc",
  "Vliverc",
  "Vlungc",
  "Vrestc",
  "Vvenc",
  "Fraction_unbound_plasma",
  "Rblood2plasma",
  "Clmetabolism",
  "Qcardiac",
  "Qgfr",
  "Qgut",
  "Qkidney",
  "Qliver",
  "Qrest",
  "Vart",
  "Vgut",
  "Vkidney",
  "Vliver",
  "Vlung",
  "Vrest",
  "Vven"
  )

# This function initializes the state vector for the compiled model:
model.list[[THIS.MODEL]]$compiled.init.func <- "initmodpbtk"

# This is the function that calculates the derivative of the model as a function
# of time, state, and parameters:
model.list[[THIS.MODEL]]$derivative.func <- "derivspbtk"

# This is the ORDERED list of variables returned by the derivative function
# (from Model variables: Outputs):
model.list[[THIS.MODEL]]$derivative.output.names <- c(
  "Cgut",
  "Cliver",
  "Cven",
  "Clung",
  "Cart",
  "Crest",
  "Ckidney",
  "Cplasma",
  "Aplasma"
  )

#list of variables to be monitored (plotted). This list should be able to be
#constructed from states and outputs. 
model.list[[THIS.MODEL]]$default.monitor.vars <- c(
  "Cgut",
  "Cliver",
  "Cven",
  "Clung",
  "Cart",
  "Crest",
  "Ckidney",
  "Cplasma",
  "Atubules",
  "Ametabolized",
  "AUC"
  )

# Allowable units assigned to dosing input:
model.list[[THIS.MODEL]]$allowed.units.input <- list(
       "oral" = c('umol','mg','mg/kg'),
       "iv" = c('umol','mg','mg/kg')
       )
  
model.list[[THIS.MODEL]]$routes <- list(
  "oral" = list(
# We need to know which compartment gets the dose 
    "entry.compartment" = "Agutlumen",
# desolve events can take the values "add" to add dose C1 <- C1 + dose,
# "replace" to change the value C1 <- dose
# or "multiply" to change the value to C1 <- C1*dose
    "dose.type" = "add",
    "dosing.params" = c(
      "daily.dose",
      "initial.dose",
      "doses.per.day",
      "dosing.matrix")),
  "iv" = list(
    "entry.compartment" = "Aven",
    "dose.type" = "add",
    "dosing.params" = c(
      "initial.dose",
      "dosing.matrix"))
  )

# ORDERED LIST of state variables (must match Model variables: 
# States in C code, each of which is associated with a differential equation),
# mostly calculated in amounts, though AUC (area under plasma concentration
# curve) also appears here: 
model.list[[THIS.MODEL]]$state.vars <- c(
    "Agutlumen",
    "Agut",
    "Aliver",
    "Aven",
    "Alung",
    "Aart",
    "Arest",
    "Akidney", 
    "Atubules",
    "Ametabolized",
    "AUC"
    ) 
    
# Actual (intrinsic) units assigned to each of the time dependent
# variables of the model system including state variables and any transformed
# outputs (for example, concentrations calculated from amounts.)
# AUC values should also be included.
model.list[[THIS.MODEL]]$compartment.units <- c(
    "Agutlumen"="umol",
    "Agut"="umol",
    "Aliver"="umol",
    "Aven"="umol",
    "Alung"="umol",
    "Aart"="umol",
    "Arest"="umol",
    "Akidney"="umol",
    "Atubules"="umol",
    "Ametabolized"="umol",
    "Cgut"="uM",
    "Cliver"="uM",
    "Cven"="uM",
    "Clung"="uM",
    "Cart"="uM",
    "Crest"="uM",
    "Ckidney"="uM",
    "Cplasma"="uM",
    "Aplasma"="umol",
    "AUC"="uM*days"
  )

# Compartment state of matter, needed for proper unit conversion, if all
# comaprtments of the same only include one state and set it to "all":
model.list[[THIS.MODEL]]$compartment.state <- list(liquid="all")

#Parameters needed to make a prediction (this is used by get_cheminfo):
model.list[[THIS.MODEL]]$required.params <- c(
  "Clint",
  "Funbound.plasma",
  "Pow",
  "pKa_Donor",
  "pKa_Accept",
  "MW"
  )

# Function for calculating Clmetabolismc after Clint is varied:
model.list[[THIS.MODEL]]$propagateuv.func <- "propagate_invitrouv_pbtk"

# If httk-pop is enabled:
# Function for converting httk-pop physiology to model parameters:
model.list[[THIS.MODEL]]$convert.httkpop.func <- NULL

# We want all the standard physiological calculations performed:
model.list[[THIS.MODEL]]$calc.standard.httkpop2httk <- TRUE

# These are the model parameters that are impacted by httk-pop:
model.list[[THIS.MODEL]]$httkpop.params <- c(
  "BW",
  "Fabsgut",
  "hematocrit",
  "liver.density",
  "million.cells.per.gliver",
  "Qcardiacc",
  "Qgfrc",
  "Qgutf",
  "Qkidneyf",
  "Qliverf",
  "Rblood2plasma",
  "Vartc",
  "Vgutc",
  "Vkidneyc",
  "Vliverc",
  "Vlungc",
  "Vrestc",
  "Vvenc")

# Do we need to recalculate partition coefficients when doing Monte Carlo?
model.list[[THIS.MODEL]]$calcpc <- TRUE

# Do we need to recalculate first pass metabolism when doing Monte Carlo?
model.list[[THIS.MODEL]]$firstpass <- FALSE

# Do we ignore the Fups where the value was below the limit of detection?
model.list[[THIS.MODEL]]$exclude.fup.zero <- TRUE

# These are the parameter names needed to describe steady-state dosing:
model.list[[THIS.MODEL]]$css.dosing.params <- list(
  oral=c("hourly.dose"))

# Filter out volatile compounds with Henry's Law Constant Threshold
model.list[[THIS.MODEL]]$log.henry.threshold <- c(-4.5)

# Filter out compounds belonging to select chemical classes
model.list[[THIS.MODEL]]$chem.class.filt <- c("PFAS")
