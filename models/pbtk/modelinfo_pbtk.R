# Add the default physiologically-base toxicokinetic (PBTK) model 
# (Pearce et al., 2017) to the list of models:
#
# Pearce, Robert G., et al. "Httk: R package for high-throughput 
# toxicokinetics." Journal of statistical software 79.4 (2017): 1.

#Analytic expression for steady-state plasma concentration.
model.list[["pbtk"]]$analytic.css.func <- "calc_analytic_css_pbtk"

# When calculating steady-state, which compartment do we test? 
# ("C" is preprended):
model.list[["pbtk"]]$steady.state.compartment <- "plasma"

# Function used for generating model parameters:
model.list[["pbtk"]]$parameterize.func <- "parameterize_pbtk"

# Function called for running the model:
model.list[["pbtk"]]$solve.func <- "solve_pbtk"

# Here are the tissues from tissue.data that are considered (for example,
# do we include placenta or not? Here, yes we do). They should correspond
# in name to the names present in the tissue.data object, if the parameters
# necessary for describing the tissue/compartment aren't going to be provided
# otherwise.
model.list[["pbtk"]]$alltissues=c(
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
model.list[["pbtk"]]$tissuelist=list(
                         liver=c("liver"),
                         kidney=c("kidney"),
                         lung=c("lung"),
                         gut=c("gut"))
                                   
# These are all the parameters returned by the R model parameterization function.
# Some of these parameters are not directly used to solve the model, but describe
# how other parameters were calculated:
model.list[["pbtk"]]$param.names <- c(
  "BW",
  "Caco2.Pab",
  "Caco2.Pab.dist",
  "Clint",
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
# code for the solver: (must match ORDER under "parameters" in C code, even if 
# some items are omitted)
model.list[["pbtk"]]$Rtosolvermap <- list(
  BW="BW",
  Clmetabolismc="Clmetabolismc",
  hematocrit="hematocrit",
  kgutabs="kgutabs",
  Kkidney2pu="Kkidney2pu",
  Kliver2pu="Kliver2pu",
  Krest2pu="Krest2pu",
  Kgut2pu="Kgut2pu",
  Klung2pu="Klung2pu",
  Qcardiacc="Qcardiacc",
  Qgfrc="Qgfrc",
  Qgutf="Qgutf",
  Qkidneyf="Qkidneyf",
  Qliverf="Qliverf",
  Vartc="Vartc",
  Vgutc="Vgutc",
  Vkidneyc="Vkidneyc",
  Vliverc="Vliverc",
  Vlungc="Vlungc",
  Vrestc="Vrestc",
  Vvenc="Vvenc",
  Fraction_unbound_plasma="Funbound.plasma",
  Rblood2plasma="Rblood2plasma"
)

# This function translates the R model parameters into the compiled model
# parameters:
model.list[["pbtk"]]$compiled.parameters.init <- "getParmspbtk"

# This needs to be a global variable so that R CMD check --as-cran can test
# the code (the HTTK package does not use this):
compiled_parameters_init <- "getParmspbtk"

# This is the ORDERED full list of parameters used by the compiled code to 
# calculate the derivative of the system of equations describing the model 
model.list[["pbtk"]]$compiled.param.names <- c(
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
model.list[["pbtk"]]$compiled.init.func <- "initmodpbtk"

# This is the function that calculates the derivative of the model as a function
# of time, state, and parameters:
model.list[["pbtk"]]$derivative.func <- "derivspbtk"

# This is the ORDERED list of variables returned by the derivative function
# (from Model variables: Outputs):
model.list[["pbtk"]]$derivative.output.names <- c(
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
model.list[["pbtk"]]$default.monitor.vars <- c(
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
model.list[["pbtk"]]$allowed.units.input <- list(
       "oral" = c('umol','mg','mg/kg'),
       "iv" = c('umol','mg','mg/kg'))

# Allowable units assigned to entries in the output columns of the ode system
model.list[["pbtk"]]$allowed.units.output <- list(
       "oral" = c('uM','mg/l','umol','mg','uM*days','mg/L*days'),
       "iv" = c('uM','mg/l','umol','mg','uM*days','mg/L*days'))

# Default set of units assigned to correspond to each of the "outputs" of 
# the model system, and possibly to other state variables to be monitored.
# AUC values should also be included.
model.list[["pbtk"]]$compartment.units <- c(
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

# These parameters specify the exposure scenario simulated by the model:
model.list[["pbtk"]]$dosing.params <- c("daily.dose",
  "initial.dose",
  "doses.per.day",
  "dosing.matrix")
model.list[["pbtk"]]$routes <- c("oral","iv")
# We need to know which compartment gets the dose 
model.list[["pbtk"]]$dose.variable <- list(oral="Agutlumen",
  iv="Aven")
# Can take the values "add" to add dose C1 <- C1 + dose,
#"replace" to change the value C1 <- dose
#or "multiply" to change the value to C1 <- C1*dose
model.list[["pbtk"]]$dose.type <- list(oral="add",
  iv="add")

# ORDERED LIST of state variables (must match Model variables: 
# States in C code, each of which is associated with a differential equation),
# mostly calculated in amounts, though AUC (area under plasma concentration
# curve) also appears here: 
model.list[["pbtk"]]$state.vars <- c(
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
       
#Parameters needed to make a prediction (this is used by get_cheminfo):
model.list[["pbtk"]]$required.params <- c(
  "Clint",
  "Funbound.plasma",
  "Pow",
  "pKa_Donor",
  "pKa_Accept",
  "MW"
   )

# Function for calculating Clmetabolismc after Clint is varied:
model.list[["pbtk"]]$propagateuv.func <- "propagate_invitrouv_pbtk"

# If httk-pop is enabled:
# Function for converting httk-pop physiology to model parameters:
model.list[["pbtk"]]$convert.httkpop.func <- NULL

# We want all the standard physiological calculations performed:
model.list[["pbtk"]]$calc.standard.httkpop2httk <- TRUE

# These are the model parameters that are impacted by httk-pop:
model.list[["pbtk"]]$httkpop.params <- c(
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
model.list[["pbtk"]]$calcpc <- TRUE
  

# Do we need to recalculate first pass metabolism when doing Monte Carlo?
model.list[["pbtk"]]$firstpass <- FALSE

# Do we ignore the Fups where the value was below the limit of detection?
model.list[["pbtk"]]$exclude.fup.zero <- T

# These are the parameter names needed to describe steady-state dosing:
model.list[["pbtk"]]$css.dosing.params <- c("hourly.dose")
