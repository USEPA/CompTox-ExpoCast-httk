# Add the 3compartment model (Pearce et al., 2017) to the list of models:
#
# Pearce, Robert G., et al. "Httk: R package for high-throughput 
# toxicokinetics." Journal of statistical software 79.4 (2017): 1.

#Analytic expression for steady-state plasma concentration.
model.list[["3compartment"]]$analytic.css.func <- "calc_analytic_css_3comp"

<<<<<<< HEAD
#Define the parameter names for each model in one place so that all functions can use them:
param.names.3comp <- c("BW",
                     "Clint",
                     "Clint.dist",
                     "Clmetabolismc",
                     "Funbound.plasma",
                     "Funbound.plasma.dist",
                     "Funbound.plasma.adjustment",
                     "Fabsgut",
                     "Fabs",
                     "Fgut",
                     "Fhep.assay.correction",
                     "hematocrit",
                     "Kgut2pu",
                     "Krbc2pu",
                     "kgutabs",
                     "Kliver2pu",
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
                     "Qliverf",
                     "Rblood2plasma",
                     "Vgutc",
                     "Vliverc",
                     "Vrestc")

param.names.3comp.solver <- c("BW",
                     "Clmetabolismc",
                     'Fraction_unbound_plasma',
                     "Kgut2plasma",
                     "kgutabs",
                     "Kliver2plasma",
                     "Krest2plasma",
                     "Qcardiacc",
                     "Qgfrc",
                     "Qgutf",
                     "Qliverf",
                     "Ratioblood2plasma",
                     "Vgut",
                     "Vliver",
                     "Vrest")

initparms3comp <- function(newParms = NULL){
  parms <- c(
    BW = 70,
    Clmetabolismc = 0.203,
    kgutabs = 1,
    Qcardiacc = 0,
    Qgfrc = 0.108,
    Qgutf = 0.205,
    Qliverf = 0.0536,
    Vgut = 0,
    Vliver = 0,
    Vrest = 0,
    Fraction_unbound_plasma = 0.0682,
    Clmetabolism = 0.0,
    Qcardiac = 0,
    Qgfr = 0.0,
    Qgut = 0.0,
    Qliver = 0.0,
    Kliver2plasma = 0,
    Krest2plasma = 0,
    Kgut2plasma = 0,
    Ratioblood2plasma = 0
=======
# Function used for generating model parameters:
model.list[["3compartment"]]$parameterize.func <- "parameterize_3comp"

# Function called for running the model:
model.list[["3compartment"]]$solve.func <- "solve_3comp"

# How the tissues from tissue.table are lumped together to form the model:
# 3 compartment model has only liver and gut compartments; everything else is
# lumped.
model.list[['3compartment']]$tissuelist = list(
               liver=c("liver"),
               gut=c("gut"))

# These are all the parameters returned by the R model parameterization function.
# Some of these parameters are not directly used to solve the model, but describe
# how other parameters were calculated:
model.list[["3compartment"]]$param.names <- c(
  "BW",
  "Clint",
  "Clint.dist",
  "Clmetabolismc",
  "Funbound.plasma",
  "Funbound.plasma.dist",
  "Funbound.plasma.adjustment",
  "Fgutabs",
  "Fhep.assay.correction",
  "hematocrit",
  "Kgut2pu",
  "Krbc2pu",
  "kgutabs",
  "Kliver2pu",
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
  "Qliverf",
  "Rblood2plasma",
  "Vgutc",
  "Vliverc",
  "Vrestc"
  )
                    
# This subset of R parameters are needed to initially parametrize the compiled
# code for the solver: (must match ORDER under "parameters" in C code)
model.list[["3compartment"]]$Rtosolvermap <- list(
  BW = "BW",
  CLmetabolismc="Clmetabolismc",
  Fraction_unbound_plasma="Funbound.plasma",
  Kgut2plasma="Kgut2pu",
  kgutabs="kgutabs",
  Kliver2plasma="Kliver2pu",
  Krest2plasma="Krest2pu",
  Qcardiacc="Qcardiacc",
  Qgfrc="Qgfrc",
  Qgutf="Qgutf",
  Qliverf="Qliverf",
  Ratioblood2plasma="Rblood2plasma",
  Vportvenc="Vgutc",
  Vliverc="Vliverc",
  Vsyscompc="Vrestc"
)

# This function translates the R model parameters into the compiled model
# parameters:
model.list[["3compartment"]]$compiled.parameters.init <- "getParms3comp"

# This is the ORDERED full list of parameters used by the compiled code to 
# calculate the derivative of the system of equations describing the model 
model.list[["3compartment"]]$compiled.param.names <- c(
  "BW",
  "Clmetabolismc",
  "kgutabs",
  "Qcardiacc",
  "Qgfrc",
  "Qgutf",
  "Qliverf",
  "Vportvenc",
  "Vliverc",
  "Vsyscompc",
  "Vportven",
  "Vliver",
  "Vsyscomp",
  "Fraction_unbound_plasma",
  "Clmetabolism",
  "Qcardiac",
  "Qgfr",
  "Qgut",
  "Qliver",
  "Kliver2plasma",
  "Krest2plasma",
  "Ratioblood2plasma"
>>>>>>> cd6935617acdc1f8696861a41ecfb6190cbebda1
  )

# This function initializes the state vector for the compiled model:
model.list[["3compartment"]]$compiled.init.func <- "initmod3comp"

# This is the function that calculates the derviative of the model as a function
# of time, state, and parameters:
model.list[["3compartment"]]$derivative.func <- "derivs3comp"

# This is the ORDERED list of variables returned by the derivative function
# (from Model variables: Outputs):
model.list[["3compartment"]]$derivative.output.names <- c(
  "Cportven",
  "Cliver",
  "Csyscomp"
)

model.list[["3compartment"]]$default.monitor.vars <- c(
  "Cliver",
  "Csyscomp",
  "Atubules",
  "Ametabolized",
  "AUC"
  )

# Allowable units:
model.list[["3compartment"]]$allowed.units <- c('um', 'mg/l')

# These parameters specific the exposure scenario simulated by the model:
model.list[["3compartment"]]$dosing.params <- c(
  "daily.dose",
  "initial.dose",
  "doses.per.day",
  "dosing.matrix")
model.list[["3compartment"]]$routes <- c("oral","iv")
# We need to know which compartment gets the dose 
model.list[["3compartment"]]$dose.variable <- list(
  oral="Aintestine",
  iv="Asyscomp")
# Can take the values "add" to add dose C1 <- C1 + dose,
#"replace" to change the value C1 <- dose
#or "multiply" to change the value to C1 <- C1*dose
model.list[["3compartment"]]$dose.type <- list(
  oral="add",
  iv="add")

# ORDERED LIST of state variables (must match Model variables: 
# States in C code, each of which is associated with a differential equation),
# mostly calculated in amounts, though AUC (area under plasma concentration
# curve) also appears here: 
model.list[["3compartment"]]$state.vars <- c(
  "Aintestine",
  "Aportven",
  "Aliver",
  "Asyscomp",
  "Ametabolized",
  "Atubules",
  "AUC"
  )
       
#Parameters needed to make a prediction (this is used by get_cheminfo):
model.list[["3compartment"]]$required.params <- c(
  "Clint",
  "Funbound.plasma",
  "Pow",
  "pKa_Donor",
  "pKa_Accept",
  "MW"
   )

# Function for calculating Clmetabolismc after Clint is varied:
model.list[["3compartment"]]$propagateuv.func <- "propagateuv_3comp"

# If httk-pop is enabled:
# Function for converting httk-pop physiology to model parameters:
model.list[["1compartment"]]$convert.httkpop.func <- NULL# We want all the standard physiological calculations performed:
model.list[["3compartment"]]$calc.standard.httkpop2httk <- TRUE
# These are the model parameters that are impacted by httk-pop:
model.list[["3compartment"]]$httkpop.params <- c(
  "BW",
  "Qcardiacc",
  "Qgfrc",
  "Qgutf",
  "Qliverf",
  "Vportvenc",
  "Vliverc",
  "Vsyscompc",
  "Vportven",
  "Vliver",
  "Vsyscomp",
  "Qcardiac",
  "Qgfr",
  "Qgut",
  "Qliver",
  "Ratioblood2plasma")

#Governs how tissues are lumped:
model.list[["pbtk"]]$tissue.list <- list(
               liver=c("liver"),
               gut=c("gut"))

# Do we need to recalculate partition coefficients when doing Monte Carlo?
model.list[["3compartment"]]$calcpc <- TRUE

# Do we need to recalculate first pass metabolism when doing Monte Carlo?
model.list[["3compartment"]]$firstpass <- FALSE

# Do we ignore the Fups where the value was below the limit of detection?
model.list[["3compartment"]]$exclude.fup.zero <- T