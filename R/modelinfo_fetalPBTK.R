# Add this model to the list of models:

#Analytic expression for steady-state plasma concentration.
#model.list[["fetal_pbtk"]]$analytic.css.func <- "calc_analytic_css_fetal_pbtk" <function not yet developed

# The is the R function for generating model parameters:
model.list[["fetal_pbtk"]]$parameterize.func <- "parameterize_fetal_pbtk"

# These are all the parameters returned by the R model parameterization function.
# Some of these parameters are not directly used to solve the model, but describe
# how other parameters were calculated:
model.list[["fetal_pbtk"]]$param.names <- c(
  "BW",
  "Clint",
  "Clmetabolismc",
  "Funbound.plasma",
  "Funbound.plasma.dist",
  "Funbound.plasma.adjustment",
  "Fgutabs",
  "Fhep.assay.correction",
  "hematocrit",
  "Kgut2pu",
  "kgutabs",
  "Kkidney2pu",
  "Kliver2pu",
  "Klung2pu",
  "Krbc2pu",
  "Kthyroid2pu",
  "Kplacenta2pu",
  "Krest2pu",
  "Kfgut2pu",
  "Kfkidney2pu",
  "Kfliver2pu",
  "Kflung2pu",
  "Kfthyroid2pu",
  "Kfbrain2pu",
  "Kfplacenta2pu",
  "Kfrest2pu",
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



parms$Kfbrain2pu <- parms$Kbrain2pu
parms$Krest2pu <- (parms$Krest2pu * parms$Vrestc + parms$Kbrain2pu * parms$Vbrainc) / ( parms$Vrestc  + parms$Vbrainc)
parms$pre_pregnant_BW <- 61.103 
parms$Vthyroidc <- 0.017/parms$pre_pregnant_BW
parms$Vkidneyc <- 0.275/parms$pre_pregnant_BW
parms$Vgutc <- 1.14/parms$pre_pregnant_BW
parms$Vliverc <- 1.4/parms$pre_pregnant_BW
parms$Vlungc <- 0.95/parms$pre_pregnant_BW
parms$Vartc <- 0.624/parms$pre_pregnant_BW         
parms$Vvenc <- 2.32/parms$pre_pregnant_BW
parms$Vfgutc <- 0.0178













# This subset of R parameters are needed to initially parametrize the compiled
# code for the solver: (must match ORDER under "parameters" in C code)
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
  Qkidney="Qkidneyf",
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

# This is the function that calculates the derviative of the model as a function
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

# Allowable units:
model.list[["pbtk"]]$allowed.units <- c('um', 'mg/l')

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

# This ORDERED LIST of variables are always calculated in amounts (must match
# Model variables: States in C code): 
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

# Do we ignore the Fups where the value was below the limit of detection?
model.list[["pbtk"]]$exclude.fup.zero <- T