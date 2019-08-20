# Add this model to the list of models:

#Analytic expression for steady-state plasma concentration.
#model.list[["fetal_pbtk"]]$analytic.css.func <- "calc_analytic_css_fetal_pbtk" <function not yet developed

# The is the R function for generating model parameters:
model.list[["fetal_pbtk"]]$parameterize.func <- "parameterize_fetal_pbtk"

# These are all the parameters returned by the R model parameterization function.
# Some of these parameters are not directly used to solve the model, but describe
# how other parameters were calculated:
model.list[["fetal_pbtk"]]$param.names <- c(
  "Clint",
  "Clint.dist",
  "Clmetabolismc",
  "Fgutabs",
  "Fhep.assay.correction",
  "Funbound.plasma",
  "Funbound.plasma.adjustment",
  "Funbound.plasma.dist",
  "hematocrit",
  "Kfplacenta2pu",
  "Kgut2pu",
  "kgutabs",
  "Kkidney2pu",
  "Kliver2pu",
  "Klung2pu",
  "Kplacenta2pu",
  "Krbc2pu",
  "Krest2pu",
  "liver.density",
  "MA",
  "million.cells.per.gliver",
  "MW",
  "pKa_Accept",
  "pKa_Donor",
  "Pow",
  "Qgfrc",
  "Rblood2plasma",
  "Vartc",
  "Vgutc",
  "Vkidneyc",
  "Vliverc",
  "Vlungc",
  "Vvenc",
  "Kfthyroid2pu",
  "Kthyroid2pu",
  "Kadipose2pu",
  "Kfliver2pu",
  "Kfkidney2pu",
  "Kfrest2pu",
  "Kfgut2pu",
  "Kflung2pu",
  "Kfbrain2pu",
  "pre_pregnant_BW",
  "BW",
  "Vthyroidc",
  "Vfgutc"
  )


# This subset of R parameters are needed to initially parametrize the compiled
# code for the solver: (must match ORDER under "parameters" in C code, even if 
# some items are omitted)
model.list[["fetal_pbtk"]]$Rtosolvermap <- list(
  pre_pregnant_BW = "pre_pregnant_BW",
  Clmetabolismc = "Clmetabolismc",
  kgutabs = "kgutabs",
  Kkidney2pu = "Kkidney2pu",
  Kliver2pu = "Kliver2pu",
  Krbc2pu = "Krbc2pu",
  Kadipose2pu = "Kadipose2pu",
  Krest2pu = "Krest2pu",
  Klung2pu = "Klung2pu",
  Kgut2pu = "Kgut2pu",
  Kthyroid2pu = "Kthyroid2pu",
  Kplacenta2pu = "Kplacenta2pu",
  Kfplacenta2pu = "Kfplacenta2pu",
  Kfkidney2pu = "Kfkidney2pu",
  Kfrest2pu = "Kfrest2pu",
  Kfthyroid2pu = "Kfthyroid2pu",
  Kfliver2pu = "Kfliver2pu",
  Kflung2pu = "Kflung2pu",
  Kfgut2pu = "Kfgut2pu",
  Kfbrain2pu = "Kfbrain2pu",
  Vartc = "Vartc",
  Vvenc = "Vvenc",
  Vgutc = "Vgutc",
  Vkidneyc = "Vkidneyc",
  Vliverc = "Vliverc",
  Vlungc = "Vlungc",
  Vthyroidc = "Vthyroidc",
  Fraction_unbound_plasma = "Funbound.plasma",
  Ratioblood2plasma = "Ratioblood2plasma"
)

# This function translates the R model parameters into the compiled model
# parameters:
model.list[["fetal_pbtk"]]$compiled.parameters.init <- "getParmsfetalpbtk"

# This is the ORDERED full list of parameters used by the compiled code to 
# calculate the derivative of the system of equations describing the model 
model.list[["fetal_pbtk"]]$compiled.param.names <- c(
  "pre_pregnant_BW",
  "Clmetabolismc",
  "kgutabs",
  "Kkidney2pu",
  "Kliver2pu",
  "Krbc2pu",
  "Kadipose2pu",
  "Krest2pu",
  "Klung2pu",
  "Kgut2pu",
  "Kthyroid2pu",
  "Kplacenta2pu",
  "Kfplacenta2pu",
  "Kfkidney2pu",
  "Kfrest2pu",
  "Kfthyroid2pu",
  "Kfliver2pu",
  "Kflung2pu",
  "Kfgut2pu",
  "Kfbrain2pu",
  "Vartc",
  "Vvenc",
  "Vgutc",
  "Vkidneyc",
  "Vliverc",
  "Vlungc",
  "Vart",
  "Vven",
  "Vgut",
  "Vkidney",
  "Vliver",
  "Vlung",
  "Vthyroidc",
  "Vthyroid",
  "Fraction_unbound_plasma",
  "Ratioblood2plasma",
  "Clmetabolism"
)

# This function initializes the state vector for the compiled model:
model.list[["fetal_pbtk"]]$compiled.init.func <- "initmodfetalpbtk"

# This is the function that calculates the derivative of the model as a function
# of time, state, and parameters:
model.list[["fetal_pbtk"]]$derivative.func <- "derivsfetalpbtk"

# This is the ORDERED list of variables returned by the derivative function
# (from Model variables: Outputs):
model.list[["fetal_pbtk"]]$derivative.output.names <- c(
  "Cgut",
  "Cliver",
  "Cven",
  "Clung",
  "Cart",
  "Cadipose",
  "Crest",
  "Ckidney",
  "Cserum",
  "Aserum",
  "Cthyroid",
  "Cplacenta",
  "Cfliver",
  "Cfven",
  "Cfart",
  "Cfgut",
  "Cflung",
  "Cfrest",
  "Cfthyroid",
  "Cfkidney",
  "Cfbrain",
  "Afserum",
  "Cfserum"
)

#Which variables to track by default (should be able to build this from
#state vars and outputs):
model.list[["fetal_pbtk"]]$default.monitor.vars <- c(
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
  "AUC",
  "fAUC",
  "Cplacenta",
  "Cfliver",
  "Cfven",
  "Cfart",
  "Cfgut",
  "Cflung",
  "Cfrest",
  "Cfthyroid",
  "Cfkidney",
  "Cfbrain",
  "Cfserum"
)

# Allowable units:
model.list[["fetal_pbtk"]]$allowed.units <- c('um', 'mg/l')

# These parameters specify the exposure scenario simulated by the model:
model.list[["fetal_pbtk"]]$dosing.params <- c("daily.dose",
                                        "initial.dose",
                                        "doses.per.day",
                                        "dosing.matrix")
model.list[["fetal_pbtk"]]$routes <- c("oral","iv")
# We need to know which compartment gets the dose 
model.list[["fetal_pbtk"]]$dose.variable <- list(oral="Agutlumen",
                                           iv="Aven")
# Can take the values "add" to add dose C1 <- C1 + dose,
#"replace" to change the value C1 <- dose
#or "multiply" to change the value to C1 <- C1*dose
model.list[["fetal_pbtk"]]$dose.type <- list(oral="add",
                                       iv="add")

# This ORDERED LIST of variables are always calculated in amounts (must match
# Model variables: States in C code): 
model.list[["fetal_pbtk"]]$state.vars <- c(
  "Agutlumen",
  "Agut",
  "Aliver",
  "Aven",
  "Alung",
  "Aart",
  "Aadipose",
  "Arest",
  "Akidney", 
  "Atubules",
  "Ametabolized",
  "AUC",
  "fAUC",
  "Athyroid",
  "Aplacenta",
  "Afgut",
  "Aflung",
  "Afliver",
  "Afven",
  "Afart",
  "Afrest",
  "Afthyroid",
  "Afkidney",
  "Afbrain"
) 

#Parameters needed to make a prediction (this is used by get_cheminfo):
model.list[["fetal_pbtk"]]$required.params <- c(
  "Clint",
  "Funbound.plasma",
  "Pow",
  "pKa_Donor",
  "pKa_Accept",
  "MW"
)

# Do we ignore the Fups where the value was below the limit of detection?
model.list[["fetal_pbtk"]]$exclude.fup.zero <- T