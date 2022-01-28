#Analytic expression for steady-state plasma concentration.
#model.list[["skin_2subcomp"]]$analytic.css.func <- "calc_analytic_css_skin_2subcomp" # added by MB 4/8/2020

# The is the R function for generating model parameters:
model.list[["skin_2subcomp"]]$parameterize.func <- "parameterize_dermal_pbtk" 

# These are all the parameters returned by the R model parameterization function.
# Some of these parameters are not directly used to solve the model, but describe
# how other parameters were calculated:
#model.list[["skin_2subcomp"]]$param.names <- c(
 
 # ) 
                    
# This subset of R parameters are needed to initially parameterize the compiled
# code for the solver: (must match ORDER under "parameters" in C code)
model.list[["skin_2subcomp"]]$Rtosolvermap <- list( #updated by AM, 1/21/2022
  skin_depth = "skin_depth",
  Fskin_depth_sc = "Fskin_depth_sc",
  Fskin_depth_cd = "Fskin_depth_cd",
  Pmedia2sc = "Pmedia2sc",
  Psc2cd = "Psc2cd",
  V0 = "V0",
  Fskin_exposed = "Fskin_exposed",
  totalSA = "totalSA",
  BW = "BW",
  Clmetabolismc = "Clmetabolismc",
  hematocrit = "hematocrit",
  kgutabs = "kgutabs",
  Ksc2media = "Ksc2media",
  Ksc2cd = "Ksc2cd",
  Kcd2pu = "Kcd2pu",
  Kkidney2pu = "Kkidney2pu",
  Kliver2pu = "Kliver2pu",
  Krest2pu = "Krest2pu",
  Kgut2pu = "Kgut2pu",
  Klung2pu = "Klung2pu",
  Qcardiacc = "Qcardiacc",
  Qgfrc = "Qgfrc",
  Qcomposite_dermalf = "Qcomposite_dermalf",
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
  Vskinc = "Vskinc",
  Vstratum_corneumc = "Vstratum_corneumc", #dummy parameter - not used
  Vcomposite_dermalc = "Vcomposite_dermalc", #dummy parameter - not used
  Fraction_unbound_plasma = "Funbound.plasma", #different to match R httk code
  Rblood2plasma = "Rblood2plasma"
)

# This function translates the R model parameters into the compiled model
# parameters:
model.list[["skin_2subcomp"]]$compiled.parameters.init <- "getParms_skin_2subcomp" #in .c file

# This is the ORDERED full list of parameters used by the compiled code to 
# calculate the derivative of the system of equations describing the model 
model.list[["skin_2subcomp"]]$compiled.param.names <- c( #updated by AM, 1/21/2022
  "skin_depth",
  "Fskin_depth_sc",
  "Fskin_depth_cd",
  "Pmedia2sc",
  "Psc2cd",
  "V0",
  "Fskin_exposed",
  "totalSA",
  "SA_exposed",
  "BW",
  "Clmetabolismc",
  "hematocrit",
  "kgutabs",
  "Ksc2media",
  "Ksc2cd",
  "Kcd2pu",
  "Kkidney2pu",
  "Kliver2pu",
  "Krest2pu",
  "Kgut2pu",
  "Klung2pu",
  "Qcardiacc",
  "Qgfrc",
  "Qcomposite_dermalf",
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
  "Vskinc",
  "Vstratum_corneumc",
  "Vcomposite_dermalc",
  "Fraction_unbound_plasma",
  "Rblood2plasma",
  "Clmetabolism",
  "Qcardiac",
  "Qcomposite_dermal",
  "Qcomposite_dermal_exposed",
  "Qcomposite_dermal_unexposed",
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
  "Vven",
  "Vskin",
  "Vstratum_corneum",
  "Vstratum_corneum_exposed",
  "Vstratum_corneum_unexposed",
  "Vcomposite_dermal",
  "Vcomposite_dermal_exposed",
  "Vcomposite_dermal_unexposed"
)

# This function initializes the state vector for the compiled model:
model.list[["skin_2subcomp"]]$compiled.init.func <- "initmod_skin_2subcomp" #in .c file

# This is the function that calculates the derviative of the model as a function
# of time, state, and parameters:
model.list[["skin_2subcomp"]]$derivative.func <- "derivs_skin_2subcomp" #in .c file

# This is the ORDERED list of variables returned by the derivative function
# (from Model variables: Outputs):
model.list[["skin_2subcomp"]]$derivative.output.names <- c( #updated by AM, 1/21/2022
  "Cgut",
  "Cliver",
  "Cven",
  "Clung",
  "Cart",
  "Crest",
  "Ckidney",
  "Cplasma",
  "Aplasma",
  "Cstratum_corneum_exposed",
  "Cstratum_corneum_unexposed",
  "Ccomposite_dermal_exposed",
  "Ccomposite_dermal_unexposed",
  "Cmedia"
  )

model.list[["skin_2subcomp"]]$default.monitor.vars <- c(
  "Cgut",
  "Cliver",
  "Cven",
  "Clung",
  "Cart",
  "Crest",
  "Ckidney",
  "Cplasma",
  "Aplasma",
  "Cstratum_corneum_exposed",
  "Cstratum_corneum_unexposed",
  "Ccomposite_dermal_exposed",
  "Ccomposite_dermal_unexposed",
  "Cmedia",
  "Atubules",
  "Ametabolized",
  "AUC"
  )

# Allowable units assigned to dosing input:
model.list[["skin_2subcomp"]]$allowed.units.input <- list(
  "oral" = c('umol','mg','mg/kg'),
  "iv" = c('umol','mg','mg/kg'),
  "dermal" = c('mg/L','uM','umol','mg'))

# Allowable units assigned to entries in the output columns of the ode system
model.list[["skin_2subcomp"]]$allowed.units.output <- list(
  "oral" = c('uM','mg/L','umol','mg','uM*days','mg/L*days'),
  "iv" = c('uM','mg/L','umol','mg','uM*days','mg/L*days'),
  "dermal" = c('uM','mg/L','umol','mg','uM*days','mg/L*days'))

# Default set of units assigned to correspond to each of the "outputs" of 
# the model system, and possibly to other state variables to be monitored
# AUC values should also be included.
model.list[["skin_2subcomp"]]$compartment.units <- c( #updated by AM, 1/21/2022
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
  "Astratum_corneum_exposed"="umol",
  "Astratum_corneum_unexposed"="umol",
  "Acomposite_dermal_exposed"="umol",
  "Acomposite_dermal_unexposed"="umol",
  "Amedia"="umol",
  "Cgut"="uM",
  "Cliver"="uM",
  "Cven"="uM",
  "Clung"="uM",
  "Cart"="uM",
  "Crest"="uM",
  "Ckidney"="uM",
  "Cplasma"="uM",
  "Aplasma"="umol",
  "Cstratum_corneum_exposed"="uM",
  "Cstratum_corneum_unexposed"="uM",
  "Ccomposite_dermal_exposed"="uM",
  "Ccomposite_dermal_unexposed"="uM",
  "Cmedia"="uM",
  "AUC"="uM*days")

model.list[["skin_2subcomp"]]$routes <- list( #updated by AM, 1/21/2022
  "oral" = list(
    # We need to know which compartment gets the dose 
    "entry.compartment" = "Agutlumen",
    # desolve events can take the values "add" to add dose C1 <- C1 + dose,
    # "replace" to change the value C1 <- dose
    # or "multiply" to change the value to C1 <- C1*dose
    "dose.type" = "add"),
  "iv" = list(
    "entry.compartment" = "Aven",
    "dose.type" = "add"),
  "dermal" = list(
    "entry.compartment" = "Astratum_corneum_exposed",
    "dose.type" = "add")   
)

# COMMENTED BY AM, 1/21/2022
# # These parameters specify the exposure scenario simulated by the model:
# model.list[["skin_2subcomp"]]$dosing.params <- c(
#   "initial.dose",
#   "daily.dose",
#   "doses.per.day",
#   "dosing.matrix",
#   "forcings")
# 
# # We need to know which compartment gets the dose #updated by AM, 1/21/2022
# model.list[["skin_2subcomp"]]$dose.variable <- list(oral="Agutlumen",
#                                                iv="Aven", dermal = "Astratum_corneum_exposed")
# 
# # Can take the values "add" to add dose C1 <- C1 + dose, #updated by AM, 1/21/2022
# #"replace" to change the value C1 <- dose
# #or "multiply" to change the value to C1 <- C1*dose
# model.list[["skin_2subcomp"]]$dose.type <- list(oral="add",
#                                            iv="add", dermal = "add")

# This ORDERED LIST of variables are always calculated in amounts (must match
# Model variables: States in C code): 
model.list[["skin_2subcomp"]]$state.vars <- c( #updated by AM, 1/21/2022
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
  "AUC",
  "Astratum_corneum_exposed",
  "Astratum_corneum_unexposed",
  "Acomposite_dermal_exposed",
  "Acomposite_dermal_unexposed",
  "Amedia"
) 

#Parameters needed to make a prediction (this is used by get_cheminfo):
model.list[["skin_2subcomp"]]$required.params <- c(
  "Clint",
  "Funbound.plasma",
  "Pow",
  "pKa_Donor",
  "pKa_Accept",
  "MW"
)

# Do we ignore the Fups where the value was below the limit of detection?
model.list[["skin_2subcomp"]]$exclude.fup.zero <- T

#Key forcings objects and names: name of forcing function as it appears in 
#.c model code for specification to ode solver (initforc), fcontrol list
#of arguments for fine-tuning inhalation forcing function in conjunction
#with existing ode integrator methods. Forcings series handled in model 
#solver itself
model.list[["skin_2subcomp"]]$forcings.materials <- list(initforc="initforc_skin_2subcomp",
                                                    fcontrol = list(method='constant',rule=2,f=0))

