#Analytic expression for steady-state plasma concentration.
#model.list[["gas_pbtk"]]$analytic.css.func <- "calc_analytic_css_gas" # added by MB 4/8/2020

# The is the R function for generating model parameters:
model.list[["gas_pbtk"]]$parameterize.func <- "parameterize_gas_pbtk" 

# These are all the parameters returned by the R model parameterization function.
# Some of these parameters are not directly used to solve the model, but describe
# how other parameters were calculated:
model.list[["gas_pbtk"]]$param.names <- c(
  "BW",
  "Clint",
  "Clint.dist",
  "Clmetabolismc",
  "Fgutabs",
  "Fhep.assay.correction",
  "Funbound.plasma",
  "Funbound.plasma.adjustment",
  "Funbound.plasma.dist",
  "hematocrit",
  "Kblood2air", #MWL 8-1-19
  "Kgut2pu",
  "kgutabs",
  "Kkidney2pu",
  "Kliver2pu",
  "Klung2pu",
  "km", #MWL 9-13-19
  "Kmuc2air", #MWL 8-1-19
  "Krbc2pu",
  "Krest2pu",
  "kUrtc",
  "liver.density",
  "MA",
  "million.cells.per.gliver",
  "MW",
  "pKa_Accept",
  "pKa_Donor",
  "Pow",
  "Qalvc", #MWL 8-1-19
  "Qcardiacc",
  "Qgfrc",
  "Qgutf",
  "Qkidneyf",
  "Qliverf",
  "Qlungf", #MWL 9-13-19
  "Rblood2plasma",
  "Vartc",
  "Vgutc",
  "Vkidneyc",
  "Vliverc",
  "Vlungc",
  "vmax", #MWL 9-13-19
  "Vmucc", #MWL 8-1-19
  "Vrestc",
  "Vvenc"
  ) 
                    
# This subset of R parameters are needed to initially parametrize the compiled
# code for the solver: (must match ORDER under "parameters" in C code)
model.list[["gas_pbtk"]]$Rtosolvermap <- list(
  BW="BW",
  Clmetabolismc="Clmetabolismc",
  vmax = "vmax", #MWL 8-1-19
  km = "km", #MWL 8-1-19
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
  Qlungf = "Qlungf", #MWL 9-13-19
  Vartc="Vartc",
  Vgutc="Vgutc",
  Vkidneyc="Vkidneyc",
  Vliverc="Vliverc",
  Vlungc="Vlungc",
  Vrestc="Vrestc",
  Vvenc="Vvenc",
  Fraction_unbound_plasma="Funbound.plasma",
  Rblood2plasma="Rblood2plasma",
  Qalvc = "Qalvc",
  Kblood2air = "Kblood2air",
  kUrtc = "kUrtc",
  Kmuc2air = "Kmuc2air",
  Vmucc = "Vmucc"
)

# This function translates the R model parameters into the compiled model
# parameters:
model.list[["gas_pbtk"]]$compiled.parameters.init <- "getParms_gas_pbtk"

# This is the ORDERED full list of parameters used by the compiled code to 
# calculate the derivative of the system of equations describing the model 
model.list[["gas_pbtk"]]$compiled.param.names <- c(
  "BW",
  "Clmetabolismc",
  "vmax",
  "km",
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
  "Qlungf", #MWL 9-13-19
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
  "Qlung",#MWL 9-13-19
  "Qrest",
  "Vart",
  "Vgut",
  "Vkidney",
  "Vliver",
  "Vlung",
  "Vrest",
  "Vven",
  "Qalvc",
  "Qalv",
  "Kblood2air",
  "kUrtc",
  "kUrt",
  "Kmuc2air",
  "Vmucc",
  "Vmuc",
  "Vmax",
  "Km"
)

# This function initializes the state vector for the compiled model:
model.list[["gas_pbtk"]]$compiled.init.func <- "initmod_gas_pbtk"

# This is the function that calculates the derviative of the model as a function
# of time, state, and parameters:
model.list[["gas_pbtk"]]$derivative.func <- "derivs_gas_pbtk"

# This is the ORDERED list of variables returned by the derivative function
# (from Model variables: Outputs):
model.list[["gas_pbtk"]]$derivative.output.names <- c(
  "Cgut",
  "Cliver",
  "Cven",
  "Clung",
  "Cart",
  "Crest",
  "Ckidney",
  "Cplasma",
  "Aplasma",
  "Calv",
  "Cendexh",
  "Cmixexh",
  "Cmuc"
  )

model.list[["gas_pbtk"]]$default.monitor.vars <- c(
  "Cgut",
  "Cliver",
  "Cven",
  "Clung",
  "Cart",
  "Crest",
  "Ckidney",
  "Cplasma",
  "Calv",
  "Cendexh",
  "Cmixexh",
  "Cmuc",
  "Atubules",
  "Ametabolized",
  "AUC"
  )

# Allowable units:
model.list[["gas_pbtk"]]$allowed.units <- c('um', 'mg/l', 'ppm')

# These parameters specify the exposure scenario simulated by the model:
model.list[["gas_pbtk"]]$dosing.params <- c("daily.dose",
  "initial.dose",
  "doses.per.day",
  "dosing.matrix")

model.list[["gas_pbtk"]]$routes <- c("oral","iv","inhalation")

# We need to know which compartment gets the dose 
model.list[["gas_pbtk"]]$dose.variable <- list(oral="Agutlumen",
  iv="Aven", inhalation = "Amuc")

# Can take the values "add" to add dose C1 <- C1 + dose,
#"replace" to change the value C1 <- dose
#or "multiply" to change the value to C1 <- C1*dose
model.list[["gas_pbtk"]]$dose.type <- list(oral="add",
  iv="add", inhalation = "add")

# This ORDERED LIST of variables are always calculated in amounts (must match
# Model variables: States in C code): 
model.list[["gas_pbtk"]]$state.vars <- c(
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
    "Amuc"
    ) 
       
#Parameters needed to make a prediction (this is used by get_cheminfo):
model.list[["gas_pbtk"]]$required.params <- c(
  "Clint",
  "Funbound.plasma",
  "Pow",
  "pKa_Donor",
  "pKa_Accept",
  "MW"
   )

# Do we ignore the Fups where the value was below the limit of detection?
model.list[["gas_pbtk"]]$exclude.fup.zero <- T
  
#Name of forcing function as it appears in .c model code for specification to ode solver
model.list[["gas_pbtk"]]$initforc <- "initforc_gas_pbtk"
