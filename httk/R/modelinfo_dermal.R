#Analytic expression for steady-state plasma concentration.
#model.list[["dermal"]]$analytic.css.func <- "calc_analytic_css_dermal" 

# The is the R function for generating model parameters:
model.list[["dermal"]]$parameterize.func <- "parameterize_dermal_pbtk" 

# Function called for running the model:
model.list[["dermal"]]$solve.func <- "solve_dermal_pbtk"

# Here are the tissues from tissue.data that are considered (for example,
# do we include placenta or not?):
model.list[["dermal"]]$alltissues=c(
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

# Which tissues from tissue.data are not lumped together when forming
# the model: The dermal PBTK model has liver, kidney, gut, lung, and skin compartments 
# that draw info from tissue.data; everything else from alltissues should be 
# lumped.
model.list[["dermal"]]$tissuelist=list(
  liver=c("liver"),
  kidney=c("kidney"),
  lung=c("lung"),
  gut=c("gut"),
  skin=c("skin"))
                    
# This subset of R parameters are needed to initially parameterize the compiled
# code for the solver: (must match ORDER under "parameters" in C code)
model.list[["dermal"]]$Rtosolvermap <- list(
  skin_depth = "skin_depth",
  InfiniteDose = "InfiniteDose",
  Fskin_depth_sc = "Fskin_depth_sc",
  Pvehicle2sc = "Pvehicle2sc",
  Psc2ed = "Psc2ed",
  Fskin_exposed = "Fskin_exposed",
  totalSA = "totalSA",
  BW = "BW",
  Clmetabolismc = "Clmetabolismc",
  hematocrit = "hematocrit",
  kgutabs = "kgutabs",
  Ksc2vehicle = "Ksc2vehicle",
  Ksc2ed = "Ksc2ed",
  Ked2pu = "Ked2pu",
  Kkidney2pu = "Kkidney2pu",
  Kliver2pu = "Kliver2pu",
  Krest2pu = "Krest2pu",
  Kgut2pu = "Kgut2pu",
  Klung2pu = "Klung2pu",
  Kblood2air = "Kblood2air",
  Qalvc = "Qalvc",
  Qcardiacc = "Qcardiacc",
  Qgfrc = "Qgfrc",
  Qskinf = "Qskinf",
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
  Vskin_scc = "Vskin_scc", # usually set to = Vskinc*Fskin_depth_sc in parameterize function
  Fraction_unbound_plasma = "Funbound.plasma", #different to match R httk code
  Rblood2plasma = "Rblood2plasma"
)

# These are all the parameters returned by the R model parameterization function.
# Some of these parameters are not directly used to solve the model, but describe
# how other parameters were calculated:
model.list[["pbtk"]]$param.names <- c(
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
  "Vvenc",
# Added by AEM:
  "totalSA",
  "skin_depth",
  "Fskin_exposed",
  "Fskin_depth_sc",
  "Fskin_depth_ed",
  "Pvehicle2sc", 
  "Psc2ed",
  "Ksc2vehicle",
  "Ksc2ed",
  "Vskin_scc",
  "Vskin_edc",
  "InfiniteDose"
  )


# This function translates the R model parameters into the compiled model
# parameters:
model.list[["dermal"]]$compiled.parameters.init <- "getParms_dermal" #in .c file

# This is the ORDERED full list of parameters used by the compiled code to 
# calculate the derivative of the system of equations describing the model 
model.list[["dermal"]]$compiled.param.names <- c(
  "skin_depth",
  "InfiniteDose",
  "Fskin_depth_sc",
  "Fskin_depth_ed",
  "Pvehicle2sc",
  "Psc2ed",
  "Fskin_exposed",
  "totalSA",
  "SA_exposed",
  "BW",
  "Clmetabolismc",
  "hematocrit",
  "kgutabs",
  "Ksc2vehicle",
  "Ksc2ed",
  "Ked2pu",
  "Kkidney2pu",
  "Kliver2pu",
  "Krest2pu",
  "Kgut2pu",
  "Klung2pu",
  "Kblood2air",
  "Qalvc",
  "Qcardiacc",
  "Qgfrc",
  "Qskinf",
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
  "Vskin_scc",
  "Vskin_edc",
  "Fraction_unbound_plasma",
  "Rblood2plasma",
  "Clmetabolism",
  "Qalv",
  "Qcardiac",
  "Qskin",
  "Qskin_exposed",
  "Qskin_unexposed",
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
  "Vskin_exposed",
  "Vskin_unexposed",
  "Vskin_sc",
  "Vskin_sc_exposed",
  "Vskin_sc_unexposed",
  "Vskin_ed",
  "Vskin_ed_exposed",
  "Vskin_ed_unexposed"
)

# This function initializes the state vector for the compiled model:
model.list[["dermal"]]$compiled.init.func <- "initmod_dermal" #in .c file

# This is the function that calculates the derviative of the model as a function
# of time, state, and parameters:
model.list[["dermal"]]$derivative.func <- "derivs_dermal" #in .c file

# This is the ORDERED list of input variables given to the C code by the solver
# (from Forcing (Input) functions -- forc):
model.list[["dermal"]]$input.var.names <- c(
  "Vvehicle"
)

# This is the ORDERED list of variables returned by the derivative function
# (from Model variables: Outputs):
model.list[["dermal"]]$derivative.output.names <- c(
  "Cgut",
  "Cliver",
  "Cven",
  "Clung",
  "Cart",
  "Crest",
  "Ckidney",
  "Cplasma",
  "Aplasma",
  "Cskin_sc_exposed",
  "Cskin_sc_unexposed",
  "Cskin_ed_exposed",
  "Cskin_ed_unexposed",
  "Cvehicle"
  )

model.list[["dermal"]]$default.monitor.vars <- c(
  "Cgut",
  "Cliver",
  "Cven",
  "Clung",
  "Cart",
  "Crest",
  "Ckidney",
  "Cplasma",
  "Aplasma",
  "Cskin_sc_exposed",
  "Cskin_sc_unexposed",
  "Cskin_ed_exposed",
  "Cskin_ed_unexposed",
  "Cvehicle",
  "Atubules",
  "Ametabolized",
  "AUC"
  )

# Allowable units assigned to dosing input:
model.list[["dermal"]]$allowed.units.input <- list(
  "oral" = c('umol','mg','mg/kg'), 
  "iv" = c('umol','mg','mg/kg'), 
  "dermal" = c('umol','mg','mg/kg'), 
  "dermal.washoff" = c('umol','mg','mg/kg'), 
  "dermal.InfiniteDose" = c('mg/L','uM','ppm','ppmw','ug/g'))

# Allowable units assigned to entries in the output columns of the ode system
model.list[["dermal"]]$allowed.units.output <- list(
  "oral" = c('uM','mg/L','umol','mg','uM*days','mg/L*days'),
  "iv" = c('uM','mg/L','umol','mg','uM*days','mg/L*days'), 
  "dermal" = c('uM','mg/L','umol','mg','uM*days','mg/L*days'), 
  "dermal.washoff" = c('uM','mg/L','umol','mg','uM*days','mg/L*days'),
  "dermal.InfiniteDose" = c('uM','mg/L','umol','mg','uM*days','mg/L*days'))

# Default set of units assigned to correspond to each of the "outputs" of 
# the model system, and possibly to other state variables to be monitored
# AUC values should also be included.
model.list[["dermal"]]$compartment.units <- c( 
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
  "Askin_sc_exposed"="umol",
  "Askin_sc_unexposed"="umol",
  "Askin_ed_exposed"="umol",
  "Askin_ed_unexposed"="umol",
  "Avehicle"="umol",
  "Ain"="umol",
  "Aexhaled"="umol",
  "Cvehicle_infinite"="uM",
  "Cgut"="uM",
  "Cliver"="uM",
  "Cven"="uM",
  "Clung"="uM",
  "Cart"="uM",
  "Crest"="uM",
  "Ckidney"="uM",
  "Cplasma"="uM",
  "Aplasma"="umol",
  "Cskin_sc_exposed"="uM",
  "Cskin_sc_unexposed"="uM",
  "Cskin_ed_exposed"="uM",
  "Cskin_ed_unexposed"="uM",
  "Cvehicle"="uM",
  "AUC"="uM*days")

model.list[["dermal"]]$routes <- list( 
  "oral" = list( # not used? - AEM, March 2022
    # We need to know which compartment gets the dose
    "entry.compartment" = "Agutlumen",
    # desolve events can take the values "add" to add dose C1 <- C1 + dose,
    # "replace" to change the value C1 <- dose
    # or "multiply" to change the value to C1 <- C1*dose
    "dose.type" = "add"),
  "iv" = list( # not used? - AEM, March 2022
    "entry.compartment" = "Aven",
    "dose.type" = "add"),
  "dermal" = list(
    "entry.compartment" = "Avehicle",
    "dose.type" = "add"),
  "dermal.washoff" = list(
    "entry.compartment" = "Avehicle",
    "dose.type" = "replace"),
  "dermal.InfiniteDose" = list( 
    "entry.compartment" = "Cvehicle_infinite",
    "dose.type" = "replace")  
)

# These parameters specify the exposure scenario simulated by the model:
model.list[["dermal"]]$dosing.params <- c(
  "initial.dose",
  "dosing.matrix",
  "forcings")

# This ORDERED LIST of variables are always calculated in amounts (must match
# Model variables: States in C code): 
model.list[["dermal"]]$state.vars <- c( 
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
  "Askin_sc_exposed",
  "Askin_sc_unexposed",
  "Askin_ed_exposed",
  "Askin_ed_unexposed",
  "Avehicle",
  "Ain",
  "Aexhaled",
  "Cvehicle_infinite"
) 

#Parameters needed to make a prediction (this is used by get_cheminfo):
model.list[["dermal"]]$required.params <- c(
  "Clint",
  "Funbound.plasma",
  "Pow",
  "pKa_Donor",
  "pKa_Accept",
  "MW"
)

# Do we ignore the Fups where the value was below the limit of detection?
model.list[["dermal"]]$exclude.fup.zero <- T

#Key forcings objects and names: name of forcing function as it appears in 
#.c model code for specification to ode solver (initforc), fcontrol list
#of arguments for fine-tuning inhalation forcing function in conjunction
#with existing ode integrator methods. Forcings series handled in model 
#solver itself
model.list[["dermal"]]$forcings.materials <- list(initforc="initforc_dermal",
                                                    fcontrol = list(method='constant',rule=2,f=0))


