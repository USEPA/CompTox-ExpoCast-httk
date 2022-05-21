#Analytic expression for steady-state plasma concentration.
#model.list[["dermal_1subcomp"]]$analytic.css.func <- "calc_analytic_css_dermal_1subcomp" # added by MB 4/8/2020

# The is the R function for generating model parameters:
model.list[["dermal_1subcomp"]]$parameterize.func <- "parameterize_dermal_pbtk" 

# Function called for running the model:
model.list[["dermal_1subcomp"]]$solve.func <- "solve_dermal_pbtk"

# Here are the tissues from tissue.data that are considered (for example,
# do we include placenta or not?):
model.list[["dermal_1subcomp"]]$alltissues=c(
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
model.list[["dermal_1subcomp"]]$tissuelist=list( 
  liver=c("liver"),
  kidney=c("kidney"),
  lung=c("lung"),
  gut=c("gut"),
  skin=c("skin"))
                    
# This subset of R parameters are needed to initially parameterize the compiled
# code for the solver: (must match ORDER under "parameters" in C code)
model.list[["dermal_1subcomp"]]$Rtosolvermap <- list( 
  skin_depth = "skin_depth",
  Fskin_exposed = "Fskin_exposed",
  totalSA = "totalSA",
  Kp = "Kp",
  Kskin2media = "Kskin2media",
  BW = "BW",
  Clmetabolismc = "Clmetabolismc",
  hematocrit = "hematocrit",
  kgutabs = "kgutabs",
  Kkidney2pu = "Kkidney2pu",
  Kliver2pu = "Kliver2pu",
  Krest2pu = "Krest2pu",
  Kgut2pu = "Kgut2pu",
  Klung2pu = "Klung2pu",
  Kskin2pu = "Kskin2pu",
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
  Fraction_unbound_plasma = "Funbound.plasma", #different to match R httk code
  Rblood2plasma = "Rblood2plasma"
)

# This function translates the R model parameters into the compiled model
# parameters:
model.list[["dermal_1subcomp"]]$compiled.parameters.init <- "getParms_dermal_1subcomp" #in .c file

# This is the ORDERED full list of parameters used by the compiled code to 
# calculate the derivative of the system of equations describing the model 
model.list[["dermal_1subcomp"]]$compiled.param.names <- c( 
  "skin_depth",
  "Fskin_exposed",
  "totalSA",
  "SA_exposed",
  "Kp",
  "Kskin2media",
  "BW",
  "Clmetabolismc",
  "hematocrit",
  "kgutabs",
  "Kkidney2pu",
  "Kliver2pu",
  "Krest2pu",
  "Kgut2pu",
  "Klung2pu",
  "Kskin2pu",
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
  "Fraction_unbound_plasma",
  "Rblood2plasma",
  "Clmetabolism",
  "Qcardiac",
  "Qgfr",
  "Qskin",
  "Qskin_exposed",
  "Qskin_unexposed",
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
  "Vskin_unexposed"
)

# This function initializes the state vector for the compiled model:
model.list[["dermal_1subcomp"]]$compiled.init.func <- "initmod_dermal_1subcomp" #in .c file

# This is the function that calculates the derviative of the model as a function
# of time, state, and parameters:
model.list[["dermal_1subcomp"]]$derivative.func <- "derivs_dermal_1subcomp" #in .c file

# This is the ORDERED list of input variables given to the C code by the solver
# (from Forcing (Input) functions -- forc):
model.list[["dermal_1subcomp"]]$input.var.names <- c(
  "Vmedia"
)

# This is the ORDERED list of variables returned by the derivative function
# (from Model variables: Outputs):
model.list[["dermal_1subcomp"]]$derivative.output.names <- c( 
  "Cgut",
  "Cliver",
  "Cven",
  "Clung",
  "Cart",
  "Crest",
  "Ckidney",
  "Cplasma",
  "Aplasma",
  "Cskin_exposed",
  "Cskin_unexposed",
  "Cmedia"
  )

model.list[["dermal_1subcomp"]]$default.monitor.vars <- c(
  "Cgut",
  "Cliver",
  "Cven",
  "Clung",
  "Cart",
  "Crest",
  "Ckidney",
  "Cplasma",
  "Aplasma",
  "Cskin_exposed",
  "Cskin_unexposed",
  "Cmedia",
  "Ain",
  "Atubules",
  "Ametabolized",
  "AUC"
  )

# Allowable units assigned to dosing input:
model.list[["dermal_1subcomp"]]$allowed.units.input <- list(
  "oral" = c('umol','mg','mg/kg'), 
  "iv" = c('umol','mg','mg/kg'), 
  "dermal" = c('mg/L','uM','umol','mg'))

# Allowable units assigned to entries in the output columns of the ode system
model.list[["dermal_1subcomp"]]$allowed.units.output <- list(
  "oral" = c('uM','mg/L','umol','mg','uM*days','mg/L*days'), 
  "iv" = c('uM','mg/L','umol','mg','uM*days','mg/L*days'), 
  "dermal" = c('uM','mg/L','umol','mg','uM*days','mg/L*days'))

# Default set of units assigned to correspond to each of the "outputs" of 
# the model system, and possibly to other state variables to be monitored
# AUC values should also be included.
model.list[["dermal_1subcomp"]]$compartment.units <- c( 
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
  "Askin_exposed"="umol",
  "Askin_unexposed"="umol",
  "Amedia"="umol",
  "Ain"="umol",
  "Cgut"="uM",
  "Cliver"="uM",
  "Cven"="uM",
  "Clung"="uM",
  "Cart"="uM",
  "Crest"="uM",
  "Ckidney"="uM",
  "Cplasma"="uM",
  "Aplasma"="umol",
  "Cskin_exposed"="uM",
  "Cskin_unexposed"="uM",
  "Cmedia"="uM",
  "AUC"="uM*days")

model.list[["dermal_1subcomp"]]$routes <- list( 
  "oral" = list( # not used? -AEM, March 2022
    # We need to know which compartment gets the dose
    "entry.compartment" = "Agutlumen",
    # desolve events can take the values "add" to add dose C1 <- C1 + dose,
    # "replace" to change the value C1 <- dose
    # or "multiply" to change the value to C1 <- C1*dose
    "dose.type" = "add"),
  "iv" = list( # not used? -AEM, March 2022
    "entry.compartment" = "Aven",
    "dose.type" = "add"),
  "dermal" = list(
    "entry.compartment" = "Amedia",
    "dose.type" = "replace")   
)


# These parameters specify the exposure scenario simulated by the model:
model.list[["dermal_1subcomp"]]$dosing.params <- c(
  "initial.dose",
  #"daily.dose",
  #"doses.per.day",
  "dosing.matrix",
  "forcings")


# This ORDERED LIST of variables are always calculated in amounts (must match
# Model variables: States in C code): 
model.list[["dermal_1subcomp"]]$state.vars <- c( 
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
  "Askin_exposed",
  "Askin_unexposed",
  "Amedia",
  "Ain"
) 

#Parameters needed to make a prediction (this is used by get_cheminfo):
model.list[["dermal_1subcomp"]]$required.params <- c(
  "Clint",
  "Funbound.plasma",
  "Pow",
  "pKa_Donor",
  "pKa_Accept",
  "MW"
)

# Do we ignore the Fups where the value was below the limit of detection?
model.list[["dermal_1subcomp"]]$exclude.fup.zero <- T

#Key forcings objects and names: name of forcing function as it appears in 
#.c model code for specification to ode solver (initforc), fcontrol list
#of arguments for fine-tuning inhalation forcing function in conjunction
#with existing ode integrator methods. Forcings series handled in model 
#solver itself
model.list[["dermal_1subcomp"]]$forcings.materials <- list(initforc="initforc_dermal_1subcomp",
                                                    fcontrol = list(method='constant',rule=2,f=0))

