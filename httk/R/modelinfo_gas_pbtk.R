# Physiologically-Based Toxicokinetic Model including Gas Inhalation
#
# Linakis et al. (2020), Journal of Exposure Science and Environmental Epidemiology

# Model identifier for the model.list:
THIS.MODEL <- "gas_pbtk"

#Analytic expression for steady-state plasma concentration.
#model.list[[THIS.MODEL]]$analytic.css.func <- "calc_analytic_css_gas" # added by MB 4/8/2020

# When calculating steady-state, which compartment do we test? 
# ("C" is preprended):
model.list[[THIS.MODEL]]$steady.state.compartment <- "plasma"

# What units does the analytic function return:
model.list[[THIS.MODEL]]$steady.state.units <- "mg/L"

# The is the R function for generating model parameters:
model.list[[THIS.MODEL]]$parameterize.func <- "parameterize_gas_pbtk" 

# Function called for running the model:
model.list[[THIS.MODEL]]$solve.func <- "solve_gas_pbtk"

# Here are the tissues from tissue.data that are considered (for example,
# do we include placenta or not?):
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

# Which tissues from tissue.data are not lumped together when forming
# the model: The gas PBTK model has liver, kidney, gut, and lung compartments 
# that draw info from tissue.data; everything else from alltissues should be 
# lumped.
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
  "Clint",
  "Clint.dist",
  "Clmetabolismc",
  "Fabsgut",
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
  "logHenry",
  "MA",
  "million.cells.per.gliver",
  "MW",
  "pKa_Accept",
  "pKa_Donor",
  "Pow",
  "Qalvc", #MWL 8-1-19
#  "Qalv", # SED 06-21-2021
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
model.list[[THIS.MODEL]]$Rtosolvermap <- list(
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
  Qalvc="Qalvc",
#  Qalv="Qalv", # (back up test)
  Kblood2air = "Kblood2air",
  kUrtc = "kUrtc",
  Kmuc2air = "Kmuc2air",
  Vmucc = "Vmucc"
)

# This function translates the R model parameters into the compiled model
# parameters:
model.list[[THIS.MODEL]]$compiled.parameters.init <- "getParms_gas_pbtk"

# This is the ORDERED full list of parameters used by the compiled code to 
# calculate the derivative of the system of equations describing the model 
model.list[[THIS.MODEL]]$compiled.param.names <- c(
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
  "InhMag",
  "Period",
  "Exposure",
  "kUrtc",
  "kUrt",
  "Kmuc2air",
  "Vmucc",
  "Vmuc",
  "Vmax",
  "Km"
)

# This function initializes the state vector for the compiled model:
model.list[[THIS.MODEL]]$compiled.init.func <- "initmod_gas_pbtk"

# This is the function that calculates the derviative of the model as a function
# of time, state, and parameters:
model.list[[THIS.MODEL]]$derivative.func <- "derivs_gas_pbtk"

# This is the ORDERED list of input variables given to the C code by the solver
# (from Forcing (Input) functions -- forc):
model.list[[THIS.MODEL]]$input.var.names <- c(
  "Cinhppmv"
  )
  
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
  "Aplasma",
  "Calvppmv", # SED 06-12-2021
  "Calv",
  "Cendexhppmv", # SED 06-12-2021
  "Cendexh",
  "Cmixexhppmv", # SED 06-12-2021
  "Cmixexh",
  "Cmuc"
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
  #"Calv",
  "Calvppmv", # SED 06-12-2021
  #"Cendexh",
  "Cendexhppmv", # SED 06-12-2021
  #"Cmixexh",
  "Cmixexhppmv", # SED 06-12-2021
  "Cmuc",
  "Atubules",
  "Ametabolized",
  "AUC"
  )

# Allowable units assigned to dosing input:
model.list[[THIS.MODEL]]$allowed.units.input <- list(
    "oral" = c('umol','mg','mg/kg'),
    "iv" = c('umol','mg','mg/kg'),
    "inhalation" = c('ppmv','mg/L','mg/m^3','uM','umol','mg'))

# Allowable units assigned to entries in the output columns of the ode system
model.list[[THIS.MODEL]]$allowed.units.output <- list(
       "oral" = c('uM','mg/L','ppmv','umol','mg','uM*days',
                  'mg/L*days','mg/m^3','mg/m^3*days'),
       "iv" = c('uM','mg/L','ppmv','umol','mg','uM*days','mg/L*days',
                'mg/m^3','mg/m^3*days'),
       "inhalation" = c('uM','mg/L','ppmv','umol','mg','uM*days','mg/L*days',
                        'mg/m^3','mg/m^3*days'))

# Actual (intrinsic) units assigned to each of the time dependent
# variables of the model system including state variables and any transformed
# outputs (for example, concentrations calculated from amounts.)
# AUC values should also be included.
model.list[[THIS.MODEL]]$compartment.units <- c(
                                          "Ainh"="umol",
                                          "Aexh"="umol",
                                          "Aart"="umol",
                                          "Agut"="umol",
                                          "Agutlumen"="umol",
                                          "Akidney"="umol", 
                                          "Aliver"="umol",
                                          "Alung"="umol",
                                          "Ametabolized"="umol",
                                          "Amuc"="umol",
                                          "Aplasma"="umol",
                                          "Arest"="umol",
                                          "Atubules"="umol",
                                          "AUC"="uM*days",
                                          "Aven"="umol",
                                          "Calv"="uM",
                                          "Calvppmv"="ppmv",
                                          "Cart"="uM",
                                          "Cendexh"="uM",
                                          "Cendexhppmv"="ppmv",
                                          "Cgut"="uM",
                                          "Cinhppmv"="ppmv",
                                          "Ckidney"="uM",
                                          "Cliver"="uM",
                                          "Clung"="uM",
                                          "Cmixexh"="uM",
                                          "Cmixexhppmv"="ppmv",
                                          "Cmuc"="uM",
                                          "Cplasma"="uM",
                                          "Crest"="uM",
                                          "Cven"="uM"
                                          )

# Compartment state of matter, needed for proper unit conversion, if all
# comaprtments of the same only include one state and set it to "all":
model.list[[THIS.MODEL]]$compartment.state <- list(
  liquid = c("Ainh",
             "Aexh",
             "Aart",
             "Agut",
             "Agutlumen",
             "Akidney", 
             "Aliver",
             "Alung",
             "Ametabolized",
             "Amuc",
             "Aplasma",
             "Arest",
             "Atubules",
             "AUC",
             "Aven",
             "Cart",
             "Cgut",
             "Ckidney",
             "Cliver",
             "Clung",
             "Cmuc",
             "Cplasma",
             "Crest",
             "Cven"
             ),
  gas = c("Calv",
          "Calvppmv",
          "Cmixexh",
          "Cmixexhppmv",
          "Cinhppmv",
          "Cendexh",
          "Cendexhppmv"
          )
  )

# These parameters specify the exposure scenario simulated by the model:
#model.list[[THIS.MODEL]]$dosing.params <- c(
#  "initial.dose",
#  "daily.dose",
#  "doses.per.day",
#  "dosing.matrix",
#  "forcings")

model.list[[THIS.MODEL]]$routes <- list(
  "oral" = list(
    # We need to know which compartment gets the dose 
    "entry.compartment" = "Agutlumen",
    # desolve events can take the values "add" to add dose C1 <- C1 + dose,
    # "replace" to change the value C1 <- dose
    # or "multiply" to change the value to C1 <- C1*dose
    "dose.type" = "add",
    "dosing.params" = c("daily.dose",
                        "initial.dose",
                        "doses.per.day",
                        "dosing.matrix")),
  "iv" = list(
    "entry.compartment" = "Aven",
    "dose.type" = "add",
    "dosing.params" = c("initial.dose",
                        "dosing.matrix")),
  "inhalation" = list(
    "entry.compartment" = "Cinhppmv",
    "dose.type" = "replace",
    "dosing.params" = "forcings")   
  )

# This ORDERED LIST of variables are always calculated in amounts (must match
# Model variables: States in C code): 
# NOTE: C code Input variables (i.e. those that get forcing data) should not 
#       be included in this list. See 'input.var.names' for C Input variables.
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
    "AUC",
    "Ainh", # SED 06-12-2021
    "Aexh", # SED 06-12-2021
    "Amuc"
    )        
       
#Parameters needed to make a prediction (this is used by get_cheminfo):
model.list[[THIS.MODEL]]$required.params <- c(
  "Clint",
  "Funbound.plasma",
  "Pow",
  "pKa_Donor",
  "pKa_Accept",
  "MW",
  "logHenry"
   )

# Do we ignore the Fups where the value was below the limit of detection?
model.list[[THIS.MODEL]]$exclude.fup.zero <- TRUE
  
#Key forcings objects and names: name of forcing function as it appears in 
#.c model code for specification to ode solver (initforc), fcontrol list
#of arguments for fine-tuning inhalation forcing function in conjunction
#with existing ode integrator methods. Forcings series handled in model 
#solver itself
model.list[[THIS.MODEL]]$forcings.materials <- list(initforc="initforc_gas_pbtk",
  fcontrol = list(method='constant',rule=2,f=0))

# These are the parameter names needed to describe steady-state dosing:
model.list[[THIS.MODEL]]$css.dosing.params <- list(
  inhalation=c("exp.conc", "period", "exp.duration"),
  oral=c("hourly.dose"))
  

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
# model.list[[THIS.MODEL]]$exclude.fup.zero <- TRUE

# Filter out compounds belonging to select chemical classes
model.list[[THIS.MODEL]]$chem.class.filt <- c("PFAS")

# Different systems of equations are better suited to different ODE solvers:
model.list[[THIS.MODEL]]$default.solver.method <- "lsode"
