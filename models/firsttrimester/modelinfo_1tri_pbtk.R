# Add a human gestational PBTK model for the 1st trimester of pregnancy 
# to augment the fetal_pbtk model 

#Analytic expression for steady-state plasma concentration.
#model.list[["1tri_pbtk"]]$analytic.css.func <- "calc_analytic_css_1tri_pbtk" <function not yet developed

# When calculating steady-state, which compartment do we test? 
# ("C" is preprended):
model.list[["1tri_pbtk"]]$steady.state.compartment <- "plasma"
                                              
# What units does the analytic function return:
# model.list[["1tri_pbtk"]]$steady.state.units <- "mg/L"

# Function used for generating model parameters:
model.list[["1tri_pbtk"]]$parameterize.func <- "parameterize_1tri_pbtk"

# Function called for running the model:
model.list[["1tri_pbtk"]]$solve.func <- "solve_1tri_pbtk"

# Here are the tissues from tissue.data that are considered (for example,
# do we include placenta or not? Here, yes we do). They should correspond
# in name to the names present in the tissue.data object, if the parameters
# necessary for describing the tissue/compartment aren't going to be provided
# otherwise.
model.list[["1tri_pbtk"]]$alltissues=c(
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
  "thyroid",
  # "placenta",
  "rest")

# Which tissues from tissue.data are not lumped together when forming
# the model: The 1tri_pbtk model has liver, kidney, gut, and lung compartments
# that draw info from tissue.data; everything else from alltissues should be 
# lumped.
model.list[["1tri_pbtk"]]$tissuelist=list(
  adipose = c("adipose"),
  brain = c("brain"),
  gut = c("gut"),
  liver=c("liver"),
  kidney=c("kidney"),
  lung=c("lung"),
  thyroid = c("thyroid")
  # conceptus = c("placenta")
  )
  
# These are all the parameters returned by the R model parameterization function.
# Some of these parameters are not directly used to solve the model, but describe
# how other parameters were calculated:
model.list[["1tri_pbtk"]]$param.names <- c(
# TK IVIVE parameters:
  "Fhep.assay.correction",
  "Funbound.plasma.adjustment",
  "Funbound.plasma.dist",
  "Clint.dist",
  "MA", # don't know what this parameter means 
  "million.cells.per.gliver",
  "MW",                           
  "pKa_Accept",
  "pKa_Donor",
  "pH_Plasma_mat",
  "Pow",            
# Basic PK parameters:
  "pre_pregnant_BW", # parms[0]
  "Clmetabolismc",                
  "Clint",
  "kgutabs",                      
  "Fgutabs",      
  "Funbound.plasma",              
# Maternal tissue partition coefficients:
  "Kadipose2pu",
  "Kgut2pu",
  "Kkidney2pu",
  "Kliver2pu",                    
  "Krest2pu",
  "Klung2pu",
  "Krbc2pu", # parms[10]
  "Kthyroid2pu",                  
#  "Kbrain2pu",
  "Kconceptus2pu",
# Scaled (1/kg BW) constant tissue volumes:
  "Vgutc",                       
  "Vkidneyc", 
  "Vliverc",
  "Vlungc", # parms[20]
  "Vthyroidc", 
#  "Vbrainc",                       
# Tissue Densities:
  "gut_density",  
  "kidney_density",
  "liver_density",                
  "lung_density", # parms[30]
  "thyroid_density",
  "adipose_density",              
  "ffmx_density",
  "placenta_density",
  "amnf_density",                 
  "brain_density",
# Kapraun 2019 growth parameters:
  "BW_cubic_theta1",
  "BW_cubic_theta2",
  "BW_cubic_theta3",
  "Wadipose_linear_theta0", # parms[40]
  "Wadipose_linear_theta1",
  "hematocrit_quadratic_theta0",
  "hematocrit_quadratic_theta1",
  "hematocrit_quadratic_theta2",
  "fBW_gompertz_theta0",
  "fBW_gompertz_theta1",
  "fBW_gompertz_theta2",
  "Vplacenta_cubic_theta1",
  "Vplacenta_cubic_theta2",
  "Vplacenta_cubic_theta3", # parms[50]
  "Vamnf_logistic_theta0",
  "Vamnf_logistic_theta1",
  "Vamnf_logistic_theta2",
  "Vplasma_mod_logistic_theta0",
  "Vplasma_mod_logistic_theta1",
  "Vplasma_mod_logistic_theta2",
  "Vplasma_mod_logistic_theta3",
  "venous_blood_fraction",
  "arterial_blood_fraction",
  "Qcardiac_cubic_theta0", # parms[60]
  "Qcardiac_cubic_theta1",
  "Qcardiac_cubic_theta2",
  "Qcardiac_cubic_theta3",
  "term",
  "Qgut_percent_initial",
  "Qgut_percent_terminal",
  "Qkidney_cubic_theta0",
  "Qkidney_cubic_theta1",
  "Qkidney_cubic_theta2",
  "Qkidney_cubic_theta3", # parms[70]
  "Qliver_percent_initial",
  "Qliver_percent_terminal",
  "Qthyroid_percent_initial",
  "Qthyroid_percent_terminal",
  "Qplacenta_linear_theta1",
  "Qadipose_percent_initial",
  "Qadipose_percent_terminal",
  "Qgfr_quadratic_theta0",
  "Qgfr_quadratic_theta1",
  "Qgfr_quadratic_theta2", # parms[80]
# Parameters unique to 1st tri model:
#  "Qbrain_percent_initial",
#  "Qbrain_percent_terminal", # parms[82]
  "fBW_13wks",
  "Vplacenta_13wks",
  "Vamnf_13wks",
  "Vconceptus_final", # parms[86]
  "Vconceptus_initial", 
  "Qconceptus_final", 
  "Qconceptus_initial" #parms[89]
  )

# This subset of R parameters are needed to initially parameterize the compiled
# code for the solver (must match ORDER under "parameters" in C code, even if 
# some items are omitted). 
#
# String representations of the R version of names of
# the parameters are assigned to the C variable name in this scheme.
#
# Note that because scaling is performed in the C code that we only need
# pass scaled parameters (V[tissue]c and Q[tissue]f)
model.list[["1tri_pbtk"]]$Rtosolvermap <- list(
  pre_pregnant_BW = "pre_pregnant_BW", # parms[0]
  Clmetabolismc = "Clmetabolismc",
  kgutabs = "kgutabs",
  Kkidney2pu="Kkidney2pu",
  Kliver2pu="Kliver2pu",
  Kadipose2pu="Kadipose2pu",
  Krest2pu="Krest2pu",
  Klung2pu="Klung2pu",
  Kgut2pu="Kgut2pu",
  Krbc2pu="Krbc2pu", # parms[10]
  Kthyroid2pu="Kthyroid2pu",
#  Kbrain2pu="Kbrain2pu",
  Kconceptus2pu="Kconceptus2pu",
  Vgutc = "Vgutc",
  Vkidneyc = "Vkidneyc",
  Vliverc = "Vliverc",
  Vlungc = "Vlungc", # parms[20]
  Vthyroidc = "Vthyroidc",
#  Vbrainc = "Vbrainc",
  Fraction_unbound_plasma = "Funbound.plasma",
  gut_density = "gut_density",
  kidney_density = "kidney_density",
  liver_density = "liver_density",
  lung_density = "lung_density", # parms[30]
  thyroid_density = "thyroid_density",
  adipose_density = "adipose_density",
  ffmx_density = "ffmx_density",
  placenta_density = "placenta_density",
  amnf_density = "amnf_density",                 
  brain_density = "brain_density",
  BW_cubic_theta1 = "BW_cubic_theta1",
  BW_cubic_theta2 = "BW_cubic_theta2",
  BW_cubic_theta3 = "BW_cubic_theta3",
  Wadipose_linear_theta0 = "Wadipose_linear_theta0", # parms[40]
  Wadipose_linear_theta1 = "Wadipose_linear_theta1",
  hematocrit_quadratic_theta0  = "hematocrit_quadratic_theta0",
  hematocrit_quadratic_theta1 = "hematocrit_quadratic_theta1",
  hematocrit_quadratic_theta2 = "hematocrit_quadratic_theta2",
  fBW_gompertz_theta0 = "fBW_gompertz_theta0",
  fBW_gompertz_theta1 = "fBW_gompertz_theta1",
  fBW_gompertz_theta2 = "fBW_gompertz_theta2",
  Vplacenta_cubic_theta1 = "Vplacenta_cubic_theta1",
  Vplacenta_cubic_theta2 = "Vplacenta_cubic_theta2",
  Vplacenta_cubic_theta3 = "Vplacenta_cubic_theta3", # parms[50]
  Vamnf_logistic_theta0 = "Vamnf_logistic_theta0",
  Vamnf_logistic_theta1 = "Vamnf_logistic_theta1",
  Vamnf_logistic_theta2 = "Vamnf_logistic_theta2",
  Vplasma_mod_logistic_theta0 = "Vplasma_mod_logistic_theta0",
  Vplasma_mod_logistic_theta1 = "Vplasma_mod_logistic_theta1",
  Vplasma_mod_logistic_theta2 = "Vplasma_mod_logistic_theta2",
  Vplasma_mod_logistic_theta3 = "Vplasma_mod_logistic_theta3",
  venous_blood_fraction = "venous_blood_fraction",
  arterial_blood_fraction = "arterial_blood_fraction",
  Qcardiac_cubic_theta0 = "Qcardiac_cubic_theta0", # parms[60]
  Qcardiac_cubic_theta1 = "Qcardiac_cubic_theta1",
  Qcardiac_cubic_theta2 = "Qcardiac_cubic_theta2",
  Qcardiac_cubic_theta3 = "Qcardiac_cubic_theta3",
  term = "term",
  Qgut_percent_initial = "Qgut_percent_initial",
  Qgut_percent_termina = "Qgut_percent_terminal",
  Qkidney_cubic_theta0 = "Qkidney_cubic_theta0",
  Qkidney_cubic_theta1 = "Qkidney_cubic_theta1",
  Qkidney_cubic_theta2 = "Qkidney_cubic_theta2",
  Qkidney_cubic_theta3 = "Qkidney_cubic_theta3", # parms[70]
  Qliver_percent_initial = "Qliver_percent_initial",
  Qliver_percent_terminal = "Qliver_percent_terminal",
  Qthyroid_percent_initial = "Qthyroid_percent_initial",
  Qthyroid_percent_terminal = "Qthyroid_percent_terminal",
  Qplacenta_linear_theta1 = "Qplacenta_linear_theta1",
  Qadipose_percent_initial = "Qadipose_percent_initial",
  Qadipose_percent_terminal = "Qadipose_percent_terminal",
  Qgfr_quadratic_theta0 = "Qgfr_quadratic_theta0",
  Qgfr_quadratic_theta1 = "Qgfr_quadratic_theta1",
  Qgfr_quadratic_theta2 = "Qgfr_quadratic_theta2" # parms[80]
  # Qbrain_percent_initial = "Qbrain_percent_initial",
  # Qbrain_percent_terminal = "Qbrain_percent_terminal", # parms[82]
  # fBW_13wks = "fBW_13wks",
  # Vplacenta_13wks = "Vplacenta_13wks",
  # Vamnf_13wks = "Vamnf_13wks",
  # Vconceptus_final = "Vconceptus_final",
  # Vconceptus_initial = "Vconceptus_initial",
  # Qconceptus_final = "Qconceptus_final",
  # Qconceptus_initial = "Qconceptus_initial"# parms[89]
)

# This function translates the R model parameters into the compiled model
# parameters:
model.list[["1tri_pbtk"]]$compiled.parameters.init <- "getParms_firsttrimester" 

# This needs to be a global variable so that R CMD check --as-cran can test
# the code (the HTTK package does not use this):
compiled_parameters_init <- "getParms_firsttrimester" 

# This is the ORDERED full list of parameters used by the compiled code to 
# calculate the derivative of the system of equations describing the model.
# The order agrees with the order present in the associated .model / .C 
# file's listing of parameters. 
model.list[["1tri_pbtk"]]$compiled.param.names <- c(
  "pre_pregnant_BW",
  "Clmetabolismc",
  "Clmetabolism",
  "kgutabs",
  "Kkidney2pu",
  "Kliver2pu",
  "Kadipose2pu",
  "Krest2pu",
  "Klung2pu",
  "Kgut2pu",
  "Krbc2pu", # parms[10]
  "Kthyroid2pu",
  # "Kbrain2pu",
  "Kconceptus2pu",
  "Vgutc",
  "Vgut",
  "Vkidneyc",
  "Vkidney",
  "Vliverc",
  "Vliver",
  "Vlungc", # parms[20]
  "Vlung",
  "Vthyroidc",
  "Vthyroid",
  # "Vbrainc",
  # "Vbrain",
  "Fraction_unbound_plasma",
  "gut_density",
  "kidney_density",
  "liver_density",
  "lung_density", # parms[30]
  "thyroid_density",
  "adipose_density",
  "ffmx_density",
  "placenta_density",
  "amnf_density",
  "brain_density",
  "BW_cubic_theta1",
  "BW_cubic_theta2",
  "BW_cubic_theta3",
  "Wadipose_linear_theta0", # parms[40]
  "Wadipose_linear_theta1",
  "hematocrit_quadratic_theta0",
  "hematocrit_quadratic_theta1",
  "hematocrit_quadratic_theta2",
  "fBW_gompertz_theta0",
  "fBW_gompertz_theta1",
  "fBW_gompertz_theta2",
  "Vplacenta_cubic_theta1",
  "Vplacenta_cubic_theta2",
  "Vplacenta_cubic_theta3", # parms[50]
  "Vamnf_logistic_theta0",
  "Vamnf_logistic_theta1",
  "Vamnf_logistic_theta2",
  "Vplasma_mod_logistic_theta0",
  "Vplasma_mod_logistic_theta1",
  "Vplasma_mod_logistic_theta2",
  "Vplasma_mod_logistic_theta3",
  "venous_blood_fraction",
  "arterial_blood_fraction",
  "Qcardiac_cubic_theta0", # parms[60]
  "Qcardiac_cubic_theta1",
  "Qcardiac_cubic_theta2",
  "Qcardiac_cubic_theta3",
  "term",
  "Qgut_percent_initial",
  "Qgut_percent_terminal",
  "Qkidney_cubic_theta0",
  "Qkidney_cubic_theta1",
  "Qkidney_cubic_theta2",
  "Qkidney_cubic_theta3", # parms[70]
  "Qliver_percent_initial",
  "Qliver_percent_terminal",
  "Qthyroid_percent_initial",
  "Qthyroid_percent_terminal",
  "Qplacenta_linear_theta1",
  "Qadipose_percent_initial",
  "Qadipose_percent_terminal",
  "Qgfr_quadratic_theta0",
  "Qgfr_quadratic_theta1",
  "Qgfr_quadratic_theta2", # parms[80]
  # "Qbrain_percent_initial",
  # "Qbrain_percent_terminal", # parms[82]
  "fBW_13wks",
  "Vplacenta_13wks", 
  "Vamnf_13wks", 
  "Vconceptus_final",  
  "Vconceptus_initial", 
  "Qconceptus_final", 
  "Qconceptus_initial" # parms[89]
)

# This function initializes the state vector for the compiled model:
model.list[["1tri_pbtk"]]$compiled.init.func <- "initmod_firsttrimester" 

# This is the function that calculates the derivative of the model as a function
# of time, state, and parameters:
model.list[["1tri_pbtk"]]$derivative.func <- "derivs_firsttrimester" 

# This is the ORDERED list of variables returned by the derivative function
# (from Model variables: Outputs):
model.list[["1tri_pbtk"]]$derivative.output.names <- c(
  "Cgut",
  "Cliver",
  "Ckidney",
  "Clung",
  "Cven",
  "Cart",
  "Cadipose",
  "Cthyroid",
  "Crest",
  # "Cbrain",
  "Cconceptus",
  #"Vconceptus",
  #"Qconceptus",
  "Cplasma",
  "Aplasma",
  "Rblood2plasma"
  )

#Which variables to track by default (should be able to build this from
#state vars and outputs):
model.list[["1tri_pbtk"]]$default.monitor.vars <- c(
  "Cgut",
  "Cliver",
  "Ckidney",
  "Clung",
  "Cven",
  "Cart",
  "Cadipose",
  "Cthyroid",
  "Crest",
  # "Cbrain",
  "Cconceptus",
  #"Vconceptus",
  #"Qconceptus",
  "Cplasma",
  "Aplasma",
  "Rblood2plasma",
  "Atubules",
  "Ametabolized",
  "AUC"
   )

# Allowable units assigned to dosing input:
model.list[["1tri_pbtk"]]$allowed.units.input <- list(
  "oral" = c('umol','mg','mg/kg'),
  "iv" = c('umol','mg','mg/kg'))

# Allowable units assigned to entries in the output columns of the ode system
model.list[["1tri_pbtk"]]$allowed.units.output <- list(
  "oral" = c('uM','mg/L','umol','mg','uM*days',
             'mg/L*days',"unitless"),
  "iv" = c('uM','mg/L','umol','mg','uM*days',
             'mg/L*days',"unitless"))

## These parameters specify the exposure scenario simulated by the model:
model.list[["1tri_pbtk"]]$routes <- list(
  "oral" = list(
# We need to know which compartment gets the dose 
    "entry.compartment" = "Agutlumen",
# desolve events can take the values "add" to add dose C1 <- C1 + dose,
# "replace" to change the value C1 <- dose
# or "multiply" to change the value to C1 <- C1*dose
    "dose.type" = "add"),
  "iv" = list(
    "entry.compartment" = "Aven",
    "dose.type" = "add")
  )

# ORDERED LIST of state variables (must match Model variables: 
# States in C code, each of which is associated with a differential equation),
# mostly calculated in amounts, though AUC (area under plasma concentration
# curve) also appears here: 
model.list[["1tri_pbtk"]]$state.vars <- c(
  "Agutlumen", # 0x00000
  "Agut",
  "Aliver",
  "Akidney",
  "Alung",
  "Aven",
  "Aart", # 0x00005
  "Aadipose",
  "Athyroid",
  "Arest",
  # "Abrain",
  "Aconceptus", # 0x0000a
  "Atubules",
  "Ametabolized",
  "AUC"# 0x0000e
  ) 

# Actual (intrinsic) units assigned to each of the time dependent
# variables of the model system including state variables and any transformed
# outputs (for example, concentrations calculated from amounts.)
# AUC values should also be included.
model.list[["1tri_pbtk"]]$compartment.units <- c(
  "Agutlumen" = "umol",
  "Agut" = "umol",
  "Aliver" = "umol",
  "Aven" = "umol",
  "Alung" = "umol",
  "Aart" = "umol",
  "Aadipose" = "umol",
  "Arest" = "umol",
  "Akidney" = "umol",
  # "Abrain" = "umol",
  "Aconceptus" = "umol",
  "Atubules" = "umol",
  "Ametabolized" = "umol",
  "AUC" = "uM*days",
  "Athyroid" = "umol",
  "Aplasma" = "umol",
  "Cgut" = "uM",
  "Cliver" = "uM",
  "Cven" = "uM",
  "Clung" = "uM",
  "Cart" = "uM",
  "Cadipose" = "uM",
  "Crest" = "uM",
  "Ckidney" = "uM",
  # "Cbrain" = "uM",
  "Cconceptus" = "uM",
  "Cplasma" = "uM",
  "Cthyroid" = "uM",
  "Rblood2plasma" = "unitless"
  #"Vconceptus" = "L", 
  #"Qconceptus" = "L/d"
  )

       
#Parameters needed to make a prediction (this is used by get_cheminfo):
model.list[["1tri_pbtk"]]$required.params <- c(
  "Clint",
  "Funbound.plasma",
  "Pow",
  "pKa_Donor",
  "pKa_Accept",
  "MW"
  )

# Do we need to recalculate partition coefficients when doing Monte Carlo?
model.list[["1tri_pbtk"]]$calcpc <- TRUE
  

# Do we need to recalculate first pass metabolism when doing Monte Carlo?
model.list[["1tri_pbtk"]]$firstpass <- FALSE

# Do we ignore the Fups where the value was below the limit of detection?
model.list[["1tri_pbtk"]]$exclude.fup.zero <- TRUE

# These are the parameter names needed to describe steady-state dosing:
model.list[["1tri_pbtk"]]$css.dosing.params <- c("hourly.dose")

# Filter out volatile compounds with Henry's Law Constant Threshold
model.list[["1tri_pbtk"]]$log.henry.threshold <- c(-4.5)

# Filter out compounds belonging to select chemical classes
model.list[["1tri_pbtk"]]$chem.class.filt <- c("PFAS")
