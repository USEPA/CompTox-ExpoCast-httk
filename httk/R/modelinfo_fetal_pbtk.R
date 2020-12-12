# Add this model to the list of models:

#Analytic expression for steady-state plasma concentration.
#model.list[["fetal_pbtk"]]$analytic.css.func <- "calc_analytic_css_fetal_pbtk" <function not yet developed

# The is the R function for generating model parameters:
model.list[["fetal_pbtk"]]$parameterize.func <- "parameterize_fetal_pbtk"

# Function used for generating model parameters:
model.list[["fetal_pbtk"]]$parameterize.func <- "parameterize_fetal_pbtk"

# Function called for running the model:
model.list[["fetal_pbtk"]]$solve.func <- "solve_fetal_pbtk"

# Here are the tissues from tissue.data that are considered (for example,
# do we include placenta or not? Here, yes we do):
model.list[["fetal_pbtk"]]$alltissues=c(
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
  "placenta")

# Which tissues from tissue.data are not lumped together when forming
# the model: The fetal_pbtk model has liver, kidney, gut, and lung compartments
# that draw info from tissue.data; everything else from alltissues should be 
# lumped.
model.list[["fetal_pbtk"]]$tissuelist=list(
  adipose = c("adipose"),
  brain = c("brain"),
  gut = c("gut"),
  liver=c("liver"),
  kidney=c("kidney"),
  lung=c("lung"),
  thyroid = c("thyroid"),
  placenta = c("placenta")
  )
  
# These are all the parameters returned by the R model parameterization function.
# Some of these parameters are not directly used to solve the model, but describe
# how other parameters were calculated:
model.list[["fetal_pbtk"]]$param.names <- c(
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
  "Kadipose2pu",
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
  "Rblood2plasma",
  "Vthyroidc",
  "Vkidneyc",
  "Vgutc",
  "Vliverc",
  "Vlungc",
  "Kfthyroid2pu",
  "Kthyroid2pu",
  "Kfliver2pu",
  "Kfkidney2pu",
  "Kfrest2pu",
  "Kfgut2pu",
  "Kflung2pu",
  "Kfbrain2pu",
  
  "Fraction_unbound_plasma_fetus",
  
  "pre_pregnant_BW",
  "gut_density",
  "kidney_density",
  "liver_density",
  "lung_density",
  "thyroid_density",
  "adipose_density",
  "ffmx_density",
  "placenta_density",
  "amnf_density",
  "brain_density",
  "BW_cubic_theta1",
  "BW_cubic_theta2",
  "BW_cubic_theta3",
  "Wadipose_linear_theta0",
  "Wadipose_linear_theta1",
  "Wfkidney_gompertz_theta0",
  "Wfkidney_gompertz_theta1",
  "Wfkidney_gompertz_theta2",
  "Wfthyroid_gompertz_theta0",
  "Wfthyroid_gompertz_theta1",
  "Wfthyroid_gompertz_theta2",
  "Wfliver_gompertz_theta0",
  "Wfliver_gompertz_theta1",
  "Wfliver_gompertz_theta2",
  "Wfbrain_gompertz_theta0",
  "Wfbrain_gompertz_theta1",
  "Wfbrain_gompertz_theta2",
  "Wfgut_gompertz_theta0",
  "Wfgut_gompertz_theta1",
  "Wfgut_gompertz_theta2",
  "Wflung_gompertz_theta0",
  "Wflung_gompertz_theta1",
  "Wflung_gompertz_theta2",
  "hematocrit_quadratic_theta0",
  "hematocrit_quadratic_theta1",
  "hematocrit_quadratic_theta2",
  "fhematocrit_cubic_theta1",
  "fhematocrit_cubic_theta2",
  "fhematocrit_cubic_theta3",
  "fBW_gompertz_theta0",
  "fBW_gompertz_theta1",
  "fBW_gompertz_theta2",
  "Vplacenta_cubic_theta1",
  "Vplacenta_cubic_theta2",
  "Vplacenta_cubic_theta3",
  "Vamnf_logistic_theta0",
  "Vamnf_logistic_theta1",
  "Vamnf_logistic_theta2",
  "Vplasma_mod_logistic_theta0",
  "Vplasma_mod_logistic_theta1",
  "Vplasma_mod_logistic_theta2",
  "Vplasma_mod_logistic_theta3",
  "venous_blood_fraction",
  "arterial_blood_fraction",
  "fblood_weight_ratio",
  "Qcardiac_cubic_theta0",
  "Qcardiac_cubic_theta1",
  "Qcardiac_cubic_theta2",
  "Qcardiac_cubic_theta3",
  "term",
  "Qgut_percent_initial",
  "Qgut_percent_terminal",
  "Qkidney_cubic_theta0",
  "Qkidney_cubic_theta1",
  "Qkidney_cubic_theta2",
  "Qkidney_cubic_theta3",
  "Qliver_percent_initial",
  "Qliver_percent_terminal",
  "Qthyroid_percent_initial",
  "Qthyroid_percent_terminal",
  "Qplacenta_linear_theta1",
  "Qadipose_percent_initial",
  "Qadipose_percent_terminal",
  "Qgfr_quadratic_theta0",
  "Qgfr_quadratic_theta1",
  "Qgfr_quadratic_theta2",
  "Qfrvtl_logistic_theta0",
  "Qfrvtl_logistic_theta1",
  "Qfrvtl_logistic_theta2",
  "Qflvtl_logistic_theta0",
  "Qflvtl_logistic_theta1",
  "Qflvtl_logistic_theta2",
  "Qfda_logistic_theta0",
  "Qfda_logistic_theta1",
  "Qfda_logistic_theta2",
  "Qfplacenta_logistic_theta0",
  "Qfplacenta_logistic_theta1",
  "Qfplacenta_logistic_theta2",
  "Qfdv_gompertz_theta0",
  "Qfdv_gompertz_theta1",
  "Qfdv_gompertz_theta2",
  "Qfnonplacental_percent",
  "Qfgut_percent",
  "Qfkidney_percent",
  "Qfbrain_percent",
  "Qbrain_percent",
  "Qkidney_percent",
  "Qgut_percent",
  "Qfliver_percent",
  "Qfthyroid_percent"
  )


# This subset of R parameters are needed to initially parameterize the compiled
# code for the solver (must match ORDER under "parameters" in C code, even if 
# some items are omitted). String representations of the R version of names of
# the parameters are assigned to the C variable name in this scheme.
model.list[["fetal_pbtk"]]$Rtosolvermap <- list(
  pre_pregnant_BW = "pre_pregnant_BW",
  Clmetabolismc = "Clmetabolismc",
  kgutabs = "kgutabs",
  Vgutc = "Vgutc",
  Vkidneyc = "Vkidneyc",
  Vliverc = "Vliverc",
  Vlungc = "Vlungc",
  Vthyroidc = "Vthyroidc",
  Fraction_unbound_plasma = "Funbound.plasma",
  Rblood2plasma = "Rblood2plasma",
  gut_density = "gut_density",
  kidney_density = "kidney_density",
  liver_density = "liver_density",
  lung_density = "lung_density",
  thyroid_density = "thyroid_density"
)

# This function translates the R model parameters into the compiled model
# parameters:
model.list[["fetal_pbtk"]]$compiled.parameters.init <- "getParmsfetalpbtk"

# This is the ORDERED full list of parameters used by the compiled code to 
# calculate the derivative of the system of equations describing the model.
# The order agrees with the order present in the associated .model / .C 
# file's listing of parameters. 
model.list[["fetal_pbtk"]]$compiled.param.names <- c(
  "pre_pregnant_BW",
  "Clmetabolismc",
  "Clmetabolism",
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
  "Vgutc",
  "Vgut",
  "Vkidneyc",
  "Vkidney",
  "Vliverc",
  "Vliver",
  "Vlungc",
  "Vlung",
  "Vthyroidc",
  "Vthyroid",
  "Fraction_unbound_plasma",
  
  "Fraction_unbound_plasma_fetus",
  
  "Rblood2plasma",
  "gut_density",
  "kidney_density",
  "liver_density",
  "lung_density",
  "thyroid_density",
  "adipose_density",
  "ffmx_density",
  "placenta_density",
  "amnf_density",
  "brain_density",
  #Further parameters correspond to the naming conventions for the 
  #"theta" coefficients and constants associated with the model 
  #equations in Kapraun et al. 2019, except where such a constant
  #already has an intuitive name in the model, as with 
  #pre_pregnant_BW (which would correspond to theta0 in associated
  #cubic model equation for BW according to Kapraun et al. 2019)
  "BW_cubic_theta1",
  "BW_cubic_theta2",
  "BW_cubic_theta3",
  "Wadipose_linear_theta0",
  "Wadipose_linear_theta1",
  "Wfkidney_gompertz_theta0",
  "Wfkidney_gompertz_theta1",
  "Wfkidney_gompertz_theta2",
  "Wfthyroid_gompertz_theta0",
  "Wfthyroid_gompertz_theta1",
  "Wfthyroid_gompertz_theta2",
  "Wfliver_gompertz_theta0",
  "Wfliver_gompertz_theta1",
  "Wfliver_gompertz_theta2",
  "Wfbrain_gompertz_theta0",
  "Wfbrain_gompertz_theta1",
  "Wfbrain_gompertz_theta2",
  "Wfgut_gompertz_theta0",
  "Wfgut_gompertz_theta1",
  "Wfgut_gompertz_theta2",
  "Wflung_gompertz_theta0",
  "Wflung_gompertz_theta1",
  "Wflung_gompertz_theta2",
  "hematocrit_quadratic_theta0",
  "hematocrit_quadratic_theta1",
  "hematocrit_quadratic_theta2",
  "fhematocrit_cubic_theta1",
  "fhematocrit_cubic_theta2",
  "fhematocrit_cubic_theta3",
  "fBW_gompertz_theta0",
  "fBW_gompertz_theta1",
  "fBW_gompertz_theta2",
  "Vplacenta_cubic_theta1",
  "Vplacenta_cubic_theta2",
  "Vplacenta_cubic_theta3",
  "Vamnf_logistic_theta0",
  "Vamnf_logistic_theta1",
  "Vamnf_logistic_theta2",
  "Vplasma_mod_logistic_theta0",
  "Vplasma_mod_logistic_theta1",
  "Vplasma_mod_logistic_theta2",
  "Vplasma_mod_logistic_theta3",
  "venous_blood_fraction",
  "arterial_blood_fraction",
  "fblood_weight_ratio",
  "Qcardiac_cubic_theta0",
  "Qcardiac_cubic_theta1",
  "Qcardiac_cubic_theta2",
  "Qcardiac_cubic_theta3",
  "term",
  "Qgut_percent_initial",
  "Qgut_percent_terminal",
  "Qkidney_cubic_theta0",
  "Qkidney_cubic_theta1",
  "Qkidney_cubic_theta2",
  "Qkidney_cubic_theta3",
  "Qliver_percent_initial",
  "Qliver_percent_terminal",
  "Qthyroid_percent_initial",
  "Qthyroid_percent_terminal",
  "Qplacenta_linear_theta1",
  "Qadipose_percent_initial",
  "Qadipose_percent_terminal",
  "Qgfr_quadratic_theta0",
  "Qgfr_quadratic_theta1",
  "Qgfr_quadratic_theta2",
  "Qfrvtl_logistic_theta0",
  "Qfrvtl_logistic_theta1",
  "Qfrvtl_logistic_theta2",
  "Qflvtl_logistic_theta0",
  "Qflvtl_logistic_theta1",
  "Qflvtl_logistic_theta2",
  "Qfda_logistic_theta0",
  "Qfda_logistic_theta1",
  "Qfda_logistic_theta2",
  "Qfplacenta_logistic_theta0",
  "Qfplacenta_logistic_theta1",
  "Qfplacenta_logistic_theta2",
  "Qfdv_gompertz_theta0",
  "Qfdv_gompertz_theta1",
  "Qfdv_gompertz_theta2",
  "Qfnonplacental_percent",
  "Qfgut_percent",
  "Qfkidney_percent",
  "Qfbrain_percent",
  "Qbrain_percent",
  "Qkidney_percent",
  "Qgut_percent",
  "Qfliver_percent",
  "Qfthyroid_percent"
)

# This function initializes the state vector for the compiled model:
model.list[["fetal_pbtk"]]$compiled.init.func <- "initmodfetalpbtk"

# This is the function that calculates the derivative of the model as a function
# of time, state, and parameters:
model.list[["fetal_pbtk"]]$derivative.func <- "derivsfetalpbtk"

# This is the ORDERED list of variables returned by the derivative function
# (from Model variables: Outputs):
model.list[["fetal_pbtk"]]$derivative.output.names <- c(
  "tw", #time in weeks
  "Cgut",
  "Cliver",
  "Cven",
  "Clung",
  "Cart",
  "Cadipose",
  "Crest",
  "Ckidney",
  "Cplasma",
  "Aplasma",
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
  "Afplasma",
  "Cfplasma"
)

#Which variables to track by default (should be able to build this from
#state vars and outputs):
model.list[["fetal_pbtk"]]$default.monitor.vars <- c(
  "Cgut",
  "Cliver",
  "Cven",
  "Clung",
  "Cart",
  "Cadipose",
  "Crest",
  "Ckidney",
  "Cplasma",
  "Atubules",
  "Ametabolized",
  "AUC",
  "AUC_fetus",
  "Aplacenta",
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
  "Cfplasma"
)


# Allowable units assigned to dosing input:
model.list[["fetal_pbtk"]]$allowed.units.input <- list(
  "oral" = c('umol','mg','mg/kg'),
  "iv" = c('umol','mg','mg/kg'))

# Allowable units assigned to entries in the output columns of the ode system
model.list[["fetal_pbtk"]]$allowed.units.output <- list(
  "oral" = c('uM','mg/L','umol','mg','uM*days',
             'mg/L*days','mg/m^3','mg/m^3*days'),
  "iv" = c('uM','mg/L','umol','mg','uM*days','mg/L*days',
           'mg/m^3','mg/m^3*days'))

# Default set of units assigned to correspond to each of the "outputs" of 
# the model system, and possibly to other state variables to be monitored.
# AUC values should also be included.
model.list[["fetal_pbtk"]]$compartment.units <- c(
  "Cgut" = "uM",
  "Cliver" = "uM",
  "Cven" = "uM",
  "Clung" = "uM",
  "Cart" = "uM",
  "Cadipose" = "uM",
  "Crest" = "uM",
  "Ckidney" = "uM",
  "Cplasma" = "uM",
  "Aplasma" = "umol",
  "Cthyroid" = "uM",
  "Cplacenta" = "uM",
  "Cfliver" = "uM",
  "Cfven" = "uM",
  "Cfart" = "uM",
  "Cfgut" = "uM",
  "Cflung" = "uM",
  "Cfrest" = "uM",
  "Cfthyroid" = "uM",
  "Cfkidney" = "uM",
  "Cfbrain" = "uM",
  "Afplasma" = "umol",
  "Cfplasma" = "uM",
  "AUC" = "uM*days",
  "AUC_fetus" = "uM*days")


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
  "AUC_fetus",
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
