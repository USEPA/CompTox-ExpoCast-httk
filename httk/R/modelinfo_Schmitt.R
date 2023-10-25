# Add this model to the list of models:

# The is the R function for generating model parameters:
model.list[["schmitt"]]$parameterize.func <- "parameterize_schmitt"


# These are all the parameters returned by the R model parameterization function.
# Some of these parameters are not directly used to solve the model, but describe
# how other parameters were calculated:
model.list[["schmitt"]]$param.names <- c(
 # "Kadipose2pu",
 # "Kbone2pu",
 # "Kbrain2pu",
 # "Kgut2pu",
 # "Kheart2pu",
 # "Kkidney2pu",
 # "Kliver2pu",
 # "Klung2pu",
 # "Kmuscle2pu",
 # "Kskin2pu",
 # "Kspleen2pu",
 # "Krbc2pu",
 # "Krest2pu",
  "Funbound.plasma",
  "Funbound.plasma.dist",
  "unadjusted.Funbound.plasma",
  "Funbound.plasma.adjustment",
  "Pow",
  "pKa_Donor",
  "pKa_Accept",
  "MA",                        
  "Fprotein.plasma",
  "plasma.pH",                 
  "alpha")

#Parameters needed to make a prediction (this is used by get_cheminfo):
model.list[["schmitt"]]$required.params <- c(
  "Funbound.plasma",
  "Pow",
  "pKa_Donor",
  "pKa_Accept",
  "MW")

#Parameters needed to run parameterize function without a chemical id:
model.list[["schmitt"]]$parameterize_params <- c(
  "Funbound.plasma",
  "Pow",
  "pKa_Donor",
  "pKa_Accept",
  "MA",
  "plasma.pH",
  "alpha",
  "Fprotein.plasma")

#choose which parameters are not to be Monte Carlo sampled
model.list[["schmitt"]]$noMC.params <- c(
  'kgutabs',
  'MW',
  'Pow',
  "MA",
  'pKa_Donor',
  'pKa_Accept',
  "Fhep.assay.correction",
  "Funbound.plasma.adjustment",
  'Fgutabs'
  )

##Define the parameter names for each model in one place so that all functions can use them:
#schmitt.names <- c("Kadipose2pu",
#                   "Kbone2pu",
#                   "Kbrain2pu",
#                   "Kgut2pu",
#                   "Kheart2pu",
#                   "Kkidney2pu",
#                   "Kliver2pu",
#                   "Klung2pu",
#                   "Kmuscle2pu",
#                   "Kskin2pu",
#                   "Kspleen2pu",
#                   "Krbc2pu",
#                   "Krest2pu")  


schmitt.specific.names <- c("Kadipose2pu",
                            "Kbone2pu",
                            "Kbrain2pu",
                            "Kheart2pu",
                            "Kmuscle2pu",
                            "Kskin2pu",
                            "Kspleen2pu") 
                              
# Do we ignore the Fups where the value was below the limit of detection?
model.list[["schmitt"]]$exclude.fup.zero <- TRUE  

# Filter out compounds belonging to select chemical classes
model.list[["schmitt"]]$chem.class.filt <- c("PFAS")
