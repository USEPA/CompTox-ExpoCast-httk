# Add this model to the list of models:

# Descritpion
model.list[[THIS.MODEL]]$Description <- "TIn Vitro distribution"

# Reference
model.list[[THIS.MODEL]]$Reference <- "Armitage et al. (2021)"

# DOI
model.list[[THIS.MODEL]]$DOI <- "https://doi.org/10.3390/toxics9110315"

# The is the R function for generating model parameters:
model.list[["armitage"]]$parameterize.func <- "parameterize_armitage"


# These are all the parameters returned by the R model parameterization function.
# Some of these parameters are not directly used to solve the model, but describe
# how other parameters were calculated:
model.list[["armitage"]]$param.names <- c(
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
  # "Funbound.plasma",
  # "Funbound.plasma.dist",
  # "unadjusted.Funbound.plasma",
  # "Funbound.plasma.adjustment",
  "Pow",
  "pKa_Donor",
  "pKa_Accept",
  "MA",                        
  # "Fprotein.plasma",
  "plasma.pH",                 
  "alpha")

#Parameters needed to make a prediction (this is used by get_cheminfo):
model.list[["armitage"]]$required.params <- c(
  "MP",
  "logP",
  "logHenry",
  "logWSol",
  "Pow",
  "pKa_Donor",
  "pKa_Accept",
  "MW")

#Parameters needed to run parameterize function without a chemical id:
model.list[["armitage"]]$parameterize_params <- c(
  "MP",
  "logP",
  "logHenry",
  "logWSol",
  "Pow",
  "pKa_Donor",
  "pKa_Accept",
  "MW")

#choose which parameters are not to be Monte Carlo sampled
model.list[["armitage"]]$noMC.params <- c()

##Define the parameter names for each model in one place so that all functions can use them:
#schmitt.specific.names <- c("Kadipose2pu",
#                            "Kbone2pu",
#                            "Kbrain2pu",
#                            "Kheart2pu",
#                            "Kmuscle2pu",
#                            "Kskin2pu",
#                            "Kspleen2pu") 

# Do we ignore the Fups where the value was below the limit of detection?
model.list[["armitage"]]$exclude.fup.zero <- TRUE  

# Filter out compounds belonging to select chemical classes
model.list[["armitage"]]$chem.class.filt <- c("PFAS")