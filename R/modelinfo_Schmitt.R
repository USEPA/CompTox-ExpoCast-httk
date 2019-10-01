#Define the parameter names for each model in one place so that all functions can use them:
schmitt.names <- c("Kadipose2pu",
                   "Kbone2pu",
                   "Kbrain2pu",
                   "Kgut2pu",
                   "Kheart2pu",
                   "Kkidney2pu",
                   "Kliver2pu",
                   "Klung2pu",
                   "Kmuscle2pu",
                   "Kskin2pu",
                   "Kspleen2pu",
                   "Krbc2pu",
                   "Krest2pu")  

param.names.schmitt <- c(schmitt.names,
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


<<<<<<< HEAD
#Parameters needed to make a prediction (this is used by get_cheminfo):
model.list[["schmitt"]]$required.params <- c(
  "Funbound.plasma",
  "Pow",
  "pKa_Donor",
  "pKa_Accept",
  "MW")

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
=======
>>>>>>> 7e1b273a530de98fe4b5c7f5630ba34b400ed812

schmitt.specific.names <- c("Kadipose2pu",
                            "Kbone2pu",
                            "Kbrain2pu",
                            "Kheart2pu",
                            "Kmuscle2pu",
                            "Kskin2pu",
                            "Kspleen2pu")   
                    