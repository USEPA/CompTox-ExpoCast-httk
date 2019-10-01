# Add this model to the list of models:
model.list[["3compartmentss"]]$analytic.css.func <- "calc_analytic_css_3compss"

#Define the parameter names for each model in one place so that all functions can use them:
param.names.3compss <- c("BW",
                       "Clint",
                       "Clint.dist",
                       "Dow74",
                       "Fgutabs",
                       "Fhep.assay.correction",
                       "Funbound.plasma",
                       "Funbound.plasma.dist",
                       "Funbound.plasma.adjustment",
                       "hepatic.bioavailability",
                       "liver.density",
                       "million.cells.per.gliver",
                       "MW",
                       "Qtotal.liverc",
                       "Qgfrc",
                       "Rblood2plasma",
<<<<<<< HEAD
                       "Vliverc")

# Allowable units (and whether they are for amounts or concentration):
model.list[["3compartmentss"]]$conc.units <- c('um', 'mg/l')
model.list[["3compartmentss"]]$amount.units <- c('umol', 'mg')

#Parameters needed to make a prediction (this is used by get_cheminfo):
model.list[["3compartmentss"]]$required.params <- c(
  "Clint",
  "Funbound.plasma",
  "Pow",
  "pKa_Donor",
  "pKa_Accept",
  "MW"
   )
   
#choose which parameters are not to be onte Carlo sampled
model.list[["3compartmentss"]]$noMC.params <- c(
  "Dow74",
  'MW',
  'Fgutabs',
  "Fhep.assay.correction",
  "Funbound.plasma.adjustment"
  )
   
   
# Do we ignore the Fups where the alue was below the limit of detection?
model.list[["3compartmentss"]]$exclude.fup.zero <- F
=======
                       "Vliverc")
>>>>>>> 7e1b273a530de98fe4b5c7f5630ba34b400ed812
