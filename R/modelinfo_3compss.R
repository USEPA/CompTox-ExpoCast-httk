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
                       "Vliverc")

#Parameters needed to make a prediction (this is used by get_cheminfo):
model.list[["3compartmentss"]]$required.params <- c(
  "Clint",
  "Funbound.plasma",
  "Pow",
  "pKa_Donor",
  "pKa_Accept",
  "MW"
   )
# Do we ignore the Fups where the alue was below the limit of detection?
model.list[["3compartmentss"]]$exclude.fup.zero <- F