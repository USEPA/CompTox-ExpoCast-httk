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

