# Add this model to the list of models:

#Analytic expression for steady-state plasma concentration.
model.list[["3compartmentss"]]$analytic.css.func <- "calc_analytic_css_3compss"

# The is the R function for generating model parameters:
model.list[["3compartmentss"]]$parameterize.func <- "parameterize_steadystate"

# These are all the parameters returned by the R model parameterization function.
# Some of these parameters are not directly used to solve the model, but describe
# how other parameters were calculated:
model.list[["3compartmentss"]]$param.names <- c("BW",
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
                       "Pow",
                       "Qtotal.liverc",
                       "Qgfrc",
                       "Rblood2plasma",
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