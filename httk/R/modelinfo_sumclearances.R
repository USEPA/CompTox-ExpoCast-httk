# Add the 3compartment steady-state model with inhalation to the list of models:

# Model identifier for the model.list:
THIS.MODEL <- "sumclearances" 

#Analytic expression for steady-state plasma concentration.
model.list[[THIS.MODEL]]$analytic.css.func <- "calc_analytic_css_sumclearances"

# What units does the analytic function return:
model.list[[THIS.MODEL]]$steady.state.units <- "mg/L"

# Function used for generating model parameters:
model.list[[THIS.MODEL]]$parameterize.func <- "parameterize_sumclearances"  

# These are all the parameters returned by the R model parameterization function.
# Some of these parameters are not directly used to solve the model, but describe
# how other parameters were calculated:
model.list[[THIS.MODEL]]$param.names <- c("BW",
                       "Clint",
                       "Clint.dist",
                       "Dow74",
                       "Fabsgut",
                       "Fhep.assay.correction",
                       "Funbound.plasma",
                       "Funbound.plasma.dist",
                       "Funbound.plasma.adjustment",
                       "hepatic.bioavailability",
                       "liver.density",
                       "logHenry",
                       "million.cells.per.gliver",
                       "MW",
                       "pKa_Accept",
                       "pKa_Donor",
                       "Pow",
                       "Qtotal.liverc",
                       "Qgfrc",
                       "Rblood2plasma",
                       "Vliverc",
                       "Qalvc",
                       "Kblood2air")


# Allowable units (and whether they are for amounts or concentration):
model.list[[THIS.MODEL]]$conc.units <- c('um', 'mg/l')
model.list[[THIS.MODEL]]$amount.units <- c('umol', 'mg')

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
   
# If httk-pop is enabled:
# We want all the standard physiological calculations performed:
model.list[[THIS.MODEL]]$calc.standard.httkpop2httk <- TRUE
# These are the model parameters that are impacted by httk-pop:
model.list[[THIS.MODEL]]$httkpop.params <- c(
  "BW",
  "Fabsgut",
  "hepatic.bioavailability",
  "liver.density",
  "million.cells.per.gliver",
  "Qtotal.liverc",
  "Qgfrc",
  "Rblood2plasma",
  "Vliverc",
  "Qalvc")

# Do we need to recalculate partition coefficients when doing Monte Carlo?
model.list[[THIS.MODEL]]$calcpc <- FALSE


# Do we need to recalculate first pass metabolism when doing Monte Carlo?
model.list[[THIS.MODEL]]$firstpass <- TRUE

# These model parameters are impacted by the in vitro measurements:
model.list[[THIS.MODEL]]$invitro.params <- c("BW",
                       "Clint",
                       "Clint.dist",
                       "Fhep.assay.correction",
                       "Funbound.plasma",
                       "Funbound.plasma.dist",
                       "Funbound.plasma.adjustment",
                       "hepatic.bioavailability",
                       "Rblood2plasma")

# Do we ignore the Fups where the alue was below the limit of detection?
model.list[[THIS.MODEL]]$exclude.fup.zero <- FALSE

# These are the parameter names needed to describe steady-state dosing:
model.list[[THIS.MODEL]]$css.dosing.params <- c("hourly.dose")

model.list[[THIS.MODEL]]$routes <- list(
  "oral" = list(
    "dosing.params" = c("daily.dose")),
  "inhalation" = list(
    "dosing.params" = c("Cinhppmv"))
  )    