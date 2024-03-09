# Add the 3compartment steady-state model (Pearce et al., 2017) to the list of 
# models:
#
# Pearce, Robert G., et al. "Httk: R package for high-throughput 
# toxicokinetics." Journal of statistical software 79.4 (2017): 1.

#Analytic expression for steady-state plasma concentration.
model.list[["3compartmentsspfas"]]$analytic.css.func <- "calc_analytic_css_3compss2"

# What units does the analytic function return:
model.list[["3compartmentsspfas"]]$steady.state.units <- "mg/L"

# Function used for generating model parameters:
model.list[["3compartmentsspfas"]]$parameterize.func <- "parameterize_pfassteadystate"  

# These are all the parameters returned by the R model parameterization function.
# Some of these parameters are not directly used to solve the model, but describe
# how other parameters were calculated:
model.list[["3compartmentsspfas"]]$param.names <- c("BW",
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
                       "million.cells.per.gliver",
                       "MW",
                       "Qtotal.liverc",
                       "Qgfrc",
                       "resorption.factor",
                       "Rblood2plasma",
                       "Vliverc",
                       "Qalvc",
                       "Kblood2air")


# Allowable units (and whether they are for amounts or concentration):
model.list[["3compartmentsspfas"]]$conc.units <- c('um', 'mg/l')
model.list[["3compartmentsspfas"]]$amount.units <- c('umol', 'mg')

#Parameters needed to make a prediction (this is used by get_cheminfo):
model.list[["3compartmentsspfas"]]$required.params <- c(
  "Clint",
  "Funbound.plasma",
  "Pow",
  "pKa_Donor",
  "pKa_Accept",
  "MW"
   )
   
# If httk-pop is enabled:
# We want all the standard physiological calculations performed:
model.list[["3compartmentsspfas"]]$calc.standard.httkpop2httk <- TRUE
# These are the model parameters that are impacted by httk-pop:
model.list[["3compartmentsspfas"]]$httkpop.params <- c(
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
model.list[["3compartmentsspfas"]]$calcpc <- FALSE


# Do we need to recalculate first pass metabolism when doing Monte Carlo?
model.list[["3compartmentsspfas"]]$firstpass <- TRUE

# These model parameters are impacted by the in vitro measurements:
model.list[["3compartmentsspfas"]]$invitro.params <- c("BW",
                       "Clint",
                       "Clint.dist",
                       "Fhep.assay.correction",
                       "Funbound.plasma",
                       "Funbound.plasma.dist",
                       "Funbound.plasma.adjustment",
                       "hepatic.bioavailability",
                       "Rblood2plasma")

# Do we ignore the Fups where the alue was below the limit of detection?
model.list[["3compartmentsspfas"]]$exclude.fup.zero <- FALSE

# These are the parameter names needed to describe steady-state dosing:
model.list[["3compartmentsspfas"]]$css.dosing.params <- c("hourly.dose")

