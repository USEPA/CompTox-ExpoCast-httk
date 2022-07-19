# Add the 3compartment steady-state model (Pearce et al., 2017) to the list of 
# models:
#
# Pearce, Robert G., et al. "Httk: R package for high-throughput 
# toxicokinetics." Journal of statistical software 79.4 (2017): 1.

#Analytic expression for steady-state plasma concentration.
model.list[["3compartmentss"]]$analytic.css.func <- "calc_analytic_css_3compss"

# What units does the analytic function return:
model.list[["3compartmentss"]]$steady.state.units <- "uM"

# Function used for generating model parameters:
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
   
# If httk-pop is enabled:
# We want all the standard physiological calculations performed:
model.list[["3compartmentss"]]$calc.standard.httkpop2httk <- TRUE
# These are the model parameters that are impacted by httk-pop:
model.list[["3compartmentss"]]$httkpop.params <- c(
  "BW",
  "Fgutabs",
  "hepatic.bioavailability",
  "liver.density",
  "million.cells.per.gliver",
  "Qtotal.liverc",
  "Qgfrc",
  "Rblood2plasma",
  "Vliverc")

# Do we need to recalculate partition coefficients when doing Monte Carlo?
model.list[["3compartmentss"]]$calcpc <- FALSE


# Do we need to recalculate first pass metabolism when doing Monte Carlo?
model.list[["3compartmentss"]]$firstpass <- TRUE

# These model parameters are impacted by the in vitro measurements:
model.list[["3compartmentss"]]$invitro.params <- c("BW",
                       "Clint",
                       "Clint.dist",
                       "Fhep.assay.correction",
                       "Funbound.plasma",
                       "Funbound.plasma.dist",
                       "Funbound.plasma.adjustment",
                       "hepatic.bioavailability",
                       "Rblood2plasma")



# Do we ignore the Fups where the alue was below the limit of detection?
model.list[["3compartmentss"]]$exclude.fup.zero <- FALSE

# These are the parameter names needed to describe steady-state dosing:
model.list[["3compartmentss"]]$css.dosing.params <- c("hourly.dose")

# Filter out volatile compounds with Henry's Law Constant Threshold
model.list[["3compartmentss"]]$log.henry.threshold <- c(-4.5)
