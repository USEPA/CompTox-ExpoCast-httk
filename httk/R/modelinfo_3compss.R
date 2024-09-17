# Add the 3compartment steady-state model (Pearce et al., 2017) to the list of 
# models:
#
# Pearce, Robert G., et al. "Httk: R package for high-throughput 
# toxicokinetics." Journal of statistical software 79.4 (2017): 1.

# Model identifier for the model.list:
THIS.MODEL <- "3compartmentss" 

#Analytic expression for steady-state plasma concentration.
model.list[[THIS.MODEL]]$analytic.css.func <- "calc_analytic_css_3compss"

# What units does the analytic function return:
model.list[[THIS.MODEL]]$steady.state.units <- "mg/L"

# Function used for generating model parameters:
model.list[[THIS.MODEL]]$parameterize.func <- "parameterize_steadystate"  

# These are all the parameters returned by the R model parameterization function.
# Some of these parameters are not directly used to solve the model, but describe
# how other parameters were calculated:
model.list[[THIS.MODEL]]$param.names <- c("BW",
  "Caco2.Pab",
  "Caco2.Pab.dist",
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
  "Rblood2plasma",
  "Vliverc")


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
  "MW"
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
  "Vliverc")

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

# Filter out volatile compounds with Henry's Law Constant Threshold
model.list[[THIS.MODEL]]$log.henry.threshold <- c(-4.5)

# Filter out compounds belonging to select chemical classes
model.list[[THIS.MODEL]]$chem.class.filt <- c("PFAS")

model.list[[THIS.MODEL]]$routes <- list(
  "oral" = list(
    "dosing.params" = c("daily.dose"))
  ) 