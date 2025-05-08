# R CMD BATCH --no-timing --no-restore --no-save montecarlo_tests.R montecarlo_tests.Rout

# Get rid of anything in the workspace:
rm(list=ls()) 

library(httk)

# Reduce the number of samples used by Monte Carlo to decrease runtime for
# CRAN checks (never use predictions with only ten draws):
NSAMP <- 5

#
#
# Test that the underlying PK models give the same answers:
calc_analytic_css(chem.cas="15972-60-8")
calc_analytic_css(chem.cas="15972-60-8",model="1compartment")
calc_analytic_css(chem.cas="15972-60-8",model="pbtk")
calc_analytic_css(chem.cas="15972-60-8",model="3compartment")



#
#
# Now test Monte Carlo for a variety of chemicals:
# Clint and Fup are distributions, clint is zero:
set.seed(1234)
uM <- calc_mc_css(chem.cas="50594-66-6",samples=NSAMP,output.units="uM")
set.seed(1234)
mgpL <- calc_mc_css(chem.cas="50594-66-6",samples=NSAMP,output.units="mg/L")
# Test unit conversions, molecular weight of Acifluorfen is 361.66:
signif(mgpL/uM*1000,3)
# Human.Clint.pvalue > 0.05, no measured Rblood2plasma
set.seed(1234)
calc_mc_css(chem.cas="116-06-3",samples=NSAMP)
# Human.Funbound.plasma is below LOD (0.005), can't do PBPK, can't predict
# Rblood2plasma
set.seed(1234)
calc_mc_css(chem.cas="101-05-3",samples=NSAMP)
# well-behaved chemical with a measured Rblood2plasma:
set.seed(1234)
calc_mc_css(chem.cas="15972-60-8",samples=NSAMP)
# Chemical where median fup is 0 but upper 95th percentile is non-zero:
set.seed(1234)
calc_mc_css(dtxsid="DTXSID5041726",samples=NSAMP)

#
#
# Now test that MC works across different models:
set.seed(1234)
calc_mc_css(chem.cas="15972-60-8",model="3compartment",samples=NSAMP)
set.seed(1234)
calc_mc_css(chem.cas="15972-60-8",model="1compartment",samples=NSAMP)
set.seed(1234)
calc_mc_css(chem.cas="15972-60-8",model="pbtk",samples=NSAMP)

# Should be the same as the mean result:
calc_analytic_css(chem.cas="90-43-7",model="pbtk",output.units="mg/L")
                   
quit("no")