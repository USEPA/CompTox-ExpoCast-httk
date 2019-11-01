#R CMD BATCH --no-timing --no-save other_tests.R other_tests.Rout
library(httk)
#options(warn=-1)
NSAMP <- 1000



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
calc_mc_css(chem.cas="50594-66-6",samples=NSAMP)
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



#
#
# Now test that MC works across different models:
set.seed(1234)
calc_mc_css(chem.cas="15972-60-8",model="pbtk",samples=NSAMP)
set.seed(1234)
calc_mc_css(chem.cas="15972-60-8",model="3compartment",samples=NSAMP)
set.seed(1234)
calc_mc_css(chem.cas="15972-60-8",model="1compartment",samples=NSAMP)
