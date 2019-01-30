#R CMD BATCH other_tests.R other_tests.Rout
library(httk)
set.seed(12345)
calc_analytic_css(chem.name="bisphenol a",model="3compartmentss")
calc_mc_css(chem.name="bisphenol a",model="3compartmentss")
calc_analytic_css(chem.cas="80-05-7",model="3compartmentss")
calc_mc_css(chem.cas="80-05-7",model="3compartmentss")
calc_analytic_css(parameters=parameterize_steadystate(chem.cas="80-05-7"),model="3compartmentss")
calc_mc_css(parameters=parameterize_steadystate(chem.cas="80-05-7"),model="3compartmentss")
