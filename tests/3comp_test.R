#R CMD BATCH other_tests.R other_tests.Rout
library(httk)
calc_analytic_css(chem.name="bisphenol a",model="3compartment")
calc_analytic_css(chem.cas="80-05-7",model="3compartment")
calc_analytic_css(parameters=parameterize_3comp(chem.cas="80-05-7"),model="3compartment")

head(solve_3comp(chem.name="bisphenol a"))
head(solve_3comp(chem.cas="80-05-7"))
head(solve_3comp(parameters=parameterize_3comp(chem.cas="80-05-7")))

script.args <- commandArgs(TRUE)
if (length(script.args) > 0) if (script.args[1]=="mctest")
{
  set.seed(12345)
  calc_mc_css(chem.name="bisphenol a",model="3compartment")
  set.seed(12345)
  calc_mc_css(chem.cas="80-05-7",model="3compartment")
  set.seed(12345)
  calc_mc_css(parameters=parameterize_3comp(chem.cas="80-05-7"),model="3compartment")
}