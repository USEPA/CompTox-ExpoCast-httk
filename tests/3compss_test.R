#R CMD BATCH other_tests.R other_tests.Rout
library(httk)
calc_analytic_css(chem.name="bisphenol a",model="3compartmentss")
calc_analytic_css(chem.cas="80-05-7",model="3compartmentss")
calc_analytic_css(parameters=parameterize_steadystate(chem.cas="80-05-7"),model="3compartmentss")
calc_analytic_css(chem.name="bisphenol a",model="3compartmentss",tissue="liver")
calc_analytic_css(chem.name="bisphenol a",model="3compartmentss",tissue="brain")

script.args <- commandArgs(TRUE)
if (length(script.args) > 0) if (script.args[1]=="mctest")
{
  set.seed(12345)
  calc_mc_css(chem.name="bisphenol a",model="3compartmentss")
  set.seed(12345)
  calc_mc_css(chem.cas="80-05-7",model="3compartmentss")
  set.seed(12345)
  calc_mc_css(parameters=parameterize_steadystate(chem.cas="80-05-7"),model="3compartmentss")
}