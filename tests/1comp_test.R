#R CMD BATCH other_tests.R other_tests.Rout
library(httk)

calc_analytic_css(chem.name="bisphenol a",model="1compartment")
calc_analytic_css(chem.cas="80-05-7",model="1compartment")
calc_analytic_css(parameters=parameterize_1comp(chem.cas="80-05-7"),model="1compartment")
calc_analytic_css(chem.cas="80-05-7",model="1compartment",tissue="liver")
calc_analytic_css(chem.cas="80-05-7",model="1compartment",tissue="brain")

head(solve_1comp(chem.name="bisphenol a"))
head(solve_1comp(chem.cas="80-05-7"))
head(solve_1comp(parameters=parameterize_1comp(chem.cas="80-05-7")))

calc_vdist(chem.name="triclosan")
calc_vdist(chem.cas="80-05-7")
params <- parameterize_schmitt(chem.name="triclosan")
calc_vdist(parameters=params)
params <- parameterize_3comp(chem.name="triclosan")
calc_vdist(parameters=params)
params <- parameterize_pbtk(chem.name="triclosan")
calc_vdist(parameters=params)

script.args <- commandArgs(TRUE)
if (length(script.args) > 0) if (script.args[1]=="mctest")
{
  set.seed(12345)
  calc_mc_css(chem.name="bisphenol a",model="1compartment")
  set.seed(12345)
  calc_mc_css(chem.cas="80-05-7",model="1compartment")
  set.seed(12345)
  calc_mc_css(parameters=parameterize_1comp(chem.cas="80-05-7"),model="1compartment")
}