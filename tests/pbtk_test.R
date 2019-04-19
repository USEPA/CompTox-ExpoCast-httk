#R CMD BATCH other_tests.R other_tests.Rout
library(httk)
calc_analytic_css(chem.name="bisphenol a",model="pbtk")
calc_analytic_css(chem.cas="80-05-7",model="pbtk")
calc_analytic_css(parameters=parameterize_pbtk(chem.cas="80-05-7"),model="pbtk")
calc_analytic_css(chem.name="bisphenol a",model="pbtk",tissue="liver")
calc_analytic_css(chem.name="bisphenol a",model="pbtk",tissue="brain")

head(solve_pbtk(chem.name="bisphenol a"))
head(solve_pbtk(chem.cas="80-05-7"))
head(solve_pbtk(parameters=parameterize_pbtk(chem.cas="80-05-7")))

script.args <- commandArgs(TRUE)
if (length(script.args) > 0) if (script.args[1]=="mctest")
{
  set.seed(12345)
  calc_mc_css(chem.name="bisphenol a",model="pbtk")
  set.seed(12345)
  calc_mc_css(chem.cas="80-05-7",model="pbtk")
  set.seed(12345)
  calc_mc_css(parameters=parameterize_pbtk(chem.cas="80-05-7"),model="pbtk")
}