#R CMD BATCH --no-timing --no-restore --no-save caco2_test.R caco2_test.Rout
library(httk)
options(warn=-1)

calc_analytic_css(chem.name="bisphenol a",model="pbtk")
calc_analytic_css(chem.name="bisphenol a",model="pbtk",Caco2.options=list(keepit100=T))

head(solve_pbtk(chem.cas="80-05-7"))
head(solve_pbtk(chem.cas="80-05-7",Caco2.options=list(keepit100=T)))


NSAMP <- 1000
set.seed(1234)
calc_mc_css(chem.cas="15972-60-8",model="3compartment",samples=NSAMP)
set.seed(1234)
calc_mc_css(chem.cas="15972-60-8",model="3compartment",samples=NSAMP,Caco2.options=list(keepit100=T))




