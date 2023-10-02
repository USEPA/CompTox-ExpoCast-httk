#R CMD BATCH --no-timing --no-restore --no-save 1comp_test.R 1comp_test.Rout
# Get rid of anything in the workspace:
rm(list=ls()) 

library(httk)

#calc_analytic_css(chem.name="bisphenol a",model="1compartment")
calc_analytic_css(chem.cas="80-05-7",model="1compartment")
calc_analytic_css(chem.cas="80-05-7",parameters=parameterize_1comp(chem.cas="80-05-7"),model="1compartment")
#calc_analytic_css(chem.cas="80-05-7",model="1compartment",tissue="liver")
calc_analytic_css(chem.cas="80-05-7",model="1compartment",tissue="brain")

##head(solve_1comp(chem.name="bisphenol a"))
#head(solve_1comp(chem.cas="80-05-7"))
head(solve_1comp(parameters=parameterize_1comp(chem.cas="80-05-7")))

calc_vdist(chem.name="triclosan")
calc_vdist(chem.cas="80-05-7")
params <- parameterize_schmitt(chem.name="triclosan")
params <- c(params, predict_partitioning_schmitt(parameters = params))
calc_vdist(parameters=params)
params <- parameterize_3comp(chem.name="triclosan")
calc_vdist(parameters=params)
params <- parameterize_pbtk(chem.name="triclosan")
calc_vdist(parameters=params)

parameterize_1comp(chem.name="Aminopterin")

quit("no")