#R CMD BATCH --no-timing --no-restore --no-save solve_gas_crude_test.R solve_gas_crude_test.Rout
library(httk)
options(warn=-1)

signif(head(solve_gas_pbtk(chem.name="pyrene")),3)
signif(head(solve_gas_pbtk(chem.cas="129-00-0")),3)
signif(head(solve_gas_pbtk(parameters=parameterize_gas_pbtk(chem.cas="129-00-0"))),3)
signif(head(solve_gas_pbtk(chem.name='pyrene',exp.conc=1,period=24,expduration=24)))
signif(head(solve_gas_pbtk(chem.cas="80-05-7",exp.conc=3,period=24,exp.duration = 6, exercise = TRUE)))

quit("no")