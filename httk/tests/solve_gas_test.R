#R CMD BATCH --no-timing --no-restore --no-save solve_gas_crude_test.R solve_gas_crude_test.Rout
library(httk)

signif(head(solve_gas_pbtk(chem.name="pyrene")),3)
signif(head(solve_gas_pbtk(chem.cas="129-00-0")),3)
signif(head(solve_gas_pbtk(parameters=parameterize_gas_pbtk(chem.cas="129-00-0"))),3)

quit("no")