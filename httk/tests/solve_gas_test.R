#R CMD BATCH --no-timing --no-restore --no-save solve_gas_test.R solve_gas_test.Rout
library(httk)

signif(head(solve_gas_pbtk(chem.name="pyrene",times=c(0,0.1,0.05)))[,-11],3)
signif(head(solve_gas_pbtk(chem.cas="129-00-0",times=c(0,0.1,0.05)))[,-11],3)
signif(head(solve_gas_pbtk(
  parameters=parameterize_gas_pbtk(chem.cas="129-00-0"),
  times=c(0,0.1,0.05)))[,-11],3)

parameterize_gas_pbtk(chem.name="styrene")

quit("no")