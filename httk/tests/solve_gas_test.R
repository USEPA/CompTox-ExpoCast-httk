#R CMD BATCH --no-timing --no-restore --no-save solve_gas_test.R solve_gas_test.Rout
# Get rid of anything in the workspace:
rm(list=ls()) 

library(httk)

head(solve_gas_pbtk(chem.name="pyrene",times=c(0,0.1,0.05)))
head(solve_gas_pbtk(chem.cas="129-00-0",times=c(0,0.1,0.05)))
head(solve_gas_pbtk(
  parameters=parameterize_gas_pbtk(chem.cas="129-00-0"),
  times=c(0,0.1,0.05)))

parameterize_gas_pbtk(chem.name="styrene")

quit("no")