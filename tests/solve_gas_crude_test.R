#Branch specific 'solve' function tests 
library(httk)

head(solve_gas_pbtk(chem.name="pyrene"))
head(solve_gas_pbtk(chem.cas="129-00-0"))
head(solve_gas_pbtk(parameters=parameterize_gas_pbtk(chem.cas="129-00-0")))