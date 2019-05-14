#Branch specific 'solve' function tests 
library(httk)

head(solve_aerosol_pbtk(chem.name="pyrene"))
head(solve_aerosol_pbtk(chem.cas="129-00-0"))
head(solve_aerosol_pbtk(parameters=parameterize_aerosol_pbtk(chem.cas="129-00-0")))



