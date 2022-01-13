#Branch specific 'solve' function tests 
library(httk)

head(solve_dermal_pbtk(chem.name="propylparaben"))
head(solve_dermal_pbtk(chem.cas="94-13-3"))
head(solve_dermal_pbtk(parameters=parameterize_dermal_pbtk(chem.cas="94-13-3")))



