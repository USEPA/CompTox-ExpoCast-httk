# R CMD BATCH --no-timing --no-restore --no-save solve_gas_test.R solve_gas_test.Rout

# Get rid of anything in the workspace:
rm(list=ls()) 

library(httk)

head(solve_gas_pbtk(chem.name="pyrene",times=c(0,0.1,0.05)))
head(solve_gas_pbtk(chem.cas="129-00-0",times=c(0,0.1,0.05)))
head(solve_gas_pbtk(
  parameters=parameterize_gas_pbtk(chem.cas="129-00-0"),
  times=c(0,0.1,0.05)))

p <- parameterize_gas_pbtk(chem.name="styrene")[sort(names(parameterize_gas_pbtk(chem.name="styrene")))]
# Try to standardize order of variable names
for (this.param in names(p)[order(toupper(names(p)))]) cat(paste(this.param,": ",p[[this.param]],"\r\n",sep=""))

quit("no")