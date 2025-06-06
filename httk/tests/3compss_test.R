# R CMD BATCH --no-timing --no-restore --no-save 3compss_test.R 3compss_test.Rout

# Get rid of anything in the workspace:
rm(list=ls()) 

library(httk)

calc_analytic_css(chem.name="bisphenol a",model="3compartmentss")
calc_analytic_css(chem.cas="80-05-7",model="3compartmentss")
calc_analytic_css(parameters=parameterize_steadystate(chem.cas="80-05-7"),model="3compartmentss")

p <- parameterize_steadystate(chem.name="Aminopterin")[sort(names(parameterize_steadystate(chem.name="Aminopterin")))]
# Try to standardize order of variable names
for (this.param in names(p)[order(toupper(names(p)))]) cat(paste(this.param,": ",p[[this.param]],"\r\n",sep=""))

quit("no")