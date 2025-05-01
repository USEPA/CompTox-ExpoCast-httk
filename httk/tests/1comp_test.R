# R CMD BATCH --no-timing --no-restore --no-save 1comp_test.R 1comp_test.Rout

# Get rid of anything in the workspace:
rm(list=ls()) 

library(httk)

calc_analytic_css(chem.cas="80-05-7",model="1compartment")

head(solve_1comp(parameters=parameterize_1comp(chem.cas="80-05-7"),days=1))

calc_vdist(chem.name="triclosan")

p <- parameterize_1comp(chem.name="Aminopterin")
p <- p[sort(names(p))]
# Try to standardize order of variable names
for (this.param in names(p)[order(toupper(names(p)))]) cat(paste(this.param,": ",p[[this.param]],"\r\n",sep=""))

quit("no")