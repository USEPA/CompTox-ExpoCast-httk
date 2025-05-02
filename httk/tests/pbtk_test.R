# R CMD BATCH --no-timing --no-restore --no-save pbtk_test.R pbtk_test.Rout

# Get rid of anything in the workspace:
rm(list=ls()) 

library(httk)

calc_analytic_css(chem.name="bisphenol a",model="pbtk")

head(solve_pbtk(chem.name="bisphenol a",days=1))

p <- parameterize_pbtk(chem.name="Aminopterin")
p <- p[sort(names(p))]
# Try to standardize order of variable names
for (this.param in names(p)[order(toupper(names(p)))]) cat(paste(this.param,": ",p[[this.param]],"\r\n",sep=""))

quit("no")