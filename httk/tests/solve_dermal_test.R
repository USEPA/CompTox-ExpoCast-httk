# R CMD BATCH --no-timing --no-restore --no-save solve_dermal_test.R solve_dermal_test.Rout

# Get rid of anything in the workspace:
rm(list=ls()) 

library(httk)

# Rationale: Use of signif is required for the same results on various OS's due
#   to precision differences.
signif(head(solve_dermal_pbtk(chem.name="propylparaben")), 
       2)

p <- parameterize_dermal_pbtk(chem.name="propylparaben")
p <- p[sort(names(p))]
# Try to standardize order of variable names
for (this.param in 
     names(p)[order(toupper(names(p)))]) cat(
     paste(this.param,": ",p[[this.param]],"\r\n",sep=""))

quit("no")