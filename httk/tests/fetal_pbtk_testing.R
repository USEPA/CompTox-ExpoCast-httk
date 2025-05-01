# R CMD BATCH --no-timing --no-restore --no-save fetal_pbtk_testing.R fetal_pbtk_testing.Rout

# Get rid of anything in the workspace:
rm(list=ls()) 

library(httk)

head(solve_fetal_pbtk(chem.name = 'bisphenol a', daily.dose = 1,
                                             doses.per.day = 3))

p <- parameterize_fetal_pbtk(chem.name='bisphenol a')
p <- p[sort(names(p))]
# Try to standardize order of variable names
for (this.param in names(p)[order(toupper(names(p)))]) cat(paste(this.param,": ",p[[this.param]],"\r\n",sep=""))

quit("no")