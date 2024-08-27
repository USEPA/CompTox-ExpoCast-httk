# R CMD BATCH --no-timing --no-restore --no-save fetal_pbtk_testing.R fetal_pbtk_testing.Rout

# Get rid of anything in the workspace:
rm(list=ls()) 

library(httk)

head(solve_fetal_pbtk(chem.name = 'bisphenol a', daily.dose = 1,
                                             doses.per.day = 3))
head(solve_fetal_pbtk(chem.cas="80-05-7"))
head(solve_fetal_pbtk(parameters=parameterize_fetal_pbtk(chem.cas="80-05-7")))

p <- parameterize_fetal_pbtk(chem.name='bisphenol a')[sort(names(parameterize_fetal_pbtk(chem.name='bisphenol a')))]
for (this.param in names(p)) cat(paste(this.param,": ",p[[this.param]],"\r\n",sep=""))

quit("no")