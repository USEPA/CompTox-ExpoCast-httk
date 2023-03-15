#R CMD BATCH --no-timing --no-restore --no-save fetal_pbtk_testing.R fetal_pbtk_testing.Rout
library(httk)
options(warn=-1)

head(solve_fetal_pbtk(chem.name = 'bisphenol a', daily.dose = 1,
                                             doses.per.day = 3))
head(solve_fetal_pbtk(chem.cas="80-05-7"))
head(solve_fetal_pbtk(parameters=parameterize_fetal_pbtk(chem.cas="80-05-7")))

quit("no")