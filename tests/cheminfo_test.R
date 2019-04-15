#R CMD BATCH cheminfo_test.R cheminfo_test.Rout
library(httk)
Css.list <- get_cheminfo()
pbpk.list <- get_cheminfo(model='pbtk')
rat.list <- get_cheminfo(species="rat")
length(Css.list)
length(pbpk.list)
length(rat.list)
