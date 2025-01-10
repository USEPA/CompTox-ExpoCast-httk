setwd("c:/users/jwambaug/git/httk-dev/datatables/")
library(httk)
out <- benchmark_httk(basic.check=TRUE, 
                      calc_mc_css.check=TRUE,
                      in_vivo_stats.check=TRUE,
                      tissuepc.check=TRUE)
