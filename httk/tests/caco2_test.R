# R CMD BATCH --no-timing --no-restore --no-save caco2_test.R caco2_test.Rout

# Get rid of anything in the workspace:
rm(list=ls()) 

library(httk)
                  
p <- parameterize_pbtk(chem.cas="80-05-7")
print(p[["MW"]])
print(p[["BW"]])
print(p[["Fabsgut"]])
                  
# Reduce the number of samples used by Monte Carlo to decrease runtime for
# CRAN checks (never use predictions with only ten draws):
NSAMP <- 10

set.seed(1234)
# Let's make sure that the monte carlo Css is also lower when some chemical
# is not absorbed:
Css1.caco <- calc_mc_css(chem.cas="15972-60-8",
            model="3compartment",
            samples=NSAMP)
# The monte carlo Css should be higher with keepit100-TRUE
set.seed(1234)
Css1.100 <- calc_mc_css(chem.cas="15972-60-8",
            model="3compartment",
            samples=NSAMP,
            Caco2.options=list(keepit100=TRUE))
Css1.caco < Css1.100

set.seed(1234)
Css2.caco <- calc_mc_css(dtxsid="DTXSID6034392",
                         samples=NSAMP,
                         which.quantile=0.5)
set.seed(1234)
Css2.100 <- calc_mc_css(dtxsid="DTXSID6034392",
                        samples=NSAMP,
                        Caco2.options = list(keepit100=TRUE),
                        which.quantile=0.5)
Css2.caco < Css2.100

quit("no")