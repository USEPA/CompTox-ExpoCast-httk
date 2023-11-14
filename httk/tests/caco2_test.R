# R CMD BATCH --no-timing --no-restore --no-save caco2_test.R caco2_test.Rout

# Get rid of anything in the workspace:
rm(list=ls()) 

library(httk)

# By default we now include calculation of Fabs and Fgut (always had Fhep):
calc_analytic_css(chem.name="bisphenol a",
                  model="pbtk")
# Therefore if we set Fabs = Fgut = 1 with keetit100=TRUE, we should get a
# higher predicted plasma steady-state concentration:
calc_analytic_css(chem.name="bisphenol a",
                  model="pbtk",
                  Caco2.options=list(keepit100=TRUE))

# By default we now include calculation of Fabs and Fgut (we explicitly model
# first-pass hepatic metabolism in the model "pbtk")
head(solve_pbtk(chem.cas="80-05-7"))
# Therefore if we set Fabs = Fgut = 1 with keetit100=TRUE, we should get a
# higher tissue concentrations:
head(solve_pbtk(chem.cas="80-05-7",
                Caco2.options=list(keepit100=TRUE)))

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