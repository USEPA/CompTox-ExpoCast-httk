#R CMD BATCH other_tests.R other_tests.Rout
library(httk)
calc_css(chem.name='nicotine')
calc_css(chem.name="perfluorooctanoic acid")
calc_stats(chem.name='nicotine',days=10)
calc_analytic_css(chem.name='Bisphenol-A',tissue='liver',species='rabbit',
                  default.to.human=TRUE,daily.dose=2)
