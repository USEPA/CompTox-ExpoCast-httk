#R CMD BATCH --no-timing other_tests.R other_tests.Rout
library(httk)
options(warn=-1)

calc_css(chem.name='nicotine')

calc_css(chem.name="perfluorooctanoic acid")

calc_stats(chem.name='nicotine',days=10)

calc_stats(dtxsid="DTXSID0020442",days=1)

calc_stats(dtxsid="DTXSID0020442",days=10)

calc_stats(dtxsid="DTXSID0020442",days=100)

calc_analytic_css(chem.name='Bisphenol-A',tissue='liver',species='rabbit',
                  default.to.human=TRUE,daily.dose=2)
