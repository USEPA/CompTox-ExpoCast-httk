#R CMD BATCH --no-timing --no-restore --no-save other_tests.R other_tests.Rout
library(httk)
options(warn=-1)

calc_css(chem.name='nicotine')

calc_css(chem.name="endrin")

calc_stats(chem.name='nicotine',days=10)

calc_stats(dtxsid="DTXSID0020442",days=1)

calc_stats(dtxsid="DTXSID0020442",days=10)

calc_stats(dtxsid="DTXSID0020442",days=100)

calc_analytic_css(
  chem.name='Bisphenol-A',
  tissue='liver',
  species='rabbit',
  parameterize.args = list(
    default.to.human=T,
    adjusted.Funbound.plasma=T,
    regression=T,
    minimum.Funbound.plasma=1e-4),
  daily.dose=2)

quit("no")