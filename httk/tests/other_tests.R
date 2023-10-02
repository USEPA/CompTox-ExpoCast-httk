#R CMD BATCH --no-timing --no-restore --no-save other_tests.R other_tests.Rout
# Get rid of anything in the workspace:
rm(list=ls()) 

library(httk)

#calc_css(chem.name='nicotine')

calc_css(chem.name='nicotine', model="1compartment")

calc_css(chem.name='nicotine', model="3compartment")

calc_css(chem.name="endrin")

calc_tkstats(chem.name='nicotine',days=10)

calc_tkstats(dtxsid="DTXSID0020442",days=1)

calc_tkstats(dtxsid="DTXSID0020442",days=10)

calc_tkstats(dtxsid="DTXSID0020442",days=100)

calc_analytic_css(
  chem.name='Bisphenol-A',
  tissue='liver',
  species='rabbit',
  parameterize.args = list(
    default.to.human=TRUE,
    adjusted.Funbound.plasma=TRUE,
    regression=TRUE,
    minimum.Funbound.plasma=1e-4),
  daily.dose=2)

predict_partitioning_schmitt(chem.name='nicotine')

quit("no")
