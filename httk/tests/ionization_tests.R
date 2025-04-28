# R CMD BATCH --no-timing --no-restore --no-save ionization_tests.R ionization_tests.Rout

# Get rid of anything in the workspace:
rm(list=ls()) 

library(httk)

calc_ionization(pH=7,pKa_Donor=5)
calc_ionization(pH=7,pKa_Accept=5)
calc_ionization(pH=7,pKa_Donor=10)
calc_ionization(pH=7,pKa_Accept=10)
calc_ionization(pH=7,pKa_Donor="5,10")
calc_ionization(pH=7,pKa_Accept="5,10")
calc_ionization(pH=7,pKa_Donor=10,pKa_Accept=5)
calc_ionization(pH=7,pKa_Accept=10,pKa_Donor=5)

calc_ionization(pH=7,pKa_Donor="5,10",pKa_Accept=4)
calc_ionization(pH=7,pKa_Accept="5,10",pKa_Donor=4)