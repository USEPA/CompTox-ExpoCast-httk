# R CMD BATCH --no-timing --no-restore --no-save 3comp_test.R 3comp_test.Rout

# Get rid of anything in the workspace:
rm(list=ls()) 

library(httk)

calc_analytic_css(chem.name="bisphenol a",model="3compartment")
calc_analytic_css(chem.cas="80-05-7",model="3compartment")
calc_analytic_css(parameters=parameterize_3comp(chem.cas="80-05-7"),model="3compartment")
calc_analytic_css(chem.name="bisphenol a",model="3compartment",tissue="liver")
calc_analytic_css(chem.name="bisphenol a",model="3compartment",tissue="brain")

head(solve_3comp(chem.name="bisphenol a",days=1))
head(solve_3comp(chem.cas="80-05-7",days=1))
head(solve_3comp(parameters=parameterize_3comp(chem.cas="80-05-7"),days=1))

#Test that the input daily.dose and doses.per.day are all that goes through, 
#excluding any default dosing. We want any specified dosing to take the place
#of the default, not add to it.

#first get BW param for 3 comp model:
BW = parameterize_3comp(chem.name = 'bisphenol a')[['BW']]
#and get MW of bisphenol a for checking units
MW = get_physchem_param(param = "MW",chem.name = "bisphenol a")
#record intended default dosing in solve_model when no other dosing specified:
default_initial_dose_target_unscaled = 1 #mg/kg BW
initial_default_dose_target = default_initial_dose_target_unscaled*
  BW/(MW*10^-3) #factor of 10^-3 to convert 
#from g/mol to mg/umol, yielding a dose target in umol
head(initial_default_dose_target)

out_default_dosing = solve_3comp(chem.name = "bisphenol a",days=2)
#The following two initial dose metrics should be the same, and the same as
#the initial_default_dose_target in turn.
initial_default_dose = sum(out_default_dosing[1,])
head(initial_default_dose)
initial_default_dose_intestine = out_default_dosing[1,"Aintestine"]
head(initial_default_dose_intestine)

out_nondefault_dosing = solve_3comp(chem.name = "bisphenol a", 
                                    daily.dose =3,doses.per.day = 5,
                                    days=2)
#so, the dose target of what should appear at time zero in the intestine is:
initial_nondefault_dose_target = 3/5*BW/(MW*10^-3)
head(initial_nondefault_dose_target)

#the following two dose metrics should also be the same:
initial_nondefault_dose = sum(out_nondefault_dosing[2,]) #Use second row because
#it looks like eventdata only gets registered in the output after time zero.
head(initial_nondefault_dose)
initial_nondefault_dose_intestine = out_nondefault_dosing[2,"Aintestine"]
head(initial_nondefault_dose_intestine)

p <- parameterize_3comp(chem.name="Aminopterin")[sort(names(parameterize_3comp(chem.name="Aminopterin")))]
for (this.param in names(p)) cat(paste(this.param,": ",p[[this.param]],"\r\n",sep=""))
quit("no")