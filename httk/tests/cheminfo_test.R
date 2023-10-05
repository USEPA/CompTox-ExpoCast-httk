# R CMD BATCH --no-timing --no-restore --no-save cheminfo_test.R cheminfo_test.Rout

# Get rid of anything in the workspace:
rm(list=ls()) 

library(httk)

# Check if the number of chemicals has changed:
Css.list <- get_cheminfo()
pbpk.list <- get_cheminfo(model='pbtk')
rat.list <- get_cheminfo(species="rat")
length(Css.list)
length(pbpk.list)
length(rat.list)
                                                 
# check for duplicate entries (all of the following should be TRUE):
# A unique CAS-RN for each row of chem.physical_and_invitro.data table:
length(unique(chem.physical_and_invitro.data$CAS)) == 
  dim(chem.physical_and_invitro.data)[1]
# A unique compound name for each row of chem.physical_and_invitro.data table:
length(unique(chem.physical_and_invitro.data$Compound)) == 
  dim(chem.physical_and_invitro.data)[1]
# A unique DTXSID for each row of chem.physical_and_invitro.data table:
length(unique(subset(chem.physical_and_invitro.data,!is.na(DTXSID))$DTXSID)) == 
  dim(subset(chem.physical_and_invitro.data,!is.na(DTXSID)))[1]
  
# Check if the requirements for diffrent models has changed:
length(get_cheminfo())
length(get_cheminfo(species="rat"))
length(get_cheminfo(model="pbtk"))
length(get_cheminfo(model="pbtk",species="rat"))
length(get_cheminfo(info="all"))
length(get_cheminfo(model="schmitt"))
length(get_cheminfo(model="schmitt",species="rat"))
length(get_cheminfo(model="1compartment"))
length(get_cheminfo(model="1compartment",species="rat"))
  
# Check to see if the paramerer values have changed:
print(subset(get_cheminfo(info="all"), CAS %in% c(
  "80-05-7",
  "335-67-1",
  "1912-24-9",
  "2921-88-2",	
  "117-81-7")),
  row.names=FALSE)

quit("no")