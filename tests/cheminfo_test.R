#R CMD BATCH --no-timing --no-save cheminfo_test.R cheminfo_test.Rout
library(httk)
options(warn=-1)

# Check if the number of chemicals has changed:
Css.list <- get_cheminfo()
pbpk.list <- get_cheminfo(model='pbtk')
rat.list <- get_cheminfo(species="rat")
length(Css.list)
length(pbpk.list)
length(rat.list)
                                                 
# check for duplicate entries (all of the following should be TRUE):
length(unique(chem.physical_and_invitro.data$CAS)) == dim(chem.physical_and_invitro.data)[1]
length(unique(chem.physical_and_invitro.data$Compound)) == dim(chem.physical_and_invitro.data)[1]
length(unique(subset(chem.physical_and_invitro.data,!is.na(DTXSID))$DTXSID)) == 
  dim(subset(chem.physical_and_invitro.data,!is.na(DTXSID)))[1]
  
