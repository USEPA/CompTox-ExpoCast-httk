#R CMD BATCH --no-timing --no-restore --no-save adddata_test.R adddata_test.Rout
library(httk)

# Number of chemicals distributed with the package:
num.chems <- length(get_cheminfo())

fake <- data.frame(Compound="Tester",
                   CASRN="222-11-1",
                   DTXSID="DTX111222",
                   MW=200,
                   logP=3.5,
                   Fup=0.1,
                   Clint=0.1,
                   Clint.pValue=0.001,stringsAsFactors=FALSE)

chem.physical_and_invitro.data <- add_chemtable(
  fake,
  current.table=chem.physical_and_invitro.data,
  data.list=list(
    Compound="Compound",
    CAS="CASRN",
    DTXSID="DTXSID",
    MW="MW",
    logP="logP",
    Funbound.plasma="Fup",
    Clint="Clint",
    Clint.pValue="Clint.pValue"),
  species="Human",
  reference="Fake")

calc_css(chem.name="Tester")

#load_sipes2017()

# We should have the ADMet Predicted chemicals from Sipes et al. (2017),
# this one is a good test since the logP is nearly 10!
#calc_css(chem.cas="26040-51-7")

#Let's see how many chemicals we have now with the Sipes (2017) data loaded)=:
#length(get_cheminfo())

#Now let's reset
reset_httk()

# We should be back to our original number:
num.chems == length(get_cheminfo())

quit("no")