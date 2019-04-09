#R CMD BATCH other_tests.R other_tests.Rout
library(httk)

fake <- data.frame(Compound="Tester",
                   CASRN="222-11-1",
                   MW=200,
                   logP=3.5,
                   Fup=0.1,
                   Clint=0.1,
                   Clint.pValue=0.001)
chem.physical_and_invitro.data <- add_chemtable(fake,current.table=chem.physical_and_invitro.data,data.list=list(Compound="Compound",CAS="CASRN",MW="MW",logP="logP",Funbound.plasma="Fup",Clint="Clint",Clint.pValue="Clint.pValue"),species="Human",reference="Fake")
calc_css(chem.name="Tester")

load_sipes2017()
# "Tester" should be gone because chem.physical_an_invitro_data was just overwritten:
"Tester" %in% get_cheminfo(info="Compound")
# But we should have the ADMet Predicted chemicals from Sipes et al. (2017),
# this one is a good test since the logP is nearly 10!
calc_css(chem.cas="26040-51-7")
