# Load HTTK
library(httk)
# Check to see if we have info on the chemical:
"793-24-8" %in% get_cheminfo()

# Since we don't look up phys-chem from dashboard:
cheminfo <- data.frame(
  Compound="6-PPD",
  CASRN="793-24-8",
  DTXSID="DTXSID9025114",
  logP=4.27, 
  logHenry=log10(7.69e-8),
  logWSol=log10(1.58e-4),
  MP=	99.4,
  MW=268.404
  )
  
# Add the information to HTTK's database:
chem.physical_and_invitro.data <- add_chemtable(
  cheminfo,
  current.table=chem.physical_and_invitro.data,
  data.list=list(
  Compound="Compound",
  CAS="CASRN",
  DTXSID="DTXSID",
  MW="MW",
  logP="logP",
  logHenry="logHenry",
  logWSol="logWSol",
  MP="MP"),
  species="Human",
  reference="CompTox Dashboard 31921")

# Run the Armitage et al. (2014) model:
out <- armitage_eval(
  casrn.vector = "793-24-8", 
  this.FBSf = 0.1,
  this.well_number = 384, 
  nomconc = 10)
  
print(out)
