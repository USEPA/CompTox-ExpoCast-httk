# Search by CAS

# Update with DSSTox Information
# Have to chop this into chunks because the dashboard batch mode can't handle
# more than 5000 chemicals at once (going with 2000 now for sanity's sake)
blocks <- seq(1,dim(chem.physical_and_invitro.data)[1],2000)
blocks <- c(blocks,dim(chem.physical_and_invitro.data)[1]+1)
for (i in 1:(length(blocks)-1))
{
  write.table(chem.physical_and_invitro.data[blocks[i]:(blocks[i+1]-1),"CAS"],
    file=paste("HTTK-ChemIDs-",i,".txt",sep=""),
    row.names=F,
    sep="\t",
    col.names=F,
    quote=F)
  cat(paste("Chemical ID's written to HTTK-ChemIDs-",i,".txt,\n",sep=""))
  cat(" use that file to Batch Search based on CAS.\n")
  cat(paste("Save Dashboard output (comma separated values/csv)to HTTK-DSSTox-output-",i,".csv.\n",sep=""))
}
cat("Download CAS, MW (average mass), desalted (QSAR-ready) SMILES,\n")
cat(" formula, DTXSIDs, and OPERA properties.\n")
cat("Enter \"c\" to continue when ready.\n")
browser()

#
#
# WAIT UNTIL TABLE IS DOWNLOADED FROM DASHBOARD
#
#

#
#
#
# READ IN DSSTOX INFORMATION
#
#
#

dsstox <- NULL
for (i in 1:(length(blocks)-1))
{
  dsstox <- rbind(dsstox,
    read.csv(paste("HTTK-DSSTox-output-",i,".csv",sep="")))
}

# Search by name
#
# STOP TO TRY TO FIND CHEMICALS WHERE CAS DID NOT WORK
#

# Get the chemicals we couldn't find by CAS
write.table(subset(chem.physical_and_invitro.data,
  is.na(DTXSID))[,"Compound"],
  file="HTTK-NoCASMatch-ChemIDs.txt",
  row.names=F,
  sep="\t",
  col.names=F,
  quote=F)
cat("Chemical with NA DTXSID's written to HTTK-NoCASMatch-ChemIDs.txt, use that file to search based on chemical name. \n")
cat("Download CAS, MW, desalted (QSAR-ready) SMILES, forumula, DTXSIDs, and OPERA properties.\n")
cat("Save Dashboard output (csv) to HTTK-NoCASMatch-DSSTox-output.csv.\n")
cat("Enter \"c\" to continue when ready.\n")
browser()
dsstox <- read.csv("HTTK-NoCASMatch-DSSTox-output.csv")

# Get rid of the ones that weren't found:
dsstox <- subset(dsstox, DTXSID!="-")
dsstox <- subset(dsstox, DTXSID!="N/A")
dsstox <- subset(dsstox, !is.na(AVERAGE_MASS))
dsstox <- subset(dsstox, AVERAGE_MASS != "N/A")
dsstox <- subset(dsstox, !is.na(OCTANOL_WATER_PARTITION_LOGP_OPERA_PRED))
dsstox <- subset(dsstox, OCTANOL_WATER_PARTITION_LOGP_OPERA_PRED != "N/A")

if (dim(dsstox)[1]>0)
{
  # Calculate log10 Henry's law constnat:
  dsstox[,"logHenry"] <- log10(as.numeric(dsstox[,
    "HENRYS_LAW_ATM.M3.MOLE_OPERA_PRED"]))
  # Calculate log10 water solubility:
  dsstox[,"logWSol"] <- log10(as.numeric(dsstox[,
    "WATER_SOLUBILITY_MOL.L_OPERA_PRED"]))
  # Set a reasonable precision for numbers:
  dsstox <- set.precision(dsstox)
  
  #
  #
  # Replace any bad CASRN's:
  #
  #
  for (this.row in 1:dim(dsstox)[1])
    if (!is.na(dsstox[this.row,"CASRN"]))
    {
      chem.physical_and_invitro.data[
        chem.physical_and_invitro.data$Compound ==
        dsstox[this.row,"PREFERRED_NAME"],"CAS"] <-
        dsstox[this.row,"CASRN"]
  }
  
  # Pick approved name when there are duplicates:
  for (this.name in unique(dsstox$INPUT[duplicated(dsstox$INPUT)]))
  {
    not.this.name <- subset(dsstox, INPUT!=this.name)
    this.subset <- subset(dsstox, INPUT==this.name)
    this.row <- this.subset[regexpr("Approved",this.subset$FOUND_BY)!=-1,]
    dsstox <- rbind(not.this.name,this.row)
  }
  
  # We just did a by-name search, correct bad CASRN and DTXSID
  for (this.name in unique(dsstox$PREFERRED_NAME))
  {
    this.dtxsid <- dsstox[dsstox$PREFERRED_NAME==this.name,"DTXSID"][1]
    this.cas <- dsstox[dsstox$PREFERRED_NAME==this.name,"CASRN"][1]
    chem.physical_and_invitro.data[chem.physical_and_invitro.data$Compound ==
                                   this.name,"DTXSID"] <- this.dtxsid
    chem.physical_and_invitro.data[chem.physical_and_invitro.data$Compound ==
                                   this.name,"CAS"] <- this.cas
  }

  # Make sure there are no duplicate rows after reading CAS and DTXSID from dashboard:
  chem.physical_and_invitro.data <- subset(chem.physical_and_invitro.data,
                                          !duplicated(CAS) &
                                          !duplicated(DTXSID))
  
  chem.physical_and_invitro.data <- add_chemtable(subset(dsstox,
                                                         !is.na(CASRN) &
                                                         !(CASRN %in% "N/A")),
    current.table = chem.physical_and_invitro.data,
    data.list=list(Compound='PREFERRED_NAME',
      CAS='CASRN',
      DTXSID="DTXSID",
      MW='AVERAGE_MASS',
      SMILES.desalt='QSAR_READY_SMILES',
      Formula="MOLECULAR_FORMULA",
      logP="OCTANOL_WATER_PARTITION_LOGP_OPERA_PRED",
      logHenry = "logHenry",
      logWSol = "logWSol",
      MP = "MELTING_POINT_DEGC_OPERA_PRED"
      ),
    reference="EPA",
    overwrite=T)
}

#
#
# CREATE .SMI FILE FOR OPERA (SO WE CAN GET PKA's)
#
#

write.table(subset(chem.physical_and_invitro.data,
# "N/A" values from CCD break OPERA so remove them:
                   SMILES.desalt != "N/A" &
# SMILES longer than 100 characters break PaDel (I think) so remove those:  
                   as.numeric(sapply(chem.physical_and_invitro.data$SMILES.desalt,nchar)) < 100)[,
            c("SMILES.desalt","CAS")],
  file="HTTK-AllChems.smi",
  row.names=F,
  sep="\t",
  col.names=F,
  quote=F)
cat("Chemical QSAR-ready SMILES written to HTTK-AllChems.smi")
cat(" use that file to in OPERA to generate phys-chem properties including pKa.\n")
cat("Enter \"c\" to continue when ready.\n")
browser()

#
#
# WAIT UNTIL TABLE IS GENERATED (COULD BE 5+ HOURS)
#
#

#
#
#
# READ IN OPERA PREDICTIONS INFORMATION
#
#
#
OPERA.VERSION <- "2.9"
cat(paste("Reading HTTK-AllChems-smi_OPERA",OPERA.VERSION,"Pred.csv\n",sep=""))
opera.preds <- read.csv(paste(
  "HTTK-AllChems-smi_OPERA",OPERA.VERSION,"Pred.csv",sep=""))
