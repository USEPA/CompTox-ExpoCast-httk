# Get chemical properties from CCD:
#CCD <- get_chem_info_batch(DTXSID=chem.physical_and_invitro.data$DTXSID[1:5])
#CCD <- subset(CCD, source=="OPERA")
#CCD <- as.data.frame(cast(CCD, dtxsid ~ name))
CCD <- as.data.frame(get_chemical_details_batch(
         DTXSID=chem.physical_and_invitro.data$DTXSID, 
         Projection = 'chemicaldetailall'))

# Calculate log10 Henry's law constnat:
CCD[,"logHenry_calc"] <- log10(as.numeric(CCD[,
  "henrysLawAtm"]))
# Calculate log10 water solubility:
CCD[,"logWSol_calc"] <- log10(as.numeric(CCD[,
  "waterSolubilityOpera"]))
#calculate water:air partition coefficinent (Kwa = Koa / Kow):
CCD[,"LogPwa_calc"] <- CCD[,"octanolAirPartitionCoeff"] - 
                       CCD[,"octanolWaterPartition"]
  
# Set a reasonable precision for numbers:
CCD <- set.precision(CCD)

# No duplicated values:
CCD <- subset(CCD, !duplicated(CCD))

chem.physical_and_invitro.data <- add_chemtable(CCD,
  current.table = chem.physical_and_invitro.data,
  data.list=list(
    CAS='casrn',
    Compound="preferredName",
    DTXSID="dtxsid",
    MW='averageMass',
    SMILES.desalt='qsarReadySmiles',
    Formula="molFormula",
    logP="octanolWaterPartition",
    logHenry = "logHenry_calc",
    logWSol = "logWSol_calc",
    logPwa = "LogPwa_calc",
    pKa_Donor="pkaaOperaPred",
    pKa_Accept="pkabOperaPred",
    MP = "meltingPointDegcOperaPred"
  ),                                                                        
  reference="EPA-CCD-OPERA",
  overwrite=TRUE)
  