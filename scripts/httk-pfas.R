# Clear memory:
rm(list=ls())

# Load httk:
library(httk)

# Load Dawson et al. (2023) Machine Learning predictions:
dawson.2021 <- read.csv("C:/Users/jwambaug/git/CompTox-PFASHalfLife/SupplementalMaterial/S3_Dawsonetal_PFAS_HL_101122.csv")
dawson.2021 <- subset(dawson.2021,
                      Species=="Human" &
                      Sex=="Female" &
                      DosingAdj=="Oral" &
                      ClassModDomain == 1)

# Load all data from the papers (Schmitt model only requires Fup, so we get some chemicals without Clint where we can't make predictions')
PFAS.Css.Table <- get_2023pfasinfo(info="all",model="schmitt")
for (this.chem in get_2023pfasinfo(model="3compartmentss2"))
{
  this.row <- which(PFAS.Css.Table$CAS == this.chem)
  # Check if chemical works in model without exhalation (non- or smei-volatile):)
  set.seed(120512)
  if (this.chem %in% get_2023pfasinfo(model="3compartmentss"))
    PFAS.Css.Table[this.row,"Css95.noexhale.uM.1mgkgday"] <- calc_mc_css(
      chem.cas=this.chem,
      model="3compartmentss",
      output.units="uM")
  set.seed(120512)
  PFAS.Css.Table[this.row,"Css95.exhale.uM.1mgkgday"] <- calc_mc_css(
    chem.cas=this.chem,
    model="3compartmentss2",
    output.units="uM")
}
for (this.chem in PFAS.Css.Table$DTXSID)
  if (this.chem %in% dawson.2021$DTXSID)
  {
    this.row <- which(PFAS.Css.Table$DTXSID == this.chem)
    PFAS.Css.Table[this.row,"Css.ML.uM.1mgkgday"] <-
    dawson.2021[dawson.2021$DTXSID==this.chem,"Css.mgpL"] *
    convert_units(dtxsid=this.chem, 
                  input.units="mg/L",
                  output.units="uM")
  }
write.table(PFAS.Css.Table,file="PFAS.HTTK.Css.uM.txt",
            row.names=FALSE, sep="\t")