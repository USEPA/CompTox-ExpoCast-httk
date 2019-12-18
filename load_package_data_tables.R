# source("L:/Lab/NCCT_ExpoCast/ExpoCast2019/HTTKDataTable/load_package_data_tables-030719.R")
# Get rid of anything in the workspace:
rm(list=ls()) 

SCRIPT.VERSION <- "December2019-1"

library(reshape)
library(gdata)
library(xlsx)
source("add_chemtable.R")
#to use augment.table in the package use augment.table

PKandTISSUEDATAFILE <- "pkdata.xlsx"



#
# CREATE TABLES physiology.data and tissue.data
#

pkdata <- read.xls(PKandTISSUEDATAFILE,sheet="Basic PK",stringsAsFactors=FALSE)
pkdata <- subset(pkdata,!is.na(Human))

tissuevolflowdata <- read.xls(PKandTISSUEDATAFILE,sheet="VolumeFlow",stringsAsFactors=FALSE)
tissuevolflowdata <- subset(tissuevolflowdata,Species!="")[,c("Tissue","Species","Reference","Volume..L.kg.","Blood.Flow..ml.min.kg..3.4..")]

tissuecompdata <- read.xls(PKandTISSUEDATAFILE,sheet="TissueComp",stringsAsFactors=FALSE,skip=1)
tissuecompdata <- subset(tissuecompdata,Species!="")
tissuecompdata <- subset(tissuecompdata, !is.na(Cells))

colnames(tissuecompdata) <- c("Tissue","Species","Reference","Fcell","Fint","FWc","FLc","FPc","Fn_Lc","Fn_PLc","Fa_PLc","pH")
for (this.col in c("Fcell","Fint","FWc","FLc","FPc","Fn_Lc","Fn_PLc","Fa_PLc","pH")) tissuecompdata[,this.col] <- as.numeric(tissuecompdata[,this.col])

colnames(tissuevolflowdata) <- c("Tissue","Species","Reference","Vol (L/kg)","Flow (mL/min/kg^(3/4))")
for (this.col in c("Vol (L/kg)","Flow (mL/min/kg^(3/4))")) tissuevolflowdata[,this.col] <- as.numeric(tissuevolflowdata[,this.col])

tissuedata <- melt(tissuecompdata,id.vars=c("Tissue","Species","Reference"))
tissuedata <- rbind(tissuedata,melt(tissuevolflowdata,id.vars=c("Tissue","Species","Reference")))        
tissuedata$Tissue <- tolower(tissuedata$Tissue)

tissue.data <- tissuedata
physiology.data <- pkdata

#
# END TABLES physiology.data and tissue.data
#




#
# CREATE TABLE chem.physical_and_invitro.data 
#

#Table CLint units are uL/min/10^6 cells:
Wetmore.tables <- read.xls("PublishedRawDataTables.xlsx",stringsAsFactors=F,skip=2)
colnames(Wetmore.tables) <- c("Compound","CAS","Reference","Species","Clint.1","pValue.1","Clint.10","pValue.10","Fub.1","Fub.10")
Wetmore.tables[Wetmore.tables[,"pValue.1"]=="&lt; 0.0001","pValue.1"] <- "0.00009"
Wetmore.tables[Wetmore.tables[,"pValue.10"]=="&lt; 0.0001","pValue.10"] <- "0.00009"
Wetmore.tables[,"pValue.10"] <- as.numeric(Wetmore.tables[,"pValue.10"])
Wetmore.tables[,"pValue.1"] <- as.numeric(Wetmore.tables[,"pValue.1"])
#Wetmore 2012 CAS for pyrithobac and not the pyrithobac salt used in ToxCast:
Wetmore.tables[Wetmore.tables$CAS=="123342-93-8","CAS"] <- "123343-16-8"
Wetmore.tables[Wetmore.tables$Fub.10=="","Fub.10"] <- 0

chem.prop <- add_chemtable(Wetmore.tables, data.list=list(
               Reference="Reference",
               CAS="CAS",
               Compound="Compound",
               Species="Species",
               Clint="Clint.1",
               Clint.pvalue="pValue.1",
               Funbound.plasma="Fub.10"))
               
# overwrite 0 bpa value            
rat.bpa <- subset(Wetmore.tables,CAS == '80-05-7' & Species=='Rat')
chem.prop <- add_chemtable(rat.bpa, current.table=chem.prop,
               data.list=list(
               Reference="Reference",
               CAS="CAS",
               Compound="Compound",
               Species="Species",
               Clint="Clint.10",
               Clint.pvalue="pValue.10"),
               overwrite=T)                  


chem.prop[chem.prop$Compound=="Bensulide",]
sum(chem.prop$Compound=="dibutyl benzene-1,2-dicarboxylate")

#
WetmorePhaseII.fub.table <- read.xls("Supp_Table_3_061114.xls",sheet=1,skip=3,stringsAsFactors=F)
WetmorePhaseII.fub.table$X10.mM[WetmorePhaseII.fub.table$X10.mM>100] <- 100
WetmorePhaseII.fub.table$X10.mM <- as.numeric(WetmorePhaseII.fub.table$X10.mM)/100
chem.prop <- add_chemtable(WetmorePhaseII.fub.table,
               species="Human",
               reference="Wetmore 2015",
               current.table=chem.prop,
               data.list=list(
                 CAS="CAS..",
                 Compound="Chemical",
                 Funbound.plasma="X10.mM"))
                 
WetmorePhaseII.clint.table <- read.xls("Supp_Table_3_061114.xls",sheet=2,skip=4,stringsAsFactors=F)
WetmorePhaseII.clint.table <- subset(WetmorePhaseII.clint.table,Conc.=="1 uM")
WetmorePhaseII.clint.table[,"P.value"] <-as.numeric(gsub("< ","",WetmorePhaseII.clint.table[,"P.value"]))
WetmorePhaseII.clint.table[,"Adj.Cl"] <-as.numeric(WetmorePhaseII.clint.table[,"Adj.Cl"])
chem.prop <- add_chemtable(WetmorePhaseII.clint.table,                                        
               species="Human",
               reference="Wetmore 2015",
               current.table=chem.prop,
               data.list=list(
                 CAS="X.1",
                 Compound="X",
                 Clint="Adj.Cl",
                 Clint.pvalue="P.value"))                 




Obach.table <- read.xls("Obach1999.xlsx",stringsAsFactors=F,skip=1)
#Wrong CAS for Diazepam:
Obach.table[Obach.table$CAS=="53320-84-6","CAS"] <- "439-14-5"

chem.prop <- add_chemtable(Obach.table, current.table=chem.prop, species="Human",reference="Obach 1999",data.list=list(
               CAS="CAS",
               Compound="Compound",
               Rblood2plasma="Blood.to.Plasma.Ratio",
               Funbound.plasma="Fraction.Unbound.in.Plasma"))


chem.prop[chem.prop$Compound=="Bensulide",]
sum(chem.prop$Compound=="dibutyl benzene-1,2-dicarboxylate")

Jones.table <- read.xls("Jones2002.xlsx",stringsAsFactors=F)
chem.prop <- add_chemtable(Jones.table, current.table=chem.prop,species="Human",reference="Jones 2002",data.list=list(
               CAS="CAS",
               Compound="Compouund",
               pKa_Donor="pKa"))

chem.prop[chem.prop$Compound=="Bensulide",]
sum(chem.prop$Compound=="dibutyl benzene-1,2-dicarboxylate")

# Table Clint units for pooled hepatocytes (lot 70 and 73) are ml/min/10^9 cells
# conversion factor is x 1000 ml -> ul and x 1/1000 10^9 to 10^6 cells, so no conversion necessary
Shibata2002.table <- read.xls("Shibata2002.xlsx",stringsAsFactors=F)[-1,]
chem.prop <- add_chemtable(Shibata2002.table, current.table=chem.prop,species="Human",reference="Shibata 2002",data.list=list(
               CAS="CASRN",
               Compound="compound",
               Clint="CLint..in.vitro..70.73",
               Funbound.plasma="fu",
               Rblood2plasma="RB"))


chem.prop[chem.prop$Compound=="Bensulide",]
sum(chem.prop$Compound=="dibutyl benzene-1,2-dicarboxylate")


#Table CLint units are uL/min/10^6 cells:
Lau2002.table <- read.xls("Lau2002.xlsx",stringsAsFactors=F)[-1,]
chem.prop <- add_chemtable(Lau2002.table, current.table=chem.prop, species="Human", reference="Lau2002",
               data.list = list(CAS="CAS",
                 Compound="Compound",
                 Clint="In.Vitro.Clearance"))

chem.prop[chem.prop$Compound=="Bensulide",]
sum(chem.prop$Compound=="dibutyl benzene-1,2-dicarboxylate")

#Table CLint units are uL/min/10^6 cells:
Naritomi.table <- read.xls("Naritomi2003.xlsx",stringsAsFactors=F,skip=1)[1:18,]
#Wrong CAS for Diazepam:
Naritomi.table[Naritomi.table$CAS=="53320-84-6","CAS"] <- "439-14-5"
chem.prop <- add_chemtable(Naritomi.table, current.table=chem.prop, reference="Naritomi 2003",
               data.list = list(CAS="CAS",
                 Compound="Compound",
                 Species="Species",
                 Clint="Clint.invitro..ul.min.10.6.cells.",
                 Funbound.plasma="fp",
                 Rblood2plasma="Rb",
                 Fgutabs="Fa"))

chem.prop[chem.prop$Compound=="Bensulide",]
sum(chem.prop$Compound=="dibutyl benzene-1,2-dicarboxylate")

#Table CLint units are uL/min/10^6 cells:
McGinnity.table <- read.xls("McGinnity2004.xlsx",stringsAsFactors=F)[-1,]
McGinnity.table[McGinnity.table$Human.Hepatic.Clint=="<1.0","Human.Hepatic.Clint"]<-0
chem.prop <- add_chemtable(McGinnity.table,
               current.table=chem.prop,
               reference="McGinnity 2004", species="Human",
               data.list=list(CAS="CASRN",
                 Compound="Compound",
                 Clint="Human.Hepatic.Clint"))

chem.prop[chem.prop$Compound=="Bensulide",]
sum(chem.prop$Compound=="dibutyl benzene-1,2-dicarboxylate")

#Table CLint units are uL/min/10^6 cells:
Ito.table <- read.xls("Ito2004.xlsx",stringsAsFactors=F)
Ito.table[Ito.table$Clint..hepatocyte.=="ND","Clint..hepatocyte."]<-0
chem.prop <- add_chemtable(Ito.table,
               current.table=chem.prop,
               reference="Ito 2004", species="Human",
               data.list=list(CAS="CAS",
                 Compound="Compound",
                 Clint="Clint..hepatocyte.",
                 Funbound.plasma="fub"))

chem.prop[chem.prop$Compound=="Bensulide",]
sum(chem.prop$Compound=="dibutyl benzene-1,2-dicarboxylate")

Schmitt.table <- read.xls("Schmitt2008-41817.xlsx",stringsAsFactors=F)
for (this.row in 1:dim(Schmitt.table)[1])
{
  this.CAS <- Schmitt.table[this.row,"CAS"]
  this.compound <- Schmitt.table[this.row,"Compound"]
  this.reference <- "Schmitt 2008"
  chem.prop <- augment.table(chem.prop,this.CAS,this.compound,"logP",Schmitt.table[this.row,"logP"],reference=this.reference)
  if (Schmitt.table[this.row,"Compound.Type"]=="A")
  {
    chem.prop <- augment.table(chem.prop,this.CAS,this.compound,"pKa_Donor",as.numeric(Schmitt.table[this.row,"pKa"]),reference=this.reference)
    chem.prop <- augment.table(chem.prop,this.CAS,this.compound,"pKa_Accept",NA,reference=this.reference)
  }
  else if (Schmitt.table[this.row,"Compound.Type"]=="B")
  {
    chem.prop <- augment.table(chem.prop,this.CAS,this.compound,"pKa_Donor",NA,reference=this.reference)
    chem.prop <- augment.table(chem.prop,this.CAS,this.compound,"pKa_Accept",as.numeric(Schmitt.table[this.row,"pKa"]),reference=this.reference)
  }
  else if (Schmitt.table[this.row,"Compound.Type"]=="N")
  {
    chem.prop <- augment.table(chem.prop,this.CAS,this.compound,"pKa_Donor",NA,reference=this.reference)
    chem.prop <- augment.table(chem.prop,this.CAS,this.compound,"pKa_Accept",NA,reference=this.reference)
  }
  chem.prop <- augment.table(chem.prop,this.CAS,this.compound,"Funbound.plasma",Schmitt.table[this.row,"Fub"],species=Schmitt.table[this.row,"Species"],reference=this.reference)
  chem.prop <- augment.table(chem.prop,this.CAS,this.compound,"logMA",Schmitt.table[this.row,"logMA"],reference=this.reference)
}


chem.prop[chem.prop$Compound=="Bensulide",]
sum(chem.prop$Compound=="dibutyl benzene-1,2-dicarboxylate")

Obach2008.table <- read.xls("Obach2008.xlsx",stringsAsFactors=F)
Obach2008.table[Obach2008.table$CAS..=="3764-87-2","Name"] <- "Trestolone"
# Dashboard doesn't recognize prefers other CAS:
Obach2008.table[Obach2008.table$CAS..=="229627-58-1","CAS.."] <- "NOCAS_43930"
Obach2008.table[Obach2008.table$CAS..=="85650-52-8","CAS.."] <- "61337-67-5"
Obach2008.table[Obach2008.table$CAS..=="135729-61-2","CAS.."] <- "135729-56-5"
Obach2008.table[Obach2008.table$CAS..=="51931-66-9","CAS.."] <- "32447-90-8"
# Get rid of non-numeric fu values:
Obach2008.table$fu <- as.numeric(Obach2008.table$fu)
Obach2008.table <- subset(Obach2008.table,!is.na(fu))
chem.prop <- add_chemtable(Obach2008.table,
               species="Human",
               reference="Obach 2008",
               current.table=chem.prop,
               data.list=list(
                 CAS="CAS..",
                 Compound="Name",
                 Funbound.plasma="fu"))


chem.prop[chem.prop$Compound=="Bensulide",]
sum(chem.prop$Compound=="dibutyl benzene-1,2-dicarboxylate")


#Table CLint units are L/h/10^6 hepatocytes
TNO.table <- read.xls("HT-PBPK compounds-122216.xlsx",stringsAsFactors=F)
TNO.table <- subset(TNO.table,TNO.table$Source!="")

for (this.row in 1:dim(TNO.table)[1])
{
  this.CAS <- TNO.table[this.row,"CAS.."]
  this.compound <- TNO.table[this.row,"Compound.name"]
  this.reference <- TNO.table[this.row,"Source"]
  if (this.reference != "EPA/Hamner")
  {
    if (!is.na(as.numeric(TNO.table[this.row,"CLint.h"]))) chem.prop <- augment.table(chem.prop,this.CAS,this.compound,"Clint",TNO.table[this.row,"CLint.h"],species="Human",reference=this.reference)
    if (!is.na(as.numeric(TNO.table[this.row,"fup"]))) chem.prop <- augment.table(chem.prop,this.CAS,this.compound,"Funbound.plasma",TNO.table[this.row,"fup"],species="Human",reference=this.reference)
  }
  this.reference <- "TNO"
  chem.prop <- augment.table(chem.prop,this.CAS,this.compound,"logP",log10(TNO.table[this.row,"logP"]),reference=this.reference)
  chem.prop <- augment.table(chem.prop,this.CAS,this.compound,"MW",TNO.table[this.row,"MW"],reference=this.reference)
  chem.prop <- augment.table(chem.prop,this.CAS,this.compound,"Rblood2plasma",TNO.table[this.row,"Rbp"],species="Human",reference=this.reference)
  if (!is.na(TNO.table[this.row,"ion"]))
  {
    if (TNO.table[this.row,"ion"] == "mpa")
    {
      chem.prop <- augment.table(chem.prop,this.CAS,this.compound,"pKa_Donor",as.numeric(TNO.table[this.row,"pKa1"]),reference=this.reference)
      chem.prop <- augment.table(chem.prop,this.CAS,this.compound,"pKa_Accept",NA,reference=this.reference)
    }
    else if (TNO.table[this.row,"ion"] == "dpa")
    {
      chem.prop <- augment.table(chem.prop,this.CAS,this.compound,"pKa_Donor",paste(as.numeric(TNO.table[this.row,"pKa1"]),as.numeric(TNO.table[this.row,"pKa2"]),sep=","),reference=this.reference)
      chem.prop <- augment.table(chem.prop,this.CAS,this.compound,"pKa_Accept",NA,reference=this.reference)
    }
    else if (TNO.table[this.row,"ion"] == "mpb")
    {
      chem.prop <- augment.table(chem.prop,this.CAS,this.compound,"pKa_Accept",as.numeric(TNO.table[this.row,"pKa1"]),reference=this.reference)
      chem.prop <- augment.table(chem.prop,this.CAS,this.compound,"pKa_Donor",NA,reference=this.reference)
    }
    else if (TNO.table[this.row,"ion"] == "dpb")
    {
      chem.prop <- augment.table(chem.prop,this.CAS,this.compound,"pKa_Accept",paste(as.numeric(TNO.table[this.row,"pKa1"]),as.numeric(TNO.table[this.row,"pKa2"]),sep=","),reference=this.reference)
      chem.prop <- augment.table(chem.prop,this.CAS,this.compound,"pKa_Donor",NA,reference=this.reference)
    }
    else if (TNO.table[this.row,"ion"] == "zwi")
    {
      if (regexpr("acid",TNO.table[this.row,"pKa1"])!=-1)
      {
        chem.prop <- augment.table(chem.prop,this.CAS,this.compound,"pKa_Donor",as.numeric(strsplit(TNO.table[this.row,"pKa1"]," ")[[1]][1]),reference=this.reference)
        chem.prop <- augment.table(chem.prop,this.CAS,this.compound,"pKa_Accept",as.numeric(strsplit(TNO.table[this.row,"pKa2"]," ")[[1]][1]),reference=this.reference)
      } else {
        chem.prop <- augment.table(chem.prop,this.CAS,this.compound,"pKa_Accept",as.numeric(strsplit(TNO.table[this.row,"pKa1"]," ")[[1]][1]),reference=this.reference)
        chem.prop <- augment.table(chem.prop,this.CAS,this.compound,"pKa_Donor",as.numeric(strsplit(TNO.table[this.row,"pKa2"]," ")[[1]][1]),reference=this.reference)
      }
    } else {
      chem.prop <- augment.table(chem.prop,this.CAS,this.compound,"pKa_Donor",NA,reference=this.reference)
      chem.prop <- augment.table(chem.prop,this.CAS,this.compound,"pKa_Accept",NA,reference=this.reference)
    }
  }
}

chem.prop[chem.prop$Compound=="Diazepam",]
chem.prop[chem.prop$Compound=="Bensulide",]
sum(chem.prop$Compound=="dibutyl benzene-1,2-dicarboxylate")


## Not sure how find the right parameters to do all the needed conversions to
##  get back to uL/min/million cells, so skipping this data for now. We lose 10
##  chemicals, including 6 with Obach 2008 in vivo data
## Must convert CLint using fub,  fuinc from Naritomi 2003,
##  and RB from Shibata 2002, Naritomi 2003, and Ito and Houston 2004
#Naritomi.table2 <- read.xls("Naritomi2003.xlsx",,sheet=2,stringsAsFactors=F)[-1,]
## Original CLint units are ml/min/kg BW
## Page 1306: 120 * 10^6 cells/g liver
## Page 1306: 1500-1800 g liver/70 kg -- 23.6 g liver/kg BW
## Conversion factor: *1000/23.6/120/10^6             (ml->uL,1/kgBW->1/g liver,1/g liver->1/10^6 cells)->uL/min/10^6 cells)
#Riley.table <- read.xls("Riley2005.xlsx",stringsAsFactors=F,sheet=1)
#for (this.row in 1:dim(Riley.table)[1])
#{
#  this.CAS <- Riley.table[this.row,"CAS"]
#  this.compound <- Riley.table[this.row,"Compound"]
#  this.reference <- "Riley 2005"
#  chem.prop <- augment.table(chem.prop,this.CAS,this.compound,"Human.Fub",Riley.table[this.row,"Fub"],reference=this.reference)
#  Fub <- Riley.table[this.row,"Fub"]
#  if (this.CAS %in% Shibata2002.table$CASRN) Rbp <- Shibata2002.table[Shibata2002.table$CASRN==this.CAS,"RB"]
#  else if (this.CAS %in% Naritomi.table$CAS) Rbp <- Naritomi.table[Naritomi.table$CAS==this.CAS,"RB"]
#  else {
#    Rbp <- chem.prop
#  }
#  if (this.CAS %in% Naritomi.table2$CAS) Fuinc <- Naritomi.table[Naritomi.table2$CAS==this.CAS,"fu.hepatocytes"]
#  else {
#    Fuinc <=
#  }
#  chem.prop <- augment.table(chem.prop,this.CAS,this.compound,"Human.Clint",as.numeric(Riley.table[this.row,"Clint.Predicted"])*1000/23.6/120/10^6,reference=this.reference)
#  if (Riley.table[this.row,"Chemical.Class"]=="N")
#  {
#    chem.prop <- augment.table(chem.prop,this.CAS,this.compound,"pKa_Donor",NA,reference=this.reference)
#    chem.prop <- augment.table(chem.prop,this.CAS,this.compound,"pKb",NA,reference=this.reference)
#  }
#}


#Table CLint units are uL/min/10^6 cells:
# For Rotroff data they average 1 and 10 uM, but then you get the same numbers
Tonnelier.table <- read.xls("Tonnelier-2012.xlsx",stringsAsFactors=F)
# Dashboard prefers different CAS:
Tonnelier.table[Tonnelier.table$Name=="Abamectin","CAS"] <- "71751-41-2"
Tonnelier.table[Tonnelier.table$CAS=="51630-58-1","CAS"] <- "67614-33-9"
Tonnelier.table$CAS <- sapply(Tonnelier.table$CAS,function(x) substr(x,regexpr("[^0]",x),nchar(x)))
chem.prop <- add_chemtable(Tonnelier.table,
               species="Human",
               reference="Tonnelier 2012",
               current.table=chem.prop,
               data.list=list(
                 CAS="CAS",
                 Compound="Name",
                 Clint="CLint..HEP......",
                 Funbound.plasma="Fu",
                 Logp="log.KOW",
                 MW="MW..g.mol."))



for (this.row in 1:dim(Tonnelier.table)[1])
{
  this.CAS <- Tonnelier.table[this.row,"CAS"]
  this.compound <- Tonnelier.table[this.row,"Name"]
  this.reference <- "Tonnelier 2012"
  if (is.na(as.numeric(Tonnelier.table[this.row,"pKa1"])))
  {
    chem.prop <- augment.table(chem.prop,this.CAS,this.compound,"pKa_Donor",NA,reference=this.reference)
    chem.prop <- augment.table(chem.prop,this.CAS,this.compound,"pKa_Accept",NA,reference=this.reference)
  }
}


chem.prop[chem.prop$Compound=="Bensulide",]
sum(chem.prop$Compound=="dibutyl benzene-1,2-dicarboxylate")


chem.prop[chem.prop$Compound=="Bensulide",]


Paixao2012.table2 <- read.xls("Paixao-2012.xlsx",stringsAsFactors=F,sheet=1)
Paixao2012.table2$fup <- 1-Paixao2012.table2$fp
chem.prop <- add_chemtable(Paixao2012.table2,                             
               species="Human",
               reference="Paixao 2012",
               current.table=chem.prop,
               data.list=list(
                 CAS="CAS",
                 Compound="Drug",
                 Funbound.plasma="fup",
                 Rblood2plasma="Rb",
                 Fgutabs="Fabs"))
# Table clint are units of L/h
# 107 x 106 cell g-1 liver (Wilson et al., 2003) and it was also assumed that liver weighed
# 20 g kg-1 of body weight.                 
Paixao2012.table3 <- read.xls("Paixao-2012.xlsx",stringsAsFactors=F,sheet=2,skip=1)
Paixao2012.table3$In.Vitro.Clint..uL.min.106hep <- Paixao2012.table3$In.vitro.Clint..L.h./1000/70/20/107*60
chem.prop <- add_chemtable(Paixao2012.table3,                             
               species="Human",
               reference="Paixao 2012",
               current.table=chem.prop,
               data.list=list(
                 CAS="X",
                 Compound="X.1",
                 Clint="In.Vitro.Clint..uL.min.106hep"))
                 
Paixao2012.table4 <- read.xls("Paixao-2012.xlsx",stringsAsFactors=F,sheet=3,skip=1)
# Table clint are units of L/h
# 107 x 106 cell g-1 liver (Wilson et al., 2003) and it was also assumed that liver weighed
# 20 g kg-1 of body weight.                 
Paixao2012.table4$In.Vitro.Clint..uL.min.106hep <- Paixao2012.table4$In.vitro.Clint..L.h./1000/70/20/107*60
chem.prop <- add_chemtable(Paixao2012.table4,                             
               species="Human",
               reference="Paixao 2012",
               current.table=chem.prop,
               data.list=list(
                 CAS="CAS",
                 Compound="Drug",
                 Clint="In.Vitro.Clint..uL.min.106hep"))
                 
chem.prop[chem.prop$Compound=="Bensulide",]
sum(chem.prop$Compound=="dibutyl benzene-1,2-dicarboxylate")
chem.prop[chem.prop$Compound=="Abamectin",]

chem.prop <- chem.prop[chem.prop$CAS.Checksum,]

chem.prop[chem.prop$Compound=="Bensulide",]
sum(chem.prop$Compound=="dibutyl benzene-1,2-dicarboxylate")
chem.prop[chem.prop$Compound=="Abamectin",]

load("CASback.RData")
library(stringr)
Cory.newpKa <- ret.df
Cory.newpKa[Cory.newpKa$CAS=="3764-87-2","Compound"] <- "Trestolone"
# Dashboard prefers a different cas:
Cory.newpKa[Cory.newpKa$CAS=="85650-52-8","CAS"] <- "61337-67-5"
 
# Old mistake gave wrong CAS for Metoprolol in list given to Cory::
Cory.newpKa <- Cory.newpKa[Cory.newpKa$CAS!="37350-58-6",]
Cory.newpKa$Donor <- str_replace(Cory.newpKa$pKaTools_APKA,";",",")
Cory.newpKa$Accept <- str_replace(Cory.newpKa$pKaTools_BPKA,";",",")
chem.prop <- add_chemtable(Cory.newpKa,
               current.table=chem.prop,
               reference="Strope 2018",
               data.list=list(
                 CAS="CAS",
                 Compound="Compound",
                 pKa_Donor="Donor",
                 pKa_Accept="Accept"))




CorypKaTable <- read.xls("HTPBPK-chems-pKa_CLS.xlsx",stringsAsFactors=F)
# Old mistake caused abamectin to be double listed in list given to Cory:
CorypKaTable <- CorypKaTable[CorypKaTable$CAS!="65195-55-3",]
# Old mistake caused pyrthiobac-sodium to be double listed in list given to Cory:
CorypKaTable <- CorypKaTable[CorypKaTable$CAS!="123342-93-8",]
# Old mistake caused Diazepam to be double listed in list given to Cory::
CorypKaTable <- CorypKaTable[CorypKaTable$CAS!="53320-84-6",]
# Old mistake gave wrong CAS for Metoprolol in list given to Cory::
CorypKaTable <- CorypKaTable[CorypKaTable$CAS!="37350-58-6",]

# Picloram CAS messed up:
CorypKaTable[CorypKaTable$CAS=="1918-02-01","CAS"] <- "1918-02-1"
CorypKaTable[regexpr(",",CorypKaTable$pKa)==-1&!is.na(CorypKaTable$pKa),"pKa"] <- as.character(signif(as.numeric( CorypKaTable[regexpr(",",CorypKaTable$pKa)==-1&!is.na(CorypKaTable$pKa),"pKa"]),3))
CorypKaTable[regexpr(",",CorypKaTable$pKb)==-1&!is.na(CorypKaTable$pKb),"pKb"] <- as.character(signif(as.numeric( CorypKaTable[regexpr(",",CorypKaTable$pKb)==-1&!is.na(CorypKaTable$pKb),"pKb"]),3))
CorypKaTable <- CorypKaTable[CorypKaTable$CAS %in% chem.prop$CAS,]
CorypKaTable$pKa.Reference[!is.na(CorypKaTable$pKa.Reference) & CorypKaTable$pKa.Reference=="SPARC"] <- "Strope 2018"
CorypKaTable$pKb.Reference[!is.na(CorypKaTable$pKb.Reference) & CorypKaTable$pKb.Reference=="SPARC"] <- "Strope 201."


chem.prop <- add_chemtable(CorypKaTable,
               current.table=chem.prop,
               data.list=list(
                 CAS="CAS",
                 Compound="Compound",
                 Reference="pKa.Reference",
                 pKa_Donor="pKa"))

chem.prop <- add_chemtable(CorypKaTable,
               current.table=chem.prop,
               data.list=list(
                 CAS="CAS",
                 Compound="Compound",
                 Reference="pKb.Reference",
                 pKa_Accept="pKb"))


chem.prop[chem.prop$CAS=="33286-22-5","Compound"] <- "Diltiazem hydrochloride"
chem.prop[chem.prop$CAS=="64118-84-9","Compound"] <- "4'-Hydroxydiclofenac"
chem.prop[chem.prop$Compound=="Abamectin",]

load("to_john.RData")

chem.prop <- add_chemtable(to.john,
               current.table=chem.prop,
               data.list=list(
                 CAS="CASRN",
                 Compound="ChemName",
                 pKa_Donor="pKaTools_APKA"),reference="Strope 2018")

chem.prop <- add_chemtable(to.john,
               current.table=chem.prop,
               data.list=list(
                 CAS="CASRN",
                 Compound="ChemName",
                 pKa_Accept="pKaTools_BPKA"),reference="Strope 2018")

MA.data <- read.xls("Endo-2011.xlsx",stringsAsFactors=F)
this.reference <- "Endo 2011"
for (this.row in 1:dim(MA.data)[1])
{
  this.CAS <- MA.data[this.row,"CAS.RN"]
  if (this.CAS %in% chem.prop$CAS)
  {
    this.compound <- MA.data[this.row,"Compound"]
    chem.prop <- augment.table(chem.prop,this.CAS,this.compound,"logMA",MA.data[this.row,"Klipw"],reference=this.reference)
  }
}

chem.prop[chem.prop$Compound=="Bensulide",]
sum(chem.prop$Compound=="dibutyl benzene-1,2-dicarboxylate")
chem.prop[regexpr("Pyrithiobac",chem.prop$Compound)!=-1,]
chem.prop[chem.prop$Compound=="Diazepam",]
chem.prop[chem.prop$Compound=="Picloram",]

# Are all chemical names listed only once:
length(unique(chem.prop$Compound))==dim(chem.prop)[1]
# Are all CAS listed only once:
length(unique(chem.prop$CAS))==dim(chem.prop)[1]

chem.physical_and_invitro.data <- chem.prop

if (unique(chem.physical_and_invitro.data$CAS) < dim(chem.physical_and_invitro.data)[1]) stop("Duplicated CAS numbers in chem.physical_and_invitro.data")
if(any(sapply(chem.physical_and_invitro.data$CAS,function(x) !CAS.checksum(x)))) stop("Failed CAS checksum in chem.physical_and_invitro.data")


cl <- read.xls('Pirovano-2016.xlsx',stringsAsFactors=F)
chem.physical_and_invitro.data <- add_chemtable(cl,current.table=chem.physical_and_invitro.data,data.list=list(Compound='Compound',CAS='CAS',Clint='clint'),species='Human',reference='Pirovano 2016',overwrite=F)
caf.cl <- subset(cl,CAS =='58-08-2')
chem.physical_and_invitro.data <- add_chemtable(caf.cl,current.table=chem.physical_and_invitro.data,data.list=list(Compound='Compound',CAS='CAS',Clint='clint'),species='Human',reference='Pirovano 2016',overwrite=T)
rb <- read.xls('Uchimura 2010 cas.xlsx',stringsAsFactors=F)
chem.physical_and_invitro.data <- add_chemtable(rb,current.table=chem.physical_and_invitro.data,data.list=list(Compound='Name',Rblood2plasma='Human.Rb2p',Funbound.plasma='Human.fup',CAS='cas'),species='Human',reference='Uchimura 2010',overwrite=F)
rb <- subset(rb,!is.na(Rat.Rb2p))
chem.physical_and_invitro.data <- add_chemtable(rb,current.table=chem.physical_and_invitro.data,data.list=list(Compound='Name',Rblood2plasma='Rat.Rb2p',Funbound.plasma='Rat.fup',CAS='cas'),species='Rat',reference='Uchimura 2010',overwrite=F)
fub <- read.xls('Gulden 2002.xlsx',stringsAsFactors=F) 
chem.physical_and_invitro.data <- add_chemtable(fub,current.table=chem.physical_and_invitro.data,data.list=list(Compound='Compound',Funbound.plasma='fup',CAS='CAS',MW='MW'),species='Human',reference='Gulden 2002',overwrite=F)
brown <- read.xls('Brown 2007.xlsx',stringsAsFactors=F)
chem.physical_and_invitro.data <- add_chemtable(brown,current.table=chem.physical_and_invitro.data,data.list=list(Compound='Compound',Clint='Clint',CAS='CAS'),species='Human',reference='Brown 2007',overwrite=T)

#Add Clint data
jones <- read.xlsx('Jones 2017 human in vitro clearance.xlsx',1)
jones <- jones[3:12,1:3]
colnames(jones) <- c('CAS','Compound','Clint')
wood.rat <- read.xlsx('Wood_2017_Rat_Clint_CLh.xlsx',1)
wood.rat <- subset(wood.rat,!is.na(Clint..uL.min.10.6.cells.) & Name != 'FK079') # No CAS for FK079
wood.human <- read.xlsx('Wood 2017 Human Clint and CLh.xlsx',1)
wood.human <- subset(wood.human,!is.na(Clint..uL.min.10.6.cells.))

sternbeck <- read.xlsx('Sternbeck Human Clearance.xlsx',1)
chem.physical_and_invitro.data <- add_chemtable(jones,current.table=chem.physical_and_invitro.data,data.list=list(Compound='Compound',CAS='CAS',Clint='Clint'),species='Human',reference='Jones 2017',overwrite=T)
chem.physical_and_invitro.data <- add_chemtable(wood.human,current.table=chem.physical_and_invitro.data,data.list=list(Compound='Name',CAS='CAS',Clint='Clint..uL.min.10.6.cells.'),species='Human',reference='Wood 2017',overwrite=T)
chem.physical_and_invitro.data <- add_chemtable(wood.rat,current.table=chem.physical_and_invitro.data,data.list=list(Compound='Name',CAS='CASRN',Clint='Clint..uL.min.10.6.cells.'),species='Rat',reference='Wood 2017',overwrite=T)
# Add new fup from Wood 2017
chem.physical_and_invitro.data <- add_chemtable(wood.human,current.table=chem.physical_and_invitro.data,data.list=list(Compound='Name',CAS='CAS',Funbound.plasma='fup'),species='Human',reference='Wood 2017',overwrite=F)
chem.physical_and_invitro.data <- add_chemtable(wood.rat,current.table=chem.physical_and_invitro.data,data.list=list(Compound='Name',CAS='CASRN',Funbound.plasma='fup'),species='Rat',reference='Wood 2017',overwrite=F)
#Add only 2 compounds with clint and fup, overwrite all Rb2p but paixao
chem.physical_and_invitro.data <- add_chemtable(subset(sternbeck,Name %in% c('Etodolac','Bufuralol')),current.table=chem.physical_and_invitro.data,data.list=list(Compound='Name',CAS='CAS',Funbound.plasma='fup',Clint='Clint'),species='Human',reference='Sternbeck 2012',overwrite=T)
chem.physical_and_invitro.data <- add_chemtable(subset(sternbeck,!CAS %in% subset(chem.physical_and_invitro.data,Human.Rblood2plasma.Reference == 'Paixao 2012')[,'CAS']),current.table=chem.physical_and_invitro.data,data.list=list(Compound='Name',CAS='CAS',Rblood2plasma='Rb'),species='Human',reference='Sternbeck 2012',overwrite=T)


#Remove overwritten clint pvalues
for(this.cas in subset(chem.physical_and_invitro.data,Human.Clint.pValue.Reference != Human.Clint.Reference)[,'CAS']) chem.physical_and_invitro.data[which(chem.physical_and_invitro.data[,'CAS'] == this.cas),'Human.Clint.pValue'] <- chem.physical_and_invitro.data[which(chem.physical_and_invitro.data[,'CAS'] == this.cas),'Human.Clint.pValue.Reference'] <- NA

# Load partition coefficient data from Pearce 2017:
pc.data.raw <- read.xls('PC_Data.xlsx',stringsAsFactors=F)
pc.data.table <- subset(pc.data.raw,tolower(Species)=='rat')
pc.data.table[which(pc.data.table[,'CAS'] %in% c('10457-90-6','5786-21-0','17617-23-1','69-23-8','2898-12-6','57562-99-9','59-99-4','2955-38-6','155-97-5','41903-57-5','58-55-9','77-32-7','59-05-2','60-54-8')),'fu'] <- NA
#pc.data <- subset(pc.data, Tissue %in% c("Adipose","Bone","Brain","Gut","Heart","Kidney","Liver","Lung","Muscle","Skin","Spleen","Blood Cells") & Species == 'Rat')   
chem.physical_and_invitro.data <- add_chemtable(pc.data.table,
                                                current.table=chem.physical_and_invitro.data,
                                                data.list=list(Compound='Drug',
                                                               CAS='CAS',
                                                               Funbound.plasma='fu',
                                                               Species='Species',
                                                               logP='LogP',
                                                               logMA='logMA',
                                                               pKa_Donor='Donor',
                                                               pKa_Accept='Accept'),
                                                reference='Pearce 2017',
                                                overwrite=T)

pc.data <- pc.data.raw[,c('CAS','Drug','Tissue','Species','fu','A.B.N','LogP','Exp_PC')]


chem.physical_and_invitro.data[which(chem.physical_and_invitro.data[,'CAS'] == '37517-30-9'),'All.Compound.Names'] <- 'Acebutolol'
chem.physical_and_invitro.data[which(chem.physical_and_invitro.data[,'CAS'] == '28434-00-6'),'Compound'] <- chem.physical_and_invitro.data[which(chem.physical_and_invitro.data[,'CAS'] == '28434-00-6'),'All.Compound.Names'] <- 's-bioallethrin'
chem.physical_and_invitro.data[which(chem.physical_and_invitro.data[,'CAS'] == '119-90-4'),'Compound'] <- chem.physical_and_invitro.data[which(chem.physical_and_invitro.data[,'CAS'] == '119-90-4'),'All.Compound.Names'] <-  "3,3'-dimethoxybenzidine"
chem.physical_and_invitro.data[which(chem.physical_and_invitro.data[,'CAS'] == '1951-25-3'),'Compound'] <- chem.physical_and_invitro.data[which(chem.physical_and_invitro.data[,'CAS'] == '1951-25-3'),'All.Compound.Names'] <-  "Amiodarone"
chem.physical_and_invitro.data[which(chem.physical_and_invitro.data[,'CAS'] == '357-70-0'),'Compound'] <- chem.physical_and_invitro.data[which(chem.physical_and_invitro.data[,'CAS'] == '357-70-0'),'All.Compound.Names']  <-  "Galantamine"
chem.physical_and_invitro.data[which(chem.physical_and_invitro.data[,'CAS'] == '57-41-0'),'Compound'] <-  "Phenytoin"
chem.physical_and_invitro.data[which(chem.physical_and_invitro.data[,'CAS'] == '330-54-1'),'Compound'] <- chem.physical_and_invitro.data[which(chem.physical_and_invitro.data[,'CAS'] == '330-54-1'),'All.Compound.Names']  <-  "Diuron"
chem.physical_and_invitro.data[which(chem.physical_and_invitro.data[,'CAS'] == '91524-16-2'),'Compound'] <- chem.physical_and_invitro.data[which(chem.physical_and_invitro.data[,'CAS'] == '91524-16-2'),'All.Compound.Names']  <-  "Timolol hemihydrate"
chem.physical_and_invitro.data[which(chem.physical_and_invitro.data[,'CAS'] == '101193-40-2'),'All.Compound.Names'] <- 'Quinotolast'
chem.physical_and_invitro.data[which(chem.physical_and_invitro.data[,'CAS'] == '77-28-1'),'All.Compound.Names'] <- 'Butylbarbitone|Butethal [nf]|Butethal'
chem.physical_and_invitro.data[which(chem.physical_and_invitro.data[,'CAS'] == '65216-93-5'),'All.Compound.Names'] <- 'Ethoxycoumarin|3-ethoxychromen-2-one'
chem.physical_and_invitro.data[which(chem.physical_and_invitro.data[,'CAS'] == '509-86-4'),'All.Compound.Names'] <- 'Heptabarbitone|Heptabarbital'
chem.physical_and_invitro.data[which(chem.physical_and_invitro.data[,'CAS'] == '58-08-2'),'Compound'] <- 'Caffeine'

#Honda 2019:
load("Honda2019/wetmore_fup.RData") #Some where rat fups were inappropriately truncated
wetmore.fup <- as.data.frame(wetmore.fup)
chem.physical_and_invitro.data <- add_chemtable(wetmore.fup,
                                                current.table=chem.physical_and_invitro.data,
                                                data.list=list(CAS="casrn",
                                                               Funbound.plasma="fup"),
                                                reference="Wetmore 2013",
                                                species="Rat",
                                                overwrite=T)
# New chemicals:
load("Honda2019/full_new_rat_04Dec2018.RData")
full.new.rat <- as.data.frame(full.new.rat)
full.new.rat <- subset(full.new.rat,use_fup|use_clint)

chemprop.new.rat <- unique(full.new.rat[,c("casrn",
                                           "DSSTox_Substance_Id",
                                           "logP",
                                           "MW",
                                           "pKa_Accept",
                                           "pKa_Donor",
                                           "preferred_name")])
# update phys-chem props
chem.physical_and_invitro.data <- add_chemtable(chemprop.new.rat,
                                                current.table=chem.physical_and_invitro.data,
                                                 data.list=list(CAS="casrn",
                                                                DTXSID="DSSTox_Substance_Id",
                                                                LogP="logP",MW="MW",
                                                                pKa_Accept="pKa_Accept",
                                                                pKa_Donor="pKa_Donor",
                                                                Compound = "preferred_name"),
                                                 reference="Honda 2019",
                                                 species="Rat",
                                                 overwrite=T)


# only use the clints that greg identified as good:
chem.physical_and_invitro.data <- add_chemtable(subset(full.new.rat,use_clint),
                                                current.table=chem.physical_and_invitro.data,
                                                data.list=list(CAS="casrn",
                                                               DTXSID="DSSTox_Substance_Id",
                                                               Clint="clearance",
                                                               Compound = "preferred_name"),
                                                reference="Honda 2019",
                                                species="Rat",
                                                overwrite=T)
# only use the fups that greg identified as good:
chem.physical_and_invitro.data <- add_chemtable(subset(full.new.rat,use_fup),
                                                current.table=chem.physical_and_invitro.data,
                                                data.list=list(CAS="casrn",
                                                               DTXSID="DSSTox_Substance_Id",
                                                               Funbound.plasma="Funbound.plasma",
                                                               Compound = "preferred_name"),
                                                reference="Honda 2019",
                                                species="Rat",
                                                overwrite=T)

#Load data from Wambaugh 2019:

load("New-HTTK-raw-2019-05-06.RData")

new.httk.data$Human.Clint <- paste(new.httk.data$Human.Clint,
                                    new.httk.data$Human.Clint.Low95,
                                    new.httk.data$Human.Clint.High95,
                                    new.httk.data$Human.Clint.pValue,sep=",")
new.httk.data$Human.Funbound.plasma <- paste(new.httk.data$Human.Funbound.plasma,
                                   new.httk.data$Human.Funbound.plasma.Low95,
                                   new.httk.data$Human.Funbound.plasma.High95,
                                   sep=",")
new.httk.data[new.httk.data$Human.Clint=="NA,NA,NA,NA", "Human.Clint"] <- NA
new.httk.data[new.httk.data$Human.Funbound.plasma=="NA,NA,NA", "Human.Funbound.plasma"] <-NA
                                   

chem.physical_and_invitro.data <- add_chemtable(new.httk.data,
  current.table=chem.physical_and_invitro.data,
  data.list=list(Compound="Compound",
    CAS="CAS",
    DTXSID="DSSTox_Substance_Id",
    Clint="Human.Clint",
    Clint.pValue="Human.Clint.pValue",
    Funbound.plasma="Human.Funbound.plasma",
    LogP="logP",MW="MW",
    pKa_Accept="pKa_Accept",
    pKa_Donor="pKa_Donor",
    SMILES.desalt="SMILES"),
  reference="Wambaugh 2019",
  species="Human",
  overwrite=T)



# ADD NEW DATA HERE:
volatile.data.raw <- read.csv('Linakis2019InhalationReferenced.csv', stringsAsFactors = F)

chem.physical_and_invitro.data <- add_chemtable(volatile.data.raw,
                                                current.table=chem.physical_and_invitro.data,
                                                data.list=list(Compound="PREFERRED_NAME",
                                                               CAS="CASRN",
                                                               DTXSID="DTXSID",
                                                               LogP="OCTANOL_WATER_PARTITION_LOGP_OPERA_PRED",
                                                               LogHenry="LOG_HENRYS_LAW_DIMENSIONLESS",
                                                               MW="AVERAGE_MASS",
                                                               SMILES.desalt="QSAR_READY_SMILES",
                                                               Species="SPECIES"),
                                                overwrite=F,
                                                reference="Linakis submitted")

chem.physical_and_invitro.data <- add_chemtable(volatile.data.raw,
                                                current.table=chem.physical_and_invitro.data,
                                                data.list=list(Compound="PREFERRED_NAME",
                                                               CAS="CASRN",
                                                               DTXSID="DTXSID",
                                                               Clint="CALC_CLINT",
                                                               Funbound.plasma="CALC_FUP",
                                                               Reference="REFERENCE",
                                                               Species="SPECIES"),
                                                overwrite=F)



# STOP ADDING NEW DATA AFTER THIS, SUBSEQUENT CODE IS TO INTERACT WITH DASHBOARD

#
# STOP TO GET NEW PHYSCHEM
#

# Update with DSSTox Information
write.table(chem.physical_and_invitro.data[,c("Compound","CAS")],file="HTTK-ChemIDs.txt",row.names=F,sep="\t")
cat("Chemical ID's written to HTTK-ChemIDs.txt, use that file to download CAS, MW, desalted (QSAR-ready) SMILES, forumula, DTXSIDs, and OPERA properties.\n")
cat("Save Dashboard output to HTTK-DSSTox-output.xls.\n")
browser()

#
# WAIT UNTIL TABLE IS DOWNLOADED FROM DASHBOARD
#

dsstox <- read.xlsx("HTTK-DSSTox-output.xls",stringsAsFactors=F,1)
# Get rid of the ones that weren't found:
dsstox <- subset(dsstox,DTXSID!="-")
dsstox[,"logHenry"] <- log10(as.numeric(dsstox$HENRYS_LAW_ATM.M3.MOLE_OPERA_PRED))
dsstox[,"logWSol"] <- log10(as.numeric(dsstox$WATER_SOLUBILITY_MOL.L_OPERA_PRED))
chem.physical_and_invitro.data <- add_chemtable(subset(dsstox,!is.na(CASRN)),
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
                                    reference=paste('CompTox Dashboard',Sys.Date()),
                                    overwrite=T)




#
# STOP TO TRY TO FIND CHEMICALS WHERE CAS DID NOT WORK
#

# Get the chemicals we couldn't find by CAS
write.csv(subset(chem.physical_and_invitro.data,is.na(DTXSID))[,c("Compound","CAS")],file="HTTK-BadCAS-ChemIDs.txt",row.names=F)
cat("Chemical with NA DTXSID's written to HTTK-BadCAS-ChemIDs.txt, use that file to download CAS, MW, desalted (QSAR-ready) SMILES, forumula, DTXSIDs, and OPERA properties.\n")
cat("Save Dashboard output to HTTK-BadCAS-DSSTox-output.xls.\n")
browser()
dsstox <- read.xlsx("HTTK-BadCAS-DSSTox-output.xls",stringsAsFactors=F,1)
# Get rid of the ones that weren't found:
dsstox <- subset(dsstox,DTXSID!="-")
dsstox[,"logHenry"] <- log10(as.numeric(dsstox$HENRYS_LAW_ATM.M3.MOLE_OPERA_PRED))
dsstox[,"logWSol"] <- log10(as.numeric(dsstox$WATER_SOLUBILITY_MOL.L_OPERA_PRED))
chem.physical_and_invitro.data <- add_chemtable(subset(dsstox,!is.na(CASRN)),
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
                                    reference=paste('CompTox Dashboard',Sys.Date()),
                                    overwrite=T)

# Some chemicals are missing from DSStox OPERA predictions:
new.opera <- read.csv('MissingPhysChem.csv',stringsAsFactors=F) 
colnames(new.opera)[1] <- "INPUT"
new.opera[,"logHenry"] <- log10(as.numeric(new.opera$HENRYS_LAW_ATM.M3.MOLE_OPERA_PRED))
new.opera[,"logWSol"] <- log10(as.numeric(new.opera$WATER_SOLUBILITY_MOL.L_OPERA_PRED))
chem.physical_and_invitro.data <- add_chemtable(new.opera,current.table = chem.physical_and_invitro.data,
                                                data.list=list(CAS='CASRN',
                                                               Compound='INPUT',
                                                               logP='OCTANOL_WATER_PARTITION_LOGP_OPERA_PRED',
                                                               logHenry = "logHenry",
                                                               logWSol = "logWSol",
                                                               MP = "MELTING_POINT_DEGC_OPERA_PRED",
                                                               MW='AVERAGE_MASS'),
                                                reference='OPERA')

# Not sure how to get water:air patition coefficient from OPERA 
# (need to check into Henry's law coefficient)
# Use EPI Suite for now
load("chemprops-072115.RData")
#Dashboard doesn't like this CAS:
chemprop.table[chemprop.table$CASRN=="51630-58-1","CASRN"] <- "67614-33-9"
chemprop.table <- subset(chemprop.table,CASRN%in%chem.physical_and_invitro.data[,"CAS"])
chemprop.table$logHenry <- log10(as.numeric(chemprop.table$Henry))
chem.physical_and_invitro.data <- add_chemtable(chemprop.table,
                                                current.table = chem.physical_and_invitro.data,
                                                data.list=list(CAS="CASRN",
                                                               logHenry="logHenry",
                                                               logP="LogP",
                                                               MP="MP",
                                                               MW="MolecularWeight",
                                                               logPwa="logPwa37p5"),
                                                reference="EPISuite")

#Add Strope 2018 new pKa
load('Strope2018.RData')
cas.donor.overwrite <- subset(chem.physical_and_invitro.data,pKa_Donor.Reference %in% c('Strope 2018','Strope 2018'))[,'CAS']
cas.accept.overwrite <- subset(chem.physical_and_invitro.data,pKa_Accept.Reference %in% c('Strope 2018','Strope 2018'))[,'CAS']
cory.donor.overwrite <- subset(CorypKaTable,CASRN.DSStox %in% cas.donor.overwrite)
cory.accept.overwrite <- subset(CorypKaTable,CASRN.DSStox %in% cas.accept.overwrite)
chem.physical_and_invitro.data <- add_chemtable(cory.accept.overwrite,current.table=chem.physical_and_invitro.data,data.list=list(CAS='CASRN.DSStox',pKa_Accept='Accept'),reference='Strope 2018',overwrite=T)
chem.physical_and_invitro.data <- add_chemtable(cory.donor.overwrite,current.table=chem.physical_and_invitro.data,data.list=list(CAS='CASRN.DSStox',pKa_Donor='Donor'),reference='Strope 2018',overwrite=T)

#
# END TABLE chem.physical_and_invitro.data 
#







#
# CREATE TABLE Wetmore.data
#

Wetmore.data <- read.xls("Supp_Table_8_SimCyp_IVIVE_input_output_083011.xls",stringsAsFactors=F,skip=1)

Wetmore.data.1 <- Wetmore.data[Wetmore.data[,"Concentration..uM."]==1,]
Wetmore.data.10 <- Wetmore.data[Wetmore.data[,"Concentration..uM."]==10,]
Wetmore.data.abridged <- NULL
Wetmore.data.1.CASlist <- unique(Wetmore.data.1[,"CASRN"])
Wetmore.data.10.CASlist <- unique(Wetmore.data.10[,"CASRN"])
good.cols <- c(1:19)
for (this.CAS in unique(Wetmore.data[,"CASRN"]))
{
  if (this.CAS %in% Wetmore.data.1.CASlist) Wetmore.data.abridged <- rbind(Wetmore.data.abridged,Wetmore.data.1[Wetmore.data.1[,"CASRN"]==this.CAS,good.cols][1,])
  if (this.CAS %in% Wetmore.data.10.CASlist) Wetmore.data.abridged <- rbind(Wetmore.data.abridged,Wetmore.data.10[Wetmore.data.10[,"CASRN"]==this.CAS,good.cols][1,])
}
Wetmore.data.abridged <- cbind(Wetmore.data.abridged,"Human")
colnames(Wetmore.data.abridged)[20] <- "Species"
Wetmore.data.abridged$Reference <- "Wetmore 2012"

Wetmore.rat.data <- read.xls("toxsci_12_0787_File012.xls",skip=2,stringsAsFactors=F)
Wetmore.rat.data.1 <- Wetmore.rat.data[Wetmore.rat.data[,"Conc.uM."]==1,]
Wetmore.rat.data.10 <- Wetmore.rat.data[Wetmore.rat.data[,"Conc.uM."]==10,]
Wetmore.rat.data.abridged <- NULL
Wetmore.rat.data.1.CASlist <- unique(Wetmore.rat.data.1[,"CASRN"])
Wetmore.rat.data.10.CASlist <- unique(Wetmore.rat.data.10[,"CASRN"])
good.cols <- c(1,2,3,4,9,9,5,10,6,11,7,8,12,15,16)
for (this.CAS in unique(Wetmore.rat.data[,"CASRN"]))
{
  if (this.CAS %in% Wetmore.rat.data.1.CASlist) Wetmore.rat.data.abridged <- rbind(Wetmore.rat.data.abridged,Wetmore.rat.data.1[Wetmore.rat.data.1[,"CASRN"]==this.CAS,good.cols][1,])
  if (this.CAS %in% Wetmore.rat.data.10.CASlist) Wetmore.rat.data.abridged <- rbind(Wetmore.rat.data.abridged,Wetmore.rat.data.10[Wetmore.rat.data.10[,"CASRN"]==this.CAS,good.cols][1,])
}

Wetmore.rat.data.abridged[,5] <- Wetmore.rat.data.abridged[,5]*100
Wetmore.rat.data.abridged <- cbind(Wetmore.rat.data.abridged,NA)
Wetmore.rat.data.abridged <- cbind(Wetmore.rat.data.abridged,NA)
Wetmore.rat.data.abridged <- cbind(Wetmore.rat.data.abridged,NA)
Wetmore.rat.data.abridged <- cbind(Wetmore.rat.data.abridged,NA)
Wetmore.rat.data.abridged <- cbind(Wetmore.rat.data.abridged,"Rat")
Wetmore.rat.data.abridged[,18] <- Wetmore.rat.data.abridged[,15]
Wetmore.rat.data.abridged[,15] <- Wetmore.rat.data.abridged[,14]
Wetmore.rat.data.abridged[,14] <- NA
Wetmore.rat.data.abridged$Reference <- "Wetmore 2013"
colnames(Wetmore.rat.data.abridged) <- colnames(Wetmore.data.abridged)

Wetmore.data <- rbind(Wetmore.data.abridged,Wetmore.rat.data.abridged)
colnames(Wetmore.data)[2] <- "CAS"

#Get rid of mixture milbemectin:
Wetmore.data <- Wetmore.data[Wetmore.data$CAS!="51596-11-3",]

WetmorePhaseII.css.table <- read.xls("Supp_Table_4_122914.xlsx",sheet=5,skip=0,stringsAsFactors=F)

for (this.cas in unique(WetmorePhaseII.css.table$CASRN))
{
  this.row <- Wetmore.data[1,]
  this.row[] <- NA
  this.row["Species"] <- "Human"
  fub.index <- which(WetmorePhaseII.fub.table$CAS == this.cas)[1]
  clint.index <- which(WetmorePhaseII.clint.table$X.1 == this.cas)[1]
  this.row["Compound"] <- WetmorePhaseII.fub.table[fub.index,"Chemical"]
  this.row["CAS"] <- this.cas
  this.row["Reference"] <- "Wetmore 2015"
  if (this.cas %in% WetmorePhaseII.css.table$CASRN)
  {
    for (this.conc in unique(subset(WetmorePhaseII.css.table,CASRN==this.cas)$Conc))
    {
      this.row["Concentration..uM."] <- this.conc
      css.index <- which(WetmorePhaseII.css.table$CASRN == this.cas & WetmorePhaseII.css.table$Conc==this.conc)[1]
      this.row["MW"] <- WetmorePhaseII.css.table[css.index,"Molec.Wt."]
      this.row["Raw.Experimental.Percentage.Unbound"] <- WetmorePhaseII.css.table[css.index,"Fu"]*100
      this.row["Entered.Experimental.Percentage.Unbound"] <- WetmorePhaseII.css.table[css.index,"Fu.2"]*100
      if (WetmorePhaseII.css.table[css.index,"Fu.1"]=="Def") this.row["source_PPB"] <- "D"
      else this.row["source_PPB"] <- "E" 
      this.row["Renal_Clearance"] <- WetmorePhaseII.css.table[css.index,"Renal.Cl"]
      this.row["Css_lower_5th_perc.uM."] <- WetmorePhaseII.css.table[css.index,"Css_Lower_5th_perc.uM."]
      this.row["Css_median_perc.uM."] <- WetmorePhaseII.css.table[css.index,"Css_Median_perc.uM."]
      this.row["Css_upper_95th_perc.uM."] <- WetmorePhaseII.css.table[css.index,"Css_upper_95th_perc.uM."]
      this.row["Css_lower_5th_perc.mg.L."] <- WetmorePhaseII.css.table[css.index,"Css_Lower_5th_perc.uM."]/10^6*this.row["MW"]*1000
      this.row["Css_median_perc.mg.L."] <- WetmorePhaseII.css.table[css.index,"Css_Median_perc.uM."]/10^6*this.row["MW"]*1000
      this.row["Css_upper_95th_perc.mg.L."] <- WetmorePhaseII.css.table[css.index,"Css_upper_95th_perc.uM."]/10^6*this.row["MW"]*1000
      this.row["Fub"] <-  WetmorePhaseII.css.table[css.index,"Fu"]
      this.row["Met_Stab"] <- NA
      this.row["Met_Stab_entered"] <- WetmorePhaseII.css.table[css.index,"Met.Stab.Ent"]
      this.row["r2"] <- NA
      this.row["p.val"] <- NA
      Wetmore.data <- rbind(Wetmore.data,this.row)    
    }
  }
}

if (any(duplicated(Wetmore.data))) stop("Duplicate entries in Wetmore.data")

#
# END TABLE Wetmore.data
#




#
# CREATE chem.lists 
#

# Read in the Tox21 and ToxCast lists from the Dashboard
Tox21 <- read.csv("Dashboard-Tox21.tsv",stringsAsFactors=F,sep="\t")
ToxCast <- read.csv("Dashboard-Tox21.tsv",stringsAsFactors=F,sep="\t")
DrugBank <- read.csv("Dashboard-DrugBank.tsv",stringsAsFactors=F,sep="\t")

NHANES.serum <- read.xls("ACT-p2m-20150330.xlsx",sheet=1,stringsAsFactors=F)
NHANES.blood <- read.xls("ACT-p2m-20150330.xlsx",sheet=2,stringsAsFactors=F)
NHANES.urine <- read.xls("ACT-p2m-20150330.xlsx",sheet=3,stringsAsFactors=F)

chem.lists <- list()
chem.lists[["NHANES.serum.parent"]] <- NHANES.serum[-1,c(1,3)]
chem.lists[["NHANES.serum.analyte"]] <- NHANES.serum[-1,c(4,6)]
chem.lists[["NHANES.blood.parent"]] <- NHANES.blood[-1,1:2]
chem.lists[["NHANES.blood.analyte"]] <- NHANES.blood[-1,3:4]
chem.lists[["NHANES.urine.parent"]] <- NHANES.urine[-1,1:2]
chem.lists[["NHANES.urine.analyte"]] <- NHANES.urine[-1,4:5]
#chem.lists[["EPA.invivo"]] <- EPAinvivo[EPAinvivo[,"In.Vivo.TK.Study.Underway"]=="Y",1:2]
chem.lists[["Tox21"]] <- Tox21[,c(2:3,1)]
chem.lists[["ToxCast"]] <- ToxCast[,c(2:3,1)]
chem.lists[["DrugBank"]] <- DrugBank[,c(2:3,1)]

for(i in 1:length(chem.lists))
{
  colnames(chem.lists[[i]]) <- c("Compound","CAS")
  chem.lists[[i]] <- chem.lists[[i]][!duplicated(chem.lists[[i]]),]
#  chem.lists[[i]] <- chem.lists[[i]][sapply(chem.lists[[i]][,2],CAS.checksum),]
}
chem.lists[["NHANES"]] <- rbind(chem.lists[["NHANES.serum.parent"]],chem.lists[["NHANES.serum.analyte"]],chem.lists[["NHANES.blood.parent"]],chem.lists[["NHANES.blood.analyte"]],chem.lists[["NHANES.urine.parent"]],chem.lists[["NHANES.urine.analyte"]])
chem.lists[["NHANES"]] <- chem.lists[["NHANES"]][!duplicated(chem.lists[["NHANES"]]$CAS),]

#
# END chem.lists 
#






#Add in vivo data from Wambaugh (2018):
load('NewInVivoTablesForHTTK.RData')

# Rename Obach data for distribution with packages                                               
                                                 
obach2008 <- Obach2008.table

#
# Load data from Sipes 2017:
#
sipes2017 <- readRDS("ADMET.data.table.RData")
# Add the predicted parameters at the very end so that we don't overwrite measured data:
sipes2017 <-merge(sipes2017,Tox21,by.x="CAS",by.y="CASRN",all.x=T)
sipes2017.table <- add_chemtable(sipes2017,
                                  current.table=chem.physical_and_invitro.data,
                                  data.list=list(Compound='Compound', 
                                    CAS='CAS',
                                    DTXSID="DTXSID", 
                                    MW = 'MW', 
                                    logP = 'logP',
                                    pKa_Donor = 'pKa_Donor', 
                                    pKa_Accept = 'pKa_Accept',
                                    Funbound.plasma = 'Human.Funbound.plasma', 
                                    Clint = 'Human.Clint', 
                                    SMILES.desalt = 'SMILES.desalt'),
                                  reference = 'Sipes 2017', 
                                  species= 'Human', 
                                  overwrite=F)

if (dim(subset(chem.physical_and_invitro.data,duplicated(Compound)))[1]>0) browser()





#
# WRITE OUT DATA
#

write.table(chem.physical_and_invitro.data,file="HTTK-Chem-Props.txt",row.names=F,quote=F,sep="\t")
write.table(chem.invivo.PK.data,file="HTTK-Chem-InVivo-Data.txt",row.names=F,quote=F,sep="\t")
write.table(chem.invivo.PK.aggregate.data,file="HTTK-Chem-InVivo-Aggregate-Data.txt",row.names=F,quote=F,sep="\t")
write.table(chem.invivo.PK.summary.data,file="HTTK-Chem-InVivo-Summary-Data.txt",row.names=F,quote=F,sep="\t")
write.table(physiology.data,file="HTTK-Physiology-Data.txt",row.names=F,quote=F,sep="\t")
write.table(tissue.data,file="HTTK-Tissue-Data.txt",row.names=F,quote=F,sep="\t")

Tables.Rdata.stamp <- paste("This Tables.RData file was created on",Sys.Date(),"by script version",SCRIPT.VERSION)
#Write the tables.Rdata file:                                  
save(chem.physical_and_invitro.data,
     chem.invivo.PK.data,
     chem.invivo.PK.aggregate.data,
     chem.invivo.PK.summary.data,
     physiology.data,
     tissue.data,
     Tables.Rdata.stamp,
     file="Tables.RData",compress="gzip",version=2)

sysdata.rda.stamp <- paste("This sysdata.rdata file was created on",Sys.Date(),"by script version",SCRIPT.VERSION)
save(Wetmore.data,
     sipes2017,
     sipes2017.table,
     chem.lists,
     sysdata.rda.stamp,                 
     file="sysdata.rda",compress="gzip",version=2)
     
     
