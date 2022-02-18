# source("L:/Lab/NCCT_ExpoCast/ExpoCast2019/HTTKDataTable/load_package_data_tables-030719.R")
# Get rid of anything in the workspace:
rm(list=ls()) 

SCRIPT.VERSION <- "Dec2021"

## R Packages ##
library(reshape)
library(readxl)
library(dplyr)
library(magrittr)
library(stringr)

# Useful function from HTTK (don't want to load the whole package since we're 
# trying to build the package here)
source("add_chemtable.R")

set.precision <- function(x, sig.figs=4)
{
  x <- as.data.frame(x)
  for (this.col in 1:(dim(x)[2]))
  {
    nona.x <- as.numeric(x[!is.na(as.numeric(x[,this.col])),this.col])
    if (length(nona.x)>0)
      if (any(is.numeric(nona.x)))
      {
        x[,this.col] <- signif(as.numeric(x[,this.col]),sig.figs)  
      }
  }
  return(x)
}

check_duplicates <- function(
  chem.table, 
  check.cols = c("DTXSID","Compound","CAS"), 
  remove.duplicates=TRUE)
{
  for (this.col in check.cols)
  {
    if (!(length(unique(tolower(chem.table[,this.col]))) == 
      dim(chem.table)[1])) print(paste(this.col,"values are not unique."))
  
    if (dim(subset(chem.table, duplicated(tolower(chem.table[,this.col]))))[1]>0) 
    {
      cat(paste("There are instances of chemicals with same",
        this.col,
        "but differing in other properties.\n"))
      dup.chems <- subset(chem.table,
        duplicated(tolower(chem.table[,this.col])))[,this.col]
      print(subset(chem.table, chem.table[,this.col]%in%dup.chems))
      
      browser()  
# merge records:
      for (this.chem in dup.chems)
      {
        dup.rows <- which(chem.table[,this.col]==this.chem)
        dup.1 <- dup.rows[1]
        dup.2 <- dup.rows[2]
        for (this.col2 in 1:dim(chem.table)[2])
        {
          if (is.na(chem.table[dup.1,this.col2]))
          {
            if (!is.na(chem.table[dup.2,this.col2]))
              chem.table[dup.1,this.col2] <- chem.table[dup.2,this.col2]
          }
        }
      }
    # Get rid of duplicates (shouldn't be any!)
      if (remove.duplicates) chem.table <- subset(chem.table,
        !duplicated(tolower(chem.table[,this.col])))
    } 
  }
  
  return(chem.table)
}

#
# CREATE TABLES physiology.data and tissue.data
#
PKandTISSUEDATAFILE <- "pkdata.xlsx"

physiology.data <- set.precision(read_excel(PKandTISSUEDATAFILE,
  sheet="Basic PK"))[1:16,]
# Write to text so Git can track changes:
write.table(physiology.data,
  file="Basic-Physiology.txt",
  row.names=F,
  sep="\t")
  
flowdata <- set.precision(read_excel(PKandTISSUEDATAFILE,
  sheet="Flows",
  skip=1))[1:19,]
write.table(flowdata,
  file="Tissue-Flows.txt",
  row.names=F,
  sep="\t")

densitydata <- set.precision(read_excel(PKandTISSUEDATAFILE,
  sheet="Human Density"))[1:29,1:3]
write.table(densitydata,
  file="Tissue-Density.txt",
  row.names=F,
  sep="\t")
  
percentBWdata <- set.precision(read_excel(PKandTISSUEDATAFILE,
  sheet="Percent BW",
  skip=2)[1:20,])
write.table(percentBWdata,
  file="Tissue-PercentBW.txt",
  row.names=F,
  sep="\t")
  
tissuevolflowdata <- set.precision(read_excel(PKandTISSUEDATAFILE,
  sheet="VolumeFlow"))
tissuevolflowdata <- subset(tissuevolflowdata,Species!="")[,c(
  "Tissue",
  "Species",
  "Volume (L/kg)",
  "Reference...4",
  "Blood Flow (ml/min/kg^(3/4))",
  "Reference...6")]
colnames(tissuevolflowdata) <- c(
  "Tissue",
  "Species",
  "Vol (L/kg)",
  "Vol Reference",
  "Flow (mL/min/kg^(3/4))",
  "Flow Reference")
for (this.col in c("Vol (L/kg)","Flow (mL/min/kg^(3/4))")) 
  tissuevolflowdata[,this.col] <- as.numeric(tissuevolflowdata[,this.col])
# Write to text so Git can track changes:
write.table(tissuevolflowdata,
  file="Tissue-Volumes-Flows.txt",
  row.names=F,
  sep="\t")
  
tissuecompdata <- set.precision(read_excel(PKandTISSUEDATAFILE,
  sheet="TissueComp",
  skip=1))
tissuecompdata <- subset(tissuecompdata,Species!="")
tissuecompdata <- subset(tissuecompdata,!is.na(Cells))
colnames(tissuecompdata) <- c(
  "Tissue",
  "Species",
  "Reference",
  "Fcell",
  "Fint",
  "FWc",
  "FLc",
  "FPc",
  "Fn_Lc",
  "Fn_PLc"
  ,"Fa_PLc"
  ,"pH")
for (this.col in c(
  "Fcell",
  "Fint",
  "FWc",
  "FLc",
  "FPc",
  "Fn_Lc",
  "Fn_PLc",
  "Fa_PLc",
  "pH")) 
  tissuecompdata[,this.col] <- signif(as.numeric(tissuecompdata[,this.col]),4)
# Add tissue densities:
tissuecompdata <- merge(tissuecompdata,densitydata[,1:2],by="Tissue",all.x=TRUE)
# Write to text so Git can track changes:
write.table(tissuecompdata,
  file="Tissue-Composition.txt",
  row.names=F,
  sep="\t")
  
tissuedata1 <- melt(tissuecompdata,id.vars=c("Tissue","Species","Reference"))
tissuedata2 <- melt(tissuevolflowdata[,c("Tissue","Species","Vol (L/kg)","Vol Reference")],id.vars=c("Tissue","Species","Vol Reference"))  
colnames(tissuedata2)[3] <- "Reference"
tissuedata3 <- melt(tissuevolflowdata[,c("Tissue","Species","Flow (mL/min/kg^(3/4))","Flow Reference")],id.vars=c("Tissue","Species","Flow Reference"))  
colnames(tissuedata3)[3] <- "Reference"
tissue.data <- rbind(tissuedata1,tissuedata2,tissuedata3)      
tissue.data$Tissue <- tolower(tissue.data$Tissue)
for (this.tissue in tolower(unique(densitydata$Tissue)))
  if (this.tissue %in% tissue.data$Tissue)
    tissue.data[tissue.data$variable=="Density (g/cm^3)" &
      tissue.data$Tissue==this.tissue,"Reference"] <-
      densitydata[tolower(densitydata$Tissue)==this.tissue,"Reference"]
      
write.table(tissue.data,
  file="Tissue-data.txt",
  row.names=F,
  sep="\t")

#
# END TABLES physiology.data and tissue.data
#




#
# CREATE TABLE chem.physical_and_invitro.data 
#

#Table CLint units are uL/min/10^6 cells:
Wetmore.tables <- as.data.frame(read_excel("PublishedRawDataTables.xlsx",skip=2))
colnames(Wetmore.tables) <- c("Compound","CAS","Reference","Species","Clint.1","pValue.1","Clint.10","pValue.10","Fub.1","Fub.10")
Wetmore.tables[Wetmore.tables[,"pValue.1"]=="< 0.0001","pValue.1"] <- "0.00009" 
Wetmore.tables[is.na(Wetmore.tables[,"pValue.10"]),"pValue.10"] <- 1
Wetmore.tables[Wetmore.tables[,"pValue.10"]=="< 0.0001","pValue.10"] <- "0.00009"
Wetmore.tables[,"pValue.10"] <- as.numeric(Wetmore.tables[,"pValue.10"])
Wetmore.tables[,"pValue.1"] <- as.numeric(Wetmore.tables[,"pValue.1"])
#Wetmore 2012 CAS for pyrithobac and not the pyrithobac salt used in ToxCast:
Wetmore.tables[Wetmore.tables$CAS=="123342-93-8","CAS"] <- "123343-16-8"
Wetmore.tables[is.na(Wetmore.tables[,"Fub.10"]),"Fub.10"] <- 0
Wetmore.tables[Wetmore.tables$Fub.10=="","Fub.10"] <- 0
Wetmore.tables <- set.precision(Wetmore.tables)

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

WetmorePhaseII.fup.table <- read.table("Wetmore2015.fup.table.txt",
  stringsAsFactors=F,
  fill=T,
  header=T,
  sep="\t",
  quote = "",
  comment.char = "")
WetmorePhaseII.fup.table$X10.mM[WetmorePhaseII.fup.table$X10.mM>100] <- 100
WetmorePhaseII.fup.table$X10.mM <- as.numeric(WetmorePhaseII.fup.table$X10.mM)/100
#There were a couple of salts in this data set, let's replicate them as acids:
salt <- subset(WetmorePhaseII.fup.table,CAS=="2795-39-3")
salt$CAS <- "1763-23-1"
salt$Name <- "Perfluorooctanesulfonic acid"
WetmorePhaseII.fup.table <- rbind(WetmorePhaseII.fup.table,salt)
salt <- subset(WetmorePhaseII.fup.table,CAS=="29420-49-3")
salt$CAS <- "375-73-5"
salt$Name <- "Perfluorobutanesulfonic acid"
WetmorePhaseII.fup.table <- rbind(WetmorePhaseII.fup.table,salt)
salt <- subset(WetmorePhaseII.fup.table,CAS=="2795-39-3")
salt$CAS <- "3825-26-1"
salt$Name <- "Perfluorooctanoic acid"
WetmorePhaseII.fup.table <- rbind(WetmorePhaseII.fup.table,salt)
salt <- subset(WetmorePhaseII.fup.table,CAS=="3871-99-6")
salt$CAS <- "355-46-4"
salt$Name <- "Perfluorohexanesulfonic acid"
WetmorePhaseII.fup.table <- rbind(WetmorePhaseII.fup.table,salt)
chem.prop <- add_chemtable(WetmorePhaseII.fup.table,
               species="Human",
               reference="Wetmore 2015",
               current.table=chem.prop,
               data.list=list(
                 CAS="CAS",
                 Compound="Name",
                 Funbound.plasma="X10.mM"),overwrite=T)
                 
WetmorePhaseII.clint.table <- read.table("Wetmore2015.clint.table.txt",
  stringsAsFactors=F,
  fill=T,
  header=T,
  sep="\t",
  quote = "",
  comment.char = "")
WetmorePhaseII.clint.table <- subset(WetmorePhaseII.clint.table,Conc.=="1 uM")
WetmorePhaseII.clint.table[,"P.value"] <-as.numeric(gsub("< ","",WetmorePhaseII.clint.table[,"P.value"]))
WetmorePhaseII.clint.table[,"Adj.Cl"] <-as.numeric(WetmorePhaseII.clint.table[,"Adj.Cl"])
#There were a couple of salts in this data set, let's replicate them as acids:
salt <- subset(WetmorePhaseII.clint.table,CAS=="2795-39-3")
salt$CAS <- "1763-23-1"
salt$Name <- "Perfluorooctanesulfonic acid"
WetmorePhaseII.clint.table <- rbind(WetmorePhaseII.clint.table,salt)
salt <- subset(WetmorePhaseII.clint.table,CAS=="29420-49-3")
salt$CAS <- "375-73-5"
salt$Name <- "Perfluorobutanesulfonic acid"
WetmorePhaseII.clint.table <- rbind(WetmorePhaseII.clint.table,salt)
salt <- subset(WetmorePhaseII.clint.table,CAS=="2795-39-3")
salt$CAS <- "3825-26-1"
salt$Name <- "Perfluorooctanoic acid"
WetmorePhaseII.clint.table <- rbind(WetmorePhaseII.clint.table,salt)
salt <- subset(WetmorePhaseII.clint.table,CAS=="3871-99-6")
salt$CAS <- "355-46-4"
salt$Name <- "Perfluorohexanesulfonic acid"
WetmorePhaseII.clint.table <- rbind(WetmorePhaseII.clint.table,salt)
chem.prop <- add_chemtable(WetmorePhaseII.clint.table,                                        
               species="Human",
               reference="Wetmore 2015",
               current.table=chem.prop,
               data.list=list(
                 CAS="CAS",
                 Compound="Name",
                 Clint="Adj.Cl",
                 Clint.pvalue="P.value"))                 




Obach.table <- set.precision(read_excel("Obach1999.xlsx",skip=1))[1:29,]
#Wrong CAS for Diazepam:
Obach.table[Obach.table$CAS=="53320-84-6","CAS"] <- "439-14-5"

chem.prop <- add_chemtable(Obach.table, 
               current.table=chem.prop, 
               species="Human",
               reference="Obach 1999",
               data.list=list(
                 CAS="CAS",
                 Compound="Compound",
                 Rblood2plasma="Blood-to-Plasma Ratio",
                 Funbound.plasma="Fraction Unbound in Plasma"))

chem.prop[chem.prop$Compound=="Bensulide",]
sum(chem.prop$Compound=="dibutyl benzene-1,2-dicarboxylate")

Jones.table <- set.precision(read_excel("Jones2002.xlsx"))
chem.prop <- add_chemtable(Jones.table, 
               current.table=chem.prop,
               species="Human",
               reference="Jones 2002",
               data.list=list(
                 CAS="CAS",
                 Compound="Compouund",
                 pKa_Donor="pKa"))

chem.prop[chem.prop$Compound=="Bensulide",]
sum(chem.prop$Compound=="dibutyl benzene-1,2-dicarboxylate")

# Table Clint units for pooled hepatocytes (lot 70 and 73) are ml/min/10^9 cells
# conversion factor is x 1000 ml -> ul and x 1/1000 10^9 to 10^6 cells, so no conversion necessary
Shibata2002.table <- set.precision(read_excel("Shibata2002.xlsx"))[-1,]
chem.prop <- add_chemtable(Shibata2002.table, 
               current.table=chem.prop,
               species="Human",
               reference="Shibata 2002",
               data.list=list(
                 CAS="CASRN",
                 Compound="compound",
                 Clint="CLint, in vitro, 70+73",
                 Funbound.plasma="fu",
                 Rblood2plasma="RB"))


chem.prop[chem.prop$Compound=="Bensulide",]
sum(chem.prop$Compound=="dibutyl benzene-1,2-dicarboxylate")


#Table CLint units are uL/min/10^6 cells:
Lau2002.table <- set.precision(read_excel("Lau2002.xlsx"))[-1,]
chem.prop <- add_chemtable(Lau2002.table, 
               current.table=chem.prop, 
               species="Human", 
               reference="Lau2002",
               data.list = list(CAS="CAS",
                 Compound="Compound",
                 Clint="In Vitro Clearance"))

chem.prop[chem.prop$Compound=="Bensulide",]
sum(chem.prop$Compound=="dibutyl benzene-1,2-dicarboxylate")

#Table CLint units are uL/min/10^6 cells:
Naritomi.table <- set.precision(read_excel("Naritomi2003.xlsx",skip=1)[1:18,])
#Wrong CAS for Diazepam:
Naritomi.table[Naritomi.table$CAS=="53320-84-6","CAS"] <- "439-14-5"
chem.prop <- add_chemtable(Naritomi.table, 
               current.table=chem.prop, 
               reference="Naritomi 2003",
               data.list = list(CAS="CAS",
                 Compound="Compound",
                 Species="Species",
                 Clint="Clint,invitro (ul/min/10^6 cells)",
                 Funbound.plasma="fp",
                 Rblood2plasma="Rb",
                 Fgutabs="Fa"))

chem.prop[chem.prop$Compound=="Bensulide",]
sum(chem.prop$Compound=="dibutyl benzene-1,2-dicarboxylate")

#Table CLint units are uL/min/10^6 cells:
McGinnity.table <- as.data.frame(read_excel("McGinnity2004.xlsx"))[-1,]
McGinnity.table[McGinnity.table[,"Human Hepatic Clint"]=="<1.0","Human Hepatic Clint"]<-0
McGinnity.table <- set.precision(McGinnity.table)
chem.prop <- add_chemtable(McGinnity.table,
               current.table=chem.prop,
               reference="McGinnity 2004", species="Human",
               data.list=list(CAS="CASRN",
                 Compound="Compound",
                 Clint="Human Hepatic Clint"))

chem.prop[chem.prop$Compound=="Bensulide",]
sum(chem.prop$Compound=="dibutyl benzene-1,2-dicarboxylate")

#Table CLint units are uL/min/10^6 cells:
Ito.table <- as.data.frame(read_excel("Ito2004.xlsx"))
# Let's interpret "not determined" as no measurment instead of 0
Ito.table[Ito.table[,"Clint (hepatocyte)"]=="ND","Clint (hepatocyte)"]<-NA
Ito.table <- set.precision(Ito.table)
chem.prop <- add_chemtable(Ito.table,
               current.table=chem.prop,
               reference="Ito 2004", species="Human",
               data.list=list(CAS="CAS",
                 Compound="Compound",
                 Clint="Clint (hepatocyte)",
                 Funbound.plasma="fub"))

chem.prop[chem.prop$Compound=="Bensulide",]
sum(chem.prop$Compound=="dibutyl benzene-1,2-dicarboxylate")

Schmitt.table <- set.precision(read_excel("Schmitt2008-41817.xlsx"))
for (this.row in 1:dim(Schmitt.table)[1])
{
  this.CAS <- Schmitt.table[this.row,"CAS"]
  this.compound <- Schmitt.table[this.row,"Compound"]
  this.reference <- "Schmitt 2008"
  chem.prop <- augment.table(chem.prop,this.CAS,this.compound,"logP",Schmitt.table[this.row,"logP"],reference=this.reference)
  if (Schmitt.table[this.row,"Compound Type"]=="A")
  {
    chem.prop <- augment.table(chem.prop,this.CAS,this.compound,"pKa_Donor",as.numeric(Schmitt.table[this.row,"pKa"]),reference=this.reference)
    chem.prop <- augment.table(chem.prop,this.CAS,this.compound,"pKa_Accept",NA,reference=this.reference)
  }
  else if (Schmitt.table[this.row,"Compound Type"]=="B")
  {
    chem.prop <- augment.table(chem.prop,this.CAS,this.compound,"pKa_Donor",NA,reference=this.reference)
    chem.prop <- augment.table(chem.prop,this.CAS,this.compound,"pKa_Accept",as.numeric(Schmitt.table[this.row,"pKa"]),reference=this.reference)
  }
  else if (Schmitt.table[this.row,"Compound Type"]=="N")
  {
    chem.prop <- augment.table(chem.prop,this.CAS,this.compound,"pKa_Donor",NA,reference=this.reference)
    chem.prop <- augment.table(chem.prop,this.CAS,this.compound,"pKa_Accept",NA,reference=this.reference)
  }
  chem.prop <- augment.table(chem.prop,this.CAS,this.compound,"Funbound.plasma",Schmitt.table[this.row,"Fub"],species=Schmitt.table[this.row,"Species"],reference=this.reference)
  chem.prop <- augment.table(chem.prop,this.CAS,this.compound,"logMA",Schmitt.table[this.row,"logMA"],reference=this.reference)
}


chem.prop[chem.prop$Compound=="Bensulide",]
sum(chem.prop$Compound=="dibutyl benzene-1,2-dicarboxylate")

Obach2018.table <- as.data.frame(read_excel(
  "Lombardo2018-Supplemental_82966_revised_corrected.xlsx",
  skip=8))
# Correct deleted CAS:
Obach2018.table[Obach2018.table[,"CAS #"]=="85650-52-8", "CAS #"] <- 
  "61337-67-5"
Obach2018.table[Obach2018.table[,"CAS #"]=="118457-14-0", "CAS #"] <- 
  "99200-09-6"
Obach2018.table[Obach2018.table[,"CAS #"]=="126544-47-6", "CAS #"] <- 
  "141845-82-1"
Obach2018.table[Obach2018.table[,"CAS #"]=="4731-52-6", "CAS #"] <- 
  "444731-52-6"
Obach2018.table[Obach2018.table[,"CAS #"]=="66981-73-5", "CAS #"] <- 
  "72797-41-2"

# Get rid of non-numeric fu values:
Obach2018.table$fu <- signif(as.numeric(Obach2018.table[,
  "fraction unbound \r\nin plasma (fu)"]),4)
Obach2018.table <- subset(Obach2018.table,!is.na(fu))
chem.prop <- add_chemtable(Obach2018.table,
               species="Human",
               reference="Lombardo 2018",
               current.table=chem.prop,
               data.list=list(
                 CAS="CAS #",
                 Compound="Name",
                 Funbound.plasma="fu"))


chem.prop[chem.prop$Compound=="Bensulide",]
sum(chem.prop$Compound=="dibutyl benzene-1,2-dicarboxylate")


#Table CLint units are L/h/10^6 hepatocytes
TNO.table <- as.data.frame(read_excel("HT-PBPK compounds-122216.xlsx"))
TNO.table <- subset(TNO.table,TNO.table$Source!="")

for (this.row in 1:dim(TNO.table)[1])
{
  this.CAS <- TNO.table[this.row,"CAS #"]
  this.compound <- TNO.table[this.row,"Compound name"]
  this.reference <- TNO.table[this.row,"Source"]
  if (this.reference != "EPA/Hamner")
  {
    if (!is.na(as.numeric(TNO.table[this.row,"CLint,h"]))) chem.prop <- 
      augment.table(
        chem.prop,
        this.CAS,
        this.compound,
        "Clint",
        TNO.table[this.row,"CLint,h"],
        species="Human",
        reference=this.reference)
    if (!is.na(as.numeric(TNO.table[this.row,"fup"]))) chem.prop <- 
      augment.table(
      chem.prop,
      this.CAS,
      this.compound,
      "Funbound.plasma",
      TNO.table[this.row,"fup"],
      species="Human",
      reference=this.reference)
  }
  this.reference <- "TNO"
  chem.prop <- augment.table(
    chem.prop,
    this.CAS,
    this.compound,
    "logP",
    log10(TNO.table[this.row,"logP"]),
    reference=this.reference)
  chem.prop <- augment.table(
    chem.prop,
    this.CAS,
    this.compound,
    "MW",
    TNO.table[this.row,"MW"],
    reference=this.reference)
  chem.prop <- augment.table(
    chem.prop,
    this.CAS,
    this.compound,
    "Rblood2plasma",
    TNO.table[this.row,"Rbp"],
    species="Human",
    reference=this.reference)
  if (!is.na(TNO.table[this.row,"ion"]))
  {
    if (TNO.table[this.row,"ion"] == "mpa")
    {
      chem.prop <- augment.table(
        chem.prop,
        this.CAS,
        this.compound,
        "pKa_Donor",
        as.numeric(TNO.table[this.row,"pKa1"]),
        reference=this.reference)
      chem.prop <- augment.table(
        chem.prop,
        this.CAS,
        this.compound,
        "pKa_Accept",
        NA,
        reference=this.reference)
    }
    else if (TNO.table[this.row,"ion"] == "dpa")
    {
      chem.prop <- augment.table(
        chem.prop,
        this.CAS,
        this.compound,
        "pKa_Donor",
        paste(as.numeric(TNO.table[this.row,"pKa1"]),
        as.numeric(TNO.table[this.row,"pKa2"]),sep=","),
        reference=this.reference)
      chem.prop <- augment.table(
        chem.prop,
        this.CAS,
        this.compound,
        "pKa_Accept",
        NA,
        reference=this.reference)
    }
    else if (TNO.table[this.row,"ion"] == "mpb")
    {
      chem.prop <- augment.table(
        chem.prop,
        this.CAS,
        this.compound,
        "pKa_Accept",
        as.numeric(TNO.table[this.row,"pKa1"]),
        reference=this.reference)
      chem.prop <- augment.table(
        chem.prop,
        this.CAS,
        this.compound,
        "pKa_Donor",
        NA,
        reference=this.reference)
    }
    else if (TNO.table[this.row,"ion"] == "dpb")
    {
      chem.prop <- augment.table(
        chem.prop,
        this.CAS,
        this.compound,
        "pKa_Accept",
        paste(as.numeric(TNO.table[this.row,"pKa1"]),
        as.numeric(TNO.table[this.row,"pKa2"]),sep=","),
        reference=this.reference)
      chem.prop <- augment.table(
        chem.prop,
        this.CAS,
        this.compound,"pKa_Donor",
        NA,
        reference=this.reference)
    }
    else if (TNO.table[this.row,"ion"] == "zwi")
    {
      if (regexpr("acid",TNO.table[this.row,"pKa1"])!=-1)
      {
        chem.prop <- augment.table(
          chem.prop,
          this.CAS,
          this.compound,
          "pKa_Donor",
          as.numeric(strsplit(TNO.table[this.row,"pKa1"]," ")[[1]][1]),
          reference=this.reference)
        chem.prop <- augment.table(
          chem.prop,
          this.CAS,
          this.compound,
          "pKa_Accept",
          as.numeric(strsplit(TNO.table[this.row,"pKa2"]," ")[[1]][1]),
          reference=this.reference)
      } else {
        chem.prop <- augment.table(
          chem.prop,
          this.CAS,
          this.compound,
          "pKa_Accept",
          as.numeric(strsplit(TNO.table[this.row,"pKa1"]," ")[[1]][1]),
          reference=this.reference)
        chem.prop <- augment.table(
          chem.prop,
          this.CAS,
          this.compound,
          "pKa_Donor",
          as.numeric(strsplit(TNO.table[this.row,"pKa2"]," ")[[1]][1]),
          reference=this.reference)
      }
    } else {
      chem.prop <- augment.table(
        chem.prop,
        this.CAS,
        this.compound,
        "pKa_Donor",
        NA,
        reference=this.reference)
      chem.prop <- augment.table(
        chem.prop,
        this.CAS,
        this.compound,
        "pKa_Accept",
        NA,
        reference=this.reference)
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
#Naritomi.table2 <- read_excel("Naritomi2003.xlsx",,sheet=2)[-1,]
## Original CLint units are ml/min/kg BW
## Page 1306: 120 * 10^6 cells/g liver
## Page 1306: 1500-1800 g liver/70 kg -- 23.6 g liver/kg BW
## Conversion factor: *1000/23.6/120/10^6             (ml->uL,1/kgBW->1/g liver,1/g liver->1/10^6 cells)->uL/min/10^6 cells)
#Riley.table <- read_excel("Riley2005.xlsx",sheet=1)
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
Tonnelier.table <- as.data.frame(read_excel("Tonnelier-2012.xlsx"))
# Dashboard prefers different CAS:
Tonnelier.table[Tonnelier.table$Name=="Abamectin","CAS"] <- "71751-41-2"
Tonnelier.table[Tonnelier.table$CAS=="51630-58-1","CAS"] <- "67614-33-9"
Tonnelier.table[Tonnelier.table$CAS=="671200","CAS"] <- "3737-09-5"
Tonnelier.table$CAS <- sapply(Tonnelier.table$CAS,function(x) substr(x,regexpr("[^0]",x),nchar(x)))
Tonnelier.table <- set.precision(Tonnelier.table)
chem.prop <- add_chemtable(Tonnelier.table,
               species="Human",
               reference="Tonnelier 2012",
               current.table=chem.prop,
               data.list=list(
                 CAS="CAS",
                 Compound="Name",
                 Clint="CLint (HEP) (**)",
                 Funbound.plasma="Fu",
                 Logp="log KOW",
                 MW="MW (g/mol)"))



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


Paixao2012.table2 <- set.precision(read_excel("Paixao-2012.xlsx",sheet=1))
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
Paixao2012.table3 <- set.precision(read_excel("Paixao-2012.xlsx",sheet=2,skip=1))
Paixao2012.table3$In.Vitro.Clint..uL.min.106hep <- 
  Paixao2012.table3[,"In vitro Clint (L/h)"]/1000/70/20/107*60
chem.prop <- add_chemtable(Paixao2012.table3,                             
               species="Human",
               reference="Paixao 2012",
               current.table=chem.prop,
               data.list=list(
                 CAS="...1",
                 Compound="...2",
                 Clint="In.Vitro.Clint..uL.min.106hep"))
                 
Paixao2012.table4 <- set.precision(read_excel("Paixao-2012.xlsx",sheet=3,skip=1))
# Table clint are units of L/h
# 107 x 106 cell g-1 liver (Wilson et al., 2003) and it was also assumed that liver weighed
# 20 g kg-1 of body weight.                 
Paixao2012.table4$In.Vitro.Clint..uL.min.106hep <- 
  Paixao2012.table4[,"In vitro Clint (L h-1)"]/1000/70/20/107*60
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




CorypKaTable <- as.data.frame(read_excel("HTPBPK-chems-pKa_CLS.xlsx"))
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

MA.data <- set.precision(read_excel("Endo-2011.xlsx"))
this.reference <- "Endo 2011"
for (this.row in 1:dim(MA.data)[1])
{
  this.CAS <- MA.data[this.row,"CAS-RN"]
  if (this.CAS %in% chem.prop$CAS)
  {
    this.compound <- MA.data[this.row,"Compound"]
    chem.prop <- augment.table(chem.prop,
      this.CAS,this.compound,
      "logMA",
      MA.data[this.row,"Klipw"],reference=this.reference)
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

if (unique(chem.physical_and_invitro.data$CAS) < 
  dim(chem.physical_and_invitro.data)[1]) 
  stop("Duplicated CAS numbers in chem.physical_and_invitro.data")
if (any(sapply(chem.physical_and_invitro.data$CAS,
  function(x) !CAS.checksum(x)))) 
  stop("Failed CAS checksum in chem.physical_and_invitro.data")


cl <- set.precision(read_excel('Pirovano-2016.xlsx'))
chem.physical_and_invitro.data <- add_chemtable(cl,
  current.table=chem.physical_and_invitro.data,
  data.list=list(Compound='Compound',CAS='CAS',Clint='clint'),
  species='Human',reference='Pirovano 2016',overwrite=F)
caf.cl <- subset(cl,CAS =='58-08-2')
chem.physical_and_invitro.data <- add_chemtable(caf.cl,
  current.table=chem.physical_and_invitro.data,
  data.list=list(Compound='Compound',CAS='CAS',Clint='clint'),
  species='Human',
  reference='Pirovano 2016',overwrite=T)
rb <- set.precision(read_excel('Uchimura 2010 cas.xlsx'))
chem.physical_and_invitro.data <- add_chemtable(rb,
  current.table=chem.physical_and_invitro.data,
  data.list=list(Compound='Name',Rblood2plasma='Human.Rb2p',
  Funbound.plasma='Human.fup',CAS='cas'),species='Human',
  reference='Uchimura 2010',overwrite=F)
rb <- subset(rb,!is.na(Rat.Rb2p))
  chem.physical_and_invitro.data <- add_chemtable(rb,
  current.table=chem.physical_and_invitro.data,
  data.list=list(
    Compound='Name',
    Rblood2plasma='Rat.Rb2p',
    Funbound.plasma='Rat.fup',
    CAS='cas'),
  species='Rat',reference='Uchimura 2010',overwrite=F)
fub <- set.precision(read_excel('Gulden 2002.xlsx')) 
chem.physical_and_invitro.data <- add_chemtable(fub,
  current.table=chem.physical_and_invitro.data,
  data.list=list(Compound='Compound',Funbound.plasma='fup',CAS='CAS',MW='MW'),
  species='Human',reference='Gulden 2002',overwrite=F)
brown <- set.precision(read_excel('Brown 2007.xlsx'))
chem.physical_and_invitro.data <- add_chemtable(brown,
  current.table=chem.physical_and_invitro.data,
  data.list=list(Compound='Compound',Clint='Clint',CAS='CAS'),
  species='Human',reference='Brown 2007',overwrite=T)

#Add Clint data
jones <- set.precision(read_excel('Jones 2017 human in vitro clearance.xlsx',1))
jones <- jones[3:12,1:3]
colnames(jones) <- c('CAS','Compound','Clint')
wood.rat <- set.precision(read_excel('Wood_2017_Rat_Clint_CLh.xlsx',1))
wood.rat <- subset(wood.rat,!is.na("Clint (uL/min/10^6 cells)") & 
  Name != 'FK079') # No CAS for FK079
wood.human <- set.precision(read_excel('Wood 2017 Human Clint and CLh.xlsx',1))
wood.human <- subset(wood.human,!is.na("Clint (uL/min/10^6 cells)"))

sternbeck <- set.precision(read_excel('Sternbeck Human Clearance.xlsx',1))
chem.physical_and_invitro.data <- add_chemtable(jones,
  current.table=chem.physical_and_invitro.data,
  data.list=list(Compound='Compound',CAS='CAS',Clint='Clint'),
  species='Human',reference='Jones 2017',overwrite=T)
chem.physical_and_invitro.data <- add_chemtable(wood.human,
  current.table=chem.physical_and_invitro.data,
  data.list=list(Compound='Name',CAS='CAS',Clint="Clint (uL/min/10^6 cells)"),
  species='Human',reference='Wood 2017',overwrite=T)
chem.physical_and_invitro.data <- add_chemtable(wood.rat,
  current.table=chem.physical_and_invitro.data,
  data.list=list(Compound='Name',CAS='CASRN',Clint="Clint (uL/min/10^6 cells)"),
  species='Rat',reference='Wood 2017',overwrite=T)
# Add new fup from Wood 2017
chem.physical_and_invitro.data <- add_chemtable(wood.human,
  current.table=chem.physical_and_invitro.data,
  data.list=list(Compound='Name',CAS='CAS',Funbound.plasma='fup'),
  species='Human',reference='Wood 2017',overwrite=F)
chem.physical_and_invitro.data <- add_chemtable(wood.rat,
  current.table=chem.physical_and_invitro.data,
  data.list=list(Compound='Name',CAS='CASRN',Funbound.plasma='fup'),
  species='Rat',reference='Wood 2017',overwrite=F)
#Add only 2 compounds with clint and fup, overwrite all Rb2p but paixao
chem.physical_and_invitro.data <- add_chemtable(subset(sternbeck,
  Name %in% c('Etodolac','Bufuralol')),
  current.table=chem.physical_and_invitro.data,
  data.list=list(Compound='Name',CAS='CAS',Funbound.plasma='fup',Clint='Clint'),
  species='Human',reference='Sternbeck 2012',overwrite=T)
chem.physical_and_invitro.data <- add_chemtable(subset(sternbeck,
  !CAS %in% subset(chem.physical_and_invitro.data,
    Human.Rblood2plasma.Reference == 'Paixao 2012')[,'CAS']),
  current.table=chem.physical_and_invitro.data,
  data.list=list(Compound='Name',CAS='CAS',Rblood2plasma='Rb'),
  species='Human',reference='Sternbeck 2012',overwrite=T)


#Remove overwritten clint pvalues
for (this.cas in subset(chem.physical_and_invitro.data,
  Human.Clint.pValue.Reference != Human.Clint.Reference)[,'CAS'])
{
  chem.physical_and_invitro.data[
    which(chem.physical_and_invitro.data[,'CAS'] == this.cas),
    'Human.Clint.pValue'] <- chem.physical_and_invitro.data[
    which(chem.physical_and_invitro.data[,'CAS'] == this.cas),
    'Human.Clint.pValue.Reference'] <- NA
}

# Load partition coefficient data from Pearce 2017:
pc.data.raw <- set.precision(read_excel('PC_Data.xlsx'))
pc.data.table <- subset(pc.data.raw,tolower(Species)=='rat')
pc.data.table[which(pc.data.table[,'CAS'] %in% c(
  '10457-90-6','5786-21-0','17617-23-1','69-23-8','2898-12-6','57562-99-9',
  '59-99-4','2955-38-6','155-97-5','41903-57-5','58-55-9','77-32-7','59-05-2',
  '60-54-8')),
  'fu'] <- NA
#pc.data <- subset(pc.data, Tissue %in% c("Adipose","Bone","Brain","Gut","Heart","Kidney","Liver","Lung","Muscle","Skin","Spleen","Blood Cells") & Species == 'Rat')   
chem.physical_and_invitro.data <- add_chemtable(
  pc.data.table,
  current.table=chem.physical_and_invitro.data,
  data.list=list(
    Compound='Drug',
   CAS='CAS',
   Funbound.plasma='fu',
   Species='Species',
   logP='LogP',
   logMA='logMA',
   pKa_Donor='Donor',
   pKa_Accept='Accept'),
  reference='Pearce 2017',
  overwrite=T)

pc.data <- pc.data.raw[,c('CAS','Drug','Tissue','Species','fu','A/B/N','LogP','Exp_PC')]
write.csv(pc.data,"Pearce2017-PC-data.txt",row.names=FALSE)

#
# Manual fixes to data:
#
chem.physical_and_invitro.data[
  which(chem.physical_and_invitro.data[,'CAS'] == '37517-30-9'),
  'All.Compound.Names'] <- 'Acebutolol'
chem.physical_and_invitro.data[
  which(chem.physical_and_invitro.data[,'CAS'] == '28434-00-6'),
  'Compound'] <- chem.physical_and_invitro.data[
  which(chem.physical_and_invitro.data[,'CAS'] == '28434-00-6'),
  'All.Compound.Names'] <- 's-bioallethrin'
chem.physical_and_invitro.data[
  which(chem.physical_and_invitro.data[,'CAS'] == '119-90-4'),
  'Compound'] <- chem.physical_and_invitro.data[
  which(chem.physical_and_invitro.data[,'CAS'] == '119-90-4'),
  'All.Compound.Names'] <-  "3,3'-dimethoxybenzidine"
chem.physical_and_invitro.data[
  which(chem.physical_and_invitro.data[,'CAS'] == '1951-25-3'),
  'Compound'] <- chem.physical_and_invitro.data[
  which(chem.physical_and_invitro.data[,'CAS'] == '1951-25-3'),
  'All.Compound.Names'] <-  "Amiodarone"
chem.physical_and_invitro.data[
  which(chem.physical_and_invitro.data[,'CAS'] == '357-70-0'),
  'Compound'] <- chem.physical_and_invitro.data[
  which(chem.physical_and_invitro.data[,'CAS'] == '357-70-0'),
  'All.Compound.Names']  <-  "Galantamine"
chem.physical_and_invitro.data[
  which(chem.physical_and_invitro.data[,'CAS'] == '57-41-0'),
  'Compound'] <-  "Phenytoin"
chem.physical_and_invitro.data[
  which(chem.physical_and_invitro.data[,'CAS'] == '330-54-1'),
  'Compound'] <- chem.physical_and_invitro.data[
  which(chem.physical_and_invitro.data[,'CAS'] == '330-54-1'),
  'All.Compound.Names']  <-  "Diuron"
chem.physical_and_invitro.data[
  which(chem.physical_and_invitro.data[,'CAS'] == '91524-16-2'),
  'Compound'] <- chem.physical_and_invitro.data[
  which(chem.physical_and_invitro.data[,'CAS'] == '91524-16-2'),
  'All.Compound.Names']  <-  "Timolol hemihydrate"
chem.physical_and_invitro.data[
  which(chem.physical_and_invitro.data[,'CAS'] == '101193-40-2'),
  'All.Compound.Names'] <- 'Quinotolast'
chem.physical_and_invitro.data[
  which(chem.physical_and_invitro.data[,'CAS'] == '77-28-1'),
  'All.Compound.Names'] <- 'Butylbarbitone|Butethal [nf]  |Butethal'
chem.physical_and_invitro.data[
  which(chem.physical_and_invitro.data[,'CAS'] == '65216-93-5'),
  'All.Compound.Names'] <- 'Ethoxycoumarin|3-ethoxychromen-2-one'
chem.physical_and_invitro.data[
  which(chem.physical_and_invitro.data[,'CAS'] == '509-86-4'),
  'All.Compound.Names'] <- 'Heptabarbitone|Heptabarbital'
chem.physical_and_invitro.data[
  which(chem.physical_and_invitro.data[,'CAS'] == '58-08-2'),
  'Compound'] <- 'Caffeine'

#Honda 2019:
load("Honda2019/wetmore_fup.RData") #Some where rat fups were inappropriately truncated
wetmore.fup <- set.precision(wetmore.fup)
chem.physical_and_invitro.data <- add_chemtable(
  wetmore.fup,
  current.table=chem.physical_and_invitro.data,
  data.list=list(CAS="casrn",
                 Funbound.plasma="fup"),
  reference="Wetmore 2013",
  species="Rat",
  overwrite=T)
  
# New chemicals:
load("Honda2019/full_new_rat_04Dec2018.RData")
full.new.rat <- set.precision(full.new.rat)
full.new.rat <- subset(full.new.rat, use_fup==1 | use_clint==1)

chemprop.new.rat <- unique(full.new.rat[,c("casrn",
                                           "DSSTox_Substance_Id",
                                           "logP",
                                           "MW",
                                           "pKa_Accept",
                                           "pKa_Donor",
                                           "preferred_name")])
# update phys-chem props
chem.physical_and_invitro.data <- add_chemtable(
  chemprop.new.rat,
  current.table=chem.physical_and_invitro.data,
  data.list=list(
    CAS="casrn",
    DTXSID="DSSTox_Substance_Id",
    LogP="logP",MW="MW",
    pKa_Accept="pKa_Accept",
    pKa_Donor="pKa_Donor",
    Compound = "preferred_name"),
   reference="Honda 2019",
   species="Rat",
   overwrite=T)


# only use the clints that greg identified as good:
chem.physical_and_invitro.data <- add_chemtable(
  subset(full.new.rat, use_clint == 1),
  current.table=chem.physical_and_invitro.data,
  data.list=list(CAS="casrn",
    DTXSID="DSSTox_Substance_Id",
    Clint="clearance",
    Compound = "preferred_name"),
  reference="Honda 2019",
  species="Rat",
  overwrite=T)
  
# only use the fups that greg identified as good:
chem.physical_and_invitro.data <- add_chemtable(
  subset(full.new.rat, use_fup == 1),
  current.table=chem.physical_and_invitro.data,
  data.list=list(CAS="casrn",
    DTXSID="DSSTox_Substance_Id",
    Funbound.plasma="Funbound.plasma",
    Compound = "preferred_name"),
  reference="Honda 2019",
  species="Rat",     
  overwrite=T)

chem.physical_and_invitro.data <- check_duplicates(
  chem.physical_and_invitro.data, check.cols="Compound")

#Load data from Wambaugh 2019:

load("New-HTTK-raw-2019-05-06.RData")

new.httk.data$Human.Clint <- paste(
  signif(new.httk.data$Human.Clint,3),
  signif(new.httk.data$Human.Clint.Low95,3),
  signif(new.httk.data$Human.Clint.High95,3),
  signif(new.httk.data$Human.Clint.pValue,3),sep=",")
new.httk.data$Human.Funbound.plasma <- paste(
  signif(new.httk.data$Human.Funbound.plasma,3),
  signif(new.httk.data$Human.Funbound.plasma.Low95,3),
  signif(new.httk.data$Human.Funbound.plasma.High95,3),
  sep=",")
new.httk.data[new.httk.data$Human.Clint=="NA,NA,NA,NA", "Human.Clint"] <- NA
new.httk.data[new.httk.data$Human.Funbound.plasma=="NA,NA,NA", "Human.Funbound.plasma"] <-NA
                     
# NOCAS_47353 is a salt, its ion is: 476013-14-6
new.httk.data[new.httk.data$CAS=="NOCAS_47353","CAS"] <- "476013-14-6"       

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

chem.physical_and_invitro.data <- check_duplicates(
  chem.physical_and_invitro.data, check.cols="Compound")
#
# Load predictions from Sipes 2017:
#
sipes2017 <- readRDS("ADMET.data.table.RData")

# Replace "bad" CAS:
sipes.bad.cas <- read.csv("SipesBadCAS.csv")
sipes.bad.cas <- subset(sipes.bad.cas,TOX21SL=="Y")

# NOCAS_47353 is a salt, its ion is: 476013-14-6
sipes2017[sipes2017$CAS=="NOCAS_47353","CAS"] <- "476013-14-6"               

for (this.row in 1:dim(sipes.bad.cas)[1])
{
  if (sipes.bad.cas[this.row,"INPUT"] %in% sipes2017$Compound)
  {
    index <- which(sipes2017$Compound==sipes.bad.cas[this.row,"INPUT"])
  } else if (sipes.bad.cas[this.row,"INPUT"] %in% sipes2017$CAS)
  {
    index <- which(sipes2017$CAS==sipes.bad.cas[this.row,"INPUT"])
  } else if (sipes.bad.cas[this.row,"INPUT"] == "DTXSID3047051")
  {
    index <- which(sipes2017$Compound=="Ocimene")
  } else if (sipes.bad.cas[this.row,"INPUT"] == "67614-33-9")
  {
    index <- which(sipes2017$Compound=="Fenvalerate")
  } else if (sipes.bad.cas[this.row,"INPUT"] == "Hydrocodone triphosphate")
  {
    index <- which(sipes2017$CAS=="Fenvalerate")
  } else {
    print(paste("Can't find",sipes.bad.cas[this.row,"INPUT"]))
    browser()
  }
  
  sipes2017[index,"Compound"] <- sipes.bad.cas[this.row,"PREFERRED_NAME"]
  sipes2017[index,"CAS"] <- sipes.bad.cas[this.row,"CASRN"]
}

# Store the chemical physprop, but don't add Fup and Clint yet:
chem.physical_and_invitro.data <- add_chemtable(sipes2017,
                                  current.table=chem.physical_and_invitro.data,
                                  data.list=list(Compound='Compound', 
                                    CAS='CAS',
                         #           DTXSID="DTXSID", 
                                    MW = 'MW', 
                                    logP = 'logP',
                                    pKa_Donor = 'pKa_Donor', 
                                    pKa_Accept = 'pKa_Accept',
                                    SMILES.desalt = 'SMILES.desalt'),
                                  reference = 'Sipes 2017', 
                                  species= 'Human', 
                                  overwrite=F)

chem.physical_and_invitro.data <- check_duplicates(
  chem.physical_and_invitro.data, check.cols="Compound")
#
#
# Data from Paini et al. (2020)
#
#
JRC.data.clint <- set.precision(read_excel(
  "APCRA-JRC_HepatocyteStability+ProteinBinding_77_Summary.xlsx",
  sheet=2))
colnames(JRC.data.clint)[5] <- "Conc"
colnames(JRC.data.clint)[8] <- "Clint"
JRC.data.clint <- subset(JRC.data.clint,!is.na(Clint)) 
JRC.data.clint2 <- NULL
for (this.id in unique(JRC.data.clint$DTXSID))
{
  this.subset <- subset(JRC.data.clint,DTXSID==this.id)
  this.row <- subset(this.subset,Conc==
    min(this.subset$Conc))
  JRC.data.clint2 <- rbind(JRC.data.clint2,this.row)
} 
chem.physical_and_invitro.data <- add_chemtable(JRC.data.clint2,
                                  current.table=chem.physical_and_invitro.data,
                                  data.list = list(
                                    Compound='Compound Name',
                                    CAS = 'CASRN',
                                    DTXSID='DTXSID',
                                    Clint="Clint"),
                                  overwrite=T,
                                  reference = 'Paini 2020',
                                  species="Human")

JRC.data.fup <- set.precision(read_excel(
  "APCRA-JRC_HepatocyteStability+ProteinBinding_77_Summary.xlsx",
  sheet=3))
JRC.data.fup <- subset(JRC.data.fup,!is.na(JRC.data.fup[,"% Unbound in Plasma"]))
JRC.data.fup$fup <- JRC.data.fup[,"% Unbound in Plasma"]/100
JRC.data.fup[,"fup.up"] <- JRC.data.fup$fup + 1.96*JRC.data.fup$SD...7/100
JRC.data.fup[,"fup.low"] <- JRC.data.fup$fup - 1.96*JRC.data.fup$SD...7/100
JRC.data.fup[JRC.data.fup$fup>1,"fup"] <- 1
JRC.data.fup[JRC.data.fup$fup<0,"fup"] <- 0
JRC.data.fup[JRC.data.fup$fup.up>1,"fup.up"] <- 1
JRC.data.fup[JRC.data.fup$fup.low>1,"fup.low"] <- 1
JRC.data.fup[JRC.data.fup$fup.low<0,"fup.low"] <- 0
JRC.data.fup[JRC.data.fup$fup.up<0,"fup.up"] <- 0
JRC.data.fup$fup <- paste(signif(JRC.data.fup$fup,4),
  signif(JRC.data.fup$fup.low,4),
  signif(JRC.data.fup$fup.up,4),
  sep=",")
chem.physical_and_invitro.data <- add_chemtable(JRC.data.fup,
                                  current.table=chem.physical_and_invitro.data,
                                  data.list = list(
                                    Compound='Compound_ID',
                                    CAS = 'CASRN',
                                    DTXSID='DTXSID',
                                    Funbound.plasma='fup'),
                                  overwrite=T,
                                  reference = 'Paini 2020',
                                  species="Human")
                                  
chem.physical_and_invitro.data <- check_duplicates(
  chem.physical_and_invitro.data, check.cols="Compound")
#
#
# Data from Linakis et al. (2020)
# https://doi.org/10.1038/s41370-020-0238-y
#
#
volatile.data.raw <- read.csv('Linakis2020.csv',stringsAsFactors = F)

chem.physical_and_invitro.data <- add_chemtable(volatile.data.raw,
                current.table = chem.physical_and_invitro.data, 
                data.list = list(Compound='PREFERRED_NAME',
                                 CAS = 'CASRN',DTXSID='DTXSID',
                                 LogP="OCTANOL_WATER_PARTITION_LOGP_OPERA_PRED",
                                 LogHenry='LOG_HENRYS_LAW_DIMENSIONLESS',
                                 MW = 'AVERAGE_MASS',SMILES.desalt='QSAR_READY_SMILES',
                                 Species='SPECIES'),overwrite=F,reference='Linakis 2020')

chem.physical_and_invitro.data <- add_chemtable(volatile.data.raw,
                                  current.table=chem.physical_and_invitro.data,
                                  data.list = list(Compound='PREFERRED_NAME',
                                  CAS = 'CASRN',DTXSID='DTXSID',Clint='CALC_CLINT',
                                  Funbound.plasma='CALC_FUP',
                                  Species='SPECIES'),overwrite=F,reference='Linakis 2020')

chem.physical_and_invitro.data <- check_duplicates(
  chem.physical_and_invitro.data, check.cols="Compound")
#
#
# Data from Dawson et al. (2021)
# https://doi.org/10.1021/acs.est.0c06117
#
#
dawson2021.training <- as.data.frame(readxl::read_xlsx(
  path = "S2_Dawson et al. Supporting_Information_Revision_Final_Sharing.xlsx",
  sheet = 3))
dawson2021.training <- subset(dawson2021.training, DTXSID != "-")

# This chemical was assigned a "NOCAS":
dawson2021.training[dawson2021.training$DTXSID=="DTXSID6057875",
  "CAS_CHEMBL_ID"] <- "NOCAS_57875"
  
chem.physical_and_invitro.data <- add_chemtable(dawson2021.training,
                                  current.table=chem.physical_and_invitro.data,
                                  data.list = list(
                                    Compound='Chemical.Name',
                                    CAS = 'CAS_CHEMBL_ID',
                                    DTXSID='DTXSID',
                                    Clint="Hepatic_Clearance"),
                                  overwrite=FALSE,
                                  reference = 'Dawson 2021',
                                  species="Human")

dawson2021.test <- as.data.frame(readxl::read_xlsx(
  path = "S2_Dawson et al. Supporting_Information_Revision_Final_Sharing.xlsx",
  sheet = 4))
dawson2021.test <- subset(dawson2021.test, DTXSID != "-")
chem.physical_and_invitro.data <- add_chemtable(dawson2021.test,
                                  current.table=chem.physical_and_invitro.data,
                                  data.list = list(
                                    Compound='Chemical.Name',
                                    CAS = 'CAS_CHEMBL_ID',
                                    DTXSID='DTXSID',
                                    Clint="Hepatic_Clearance"),
                                  overwrite=FALSE,
                                  reference = 'Dawson 2021',
                                  species="Human")

chem.physical_and_invitro.data <- check_duplicates(
  chem.physical_and_invitro.data, check.cols="Compound")
#
#
#
#
#
#
#
#
#
#
# ADD NEW DATA HERE:
#
#
#
#
#
#
#
#
#
#

#
#
#
#
#
#
#
#
#
#
# STOP ADDING NEW DATA AFTER THIS, SUBSEQUENT CODE IS TO INTERACT WITH DASHBOARD
#
#
#
#
#
#
#
#
#
#

#
# STOP TO GET NEW PHYSCHEM
#

#
#
# Replace bad CAS where we have DTXSID's
#
#
chem.physical_and_invitro.data[
  !sapply(chem.physical_and_invitro.data$CAS,CAS.checksum) &
  regexpr("NOCAS",chem.physical_and_invitro.data$CAS) == -1 &
  !(is.na(chem.physical_and_invitro.data$DTXSID)),"CAS"] <-
  chem.physical_and_invitro.data[
  !sapply(chem.physical_and_invitro.data$CAS,CAS.checksum) &
  regexpr("NOCAS",chem.physical_and_invitro.data$CAS) == -1 &
  !(is.na(chem.physical_and_invitro.data$DTXSID)),"DTXSID"]

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
  cat(paste("Save Dashboard output (tab separated values/tsv)to HTTK-DSSTox-output-",i,".tsv.\n",sep=""))
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
    read.csv(paste("HTTK-DSSTox-output-",i,".tsv",sep="")))
}

# Get rid of the ones that weren't found:
dsstox <- subset(dsstox,DTXSID!="-")
# Don't use DTXSID as CASRN:
dsstox[regexpr("DTXSID",dsstox[,"CASRN"])!=-1,"CASRN"] <- NA
# Calculate log10 Henry's law constnat:
dsstox[,"logHenry"] <- log10(as.numeric(dsstox[,
  "HENRYS_LAW_ATM.M3.MOLE_OPERA_PRED"]))
# Calculate log10 water solubility:
dsstox[,"logWSol"] <- log10(as.numeric(dsstox[,
  "WATER_SOLUBILITY_MOL.L_OPERA_PRED"]))
# Set a reasonable precision for numbers:
dsstox <- set.precision(dsstox)

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
  reference="EPA",
  overwrite=T)

EPA.ref <- paste('CompTox Dashboard',
  file.info(paste("HTTK-DSSTox-output-",i,".tsv",sep=""))$ctime)


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
cat("Save Dashboard output (tsv) to HTTK-NoCASMatch-DSSTox-output.tsv.\n")
cat("Enter \"c\" to continue when ready.\n")
browser()
dsstox <- read.csv("HTTK-NoCASMatch-DSSTox-output.tsv")

# Get rid of the ones that weren't found:
dsstox <- subset(dsstox,DTXSID!="-")
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
  reference="EPA",
  overwrite=T)

# Some chemicals are missing from DSStox OPERA predictions, so run OPERA and 
# add them to MissingPhysChem.csv
new.opera <- read.csv('MissingPhysChem.csv') 
colnames(new.opera)[1] <- "INPUT"
new.opera[,"logHenry"] <- log10(as.numeric(new.opera$HENRYS_LAW_ATM.M3.MOLE_OPERA_PRED))
new.opera[,"logWSol"] <- log10(as.numeric(new.opera$WATER_SOLUBILITY_MOL.L_OPERA_PRED))
chem.physical_and_invitro.data <- add_chemtable(new.opera,
  current.table = chem.physical_and_invitro.data,
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
   
#Annotate important chemicals classes as concatonated list:
chem.physical_and_invitro.data[,"Chemical.Class"] <- ""

#PFAS:
PFAS <- read.csv("Dashboard-PFASMaster-091620.tsv",sep="\t")
chem.physical_and_invitro.data[
  chem.physical_and_invitro.data[,"DTXSID"] %in% PFAS[,"DTXSID"],
  "Chemical.Class"] <- sapply(chem.physical_and_invitro.data[
    chem.physical_and_invitro.data[,"DTXSID"] %in% PFAS[,"DTXSID"],
    "Chemical.Class"], function(x) ifelse(x=="","PFAS",paste(x,"PFAS",sep=","))) 


#Remove chemicals with no DTXSID:
if (any(is.na(chem.physical_and_invitro.data$DTXSID)))
{
  cat("There are chemicals missing DTXSID's that have been removed from the\n\
chem.physical_and_invitro.data table -- written to nodtxsid.txt.\n")
  write.csv(subset(chem.physical_and_invitro.data,is.na(DTXSID)), 
    file="nodtxsid.txt",row.names=FALSE)
  chem.physical_and_invitro.data <- subset(chem.physical_and_invitro.data,
    !is.na(DTXSID))
}

chem.physical_and_invitro.data <- check_duplicates(
  chem.physical_and_invitro.data)
  
# Check for unique DTXSID for each row of chem.physical_and_invitro.data table:
length(unique(subset(chem.physical_and_invitro.data,!is.na(DTXSID))$DTXSID)) == 
  dim(subset(chem.physical_and_invitro.data,!is.na(DTXSID)))[1]

# Check for unique CAS-RN for each row of chem.physical_and_invitro.data table:
length(unique(chem.physical_and_invitro.data$CAS)) == 
  dim(chem.physical_and_invitro.data)[1]

# Check for unique name for each row of chem.physical_and_invitro.data table:
length(unique(chem.physical_and_invitro.data$Compound)) == 
  dim(chem.physical_and_invitro.data)[1]
#
#
#
# END TABLE chem.physical_and_invitro.data 
#
#
#






#
# CREATE TABLE Wetmore.data
#

Wetmore.data <- as.data.frame(
  read_excel("Supp_Table_8_SimCyp_IVIVE_input_output_083011.xls",skip=1))

Wetmore.data.1 <- Wetmore.data[Wetmore.data[,"Concentration..uM."]==1,]
Wetmore.data.10 <- Wetmore.data[Wetmore.data[,"Concentration..uM."]==10,]
Wetmore.data.abridged <- NULL
Wetmore.data.1.CASlist <- unique(Wetmore.data.1[,"CASRN"])
Wetmore.data.10.CASlist <- unique(Wetmore.data.10[,"CASRN"])
good.cols <- c(1:19)
for (this.CAS in unique(Wetmore.data[,"CASRN"]))
{
  if (this.CAS %in% Wetmore.data.1.CASlist) Wetmore.data.abridged <- 
    rbind(Wetmore.data.abridged,
    Wetmore.data.1[Wetmore.data.1[,"CASRN"]==this.CAS,good.cols][1,])
  if (this.CAS %in% Wetmore.data.10.CASlist) Wetmore.data.abridged <- 
    rbind(Wetmore.data.abridged,
    Wetmore.data.10[Wetmore.data.10[,"CASRN"]==this.CAS,good.cols][1,])
}
Wetmore.data.abridged <- cbind(Wetmore.data.abridged,"Human")
colnames(Wetmore.data.abridged)[20] <- "Species"
Wetmore.data.abridged$Reference <- "Wetmore 2012"

Wetmore.rat.data <- as.data.frame(
  read_excel("toxsci_12_0787_File012.xls",skip=4))
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

WetmorePhaseII.css.table <- read_excel("Supp_Table_4_122914.xlsx",sheet=5,skip=0)

for (this.cas in unique(WetmorePhaseII.css.table$CASRN))
{
  this.row <- Wetmore.data[1,]
  this.row[] <- NA
  this.row["Species"] <- "Human"
  fub.index <- which(WetmorePhaseII.fup.table$CAS == this.cas)[1]
  clint.index <- which(WetmorePhaseII.clint.table$X.1 == this.cas)[1]
  this.row["Compound"] <- WetmorePhaseII.fup.table[fub.index,"Name"]
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
Tox21 <- read.csv("Dashboard-Tox21.tsv",sep="\t")
ToxCast <- read.csv("Dashboard-Tox21.tsv",sep="\t")
DrugBank <- read.csv("Dashboard-DrugBank.tsv",sep="\t")

NHANES.serum <- read_excel("ACT-p2m-20150330.xlsx",sheet=1)
NHANES.blood <- read_excel("ACT-p2m-20150330.xlsx",sheet=2)
NHANES.urine <- read_excel("ACT-p2m-20150330.xlsx",sheet=3)

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


#
# Create dawson2021 Data
#

## Load in Data ##
dawson2021_full <- as.data.frame(readxl::read_xlsx(
  path = "S2_Dawson et al. Supporting_Information_Revision_Final_Sharing.xlsx",
  sheet = 14))
dawson2021      <- dawson2021_full[,c("CASRN",
                                      "QSAR_Clint","Clint QSAR AD Outlier",
                                      "QSAR_Fup","Fup QSAR AD Outlier")]
#
# END dawson2021 Creation
#

#
# Create pradeep2020 Data
#
## Load in Data ##
# load chem data
pradeep.chem <- as.data.frame(readxl::read_xlsx(
  path = "pradeep-Tox21_httk_predictions.xlsx",
  sheet = 1
  ))
# load clint data
pradeep.clint <- as.data.frame(readxl::read_xlsx(
  path = "pradeep-Tox21_httk_predictions.xlsx",
  sheet = 3
  ))
# rename column name for chemical identifier - DTXSID
pradeep.clint <- dplyr::rename(
  pradeep.clint,    # data
  "DTXSID" = "...1" # new_name = old_name
  )
# load fup data
pradeep.fup <- as.data.frame(readxl::read_xlsx(
  path = "pradeep-Tox21_httk_predictions.xlsx",
  sheet = 2
  ))
# rename column name for chemical identifier - DTXSID - & Predicted 'Fub' values
pradeep.fup <- dplyr::rename(
  pradeep.fup,             # data
  "DTXSID" = "dsstox_sid" # new_name = old_name
  )
# join prediction tables by chemical identifier
pradeep_full <- dplyr::full_join(
  pradeep.clint, # clint data
  pradeep.fup,   # fup data
  by = 'DTXSID'  # chemical ID
  ) %>%
  dplyr::left_join(.,             # clint and fup data
                   pradeep.chem,  # chemical information data
                   by = "DTXSID") # chemical ID

pradeep2020 <- dplyr::select(
  pradeep_full, # data
  c('DTXSID',"CASRN",'pred_clint_rf','Consensus (SVM,RF)') # vars to keep
  )
#
# END pradeep2020 Creation
#


#
#
#
#
#
#Add in vivo data from Wambaugh (2018):
load('NewInVivoTablesForHTTK.RData')

#
#
#
#
#
# Create Matrix pearce2017regression:
#
pearce2017regression <- # regression parameter estimates from Pearce et al. (2017)
  matrix(data = c(-0.167,0.543,-0.117,0.377,   # brain
                  -0.325,0.574,-0.324,0.544,   # adipose
                  -0.006,0.267,-0.022,0.196,   # red blood cells
                  0.143, 0.764,0.14, 0.735,    # gut
                  0.116, 0.683,0.12, 0.534,    # heart
                  0.452, 0.673,0.443, 0.631,   # kidney
                  0.475, 0.621,0.487, 0.513,   # liver
                  0.087, 0.866,0.113, 0.75,    # lung
                  -0.022, 0.658,-0.025, 0.537, # muscle
                  -0.09, 0.566,-0.086, 0.498,  # skin
                  0.034, 0.765,0.011, 0.675,   # spleen
                  0.036, 0.781,0.025, 0.758),  # bone
         nrow = 12, ncol = 4,byrow = T,
         dimnames = list(c("brain","adipose","red blood cells",
                           "gut","heart","kidney",
                           "liver","lung","muscle",
                           "skin","spleen","bone"),
                         c("adj.fup.intercept","adj.fup.slope",
                           "fup.intercept","fup.slope"))
  )
# Write to text so Git can track changes:
write.table(pearce2017regression,file = "Pearce_2017_Regression.txt",quote = F,sep = "\t")

#
# END pearce2017regression Creation
#

#
# Kapraun 2019 human gestational parameters:
#Now for the many parameters associated with the dynamic physiologic equations
#for pregnancy from Kapraun et al. (2019):
  kapraun2019 <- list(
  BW_cubic_theta1 = -0.010614,
  BW_cubic_theta2 = 0.029161,
  BW_cubic_theta3 = -5.0203e-4,
  Wadipose_linear_theta0 = 17.067,
  Wadipose_linear_theta1 = 0.14937,
  Wfkidney_gompertz_theta0 = 6.3327e-5,
  Wfkidney_gompertz_theta1 = 1.0409,
  Wfkidney_gompertz_theta2 = 0.076435,
  Wfthyroid_gompertz_theta0 = 0.0038483,
  Wfthyroid_gompertz_theta1 = 0.30799,
  Wfthyroid_gompertz_theta2 = 0.039800,
  Wfliver_gompertz_theta0 = 0.0074774,
  Wfliver_gompertz_theta1 = 0.65856,
  Wfliver_gompertz_theta2 = 0.061662,
  Wfbrain_gompertz_theta0 = 0.01574,
  Wfbrain_gompertz_theta1 = 0.70707,
  Wfbrain_gompertz_theta2 = 0.064827,
  Wfgut_gompertz_theta0 = 8.1828e-4,
  Wfgut_gompertz_theta1 = 0.65028,
  Wfgut_gompertz_theta2 = 0.047724,
  Wflung_gompertz_theta0 = 3.0454e-4,
  Wflung_gompertz_theta1 = 1.0667,
  Wflung_gompertz_theta2 = 0.084604,
  hematocrit_quadratic_theta0 = 39.192, 
  hematocrit_quadratic_theta1 = -0.10562,
  hematocrit_quadratic_theta2 = -7.1045e-4,
  fhematocrit_cubic_theta1 = 4.5061,
  fhematocrit_cubic_theta2 = -0.18487,
  fhematocrit_cubic_theta3 = 0.0026766,
  fBW_gompertz_theta0 = 0.0018282,
  fBW_gompertz_theta1 = 1.1735,
  fBW_gompertz_theta2 = 0.077577,
  Vplacenta_cubic_theta1 = -1.7646,
  Vplacenta_cubic_theta2 = 0.91775,
  Vplacenta_cubic_theta3 = -0.011543,
  Vamnf_logistic_theta0 = 822.34,
  Vamnf_logistic_theta1 = 0.26988,
  Vamnf_logistic_theta2 = 20.150,
  Vplasma_mod_logistic_theta0 = 1.2406,
  Vplasma_mod_logistic_theta1 = 0.31338,
  Vplasma_mod_logistic_theta2 = 17.813,
  Vplasma_mod_logistic_theta3 = 2.4958,
  venous_blood_fraction = 0.595,
  arterial_blood_fraction = 0.16,
  fblood_weight_ratio = 80, #in ml/kg
  Qcardiac_cubic_theta0 = 301.78,
  Qcardiac_cubic_theta1 = 3.2512,
  Qcardiac_cubic_theta2 = 0.15947,
  Qcardiac_cubic_theta3 = -0.0047059,
  term = 40.0, #weeks at delivery
  Qgut_percent_initial = 17.0,
  Qgut_percent_terminal = 12.5,
  Qkidney_cubic_theta0 = 53.248,
  Qkidney_cubic_theta1 = 3.6447,
  Qkidney_cubic_theta2 = -0.15357,
  Qkidney_cubic_theta3 = 0.0016968,
  Qliver_percent_initial = 27.0,
  Qliver_percent_terminal = 20.0,
  Qthyroid_percent_initial = 1.5,
  Qthyroid_percent_terminal = 1.1,
  Qplacenta_linear_theta1 = 0.059176,
  Qadipose_percent_initial = 8.5,
  Qadipose_percent_terminal = 7.8,
  Qgfr_quadratic_theta0 = 113.73,
  Qgfr_quadratic_theta1 = 3.5784,
  Qgfr_quadratic_theta2 = -0.067272,
  Qfrvtl_logistic_theta0 = 2466.5,
  Qfrvtl_logistic_theta1 = 0.14837,
  Qfrvtl_logistic_theta2 = 43.108,
  Qflvtl_logistic_theta0 = 506.30,
  Qflvtl_logistic_theta1 = 0.21916,
  Qflvtl_logistic_theta2 = 30.231,
  Qfda_logistic_theta0 = 1125.3,
  Qfda_logistic_theta1 = 0.18031,
  Qfda_logistic_theta2 = 35.939,
  Qfplacenta_logistic_theta0 = 262.20,
  Qfplacenta_logistic_theta1 = 0.22183,
  Qfplacenta_logistic_theta2 = 28.784,
  Qfdv_gompertz_theta0 = 1.892,
  Qfdv_gompertz_theta1 = 0.098249,
  Qfdv_gompertz_theta2 = 0.0064374,
  Qfnonplacental_percent = 75.0,
  Qfgut_percent = 6.8,
  Qfkidney_percent = 5.4,
  Qfbrain_percent = 14.3,
  Qbrain_percent = 12, #average of male/female ICRP 2002
  Qkidney_percent = 18, #average of male/female ICRP 2002
  Qgut_percent = 16, #average of male/female ICRP 2002
  Qfliver_percent = 6.5,
  Qfthyroid_percent = 1.5
  )
#
#
#


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
     dawson2021,
     pradeep2020,
     physiology.data,
     pearce2017regression,
     kapraun2019,
     tissue.data,
     Tables.Rdata.stamp,
     EPA.ref,
     file="Tables.RData",
     compress="xz",
     version=2)

cat("Move the Tables.RData to the httk/data directory.\n")
cat("Move the sysdata.rdaa to the httk/R directory.\n")

sysdata.rda.stamp <- paste("This sysdata.rdata file was created on",Sys.Date(),"by script version",SCRIPT.VERSION)

sipes2017 <- sipes2017[,c(
               'CAS',
               'Human.Funbound.plasma',
               'Human.Clint')]
 
save(Wetmore.data,
     sipes2017,
     chem.lists,
     sysdata.rda.stamp,                 
     file="sysdata.rda",
     compress="xz",
     version=2)
     
## Session Information ##
Sys.time() # capture date and time of generating data
sessionInfo() # capture package information for generating data