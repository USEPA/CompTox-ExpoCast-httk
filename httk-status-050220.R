library(httk)
library(gdata)

setwd("c:/users/jwambaug/git/httk-status")

Cyprotex.failed <- read.csv("Cyprotex-NoAnalyticChem.txt",stringsAsFactors=F)$x
Cyprotex.forgot <- read.xls("TO12-nomethods.xls",stringsAsFactors=F)$DTXSID
Cyprotex.failed <- Cyprotex.failed[!(Cyprotex.failed %in% Cyprotex.forgot)]
Cyprotex.Caco2 <- read.csv("caco2_toxcast_chems.csv",stringsAsFactors=F)$dtxsid
                                                 
Health.Canada <- read.xls("20191025-Health_Canada_Prospective_Data_HTTK.xlsx",stringsAsFactors=F)
Health.Canada.NoFup <- subset(Health.Canada,is.na(Human.Funbound.plasma))$DTXSID
Health.Canada <- Health.Canada$DTXSID

JRC.clint <- read.xls("APCRA-JRC_HepatocyteStability+ProteinBinding_77_Summary.xlsx",
  stringsAsFactors=F,sheet=2)
JRC.clint <- unique(subset(JRC.clint, !is.na(CL.int.cells..µL.min.106.cells.))$DTXSID)
JRC.fup <- read.xls("APCRA-JRC_HepatocyteStability+ProteinBinding_77_Summary.xlsx",
 stringsAsFactors=F,sheet=3)
JRC.fup <- unique(subset(JRC.fup, !is.na(X..Unbound.in.Plasma..))$DTXSID)
JRC.NoFup <- JRC.clint[!(JRC.clint%in%JRC.fup)]

EFSA.interest <- read.xls("EFSA Compounds for HTTK_030119.xlsx",stringsAsFactors=F)$DTXSID

ACC.rathep <- read.xls("ACC_List_3_30_2020 v2.xlsx",stringsAsFactors=F)
ACC.mousehep <- subset(ACC.rathep,Tox.species %in% "MOUSE")$DTXSID
ACC.doghep <- subset(ACC.rathep,Tox.species %in% "DOG")$DTXSID
ACC.rabbithep <- subset(ACC.rathep,Tox.species %in% "MOUSE")$DTXSID
ACC.rathep <- subset(ACC.rathep,Tox.species %in% c("RAT","rat","rats","Wistar rats"))$DTXSID


httk.human <- as.character(get_cheminfo(info="DTXSID"))
httk.NoFup <- httk.human[!(httk.human %in% get_cheminfo(info="DTXSID",model="PBTK"))]
httk.rat <- as.character(get_cheminfo(species="Rat",info="DTXSID"))
httk.ratnohuman <- httk.rat[!(httk.rat %in% httk.human)]
 
TSCA.430 <- read.xls("TSCA_430_Wetmore_plating_5Dec2018.xlsx",stringsAsFactors=F)$DTXSID

all.chems <- sort(unique(c(
  Cyprotex.failed,
  Cyprotex.Caco2,
  Health.Canada,
  JRC.clint,
  JRC.fup,
  EFSA.interest,
  ACC.rathep,
  ACC.mousehep,
  ACC.doghep,
  ACC.rabbithep,
  httk.human,
  httk.rat,
  TSCA.430)))

#The ToxCast list:
ToxCast <- read.csv("Dashboard-ToxCast.tsv",stringsAsFactors=F,sep="\t")$DTXSID
Tox21 <- read.csv("Dashboard-Tox21.tsv",stringsAsFactors=F,sep="\t")$DTXSID

out.table <- NULL
for (this.id in all.chems)
{
      this.row <- as.data.frame(this.id)
      this.row <- cbind(this.row,t(as.data.frame(rep(0,times=17))))
      colnames(this.row) <- c(
        "DTXSID",
        "Tox21",
        "ToxCast",
        "HTTK.Human.Any",
        "HTTK.Human.Public",
        "HTTK.NoFup",
        "HTTK.Rat",
        "EPA.Internal",
        "EPA.Caco2",
        "EPA.Analytical.Failed",
        "Health.Canada",
        "JRC.Hep",
        "JRC.Fup",
        "EFSA.Interest",
        "ACC.Rat.Hep",
        "ACC.Mouse.Hep",
        "ACC.Dog.Hep",
        "ACC.Rabbit.Hep")
      if (this.id %in% TSCA.430) this.row["EPA.Internal"] <- 1
      if (this.id %in% Health.Canada) this.row["Health.Canada"] <- 1
      if (this.id %in% JRC.clint) this.row["JRC.Hep"] <- 1
      if (this.id %in% JRC.fup) this.row["JRC.Fup"] <- 1
      if (this.id %in% EFSA.interest) this.row["EFSA.Interest"] <- 1
      if (this.id %in% c(httk.human,Health.Canada,JRC.clint,EFSA.interest,TSCA.430)) 
        this.row["HTTK.Human.Any"] <- 1
      if (this.id %in% httk.human) this.row["HTTK.Human.Public"] <- 1
      if (this.id %in% httk.NoFup) this.row["HTTK.NoFup"] <- 1
      if (this.id %in% httk.rat) this.row["HTTK.Rat"] <- 1
      if (this.id %in% Cyprotex.failed) this.row["EPA.Analytical.Failed"] <- 1
      if (this.id %in% Cyprotex.Caco2) this.row["EPA.Caco2"] <- 1
      if (this.id %in% ACC.doghep) this.row["ACC.Dog.Hep"] <- 1
      if (this.id %in% ACC.mousehep) this.row["ACC.Mouse.Hep"] <- 1
      if (this.id %in% ACC.rabbithep) this.row["ACC.Rabbit.Hep"] <- 1
      if (this.id %in% ACC.rathep) this.row["ACC.Rat.Hep"] <- 1
      if (this.id %in% Tox21) this.row["Tox21"] <- 1
      if (this.id %in% ToxCast) this.row["ToxCast"] <- 1

      
      out.table <- rbind(out.table,this.row)
}

write.table(out.table,file=paste("HTTK-status-",Sys.Date(),".txt",sep=""),row.names=F,sep="\t")





