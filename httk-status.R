library(httk)
library(readxl)

#setwd("c:/users/jwambaug/git/httk-status")

ToxCastPh1 <- read.csv("Dashboard-ToxCastPhase1v2.tsv",
  sep="\t",
  stringsAsFactors=F)$DTXSID
ToxCastPh2 <- read.csv("Dashboard-ToxCastPhase2.tsv",
  sep="\t",
  stringsAsFactors=F)$DTXSID
ToxCastPh2 <- ToxCastPh2[regexpr("DTXSID",ToxCastPh2)!=-1]
FailedPharma <- 
  read_excel("DSSTox_TOXCAST_donatedpharma_20170531.xlsx")$DSSTox_Substance_Id
ToxCastPh12 <- sort(unique(c(ToxCastPh1,ToxCastPh2)))

ToxCastPh12.test <- ToxCastPh12[!(ToxCastPh12 %in% FailedPharma)]

# HTTK TO1 200 Chemicals 2020
HTTK2.TO1 <- read.table("HTTK2TO1-datastatus.txt",header=TRUE)
HTTK2.TO1.sent <- read_excel("EPA_35957_Cyprotex-MA_215_20200611_key.xlsx")$DTXSID
HTTK2.TO1.Clint <- subset(HTTK2.TO1,Clint==1)$DTXSID
HTTK2.TO1.Fup <- subset(HTTK2.TO1,PPB==1)$DTXSID
HTTK2.TO1.CACO2 <- subset(HTTK2.TO1,Caco2==1)$DTXSID
HTTK2.TO1.RB2P <- subset(HTTK2.TO1,B2P==1)$DTXSID
HTTK2.TO1 <- intersect(HTTK2.TO1.Clint,HTTK2.TO1.Fup)

HTTK2.TO1.failed <- HTTK2.TO1.sent[!(HTTK2.TO1.sent %in% 
  HTTK2.TO1)]
  
# HTTK TO2 Pregnancy Model HTTK TO3 Barbara
HTTK2.TO23 <-read.table("TO23-status.txt",header=TRUE)
HTTK2.TO23.failed <<- subset(HTTK2.TO23,Failed==1)$DTXSID
HTTK2.TO23 <<- subset(HTTK2.TO23,Failed==0)$DTXSID



  

HTTK1.failed <- read.csv("Cyprotex-NoAnalyticChem.txt",stringsAsFactors=F)$x
HTTK1.forgot <- read_excel("TO12-nomethods.xls")$DTXSID
HTTK1.failed <- HTTK1.failed[!(HTTK1.failed %in% HTTK1.forgot)]
HTTK1.Caco2 <- read.csv("caco2_toxcast_chems.csv",stringsAsFactors=F)$dtxsid
      
Cyprotex.failed <- c(HTTK1.failed,HTTK2.TO1.failed, HTTK2.TO23.failed)                                             
                                                 
Health.Canada <- read_excel("20191025-Health_Canada_Prospective_Data_HTTK.xlsx")
Health.Canada.NoFup <- subset(Health.Canada,is.na(Human.Funbound.plasma))$DTXSID
Health.Canada <- Health.Canada$DTXSID

EFSA.interest <- read_excel("EFSA Compounds for HTTK_030119.xlsx")$DTXSID
     
ACC.rathep <- read_excel("ACC_List_3_30_2020 v2.xlsx")
ACC.mousehep <- subset(ACC.rathep,'Tox species' %in% "MOUSE")$DTXSID
ACC.doghep <- subset(ACC.rathep,'Tox species' %in% "DOG")$DTXSID
ACC.rabbithep <- subset(ACC.rathep,'Tox species' %in% "MOUSE")$DTXSID
ACC.rathep <- subset(ACC.rathep,'Tox species' %in% c("RAT","rat","rats","Wistar rats"))$DTXSID


httk.human <- as.character(get_cheminfo(info="DTXSID"))
httk.NoFup <- httk.human[!(httk.human %in% get_cheminfo(info="DTXSID",model="PBTK"))]
httk.rat <- as.character(get_cheminfo(species="Rat",info="DTXSID"))
httk.ratnohuman <- httk.rat[!(httk.rat %in% httk.human)]
httk.badFup <- get_cheminfo(info="DTXSID",fup.ci.cutoff=F)[
  !(get_cheminfo(info="DTXSID",fup.ci.cutoff=F)%in%httk.human)]
ToxCastPh12.untested <- ToxCastPh12.test[!(ToxCastPh12.test %in%
  c(httk.human,HTTK2.TO1))]
  
   
TSCA.430 <- read_excel("TSCA_430_Wetmore_plating_5Dec2018.xlsx")$DTXSID

#The ToxCast list:
ToxCast <- read.csv("Dashboard-ToxCast.tsv",stringsAsFactors=F,sep="\t")$DTXSID
Tox21 <- read.csv("Dashboard-Tox21.tsv",stringsAsFactors=F,sep="\t")$DTXSID
Pesticides <- read.csv("Dashboard-PestActives.tsv",stringsAsFactors=F,sep="\t")$DTXSID
Avail.Pesticides <- ToxCast[ToxCast %in% Pesticides]


all.chems <- sort(unique(c(
  ToxCastPh12.test,
  HTTK2.TO1.sent,
  HTTK2.TO23,
  HTTK2.TO23.failed,
  Cyprotex.failed,
  HTTK1.Caco2,
  HTTK1.failed,
  HTTK1.forgot,
  Health.Canada,
  EFSA.interest,
  ACC.rathep,
  ACC.mousehep,
  ACC.doghep,
  ACC.rabbithep,
  httk.human,
  httk.rat,
  TSCA.430,
  Avail.Pesticides)))


out.table <- NULL
for (this.id in all.chems)
{
      this.row <- as.data.frame(this.id)
      this.row <- cbind(this.row,t(as.data.frame(rep(0,times=20))))
      colnames(this.row) <- c(
        "DTXSID",
        "Pesticide",
        "Tox21",
        "ToxCast",
        "ToxCastPh12",
        "ToxCastPh12.Remaining",
        "HTTK.Human.Any",
        "HTTK.Human.Public",
        "HTTK.BadFup",
        "HTTK.Rat",
        "EPA.Contract.New",
        "EPA.Internal",
        "EPA.CACO2",
        "EPA.RB2P",
        "EPA.Analytical.Failed",
        "Health.Canada",
        "EFSA.Interest",
        "ACC.Rat.Hep",
        "ACC.Mouse.Hep",
        "ACC.Dog.Hep",
        "ACC.Rabbit.Hep")
      if (this.id %in% Pesticides) this.row["Pesticide"] <- 1
      if (this.id %in% TSCA.430) this.row["EPA.Internal"] <- 1
      if (this.id %in% Health.Canada) this.row["Health.Canada"] <- 1
      if (this.id %in% EFSA.interest) this.row["EFSA.Interest"] <- 1
      if (this.id %in% c(
        httk.human,
        Health.Canada,
        HTTK2.TO1,
        EFSA.interest,
        TSCA.430)) 
        this.row["HTTK.Human.Any"] <- 1
      if (this.id %in% httk.human) this.row["HTTK.Human.Public"] <- 1
      if (this.id %in% c(httk.NoFup,httk.badFup)) this.row["HTTK.BadFup"] <- 1
      if (this.id %in% httk.rat) this.row["HTTK.Rat"] <- 1
      if (this.id %in% c(HTTK2.TO1,HTTK2.TO23)) this.row["EPA.Contract.New"] <- 1
      if (this.id %in% Cyprotex.failed) this.row["EPA.Analytical.Failed"] <- 1
      if (this.id %in% c(HTTK1.Caco2,HTTK2.TO1.CACO2)) this.row["EPA.CACO2"] <- 1
      if (this.id %in% HTTK2.TO1.RB2P) this.row["EPA.RB2P"] <- 1
      if (this.id %in% ACC.doghep) this.row["ACC.Dog.Hep"] <- 1
      if (this.id %in% ACC.mousehep) this.row["ACC.Mouse.Hep"] <- 1
      if (this.id %in% ACC.rabbithep) this.row["ACC.Rabbit.Hep"] <- 1
      if (this.id %in% ACC.rathep) this.row["ACC.Rat.Hep"] <- 1
      if (this.id %in% Tox21) this.row["Tox21"] <- 1
      if (this.id %in% ToxCast) this.row["ToxCast"] <- 1
      if (this.id %in% ToxCastPh12.test) this.row["ToxCastPh12"] <- 1
      if (this.id %in% ToxCastPh12.untested) this.row["ToxCastPh12.Remaining"] <- 1
      
      out.table <- rbind(out.table,this.row)
}

write.table(out.table,"HTTK-status-all.txt",row.names=FALSE,sep="\t")
write.table(subset(out.table, EPA.Analytical.Failed == 0),"HTTK-status-good.txt",row.names=FALSE,sep="\t")
write.table(subset(out.table, EPA.Analytical.Failed == 1),"HTTK-status-failed.txt",row.names=FALSE,sep="\t")
write.table(Sys.Date(),row.names=FALSE, file="timestamp.txt")
write.table(packageVersion("httk"),row.names=FALSE, file="httkversion.txt")





