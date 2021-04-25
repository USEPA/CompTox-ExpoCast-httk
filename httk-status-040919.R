library(httk)
library(gdata)

setwd("L:/Lab/NCCT_ExpoCast/ExpoCast2019/httk-status")

source("L:/Lab/NCCT_ExpoCast/ExpoCast2018/HTTKNewData/Summary/build-httk-master-list-061318.R")
  
Cyprotex.attempted <- subset(master.table,Species=="Human")$DTXSID
Cyprotex.paper <- subset(chem.physical_and_invitro.data,
                         Human.Clint.Reference=="Wambaugh Submitted" | 
                         Human.Funbound.plasma.Reference=="Wambaugh Submitted")$DTXSID
Cyprotex.failed <- Cyprotex.attempted[!(Cyprotex.attempted%in%Cyprotex.paper)]
Cyprotex.clint <- subset(chem.physical_and_invitro.data,
                         Human.Clint.Reference=="Wambaugh Submitted")$DTXSID
Cyprotex.fup <- subset(chem.physical_and_invitro.data, 
                         Human.Funbound.plasma.Reference=="Wambaugh Submitted")$DTXSID                         
                                                  
invivo<- read.xls("TableS11-TKSummaryStats-2017-08-30.xlsx",stringsAsFactors=F)$DSSTox_Substance_Id

unilever <- c("DTXSID8031077","DTXSID8021480","DTXSID00872663","DTXSID30865801","DTXSID3041035","DTXSID0020232","DTXSID7020348","DTXSID7020716")

httk110.human <- as.character(get_cheminfo(info="DTXSID"))
httk110.rat <- as.character(get_cheminfo(species="Rat",info="DTXSID"))

sharktank <- read.csv("HTTr-crosswalk-4Dec2018.csv",stringsAsFactors=F)
shark1 <- subset(sharktank,shark=="shark_1")$DTXSID
shark2 <- subset(sharktank,shark=="shark_2")$DTXSID
shark3 <- subset(sharktank,shark=="shark_3")$DTXSID
sharkbackfill <- subset(sharktank,shark=="shark_backfill")$DTXSID
TSCA.lps <- read.csv("L:/Lab/NCCT_ExpoCast/ExpoCast2019/TSCA-Fire-Drill/TigerTeamExposure-LPS-2019-03-21.txt",stringsAsFactors=F)$DTXSID
TSCA.poc <- read.csv("L:/Lab/NCCT_ExpoCast/ExpoCast2019/TSCA-Fire-Drill/TigerTeamExposure-PoC-2019-03-21.txt",stringsAsFactors=F)$DTXSID
TSCA.all <- read.csv("L:/Lab/NCCT_ExpoCast/ExpoCast2019/TSCA-Fire-Drill/TigerTeamExposure-2019-03-21.txt",stringsAsFactors=F)$DTXSID
TSCA.430 <- read.xls("TSCA_430_Wetmore_plating_5Dec2018.xlsx",stringsAsFactors=F)$DTXSID

all.chems <- sort(unique(c(unilever, invivo,Cyprotex.attempted,httk110.human,httk110.rat,shark1,shark2,shark3,sharkbackfill,TSCA.430,TSCA.poc)))


#The ToxCast list:
ToxCast <- read.xls("L:/Lab/NCCT_ExpoCast/ExpoCast2018/httk-status/ToxCast-list_chemicals-2018-04-11-15-38-51.xls",stringsAsFactors=F)$DTXSID
Tox21 <- read.xls("L:/Lab/NCCT_ExpoCast/ExpoCast2018/httk-status/Tox21-list_chemicals-2018-04-11-15-40-00.xls",stringsAsFactors=F)$DTXSID

out.table <- NULL
for (this.id in all.chems)
{
      this.row <- as.data.frame(this.id)
      this.row <- cbind(this.row,t(as.data.frame(rep(0,times=15))))
      colnames(this.row) <- c("DTXSID","TSCA","TSCA.PoC","TSCA.430","Tox21","ToxCast","Unilever","HTTK.1.10.Human","HTTK.1.10.Rat","HTTK.Cyprotex.Clint","HTTK.Cyprotex.Fup","HTTK.Cyprotex.Failed","Sharktank.1","Sharktank.2","Sharktank.3","Sharktank.Backfill")
      if (this.id %in% TSCA.all) this.row["TSCA"] <- 1
      if (this.id %in% TSCA.poc) this.row["TSCA.PoC"] <- 1
      if (this.id %in% TSCA.430) this.row["TSCA.430"] <- 1
      if (this.id %in% httk110.human) this.row["HTTK.1.10.Human"] <- 1
      if (this.id %in% httk110.rat) this.row["HTTK.1.10.Rat"] <- 1
      if (this.id %in% Cyprotex.clint) this.row["HTTK.Cyprotex.Clint"] <- 1
      if (this.id %in% Cyprotex.fup) this.row["HTTK.Cyprotex.Fup"] <- 1
      if (this.id %in% Cyprotex.failed) this.row["HTTK.Cyprotex.Failed"] <- 1
      if (this.id %in% Tox21) this.row["Tox21"] <- 1
      if (this.id %in% ToxCast) this.row["ToxCast"] <- 1
      if (this.id %in% unilever) this.row["Unilever"] <- 1
      if (this.id %in% shark1) this.row["Sharktank.1"] <- 1
      if (this.id %in% shark2) this.row["Sharktank.2"] <- 1
      if (this.id %in% shark3) this.row["Sharktank.3"] <- 1
      if (this.id %in% sharkbackfill) this.row["Sharktank.Backfill"] <- 1
      
      out.table <- rbind(out.table,this.row)
}
dim(subset(out.table,TSCA.PoC==1))
dim(subset(out.table,TSCA.PoC==1&HTTK.1.10.Human==1))
dim(subset(out.table,TSCA.PoC==1&HTTK.1.10.Human==0))
dim(subset(out.table,TSCA.430==1))
dim(subset(out.table,TSCA.430==1&HTTK.1.10.Human==1))
dim(subset(out.table,TSCA.PoC==1&HTTK.1.10.Human==0&TSCA.430==0))
dim(subset(out.table,TSCA.PoC==1&HTTK.1.10.Human==0&TSCA.430==1))

dim(subset(out.table,TSCA.PoC==1&HTTK.1.10.Human==0&TSCA.430==0&ToxCast==1))



write.table(out.table,file=paste("HTTK-status-",Sys.Date(),".txt",sep=""),row.names=F,sep="\t")

write.table(subset(out.table,HTTK.Cyprotex.Failed==1),file=paste("HTTK-CyprotexFails-",Sys.Date(),".txt",sep=""),row.names=F,sep="\t")




