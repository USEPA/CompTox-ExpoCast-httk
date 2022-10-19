
.libPaths(new = "C:/Users/GHONDA/Documents/R/win-library/3.3")


library(ggplot2)
library(hexbin)
library(dplyr)
library(gplots)
library(httk)
library(RColorBrewer)
library(viridis)
library(data.table)
library(magrittr)
library(stringr)
library(gridExtra)
library(stringi)
library(RMySQL)
library(DBI)
library(grid)
library(xlsx)
library(lattice)
library(gtable)
library(ggpubr)
library(opeRa)

loc.wd <- "C:/Users/GHONDA/Documents/R homebrew/read_new_rat_v1"
##### read new rat data #####
  # filtering new rat data
setwd(paste0(loc.wd,"/NewData/new_data_040417"))
  CeeTox <- fread("EPA_11117_CeeTox_150RTK_2mg_24Jun2015.csv") %>% 
    setnames(c("EPA_SAMPLE_ID","ALIQUOT_VIAL_BARCODE","preferred_name"),c("sampleID","aliquot","compound")) %>% 
    .[,casrn := gsub("[']","",casrn)]
  
  Cyprotex <- fread("EPA_11273_Cyprotex_83HTTK_2mg_12Oct2015.csv") %>% 
    .[,aliquot := as.character(ALIQUOT_VIAL_BARCODE)] %>% 
    setnames(c("Substance_CAS-RN","Substance_Name","DSSTox_Substance_Id","EPA_SAMPLE_ID"),
             c("casrn","compound","DSSToxSID","sampleID"))
  
  Fub.Raw1 <- as.data.table(read.csv("EPA_T05_Rat_PPB_Final Data_HumanPlasma.csv",stringsAsFactors = F))
  Fub1 <- Fub.Raw1[,list(aliquot = Test.Article,Fub=Avg...Free,rec=Avg...Recover,plasma.conc=Human.Plasma.Protein..Conc.,Comment=Comment)] %>% 
    .[,Fub:=as.numeric(gsub("%","",Fub))/100] %>% 
    .[,rec:=as.numeric(gsub("%","",rec))/100] %>% 
    .[,.(Fub10=mean(Fub[plasma.conc == "10%"]),
         Fub30=mean(Fub[plasma.conc == "30%"]),
         Fub100=mean(Fub[plasma.conc == "100%"]),
         rec10=mean(rec[plasma.conc == "10%"]),
         rec30=mean(rec[plasma.conc == "30%"]),
         rec100=mean(rec[plasma.conc == "100%"]),
         Comment=Comment[plasma.conc=="100%"][1]),.(aliquot)] %>% 
    .[aliquot!=""] %>% 
    Cyprotex[,list(aliquot,casrn,DSSToxSID,compound)][.,on="aliquot"] %>% 
    .[aliquot =="Propranolol",compound:="Propranolol"] %>% 
    .[aliquot=="Propranolol",casrn:="525-66-6"] %>% 
    .[aliquot=="Warfarin",compound:="Warfarin"] %>% 
    .[aliquot=="Warfarin",casrn:="81-81-2"] %>% 
    .[,aliquot:=NULL]
  
  Fub.Raw2 <- as.data.table(read.csv("EPA_T05_PPB _Final_Report_FUB.csv",stringsAsFactors = F))
  Fub2 <- Fub.Raw2[,list(sampleID = Test.Article, Fub=as.numeric(Free.Average),rec=as.numeric(Rec.Average),plasma.conc = Rat.Plasma.Protein.Conc,Comments=Comments)] %>% 
    .[,rec:=rec/100] %>% 
    .[,.(Fub10=mean(Fub[plasma.conc == "10%"]),
         Fub30=mean(Fub[plasma.conc == "30%"]),
         Fub100=mean(Fub[plasma.conc == "100%"]),
         rec10=mean(rec[plasma.conc == "10%"]),
         rec30=mean(rec[plasma.conc == "30%"]),
         rec100=mean(rec[plasma.conc == "100%"]),
         Comment=Comments[plasma.conc=="100%"][1]),.(sampleID)] %>% 
    CeeTox[,list(sampleID,casrn,compound)][.,on="sampleID"]  %>% 
    .[sampleID =="propranolol",compound:="Propranolol"] %>% 
    .[sampleID=="propranolol",casrn:="525-66-6"] %>% 
    .[sampleID=="warfarin",compound:="Warfarin"] %>% 
    .[sampleID=="warfarin",casrn:="81-81-2"] %>% 
    .[,sampleID:=NULL]
  
  Clint.Raw <- as.data.table(read.csv("T05_Rat_Hep_Report_160308.csv",stringsAsFactors = F))
  setnames(Clint.Raw,c(1,4),c("aliquot","Clearance"))
  Clint1 <- Clint.Raw[,list(aliquot,Concentration,microM, Clearance)] %>% 
    .[regexpr("-",aliquot)!=-1,aliquot:=sapply(str_split(aliquot,"-"),function(x) x[1])] %>% 
    .[,.(clear1=Clearance[1],clear10=Clearance[2]),.(aliquot)] %>% 
    .[!(aliquot=="")] %>% 
    Cyprotex[,list(aliquot,casrn,DSSToxSID,compound)][.,on="aliquot"] %>% 
    .[aliquot=="Warfarin",casrn:= "81-81-2"] %>%
    .[aliquot=="Warfarin",compound:="Warfarin"] %>% 
    .[aliquot=="Verapamil", casrn:="52-53-9"] %>% 
    .[aliquot=="Verapamil",compound:="Verapamil"] %>% 
    .[,aliquot:=NULL]
  
  Clint.Raw2 <- as.data.table(read.csv("EPA_T05_Rat Hepatocytes_Clearance.csv",stringsAsFactors = F))
  Clint2 <- Clint.Raw2[,list(sampleID=EPA_SAMPLE_ID,compound = Preferred_name,clear1=Clearance.1.mM,clear10=Clearance.10.mM)] %>% 
    CeeTox[,list(sampleID,casrn)][.,on="sampleID"] %>% 
    .[compound =="verapamil",compound:="Verapamil"] %>% 
    .[compound=="Verapamil",casrn:="52-53-9"] %>% 
    .[compound=="warfarin",compound:="Warfarin"] %>% 
    .[compound=="Warfarin",casrn:="81-81-2"] %>% 
    .[,sampleID:=NULL]
  
  setwd(loc.wd)
  
  rat.data <- rbind(Fub1[,list(casrn,Fub10,Fub30,Fub100)][Clint1,on="casrn"][,DSSToxSID:=NULL][,reference:="unpublished Cyprotex"],
                    Fub2[,list(casrn,Fub10,Fub30,Fub100)][Clint2,on="casrn"][,reference:="unpublished Ceetox"])
  new.rat <- copy(rat.data)
  
  load(paste0(loc.wd,"/NewData/chemprops-011717.RData")) # episuite chemprops
  
  
  #ACD labs pka predictions
  # pkas
  john.pkas <- fread(paste0(loc.wd,"/NewData/ratcast-pKas.txt")) # measured in literature
  tony.pkas <- fread(paste0(loc.wd,"/NewData/pKas_ACDLabs_TWilliams.csv"))
  
  setnames(tony.pkas,"Substance_CASRN","casrn")
  setnames(tony.pkas,c("ACD_pKa_Acidic_Apparent_1","ACD_pKa_Basic_Apparent_1"),c("pKa_Donor_t","pKa_Accept_t"))
  setnames(john.pkas,"CAS","casrn")
  
  full.pkas <- tony.pkas[john.pkas,c("casrn","pKa_Donor","pKa_Accept",
                                     "pKa_Donor_t","pKa_Accept_t"),on="casrn"]
  
  full.pkas[,pKa_Donor_source:=as.character(NA)] %>% 
    .[,pKa_Accept_source:=as.character(NA)] %>% 
    .[pKa_Donor!="",pKa_Donor_source:="jwambaugh"] %>% 
    .[pKa_Accept!="",pKa_Accept_source:="jwambaugh"] %>% 
    .[pKa_Donor=="",pKa_Donor_source:="twilliams"] %>% 
    .[pKa_Accept=="",pKa_Accept_source:="twilliams"] %>% 
    .[pKa_Donor=="",pKa_Donor:=as.character(pKa_Donor_t)] %>% 
    .[pKa_Accept=="",pKa_Accept:=as.character(pKa_Accept_t)] %>% 
    .[,pKa_Donor_t:=NULL] %>% 
    .[,pKa_Accept_t:=NULL]
  rat.pKas <- full.pkas
  
  rat.pKas <- unique(rat.pKas)
  rat.pKas <- rbind(rat.pKas,data.table(casrn="2467-02-9",pKa_Accept = NA,pKa_Donor = 9.81,pKa_Donor_source="jwambaugh",pKa_Accept_source=as.character(NA))) # episuite, measured; date error in .txt
  rat.pKas[,pKa_Accept:=gsub(";",",",pKa_Accept)] %>% 
    .[,pKa_Donor:=gsub(";",",",pKa_Donor)]
  
  new.rat <- rat.pKas[new.rat,on="casrn"]
  
  for (this.cas in new.rat$casrn)
  {
    if (this.cas %in% chemprop.table$CASRN)
    {
      new.rat[new.rat$casrn==this.cas,"MW"] <- chemprop.table[chemprop.table$CASRN==this.cas,"MolecularWeight"]
      new.rat[new.rat$casrn==this.cas,"logP"] <- chemprop.table[chemprop.table$CASRN==this.cas,"LogP"]
    }
  }
  
  load(paste0(loc.wd, "/NewData/rat_data_httkv1.8.RData"))
  old.rat <- copy(ratv1.8)
  
  new.rat <- new.rat[,clear1.orig:=clear1] %>% 
    .[,clear10.orig:=clear10] %>% 
    .[clear1 %in% c("< 2.9","<2.9","< 2.9 "),clear1:="0.0"] %>% 
    .[clear10 %in% c("< 2.9","<2.9","< 2.9 "),clear10:="0.0"] %>% 
    .[,clearance:=as.numeric(clear1)] %>% 
    .[is.na(clearance),clearance:=as.numeric(clear10)] %>%
    .[,Funbound.plasma := Fub100] %>% 
    .[Funbound.plasma<0.005,Funbound.plasma := 0.00] %>% 
    .[casrn == "1319-77-3",pKa_Donor := "10.2"] %>% # 	PEARCE,PJ & SIMKINS,RJJ (1968)
    .[casrn == "54965-24-1",pKa_Accept := "8.69"] # ACD
  
  new.rat[,clearance1:=as.numeric(clear1)] %>% 
    .[,clearance10:=as.numeric(clear10)]
  
  new.rat[,.N,.(casrn)][N>1,]
  new.rat[casrn%in% c("81-81-2","52-53-9")]
  
  #drop similar repeats
  # new.rat <- new.rat[!(casrn=="81-81-2" & Fub100 == 0.005)] %>% 
  #   .[!(casrn=="52-53-9" & clearance == 45.5)]
  
  # add in dsstox_sid
  dsstox <- fread(paste0(loc.wd,"/NewData/opera_prediction_19Mar2018.csv")) # pulled from dashboard 9/28/2017
  setnames(dsstox,c("DTXSID","PREFERRED_NAME"),c("DSSTox_Substance_Id","preferred_name"))
  new.rat <- dsstox[,.(casrn,DSSTox_Substance_Id,preferred_name)][new.rat,on="casrn"]
  # temp2 <- new.rat[casrn %in% old.rat$CAS,]
  # fupdate.rat<- copy(temp2)[casrn!="52-53-9",]
  
  #new.rat <- new.rat[!(casrn %in% old.rat$CAS) & !is.na(Funbound.plasma) & !is.na(clearance)]
  comptox.props <- fread(paste0(loc.wd,"/NewData/opera_prediction_19Mar2018.csv"))
  comptox.props[,Henry_opera:=`HENRYS_LAW_ATM-M3/MOLE_OPERA_PRED`] %>% 
    .[,WaterSol_opera:=`WATER_SOLUBILITY_MOL/L_OPERA_PRED`] %>% 
    .[,MP_opera:=MP]
  
  new.rat <- comptox.props[,.(casrn,logP_opera)][new.rat,on="casrn"]
  
  meas.rat <- unique(c(new.rat$casrn,old.rat$CAS))
  these.chemprop <- as.data.table(copy(chemprop.table))[CASRN %in% meas.rat,]
  setnames(these.chemprop,c("CASRN"),"casrn")
  
  comb.chemprop <- these.chemprop[,.(casrn,
                                     MolecularWeight,
                                     LogP_EPI,
                                     LogP_Exp,
                                     MP_Exp,
                                     WaterSol_Exp,
                                     Henry_Exp)][comptox.props[casrn%in%meas.rat,.(casrn,
                                                                                   logP_opera,
                                                                                   MP_opera,
                                                                                   WaterSol_opera,
                                                                                   Henry_opera)],on="casrn"]
  temp <- copy(comb.chemprop)
  comb.chemprop[,MW:=MolecularWeight] %>% 
    .[,logP_source:="opera"] %>% 
    .[,logP:=as.numeric(logP_opera)] %>% 
    .[,MP:=as.numeric(MP_opera)] %>% 
    .[,WaterSol_Exp:=(WaterSol_Exp/(1000*MW))] %>% # convert to mol/L
    .[,WaterSol:=WaterSol_opera] %>%
    .[,WaterSol_source:="opera"] %>% 
    .[,Henry:=Henry_opera] %>% 
    .[,Henry_source:="opera"]
  
  
  new.rat <- comb.chemprop[,.(casrn,logP2=logP,logP_source)][new.rat,on="casrn"]
  new.rat[,logP:=logP2] %>% 
    .[,logP2:=NULL] %>% 
    .[,logP_opera:=NULL]
  
  repeat_cas <- new.rat[,.N,.(casrn)][N>1,casrn]
  new.rat[,notes:="none"] %>% 
    .[casrn %in% repeat_cas, notes:="repeat"] %>% # note repeated measures
    .[casrn %in% old.rat$CAS, notes:="replicate_chem_meas_v1.8"] %>% # note replicate measures
    .[,c("use_fup","use_clint") := FALSE] %>% 
    .[notes != "repeat" & !is.na(Funbound.plasma),use_fup:=TRUE] %>% #update fup, keep old clearance values (old values generally better measured/more conservative)
    .[notes == "none" & !is.na(clearance),use_clint := TRUE] %>% 
    .[casrn=="81-81-2" & Fub100 != 0.005, c("use_fup","use_clint") := TRUE] # note we keep old values for verapamil
  
  full.new.rat <- copy(new.rat)
  # library(httk)
  # current_httk_rat <- as.data.table(get_cheminfo(species="Rat",info = "all"))
  # current_chem.dt <- as.data.table(copy(chem.physical_and_invitro.data))
  # 
  # #save(new.rat,file=paste0(loc.wd,"/NewData/new_rat_12Jul2018.RData"))
  # 
  # rat.compare <- current_httk_rat[full.new.rat[, .(CAS = casrn,
  #                  update_fup = Funbound.plasma,
  #                  update_clint = clearance,
  #                  use_fup,
  #                  use_clint,
  #                  update_logP = logP,
  #                  update_MW = MW,
  #                  update_pKa_Donor = pKa_Donor,
  #                  update_pKa_Accept = pKa_Accept)], on = "CAS"]
  # 
  # write.xlsx(rat.compare,file="rat_compare.xlsx")
# 
#   plot(rat.compare[use_fup==T & use_clint==T,.(Rat.Funbound.plasma,update_fup)])
#   rat.compare[use_fup==T & use_clint==T,.(Rat.Clint,update_clint)]
#   
##### save new rat data #####
full.new.rat <- read_new_rat(loc.wd = loc.wd)
save(full.new.rat,file=paste0(loc.wd,"/NewData/full_new_rat_04Dec2018.RData"))


