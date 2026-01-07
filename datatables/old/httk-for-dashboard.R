library(httk)

# Clear the memory:
rm(list=ls())

#setwd("L:/Lab/NCCT_ExpoCast/ExpoCast2019/HTTKDataTable/")
# Default is 1000, but this is more stable:
NUM.SAMPLES <- 1e4

# All chemicals with in vitro data:
invitro.ids <- get_cheminfo(info="DTXSID")
# Pull in the ADMet Predictions from Sipes (2017):
load_sipes2017()
insilico.ids <- get_cheminfo(info="DTXSID")
insilico.ids <- insilico.ids[!is.na(insilico.ids)]
insilico.ids <- insilico.ids[!(insilico.ids %in% invitro.ids)]

dashboard.table <- get_cheminfo(info=c("DTXSID","Clint","Funbound.plasma"),
                     fup.lod.default = 0)
dashboard.table <- subset(dashboard.table,!is.na(DTXSID))
dashboard.table$Clint.Measured <- NA
dashboard.table$Funbound.plasma.Measured <- NA
dashboard.table$Clint.Predicted <- NA
dashboard.table$Funbound.plasma.Predicted <- NA
dashboard.table$Vd <- NA
dashboard.table$Days.to.Steady.State <- NA
dashboard.table$Half.Life <- NA
dashboard.table$Css.Med <- NA
dashboard.table$Css.95 <- NA
dashboard.table$MW <- NA



all.ids <- sort(unique(dashboard.table$DTXSID))
num.chems <- length(all.ids)
ids <- sort(unique(subset(dashboard.table,is.na(Css.Med))$DTXSID))
chem.info <- get_cheminfo(info=c("DTXSID","MW","CAS"))
for (this.id in ids)
  if (this.id %in% chem.info[["DTXSID"]] &
    is.na(dashboard.table[dashboard.table$DTXSID==this.id,"Css.Med"])) 
 {
   print(paste(this.id,"-",which(this.id==all.ids),"of",num.chems))
   this.info <- subset(chem.info,DTXSID==this.id)
   this.cas <- this.info[["CAS"]]
   if (this.id %in% invitro.ids) 
   {
     dashboard.table[dashboard.table$DTXSID==this.id,"Clint.Measured"] <-
       dashboard.table[dashboard.table$DTXSID==this.id,"Human.Clint"]
     dashboard.table[dashboard.table$DTXSID==this.id,"Funbound.plasma.Measured"] <-
       dashboard.table[dashboard.table$DTXSID==this.id,"Human.Funbound.plasma"]
   } else {
     dashboard.table[dashboard.table$DTXSID==this.id,"Clint.Predicted"] <-
       dashboard.table[dashboard.table$DTXSID==this.id,"Human.Clint"]
     dashboard.table[dashboard.table$DTXSID==this.id,"Funbound.plasma.Predicted"] <-
       dashboard.table[dashboard.table$DTXSID==this.id,"Human.Funbound.plasma"]
   } 
   if (dashboard.table[dashboard.table$DTXSID==this.id,"Human.Funbound.plasma"]>0)
   {
     dashboard.table[dashboard.table$DTXSID==this.id,"Vd"] <- try(calc_vdist(chem.cas=this.cas))
     dashboard.table[dashboard.table$DTXSID==this.id,"Half.Life"] <- 
       signif(log(2)/try(calc_elimination_rate(chem.cas=this.cas)),3)
     dashboard.table[dashboard.table$DTXSID==this.id,"Days.to.Steady.State"] <- 
       try(calc_css(chem.cas=this.cas)$the.day)
   }
   dashboard.table[dashboard.table$DTXSID==this.id,"MW"] <- this.info[["MW"]]
   set.seed(123456)
   dashboard.table[dashboard.table$DTXSID==this.id,
     c("Css.Med","Css.95")] <- 
     try(calc_mc_css(chem.cas=this.cas,
       which.quantile=c(0.5,0.95),
       samples=NUM.SAMPLES,
       output.units="mg/L"))
 }
 
# Drop the Wambaugh et al. (2019) confidence intervals:
dashboard.table$Human.Funbound.plasma <- 
  as.numeric(lapply(strsplit(dashboard.table$Human.Funbound.plasma,","),
    function(x) x[[1]]))
# Drop the Wambaugh et al. (2019) confidence intervals:
dashboard.table$Human.Clint <- 
  as.numeric(lapply(strsplit(dashboard.table$Human.Clint,","),
    function(x) x[[1]]))
# Drop the Wambaugh et al. (2019) confidence intervals:
dashboard.table$Funbound.plasma.Measured <- 
  as.numeric(lapply(strsplit(dashboard.table$Funbound.plasma.Measured,","),
    function(x) x[[1]]))
# Drop the Wambaugh et al. (2019) confidence intervals:
dashboard.table$Clint.Measured <- 
  as.numeric(lapply(strsplit(dashboard.table$Clint.Measured,","),
    function(x) x[[1]]))
 
write.table(
  dashboard.table,
  file=paste(
    "Dashboard-HTTK-v",
    sessionInfo()$otherPkgs$httk$Version,
    "-mgL-",Sys.Date(),
    ".txt",sep=""),
  row.names=F,
  quote=F,
  sep="\t")

# Columns:
# DTXSID: Chemical Identifier
#	Human.Clint, Dashboard field "In Vitro Intrisntic Hepatic Clearance"), uL/min/10^6 hepatocyhtes
# Human.Funbound.plasma, Dashboard field "Fraction Unbound in Human Plasma", unitless	
# Clint.Measured, not currently used, experimentally measured value
# Funbound.plasma.Measured, not currently used, experimentally measured value	
# Clint.Predicted, not currently used, in silico prediction from Sipes et al, (2016)
# Funbound.plasma.Predicted, not currently used, in silico prediction from Sipes et al, (2016)
# Vd, dashboard field "Volume of Distribution", L/kg
# Days.to.Steady.State, dashboard field "Days to Steady State", days	
# Half.Life, dashboard field "PK Half Life", hours
# Css.Med, not currently used, HTTK prediction of population median Css	
# Css.95, dashboard field "Human Steady-State Plasma Concentration", mg/L
# MW, not currently used, Molecular Weght, g/mol
#











