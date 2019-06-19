library(httk)
setwd("L:/Lab/NCCT_ExpoCast/ExpoCast2019/HTTKDataTable/")

invitro.ids <- get_cheminfo(info="DTXSID")
# Pull in the ADMet Predictions from Sipes (2017):
load_sipes2017(load.image=T)
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

for (this.id in sort(unique(dashboard.table$DTXSID)))
 if (this.id %in% get_cheminfo(info="DTXSID")) 
 {
   set.seed(123456)
   print(this.id)
   this.cas <- subset(get_cheminfo(info=c("DTXSID","CAS")),DTXSID==this.id)[,1]
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
     dashboard.table[dashboard.table$DTXSID==this.id,"Vd"] <- calc_vdist(chem.cas=this.cas)
     dashboard.table[dashboard.table$DTXSID==this.id,"Half.Life"] <- 
       log(2)/calc_elimination_rate(chem.cas=this.cas)
     dashboard.table[dashboard.table$DTXSID==this.id,"Days.to.Steady.State"] <- 
       calc_css(chem.cas=this.cas)$the.day
   }
   dashboard.table[dashboard.table$DTXSID==this.id,
     c("Css.Med","Css.95")] <- 
     calc_mc_css(chem.cas=this.cas,
       which.quantile=c(0.5,0.95),
       output.units="mg/L")
 }
write.csv(dashboard.table,file=paste("Dashboard-HTTK-mgL-",Sys.Date(),".txt",sep=""))












