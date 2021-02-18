library(gdata)
library(ggplot2)
library(httk)
library(scales)

setwd("c:/users/jwambaug/git/httk-dev/work")

scientific_10 <- function(x) {                                  
  out <- gsub("1e", "10^", scientific_format()(x))              
  out <- gsub("\\+","",out)                                     
  out <- gsub("10\\^01","10",out)                               
  out <- parse(text=gsub("10\\^00","1",out))                    
}  


fup.table <- NULL
all.chems <-get_cheminfo(model="fetal_pbtk",info="all") 
for (this.chem in all.chems[,"CAS"])
{
  temp <- parameterize_fetal_pbtk(chem.cas=this.chem)
  this.row <- data.frame(DTXSID=all.chems[all.chems[,"CAS"]==this.chem,"DTXSID"],
    Compound=all.chems[all.chems[,"CAS"]==this.chem,"Compound"],
    CAS=this.chem,
    Fup.Mat.Pred = temp$Funbound.plasma,
    Fup.Neo.Pred = temp$Fraction_unbound_plasma_fetus
    )
  fup.table <- rbind(fup.table,this.row)
}


FigA  <- ggplot(data=fup.table) +
  geom_point(aes(
    x=Fup.Mat.Pred,
    y=Fup.Neo.Pred),
    size=3)   +
  geom_abline(slope=1, intercept=0) + 
  ylab(expression(paste("Predicted Neonate ",f[up]))) + 
  xlab(expression(paste(italic("In vitro")," Measured ",f[up]))) +
   scale_x_log10(label=scientific_10) +
   scale_y_log10(label=scientific_10) +
  theme_bw()  +
  theme( text  = element_text(size=14)) 
    
print(FigA) 



MFdata <- read.xls("Aylward-MatFet.xlsx",stringsAsFactors=F)
MFdata.httk <- subset(MFdata,DTXSID %in% get_cheminfo(info="DTXSID",model="pbtk"))
MFdata.httk[MFdata.httk$Chemical.Category=="bromodiphenylethers",
  "Chemical.Category"] <- "Bromodiphenylethers"  
MFdata.httk[MFdata.httk$Chemical.Category=="organochlorine Pesticides",
  "Chemical.Category"] <- "Organochlorine Pesticides"  
  MFdata.httk[MFdata.httk$Chemical.Category=="polyaromatic hydrocarbons",
  "Chemical.Category"] <- "Polyaromatic Hydrocarbons"  

colnames(MFdata.httk)[colnames(MFdata.httk) == 
  "infant.maternal.conc...Central.tendency..calculate.j.k..or.report.paired.result."] <-
  "MFratio"
colnames(MFdata.httk)[colnames(MFdata.httk) == 
  "PREFERRED_NAME"] <-
  "Chemical"
colnames(MFdata.httk)[colnames(MFdata.httk) == 
  "details.on.matrix.comparison...e.g...cord.blood.lipid..maternal.serum.lipid..or.cord.blood.wet.weight..maternal.whole.blood.wet.weight"] <-
  "Matrix"

# Format the columns:
MFdata.httk$MFratio <- as.numeric(MFdata.httk$MFratio)
MFdata.httk$Chemical <- as.factor(MFdata.httk$Chemical)  
MFdata.httk$Matrix <- as.factor(MFdata.httk$Chemical)  
MFdata.httk$Chemical.Category <- as.factor(MFdata.httk$Chemical.Category)  

colnames(MFdata.httk)[15] <- "infant"
colnames(MFdata.httk)[16] <- "maternal"
colnames(MFdata.httk)[17] <- "obs.units"

MFdata.httk$infant <- as.numeric(MFdata.httk$infant)
MFdata.httk$maternal <- as.numeric(MFdata.httk$maternal)
MFdata.httk$AVERAGE_MASS <- as.numeric(MFdata.httk$AVERAGE_MASS)



# Convert the units:
convert1.units <- c("ng/ml","ng/mL","ug/L","ug/l","ng/mL serum","ng/g",
  "ng/g wet wt.","ppb")

MFdata.httk[MFdata.httk$obs.units%in%convert1.units,"infant"] <- 
  MFdata.httk[MFdata.httk$obs.units%in%convert1.units,"infant"] / # ng/ml = ug/L
  MFdata.httk[MFdata.httk$obs.units%in%convert1.units,"AVERAGE_MASS"]  # ug/L -> uM
MFdata.httk[MFdata.httk$obs.units%in%convert1.units,"maternal"] <- 
  MFdata.httk[MFdata.httk$obs.units%in%convert1.units,"maternal"] / # ng/ml = ug/L
  MFdata.httk[MFdata.httk$obs.units%in%convert1.units,"AVERAGE_MASS"]  # ug/L -> uM
MFdata.httk[MFdata.httk$obs.units%in%convert1.units,"obs.units"] <- "uM" 
  
convert2.units <- c("mg/L","ppm")

MFdata.httk[MFdata.httk$obs.units%in%convert2.units,"infant"] <- 
  MFdata.httk[MFdata.httk$obs.units%in%convert2.units,"infant"] * 1000 / # mg/L = ug/L
  MFdata.httk[MFdata.httk$obs.units%in%convert2.units,"AVERAGE_MASS"]  # ug/L -> uM
MFdata.httk[MFdata.httk$obs.units%in%convert2.units,"maternal"] <- 
  MFdata.httk[MFdata.httk$obs.units%in%convert2.units,"maternal"]* 1000 / # mg/L = ug/L
  MFdata.httk[MFdata.httk$obs.units%in%convert2.units,"AVERAGE_MASS"]  # ug/L -> uM
MFdata.httk[MFdata.httk$obs.units%in%convert2.units,"obs.units"] <- "uM" 
  


# Make the HTTK Predictions:
  
for (this.id in unique(MFdata.httk$DTXSID))
{
  p <- parameterize_fetal_pbtk(dtxsid=this.id,
    fetal_fup_adjustment =TRUE)
  out <- solve_fetal_pbtk(
    parameters=p,
    dose=0,
    daily.dose=1,
    doses.per.day=3,
    output.units = "uM")
  last.row <- which(out[,"time"]>279) # The whole final day
# The compartments below will have to be changed when using the maternal-fetal model:
  MFdata.httk[MFdata.httk$DTXSID==this.id,"Mat.pred"] <- mean(out[last.row,"Cplasma"])
  MFdata.httk[MFdata.httk$DTXSID==this.id,"Fet.pred"] <- mean(out[last.row,"Cfplasma"])
  MFdata.httk[MFdata.httk$DTXSID==this.id,"MFratio.pred"] <- 
    mean(out[last.row,"Cplasma"])/mean(out[last.row,"Cfplasma"])
  p <- parameterize_fetal_pbtk(dtxsid=this.id,
    fetal_fup_adjustment =FALSE)
  out <- solve_fetal_pbtk(
    parameters=p,
    dose=0,
    daily.dose=1,
    doses.per.day=3)
  last.row <- which(out[,"time"]>279) # The whole final day
# The compartments below will have to be changed when using the maternal-fetal model:
  MFdata.httk[MFdata.httk$DTXSID==this.id,"Mat.pred.nofup"] <- mean(out[last.row,"Cplasma"])
  MFdata.httk[MFdata.httk$DTXSID==this.id,"Fet.pred.nofup"] <- mean(out[last.row,"Cfplasma"])
  MFdata.httk[MFdata.httk$DTXSID==this.id,"MFratio.pred.nofup"] <- 
    mean(out[last.row,"Cplasma"])/mean(out[last.row,"Cfplasma"])
}






# Something is wrong with cotinine:
MFdata.httk <- subset(MFdata.httk,Chemical!="Cotinine")

Fig0a  <- ggplot(data=subset(MFdata.httk,obs.units=="uM")) +
  geom_point(aes(
    x=Fet.pred,
    y=infant,
    shape=Chemical.Category,
    color=Chemical.Category),
    size=3)   +
  scale_shape_manual(values=c(15, 16,2, 23, 0, 1, 17, 5, 6))+ 
  geom_abline(slope=1, intercept=0) + 
  ylab(expression(paste(italic("In vivo")," Infant Plasma Conc. (uM))"))) + 
  xlab(expression(paste(italic("In vitro")," Predicted Conc. (uM)"))) +
  theme_bw()  +
  theme(legend.position="bottom")+
  theme( text  = element_text(size=14))+ 
  theme(legend.text=element_text(size=10))+ 
  guides(color=guide_legend(title="Class",nrow=3,byrow=TRUE))+ 
  guides(shape=guide_legend(title="Class",nrow=3,byrow=TRUE))
    
print(Fig0a)  

Fig0b  <- ggplot(data=subset(MFdata.httk,obs.units=="uM")) +
  geom_point(aes(
    x=Mat.pred,
    y=maternal,
    shape=Chemical.Category,
    color=Chemical.Category),
    size=3)   +
  scale_shape_manual(values=c(15, 16,2, 23, 0, 1, 17, 5, 6))+ 
  geom_abline(slope=1, intercept=0) + 
  ylab(expression(paste(italic("In vivo")," Maternal Plasma Conc. (uM))"))) + 
  xlab(expression(paste(italic("In vitro")," Predicted Conc. (uM)"))) +
  theme_bw()  +
  theme(legend.position="bottom")+
  theme( text  = element_text(size=14))+ 
  theme(legend.text=element_text(size=10))+ 
  guides(color=guide_legend(title="Class",nrow=3,byrow=TRUE))+ 
  guides(shape=guide_legend(title="Class",nrow=3,byrow=TRUE))
    
print(Fig0b)  

# Hmm, not sure this really makes sense because we don't know when the exposures occured, lets see how we do for ratio:
  
MFdata.main <- NULL
MFdata.outliers <- NULL
for (this.id in unique(MFdata.httk$DTXSID))
{
  this.subset <- subset(MFdata.httk,DTXSID==this.id)
  this.row <- this.subset[1,]
  this.row$MFratio <- median(this.subset$MFratio)
  this.row$MFratio.Q25 <- quantile(this.subset$MFratio,0.25)
  this.row$MFratio.Q75 <- quantile(this.subset$MFratio,0.75)
  MFdata.main <- rbind(MFdata.main,this.row)
  this.subset <- subset(this.subset,
    MFratio<this.row$MFratio.Q25 |
    MFratio>this.row$MFratio.Q75)
  MFdata.outliers <- rbind(MFdata.outliers,this.subset) 
}
  
  
  
  
Fig1b  <- ggplot(data=MFdata.main) +
  geom_segment(color="grey",aes(
    x=MFratio.pred,
    y=MFratio.Q25,
    xend=MFratio.pred,
    yend=MFratio.Q75))+    
  geom_point(aes(
    x=MFratio.pred,
    y=MFratio,
    shape=Chemical.Category,
    color=Chemical.Category),
    size=3)   +
  scale_shape_manual(values=c(15, 16,2, 23, 0, 1, 17, 5, 6))+ 
  geom_point(data=MFdata.outliers,aes(
    x=MFratio.pred,
    y=MFratio,
    shape=Chemical.Category,
    color=Chemical.Category),
    size=1)   +
  xlim(0,2) +
  ylim(0,3) +
#   geom_text(aes(x=AUC,y=Critical.concentration,label=Compound.abbrev,color=Chemical)) +
#   scale_y_log10(label=scientific_10) +
#,limits=c(10^-7,100)) +
#   scale_x_log10(label=scientific_10) +
#   ,limits=c(10^-7,100)) +
#    annotation_logticks() + 
  geom_abline(slope=1, intercept=0) + 
#    geom_abline(slope=1, intercept=1,linetype="dashed") + 
#    geom_abline(slope=1, intercept=-1,linetype="dashed") + 
  ylab(expression(paste(italic("In vivo")," Maternal:Fetal Plasma Ratio"))) + 
  xlab(expression(paste(italic("In vitro")," Predicted Ratio"))) +
#    scale_color_brewer(palette="Set2") + 
  theme_bw()  +
  theme(legend.position="bottom")+
  theme( text  = element_text(size=14))+ 
  theme(legend.text=element_text(size=10))+ 
  guides(color=guide_legend(title="Class",nrow=3,byrow=TRUE))+ 
  guides(shape=guide_legend(title="Class",nrow=3,byrow=TRUE))
    
print(Fig1b)

fit1 <- lm(data=MFdata.main,MFratio~MFratio.pred)
summary(fit1)




TKstats <- read.xls("Dallmann-2018.xlsx",stringsAsFactors=F,skip=1)
TKstats <- subset(TKstats,DTXSID!="")

for (this.id in unique(TKstats$DTXSID))
{
  if (any(regexpr("ng",TKstats[TKstats$DTXSID==this.id,"X.unit."])!=-1))
  {
    TKstats[TKstats$DTXSID==this.id,"Observed"] <- 1e-6 *
      TKstats[TKstats$DTXSID==this.id,"Observed"]
    TKstats[TKstats$DTXSID==this.id,"Predicted"] <- 1e-6 *
      TKstats[TKstats$DTXSID==this.id,"Predicted"]
    TKstats[TKstats$DTXSID==this.id,"Observed.1"] <- 1e-6 *
      TKstats[TKstats$DTXSID==this.id,"Observed.1"]
    TKstats[TKstats$DTXSID==this.id,"Predicted.1"] <- 1e-6 *
      TKstats[TKstats$DTXSID==this.id,"Predicted.1"]  
    TKstats[TKstats$DTXSID==this.id,"X.unit."] <- 
      gsub("ng","mg",TKstats[TKstats$DTXSID==this.id,"X.unit."])  
  }  
  if (this.id %in% get_cheminfo(info="DTXSID",model="pbtk"))
  {
    this.subset <- subset(TKstats,DTXSID==this.id)
    out.nonpreg <- solve_pbtk(
      dtxsid=this.id,
      times = seq(0, 1*7, 0.5),
      dose=1,
      daily.dose=NULL)
    out.preg2 <- solve_fetal_pbtk(
      dtxsid=this.id,
      times = seq(13 * 7, 14 * 7, 0.5),
      dose=1,
      daily.dose=NULL)
    if (any(regexpr("AUC",this.subset$Parameter)!=-1))
    {
      TKstats[TKstats$DTXSID==this.id &
        regexpr("AUC",TKstats$Parameter)!=-1, 
        "Predicted.httk"] <- max(out.nonpreg[,"AUC"])                       
      TKstats[TKstats$DTXSID==this.id &
        regexpr("AUC",TKstats$Parameter)!=-1, 
        "Predicted.1.httk"] <- max(out.preg[,"AUC_fetus"])        
    }
    if (any(regexpr("Cmax",this.subset$Parameter)!=-1))
    {
      TKstats[TKstats$DTXSID==this.id &
        regexpr("Cmax",TKstats$Parameter)!=-1, 
        "Predicted.httk"] <- max(out.nonpreg[,"Cplasma"])
      TKstats[TKstats$DTXSID==this.id &
        regexpr("Cmax",TKstats$Parameter)!=-1, 
        "Predicted.1.httk"] <- max(out.preg[,"Cplasma"])        
    }
  }
}

TKstats$Ratio.obs <- TKstats$Observed / TKstats$Observed.1
TKstats$Ratio.httk <- TKstats$Predicted.httk / TKstats$Predicted.1.httk



Fig2a  <- ggplot(data=TKstats) +
  geom_point(aes(
    y=Observed,
    x=Predicted.httk,
    shape=Parameter,
    color=Parameter),
    size=3)   +
  scale_shape_manual(values=c(15, 16,2, 23, 0, 1, 17, 5, 6))+ 
  xlab("httk Predicted (mg/L or mg/L*h)") + 
  ylab("Non-Pregnant Observed (Dallmann, 2018)") +
  scale_x_log10() +
  scale_y_log10() +
  theme_bw()  +
  theme(legend.position="bottom")+
  theme( text  = element_text(size=14))+ 
  theme(legend.text=element_text(size=10))+ 
  guides(color=guide_legend(title="Class",nrow=3,byrow=TRUE))+ 
  guides(shape=guide_legend(title="Class",nrow=3,byrow=TRUE))
    
print(Fig2a)

Fig2b  <- ggplot(data=TKstats) +
  geom_point(aes(
    y=Observed.1,
    x=Predicted.1.httk,
    shape=Parameter,
    color=Parameter),
    size=3)   +
  scale_shape_manual(values=c(15, 16,2, 23, 0, 1, 17, 5, 6))+ 
  xlab("httk Predicted (mg/L or mg/L*h)") + 
  ylab("Pregnant Observed (Dallmann, 2018)") +
  scale_x_log10() +
  scale_y_log10() +
  theme_bw()  +
  theme(legend.position="bottom")+
  theme( text  = element_text(size=14))+ 
  theme(legend.text=element_text(size=10))+ 
  guides(color=guide_legend(title="Class",nrow=3,byrow=TRUE))+ 
  guides(shape=guide_legend(title="Class",nrow=3,byrow=TRUE))
    
print(Fig2b)

Fig2c  <- ggplot(data=TKstats) +
  geom_point(aes(
    y=Ratio.obs,
    x=Ratio.httk,
    shape=Parameter,
    color=Parameter),
    size=3)   +
  scale_shape_manual(values=c(15, 16,2, 23, 0, 1, 17, 5, 6))+ 
  xlab("Non-Pregnant:Pregnant Ratio httk Predicted") + 
  ylab("Non-Pregnant:Pregnant Observed (Dallmann, 2018)") +
#  scale_x_log10() +
#  scale_y_log10() +
  theme_bw()  +
  theme(legend.position="bottom")+
  theme( text  = element_text(size=14))+ 
  theme(legend.text=element_text(size=10))+ 
  guides(color=guide_legend(title="Class",nrow=3,byrow=TRUE))+ 
  guides(shape=guide_legend(title="Class",nrow=3,byrow=TRUE))
    
print(Fig2c)



TKstats.obs <-TKstats[,c(1:8,11)]
TKstats.obs$Type <- "Observed"
colnames(TKstats.obs) <- c(
  "Drug", "DTXSID", "PREFERRED_NAME", "CASRN", "Parameter", "X.unit.", "Note",
  "Maternal", "Fetal", "Type")
TKstats.Dallmann <-TKstats[,c(1:7,9,12)]
TKstats.Dallmann$Type <- "Dallmann (2018)"
colnames(TKstats.Dallmann) <- c(
  "Drug", "DTXSID", "PREFERRED_NAME", "CASRN", "Parameter", "X.unit.", "Note",
  "Maternal", "Fetal", "Type")
TKstats.httk <-TKstats[,c(1:7,14:15)]
TKstats.httk$Type <- "httk-fetal"
colnames(TKstats.httk) <- c(
  "Drug", "DTXSID", "PREFERRED_NAME", "CASRN", "Parameter", "X.unit.", "Note",
  "Maternal", "Fetal", "Type")
TKstats2 <- rbind(TKstats.obs,TKstats.Dallmann,TKstats.httk)




Fig3  <- ggplot(data=TKstats2) +
  geom_point(aes(
    x=Maternal,
    y=Fetal,
    shape=Parameter,
    color=Type),
    size=3)   +
  scale_shape_manual(values=c(15, 16,2, 23, 0, 1, 17, 5, 6))+ 
  ylab("Fetal") + 
  xlab("Maternal") +
  scale_x_log10() +
  scale_y_log10() +
  theme_bw()  +
  theme(legend.position="bottom")+
  theme( text  = element_text(size=14))+ 
  theme(legend.text=element_text(size=10))+ 
  guides(color=guide_legend(title="Class",nrow=3,byrow=TRUE))+ 
  guides(shape=guide_legend(title="Class",nrow=3,byrow=TRUE))
    
print(Fig3)
















MFdata.noreps <- NULL
for (this.id in unique(MFdata.httk$DTXSID))
{
  this.subset <- subset(MFdata.httk,DTXSID==this.id)
  this.row <- this.subset[1,]
  this.row$MFratio <- mean(this.subset$MFratio,na.rm=T)

  out <- calc_mc_css(chem.cas=this.cas,  #where is this.cas defined?
    which.quantile = c(0.5,0.975),
    invitro.mc.arg.list = list(
      adjusted.Funbound.plasma = T, 
      poormetab = T,
      fup.censored.dist = FALSE, 
      fup.lod = 0.01, 
      fup.meas.cv = 0.4, 
      clint.meas.cv = 0.3,
      fup.pop.cv = 0.0, 
      clint.pop.cv = 0.0))
  this.row$Unc.invitro <- out[2]/out[1]
     
  MFdata.noreps <- rbind(MFdata.noreps,this.row)
}

Fig2 <- ggplot(data=MFdata.noreps)+
   geom_histogram(binwidth = 0.25,alpha=0.2,fill="Red",aes(MFratio))+ 
   geom_histogram(binwidth = 0.25,alpha=0.2,fill="Blue",aes(MFratio.pred))+ 
  xlab("Maternal:Fetal Plasma Ratio") +
  ylab("Number of chemicals")+
#   scale_x_log10(label=scientific_10,limits=c(5*10^-4,1.05))+
#   labs(title=paste("CI:",signif(median(Fig1.table$Error1,na.rm=T),2),"CV:",signif(median(Fig1.table$CV1,na.rm=T),1),"/ CI:",signif(median(Fig1.table$Error2,na.rm=T),1),"CV:",signif(median(Fig1.table$CV2,na.rm=T),1)))+
    theme_bw()+
    theme( text  = element_text(size=14))+
    annotate("text", x=2,y=6,size=14,label="Observed", color="red",alpha=0.5)+
    annotate("text", x=1.75,y=20,size=14,label="Predicted", color="blue",alpha=0.5)
    

print(Fig2)

# Natural scale RMSE:
RMSE <- mean((MFdata.noreps$MFratio-MFdata.noreps$MFratio.pred)^2)^(1/2)

# For HTTK pop we use log scale:
RMSE.log <- mean((log10(MFdata.noreps$MFratio)-log10(MFdata.noreps$MFratio.pred))^2)^(1/2)
# Converrt to stddev
sigma <- dim(MFdata.noreps)[1]^(1/2)*RMSE.log
Unc.invitro <- mean(log10(MFdata.noreps$Unc.invitro))

Wangchems <- read.xls("Wang2018.xlsx",stringsAsFactors=F,sheet=3,skip=2)
maternal.list <- Wangchems$CASRN[Wangchems$CASRN%in%get_cheminfo(model="pbtk")]

pred.table1 <- subset(get_cheminfo(info=c("Compound","CAS","DTXSID")),
  CAS %in% maternal.list)
  
for (this.cas in maternal.list)
{
  out <- solve_fetal_pbtk(chem.cas=this.cas,dose=0,daily.dose=1,doses.per.day=3)
  last.row <- dim(out)[1]
  pred.table1[pred.table1$CAS==this.cas,"MFratio.pred"] <- 
    out[last.row,"Cplasma"]/out[last.row,"Cfplasma"] 
}

pred.table1$Uncertainty <- "Fetal Plasma"

pred.table2 <- pred.table1
pred.table2$Uncertainty <- "In Vitro Measurement"
for (this.cas in maternal.list)
{
  out <- calc_mc_css(chem.cas=this.cas,
    which.quantile = c(0.5,0.975),
    invitro.mc.arg.list = list(
      adjusted.Funbound.plasma = T, 
      poormetab = T,
      fup.censored.dist = FALSE, 
      fup.lod = 0.01, 
      fup.meas.cv = 0.4, 
      clint.meas.cv = 0.3,
      fup.pop.cv = 0.0, 
      clint.pop.cv = 0.0))
  pred.table2[pred.table2$CAS==this.cas,"MFratio.pred"] <- 
    pred.table1[pred.table1$CAS==this.cas,"MFratio.pred"]*out[2]/out[1] 
}

pred.table3 <- pred.table1
pred.table3$Uncertainty <- "Plasma Error (Fig. 1)"
empirical.error <- 10^(sigma^2-Unc.invitro^2)^(1/2)
for (this.cas in maternal.list)
{

  pred.table3[pred.table3$CAS==this.cas,"MFratio.pred"] <- 
    pred.table2[pred.table2$CAS==this.cas,"MFratio.pred"]*empirical.error 
}

pred.table4 <- pred.table1
pred.table4$Uncertainty <- "Fetal Brain"
for (this.cas in maternal.list)
{
  Kbrain2pu <- predict_partitioning_schmitt(chem.cas=this.cas)$Kbrain2pu
  out <- parameterize_pbtk(chem.cas=this.cas) 
  Rb2p <- out$Rblood2plasma
  fup <- out$Funbound.plasma
  pred.table4[pred.table4$CAS==this.cas,"MFratio.pred"] <- 
    pred.table3[pred.table3$CAS==this.cas,"MFratio.pred"]*
    Kbrain2pu * fup / Rb2p
}

pred.table5 <- pred.table1
pred.table5$Uncertainty <- "Brain Error"
for (this.cas in maternal.list)
{                                             
  Kbrain2pu <- predict_partitioning_schmitt(chem.cas=this.cas)$Kbrain2pu
# From Pearce et al. (2017) PC paper:
  Kbrain2pu <- 10^(log10(Kbrain2pu)+0.647)
  out <- parameterize_pbtk(chem.cas=this.cas) 
  Rb2p <- out$Rblood2plasma
  fup <- out$Funbound.plasma
  pred.table5[pred.table5$CAS==this.cas,"MFratio.pred"] <- 
    pred.table3[pred.table3$CAS==this.cas,"MFratio.pred"]*
    Kbrain2pu * fup / Rb2p
}



pred.levels <- pred.table5$Compound[order(pred.table5$MFratio.pred)]

pred.table <- rbind(pred.table1,pred.table2,pred.table3,pred.table4,pred.table5)
pred.table$Compound <- factor(pred.table$Compound,
  levels = pred.levels)


Fig3  <- ggplot(data=pred.table) +
  geom_point(aes(
    x=MFratio.pred,
    y=Compound,
    color = Uncertainty,
    shape = Uncertainty),
    size=3)   +
    scale_shape_manual(values=c(15, 16,2, 23, 0, 1, 17, 5, 6))+ 
  scale_x_log10(limits=c(0.8,10^3))+
  ylab(expression(paste(
    "Chemicals Found in Maternal Plasma by Wang   ",italic("et al.")," (2018)"))) + 
  xlab("Predicted Ratio to Maternal Plasma") +
  theme_bw()  +
  theme(legend.position="bottom")+
  theme( text  = element_text(size=14))+ 
  theme(legend.text=element_text(size=10))+ 
  guides(color=guide_legend(nrow=3,byrow=TRUE))+ 
  guides(shape=guide_legend(nrow=3,byrow=TRUE))
    
print(Fig3)
