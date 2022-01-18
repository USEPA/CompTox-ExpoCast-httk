library(gdata)
library(ggplot2)
library(httk)
library(scales)

setwd("L:/Lab/NCCT_ExpoCast/ExpoCast2020/HTTKGestational")

MFdata <- read.xls("Aylward-MatFet.xlsx",stringsAsFactors=F)
MFdata.httk <- subset(MFdata,DTXSID %in% get_cheminfo(info="DTXSID",model="pbtk"))
MFdata.httk[MFdata.httk$Chemical.Category=="bromodiphenylethers",
  "Chemical.Category"] <- "Bromodiphenylethers"  
MFdata.httk[MFdata.httk$Chemical.Category=="organochlorine Pesticides",
  "Chemical.Category"] <- "Organochlorine Pesticides"  
  MFdata.httk[MFdata.httk$Chemical.Category=="polyaromatic hydrocarbons",
  "Chemical.Category"] <- "Polyaromatic Hydrocarbons"  
  
for (this.id in unique(MFdata.httk$DTXSID))
{
  out <- solve_fetal_pbtk(dtxsid=this.id,dose=0,daily.dose=1,doses.per.day=3)
  last.row <- dim(out)[1]
# The compartments below will have to be changed when using the maternal-fetal model:
  MFdata.httk[MFdata.httk$DTXSID==this.id,"Mat.pred"] <- out[last.row,"Cplasma"]
  MFdata.httk[MFdata.httk$DTXSID==this.id,"Fet.pred"] <- out[last.row,"Cfplasma"]
  MFdata.httk[MFdata.httk$DTXSID==this.id,"MFratio.pred"] <- 
    out[last.row,"Cplasma"]/out[last.row,"Cfplasma"]
}

colnames(MFdata.httk)[colnames(MFdata.httk) == 
  "infant.maternal.conc...Central.tendency..calculate.j.k..or.report.paired.result."] <-
  "MFratio"
colnames(MFdata.httk)[colnames(MFdata.httk) == 
  "PREFERRED_NAME"] <-
  "Chemical"
colnames(MFdata.httk)[colnames(MFdata.httk) == 
  "details.on.matrix.comparison...e.g...cord.blood.lipid..maternal.serum.lipid..or.cord.blood.wet.weight..maternal.whole.blood.wet.weight"] <-
  "Matrix"
MFdata.httk$MFratio <- as.numeric(MFdata.httk$MFratio)
MFdata.httk$Lower.bound <- as.numeric(MFdata.httk$Lower.bound)
MFdata.httk$Upper.bound <- as.numeric(MFdata.httk$Upper.bound)
MFdata.httk$Chemical <- as.factor(MFdata.httk$Chemical)  
MFdata.httk$Matrix <- as.factor(MFdata.httk$Chemical)  
MFdata.httk$Chemical.Category <- as.factor(MFdata.httk$Chemical.Category)  
  
scientific_10 <- function(x) {                                  
  out <- gsub("1e", "10^", scientific_format()(x))              
  out <- gsub("\\+","",out)                                     
  out <- gsub("10\\^01","10",out)                               
  out <- parse(text=gsub("10\\^00","1",out))                    
}  
  
Fig1  <- ggplot(data=MFdata.httk) +
  geom_segment(color="grey",aes(
    x=MFratio.pred,
    y=Lower.bound,
    xend=MFratio.pred,
    yend=Upper.bound))+    
  geom_point(aes(
    x=MFratio.pred,
    y=MFratio,
    shape=Chemical.Category,
    color=Chemical.Category),
    size=3)   +
    scale_shape_manual(values=c(15, 16,2, 23, 0, 1, 17, 5, 6))+ 
  xlim(0,3) +
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
    
print(Fig1)

MFdata.noreps <- NULL
for (this.id in unique(MFdata.httk$DTXSID))
{
  this.subset <- subset(MFdata.httk,DTXSID==this.id)
  this.row <- this.subset[1,]
  this.row$MFratio <- mean(this.subset$MFratio,na.rm=T)

  out <- calc_mc_css(chem.cas=this.cas,  #where is this.cas defined?
    which.quantile = c(0.5,0.975),
    invitro.mc.arg.list = list(
      adjusted.Funbound.plasma = TRUE, 
      poormetab = TRUE,
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
      adjusted.Funbound.plasma = TRUE, 
      poormetab = TRUE,
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
