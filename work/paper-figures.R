#
#
# Setup
#
#
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

RMSE <- function(x)
{
  mean(x$residuals^2)^(1/2)
}


#
#
# PRotein Binding FIgures
#
#

fup.table <- NULL
all.chems <-get_cheminfo(model="fetal_pbtk",info="all") 
# Get rid of median fup 0:
all.chems <- subset(all.chems,
  as.numeric(unlist(lapply(strsplit(
    all.chems$Human.Funbound.plasma,","),function(x) x[[1]])))!=0)
for (this.chem in all.chems[,"CAS"])
{
  temp <- parameterize_fetal_pbtk(chem.cas=this.chem)
  state <- calc_ionization(
      pH=7.26,
      pKa_Donor=temp$pKa_Donor,
      pKa_Accept=temp$pKa_Accept)
  if (state$fraction_positive > 0.5) this.charge <- "Positive"
  else if (state$fraction_negative > 0.5) this.charge <- "Negative"
  else this.charge <- "Neutral"
  this.row <- data.frame(DTXSID=all.chems[all.chems[,"CAS"]==this.chem,"DTXSID"],
    Compound=all.chems[all.chems[,"CAS"]==this.chem,"Compound"],
    CAS=this.chem,
    Fup.Mat.Pred = temp$Funbound.plasma,
    Fup.Neo.Pred = temp$Fraction_unbound_plasma_fetus,
    Charge = this.charge
    )
  fup.table <- rbind(fup.table,this.row)
}

fup.table[fup.table$Charge=="Positive","Charge"] <- paste("Positive (n=",
  sum(fup.table$Charge=="Positive"),
  ")",sep="")
fup.table[fup.table$Charge=="Negative","Charge"] <- paste("Negative (n=",
  sum(fup.table$Charge=="Negative"),
  ")",sep="")
fup.table[fup.table$Charge=="Neutral","Charge"] <- paste("Neutral (n=",
  sum(fup.table$Charge=="Neutral"),
  ")",sep="")
  

FigA  <- ggplot(data=fup.table) +
  geom_point(alpha=0.25, aes(
    x=Fup.Mat.Pred,
    y=Fup.Neo.Pred,
    shape=Charge,
    color=Charge),
    size=3)   +
  geom_abline(slope=1, intercept=0) + 
  ylab(expression(paste("Predicted Neonate ",f[up]))) + 
  xlab(expression(paste(italic("In vitro")," Measured Adult ",f[up]))) +
   scale_x_log10(label=scientific_10) +
   scale_y_log10(label=scientific_10) +
  theme_bw()  +
  theme( text  = element_text(size=14)) 
    
print(FigA) 


#
#
# Aylward cord-blood data figures
#
#

MFdata <- read.xls("Aylward-MatFet.xlsx",stringsAsFactors=F)

cat(paste("summarized data from over 100 studies covering ",
  length(unique(MFdata$DTXSID)[!(unique(MFdata$DTXSID)%in%c("","-"))]),
  " unique chemicals structures\n",sep=""))
  

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
times <- sort(unique(c(seq(13 * 7, 40 * 7, 0.25),seq(278,280,.1))))
  
for (this.id in unique(MFdata.httk$DTXSID))
{
  p <- parameterize_fetal_pbtk(dtxsid=this.id,
    fetal_fup_adjustment =TRUE)
  if (!is.na(p$Funbound.plasma.dist))
  {
    if (as.numeric(strsplit(p$Funbound.plasma.dist,",")[[1]][3])>0.9 & 
      as.numeric(strsplit(p$Funbound.plasma.dist,",")[[1]][2])<0.11)
    {
      skip <- TRUE
    } else skip <- FALSE
  } else skip <- FALSE
  
  if (!skip)
  {  
    out <- solve_fetal_pbtk(
      parameters=p,
      dose=0,
      times=times,
      daily.dose=1,
      doses.per.day=3,
      output.units = "uM")
# The whole final day
    last.row <- which(out[,"time"]>279)
    last.row <- last.row[!duplicated(out[last.row,"time"])]
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
      times=times,
      daily.dose=1,
      doses.per.day=3,
      output.units = "uM",
      maxsteps=1e7)
      
    last.row <- which(out[,"time"]>279) # The whole final day
    last.row <- last.row[!duplicated(out[last.row,"time"])]
  # The compartments below will have to be changed when using the maternal-fetal model:
    MFdata.httk[MFdata.httk$DTXSID==this.id,"Mat.pred.nofup"] <- mean(out[last.row,"Cplasma"])
    MFdata.httk[MFdata.httk$DTXSID==this.id,"Fet.pred.nofup"] <- mean(out[last.row,"Cfplasma"])
    MFdata.httk[MFdata.httk$DTXSID==this.id,"MFratio.pred.nofup"] <- 
      mean(out[last.row,"Cplasma"])/mean(out[last.row,"Cfplasma"])
  }
  if (!is.nan(MFdata.httk[MFdata.httk$DTXSID==this.id,"Mat.pred"]) &
    is.nan(MFdata.httk[MFdata.httk$DTXSID==this.id,"Mat.pred.nofup"]))
    browser()
}

# Something is wrong with cotinine:
MFdata.httk <- subset(MFdata.httk,Chemical!="Cotinine")


# Aylward absolute comparison:
# Can't do an absolute scale prediction because we don't know the dose rate:
#
#FigBa  <- ggplot(data=subset(MFdata.httk,obs.units=="uM")) +
#  geom_point(aes(
#    x=Fet.pred,
#    y=infant,
#    shape=Chemical.Category,
#    color=Chemical.Category),
#    size=3)   +
#  scale_shape_manual(values=c(15, 16,2, 23, 0, 1, 17, 5, 6))+ 
#  geom_abline(slope=1, intercept=0) + 
#  ylab(expression(paste(italic("In vivo")," Infant Plasma Conc. (uM))"))) + 
#  xlab(expression(paste(italic("In vitro")," Predicted Conc. (uM)"))) +
#  theme_bw()  +
#  theme(legend.position="bottom")+
#  theme( text  = element_text(size=14))+ 
#  theme(legend.text=element_text(size=10))+ 
#  guides(color=guide_legend(title="Class",nrow=3,byrow=TRUE))+ 
#  guides(shape=guide_legend(title="Class",nrow=3,byrow=TRUE))
#    
#print(FigBa)  
#
#FigBb  <- ggplot(data=subset(MFdata.httk,obs.units=="uM")) +
#  geom_point(aes(
#    x=Mat.pred,
#    y=maternal,
#    shape=Chemical.Category,
#    color=Chemical.Category),
#    size=3)   +
#  scale_shape_manual(values=c(15, 16,2, 23, 0, 1, 17, 5, 6))+ 
#  geom_abline(slope=1, intercept=0) + 
#  ylab(expression(paste(italic("In vivo")," Maternal Plasma Conc. (uM))"))) + 
#  xlab(expression(paste(italic("In vitro")," Predicted Conc. (uM)"))) +
#  theme_bw()  +
#  theme(legend.position="bottom")+
#  theme( text  = element_text(size=14))+ 
#  theme(legend.text=element_text(size=10))+ 
#  guides(color=guide_legend(title="Class",nrow=3,byrow=TRUE))+ 
#  guides(shape=guide_legend(title="Class",nrow=3,byrow=TRUE))
#    
#print(FigBb)  

  
max.chem <- MFdata.httk[which(MFdata.httk$MFratio==max(MFdata.httk$MFratio)),]
min.chem <- MFdata.httk[which(MFdata.httk$MFratio==min(MFdata.httk$MFratio)),]
cat(paste("The minimum observed ratio was ",
  signif(min.chem[,"MFratio"],2),
  " for ",
  min.chem[,"Chemical"],
  " and the maximum was ",
  signif(max.chem[,"MFratio"],2),
  " for ",
  max.chem[,"Chemical"],
  ".\n",sep=""))


# Aylward Ratio figures:

# Clean up repeated observations:
MFdata.main <- NULL
MFdata.outliers <- NULL
for (this.id in unique(MFdata.httk$DTXSID))
{
  this.subset <- subset(MFdata.httk,DTXSID==this.id)
  this.row <- this.subset[1,]
  this.row$N.obs <- dim(this.subset)[1]
  this.row$MFratio <- median(this.subset$MFratio)
  this.row$MFratio.Q25 <- quantile(this.subset$MFratio,0.25)
  this.row$MFratio.Q75 <- quantile(this.subset$MFratio,0.75)
  MFdata.main <- rbind(MFdata.main,this.row)
  this.subset <- subset(this.subset,
    MFratio<this.row$MFratio.Q25 |
    MFratio>this.row$MFratio.Q75)
  MFdata.outliers <- rbind(MFdata.outliers,this.subset) 
}

FigCa  <- ggplot(data=MFdata.main) +
  geom_segment(color="grey",aes(
    x=MFratio.pred.nofup,
    y=MFratio.Q25,
    xend=MFratio.pred.nofup,
    yend=MFratio.Q75))+    
  geom_point(aes(
    x=MFratio.pred.nofup,
    y=MFratio,
    shape=Chemical.Category,
    color=Chemical.Category),
    size=3)   +
  scale_shape_manual(values=c(15, 16,2, 23, 0, 1, 17, 5, 6))+ 
  geom_point(data=MFdata.outliers,aes(
    x=MFratio.pred.nofup,
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
    
print(FigCa)

cat(paste("In Figure 4 we compare predictions made with our high-throughput \
human gestational PBTK model with experimental observations on a per chemical \
basis wherever we had both in vitro HTTK data and in vivo observations (",
length(unique(MFdata.main$DTXSID)),
" chemicals).\n",sep=""))


repeats <- subset(MFdata.main,N.obs>1)

cat(paste("Multiple observations were available for ",
  dim(repeats)[1],
  " of the chemicals,\n",sep=""))


max.chem <- repeats[which(repeats$MFratio==max(repeats$MFratio)),]
min.chem <- repeats[which(repeats$MFratio==min(repeats$MFratio)),]

cat(paste("However, among the chemicals with repeated observations, the median \
observations only ranged from ",
  signif(min.chem[,"MFratio"],2),
  " for ",
  min.chem[,"Chemical"],
  " to ",
  signif(max.chem[,"MFratio"],2),
  " for ",
  max.chem[,"Chemical"],
  ".\n",sep=""))
  
max.chem <- MFdata.main[which(MFdata.main$MFratio.pred==max(MFdata.main$MFratio.pred,na.rm=T)),]
min.chem <- MFdata.main[which(MFdata.main$MFratio.pred==min(MFdata.main$MFratio.pred,na.rm=T)),]

cat(paste("The predictions for all chemicals ranged from ",
  signif(min.chem[,"MFratio.pred"],2),
  " for ",
  min.chem[,"Chemical"],
  " to ",
  signif(max.chem[,"MFratio.pred"],2),
  " for ",
  max.chem[,"Chemical"],
  ".\n",sep=""))
  
    

fit1 <- lm(data=MFdata.main,MFratio~MFratio.pred.nofup)
summary(fit1)
RMSE(fit1)

fit1sub <- lm(data=subset(MFdata.main,
  !(Chemical.Category %in% c(
    "Fluorinated compounds",
    "Polyaromatic Hydrocarbons"))),
  MFratio~MFratio.pred.nofup)
summary(fit1sub)
RMSE(fit1sub)

  
# Mean logHenry's law constant for PAH's:
mean(subset(chem.physical_and_invitro.data,DTXSID%in%subset(MFdata.main,Chemical.Category=="Polyaromatic Hydrocarbons")$DTXSID)$logHenry)

nonvols <- subset(chem.physical_and_invitro.data,logHenry < -4.5)$DTXSID
fluoros <- chem.physical_and_invitro.data$DTXSID[regexpr("fluoro",tolower(chem.physical_and_invitro.data$Compound))!=-1]

cat(paste("When volatile and fluorinated chemicals are omitted only ",
  dim(subset(MFdata.main,
  !(Chemical.Category %in% c(
    "Fluorinated compounds",
    "Polyaromatic Hydrocarbons"))))[1],
  " evaluation chemicals remain\n",
  sep=""))
  
  
  
FigCb  <- ggplot(data=MFdata.main) +
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
    
print(FigCb)


fit2 <- lm(data=MFdata.main,MFratio~MFratio.pred)
summary(fit2)
RMSE(fit2)

fit2sub <- lm(data=subset(MFdata.main,
  !(Chemical.Category %in% c(
    "Fluorinated compounds",
    "Polyaromatic Hydrocarbons"))),
  MFratio~MFratio.pred)
summary(fit2sub)

RMSE(fit2sub)

cat(paste("We compare the RMSE for our predictions to the standard deviation \
of the observations ",
  signif(sd(MFdata.main$MFratio)[1],2),
  " (",
  signif(sd(subset(MFdata.main,
  !(Chemical.Category %in% c(
    "Fluorinated compounds",
    "Polyaromatic Hydrocarbons")))$MFratio),2),
  " for non PAH or fluorinated compounds).\n",sep=""))

cat(paste("The average standard deviation for chemicals with repeated observations was ",
  signif(sd(subset(MFdata.main,N.obs>1)$MFratio)[1],2),
  " (",
  signif(sd(subset(MFdata.main,
  N.obs > 1 &
  !(Chemical.Category %in% c(
    "Fluorinated compounds",
    "Polyaromatic Hydrocarbons")))$MFratio),2),
  " for non PAH or fluorinated compounds).\n",sep=""))

fit3 <- lm(data=repeats,MFratio~MFratio.pred.nofup)
summary(fit3)
RMSE(fit3)

fit3sub <- lm(data=subset(MFdata.main, N.obs > 1 &
  !(Chemical.Category %in% c(
    "Fluorinated compounds",
    "Polyaromatic Hydrocarbons"))),
  MFratio~MFratio.pred.nofup)
summary(fit3sub)


fit4 <- lm(data=subset(MFdata.main,N.obs > 1),MFratio~MFratio.pred)
summary(fit4)
RMSE(fit4)

fit4sub <- lm(data=subset(MFdata.main, N.obs > 1 &
  !(Chemical.Category %in% c(
    "Fluorinated compounds",
    "Polyaromatic Hydrocarbons"))),
  MFratio~MFratio.pred)
summary(fit4sub)

repeats <-subset(MFdata.main,N.obs > 1)
cat(paste("The RMSE of the predictions for the ",
  dim(subset(repeats,!(Chemical.Category %in% c(
    "Fluorinated compounds",
    "Polyaromatic Hydrocarbons"))))[1],
  " non-PAH and non-fluorinated compounds with repeated observations is ",
  signif(RMSE(fit4sub),2),
  " with the fup correction and ",
  signif(RMSE(fit3sub),2),
  " without.\n",sep=""))
  
  

#
#
# Maternal-Fetal Predictions acrosss HTTK:
#
#

times <- sort(unique(c(seq(13 * 7, 40 * 7, 0.25),seq(278,280,.1))))
  
MFratio.pred <- NULL
all.chems <- get_cheminfo(model="fetal_pbtk", info=c("Chemical","DTXSID"))
for (this.id in all.chems$DTXSID)
  if ((this.id %in% nonvols) & 
    !(this.id %in% fluoros))
{
  p <- parameterize_fetal_pbtk(dtxsid=this.id,
    fetal_fup_adjustment =TRUE)
  if (!is.na(p$Funbound.plasma.dist))
  {
    if (as.numeric(strsplit(p$Funbound.plasma.dist,",")[[1]][1])>0 &
      as.numeric(strsplit(p$Funbound.plasma.dist,",")[[1]][3])>0.9 & 
      as.numeric(strsplit(p$Funbound.plasma.dist,",")[[1]][2])<0.11)
    {
      skip <- TRUE
    } else skip <- FALSE
  } else skip <- FALSE
  
  if (!skip)
  {  
    out <- solve_fetal_pbtk(
      parameters=p,
      fetal_fup_adjustment =FALSE,
      dose=0,
      times=times,
      daily.dose=1,
      doses.per.day=3,
      output.units = "uM")
    last.row <- which(out[,"time"]>279)
    last.row <- last.row[!duplicated(out[last.row,"time"])]
    new.row <- data.frame(
      Chemical = all.chems[DTXSID==this.id,"Compound"],
      DTXSID = this.id,
      Mat.pred = mean(out[last.row,"Cplasma"]),
      Fet.pred = mean(out[last.row,"Cfplasma"]),
      MFratio.pred = mean(out[last.row,"Cplasma"])/mean(out[last.row,"Cfplasma"])
      )
    MFratio.pred <- rbind(MFratio.pred,new.row)
  }
}

FigD <- ggplot(data=MFratio.pred)+
   geom_histogram(binwidth = 0.05,fill="Red",aes(MFratio.pred))+ 
  xlab("Maternal:Fetal Plasma Concentration Ratio") +
  ylab("Number of chemicals")+
    theme_bw()+
    theme( text  = element_text(size=14))
    

print(FigD)

max.chem <- MFratio.pred[which(MFratio.pred$MFratio.pred==max(MFratio.pred$MFratio.pred,na.rm=T)),]
min.chem <- MFratio.pred[which(MFratio.pred$MFratio.pred==min(MFratio.pred$MFratio.pred,na.rm=T)),]
cat(paste("In Figure X we examine the ratios predicted for the ",
  dim(MFratio.pred)[1],
  " appropriate (non-volatile or PFAS) chemicals with measured HTTK data.\n",
  sep=""))


cat(paste("We observe a median value of ",
  signif(median(MFratio.pred$MFratio.pred,na.rm=T),3),
  " ranging from ",
  signif(min.chem[,"MFratio.pred"],3),
  " for ",
  min.chem[,"DTXSID"],
  " to ",
  signif(max.chem[,"MFratio.pred"],3),
  " for ",
  max.chem[,"DTXSID"],
  ".\n",sep=""))
  
# Check out phys-chem > 1.6, < 1:
highratio <- subset(chem.physical_and_invitro.data,DTXSID%in%subset(MFratio.pred,MFratio.pred>1.6)$DTXSID)
# all highly bound
highratio$Compound
apply(highratio,2,function(x) mean(as.numeric(x),na.rm=2))


lowratio <- subset(chem.physical_and_invitro.data,DTXSID%in%subset(MFratio.pred,MFratio.pred<0.9)$DTXSID)
# No obvious pattern




#
#
# Dallmann 2018 Comparison
#
#


TKstats <- read.xls("Dallmann-2018.xlsx",stringsAsFactors=F,skip=1)
TKstats <- subset(TKstats,DTXSID!="")

TKstats[TKstats$Drug=="Caffeine","Gestational.Age.Weeks"] <- 36
TKstats[TKstats$Drug=="Midazolam","Gestational.Age.Weeks"] <- 30
TKstats[TKstats$Drug=="Nifedipine","Gestational.Age.Weeks"] <- 32
TKstats[TKstats$Drug=="Metoprolol","Gestational.Age.Weeks"] <- 37
TKstats[TKstats$Drug=="Ondansetron","Gestational.Age.Weeks"] <- 39  # Prior to C-section
TKstats[TKstats$Drug=="Granisetron","Gestational.Age.Weeks"] <- 15
TKstats[TKstats$Drug=="Diazepam","Gestational.Age.Weeks"] <- 30 # Paper doesn't say
TKstats[TKstats$Drug=="Metronidazole","Gestational.Age.Weeks"] <- 39 # Prior to C-section

TKstats[TKstats$Drug=="Caffeine","NonPreg.Duration.Days"] <- 12/24
TKstats[TKstats$Drug=="Midazolam","NonPreg.Duration.Days"] <- 6/24
TKstats[TKstats$Drug=="Nifedipine","NonPreg.Duration.Days"] <- 24/24
TKstats[TKstats$Drug=="Metoprolol","NonPreg.Duration.Days"] <- 12/24
TKstats[TKstats$Drug=="Ondansetron","NonPreg.Duration.Days"] <- 8/24
TKstats[TKstats$Drug=="Granisetron","NonPreg.Duration.Days"] <- 24/24
TKstats[TKstats$Drug=="Diazepam","NonPreg.Duration.Days"] <- 24/24
TKstats[TKstats$Drug=="Metronidazole","NonPreg.Duration.Days"] <- 48/24

TKstats[TKstats$Drug=="Caffeine","Preg.Duration.Days"] <- 24/24
TKstats[TKstats$Drug=="Midazolam","Preg.Duration.Days"] <- 6/24
TKstats[TKstats$Drug=="Nifedipine","Preg.Duration.Days"] <- 8/24
TKstats[TKstats$Drug=="Metoprolol","Preg.Duration.Days"] <- 12/24
TKstats[TKstats$Drug=="Ondansetron","Preg.Duration.Days"] <- 8/24
TKstats[TKstats$Drug=="Granisetron","Preg.Duration.Days"] <- 24/24
TKstats[TKstats$Drug=="Diazepam","Preg.Duration.Days"] <- 9/24
TKstats[TKstats$Drug=="Metronidazole","Preg.Duration.Days"] <- 48/24


# do unit conversions:
ng.rows <- regexpr("ng",TKstats[,"X.unit."])!=-1

# Get the molecular weights:
TKstats$MW <- merge(
  TKstats[,c("Drug","DTXSID")],
  chem.physical_and_invitro.data[,c("DTXSID","MW")],by="DTXSID")$MW

# Assumed body weight (kg):
BW <- 61.103

TKstats
for (this.col in c("Observed","Predicted","Observed.1","Predicted.1"))
{
  TKstats[ng.rows,this.col] <- 1e-3 * # Is the paper wrong about units?
#  TKstats[ng.rows,this.col] <- 1e-6 *
    TKstats[ng.rows,this.col] # ng -> mg
# Normalize to 1 mg/kg dose:
  TKstats[TKstats$Note=="a",this.col] <- 
    TKstats[TKstats$Note=="a",this.col]/150*BW
  TKstats[TKstats$Note=="b",this.col] <- 
    TKstats[TKstats$Note=="b",this.col]/20*BW
  TKstats[TKstats$Note=="d",this.col] <- 
    TKstats[TKstats$Note=="d",this.col]/10*BW
# Covert mg/L -> uM:
  TKstats[,this.col] <- TKstats[,this.col]/TKstats$MW*1000
}  
TKstats
 

TKstats[,"X.unit."] <- 
  gsub("ng","mg",TKstats[,"X.unit."])  
TKstats[,"X.unit."] <- 
  gsub("mg/L","uM",TKstats[,"X.unit."])  




for (this.id in unique(TKstats$DTXSID))
{
  if (any(regexpr("ng",TKstats[TKstats$DTXSID==this.id,"X.unit."])!=-1))
  {
  }  
  if (this.id %in% get_cheminfo(info="DTXSID",model="pbtk"))
  {
    this.subset <- subset(TKstats,DTXSID==this.id)
    p <- parameterize_pbtk(dtxsid=this.id)
    p$hematocrit <- 0.39412 # Kapraun 2019 (unitless)
    p$Rblood2plasma <- calc_rblood2plasma(parameters=p)
    p$BW <- 61.103 # Kapraun 2019 (kg) 
    p$Qcardiacc <- 301.78 / p$BW^(3/4) # Kapraun 2019 (L/h/kg^3/4)
    out.nonpreg <- solve_pbtk(
      parameters=p,
      times = seq(0, this.subset[1,"NonPreg.Duration.Days"], 0.1),
      dose=1,
      daily.dose=NULL)
    out.preg <- solve_fetal_pbtk(
      dtxsid=this.id,
      times = seq(
        this.subset[1,"Gestational.Age.Weeks"]*7, 
        this.subset[1,"Gestational.Age.Weeks"]*7 + 
          this.subset[1,"Preg.Duration.Days"], 
        0.1),
      dose=1,
      daily.dose=NULL)
    if (any(regexpr("AUC",this.subset$Parameter)!=-1))
    {
      TKstats[TKstats$DTXSID==this.id &
        regexpr("AUC",TKstats$Parameter)!=-1, 
        "Predicted.httk"] <- max(out.nonpreg[,"AUC"])                       
      TKstats[TKstats$DTXSID==this.id &
        regexpr("AUC",TKstats$Parameter)!=-1, 
        "Predicted.1.httk"] <- max(out.preg[,"AUC"])        
    }
    if (any(regexpr("Cmax",this.subset$Parameter)!=-1))
    {
      TKstats[TKstats$DTXSID==this.id &
        regexpr("Cmax",TKstats$Parameter)!=-1, 
        "Predicted.httk"] <- max(out.nonpreg[,"Cplasma"])
      TKstats[TKstats$DTXSID==this.id &
        regexpr("Cmax",TKstats$Parameter)!=-1, 
        "Predicted.1.httk"] <- max(out.preg[,"Cfplasma"])        
    }
  }
}

TKstats$Ratio.obs <- TKstats$Observed / TKstats$Observed.1
TKstats$Ratio.httk <- TKstats$Predicted.httk / TKstats$Predicted.1.httk



FigEa  <- ggplot(data=subset(TKstats,Parameter=="AUCinf")) +
  geom_point(aes(
    y=Observed,
    x=Predicted.httk,
    shape=Drug,
    color=Drug),
    size=3)   +
  geom_abline(slope=1, intercept=0) +
  geom_abline(slope=1, intercept=1, linetype=3) + 
  geom_abline(slope=1, intercept=-1, linetype=3) + 
  scale_shape_manual(values=c(15, 16,2, 23, 0, 1, 17, 5, 6))+ 
  xlab("httk Predicted (uM*h)") + 
  ylab("Non-Pregnant Observed (Dallmann, 2018)") +
  scale_x_log10(limits=c(10^-2,10^2),label=scientific_10) +
  scale_y_log10(limits=c(10^-2,10^2),label=scientific_10) +
  theme_bw()  +
  theme(legend.position="bottom")+
  theme( text  = element_text(size=14))+ 
  theme(legend.text=element_text(size=10))
    
print(FigEa)

FigEb  <- ggplot(data=subset(TKstats,Parameter=="AUCinf")) +
  geom_point(aes(
    y=Observed.1,
    x=Predicted.1.httk,
    shape=Drug,
    color=Drug),
    size=3)   +
      geom_abline(slope=1, intercept=0) +
  geom_abline(slope=1, intercept=1, linetype=3) + 
  geom_abline(slope=1, intercept=-1, linetype=3) +
  scale_shape_manual(values=c(15, 16,2, 23, 0, 1, 17, 5, 6))+ 
  xlab("httk Predicted (uM*h)") + 
  ylab("Pregnant Observed (Dallmann, 2018)") +
  scale_x_log10(limits=c(10^-5,10^3),label=scientific_10) +
  scale_y_log10(limits=c(10^-5,10^3),label=scientific_10) +
  theme_bw()  +
  theme(legend.position="bottom")+
  theme( text  = element_text(size=14))+ 
  theme(legend.text=element_text(size=10))
    
print(FigEb)

FigEc  <- ggplot(data=subset(TKstats,Parameter=="AUCinf" &
  !is.na(Ratio.httk))) +
  geom_point(aes(
    y=Ratio.obs,
    x=Ratio.httk,
    shape=Drug,
    color=Drug),
    size=3)   +
  scale_shape_manual(values=c(15, 16,2, 23, 0, 1, 17, 5, 6))+ 
  xlab("Non-Pregnant:Pregnant Ratio httk Predicted") + 
  ylab("Non-Pregnant:Pregnant Observed (Dallmann, 2018)") +
  scale_x_continuous(limits=c(0.25,2.5)) +
  scale_y_continuous(limits=c(0.25,2.5)) +
  geom_abline(slope=1, intercept=0) +
  geom_abline(slope=1, intercept=1.15, linetype=3) + 
  geom_abline(slope=1, intercept=-1.15, linetype=3) + 
  theme_bw()  +
  theme(legend.position="bottom")+
  theme( text  = element_text(size=14))+ 
  theme(legend.text=element_text(size=10))
    
print(FigEc)

write.csv(subset(TKstats,Parameter=="AUCinf" &
  !is.na(Ratio.httk)),
  file="DallmanTable.txt")
  
a <- c(0.3/1.1,1.9/1.2,1.2/1.5,2.1/1.1,1.4/1.1,1.1/1.1)
mean(a)
10^(abs(log10(a)))
10^mean(abs(log10(a)))

#
#
# Curley 1969
#
#
Curley <- read.xls("Curley1969.xlsx",stringsAsFactors=F)
dim(Curley)
Curley.compounds <- Curley[1,4:13]
Curley <- Curley[4:47,]
colnames(Curley)[1] <- "Tissue"
colnames(Curley)[2] <- "N"
colnames(Curley)[3] <- "Stat"

Curley.pcs <- NULL
cord.blood <- subset(Curley, Tissue == "Cord Blood") 
for (this.tissue in unique(Curley$Tissue))
  if (this.tissue != "Cord Blood")
  {
    this.subset <- subset(Curley, Tissue == this.tissue)
    for (this.chemical in colnames(Curley)[4:13])
    {
      if (!is.na((as.numeric(subset(this.subset,Stat=="Mean")[,this.chemical]))) &
        !is.na((as.numeric(subset(cord.blood,Stat=="Mean")[,this.chemical]))))
      {
        this.row <- data.frame(
          Compound = Curley.compounds[,this.chemical],
          DTXSID = this.chemical,
          Tissue = this.tissue,
          PC = as.numeric(subset(this.subset,Stat=="Mean")[,this.chemical]) /
            as.numeric(subset(cord.blood,Stat=="Mean")[,this.chemical])
          )
        Curley.pcs <- rbind(Curley.pcs,this.row)
      } else if (!is.na((as.numeric(subset(this.subset,Stat=="Range")[,this.chemical]))) &
        !is.na((as.numeric(subset(cord.blood,Stat=="Mean")[,this.chemical]))))
      {
        this.row <- data.frame(
          Compound = Curley.compounds[,this.chemical],
          DTXSID = this.chemical,
          Tissue = this.tissue,
          PC = as.numeric(subset(this.subset,Stat=="Range")[,this.chemical]) /
            as.numeric(subset(cord.blood,Stat=="Mean")[,this.chemical])
          )
        Curley.pcs <- rbind(Curley.pcs,this.row)
      }
    }
  }
Curley.pcs[Curley.pcs$Tissue=="Lungs","Tissue"] <- "Lung"

for (this.chemical in unique(Curley.pcs$DTXSID))
  if (this.chemical %in% get_cheminfo(info="DTXSID",model="fetal_pbtk"))
  {
    this.subset <- subset(Curley.pcs,DTXSID==this.chemical)
    p <- parameterize_fetal_pbtk(dtxsid=this.chemical,
      fetal_fup_adjustment = FALSE)
    fetal.blood.pH <- 7.26   
    Fup <- p$Fraction_unbound_plasma_fetus
    fetal_schmitt_parms <- parameterize_schmitt(dtxsid=this.chemical)
    fetal_schmitt_parms$plasma.pH <- fetal.blood.pH
    fetal_schmitt_parms$Funbound.plasma <- Fup
    fetal_pcs <- predict_partitioning_schmitt(parameters = fetal_schmitt_parms)
    fetal_pcs.nocal <- predict_partitioning_schmitt(
      parameters = fetal_schmitt_parms,
      regression=FALSE)
    out <- solve_fetal_pbtk(
      dtxsid = this.chemical,
      fetal_fup_adjustment =FALSE)
    Rb2p <- out[dim(out)[1],"Rfblood2plasma"]
    for (this.tissue in this.subset$Tissue)
      if (tolower(this.tissue) %in% 
        unique(subset(tissue.data,Species=="Human")$Tissue))
      {
        Curley.pcs[Curley.pcs$DTXSID==this.chemical &
          Curley.pcs$Tissue == this.tissue, "HTTK.pred"] <-
          fetal_pcs[[paste("K",tolower(this.tissue),"2pu",sep="")]]*fup/Rb2p
        Curley.pcs[Curley.pcs$DTXSID==this.chemical &
          Curley.pcs$Tissue == this.tissue, "HTTK.pred.nocal"] <-
          fetal_pcs.nocal[[paste("K",tolower(this.tissue),"2pu",sep="")]]*fup/Rb2p
      } else {
       print(this.tissue)
      }  
  } else print(this.chemical)

FigFa  <- ggplot(data=subset(Curley.pcs,!is.na(HTTK.pred))) +
  geom_point(size=3,aes(
    y=PC,
    x=HTTK.pred,
    shape=Compound,
    color=Compound))   +
  geom_abline(slope=1, intercept=0) +
  geom_abline(slope=1, intercept=1, linetype=3) + 
  geom_abline(slope=1, intercept=-1, linetype=3) +
  scale_shape_manual(values=c(15, 16,2, 23, 0, 1, 17, 5, 6))+
  xlab("httk Predicted Tissue:Blood Partition Coefificent") + 
  ylab("Observed (Curley, 1969)") +
  scale_x_log10(label=scientific_10) +
  scale_y_log10(label=scientific_10) +
  theme_bw()  +
  theme(legend.position="bottom")+
  theme( text  = element_text(size=18))+ 
  theme(legend.text=element_text(size=14))+  
  guides(shape=guide_legend(nrow=3,byrow=TRUE))

print(FigFa)

fitFa <- lm(data=Curley.pcs,log10(PC)~log10(HTTK.pred))
RMSE(fitFa)
fitFb <- lm(data=Curley.pcs,log10(PC)~log10(HTTK.pred.nocal))
RMSE(fitFb)

FigFb  <- ggplot(data=subset(Curley.pcs,!is.na(HTTK.pred))) +
  geom_point(size=3, aes(
    y=PC,
    x=HTTK.pred,
    shape=Tissue,
    color=Tissue))   +
  geom_abline(slope=1, intercept=0) +
  geom_abline(slope=1, intercept=1, linetype=3) + 
  geom_abline(slope=1, intercept=-1, linetype=3) +
    scale_shape_manual(values=c(15, 16,2, 23, 0, 1, 17, 5, 6, 19))+
  xlab("httk Predicted Tissue:Blood Partition Coefificent") + 
  ylab("Observed (Curley, 1969)") +
  scale_x_log10(label=scientific_10) +
  scale_y_log10(label=scientific_10) +
  theme_bw()  +
  theme(legend.position="bottom")+
  theme( text  = element_text(size=18))+ 
  theme(legend.text=element_text(size=14)) +  
  guides(shape=guide_legend(nrow=3,byrow=TRUE))

print(FigFb)


#
#
# Wang 2018
#
#


Wangchems <- read.xls("Wang2018.xlsx",stringsAsFactors=F,sheet=3,skip=2)
maternal.list <- Wangchems$CASRN[Wangchems$CASRN%in%get_cheminfo(model="fetal_pbtk")]
nonvols <- subset(chem.physical_and_invitro.data,logHenry < -4.5)$CAS
nonfluoros <- chem.physical_and_invitro.data$CAS[
  regexpr("fluoro",tolower(chem.physical_and_invitro.data$Compound))==-1]
maternal.list <- maternal.list[maternal.list %in% intersect(nonvols,nonfluoros)]
  

pred.table1 <- subset(get_cheminfo(
  info=c("Compound","CAS","DTXSID","logP","pka_accept","pka_donor"),
  model="fetal_pbtk"),
  CAS %in% maternal.list)
pred.table1$Compound <- gsub("\"","",pred.table1$Compound)    

for (this.cas in maternal.list)
{
  p <- parameterize_fetal_pbtk(chem.cas=this.cas)
  if (!is.na(p$Funbound.plasma.dist))
  {
    if (as.numeric(strsplit(p$Funbound.plasma.dist,",")[[1]][3])>0.9 & 
      as.numeric(strsplit(p$Funbound.plasma.dist,",")[[1]][2])<0.1)
    {
      skip <- TRUE
    } else skip <- FALSE
  } else skip <- FALSE
  
  if (!skip)
  {
    out <- solve_fetal_pbtk(
      chem.cas=this.cas,
      dose=0,
      daily.dose=1,
      doses.per.day=3,
      fetal_fup_adjustenment=FALSE)
    last.row <- which(out[,"time"]>279)
    last.row <- last.row[!duplicated(out[last.row,"time"])]
    pred.table1[pred.table1$CAS==this.cas,"Ratio.pred"] <- 
      mean(out[last.row,"Cfplasma"])/mean(out[last.row,"Cplasma"]) 
  }
}
# Create final table holding all predicitons for paper:
PC.table <- pred.table1
colnames(PC.table)[colnames(PC.table)=="Ratio.pred"] <- "R.plasma.FtoM"
pred.table1$Uncertainty <- "Predicted F:M Plasma Ratio"

# In vitro error is included in Aylward eval:
#pred.table2 <- pred.table1
#pred.table2$Uncertainty <- "In Vitro Meas. Error"
#for (this.cas in maternal.list)
#{
#  p <- parameterize_fetal_pbtk(chem.cas=this.cas)
#  if (!is.na(p$Funbound.plasma.dist))
#  {
#    if (as.numeric(strsplit(p$Funbound.plasma.dist,",")[[1]][3])>0.9 & 
#      as.numeric(strsplit(p$Funbound.plasma.dist,",")[[1]][2])<0.1)
#    {
#      skip <- TRUE
#    } else skip <- FALSE
#  } else skip <- FALSE
#  
#  if (!skip)
#  {
#    out <- calc_mc_css(chem.cas=this.cas,
#      which.quantile = c(0.5,0.975),
#      httkpop=F,
##      invitro.mc.arg.list = list(
##        adjusted.Funbound.plasma = TRUE, 
##        poormetab = TRUE,
##        fup.censored.dist = FALSE, 
##        fup.lod = 0.01, 
##        fup.meas.cv = 0.4, 
##        clint.meas.cv = 0.3,
##        fup.pop.cv = 0.0, 
##        clint.pop.cv = 0.0)
#      )
#    pred.table2[pred.table2$CAS==this.cas,"MFratio.pred"] <- 
#      pred.table1[pred.table1$CAS==this.cas,"MFratio.pred"]*out[2]/out[1] 
#  }
#}

pred.table3 <- pred.table1
pred.table3$Uncertainty <- "Plasma Error (Fig. 4b)"
empirical.error <- RMSE(fit2sub)
for (this.cas in maternal.list)
{

  pred.table3[pred.table3$CAS==this.cas,"Ratio.pred"] <- 
#    pred.table2[pred.table2$CAS==this.cas,"MFratio.pred"]*(1+empirical.error) 
    1/((1/pred.table1[pred.table2$CAS==this.cas,"Ratio.pred"])*(1-1.96*empirical.error)) 
}
# Update final table for paper:
PC.table$RMSE <- RMSE(fit2sub)
PC.table$R.plasma.FtoM.upper <- pred.table3$Ratio.pred

pred.table4 <- pred.table1
pred.table4$Uncertainty <- "Fetal Brain Partioning"
for (this.cas in maternal.list)
{
  p <- parameterize_fetal_pbtk(chem.cas=this.cas,
      fetal_fup_adjustment=FALSE)
  Kbrain2pu <- p$Kfbrain2pu
  fup <- p$Fraction_unbound_plasma_fetus
#  out <- solve_fetal_pbtk(
#    chem.cas=this.cas,
#    dose=0,
#    daily.dose=1,
#    doses.per.day=3,
#    fetal_fup_adjustenment=FALSE)
#  Rb2p <- out[dim(out)[1],"Rfblood2plasma"]
  pred.table4[pred.table4$CAS==this.cas,"Ratio.pred"] <- 
    pred.table3[pred.table3$CAS==this.cas,"Ratio.pred"]*
    Kbrain2pu * fup
  PC.table[PC.table$CAS==this.cas,"Kbrain2pu"] <- Kbrain2pu
  PC.table[PC.table$CAS==this.cas,"fup"] <- fup
#  PC.table[PC.table$CAS==this.cas,"Rb2p"] <- Kbrain2pu
  PC.table[PC.table$CAS==this.cas,"R.brain.FtoM"] <- 
    pred.table4[pred.table4$CAS==this.cas,"Ratio.pred"]
}

pred.table5 <- pred.table1
pred.table5$Uncertainty <- "Brain Partitioning Error"
for (this.cas in maternal.list)
{                                             
  p <- parameterize_fetal_pbtk(chem.cas=this.cas,
      fetal_fup_adjustment=FALSE)
  Kbrain2pu <- p$Kfbrain2pu
  fup <- p$Fraction_unbound_plasma_fetus  
#  out <- solve_fetal_pbtk(
#    chem.cas=this.cas,
#    dose=0,
#    daily.dose=1,
#    doses.per.day=3,
#    fetal_fup_adjustenment=FALSE)
#  Rb2p <- out[dim(out)[1],"Rfblood2plasma"]
# From Pearce et al. (2017) PC paper:
  Kbrain2pu.upper <- Kbrain2pu*10^(1.96*0.647)
  quad.error <- ((RMSE(fit2sub) /
    pred.table1[pred.table1$CAS==this.cas,"Ratio.pred"])^2 +
    (log(10)*10^0.647)^2)^(1/2)
  pred.table5[pred.table5$CAS==this.cas,"Ratio.pred"] <- 
    pred.table1[pred.table1$CAS==this.cas,"Ratio.pred"]*Kbrain2pu*
    (1+1.96*quad.error)* fup 
  PC.table[PC.table$CAS==this.cas,"Kbrain2pu.upper"] <- Kbrain2pu.upper
  PC.table[PC.table$CAS==this.cas,"Quad.error"] <- quad.error
  PC.table[PC.table$CAS==this.cas,"R.brain.FtoM.upper"] <- 
    pred.table5[pred.table5$CAS==this.cas,"Ratio.pred"]
}



pred.levels <- pred.table5$Compound[order(pred.table5$Ratio.pred)]

pred.table <- rbind(
  pred.table1,
#  pred.table2,
  pred.table3,
  pred.table4,
  pred.table5)
pred.table$Compound <- factor(pred.table$Compound,
  levels = pred.levels)
  
pred.table$Uncertainty <- factor(pred.table$Uncertainty, 
  levels = c(pred.table1[1,"Uncertainty"],
#    pred.table2[1,"Uncertainty"],
    pred.table3[1,"Uncertainty"],
    pred.table4[1,"Uncertainty"],
    pred.table5[1,"Uncertainty"]))


#Wang 2018 confirmed 6 chemical identities:
confirmed.chemicals <- c(
  "2,4-Di-tert-butylphenol",
  "2,4-Dinitrophenol",
  "Pyrocatechol",
  "2'-Hydroxyacetophenone",
  "3,5-Di-tert-butylsalicylic acid",
  "4-Hydroxycoumarin"
  )
confirmed.chemicals <- c(
  "96-76-4",
  "19715-19-6",
  "51-28-5",
  "120-80-9",
  "118-93-4",
  "1076-38-6")


FigG  <- ggplot(data=pred.table) +
  geom_point(aes(
    x=Ratio.pred,
    y=Compound,
    color = Uncertainty,
    shape = Uncertainty),
    size=3)   +
    scale_shape_manual(values=c(15, 16,2, 23, 0, 1, 17, 5, 6))+ 
  scale_x_log10(limits=c(10^-2,10^3),label=scientific_10)+
  ylab(expression(paste(
    "Chemicals Found in Maternal Plasma by Wang   ",italic("et al.")," (2018)"))) + 
  xlab("Predicted Ratio to Maternal Plasma") +
  theme_bw()  +
#  theme(legend.position="bottom")+
  theme( text  = element_text(size=14))+ 
  theme(legend.text=element_text(size=10))#+ 
 # guides(color=guide_legend(nrow=4,byrow=TRUE))+ 
  #guides(shape=guide_legend(nrow=4,byrow=TRUE))
  #+
 # theme(legend.justification = c(0, 0), legend.position = c(0, 0))
    
print(FigG)

# Need to elaborate on difference between 2-tert-butylphenol and 2,4di-tert-butylphenol


for (this.col in 7:14)
  PC.table[,this.col] <- signif(PC.table[,this.col],3)

PC.table <- PC.table[order(PC.table$R.brain.FtoM.upper,decreasing=T),]

for (this.row in 1:dim(PC.table)[1])
{
  out <- calc_ionization(
    pH=7.26,
    pKa_Donor=PC.table[this.row,"pKa_Donor"],
    pKa_Accept=PC.table[this.row,"pKa_Accept"])
  if (out$fraction_neutral>0.9) PC.table[this.row,"Charge_726"] <- "Neutral"
  else if (out$fraction_positive>0.1) PC.table[this.row,"Charge_726"] <- 
    paste(signif(out$fraction_positive*100,2),"% Positive",sep="")
  else if (out$fraction_negative>0.1) PC.table[this.row,"Charge_726"] <- 
    paste(signif(out$fraction_negative*100,2),"% Negative",sep="")
  else if (out$fraction_zwitter>0.1) PC.table[this.row,"Charge_726"] <- 
    paste(signif(out$fraction_zwitter*100,2),"% Zwitterion",sep="")
}

PC.table <- PC.table[,c(
  "Compound",
  "CAS",
  "DTXSID",
  "logP",
  "Charge_726",
  "R.plasma.FtoM",
  "RMSE",
  "R.plasma.FtoM.upper",
  "Kbrain2pu",
  "fup",
  "R.brain.FtoM",
  "Kbrain2pu.upper",
  "R.brain.FtoM.upper")]
  
write.csv(PC.table,
  file="WangTable.txt",
  row.names=F)   

  
  
  
