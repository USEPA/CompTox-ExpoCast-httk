rm(list=ls())

### CALCULATING CSS FOR CHEMICALS, EVALUATING CL PREDICTION IMPACT ####
## Original script from John Wambaugh (June 2016)
## Modifcations from Brandall Ingle (January 2018, June 2018)
##Modifications by Dan Dawson (Septembmer 2019-January 2021)


#Note that the css calculations need the non-fraction unbound hepatic corrected version of Clint. So, in vitro data is pulled in
#from the training and test sets with Bin and not Bin.Adj. Likewise, the Bin medians are calculated from Bin, not Bin.Ad. The Adjuseted Bins
#predicted by the QSAR model are converted to median bin values for CSS by first taking the median values of the training set corresponding to the 
#bins. To approximately "uncorrect" them, they are they multipled by the median correction factor applied to each bin corresponding, uncorrected bin.  
#Also, in 

### SECTION 1: SETTING DIRECTORY, LOADING PROGRAMS, IMPORT FILES ###
writesuff="DED02022021"

library(httk)
library(gdata)
library(ggplot2)
library(scales)
library(reshape)


#Pull in predictions from Clearance QSARS and associate with the test set
##Best predictor sets are:
#Cl: The all predictor set with 50 predictors
#Fub: The all Y set with X predictors 
load(paste("Model_evaluation_testsets_withSeed1255_DED12152020.RData",sep=""))
CLpredsBin3=Cltestsetresults$TestSetPreds$a_3$PredToxCast

####Associate predicted values to test set
test=read.csv(paste("HepaticClearance/test2_a_ADfiltered_DED12152020.csv",sep=""))
test$CLpredsBin3=CLpredsBin3

#Pull in predictions from Fup QSAR
RF.Fup <- read.csv("AER/Full_fup_predictions_SqrtTransAll_30desc_020121_AD.csv")
RF.Fup=RF.Fup[-c(which(duplicated(RF.Fup$DTXSID))),]
RF.Fup=RF.Fup[RF.Fup$AD_out==0,] #Removal of 1 compound from AD of Fup
RF.Fup<-RF.Fup[which(RF.Fup$DTXSID%in%test$DTXSID),]
test=test[which(test$DTXSID%in%RF.Fup$DTXSID),]



#pull in training set data (for bin medians)
train=read.csv(paste("HepaticClearance/train_a_DED12152020.csv",sep="")) #Reads from two steps up the directory 
train <- train[,2:17]#Note, changed this to 1:16 from 1:12


#transfer bin values from train and test; 
train$Bin.Adj=ifelse(train$Bin.Adj==4,3,train$Bin.Adj)
train$Bin = ifelse(train$Bin==4,3, train$Bin)
test$Bin.Adj=ifelse(test$Bin.Adj==4,3,test$Bin.Adj)
test$Bin=ifelse(test$Bin==4,3,test$Bin)


### SECTION 2: COMBINING DATASETS, RENAMING BINS, ASSIGNING CHARGE ###

#pull chemicals from human.httk that are in the test
#pull data from httk (all our chemicals are in here)
human.httk.data <- get_cheminfo(info="All") #data from httk package
human.httk.data <- subset(human.httk.data, human.httk.data$CAS %in% test$ID)


#human.httk.data$order=seq(1,length(human.httk.data.complete[,1]))

###Human bins are merged in here
human.httk.data=merge(human.httk.data, test[,c("ID", "Bin", "Bin.Adj","Clearance", "Clearance.Adj" )], by.x="CAS", by.y="ID") 

#pull chemicals from test set that are in httk
test <- subset(test,test$ID %in% human.httk.data$CAS)
test$ID<-as.character(test$ID)

#sort both sets by CAS to make sure rows match
test <- test[order(test$ID),]
human.httk.data <- human.httk.data[order(human.httk.data$CAS),]
identical(human.httk.data$CAS, test$ID)

#Assign same names in test as in training 
test[test$Bin.Adj=="1","Human.Clint.Bin3.Adj_Names"] <- "Very Slow"
test[test$Bin.Adj=="2","Human.Clint.Bin3.Adj_Names"] <- "Slow"
test[test$Bin.Adj=="3","Human.Clint.Bin3.Adj_Names"] <- "Fast"

test[test$Bin=="1","Human.Clint.Bin3_Names"] <- "Very Slow"
test[test$Bin=="2","Human.Clint.Bin3_Names"] <- "Slow"
test[test$Bin=="3","Human.Clint.Bin3_Names"] <- "Fast"

#name bins in training set
train[train$Bin.Adj=="1","Bin.Adj.Bin3"] <- "Very Slow"
train[train$Bin.Adj=="2","Bin.Adj.Bin3"] <- "Slow"
train[train$Bin.Adj=="3","Bin.Adj.Bin3"] <-"Fast"

train[train$Bin=="1","Bin.Bin3"] <- "Very Slow"
train[train$Bin=="2","Bin.Bin3"] <- "Slow"
train[train$Bin=="3","Bin.Bin3"] <-"Fast"


#Make fraction unbound correction table from adjusted training data
FUcorrectiontab=aggregate(Fu_assay~Bin.Adj, data=train,FUN="median")
FUcorrectiontab[FUcorrectiontab$Bin=="1","Human.Clint.Bin3_Names"] <- "Very Slow"
FUcorrectiontab[FUcorrectiontab$Bin=="2","Human.Clint.Bin3_Names"] <- "Slow"
FUcorrectiontab[FUcorrectiontab$Bin=="3","Human.Clint.Bin3_Names"] <- "Fast"
#save(FUcorrectiontab, file="AER/Fuhepcorrection_from_training_data_DED12152020.RData")



#Set names for Bins in human.httk.data
human.httk.data[human.httk.data$Bin.Adj=="1","Human.Clint.Bin3_Names"] <- "Very Slow"
human.httk.data[human.httk.data$Bin.Adj=="2","Human.Clint.Bin3_Names"] <- "Slow"
human.httk.data[human.httk.data$Bin.Adj=="3","Human.Clint.Bin3_Names"] <- "Fast"


#transfer classification predicted clearance values from test set and assign same names 
human.httk.data$Human.Clint.QSAR.Bin3 <- as.character(test$CLpredsBin3)
human.httk.data[human.httk.data$Human.Clint.QSAR.Bin3=="1","Human.Clint.QSAR.Bin3_Names"] <- "Very Slow"
human.httk.data[human.httk.data$Human.Clint.QSAR.Bin3=="2","Human.Clint.QSAR.Bin3_Names"] <- "Slow"
human.httk.data[human.httk.data$Human.Clint.QSAR.Bin3=="3","Human.Clint.QSAR.Bin3_Names"] <- "Fast"



#scientific notation formatting
scientific_10 <- function(x) {
  out <- gsub("1e", "10^", scientific_format()(x))
  out <- gsub("//+","",out)
  out <- gsub("10//^01","10",out)
  out <- parse(text=gsub("10//^00","1",out))
}



#histogram of predicted clearance values, showing bins
Fig1 <- ggplot(human.httk.data,aes(x=as.numeric(Human.Clint.QSAR.Bin3))) +
  geom_histogram(aes(fill=Human.Clint.QSAR.Bin3_Names))+
  scale_x_log10(label=scientific_10)
print(Fig1)
print(paste(dim(subset(human.httk.data,Human.Clint.QSAR.Bin3_Names=="Very Slow"))[1],"chemicals with Very Slow clearance,", 
            dim(subset(human.httk.data,Human.Clint.QSAR.Bin3_Names=="Slow"))[1],"with moderate clearance, and",
            dim(subset(human.httk.data,Human.Clint.QSAR.Bin3_Names=="Fast"))[1],"with fast clearance"))


#describe charged state, put in column "Charge.Ph7.4"
for (this.chem in human.httk.data$CAS)
{
  state <- calc_ionization(chem.cas=this.chem,pH=7.4)
  if (state$fraction_neutral>0.5) charge.state <- "Neutral"
  else if (state$fraction_negative>0.5) charge.state <- "Anionic"
  else if (state$fraction_positive>0.5) charge.state <- "Cationic"
  else if (state$fraction_zwitter>0.5) charge.state <- "Zwitterionic"
  else charge.state <- "Mixed"
  human.httk.data[human.httk.data$CAS==this.chem,"Charge.Ph7.4"] <- charge.state
}
#print out breakdown of charges
print(paste(dim(subset(human.httk.data,Charge.Ph7.4=="Neutral"))[1],
            "neutral chemicals at pH 7.4,",
            dim(subset(human.httk.data,Charge.Ph7.4=="Anionic"))[1],
            "anionic,",
            dim(subset(human.httk.data,Charge.Ph7.4=="Cationic"))[1],
            "cationic,",
            dim(subset(human.httk.data,Charge.Ph7.4=="Zwitterionic"))[1],
            "Zwitterionic, and",
            dim(subset(human.httk.data,Charge.Ph7.4=="Mixed"))[1],
            "with a mixed charge state at pH 7.4."))


human.httk.data$Human.Clint.Bin3=as.character(human.httk.data$Human.Clint.Bin3)

#write file with inputs for httk
#write.csv(human.httk.data,file=paste("Human.Clint.data_",writesuff,".txt",sep=""),row.names=F)

#Is this important?
human.httk.data.complete <- human.httk.data[!complete.cases(human.httk.data),]
human.httk.data.complete=human.httk.data
dim(human.httk.data.complete) #This doesn't seem to kill anybody; all 97 still there.



#Assign uncorrected of Clint from test set to human.httk.data.complete
#Note, remember to use "Clearance" in the httk function, not "Human.Clint" for in vitro 
#human.httk.data.complete=human.httk.data.complete[order(human.httk.data.complete$order),]

#insert median and SD values based on actual (in vitro) bin
human.httk.data.complete$Human.Clint.Bin3.MD <- 0
human.httk.data.complete$Human.Clint.Bin3.MD <- 0  #Here is a difference; I substiuted Bin in here for Bin.Adj
for(j in c("Very Slow", "Slow", "Fast")){ #Take the median values of all very slow, slow, moderate, and fast clearance values from the training set
human.httk.data.complete[human.httk.data.complete$Human.Clint.Bin3_Names == j,"Human.Clint.Bin3.MD"] <- median(subset(train, Bin.Bin3 == j)$Clearance)
human.httk.data.complete[human.httk.data.complete$Human.Clint.Bin3_Names == j,"Human.Clint.Bin3.SD"] <- sd(subset(train,Bin.Bin3 == j)$Clearance)
}
#table(train$Bin.Adj.Bin3)
#human.httk.data.complete=human.httk.data.complete[order(human.httk.data.complete$order),]
#QSAR Clint:First find median of corrected bin. Then adjust by the FUcorrectiontab, as calculated above. This way, the median value for each 
#adjusted bin, gets adusted to close to it's original bin, based on the the factor used to get it to that bin.  

#insert median values for the predicted bin from classification
#First,  grab values based on the adjusted bin here. Then adjust back to original bin after applying the class-median correction 
human.httk.data.complete$Human.Clint.QSAR.Bin3.MD <- 0
human.httk.data.complete$Human.Clint.QSAR.Bin3.SD <- 0
for(j in c("Very Slow", "Slow", "Fast")){ #take the median values for the very slow, slow, fast,and very fast from the test set
human.httk.data.complete[human.httk.data.complete$Human.Clint.QSAR.Bin3_Names == j,"Human.Clint.QSAR.Bin3.MD"] <- median(subset(train,Bin.Adj.Bin3 == j)$Clearance.Adj)
human.httk.data.complete[human.httk.data.complete$Human.Clint.QSAR.Bin3_Names == j,"Human.Clint.QSAR.Bin3.SD"] <- sd(subset(train,Bin.Adj.Bin3 == j)$Clearance.Adj)
}

for(j in c("Very Slow", "Slow", "Fast")){ #Take the median values of all very slow, slow, moderate, and fast clearance values from the training set
  human.httk.data.complete[human.httk.data.complete$Human.Clint.QSAR.Bin3_Names == j,"Human.QSAR.Clint.Bin3.MD.hep.fup.corr"] <- FUcorrectiontab[which(FUcorrectiontab$Human.Clint.Bin3_Names==j),2]
}

#Make into numeric
human.httk.data.complete$Human.QSAR.Clint.Bin3.MD.hep.fup.corr=as.numeric(human.httk.data.complete$Human.QSAR.Clint.Bin3.MD.hep.fup.corr)


#Now, offset the clint value by multiplying by the correction; it will be divided by it in the function 
human.httk.data.complete$Human.Clint.QSAR.Bin3.MD = human.httk.data.complete$Human.Clint.QSAR.Bin3.MD * human.httk.data.complete$Human.QSAR.Clint.Bin3.MD.hep.fup.corr


#########
# Add the model predicted Fup values:
human.httk.data.complete <- merge(human.httk.data.complete,RF.Fup[,c("CAS","Fup.RF.pred")],by.x="CAS",by.y="CAS",all.x=T)

#Fix label: Fup to Fub
human.httk.data.complete$Fub.RF.pred<-human.httk.data.complete$Fup.RF.pred


#Take average value of Human.Funbound.plasma to deal with multiple empirical values per slot
hfp=NULL
for(i in 1:length(human.httk.data.complete$Human.Funbound.plasma)){
  sub=unlist(strsplit(human.httk.data.complete$Human.Funbound.plasma[i], ",")  )
  if(length(sub)==1){
  hfp=c(hfp, as.numeric(human.httk.data.complete$Human.Funbound.plasma[i]))} else {
    hfp=c(hfp,mean(as.numeric(unlist(sub))))
  }}  
human.httk.data.complete$Human.Funbound.plasma=hfp




#y-randomization of clearance and yrand values
set.seed(1255)
human.httk.data.complete$Human.Clint.Yrand <- sample(human.httk.data.complete$Clearance, length(human.httk.data.complete$Clearance), replace = F)
hist(human.httk.data.complete$Human.Clint.Yrand)

#Take fub yrand 
set.seed(1255)
human.httk.data.complete$Human.Funbound.plasma.Yrand <- sample(human.httk.data.complete$Human.Funbound.plasma, length(human.httk.data.complete$Human.Funbound.plasma), replace=F)


#show the spread of clearance values directed to each bin now
#issue in number order for figure 2

Fig4 <- ggplot(human.httk.data.complete,aes(x=log10(Clearance),y=log10(Human.Clint.QSAR.Bin3.MD))) +
  geom_point()+
  geom_abline(intercept = 0, slope = 1,linetype="dashed",color="blue")

Fig5 <- ggplot(human.httk.data.complete,aes(x=log10(Human.Funbound.plasma),y=log10(Fup.RF.pred))) +
  geom_point()+
  geom_abline(intercept = 0, slope = 1,linetype="dashed",color="blue")




##Predict CSS using HTTK model 
#The add_chemtable function substitutes in values of a dataframe, like human.httk.data.complete, into the chem.physical_and_invitro.data dataframe,
#the latter of which is a permenant fixture of the httk package. It is this dataframe that is then accessed via the httk package
#using the calc_ms_css function to calculate steady state concencentrations using a monte carlo simulation. 

#For, for each iteration of the loop, chem.physical_and_invitro.data get read in for httk, values correpsonding
#to the values in paramtab get substituted in for Clint and Funbound.plasma, according to the scenarios set up
# in paramtab. Then, then, the steady state concentration values (css) are calculated for these scenarios using the calc_mc_css function (which is the monte carlo sampler)
#and they assigned to columns labeled with the CSSoutputs labels of the paramtab. Clever. 

#Scenarios include:
#1.Invitro clearance and Fub values, 
#2&3:In vitro with binned clearance values(4 and 3 bins), 
#4&5:QSAR predictions with binned clearance values (4 and 3 bins) 
#6:Y random



#Note, that "Clearance" is used instead of "Human.Clint", which is the value listed in the test set
Comp1=c("Clearance", "Human.Funbound.plasma", "CSS.InVitro")
Comp2=c("Human.Clint.Bin3.MD", "Human.Funbound.plasma","CSS.InVitro_CL3BinsMD")
Comp3=c("Human.Clint.QSAR.Bin3.MD", "Fub.RF.pred","CSS.QSAR_CL3BinsMD")
Comp4=c("Human.Clint.Yrand", "Fub.RF.pred", "CSS.FubPred_CLYrand")
Comp5=c("Human.Clint.QSAR.Bin3.MD", "Human.Funbound.plasma.Yrand", "CSS.CL3BinsMD_FubYrand")
Comp6=c("Human.Clint.Yrand", "Human.Funbound.plasma.Yrand", "CSS.ClYrand_FubYrand")


paramtab=rbind(Comp1,Comp2,Comp3,Comp4,Comp5, Comp6)
paramtab=data.frame("Clint_types"=paramtab[,1], "Funbound_types"=paramtab[,2], "CSSoutputs"=paramtab[,3])


library(httk)
for(i in 1:length(paramtab[1,])){paramtab[,i]<-as.character(paramtab[,i])} #to keep them characters instead of factors


RNUMSEED <- 1255
for(i in 1:6){ #ength(paramtab[,1])){
chem.physical_and_invitro.data <- add_chemtable(human.httk.data.complete,
                                                current.table=chem.physical_and_invitro.data,
                                                data.list=list(CAS="CAS",
                                                               Clint=paramtab[i,names(paramtab)=="Clint_types"], #these column headers get combined with species, so they become "Human.Clint" in the new chem.physica_and_invitro.data dataframe
                                                               Funbound.plasma=paramtab[i,names(paramtab)=="Funbound_types"]),
                                                reference=paramtab[i, names(paramtab)=="Clint_types"],
                                                species="Human",
                                                overwrite=T)
#chem.physical_and_invitro.data$Human.Clint.pValue <- NA
#predict steady-state serum concentration using the supplied in vitro values:
for (j in 1:length(human.httk.data.complete$CAS)) {
  set.seed(RNUMSEED)
  human.httk.data.complete[which(human.httk.data.complete$CAS==human.httk.data.complete$CAS[j]),paramtab[i,names(paramtab)=="CSSoutputs"]] <- 
      calc_mc_css(chem.cas=human.httk.data.complete$CAS[j],output.units="uM", model="3compartmentss")
  }

}


###Linear Models of CSS

a=lm(log10(human.httk.data.complete$CSS.InVitro)~log10(human.httk.data.complete$CSS.InVitro_CL3BinsMD))
summary(a)
#plot(a)
b=lm(log10(human.httk.data.complete$CSS.InVitro)~log10(human.httk.data.complete$CSS.QSAR_CL3BinsMD))
summary(b)
#plot(b)
c=lm(log10(human.httk.data.complete$CSS.InVitro)~log10(human.httk.data.complete$CSS.CL3BinsMD_FubYrand))
#plot(c)
d=lm(log10(human.httk.data.complete$CSS.InVitro)~log10(human.httk.data.complete$CSS.FubPred_CLYrand))
#plot(d)
e=lm(log10(human.httk.data.complete$CSS.InVitro)~log10(human.httk.data.complete$CSS.ClYrand_FubYrand))
#plot(e)


modlist=list(a,b,c,d,e)

  modtab=lapply(modlist, function(x){
    int=summary(x)$coefficients[1]
    slope=summary(x)$coefficients[2]
    sig=summary(x)$sigma
    rsq=summary(x)$r.squared
    pval=pf(summary(x)$fstatistic[1],
       summary(x)$fstatistic[2],
       summary(x)$fstatistic[3],
       lower.tail=FALSE)
   tab=data.frame(int, slope,sig,rsq, pval)  
    
  return(tab)
  })
modtab=do.call("rbind", modtab)
modtab=data.frame("Scenario"=c("IVCL3Bins","CLQSAR_FupQSAR", "CLQSAR_FupYrand", "CLYrand_FubQSAR", "CLYrand_FupYrand"),modtab) 

modtab #for supplemental data
#save(modtab, file=paste("CSS_comparision_ToxCast_TestSet_", writesuff, ".RData", sep=""))
#load(paste("CSS_comparision_ToxCast_TestSet_", writesuff, ".RData", sep=""))
#save(human.httk.data.complete, file=paste("AER/Chem_and_CSS_table_", writesuff, ".RData", sep=""))
#load(paste("AER/Chem_and_CSS_table_", writesuff, ".RData", sep=""))

#show impact of in vitro vs. bin clearance on Css
Fig5 <- ggplot(human.httk.data.complete,aes(x=CSS.InVitro ,y=CSS.InVitro_CL3BinsMD)) +
  geom_point(aes(color=Human.Clint.Bin3))+
  scale_x_log10(label=scientific_10)+
  scale_y_log10(label=scientific_10)+
  geom_abline(intercept = 0, slope = 1,linetype="dashed",color="blue")+
  labs(color="Clearance Bins")+
  xlab("Css in Vitro")+
  ylab("Css Using Medians of Binned(4) in Vitro Clearance")
  print(Fig5)


#show impact of CLASSIFICATION predicted clearance BIN on Css

Fig6 <- ggplot(human.httk.data.complete,aes(x=CSS.InVitro,y=CSS.QSAR_CL3BinsMD)) +
  geom_point(aes(color=Human.Clint.QSAR.Bin3_Names))+
  scale_x_log10(label=scientific_10)+
  scale_y_log10(label=scientific_10)+
  geom_abline(intercept = 0, slope = 1,linetype="dashed",color="blue")+
  labs(color="Clearance Bins")+
  xlab("Css in Vitro")+
  ylab("Css using Bin Medians of Binned(3) QSAR Predictions")
print(Fig6)


#show impact of in vitro vs. y-randomized clearance on Css
Fig7 <- ggplot(human.httk.data.complete,aes(x=CSS.InVitro,y=CSS.CL3BinsMD_FubYrand)) +
  geom_point(aes(color=Human.Clint.QSAR.Bin3_Names))+
  scale_x_log10(label=scientific_10)+
  scale_y_log10(label=scientific_10)+
  geom_abline(intercept = 0, slope = 1,linetype="dashed",color="blue")+
  labs(color="Clearance Bins")+
  xlab("Css in Vitro")+
  ylab("Css using QSAR Cl values and Y-Randomized Fub values")
print(Fig7)


#show impact of in vitro vs. zero clearance on Css
Fig8 <- ggplot(human.httk.data.complete,aes(x=CSS.InVitro,y=CSS.FubPred_CLYrand)) +
  geom_point()+
  scale_x_log10(label=scientific_10)+
  scale_y_log10(label=scientific_10)+
  geom_abline(intercept = 0, slope = 1,linetype="dashed",color="blue")+
labs(color="Clearance Bins")+
  xlab("Css in Vitro")+
  ylab("Css using Y-Random Cl and QSAR Fub values ")
print(Fig8)

Fig9 <- ggplot(human.httk.data.complete,aes(x=CSS.InVitro,y=CSS.ClYrand_FubYrand)) +
  geom_point()+
  scale_x_log10(label=scientific_10)+
  scale_y_log10(label=scientific_10)+
  geom_abline(intercept = 0, slope = 1,linetype="dashed",color="blue")+
 labs(color="Clearance Bins")+
  xlab("Css in Vitro")+
  ylab("Css with Y-Randomized CL and Fub values")
print(Fig9)



##Compare CSS to Activity Values
#check paths to files!!!
##In this section, we're going to add data about concentrations of each chemical that cross some kind of toxicological threshold
#in the Tox21 dataset. Then, were going to add in information predicted exposure using NHANES data and some 
#heuristic model if NHANEs is missing. The data from NHANES is being used to gauge how well our predictions do in comparison to empircal data, and
#is displayed on the graph as median and maximum values. 


#head to https://www3.epa.gov/research/COMPTOX/toxcast_summary.html to get the ToxCast/Tox21 data:
#added to my Documents

Tox21.ids <- read.csv("AER/Exposure_Tox_Data/Chemical_Summary_190708.csv")

# These are the concentrations that caused activity in excess of the background (Activity Concentration Cutoff=acc); that is,  "hits":
##DED: These are concentrations that trigger some kind of toxicological threshold for any number of Tox asssays
Tox21.acc <- read.csv("AER/Exposure_Tox_Data/Tox21_acc_Matrix_190708.csv")
#merge datasets
Tox21.acc <- merge(Tox21.ids,Tox21.acc,by.x="code",by.y="X",stringsAsFactors=F)

# Subset this to just the chemicals we have numbers for (this won't help if you predict everything): #Note, this results in a dataframe with 91 chemicals 
Tox21.acc <- subset(Tox21.acc,casn %in% human.httk.data.complete$CAS)
NotinTox21.acc <- subset(human.httk.data.complete, !(human.httk.data.complete$CAS%in%Tox21.acc$casn))#9 chemicals in human.httk.data.complete not found in the Tox21 database


#we need to turn this into a data frame with a row for each hit:
#uses reshape package
Tox21.acc.rows <- melt(Tox21.acc,id=c("code","chid","chnm","casn","dsstox_substance_id","clib")) #Goes from wide to long form 


#get rid of the NA's (non-hits):
Tox21.acc.numeric <- subset(Tox21.acc.rows,!is.na(value))

#Tests per chemical
Tox21.acc.numeric$Count=1
range(aggregate(Count~casn * chnm, data=Tox21.acc.numeric, FUN="sum")[3])

#How many chemical done by test
Testsperchem=aggregate(Count~variable, data=Tox21.acc.numeric, FUN="sum")

#subset to those chemicals with HTS hits:
human.hits <- subset(human.httk.data.complete,CAS %in% Tox21.acc.numeric$casn) #this produces a set of 91 chemicals with effects data

#open exposure estimates
load("AER/Exposure_Tox_Data/chem.preds-2018-11-28.RData")
heuristics.preds=chem.preds

load("AER/Exposure_Tox_Data/NHANES_2018_Summary.RData")
directnhanes.preds=NHANES.obs


#add exposure predictions, first NHANES, then heuristics model (Wambaugh 2014)
#DED: This adds exposure predicts based on empirical data? Not sure. I need to clarify what 
#data is in the directnhanes.preds vs heuristics.preds files
#I think what's going on is that we're trying to fill in information where available. So,
#if the data for a chemical is available in the nhanes data, then it fills in from there. If it
#isn't, it fills in from this "heuristics.preds" file, which I don't know the origin of. 
for (this.chem in human.hits$CAS)
{print(this.chem)
  if (this.chem %in% directnhanes.preds$CAS)
  {human.hits[human.hits$CAS==this.chem,"Exposure.median"] <- 
      directnhanes.preds[directnhanes.preds$CAS==this.chem,"mgpkgpday"]
    human.hits[human.hits$CAS==this.chem,"Exposure.high"] <- 
      directnhanes.preds[directnhanes.preds$CAS==this.chem,"lP.max"]   
   human.hits[human.hits$CAS==this.chem, "ExposureSouce"] <- "NHanes"
    
    } else if (this.chem %in% heuristics.preds$CAS) {
    human.hits[human.hits$CAS==this.chem,"Exposure.median"] <- 
      heuristics.preds[heuristics.preds$CAS==this.chem,"seem3"]
    human.hits[human.hits$CAS==this.chem,"Exposure.high"] <- 
      heuristics.preds[heuristics.preds$CAS==this.chem,"seem3.u95"]  
    human.hits[human.hits$CAS==this.chem, "ExposureSouce"] <- "SEEM3"
    }
}

#Note, data is mostly coming from SEEM3 model. 

#calculate the oral equivalent dose for each chemical
# VERY IMPORTANT -- MUST CHECK WHETHER VALUES IN THE ACC DATA FILE ARE LOG BASE E OR 10; 
#Based on the documentation for this, its in logbase10, so need to transform using 10^x format, not exp
#for in vitro measurement
#This resets the values of the chem.physical_and_invitro.data set to empirical values

Comp1a=c("Clearance", "Human.Funbound.plasma", "IV")
Comp2a=c("Human.Clint.Bin3.MD", "Human.Funbound.plasma", "IV.CLBin3.MD")
Comp3a=c("Human.Clint.QSAR.Bin3.MD", "Fub.RF.pred", "QSAR_FubPred_CLPredBin3.MD")
Comp4a=c("Human.Clint.Yrand", "Fub.RF.pred", "FubPred_CLYrand")
Comp5a=c("Human.Clint.QSAR.Bin3.MD", "Human.Funbound.plasma.Yrand", "FubYrand_CLPredBin3")
Comp6a=c("Human.Clint.Yrand", "Human.Funbound.plasma.Yrand", "FubYrand_CLYrand")

paramtab1=rbind(Comp1a,Comp2a,Comp3a,Comp4a,Comp5a, Comp6a)
paramtab1=data.frame("Clint_types"=paramtab1[,1], "Funbound_types"=paramtab1[,2], "DoseEquivoutputs"=paramtab1[,3])

for(i in 1:length(paramtab1[1,])){paramtab1[,i]<-as.character(paramtab1[,i])} #to keep them characters instead of factors

#This section calculates the median, 10%, the minimum, and maximum of the values of the toxicological variables included. 
#then, it calculates oral dose equivalents for each of the doses. Note that the dose listed in in Tox21.acc are apparently some kind
#of log, either natural or 10, though they are currently listed as natural log. 
#Notes that this takes a bit of time because it has to calculate dose equivents at each of the four
#does below, including the median, the 10%tile, the low, and high exposure doses. What this is doing is
#is saying, using a particular model paramterization, what would the doses have to be in order to
#get exposed to these particular levels. 


##Data Preparation for Plotting AER; going to try as a parallel process
for(i in 1:length(paramtab1[,1])){
library(httk)
chem.physical_and_invitro.data <- add_chemtable(human.hits,
  current.table=chem.physical_and_invitro.data,
  data.list=list(CAS="CAS",
                 Clint=paramtab1[i, names(paramtab1)=="Clint_types"],             #This the adjusted empirical invitro data. 
                 Funbound.plasma=paramtab1[i, names(paramtab1)=="Funbound_types"]), #remember, the Human.Funbound.plasma is part of the dataset.
  reference="HTTK2.0.1",
  species="Human",
  overwrite=T)

###In this section, we are taking toxicological endpoints from Tox21, and finding the min, max, Q10 and median values that we've got. 
#Then, we're calculating the oral equivalent doses in mgkgday of those doses for each chemical

for (this.chem in human.hits$CAS){
  med.conc <- median(10^(subset(Tox21.acc.numeric,casn==this.chem)$value)) #This are the threshold values for any available tox data
  q10.conc <- quantile(10^(subset(Tox21.acc.numeric,casn==this.chem)$value),0.1)
  low.conc <- min(10^(subset(Tox21.acc.numeric,casn==this.chem)$value))
  high.conc <- max(10^(subset(Tox21.acc.numeric,casn==this.chem)$value))
  human.hits[human.hits$CAS==this.chem,"HTS.Median.ACC"] <- med.conc
  human.hits[human.hits$CAS==this.chem,"HTS.Q10.ACC"] <- q10.conc
  human.hits[human.hits$CAS==this.chem,"HTS.Low.ACC"] <- low.conc
  human.hits[human.hits$CAS==this.chem,"HTS.High.ACC"] <- high.conc
  human.hits[human.hits$CAS==this.chem,paste("HTS.Median.equivdose.",paramtab1[i, names(paramtab1)=="DoseEquivoutputs"],sep="")] <- 
    calc_mc_oral_equiv(med.conc,chem.cas=this.chem)
  human.hits[human.hits$CAS==this.chem,paste("HTS.Q10.equivdose.",paramtab1[i, names(paramtab1)=="DoseEquivoutputs"],sep="")] <- 
    calc_mc_oral_equiv(q10.conc,chem.cas=this.chem)
  human.hits[human.hits$CAS==this.chem,paste("HTS.Low.equivdose.",paramtab1[i, names(paramtab1)=="DoseEquivoutputs"],sep="")] <- 
    calc_mc_oral_equiv(low.conc,chem.cas=this.chem)
  human.hits[human.hits$CAS==this.chem,paste("HTS.High.equivdose.",paramtab1[i, names(paramtab1)=="DoseEquivoutputs"],sep="")] <- 
    calc_mc_oral_equiv(high.conc,chem.cas=this.chem)
  }
}  

#save(human.hits, file=paste("AER/DoseEquivalents_Multiple_Scenarios_", writesuff,".RData", sep=""))


#load(paste("AER/DoseEquivalents_Multiple_Scenarios_", writesuff,".RData", sep=""))


human.hits$AER_IV=human.hits$HTS.Q10.equivdose.IV/human.hits$Exposure.high
human.hits$AER_IV.CLBin3.MD=human.hits$HTS.Q10.equivdose.IV/human.hits$Exposure.high
human.hits$AER_QSAR_FubPred_CLPredBin3.MD<-human.hits$HTS.Q10.equivdose.QSAR_FubPred_CLPredBin3.MD/human.hits$Exposure.high
human.hits$AER_FubPred_CLYrand<-human.hits$HTS.Q10.equivdose.FubPred_CLYrand/human.hits$Exposure.high
human.hits$AER_FubYrand_CLPredBin3<-human.hits$HTS.Q10.equivdose.FubYrand_CLPredBin3/human.hits$Exposure.high
human.hits$AER_FubYrand_CLYrand<-human.hits$HTS.Q10.equivdose.FubYrand_CLYrand/human.hits$Exposure.high


human.hits1<-human.hits[order(human.hits$AER_IV, decreasing=FALSE),]
human.hits1$Compound <- factor(human.hits1$Compound, levels=(human.hits1$Compound)) #this sorts the compounds by potency, or  
human.hits1$CompoundSequence=seq(1,length(human.hits1[,1]))
human.hits1$CompoundSequenceFact=factor(human.hits1$CompoundSequence)

#Exceeds
dim(human.hits1[human.hits1$AER_IV.CLBin3.MD<1,])[1] #14 exceeds for in vitro
dim(human.hits1[human.hits1$AER_QSAR_FubPred_CLPredBin3.MD<1,])[1] #18 exceeds for QSAR


#Concordance
BothExceed=human.hits1[human.hits1$AER_QSAR_FubPred_CLPredBin3.MD<1 & human.hits1$AER_IV<1,]
BothNotExceed=human.hits1[human.hits1$AER_QSAR_FubPred_CLPredBin3.MD>1 & human.hits1$AER_IV>1,]

(dim(BothExceed)[1] + dim(BothNotExceed)[1])/dim(human.hits1)[1] #96% Concordance


#False Negatives
#QSAR   FN/TP + FN
FNQ=length(which(human.hits1[human.hits1$AER_IV<1, "CAS"]%in%human.hits1[human.hits1$AER_QSAR_FubPred_CLPredBin3.MD<1, "CAS"]==FALSE))
FNQR=length(FNQ)/(length(which(human.hits1[human.hits1$AER_QSAR_FubPred_CLPredBin3.MD<1, "CAS"]%in%human.hits1[human.hits1$AER_IV<1, "CAS"]==TRUE)) + FNQ)


#RandClit
FNCLrand=length(which(human.hits1[human.hits1$AER_IV<1, "CAS"]%in%human.hits1[human.hits1$AER_FubPred_CLYrand<1, "CAS"]==FALSE))
FNCLrandRate=FNCLrand/(length(which(human.hits1[human.hits1$AER_FubPred_CLYrand<1, "CAS"]%in%human.hits1[human.hits1$AER_IV<1, "CAS"]==TRUE)) + FNCLrand)
FNCLrand
FNCLrandRate

#RandFub
FNFubrand=length(which(human.hits1[human.hits1$AER_IV<1, "CAS"]%in%human.hits1[human.hits1$AER_FubYrand_CLPredBin3<1, "CAS"]==FALSE))
FNFubrandR=FNFubrand/(length(which(human.hits1[human.hits1$AER_FubYrand_CLPredBin3<1, "CAS"]%in%human.hits1[human.hits1$AER_IV<1, "CAS"]==TRUE)) + FNFubrand)
FNFubrand
FNFubrandR

#RandBoth 
FNFubrandClrand=length(which(human.hits1[human.hits1$AER_IV<1, "CAS"]%in%human.hits1[human.hits1$AER_FubYrand_CLYrand<1, "CAS"]==FALSE))
FNFubrandClrandR=FNFubrandClrand/(length(which(human.hits1[human.hits1$AER_FubYrand_CLYrand<1, "CAS"]%in%human.hits1[human.hits1$AER_IV<1, "CAS"]==TRUE)) + FNFubrandClrand)
FNFubrandClrand
FNFubrandClrandR

#False Positives 
#QSAR    FP/FP + TN
FPQ=length(which(human.hits1[human.hits1$AER_QSAR_FubPred_CLPredBin3.MD<1, "CAS"]%in%human.hits1[human.hits1$AER_IV<1, "CAS"]==FALSE))
FPQR=FPQ/(length(which(human.hits1[human.hits1$AER_QSAR_FubPred_CLPredBin3.MD>1, "CAS"]%in%human.hits1[human.hits1$AER_IV>1, "CAS"]==TRUE)) + FPQ)
FPQ
FPQR
dim(human.hits1[human.hits1$AER_IV>1,])

#Yrandfub
FPFubRand=length(which(human.hits1[human.hits1$AER_FubYrand_CLPredBin3<1, "CAS"]%in%human.hits1[human.hits1$AER_IV<1, "CAS"]==FALSE))
FPFubRandR=FPFubRand/(length(which(human.hits1[human.hits1$AER_FubYrand_CLPredBin3>1, "CAS"]%in%human.hits1[human.hits1$AER_IV>1, "CAS"]==TRUE)) + FPFubRand)
FPFubRand
FPFubRandR

#Yrandclint
FPClintRand=length(which(human.hits1[human.hits1$AER_FubPred_CLYrand<1, "CAS"]%in%human.hits1[human.hits1$AER_IV<1, "CAS"]==FALSE))
FPClintRandR=FPClintRand/(length(which(human.hits1[human.hits1$AER_FubPred_CLYrand>1, "CAS"]%in%human.hits1[human.hits1$AER_IV>1, "CAS"]==TRUE)) + FPClintRand)
FPClintRand
FPClintRandR

#Yrandboth
FPClintFubRand=length(which(human.hits1[human.hits1$AER_FubYrand_CLYrand<1, "CAS"]%in%human.hits1[human.hits1$AER_IV<1, "CAS"]==FALSE))
FPClintFubRandR=FPClintFubRand/(length(which(human.hits1[human.hits1$AER_FubYrand_CLYrand>1, "CAS"]%in%human.hits1[human.hits1$AER_IV>1, "CAS"]==TRUE)) + FPClintFubRand)
FPClintFubRand
FPClintFubRandR


##BarCharts: This section creates barcharts of AER, as defined above. 

FigFunction<-function(dataset=human.hits, predset="IV", SourceLabel="In Vitro", filelabel=suff, PredSourceLabel="Opera", filesave=TRUE, sourcelabel=TRUE, xaxislabels=TRUE, subsetsize="all", xlabelsize=12, yaxislabels=TRUE){
  if (subsetsize != "all") {
    dataset = dataset[c(1:subsetsize),]
  }
  # Add a range from median to lowest ToxCast equivalent value:
  
Fig=ggplot(dataset, aes(x=CompoundSequence, y=log(get(paste("AER_",predset, sep="")))))+
    geom_bar(stat="identity", width=0.1) 
  
Fig <- Fig + scale_x_continuous(breaks=seq(0,90,10), limits=c(0,95))
  # Angle the chemical names for readability:
  if(xaxislabels==TRUE){
    #Fig <- Fig + theme(axis.text.x = element_text(angle = 60, hjust = 1,size=xlabelsize), legend.position="none")
   # Fig <- Fig + scale_x_continuous(breaks=seq(0,90,10), limits=c(0,95))
    # Adjust font sizes:
    Fig <- Fig + theme(axis.title.x = element_text(size=16))} else {
      
      Fig<- Fig + theme(axis.title.x=element_blank(), 
                        axis.text.x=element_blank(),
                        axis.ticks.x=element_blank())
      
    }
  
  
  Fig <- Fig + theme(axis.title.y = element_text(size=15))
  
  
  # Label the axes:
  Fig <- Fig + xlab("Chemicals")
  
  if(yaxislabels==TRUE){
    Fig <- Fig + ylab(paste("OED of Q10 Concentration / Concentration of Highest Expected Exposure"))
    } else {
      Fig <- Fig + ylab(paste(""))
      #Fig <- Fig + labs(color="Exposure Prediction/n(Median and Upper 95%)")  
    }
  if (sourcelabel==TRUE){
    Fig <- Fig + annotate(geom="Text", x=1, y=max(log(human.hits$AER_IV))*0.8, label=SourceLabel, color="black", size=8, hjust=0)
  }
  
  print(Fig)
  #ggsave(paste("AER_", predset,"_",y,"_",filelabel,".png", sep=""))
  if(filesave==TRUE){
    ggsave(paste("AER_", predset,"_",filelabel,".png", sep=""))
  }
  return(Fig)
}


#Make All plots
p1=FigFunction(dataset=human.hits1, predset="IV", SourceLabel="A",  filelabel=suff, filesave=FALSE, sourcelabel=TRUE,xaxislabels=FALSE, subsetsize = "all", xlabelsize = 8, yaxislabels = FALSE)
p2=FigFunction(dataset=human.hits1, predset="QSAR_FubPred_CLPredBin3.MD", SourceLabel="B",  filelabel=suff, filesave=FALSE, sourcelabel=TRUE,xaxislabels=FALSE, subsetsize = "all", xlabelsize = 8, yaxislabels = TRUE)
p3=FigFunction(dataset=human.hits1, predset="FubYrand_CLYrand", SourceLabel="C",  filelabel=suff, filesave=FALSE, sourcelabel=TRUE,xaxislabels=TRUE, subsetsize = "all", xlabelsize = 8, yaxislabels = FALSE)


library(grid)
grid.newpage()
grid.draw(rbind(ggplotGrob(p1),ggplotGrob(p2),ggplotGrob(p3), size = "last"))
#600 X 700


#This considers the ratio of the high exposure dose to the Q10 dose; basically the percentage that high exposure dose is to the oral equivalent dose.  
AER          <- human.hits1$HTS.Q10.equivdose.IV/human.hits1$Exposure.high
AER.bin3.md       <- human.hits1$HTS.Q10.equivdose.IV.CLBin3.MD/human.hits1$Exposure.high
AER.qsar.bin3.md  <- human.hits1$HTS.Q10.equivdose.QSAR_FubPred_CLPredBin3.MD/human.hits1$Exposure.high
AER.FubPredandCLYrand    <- human.hits1$HTS.Q10.equivdose.FubPred_CLYrand/human.hits1$Exposure.high
AER.FubyrandCL3    <- human.hits1$HTS.Q10.equivdose.FubYrand_CLPredBin3/human.hits1$Exposure.high
AER.FubYrandCLYrand    <- human.hits1$HTS.Q10.equivdose.FubYrand_CLYrand/human.hits1$Exposure.high



#This considers the correlation between the in vitro data and the structures(binned), predictions(QSAR), and null checks(Yrand, Zero).
corr.3bin.md       <- cor(AER,AER.bin3.md, method = c("spearman"))        # 0.8959445
corr.qsar.3bin.md  <- cor(AER,AER.qsar.bin3.md, method = c("spearman"))   # 0.6670344
corr.fubyrand.cl3bin<- cor(AER,AER.FubPredandCLYrand, method = c("spearman"))
corr.fubpred.clyrand<- cor(AER,AER.FubyrandCL3, method = c("spearman"))
corr.fubyrand.clyrand<-cor(AER,AER.FubYrandCLYrand, method = c("spearman"))

corr.results=data.frame(corr.3bin.md, corr.qsar.3bin.md, corr.fubyrand.cl3bin,corr.fubpred.clyrand, corr.fubyrand.clyrand)

#RSME results
sqrt(mean((AER-AER.bin3.md)^2))
sqrt(mean((AER-AER.qsar.bin3.md)^2))
sqrt(mean((AER-AER.FubPredandCLYrand)^2))
sqrt(mean((AER-AER.FubyrandCL3)^2))
sqrt(mean((AER-AER.FubYrandCLYrand)^2))


#write file

SuppTab=subset(human.hits, select=c("Compound", "CAS", "DTXSID", "Clearance.Adj", "Human.Clint.Bin3", "Human.Funbound.plasma",
                                    "Human.Clint.QSAR.Bin3", "Human.Clint.QSAR.Bin3.MD", "Fub.RF.pred", "Exposure.high", "ExposureSouce",
                                    "HTS.Q10.equivdose.IV", "HTS.Q10.equivdose.IV.CLBin3.MD", "HTS.Q10.equivdose.QSAR_FubPred_CLPredBin3.MD", 
                                    "HTS.Q10.equivdose.FubPred_CLYrand","HTS.Q10.equivdose.FubYrand_CLPredBin3","HTS.Q10.equivdose.FubYrand_CLYrand"))

names(SuppTab)=c("Name", "CASRN", "DTXSID", "In vitro Clint","In vitro Clint Bin", "In vitro Fup",  "QSAR Clint Bin", "QSAR Clint", "QSAR Fup", "Exposure Prediction", "Exposure Pred Source", 
                 "In vitro Q10 EOD", "In vitro Binned Q10 EOD", "QSAR Q10 EOD", "QSAR_Fup and Yrand_Clint Q10 EOD", "QSAR_Clint and Yrand_Fup Q10 EOD",
                 "Yrand_Fup and Yrand_Clint Q10 EOD")
write.csv(SuppTab, file = paste("AER/Dose_Equiv_and_Pred_Exp_results_ToxCast_" ,writesuff,".csv",sep=""))
#impact on AER metrics




