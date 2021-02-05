rm(list=ls())

### CALCULATING CSS FOR CHEMICALS, EVALUATING CL PREDICTION IMPACT ####
## Original script from John Wambaugh (June 2016)
## Modifcations from Brandall Ingle (January 2018, June 2018)

setwd("c:/users/jwambaug/git/httk-dev/work/")

### SECTION 1: SETTING DIRECTORY, LOADING PROGRAMS, IMPORT FILES ###
readsuff="DED12152020"
writesuff="DED01042021"

library(httk)
library(gdata)
library(ggplot2)
library(scales)
library(reshape)


#Pull in predictions from Clearance QSARS and associate with the test set
##Best predictor sets are:
#Cl: The all predictor set with 50 predictors
#Fub: The all Y set with X predictors 
load(paste("Model_evaluation_testsets_withSeed1255_", readsuff, ".RData",sep=""))
CLpredsBin3=Cltestsetresults$TestSetPreds$a_3$PredToxCast

####Associate predicted values to test set
test=read.csv(paste("HepaticClearance/test2_a_ADfiltered_DED12152020.csv",sep=""))
test$CLpredsBin3=CLpredsBin3

#Pull in predictions from Fup QSAR
PaDEL.RF.Fup <- read.csv("AER/test2_fup_All_40desc_060320_.csv")

#pull in training set data (for bin medians)
load(paste("HepaticClearance/train_3_a_DED121520.RData",sep="")) #Reads from two steps up the directory 
train <- train[,1:15]#Note, changed this to 1:16 from 1:12


### SECTION 2: COMBINING DATASETS, RENAMING BINS, ASSIGNING CHARGE ###

#pull chemicals from human.httk that are in the test
#pull data from httk (all our chemicals are in here)
human.httk.data <- get_cheminfo(info="All") #data from httk package
human.httk.data <- subset(human.httk.data, human.httk.data$CAS %in% test$ID)

#pull chemicals from test set that are in httk
test <- subset(test,test$ID %in% human.httk.data$CAS)
test$ID<-as.character(test$ID)

#sort both sets by CAS to make sure rows match
test <- test[order(test$ID),]
human.httk.data <- human.httk.data[order(human.httk.data$CAS),]
identical(human.httk.data$CAS, test$ID)

#transfer bin values from test; 
test$Bin.Adj=ifelse(test$Bin.Adj==4,3,test$Bin.Adj)
human.httk.data$Human.Clint.Bin3=test$Bin.Adj


human.httk.data[human.httk.data$Human.Clint.Bin3=="1","Human.Clint.Bin3_Names"] <- "Very Slow"
human.httk.data[human.httk.data$Human.Clint.Bin3=="2","Human.Clint.Bin3_Names"] <- "Slow"
human.httk.data[human.httk.data$Human.Clint.Bin3=="3","Human.Clint.Bin3_Names"] <- "Fast"


#transfer classification predicted clearance values from test set
human.httk.data$Human.Clint.QSAR.Bin3 <- as.character(test$CLpredsBin3)
human.httk.data[human.httk.data$Human.Clint.QSAR.Bin3=="1","Human.Clint.QSAR.Bin3_Names"] <- "Very Slow"
human.httk.data[human.httk.data$Human.Clint.QSAR.Bin3=="2","Human.Clint.QSAR.Bin3_Names"] <- "Slow"
human.httk.data[human.httk.data$Human.Clint.QSAR.Bin3=="3","Human.Clint.QSAR.Bin3_Names"] <- "Fast"


#name bins in training set
train[train$Bin.Adj=="1","Bin.Adj.Bin3"] <- "Very Slow"
train[train$Bin.Adj=="2","Bin.Adj.Bin3"] <- "Slow"
train[train$Bin.Adj=="3","Bin.Adj.Bin3"] <-"Fast"


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
write.csv(human.httk.data,file=paste("Human.Clint.data_",writesuff,".txt",sep=""),row.names=F)
#prune down to those chemicals with all required data (cut 18 chemicals)  

#Is this important?
human.httk.data.complete <- human.httk.data[!complete.cases(human.httk.data),]
human.httk.data.complete=human.httk.data
dim(human.httk.data.complete) #This doesn't seem to kill anybody; all 97 still there.

#insert median and SD values based on actual (in vitro) bin
human.httk.data.complete$Human.Human.Clint.Bin3.MD <- 0
human.httk.data.complete$Human.Human.Clint.Bin3.MD <- 0
for(j in c("Very Slow", "Slow", "Fast")){ #Take the median values of all very slow, slow, moderate, and fast clearance values from the training set
human.httk.data.complete[human.httk.data.complete$Human.Clint.Bin3_Names == j,"Human.Clint.Bin3.MD"] <- median(subset(train,Bin.Adj.Bin3 == j)$Clearance.Adj)
human.httk.data.complete[human.httk.data.complete$Human.Clint.Bin3_Names == j,"Human.Clint.Bin3.SD"] <- sd(subset(train,Bin.Adj.Bin3 == j)$Clearance.Adj)
}


#insert median values for the predicted bin from classification
human.httk.data.complete$Human.Clint.QSAR.Bin3.MD <- 0
human.httk.data.complete$Human.Clint.QSAR.Bin3.SD <- 0
for(j in c("Very Slow", "Slow", "Fast")){ #take the median values for the very slow, slow, fast,and very fast from the test set
human.httk.data.complete[human.httk.data.complete$Human.Clint.QSAR.Bin3_Names == j,"Human.Clint.QSAR.Bin3.MD"] <- median(subset(train,Bin.Adj.Bin3 == j)$Clearance.Adj)
human.httk.data.complete[human.httk.data.complete$Human.Clint.QSAR.Bin3_Names == j,"Human.Clint.QSAR.Bin3.SD"] <- sd(subset(train,Bin.Adj.Bin3 == j)$Clearance.Adj)
}

#Transfer in-vitro clearance values from test data to assess against
human.httk.data.complete=human.httk.data.complete[order(human.httk.data.complete$CAS),]
test=test[order(test$ID),]
human.httk.data.complete$Human.Clint.Correct <- test$Clearance.Adj #I'm guessing this is more up to date

#y-randomization of clearance based in vitro clearance values
set.seed(1255)
human.httk.data.complete$Human.Clint.Yrand <- sample(human.httk.data.complete$Human.Clint.Correct, length(human.httk.data.complete$Human.Clint.Correct), replace = F)


# Add the model predicted Fup values:
human.httk.data.complete <- merge(human.httk.data.complete,PaDEL.RF.Fup[,c("CAS","Fup.RF.pred")],by.x="CAS",by.y="CAS",all.x=T)
#show <- (cor(human.httk.data.complete$Fub.PaDEL.RF.pred, human.httk.data.complete$Human.Funbound.plasma))^2

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
set.seed(1255)
human.httk.data.complete$Human.Funbound.plasma.Yrand <- sample(human.httk.data.complete$Human.Funbound.plasma, length(human.httk.data.complete$Human.Funbound.plasma), replace=F)


set.seed(1255)
human.httk.data.complete$Human.Clint.Yrand <- sample(human.httk.data.complete$Human.Clint.Correct, length(human.httk.data.complete$Human.Clint.Correct), replace=F)

#show the spread of clearance values directed to each bin now
#issue in number order for figure 2

Fig4 <- ggplot(human.httk.data.complete,aes(x=log10(Human.Clint.Correct),y=log10(Human.Clint.Bin3.MD))) +
  geom_point()+
  geom_abline(intercept = 0, slope = 1,linetype="dashed",color="blue")

Fig5 <- ggplot(human.httk.data.complete,aes(x=log10(Human.Funbound.plasma),y=log10(Fup.RF.pred))) +
  geom_point()+
  geom_abline(intercept = 0, slope = 1,linetype="dashed",color="blue")


Fig4
Fig5

table(human.httk.data.complete$Human.Clint.Bin3.MD)

# We'd like the Monte Carlo to give us the same answer if the script is repeated:
RNUMSEED <- 1255


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
#7:Zero value



Comp1=c("Human.Clint.Correct", "Human.Funbound.plasma", "CSS.InVitro")
Comp2=c("Human.Clint.Bin3.MD", "Human.Funbound.plasma","CSS.InVitro_CL3BinsMD")
Comp3=c("Human.Clint.QSAR.Bin3.MD", "Fub.RF.pred","CSS.QSAR_CL3BinsMD")
Comp4=c("Human.Clint.Yrand", "Fub.RF.pred", "CSS.FubPred_CLYrand")
Comp5=c("Human.Clint.QSAR.Bin3.MD", "Human.Funbound.plasma.Yrand", "CSS.CL3BinsMD_FubYrand")
Comp6=c("Human.Clint.Yrand", "Human.Funbound.plasma.Yrand", "CSS.ClYrand_FubYrand")


paramtab=rbind(Comp1,Comp2,Comp3,Comp4,Comp5, Comp6)
paramtab=data.frame("Clint_types"=paramtab[,1], "Funbound_types"=paramtab[,2], "CSSoutputs"=paramtab[,3])

#Remember, need to turn off protein correction 

for(i in 1:length(paramtab[1,])){paramtab[,i]<-as.character(paramtab[,i])} #to keep them characters instead of factors
#load the in vitro clearances from the httk package, using the table "chem.physical_and_invitro.data
#The below loops adds invitro clearance values from the httk.package, and then creates a column called CSSoutputs

old <- chem.physical_and_invitro.data

# Obliterater pvalues:
 chem.physical_and_invitro.data$Human.Clint.pValue <- NA



for(i in 1:length(paramtab[,1])){
chem.physical_and_invitro.data <- add_chemtable(
  human.httk.data.complete,
  current.table=chem.physical_and_invitro.data,
  data.list=list(
    CAS="CAS",
#these column headers get combined with species, so they become "Human.Clint" 
# in the new chem.physica_and_invitro.data dataframe:
    Clint=paramtab[i,names(paramtab)=="Clint_types"],
#    Clint.pValue="Fake.pValue", 
    Funbound.plasma=paramtab[i,names(paramtab)=="Funbound_types"]),
  reference=paramtab[i,names(paramtab)=="Clint_types"],
  species="Human",
  overwrite=T)

#predict steady-state serum concentration using the supplied in vitro values:
for (this.chem in human.httk.data.complete$CAS) {
  set.seed(RNUMSEED)
  human.httk.data.complete[human.httk.data.complete$CAS==this.chem,paramtab[i,names(paramtab)=="CSSoutputs"]] <- 
    calc_mc_css(chem.cas=this.chem,output.units="uM")
  }

}

###Linear Models of CSS

a=lm(log10(human.httk.data.complete$CSS.InVitro)~log10(human.httk.data.complete$CSS.InVitro_CL3BinsMD))
b=lm(log10(human.httk.data.complete$CSS.InVitro)~log10(human.httk.data.complete$CSS.QSAR_CL3BinsMD))
c=lm(log10(human.httk.data.complete$CSS.InVitro)~log10(human.httk.data.complete$CSS.CL3BinsMD_FubYrand))
d=lm(log10(human.httk.data.complete$CSS.InVitro)~log10(human.httk.data.complete$CSS.FubPred_CLYrand))
e=lm(log10(human.httk.data.complete$CSS.InVitro)~log10(human.httk.data.complete$CSS.ClYrand_FubYrand))

packageVersion("httk")
summary(b)
save.image(file="v202.RData")























plot(log10(human.httk.data.complete$CSS.InVitro), log10(human.httk.data.complete$CSS.InVitro_CL3BinsMD))
plot(log10(human.httk.data.complete$CSS.InVitro), log10(human.httk.data.complete$CSS.QSAR_CL3BinsMD))

modlist=list(a,b,c,d,e)
modtab=lapply(modlist, function(x){
  summary(x)$r.squared
})
modtab=do.call("rbind", modtab)
modtab=data.frame("Scenario"=c("IVCL3Bins","CLQSAR_FupQSAR", "CLQSAR_FupYrand", "CLYrand_FubQSAR", "CLYrand_FupYrand"),modtab) 
save(modtab, file=paste("CSS_comparision_ToxCast_TestSet_", writesuff, ".RData", sep=""))
save(human.httk.data.complete, file=paste("AER/Chem_and_CSS_table_", writesuff, ".RData", sep=""))
load(paste("AER/Chem_and_CSS_table_", writesuff, ".RData", sep=""))

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
#Tox21.ids <- read.csv("L:/Lab/NCCT_ExpoCast/ExpoCast2019/IngleHTTKQSPR/Summary_for_Mike (Brandall Ingle)/Clearance/4-httk/httk_inputs/Chemical_Summary_151020.csv",stringsAsFactors=F)
#Tox21.ids <- read.csv("/home/rtorne02/Desktop/4-httk_with_Fup/httk_inputs/Chemical_Summary_151020.csv",stringsAsFactors=F)
#Tox21.ids <- read.csv("/home/rtorne02/Desktop/4-httk_with_Fup/httk_inputs/Chemical_Summary_151020.csv",stringsAsFactors=F)
Tox21.ids <- read.csv("AER/Exposure_Tox_Data/Chemical_Summary_190708.csv")

# These are the concentrations that caused activity in excess of the background (Activity Concentration Cutoff=acc); that is,  "hits":
##DED: These are concentrations that trigger some kind of toxicological threshold for any number of Tox asssays
Tox21.acc <- read.csv("AER/Exposure_Tox_Data/Tox21_acc_Matrix_190708.csv")
unique(Tox21.acc$chnm)
dim(Tox21.acc)
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
range(Testsperchem$Count)
dim(Testsperchem[Testsperchem$Count==1,])
dim(Testsperchem[Testsperchem$Count==53,])
Tox21.acc.numeric[Tox21.acc.numeric$variable=="TOX21_DT40",]

#subset to those chemicals with HTS hits:
human.hits <- subset(human.httk.data.complete,CAS %in% Tox21.acc.numeric$casn) #this produces a set of 90 chemicals with effects data

#open exposure estimates
load("AER/Exposure_Tox_Data/chem.preds-2018-11-28.RData")
heuristics.preds=chem.preds

load("AER/Exposure_Tox_Data/NHANES_2018_Summary.RData")
directnhanes.preds=NHANES.obs

#Remember, the inhanes and heuristics are added here to provide empirical exposure data. 

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

Comp1a=c("Human.Clint.Correct", "Human.Funbound.plasma", "IV")
Comp2a=c("Human.Clint.Bin3.MD", "Human.Funbound.plasma", "IV.CLBin3.MD")
Comp3a=c("Human.Clint.QSAR.Bin3.MD", "Fub.RF.pred", "QSAR_FubPred_CLPredBin3.MD")
Comp4a=c("Human.Clint.Yrand", "Fub.RF.pred", "FubPred_CLYrand")
Comp5a=c("Human.Clint.QSAR.Bin3.MD", "Human.Funbound.plasma.Yrand", "FubYrand_CLPRedBin3")
Comp6a=c("Human.Clint.Yrand", "Human.Funbound.plasma.Yrand", "FubYrand_CLYrand")

paramtab1=rbind(Comp1a,Comp2a,Comp3a,Comp4a,Comp5a, Comp6a)
paramtab1=data.frame("Clint_types"=paramtab1[,1], "Funbound_types"=paramtab1[,2], "DoseEquivoutputs"=paramtab1[,3])

for(i in 1:length(paramtab1[1,])){paramtab1[,i]<-as.character(paramtab1[,i])} #to keep them characters instead of factors

#This section calculates the median, 10%, the minimum, and maximum of the values of the toxicological variables included. 
#then, it calculates oral dose equivalents for each of the doses. Note that the doese liseted in in Tox21.acc are apparently some kind
#of log, either natural or 10, though they are currently listed as natura log. 
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
  reference="HTTK2.0.3",
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

save(human.hits, file=paste("DoseEquivalents_Multiple_Scenarios_", writesuff,".RData", sep=""))
#load(paste("DoseEquivalents_Multiple_Scenarios_", suff,"_LinuxBox.RData",sep=""))


###potency increases as the ratio of Q10equivdose to Expousre high gets smaller; we want to order by high potency, so want order ratio to increase
human.hits$AER_IV=human.hits$HTS.Q10.equivdose.IV/human.hits$Exposure.high
human.hits$AER_IV.CLBin3.MD=human.hits$HTS.Q10.equivdose.IV/human.hits$Exposure.high
human.hits$AER_QSAR_FubPred_CLPredBin3.MD<-human.hits$HTS.Q10.equivdose.QSAR_FubPred_CLPredBin3.MD/human.hits$Exposure.high
human.hits$AER_FubPred_CLYrand<-human.hits$HTS.Q10.equivdose.FubPred_CLYrand/human.hits$Exposure.high
human.hits$AER_FubYrand_CLPRedBin3<-human.hits$HTS.Q10.equivdose.FubYrand_CLPRedBin3/human.hits$Exposure.high
human.hits$AER_FubYrand_CLYrand<-human.hits$HTS.Q10.equivdose.FubYrand_CLYrand/human.hits$Exposure.high

human.hits<-human.hits[order(human.hits$AER_IV, decreasing=FALSE),]
human.hits$Compound <- factor(human.hits$Compound, levels=(human.hits$Compound)) #this sorts the compounds by potency, or  
human.hits$CompoundSequence=seq(1,length(human.hits[,1]))
human.hits$CompoundSequenceFact=factor(human.hits$CompoundSequence)

##

par(mfrow=c(2,1))
barplot(log(human.hits$potency_iv)~human.hits$Compound, xlab="Chemical", ylab="AER Ratio", main="AER Ratio: In Vitro")
barplot(log(human.hits$potency_QSAR)~human.hits$Compound, xlab="Chemical", ylab="AER Ratio", main="AER Ratio: Best QSAR")
barplot(log(human.hits$potency_FubPredCLZero)~human.hits$Compound,ylab="AER Ratio", main="AER Ratio: FubPred and CLZero")
barplot(log(human.hits$potency_FubPredCL)~human.hits$Compound,ylab="AER Ratio", main="AER Ratio: FubPred and CLZero")





#This function creates a segment plot showing the actual concentrations of OED and Expected exposure
FigFunction<-function(dataset=human.hits, predset="IV", SourceLabel="In Vitro", filelabel=suff, PredSourceLabel="Opera", filesave=TRUE, sourcelabel=TRUE, xaxislabels=TRUE, subsetsize="all", xlabelsize=12, yaxislabels=TRUE){
if (subsetsize != "all") {
  dataset = dataset[c(1:subsetsize),]
}
Fig <- ggplot()
# Add a range from median to lowest ToxCast equivalent value:

Fig <- Fig + geom_segment(data=dataset,aes(x=Compound,xend=Compound, #This bounds bars for each chemical, low dose to high dose
    y=get(paste("HTS.Low.equivdose.", predset,sep="")),yend=get(paste("HTS.High.equivdose.", predset,sep=""))),size=2,color="red")




# Add points for the median and 10th percentile:
Fig <- Fig + geom_point(data=dataset,aes(x=CompoundSequence,y=get(paste("HTS.Median.equivdose.", predset,sep=""))), shape=3, size=2) #+'s for median equivalent dose 
Fig <- Fig + geom_point(data=dataset,aes(x=CompoundSequence,y=get(paste("HTS.Q10.equivdose.", predset,sep=""))), size=2) #filled circle for Q10 equivalent dose
# Angle the chemical names for readability:
if(xaxislabels==TRUE){
Fig <- Fig + theme(axis.text.x = element_text(angle = 60, hjust = 1,size=xlabelsize), legend.position="none")
# Adjust font sizes:
Fig <- Fig + theme(axis.title.x = element_text(size=16))} else {

Fig<- Fig + theme(axis.title.x=element_blank(), 
                  axis.text.x=element_blank(),
                  axis.ticks.x=element_blank())
  
}



Fig <- Fig + theme(axis.title.y = element_text(size=10))


# Logarithmic scale for y-axis:
#Fig <- Fig + scale_y_log10(limits = c(10^-9,10^3))
Fig <- Fig + scale_y_log10()   
  

# Add a one-sided confidence interval (median to upper 95%) for exposure: 
Fig <- Fig + geom_segment(data=dataset,aes(x=Compound,xend=Compound,
    y=Exposure.high,yend=Exposure.median),size=2,color="blue")
# Add the midpoints for the Exposure estimates:
Fig <- Fig + geom_point(data=dataset,aes(x=Compound,y=Exposure.median),shape=2, size=2) #triangle for median exposure

# Label the axes:
Fig <- Fig + xlab("Chemicals")

if(yaxislabels==TRUE){
Fig <- Fig + ylab(paste("HTS Equivalent Dose (Red) / Predicted Exposure (Blue): in Mg/Kg per Day"))
Fig <- Fig + labs(color="Exposure Prediction/n(Median and Upper 95%)")} else {
Fig <- Fig + ylab(paste(""))
#Fig <- Fig + labs(color="Exposure Prediction/n(Median and Upper 95%)")  
  }
if (sourcelabel==TRUE){
Fig <- Fig + annotate(geom="Text", x=dim(dataset)[1]-0.5, y=max(dataset$HTS.High.equivdose.IV)*0.8, label=SourceLabel, color="black", size=5, hjust=0)
}

print(Fig)
#ggsave(paste("AER_", predset,"_",y,"_",filelabel,".png", sep=""))
if(filesave==TRUE){
ggsave(paste("AER_", predset,"_",filelabel,".png", sep=""))
}
return(Fig)
}

setwd("L:/Lab/NCCT_ExpoCast/ExpoCast2019/IngleHTTKQSPR/Wambaugh/Brandall_paper_update_2019/AER")
#human.hits1<-human.hits[-c(which(is.na(human.hits$Exposure.median))),]
predset=paramtab1[,3]
labellist=c("In Vitro", "In Vitro Bin (3)","QSAR:Fub & CL(3 Bins)","Fub:QSAR & CL:Y Random","Fub:Yrand & CL:QSAR(3 Bins)", "Fub:Yrand & CL:Yrand")
for(i in 1:length(labellist)){ FigFunction(dataset=human.hits, predset=predset[i], SourceLabel=labellist[i], filelabel=suff)}

##Specific Pairs for better figure; wan to limit to the first, say 20 or so; maybe 30(1/3 of graph)
#On Monday; arrange a figure with each of these three graphs that are stacked. 
paramtab1[,3]

#Make stacked plots
p1=FigFunction(dataset=human.hits, predset="IV", SourceLabel="A",  filelabel=suff, filesave=FALSE, sourcelabel=TRUE,xaxislabels=FALSE, subsetsize = 25, xlabelsize = 8, yaxislabels = FALSE)
p2=FigFunction(dataset=human.hits, predset="QSAR_FubPred_CLPredBin3.MD", SourceLabel="B", filelabel=suff, filesave=FALSE, sourcelabel=TRUE,xaxislabels=TRUE, subsetsize = 25, yaxislabels = FALSE)
p3=FigFunction(dataset=human.hits, predset="FubPred_CLZero", SourceLabel="C", filelabel=suff, filesave=FALSE, sourcelabel=TRUE, xaxislabels = TRUE, subsetsize = 25, xlabelsize = 10, yaxislabels = FALSE)

library(grid)
grid.newpage()
grid.draw(rbind(ggplotGrob(p1),ggplotGrob(p2), size = "last"))


##BarCharts: This section creates barcharts of AER, as defined above. 

FigFunction2<-function(dataset=human.hits, predset="IV", SourceLabel="In Vitro", filelabel=suff, PredSourceLabel="Opera", filesave=TRUE, sourcelabel=TRUE, xaxislabels=TRUE, subsetsize="all", xlabelsize=12, yaxislabels=TRUE){
  if (subsetsize != "all") {
    dataset = dataset[c(1:subsetsize),]
  }
  # Add a range from median to lowest ToxCast equivalent value:
  
Fig=ggplot(dataset, aes(x=CompoundSequence, y=log(get(paste("AER_",predset, sep="")))))+
    geom_bar(stat="identity", width=0.1) 
  
  
  # Angle the chemical names for readability:
  if(xaxislabels==TRUE){
    #Fig <- Fig + theme(axis.text.x = element_text(angle = 60, hjust = 1,size=xlabelsize), legend.position="none")
    Fig <- Fig + scale_x_continuous(breaks=seq(0,90,10), limits=c(0,95))
    # Adjust font sizes:
    Fig <- Fig + theme(axis.title.x = element_text(size=16))} else {
      
      Fig<- Fig + theme(axis.title.x=element_blank(), 
                        axis.text.x=element_blank(),
                        axis.ticks.x=element_blank())
      
    }
  
  
  Fig <- Fig + theme(axis.title.y = element_text(size=10))
  
  
  # Label the axes:
  Fig <- Fig + xlab("Chemicals")
  
  if(yaxislabels==TRUE){
    Fig <- Fig + ylab(paste("OED of Q10 Concentration / Concentration of Highest Expected Exposure"))
    } else {
      Fig <- Fig + ylab(paste(""))
      #Fig <- Fig + labs(color="Exposure Prediction/n(Median and Upper 95%)")  
    }
  if (sourcelabel==TRUE){
    Fig <- Fig + annotate(geom="Text", x=1, y=max(log(human.hits$AER_IV))*0.9, label=SourceLabel, color="black", size=5, hjust=0)
  }
  
  print(Fig)
  #ggsave(paste("AER_", predset,"_",y,"_",filelabel,".png", sep=""))
  if(filesave==TRUE){
    ggsave(paste("AER_", predset,"_",filelabel,".png", sep=""))
  }
  return(Fig)
}


#Make All plots

labellist=c("In Vitro", "In Vitro Bin (3)","QSAR:Fub & CL(3 Bins)","Fub:QSAR & CL:Y Random","Fub:Yrand & CL:QSAR(3 Bins)", "Fub:Yrand & CL:Yrand")
for(i in 1:length(labellist)){ FigFunction2(dataset=human.hits, predset=predset[i], SourceLabel=labellist[i], filelabel=suff)}
human.hits$AER

predset=paramtab1[,3]
p4=FigFunction2(dataset=human.hits, predset="IV", SourceLabel="A",  filelabel=suff, filesave=FALSE, sourcelabel=TRUE,xaxislabels=FALSE, subsetsize = "all", xlabelsize = 8, yaxislabels = FALSE)
p5=FigFunction2(dataset=human.hits, predset="QSAR_FubPred_CLPredBin3.MD", SourceLabel="B",  filelabel=suff, filesave=FALSE, sourcelabel=TRUE,xaxislabels=FALSE, subsetsize = "all", xlabelsize = 8, yaxislabels = TRUE)
p6=FigFunction2(dataset=human.hits, predset="FubYrand_CLYrand", SourceLabel="C",  filelabel=suff, filesave=FALSE, sourcelabel=TRUE,xaxislabels=TRUE, subsetsize = "all", xlabelsize = 9, yaxislabels = FALSE)

library(grid)
grid.newpage()
grid.draw(rbind(ggplotGrob(p4),ggplotGrob(p5),ggplotGrob(p6), size = "last"))



#This considers the ratio of the high exposure dose to the Q10 dose; basically the percentage that high exposure dose is to the oral equivalent dose.  
AER          <- human.hits$HTS.Q10.equivdose.IV/human.hits$Exposure.high
AER.bin3.md       <- human.hits$HTS.Q10.equivdose.IV.CLBin3.MD/human.hits$Exposure.high
AER.qsar.bin3.md  <- human.hits$HTS.Q10.equivdose.QSAR_FubPred_CLPredBin3.MD/human.hits$Exposure.high
AER.FubPredandCLYrand    <- human.hits$HTS.Q10.equivdose.FubPred_CLYrand/human.hits$Exposure.high
AER.FubyrandCL3    <- human.hits$HTS.Q10.equivdose.FubYrand_CLPRedBin3/human.hits$Exposure.high
AER.FubYrandCLYrand    <- human.hits$HTS.Q10.equivdose.FubYrand_CLYrand/human.hits$Exposure.high



#This considers the correlation between the in vitro data and the structures(binned), predictions(QSAR), and null checks(Yrand, Zero).
corr.3bin.md       <- cor(AER,AER.bin3.md, method = c("spearman"))        # 0.8959445
corr.qsar.3bin.md  <- cor(AER,AER.qsar.bin3.md, method = c("spearman"))   # 0.6670344
corr.fubyrand.cl3bin<- cor(AER,AER.FubPredandCLYrand, method = c("spearman"))
corr.fubpred.clyrand<- cor(AER,AER.FubyrandCL3, method = c("spearman"))
corr.fubyrand.clyrand<-cor(AER,AER.FubYrandCLYrand, method = c("spearman"))


corr.results=data.frame(corr.3bin.md, corr.qsar.3bin.md, corr.fubyrand.cl3bin,corr.fubpred.clyrand, corr.fubyrand.clyrand)

#write file
write.csv(human.hits, file = paste("Dose_Equiv_and_Pred_Exp_results_ToxCast_" ,suff,".csv",sep=""))
#impact on AER metrics

Results<-list(human.hits, human.httk.data.complete, corr.results)
save(Results, file=paste("AER_results_", suff,".RData",sep=""))

#Exposure is probably driving this; good point. 
#Justify the need to get better exposure estimates. 

names(human.hits)
human.hits$CompoundSequence=seq(1,length(Chemicallist[,1]),1)
Chemicallist=subset(human.hits, select=c("CompoundSequence", "CAS", "DTXSID", "Compound"))

write.csv(Chemicallist, file=paste("ToxCast_AER_ChemicalSet_", suff,".csv", sep = ""))

#Stats
human.hits$Count=1
unique(human.hits$Human.Clint.Bin3.MD)
range(human.hits$Fub.RF.pred)
range(human.hits$Human.Funbound.plasma)



