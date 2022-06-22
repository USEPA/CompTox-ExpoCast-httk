## ----setup--------------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE, fig.width=5, fig.height=4)
<<<<<<< HEAD
library(gdata)
library(httk)
=======
library(httk)
#library(gdata)
>>>>>>> 2ef7501b031b24a4a057d29f4e879e36c92caedf
library(ggplot2)
library(scales)

## ----scientific.notation------------------------------------------------------
scientific_10 <- function(x) {                                  
  out <- gsub("1e", "10^", scientific_format()(x))              
  out <- gsub("\\+","",out)                                     
  out <- gsub("10\\^01","10",out)                               
  out <- parse(text=gsub("10\\^00","1",out))                    
}  

## ----Frank2018data------------------------------------------------------------
chem.table <- Frank2018invivo

## ----ivive.loop---------------------------------------------------------------
for (this.row in 1:dim(chem.table)[1])
{
  this.cas <- chem.table[this.row,"Substance_CASRN"]
  if (tolower(chem.table[this.row,"Species"])=="rodent") 
  {  
    this.species <- "rat"
  } else if (tolower(chem.table[this.row,"Species"])=="rat") 
  {
    this.species <- "rat"
  } else if (tolower(chem.table[this.row,"Species"])=="human")
  {
    this.species <- "human"
  }
  else if (tolower(chem.table[this.row,"Species"])=="mouse") 
  {
    this.species <- "mouse"
  }
  else browser()
  if (chem.table[this.row,"Route"] %in% c("i.p.","s.c.","i.m.")) iv.dose =TRUE
  else if (chem.table[this.row,"Route"]=="oral") iv.dose = F
  else browser()
  this.dose <- chem.table[this.row,"Dose"]
  this.days <- chem.table[this.row,"Days"]
# Make sure the dose units are in mg/kg body weight:
  if (regexpr("ug",chem.table[this.row,"Dose.Units"])!=-1) 
  {
    this.dose <- this.dose/1000
  }
  if (regexpr("/kg",chem.table[this.row,"Dose.Units"])==-1) 
  {
    this.dose <- this.dose/0.25
  }
# Here we run the HTTK PBPK Model:
  out <- suppressWarnings(solve_pbtk(chem.cas=this.cas,
           dose=this.dose,
           species=this.species,
# This was used in 2017 but I don't agree with it anymore:
#           restrictive.clearance=FALSE,
           days=this.days,
           iv.dose=iv.dose,
           default.to.human=TRUE))
<<<<<<< HEAD
=======

>>>>>>> 2ef7501b031b24a4a057d29f4e879e36c92caedf
# Record the Cmax and the AUC:
  chem.table[this.row,"Cmax"] <- max(out[,"Cplasma"])
  chem.table[this.row,"AUC"] <- max(out[,"AUC"])
}

## ----Frank2018.Fig6-----------------------------------------------------------
Fig.AUC <- ggplot(data=chem.table) +
  geom_segment(color="grey",aes(x=AUC,y=Lower.95..CI,xend=AUC,yend=Higher.95..CI))+    
#  geom_point(aes(x=AUC,y=Critical.concentration,color="Chemical"))+ 
   geom_text(aes(x=AUC,y=Critical.concentration,label=Compound.abbrev,color=Chemical)) +
   scale_y_log10(label=scientific_10,limits=c(10^-7,100)) +
   scale_x_log10(label=scientific_10,limits=c(10^-7,100)) +
    annotation_logticks() + 
    geom_abline(slope=1, intercept=0) + 
    geom_abline(slope=1, intercept=1,linetype="dashed") + 
    geom_abline(slope=1, intercept=-1,linetype="dashed") + 
    xlab(expression(paste(italic("In vivo")," AUC estimated with HTTK (uM*day)"))) + 
    ylab(expression(paste(italic("In vitro")," predicted Critical Conc. (uM)"))) +
    scale_color_brewer(palette="Set2") + 
    theme_bw()  +
    theme(legend.position="bottom")
    
print(Fig.AUC)

