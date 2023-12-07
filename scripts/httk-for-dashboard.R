library(httk)
library(parallel)
library(data.table)

# Clear the memory:
rm(list=ls())

prev.table <- read.csv("Dashboard-HTTK-CssunitsmgpL.txt",sep="\t")
write.table(
  prev.table,
  file=paste(
    "Dashboard-HTTK-CssunitsmgpL-previous.txt",sep=""),
  row.names=F,
  quote=F,
  sep="\t")
  
#setwd("L:/Lab/NCCT_ExpoCast/ExpoCast2019/HTTKDataTable/")
# Number of samples for Monte Carlo, default is 1000, but this is more stable:
NUM.SAMPLES <- 1e4
 
# For reproducible Monte Carlo
RANDOM.SEED <- 123456
          
# Which quantiles to provide from Monte Carlo:
WHICH.QUANTILES <- c(0.5,0.95)          
 
# For which species do we want predictions:
SPECIES.LIST <- c("Human","Rat")

# For which TK models do we want Css predictions:
MODELS.LIST <- c("3compartmentss","PBTK")

# How many processors are available for parallel computing:
NUM.CPU <- 6
 
# Add the in silico predictions:
# Categorical QSPR:
load_dawson2021()
# ADmet Predictor:
load_sipes2017()
# Machine learning model:
load_pradeep2020()
# Caco-2 QSPR:
load_honda2023()
 
# Organize HTTK data by species:
HTTK.data.list <- list()
all.ids <- NULL
for (this.species in SPECIES.LIST)
{
  HTTK.data.list[[this.species]] <- get_cheminfo(
    info=c(
      "Compound",
      "CAS",
      "DTXSID",
      "Clint",
      "Funbound.plasma"),
    fup.lod.default = 0,
    median.only=TRUE,
    species=this.species)
# Create a master list of the chemical DTXSID's:
  all.ids <- sort(unique(c(all.ids,HTTK.data.list[[this.species]]$DTXSID)))
}

# temporilay make it run fast:
ivpkfit <- read.csv("invivoPKfit-params.for.dashboard.txt")
short.list <- ivpkfit$Chemical[1:25]
all.ids <- all.ids[all.ids %in% short.list]


# We want one parameter per line, but the code is pretty different wrt how we
# retrieve/calculate these values:
param.list <- c("Clint","Fup","Vd","Days.Css","TK.Half.Life","Css")
units.list <- list()
units.list[["Clint"]] <- "uL/min/million hepatocytes"
units.list[["Fup"]] <- ""
units.list[["Vd"]] <- "L/kg"
units.list[["Days.Css"]] <- "Days"
units.list[["TK.Half.Life"]] <- "hours"
units.list[["Css"]] <- "mg/L"



# Function to create all the rows of info for a particular chemical:
make.ccd.table <- function(
  this.id,
  HTTK.data.list,
  species.list,
  model.list,
  param.list,
  units.list,
  all.ids,
  RANDOM.SEED,
  which.quantiles,
  num.samples
  ) {
  # Total number of unique chemicals:
  num.chems <- length(all.ids)
  
  # Initialize the local (chemical-specific) dashboard table:
  dashboard.table <- NULL
  
  print(paste(this.id,"-",which(this.id==all.ids),"of",num.chems))
  for (this.species in species.list) {
  print(this.species)
    if (this.id %in% HTTK.data.list[[this.species]]$DTXSID) {
      HTTK.data <- HTTK.data.list[[this.species]]
      default.to.human=FALSE
    } else {
      HTTK.data <- HTTK.data.list[["Human"]]
      default.to.human=TRUE
    }
    HTTK.index <- which(HTTK.data$DTXSID==this.id)
# Test to see if we found this chemical in HTTK.data:
    if (length(HTTK.index) >0) {
      for (this.param in param.list) {
        this.row <- data.frame(
          DTXSID=this.id,
          Parameter=this.param,
          Measured=NA,
          Predicted=NA,
          Units= units.list[[this.param]],
          Model=NA,
          Reference=NA,
          Percentile=NA,
          Species=this.species,
          Data.Source.Species=ifelse(this.species=="Human" |
                                     default.to.human,"Human","Rat"))
  # Clint:
        if (this.param == "Clint" & !default.to.human) {
          clint.ref <- subset(chem.physical_and_invitro.data,DTXSID==this.id)[,
            paste(this.row["Data.Source.Species"],"Clint.Reference",sep=".")]
          if (is.na(clint.ref))
          {
            this.row$Measured <- HTTK.data.list[["Human"]][
              HTTK.data.list[["Human"]]$DTXSID==this.id,4]
            this.row$Data.Source.Species <- "Human"
          } else if (clint.ref == "Sipes 2017") {
            this.row$Predicted <- HTTK.data[HTTK.index,4]
            this.row$Model <- "ADMet"
          } else {
            this.row$Measured <- HTTK.data[HTTK.index,4]
          }
          this.row$Reference <- clint.ref
          dashboard.table <- rbind(dashboard.table, this.row)
  # Fup:
        } else if (this.param == "Fup" & !default.to.human) {
          fup.ref <- subset(chem.physical_and_invitro.data,DTXSID==this.id)[,
            paste(this.row["Data.Source.Species"],"Funbound.plasma.Reference",sep=".")]
          if (is.na(fup.ref)) {
            this.row$Measured <- HTTK.data.list[["Human"]][
              HTTK.data.list[["Human"]]$DTXSID==this.id,5]
            this.row$Data.Source.Species <- "Human"        
          } else if (fup.ref == "Sipes 2017") {
            this.row$Predicted <- HTTK.data[HTTK.index,5]
            this.row$Model <- "ADMet"
          } else {
            this.row$Measured <- HTTK.data[HTTK.index,5]
          }
          this.row$Reference <- fup.ref
          dashboard.table <- rbind(dashboard.table, this.row)
  # Vd:
        } else if (this.param == "Vd") {
          #check for Fup >0 (can't do Vd otherwise):
          if (HTTK.data[HTTK.index,5] > 0)
          {
            this.vd <- try(calc_vdist(dtxsid=this.id,
              default.to.human=default.to.human,
              species=this.species))
            if (!inherits(this.vd, "try-error")) { 
              this.row$Predicted <- this.vd
              this.row$Model <- "1compartment"
              dashboard.table <- rbind(dashboard.table, this.row)
            }
          }
  # TK.Half.Life:
        } else if (this.param == "TK.Half.Life") {
          #check for Fup >0 (can't do Vd otherwise):
          if (HTTK.data[HTTK.index,5] > 0) {
            # need cas because of bug with DTXSID's:
            this.cas <- HTTK.data[HTTK.index,"CAS"]
            this.tkhalflife <- try(calc_half_life(chem.cas=this.cas,
                default.to.human=default.to.human,
                species=this.species))
            if (!inherits(this.tkhalflife, "try-error")) { 
              this.row$Model <- "1compartment"
              this.row$Predicted <- this.tkhalflife
              dashboard.table <- rbind(dashboard.table, this.row)
            }
          }
  # Days to Css:
        } else if (this.param == "Days.Css") {
          #check for Fup >0 (can't do pbtk otherwise):
          if (HTTK.data[HTTK.index,5] > 0) {
            this.dayscss <- try(calc_css(dtxsid=this.id,
              species=this.species,
              default.to.human=default.to.human)$the.day)
            if (!inherits(this.dayscss, "try-error")) { 
              this.row$Predicted <- this.dayscss
              this.row$Model <- "PBTK"
              dashboard.table <- rbind(dashboard.table, this.row)
            }
          }
  # Css:
        } else if (this.param == "Css") {
          for (this.model in model.list)
          {
            if (HTTK.data[HTTK.index,5] > 0 | this.model=="3compartmentss") {
              this.row$Model <- this.model
              parameterize.arg.list = list(
                default.to.human = default.to.human, 
                clint.pvalue.threshold = 0.05,
                restrictive.clearance = TRUE, 
                regression = TRUE)
              # For reproducible pseudo-random numbers:
              set.seed(RANDOM.SEED)
              this.css <-try(calc_mc_css(chem.cas=this.cas,
                which.quantile=which.quantiles,
                samples=num.samples,
                output.units="mg/L",
                species=this.species,
                model=this.model,
                parameterize.arg.list=parameterize.arg.list))
              if (!inherits(this.css, "try-error")) {
                for (this.quantile in names(this.css))
                {
                  this.row$Predicted <- this.css[this.quantile]
                  this.row$Percentile <- this.quantile
                  dashboard.table <- rbind(dashboard.table, this.row)
                }
              }
            }
          }     
        }
      }
    }
  }
  return(dashboard.table)
}

# Create a multicore cluster:
cl <- parallel::makeCluster(detectCores()-1)

# Load httk on all cores:
clusterEvalQ(cl, library(httk))
# Clear memory all cores:
clusterEvalQ(cl, rm(list=ls()))
# Load ADMet predicitons:
clusterEvalQ(cl, load_sipes2017())
# Define the table creator function on all cores:
clusterExport(cl, "make.ccd.table")
# Share data with all cores:
clusterExport(cl, c(
  "HTTK.data.list",
  "SPECIES.LIST",
  "MODELS.LIST",
  "param.list",
  "units.list",
  "all.ids",
  "RANDOM.SEED",
  "WHICH.QUANTILES",
  "NUM.SAMPLES"))

# Create a list with one table per chemical:
dashboard.list <- clusterApply(cl,all.ids,function(x)
  make.ccd.table(
    this.id=x,
    HTTK.data.list=HTTK.data.list,
    species.list=SPECIES.LIST,
    model.list=MODELS.LIST,
    param.list=param.list,
    units.list=units.list,
    all.ids=all.ids,
    RANDOM.SEED=RANDOM.SEED,
    which.quantiles=WHICH.QUANTILES,
    num.samples=NUM.SAMPLES
    ))

stopCluster(cl)
# Combine all the individual tables into a single table:
dashboard.table <- rbindlist(dashboard.list)

Css.units <- "mgL"
httk.version <- sessionInfo()$otherPkgs$httk$Version
output.date <- Sys.Date()

save(Css.units,httk.version,output.date,file="Dashboard-HTTK-stamp.RData")

# Convert to data.frame:
dashboard.table <- as.data.frame(dashboard.table)

#
#
# LOAD INVIVOPKFIT PARAMETERS
#
#

ivpkfit <- read.csv("invivoPKfit-params.for.dashboard.txt")

for (this.parameter in c(
                          "Vdist",
                          "Fgutabs",
                          "kgutabs",
                          "Thalf",
                          "Css"))
  {
    this.param.data <- subset(ivpkfit, !is.na(this.parameter))
    for (this.chem in unique(this.param.data$Chemical))
    {
      if (this.parameter == "Vdist") param.name <- "Vd"
      else if (this.parameter == "Thalf") param.name <- "TK.Half.Life"
      else if (this.parameter == "Fgutabs") param.name <- "Fsysbio"
      else param.name <- this.parameter
      
      for (this.species in subset(this.param.data, Chemical==this.chem)$Species)
      {
        ivpkfit.row <- which(ivpkfit$Chemical==this.chem &
                             tolower(ivpkfit$Species) == tolower(this.species))
        if (!is.na(ivpkfit[ivpkfit.row, this.parameter]))
        {
          dashboard.row <- dashboard.table$DTXSID==this.chem &
                      tolower(dashboard.table$Species) == tolower(this.species) &
                      tolower(dashboard.table$Parameter) == tolower(param.name)
          if (any(dashboard.row)) {
            dashboard.row <- which(dashboard.row)
          } else {
            dashboard.row <- dim(dashboard.table)[1]+1
            dashboard.table[dashboard.row,"DTXSID"] <- this.chem
            dashboard.table[dashboard.row,"Species"] <- this.species
            dashboard.table[dashboard.row,"Parameter"] <- param.name
            if (param.name == "Vd") {
              dashboard.table[dashboard.row, "Units"] <- "L/kg"
            } else if (param.name == "TK.Half.Life") {
              dashboard.table[dashboard.row, "Units"] <- "hours"
            } else if (param.name == "Css") {
              dashboard.table[dashboard.row, "Units"] <- "mg/L"
            } else if (param.name == "kgutabs") {
              dashboard.table[dashboard.row, "Units"] <- "1/h"
            } else if (param.name == "Fsysbio") {
              dashboard.table[dashboard.row, "Units"] <- "fraction"
            }
          }
          for (this.ref in unique(dashboard.table[dashboard.row,"Reference"]))
          {
            if (!is.na(this.ref))
            {
              dashboard.row.ref <- dashboard.table$DTXSID==this.chem &
                tolower(dashboard.table$Species) == tolower(this.species) &
                tolower(dashboard.table$Parameter) == tolower(param.name)
                dashboard.table$Reference == this.ref
              dashboard.table[dashboard.row.ref,"Reference"] <- paste(
                this.ref,
                ivpkfit[ivpkfit.row,"Ref"])                                             
            } else dashboard.table[dashboard.row,"Reference"] <-
              ivpkfit[ivpkfit.row,"Ref"]
            dashboard.table[dashboard.row,"Measured"] <- ivpkfit[ivpkfit.row,
                            this.parameter]
        }
    }  
  }
}  


#
#
# Replace references with DOI:
#
#
dashboard.table[is.na(dashboard.table[,"Reference"]), 
                "Reference"] <- "temp"
dashboard.table[dashboard.table[,"Reference"]=="Sipes 2017", 
                "Reference"] <- "https://doi.org/10.1021/acs.est.7b00650"
dashboard.table[dashboard.table[,"Reference"]=="Wambaugh 2019", 
                "Reference"] <- "https://doi.org/10.1093/toxsci/kfz205"
dashboard.table[dashboard.table[,"Reference"]=="Wetmore 2013", 
                "Reference"] <- "https://doi.org/10.1093/toxsci/kft012"
dashboard.table[dashboard.table[,"Reference"]=="Lombardo 2018", 
                "Reference"] <- "https://doi.org/10.1124/dmd.118.082966"
dashboard.table[dashboard.table[,"Reference"]=="Paini 2020", 
                "Reference"] <- "https://data.europa.eu/89h/a2ff867f-db80-4acf-8e5c-e45502713bee"
dashboard.table[dashboard.table[,"Reference"]=="Wood 2017", 
                "Reference"] <- "https://doi.org/10.1124/dmd.117.077040"
dashboard.table[dashboard.table[,"Reference"]=="Honda 2019", 
                "Reference"] <- "https://doi.org/10.1371/journal.pone.0217564"
dashboard.table[dashboard.table[,"Reference"]=="Wetmore 2015", 
                "Reference"] <- "https://doi.org/10.1093/toxsci/kfv171"
dashboard.table[dashboard.table[,"Reference"]=="Wetmore 2012", 
                "Reference"] <- "https://doi.org/10.1093/toxsci/kfr254"
dashboard.table[dashboard.table[,"Reference"]=="Paixao 2012", 
                "Reference"] <- "https://doi.org/10.1016/j.ijpharm.2012.03.019"
dashboard.table[dashboard.table[,"Reference"]=="Tonnelier 2012", 
                "Reference"] <- "https://doi.org/10.1007/s00204-011-0768-0"
dashboard.table[dashboard.table[,"Reference"]=="Ito 2004", 
                "Reference"] <- "https://doi.org/10.1023/B:PHAM.0000026429.12114.7d"
dashboard.table[dashboard.table[,"Reference"]=="Dawson 2021", 
                "Reference"] <- "https://doi.org/10.1021/acs.est.0c06117"
dashboard.table[dashboard.table[,"Reference"]=="Obach 1999", 
                "Reference"] <- "Obach, R. S. (1999) Drug Metabolism and Disposition 27(11), 1350-9"
dashboard.table[dashboard.table[,"Reference"]=="Pearce 2017", 
                "Reference"] <- "https://doi.org/10.18637%2Fjss.v079.i04"
dashboard.table[dashboard.table[,"Reference"]=="Uchimura 2010", 
                "Reference"] <- "https://doi.org/10.1002/bdd.711"
dashboard.table[dashboard.table[,"Reference"]=="Gulden 2002", 
                "Reference"] <- "https://doi.org/10.1016/S0300-483X(02)00085-9"
dashboard.table[dashboard.table[,"Reference"]=="ECVAM", 
                "Reference"] <- "https://data.europa.eu/89h/a2ff867f-db80-4acf-8e5c-e45502713bee"
dashboard.table[dashboard.table[,"Reference"]=="Shibata 2002", 
                "Reference"] <- "https://doi.org/10.1124/dmd.30.8.892"
dashboard.table[dashboard.table[,"Reference"]=="McGinnity 2004", 
                "Reference"] <- "https://doi.org/10.1124/dmd.104.000026"
dashboard.table[dashboard.table[,"Reference"]=="Linakis 2020", 
                "Reference"] <- "https://doi.org/10.1038/s41370-020-0238-y"
dashboard.table[dashboard.table[,"Reference"]=="Lau2002", 
                "Reference"] <- "https://doi.org/10.1124/dmd.30.12.1446"
dashboard.table[dashboard.table[,"Reference"]=="Naritomi 2003", 
                "Reference"] <- "https://doi.org/10.1124/dmd.31.5.580"
dashboard.table[dashboard.table[,"Reference"]=="Brown 2007", 
                "Reference"] <- "https://doi.org/10.1124/dmd.106.011569"
dashboard.table[dashboard.table[,"Reference"]=="Ito/Riley", 
                "Reference"] <- "https://doi.org/10.1124/dmd.105.0042595"
dashboard.table[dashboard.table[,"Reference"]=="Sternbeck 2012", 
                "Reference"] <- "https://doi.org/10.3109/00498254.2012.669080"
dashboard.table[dashboard.table[,"Reference"]=="Pirovano 2016", 
                "Reference"] <- "https://doi.org/10.1016/j.etap.2016.01.017"
dashboard.table[dashboard.table[,"Reference"]=="Jones 2017", 
                "Reference"] <- "https://doi.org/10.1124/dmd.117.077396"
dashboard.table[dashboard.table[,"Reference"]=="temp", 
                "Reference"] <- NA

write.table(
  dashboard.table,
  file=paste(
    "Dashboard-HTTK-CssunitsmgpL.txt",sep=""),
  row.names=F,
  quote=F,
  sep="\t")


#
#
# CREATE FIGURE SHOWNG CHANGE IN MONTE CARLO Css FROM PREVIOUS VERSION
#
#

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

tmp <- merge(subset(dashboard.table, Parameter=="Css" & 
                    Model=="3compartmentss" & 
                    Species=="Human" & 
                    Percentile=="95%"),
             subset(prev.table, Parameter=="Css" & 
                    Model=="3compartmentss" & 
                    Species=="Human" & 
                    Percentile=="95%"),
             all.x=TRUE, 
             by=c("DTXSID","Parameter","Model","Species","Percentile"))
tmp$Change <- tmp$Predicted.x/tmp$Predicted.y

library(ggplot2)
library(scales)
scientific_10 <- function(x) {                                  
  out <- gsub("1e", "10^", scientific_format()(x))              
  out <- gsub("\\+","",out)                                     
  out <- gsub("10\\^01","10",out)                               
  out <- parse(text=gsub("10\\^00","1",out))                    
}  

Fig <- ggplot(tmp, aes(x=Change)) + 
         geom_histogram() + 
         scale_x_log10(label=scientific_10) +
         scale_y_log10(label=scientific_10) +
         ggtitle(paste0("httk v",httk.version," Change from Previous Version"))+
         ylab("Number of Chemicals") +
         xlab("Fold Change in calc_mc_css Prediction") +
         geom_vline(xintercept=1,linetype="dashed", color="blue", linewidth=2) +
         geom_vline(xintercept=0.1,linetype="dotted", color="blue", linewidth=1) +
         geom_vline(xintercept=10,linetype="dotted", color="blue", linewidth=1)
         
         
png("calc-mc-css-fold-change.png")         
print(Fig)
dev.off()

tmp <- tmp[,colnames(tmp)[c(1:5,7:8,12:13,16)]]
colnames(tmp) <- gsub("x","prev",colnames(tmp))
colnames(tmp) <- gsub("y","new",colnames(tmp))
write.table(
  tmp,
  file=paste(
    "Dashboard-HTTK-CssunitsmgpL-change.txt",sep=""),
  row.names=F,
  quote=F,
  sep="\t")







