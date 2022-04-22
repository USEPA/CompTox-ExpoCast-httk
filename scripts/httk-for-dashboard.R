library(httk)
library(parallel)

# Clear the memory:
rm(list=ls())

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
NUM.CPU <- 2
 
# Add the in silico predictions:
load_sipes2017()
 
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
  all.ids <- unique(c(all.ids,HTTK.data.list[[this.species]]$DTXSID))
}

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
  all.ids
  ) {
  # Total number of unique chemicals:
  num.chems <- length(all.ids)
  
  # Initialize the local (chemical-specific) dashboard table:
  dashboard.table <- NULL
  
  print(paste(this.id,"-",which(this.id==all.ids),"of",num.chems))
  for (this.species in species.list) {
  print(this.species)
    if (this.id %in% HTTK.data.list[[this.species]]) {
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
          Data.Source.Species=ifelse(default.to.human,"Human","Rat"))
  # Clint:
        if (this.param == "Clint") {
          clint.ref <- subset(chem.physical_and_invitro.data,DTXSID==this.id)[,
            paste(this.row["Data.Source.Species"],"Clint.Reference",sep=".")]
          if (is.na(clint.ref))
          {
            this.row$Measured <- HTTK.human[HTTK.human$DTXSID==this.id,4]
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
        } else if (this.param == "Fup") {
          fup.ref <- subset(chem.physical_and_invitro.data,DTXSID==this.id)[,
            paste(this.row["Data.Source.Species"],"Funbound.plasma.Reference",sep=".")]
          if (is.na(fup.ref)) {
            this.row$Measured <- HTTK.human[HTTK.human$DTXSID==this.id,5]
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
            this.row$Predicted <- try(calc_vdist(dtxsid=this.id,
              default.to.human=default.to.human,
              species=this.species))
            this.row$Model <- "1compartment"
          }
          dashboard.table <- rbind(dashboard.table, this.row)
  # TK.Half.Life:
        } else if (this.param == "TK.Half.Life") {
          #check for Fup >0 (can't do Vd otherwise):
          if (HTTK.data[HTTK.index,5] > 0)
          {
            # need cas because of bug with DTXSID's:
            this.cas <- HTTK.data[HTTK.index,"CAS"]
            this.row$Predicted <- try(calc_half_life(chem.cas=this.cas,
                default.to.human=default.to.human,
                species=this.species)) 
            this.row$Model <- "1compartment"
          }
          dashboard.table <- rbind(dashboard.table, this.row)
  # Days to Css:
        } else if (this.param == "Days.Css") {
          #check for Fup >0 (can't do pbtk otherwise):
          if (HTTK.data[HTTK.index,5] > 0)
          {
            this.row$Predicted <- try(calc_css(dtxsid=this.id,
              species=this.species,
              default.to.human=default.to.human)$the.day)
            this.row$Model <- "PBTK"
          }
          dashboard.table <- rbind(dashboard.table, this.row)
  # Css:
        } else if (this.param == "Css") {
          for (this.model in model.list)
          {
            if (HTTK.data[HTTK.index,5] > 0 | this.model=="3compartmentss")
            {
              this.row$Model <- this.model
              parameterize.arg.list = list(
                default.to.human = default.to.human, 
                clint.pvalue.threshold = 0.05,
                restrictive.clearance = TRUE, 
                regression = TRUE)
              # For reproducible pseudo-random numbers:
              set.seed(RANDOM.SEED)
              this.css <-try(calc_mc_css(chem.cas=this.cas,
                which.quantile=WHICH.QUANTILES,
                samples=NUM.SAMPLES,
                output.units="mg/L",
                species=this.species,
                model=this.model,
                parameterize.arg.list=parameterize.arg.list))
              if (!inherits(this.css, "try-error"))
              {
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
cl <- makeCluster(NUM.CPU)
# Load httk on all cores:
clusterEvalQ(cl, library(httk))
# Define the table creator function on all cores:
clusterExport(cl, "make.ccd.table")
# Share data with all cores:
clusterExport(cl, c(
  "HTTK.data.list",
  "SPECIES.LIST",
  "MODELS.LIST",
  "param.list",
  "units.list",
  "all.ids"))

# Create a list with one table per chemical:
dashboard.list <- clusterApply(cl,all.ids,function(x)
  make.ccd.table(
    this.id=x,
    HTTK.data.list=HTTK.data.list,
    species.list=SPECIES.LIST,
    model.list=MODELS.LIST,
    param.list=param.list,
    units.list=units.list,
    all.ids=all.ids
    ))

stopCluster(cl)

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











