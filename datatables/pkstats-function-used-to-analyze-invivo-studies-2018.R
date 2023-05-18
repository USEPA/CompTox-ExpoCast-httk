#by John Wambaugh
pkstats <- function(in.table,Fbio.table=NULL,default.to.human=T)
{
  out.table <- NULL
  for (this.species in unique(in.table$Species))
  {
    print(this.species)
    this.species.subset <- subset(in.table,Species==this.species)
    for (this.reference in unique(this.species.subset$Reference))
    {
      print(this.reference)
      this.reference.subset <- subset(this.species.subset,Reference==this.reference)
      for (this.compound in unique(this.reference.subset$Compound))
      {
        print(this.compound)
        this.chemical.subset <- subset(this.reference.subset,Compound==this.compound&!is.na(Value))
        this.cas <- this.chemical.subset$CAS[1]
        for (this.route in unique(this.chemical.subset$Route))
        {
          this.route.subset <- subset(this.chemical.subset,Route==this.route)
          for (this.dose in unique(this.route.subset$Dose))
          {
            this.dose.subset <- subset(this.route.subset,Dose==this.dose)
            times <- unique(this.dose.subset$Time)
            delta.time <-times[1]
            delta.value <- mean(subset(this.dose.subset,Time==times[1])$Value)
            for (i in 2:length(times))
            {
              delta.time <- c(delta.time,times[i]-times[i-1])
              delta.value <- c(delta.value,(mean(subset(this.dose.subset,Time==times[i])$Value)+mean(subset(this.dose.subset,Time==times[i-1])$Value))/2)
            }
#            delta.time <- delta.time[!is.na(delta.value)]
#            delta.value <- delta.value[!is.na(delta.value)]
            this.row <- data.frame(Reference=this.reference,Species=this.species,Compound=this.compound,CAS=this.cas,Route=this.route,dose=this.dose,dose.units="mg/kg",time=max(this.dose.subset$Time),time.units="h",AUC=delta.value%*%delta.time,AUC.units="mg*h/L",Cmax=max(delta.value,na.rm=T),Cmax.units="mg/L",stringsAsFactors=F)
            if (!is.null(Fbio.table))
            {
              if (this.compound %in% Fbio.table$Compound)
              {
                if (dim(Fbio.table[Compound==this.compound,"SelectedFbio"])[1]==1)
                {
                  this.row["Fbio"] <- as.numeric(Fbio.table[Compound==this.compound,"SelectedFbio"])
                } else {
                  this.row["Fbio"] <- as.numeric(Fbio.table[Compound==this.compound&Reference==this.reference,"SelectedFbio"])
                }
              } else this.row["Fbio"] <- NA
            }
            else this.row["Fbio"] <- NA
      #      temp <- calc_stats(chem.cas=this.cas,species="Rat",default.to.human=T,dose=this.dose,days=this.row$time/24,output.units="mg/L",model="1compartment")
            temp <- calc_stats(chem.cas=this.cas,species=this.species,default.to.human=default.to.human,dose=this.dose,days=this.row$time/24,output.units="mg/L",model="1compartment",iv.dose=this.route=="iv")
            this.row["AUC.pred"] <- temp$AUC
            this.row["Cmax.pred"] <- temp$peak
            if (!is.na(this.row["Fbio"]))
            {
              parms <- parameterize_1comp(chem.cas=this.cas,species=this.species,default.to.human=default.to.human)
              if (dim(Fbio.table[Compound==this.compound,"SelectedFbio"])[1]==1)
              {
                parms$Fgutabs <- as.numeric(Fbio.table[Compound==this.compound,"SelectedFbio"])
              } else {
                parms$Fgutabs <- as.numeric(Fbio.table[Compound==this.compound&Reference==this.reference])
              }
              temp <- calc_stats(parameters=parms,species=this.species,dose=this.dose,days=this.row$time/24,output.units="mg/L",model="1compartment",iv.dose=this.route=="iv")
              this.row["AUC.pred.Fbio"] <- temp$AUC
              this.row["Cmax.pred.Fbio"] <- temp$peak
            } else {
              this.row["AUC.pred.Fbio"] <- NA
              this.row["Cmax.pred.Fbio"] <- NA
            }
            out.table <- rbind(out.table,this.row)
          }
        }
      }
    }
  }
  return(out.table)
}