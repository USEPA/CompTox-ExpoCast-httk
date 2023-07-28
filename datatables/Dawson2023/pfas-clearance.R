rm(list = ls())
packages=c("readxl", "httk")
sapply(packages, require, character.only=TRUE)

hl.table <- as.data.frame(read_excel("S2_Dawson_PFAS_HL_102422.xlsx", sheet=2))
for (this.row in 1:dim(hl.table)[1])
{
  hl.table[this.row, "HalflifeHours"] <- as.numeric(hl.table[this.row,"Mean"])
  if (!is.na(hl.table[this.row, "Orignal Unit"]))
  {
    if (hl.table[this.row,"Orignal Unit"]=="Yrs") 
      hl.table[this.row, "HalflifeHours"] <-
      hl.table[this.row, "HalflifeHours"]*365*24
    if (hl.table[this.row,"Orignal Unit"]=="Days") 
      hl.table[this.row, "HalflifeHours"] <-
      hl.table[this.row, "HalflifeHours"]*24
  }
}

Vd.table <- as.data.frame(read_excel("S2_Dawson_PFAS_HL_102422.xlsx", sheet=9))
Vd.table[Vd.table$Vd_Unit=="mL/kg","Vd"] <- 
  Vd.table[Vd.table$Vd_Unit=="mL/kg","Vd"]/1000
Vd.table[Vd.table$Vd_Unit=="mL/kg","Vd_Unit"] <- "L/kg"


Cl.table <- data.frame()
for (this.chem in unique(hl.table$DTXSID))
{
  this.chem.subset <- subset(hl.table, DTXSID==this.chem)
  for (this.species in unique(this.chem.subset$Species))
  {
    for (this.sex in c("Female","Male"))
    {
      this.subset <- subset(this.chem.subset, Species == this.species &
                            Sex == this.sex)
      if (dim(this.subset)[1]>0)
      {
        this.hl <- signif(median(this.subset[,"HalflifeHours"]), 4)
        this.row <- data.frame(DTXSID=this.chem,
                               Species=this.species,
                               Sex=this.sex,
                               HalfLifeHours=this.hl,
                               HlReference=paste(unique(this.subset$Source),
                                                 collapse=", "),
                               VdLpkgbw = 0.205,
                               VdReference="Dawson et al. 2023")
        Cl.table <- rbind(Cl.table,this.row)
      }
    }
  }
}
for (this.chem in unique(Vd.table$DTXSID))
{
  this.chem.subset <- subset(Vd.table, DTXSID==this.chem)
  for (this.species in unique(this.chem.subset$Species))
  {
    for (this.sex in c("Female","Male"))
    {
      this.subset <- subset(this.chem.subset, Species == this.species &
                            Sex == this.sex)
      if (dim(this.subset)[1]>0)
      {
        this.Vd <- signif(median(this.subset[,"Vd"]), 4)
        Cl.table[Cl.table$DTXSID==this.chem &
                 Cl.table$Species==this.species &
                 Cl.table$Sex==this.sex, "VdLpkgbw"] <- this.Vd
        Cl.table[Cl.table$DTXSID==this.chem &
                 Cl.table$Species==this.species &
                 Cl.table$Sex==this.sex, "VdReference"] <-
                 paste(unique(this.subset$Source),
                   collapse=", ")     
      }
    }
  }
}
Cl.table$Kelimph <- signif(log(2)/Cl.table$HalfLifeHours, 4)
Cl.table$ClLphpkgbw <- signif(Cl.table$Kelimph * Cl.table$VdLpkgbw, 4)

write.csv(Cl.table,file="PFAS-Clearance.txt",row.names=FALSE)
