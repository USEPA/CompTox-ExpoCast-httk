library(gdata)
library(ggplot2)

setwd("C:/Users/jwambaug/aylward")

MFdata1 <- read.xls("Cord blood graphs - PCBs-DTXSID.xlsx",stringsAsFactors=F,skip=2)
MFdata1 <- MFdata1[,c(4,2,1,9:27)]

MFdata2 <- read.xls("Cord blood graphs - brominated compounds.xlsx",stringsAsFactors=F,skip=3)
MFdata2 <- MFdata2[,c(3,2,1,5:23)]
colnames(MFdata1)[14] <- "Lipid.adjusted."
colnames(MFdata2)[11] <- "Study.size"
colnames(MFdata1)[!(colnames(MFdata1)%in%colnames(MFdata2))]
colnames(MFdata2)[!(colnames(MFdata2)%in%colnames(MFdata1))]


MFdata3 <- read.xls("Cord blood graphs - Dioxins and Furans.xlsx",stringsAsFactors=F,skip=3)
MFdata3 <- MFdata3[,c(3,2,1,5,7:24)]
colnames(MFdata1)[!(colnames(MFdata1)%in%colnames(MFdata3))]
colnames(MFdata3)[!(colnames(MFdata3)%in%colnames(MFdata1))]



MFdata4 <- read.xls("Cord blood graphs - PFCs.xlsx",stringsAsFactors=F,skip=3)
MFdata4 <- MFdata4[,c(3,2,1,4:13,15:21,23)]
MFdata4$Lipid.adjusted. <-NA
colnames(MFdata4)[21] <- "Study.size"
colnames(MFdata1)[!(colnames(MFdata1)%in%colnames(MFdata4))]
colnames(MFdata4)[!(colnames(MFdata4)%in%colnames(MFdata1))]


MFdata5 <- read.xls("Organochlorine pesticides.xlsx",stringsAsFactors=F,skip=4)
MFdata5 <- MFdata5[,c(3,2,1,4,6:23)]
colnames(MFdata5)[5] <- "Lipid.adjusted."
colnames(MFdata1)[!(colnames(MFdata1)%in%colnames(MFdata5))]
colnames(MFdata5)[!(colnames(MFdata5)%in%colnames(MFdata1))]



MFdata6 <- read.xls("PAHs.xlsx",stringsAsFactors=F,skip=2)
MFdata6 <- MFdata6[,c(3,2,1,5:23)]
colnames(MFdata1)[!(colnames(MFdata1)%in%colnames(MFdata6))]
colnames(MFdata6)[!(colnames(MFdata6)%in%colnames(MFdata1))]



MFdata7 <- read.xls("Tobacco smoke components.xlsx",stringsAsFactors=F,skip=2)
MFdata7 <- MFdata7[,c(3,2,1,5:22)]
colnames(MFdata7)[9] <- "Study.size"
MFdata7$Lipid.adjusted. <-NA
colnames(MFdata1)[!(colnames(MFdata1)%in%colnames(MFdata7))]
colnames(MFdata7)[!(colnames(MFdata7)%in%colnames(MFdata1))]

MFdata8 <- read.xls("Vitamins.xlsx",stringsAsFactors=F)     
MFdata8 <- MFdata8[,c(3,2,1,5:21)]
MFdata8$Lipid.adjusted. <-NA
MFdata8$Study.size <-NA

colnames(MFdata1)[!(colnames(MFdata1)%in%colnames(MFdata8))]
colnames(MFdata8)[!(colnames(MFdata8)%in%colnames(MFdata1))]
    
MFdata <- rbind(MFdata1,
  MFdata2[,colnames(MFdata1)],
  MFdata3[,colnames(MFdata1)],
  MFdata4[,colnames(MFdata1)],
  MFdata5[,colnames(MFdata1)],
  MFdata6[,colnames(MFdata1)],
  MFdata7[,colnames(MFdata1)],
  MFdata8[,colnames(MFdata1)]
  )
 
write.table(MFdata,row.names=F,file="Aylward-MatFet.txt",sep="\t") 
