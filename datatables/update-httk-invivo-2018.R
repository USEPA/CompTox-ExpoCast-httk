library(httk)

load("PKdata-2017-12-11.RData")
chem.invivo.PK.data <- as.data.frame(pkdataset_nheerlcleaned)

chem.invivo.PK.aggregate.data <- read.csv("Z:/Research Projects/invivoPKlibrary/invivoPK_work/ChemProps-2018-02-05.txt",stringsAsFactors=F)
chem.invivo.PK.summary.data <- read.csv("Z:/Research Projects/invivoPKlibrary/invivoPK_work/output/PKstats-2018-01-16.txt",stringsAsFactors=F)

pharma.chems <- read.csv("Z:/Research Projects/invivoPKlibrary/invivoPK_work/Obach2008.csv",stringsAsFactors=F)
pharma.chems <- pharma.chems[,1:2]
colnames(pharma.chems) <- c("Compound","CAS")
pharma.chems <- rbind(pharma.chems,c("Phenytion","57-41-0"))
chem.lists[["Pharma"]] <- pharma.chems

is.pharma <- function (chem.cas) 
return(in.list(chem.cas = chem.cas, which.list = "Pharma"))

save(chem.invivo.PK.data,chem.lists,chem.invivo.PK.aggregate.data,chem.invivo.PK.summary.data,file="NewInVivoTablesForHTTK.RData")

                                            