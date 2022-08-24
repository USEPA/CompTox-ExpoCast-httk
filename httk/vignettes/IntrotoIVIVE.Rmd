toxcast <- read.csv("ac50_Matrix_190708.csv",stringsAsFactors=F)

toxcast$CAS <- sapply(toxcast$X,function(x) paste(
  substr(x,2,nchar(x)-3),
  substr(x,nchar(x)-2,nchar(x)-1),
  substr(x,nchar(x),nchar(x)),
  sep="-"))
toxcast[regexpr("NOCAS",toxcast$CAS)!=-1,"CAS"] <- 
  gsub("-","",toxcast[regexpr("NOCAS",toxcast$CAS)!=-1,"CAS"])
  
library(gtools)
#install.packages("gtools") if you don't have it

mychems <- read.xls("mychems.xls",stringsAsFactors=F)
#take a look at the first few rows:
head(mychems)

#Get the ToxCast for only the chemicals of interest:
my.tox <- subset(toxcast,CAS%in%mychems$CASRN)

# How many chemicals covered by ToxCast:
dim(my.tox)
# Out of how many chemicals of intrest:
dim(mychems)  
  
# Mark the columns with ToxCast AC50s:
toxcast.start <- 2
toxcast.end <- 1474
# Find the tenth percentile AC50 for each chemical:
my.tox$tenth <- apply(my.tox[,toxcast.start:toxcast.end],
                  1,
                  function(x) quantile(x,0.1,na.rm=T))
# Get rid of chemicals where there is no hit:
my.tox <- subset(my.tox,tenth<1e6)
# Reduce to just the tneth percentile concentration (uM):
my.tox <- my.tox[,c("CAS","tenth")]
# Add the other chemical information:
my.tox <- merge(my.tox,mychems,by.x="CAS",by.y="CASRN")

# Add the HTTK plasma steady-state concentration where available:
library(httk)
for (this.cas in my.tox$CAS)
{
  if (this.cas %in% get_cheminfo())
  {
    set.seed(12345)
# This gives us the predicted plasma concentration for a 1 mg/kg/day dose at 
# steady-state:
    my.tox[my.tox$CAS==this.cas,"Css"] <- 
      calc_mc_css(chem.cas=this.cas,output.units="uM")
    my.tox[my.tox$CAS==this.cas,"Css.Type"] <- "in vitro"
  }
}
my.tox[,c("CAS","tenth","CSS","Css.Type")]


load_sipes2017()
for (this.cas in my.tox$CAS)
{
  if (this.cas %in% get_cheminfo() &
    is.na(my.tox[my.tox$CAS==this.cas,"Css"]))
  {
    set.seed(12345)
# This gives us the predicted plasma concentration for a 1 mg/kg/day dose at 
# steady-state:
    my.tox[my.tox$CAS==this.cas,"Css"] <- 
      calc_mc_css(chem.cas=this.cas,output.units="uM")
    my.tox[my.tox$CAS==this.cas,"Css.Type"] <- "in silico"
  }
}
my.tox[,c("CAS","tenth","CSS","Css.Type")]

# Equivalent steady-stat dose to produce plasma cocnentration equal to tenth
# percentile Toxcast AC50
my.tox$EquivDose <- my.tox$tenth / my.tox$Css
my.tox[,c("PREFERRED_NAME","tenth","EquivDose")]          
                  


