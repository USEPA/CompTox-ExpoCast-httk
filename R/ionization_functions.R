# alpha: Ratio of Distribution coefficient D of totally charged species and that of the neurtral form
calc_dow <- function(Pow,pH=NA,pKa_Donor=NA,pKa_Accept=NA,fraction_charged=NULL,alpha=0.001) 
{
  # Octonol:water distribution coefficient,
  if (is.null(fraction_charged))
  {
    if (is.na(pH)) stop("pH or fraction_charged must be specified in calc_dow.")
    ionization <- calc_ionization(pH=pH,pKa_Donor=pKa_Donor,pKa_Accept=pKa_Accept)
    fraction_charged  <- ionization[["fraction_charged"]]
  }
  
  Dow <- Pow*(1 + (alpha - 1)*fraction_charged)
  return(Dow)
}

#calc_ionization <- function(pH,pKa=NA,pKb=NA) 
#{
#  # Need to calculate the amount of un-ionized parent:
#  denom <- 1
#  acid <- 0
#  base <- 0
#  if (!is.na(pKa))
#  {
#    if (regexpr(",",pKa)!=-1) pKa <- sort(as.numeric(strsplit(pKa,",")[[1]]))
#    for (i in 1:length(pKa))
#      denom <- denom + 10^(i*pH-sum(pKa[1:i]))
#      acid <- acid + 10^(i*pH-sum(pKa[1:i]))
#  }
#  if (!is.na(pKb)) 
#  {
#    if (regexpr(",",pKb)!=-1) pKb <- sort(as.numeric(strsplit(pKb,",")[[1]]),decreasing=T)
#      for (i in 1:length(pKb))
#        denom <- denom + 10^(sum(pKb[1:i])-i*pH)
#        base <- base + 10^(sum(pKb[1:i])-i*pH)
#  }
#  return(list(fraction_neutral = 1/denom,
#    fraction_charged = 1 - 1/denom,
#    fraction_negative = acid/denom,
#    fraction_positive = base/denom))
#}

calc_ionization <- function(chem.cas=NULL,chem.name=NULL,parameters=NULL,pH=NULL,pKa_Donor=NA,pKa_Accept=NA)
{

  if (is.null(pH)) stop("pH is required to calculate the ionization.")
  if (!is.null(chem.cas) | !is.null(chem.name)) 
  {
    out <- get_chem_id(chem.cas=chem.cas,chem.name=chem.name)
    chem.cas <- out$chem.cas
    pKa_Donor <- suppressWarnings(get_physchem_param("pKa_Donor",chem.CAS=chem.cas))
    pKa_Accept <- suppressWarnings(get_physchem_param("pKa_Accept",chem.CAS=chem.cas))
  } else if (all(c("pKa_Donor","pKa_Accept") %in% names(parameters)))
  {
    pKa_Donor <- parameters$pKa_Donor
    pKa_Accept <- parameters$pKa_Accept
  }
  
  # Number of ionizations to calculate:
  if (is.null(parameters))
  {
    calculations <- 1
  } else {  # Can't use pKa's because they could be vectors even for one set of parameters
    calculations <- length(parameters$Pow)
    if (calculations > 1)
    {
# If pKa's aren't actually varying let's not waste computing time:    
      if (length(unique(pKa_Donor))==1 & length(unique(pKa_Accept))==1 & length(unique(pH))==1)
      {
        calculations <- 1
        pKa_Donor <- pKa_Donor[1]
        pKa_Accept <- pKa_Accept[1]
        pH <- pH[1]
        if (!is.na(pKa_Donor))
        {
          if (any(regexpr(",",pKa_Donor)!=-1))
          { 
            pKa_Donor <- strsplit(pKa_Donor,",")[[1]]
          }
          pKa_Donor <- as.numeric(pKa_Donor)
        }
        if (!is.na(pKa_Accept))
        {
          if (any(regexpr(",",pKa_Accept)!=-1))
          { 
            pKa_Accept <- strsplit(pKa_Accept,",")[[1]]
          }
          pKa_Accept <- as.numeric(pKa_Accept)
        }
      }
    }
  }
  
  fraction_neutral <- NULL
  fraction_charged <- NULL
  fraction_negative <- NULL
  fraction_positive <- NULL
  fraction_zwitter <- NULL
  for (index in 1:calculations)
  {
    if (calculations==1)
    {
      this.pKa_Donor <- pKa_Donor
      this.pKa_Accept <- pKa_Accept
    } else {
      this.pKa_Donor <- pKa_Donor[[index]]
      this.pKa_Accept <- pKa_Accept[[index]]
      if (!is.na(this.pKa_Donor))
      {
        if (any(regexpr(",",this.pKa_Donor)!=-1))
        { 
          this.pKa_Donor <- strsplit(this.pKa_Donor,",")[[1]]
        }
        this.pKa_Donor <- as.numeric(this.pKa_Donor)
      }
      if (!is.na(this.pKa_Accept))
      {
        if (any(regexpr(",",this.pKa_Accept)!=-1))
        { 
          this.pKa_Accept <- strsplit(this.pKa_Accept,",")[[1]]
        }
        this.pKa_Accept <- as.numeric(this.pKa_Accept)
      }
    }  
  # Need to calculate the amount of un-ionized parent:


  
  # Multiple equilibirum points may still be separated by commas, split them into vectors here:
     
    if(all(is.na(this.pKa_Donor))) this.pKa_Donor <- NULL
    if(all(is.na(this.pKa_Accept)))  this.pKa_Accept <- NULL
  
  # Make a vector of all equilibirum points:
    eq.points <- c(this.pKa_Donor,this.pKa_Accept)
    if (all(!is.null(eq.points)))
    {
  # Annotate whether each equilibirum point is a H-donation or acceptance:
      eq.point.types <- c(rep("Donate",length(this.pKa_Donor)),rep("Accept",length(this.pKa_Accept)))
      eq.point.types <- eq.point.types[order(eq.points)]    #label each point
      eq.points <- eq.points[order(eq.points)]     #order points
    }
  
    neutral <- 0
    negative <- 0
    positive <- 0
    zwitter <- 0
    denom <- 1
      
    if(is.null(this.pKa_Donor) & is.null(this.pKa_Accept)){
      neutral <- 1
    }else{
      nz <- NULL;
    #Find where charge is neutral or zwitter
      if(all(eq.point.types == "Donate") | all(eq.point.types == "Accept")){
        neutral <- 1
        if(all(eq.point.types == "Donate")){
          nz <- 0
        }else  nz <- length(eq.points)
      }else{ 
        for(i in 1:(length(eq.points) - 1)){
          charge <- sum(eq.point.types[(i+1):length(eq.point.types)]=="Accept") - sum(eq.point.types[1:i]=="Donate")
          if(charge == 0){
            nz <- i
            if(sum(eq.point.types[(i+1):length(eq.point.types)]=="Accept") == 0){
              neutral <- 1
            }else zwitter <- 1
          }  
        }   
      }    
      if(nz == 0){
        for(i in 1:length(eq.points))  negative <- negative + 10^(i * pH - sum(eq.points[1:i]))
      }else if(nz == length(eq.points)){
        for(i in 1:length(eq.points)) positive <- positive + 10^(sum(eq.points[(length(eq.points) + 1 - i):length(eq.points)])- i * pH)
      }else{
        for(i in 1:nz) positive <- positive + 10^(sum(eq.points[(nz + 1 - i):nz])- i * pH)
        for(i in 1:(length(eq.points)-nz)) negative <- negative + 10^(i * pH - sum(eq.points[(nz+1):(nz+i)])) 
      }
      denom <- denom + positive + negative      
    }
    if (length(neutral)>1) browser()
    fraction_neutral[index] <- neutral/denom
    fraction_charged[index] <- (negative+positive)/denom
    fraction_negative[index] <- negative/denom
    fraction_positive[index] <- positive/denom
    fraction_zwitter[index] <- zwitter/denom
  }
  
  # If pKa's aren't actually varying let's not waste computing time:  
  if (!is.null(parameters))
  {
    if (length(parameters$Pow)>1 & calculations == 1)
    {
      fraction_neutral <- rep(fraction_neutral,length(parameters$Pow))
      fraction_charged <- rep(fraction_charged,length(parameters$Pow))
      fraction_negative <- rep(fraction_negative,length(parameters$Pow))
      fraction_positive <- rep(fraction_positive,length(parameters$Pow))
      fraction_zwitter <- rep(fraction_zwitter,length(parameters$Pow))
    }
  }  
  
  return(list(fraction_neutral = fraction_neutral,
    fraction_charged = fraction_charged,
    fraction_negative = fraction_negative,
    fraction_positive = fraction_positive,
    fraction_zwitter = fraction_zwitter))
}

is_acid <- function(pH=7,pKa_Donor=NA,pKa_Accept=NA,fraction_negative=NULL)
{
  if (is.null(fraction_negative))
  {
    ionization <- calc_ionization(pH=pH,pKa_Donor=pKa_Donor,pKa_Accept=pKa_Accept)
    fraction_negative  <- ionization[["fraction_negative"]]
  }
  if (fraction_negative > 0.5) return(TRUE)
  else return(FALSE)
}

is_base <- function(pH=7,pKa_Donor=NA,pKa_Accept=NA,fraction_positive=NULL)
{
  if (is.null(fraction_positive))
  {
    ionization <- calc_ionization(pH=pH,pKa_Donor=pKa_Donor,pKa_Accept=pKa_Accept)
    fraction_positive  <- ionization[["fraction_positive"]]
  }
  if (fraction_positive > 0.5) return(TRUE)
  else return(FALSE)
}

is_neutral <- function(pH=7,pKa_Donor=NA,pKa_Accept=NA,fraction_postive=NULL)
{
  if (is.null(fraction_neutral))
  {
    ionization <- calc_ionization(pH=pH,pKa_Donor=pKa_Donor,pKa_Accept=pKa_Accept)
    fraction_neutral  <- ionization[["fraction_neutral"]]
  }
  if (fraction_neutral > 0.5) return(TRUE)
  else return(FALSE)
}