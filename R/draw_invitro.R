#' Draw in vitro TK parameters including uncertainty and variability.
#' 
#' Given a CAS in the HTTK data set, a virtual population from HTTK-Pop, some
#' user specifications on the assumed distributions of Funbound.plasma and
#' Clint, draw "individual" values of Funbound.plasma and Clint from those
#' distributions.
#' 
#' @param this.chem The CAS number of one of the HTTK chemicals (see
#' \code{\link[httk]{get_cheminfo}}).
#' @param parameters A list of chemical-specific model parameters containing at
#' least Funbound.plasma, Clint, and Fhep.assay.correction.
#' @param nsamp The number of samples to draw.
#' @param fup.meas.cv Coefficient of variation of distribution of measured
#' \code{Funbound.plasma} values. 
#' @param clint.meas.cv Coefficient of variation of distribution of measured 
#' \code{Clint} values.
#' @param fup.pop.cv Coefficient of variation of distribution of population
#' \code{Funbound.plasma} values.
#' @param clint.pop.cv Coefficient of variation of distribution of population
#' \code{Clint} values.
#' @param poormetab Logical. Whether to include poor metabolizers in the Clint
#' distribution or not.
#' @param fup.lod The average limit of detection for \code{Funbound.plasma}, below
#' which distribution will be censored if fup.censored.dist is TRUE. Default 0.01.
#' @param fup.censored.dist Logical. Whether to draw \code{Funbound.plasma} from a
#' censored distribution or not.
#' @param adjusted.Funbound.plasma Uses adjusted Funbound.plasma when set to
#' TRUE.
#' @param clint.pvalue.threshold Hepatic clearance for chemicals where the in
#' vitro clearance assay result has a p-values greater than the threshold are
#' set to zero.
#' @param minimum.Funbound.plasma Monte Carlo draws less than this value are set 
#' equal to this value (default is 0.0001 -- half the lowest measured Fup in our
#' dataset).
#' @return A data.table with three columns: \code{Funbound.plasma} and
#' \code{Clint}, containing the sampled values, and
#' \code{Fhep.assay.correction}, containing the value for fraction unbound in
#' hepatocyte assay.
#' @author Caroline Ring and John Wambaugh
#' @import stats
#' @export draw_invitro

draw_invitro <- function(this.chem=NULL,
                           parameters=NULL,
                           nsamp,
                           fup.meas.cv=0.4,
                           clint.meas.cv=0.3,                           
                           fup.pop.cv=0.3,
                           clint.pop.cv=0.3,
                           poormetab=TRUE,
                           fup.lod=0.01,
                           fup.censored.dist=FALSE,
                           adjusted.Funbound.plasma=T,
                           clint.pvalue.threshold=0.05,
                           minimum.Funbound.plasma=0.0001)
{
  #R CMD CHECK throws notes about "no visible binding for global variable", for
  #each time a data.table column name is used without quotes. To appease R CMD
  #CHECK, a variable has to be created for each of these column names and set to
  #NULL. Note that within the data.table, these variables will not be NULL! Yes,
  #this is pointless and annoying.
  Clint.mu<-Clint.sd<-unadjusted.Funbound.plasma<-Flipid<-physiology.data<-NULL
  Parameter<-Funbound.plasma.adjustment<-fup.mean<-X<-Clint.dist<-Dow74<-NULL
  Funbound.plasma.dist<-fup.sd<-NULL
  #End R CMD CHECK appeasement.

  #To get the measured values of Funbound.plasma and Clint, get the HTTK default
  #parameter set for the steady-state model, which contains the measured values
  #of Funbound.plasma and Clint
  if (!is.null(this.chem))
  {
    parameters<-httk::parameterize_steadystate(chem.cas=this.chem,
                    species='Human',
                    adjusted.Funbound.plasma=adjusted.Funbound.plasma,
                    clint.pvalue.threshold=clint.pvalue.threshold,
                    suppress.messages=T)
 } else {
    if (!"Dow74"%in%names(parameters))
    {
      pKa_Donor <- parameters[["pKa_Donor"]]
      pKa_Accept <- parameters[["pKa_Accept"]]
      Pow <- parameters[["Pow"]] # Octanol:water partition coeffiecient
      ion <- calc_ionization(pH=7.4,pKa_Donor=pKa_Donor,pKa_Accept=pKa_Accept)
      dow <- Pow * (ion$fraction_neutral + 0.001 * ion$fraction_charged + ion$fraction_zwitter)
      parameters[["Dow74"]] <- dow
    }
    if (any(!(c("Funbound.plasma","Clint","Dow74","Fhep.assay.correction")%in%names(parameters))))
    {
      stop("Funbound.plasma, Clint, Dow74, and Fhep.assay.correction needed in draw_fup_clint.")
    }
  }

  
  # Initalize the data.table:
  indiv_tmp <- data.table(Funbound.plasma=rep(parameters[["Funbound.plasma"]],nsamp),
    Clint=rep(parameters[["Clint"]],nsamp),
    Dow74=rep(parameters[["Dow74"]],nsamp))

  #
  #
  #
  # MEASUREMENT UNCERTAINTY
  #
  #
  #
  # Hepatocyte clearance assay uncertainty:
  #
  #
  #
  # If the default CV is set to NULL, we just use the point estimate with no
  # uncertainty:
  if (is.null(clint.meas.cv))
  {         
    Clint <- parameters$Clint
    Clint.l95 <- NULL
    Clint.u95 <- NULL
    Clint.pvalue <- NULL
  }
  # We need to determine what sort of information we have been provided about
  # measurment uncertainty. We first check for a comma separated list with a
  # median, lower, and upper 95th credible interval limits:
  else if (!is.na(parameters$Clint.dist))
  {
    if (nchar(parameters$Clint.dist) -
      nchar(gsub(",","",parameters$Clint.dist))!=3) 
    {
      stop("Clint distribution should be four values (median,low95th,high95th,pValue) separated by commas.")
    }
    temp <- strsplit(parameters$Clint.dist,",")
    Clint <- as.numeric(temp[[1]][1])
    Clint.l95 <- as.numeric(temp[[1]][2])
    Clint.u95 <- as.numeric(temp[[1]][3])
    Clint.pvalue <- as.numeric(temp[[1]][4])
  # If we don't have that, we use the default coefficient of variation to
  # generate confidence limits:
  } else {
    Clint <- parameters$Clint
    if (Clint > 0)
    { 
      # zero doesn't behave well in a log-normal distribution (negatives too):
      Clint.l95 <- max(Clint*(1 - clint.meas.cv*1.96),1)  
      Clint.u95 <- Clint*(1 + clint.meas.cv*1.96)
      Clint.pvalue <- 0
    }
  }

  #Assign the HTTK default value for fraction unbound in hepatocyte assay to all
  #individuals (this is chemical-specific and does not vary between individuals).
  Fhep.assay.correction <- parameters$Fhep.assay.correction

  # Now do the uncertainty Monte Carlo analysis -- draw a series of plausible 
  # "true" values for Clint that are consistent with the measurment .
  # If a credible interval was specified for Clint, draw from that interval:
  if (Clint == 0)
  {
    indiv_tmp[,Clint:=0]
  } 
  else if (!is.null(Clint.u95))
  {
    if (Clint.u95>0& Clint>0)
    {
      # Optimize to find parameters for a log-normal distribution that have
      # the least squares difference using the three quantiles (median, l95, u95)
      clint.fit <- suppressWarnings(optim(c(log(Clint),clint.meas.cv), function(x) (0.95-
                                              plnorm(Clint.u95,x[1],x[2])+
                                              plnorm(Clint.l95,x[1],x[2]))^2+
                                              (Clint-qlnorm(0.5,x[1],x[2]))^2))
      indiv_tmp[,Clint:=rlnorm(n=nsamp,clint.fit$par[1],clint.fit$par[2])]
    } else if (Clint.u95>0)
    {
      # Assume that the minimum non-zero Clint is 1
      # Assume that since the median is zero but the u95 is not, that there is 
      # an exponential distribution:
      # 97.5% of clearance values will be below Clint.u95:
      indiv_tmp[,Clint:=exp(runif(n=nsamp,log(1),(log(Clint.u95)-log(1))/0.975))]
    } else indiv_tmp[,Clint:=Clint]
  # the Bayesian "p-value" here reflects how often there is no clearance:
    indiv_tmp[as.logical(rbinom(n=nsamp,1,Clint.pvalue)),Clint:=0] 
  } else indiv_tmp[,Clint:=Clint]
  # Store NA so data.table doesn't convert everything to text:
  indiv_tmp[,Clint.dist:=NA]

  #
  #
  #
  # fup uncertainty Monte Carlo:
  #
  #
  #
  # If the default CV is set to NULL, we just use the point estimate with no
  # uncertainty:
  if (is.null(fup.meas.cv))
  {
    Funbound.plasma <- parameters$Funbound.plasma
    Funbound.plasma.l95 <- NULL
    Funbound.plasma.u95 <- NULL
  # We need to determine what sort of information we have been provided about
  # measurment uncertainty. We first check for a comma separated list with a
  # median, lower, and upper 95th credible interval limits:
  } else if(!is.na(parameters$Funbound.plasma.dist))
  {
    if (nchar(parameters$Funbound.plasma.dist) - 
      nchar(gsub(",","",parameters$Funbound.plasma.dist))!=2)
    {
      stop("Funbound.plasma distribution should be three values (median,low95th,high95th) separated by commas.")
    }
    temp <- strsplit(parameters$Funbound.plasma.dist,",")
    Funbound.plasma <- as.numeric(temp[[1]][1])
    Funbound.plasma.l95 <- as.numeric(temp[[1]][2])
    Funbound.plasma.u95 <- as.numeric(temp[[1]][3])
  # If we don't have that, we use the default coefficient of variation to
  # generate confidence limits:
  } else {
    Funbound.plasma <- parameters$Funbound.plasma
    Funbound.plasma.l95 <- max(Funbound.plasma*(1-fup.meas.cv*1.96),0)
    Funbound.plasma.u95 <- min(Funbound.plasma*(1+fup.meas.cv*1.96),1)
  }
  
  # We need the fraction of lipid in plasma to use Robert's(Pearce, 2017)
  # plasma protein binding assay correction:
  Flipid <-subset(httk::physiology.data,
             Parameter=='Plasma Effective Neutral Lipid Volume Fraction')[,
             which(colnames(httk::physiology.data) == 'Human')]

  # Check to see if fup was below lod:
  if (Funbound.plasma==0)
  {
  # If so, assign values between zero and the lod:
    indiv_tmp[, fup.mean := runif(n=1,0,fup.lod)]
  # Otherwise, check to see if fup credible interval was provided:
  } else if (!is.null(Funbound.plasma.u95)) 
  {
    if (Funbound.plasma>minimum.Funbound.plasma)
    {
      # Use optim to estimate parameters for a beta distribution (alpha and beta)
      # such that the median and 95% credible interval approximate the given values:
      if (Funbound.plasma < 0.99)
      {
        ppb.fit <- suppressWarnings(optim(c(2,
          (1-Funbound.plasma)/Funbound.plasma*2), 
          function(x) (0.95-
          pbeta(Funbound.plasma.u95,x[1],x[2])+
          pbeta(Funbound.plasma.l95,x[1],x[2]))^2+
          (Funbound.plasma-qbeta(0.5,x[1],x[2]))^2,
          method="BFGS"))
      } else { 
        ppb.fit <- suppressWarnings(optim(c(2,1), function(x) (0.95-
          pbeta(Funbound.plasma.u95,x[1],x[2])+
          pbeta(Funbound.plasma.l95,x[1],x[2]))^2+
          (Funbound.plasma-qbeta(0.5,x[1],x[2]))^2,
          method="BFGS"))
      }
      # We are drawing new values for the unadjusted Fup:
      indiv_tmp[, unadjusted.Funbound.plasma:=rbeta(n=nsamp,
        ppb.fit$par[1],
        ppb.fit$par[2])]
    } else if (Funbound.plasma.u95 > minimum.Funbound.plasma)
    {
      # Assume that since the median is zero but the u95 is not, that there is 
      # an uniform distribution:
      # 97.5% of clearance values will be below Funbound.plasma.u95:
      indiv_tmp[,unadjusted.Funbound.plasma:=runif(n=nsamp,
        minimum.Funbound.plasma,
        min(1,minimum.Funbound.plasma+
        2*(Funbound.plasma.u95-minimum.Funbound.plasma)))]
      indiv_tmp[as.logical(rbinom(n=nsamp,1,.95)),
        unadjusted.Funbound.plasma:=minimum.Funbound.plasma]      
    } else {
      indiv_tmp[,unadjusted.Funbound.plasma:=minimum.Funbound.plasma]
    }
# Adjust for in vitro binding:    
    indiv_tmp[, Funbound.plasma.adjustment:=1 / (Dow74 * Flipid + 
      1 / unadjusted.Funbound.plasma)/unadjusted.Funbound.plasma]
    indiv_tmp[, fup.mean:=unadjusted.Funbound.plasma*Funbound.plasma.adjustment]
  # Otherwise use point estimate:
  } else {
    indiv_tmp[,fup.mean:=Funbound.plasma]
  }
  #if measured Funbound.plasma > 1, then set it to 1
  indiv_tmp[fup.mean>1, fup.mean := 1]
  # Store NA so data.table doesn't convert everything to text:
  indiv_tmp[,Funbound.plasma.dist:=NA]

  #
  #
  #
  # POPULATION VARIABILITY:
  #
  #
  #
  # Clint variability Monte Carlo:
  #
  #
  #
  #do not sample Clint if measured value is zero,
  #or if user said not to vary Clint.
  if (!is.null(clint.pop.cv) & Clint>0)
  {
    #Draw Clint from a normal distribution if poor metabolizers excluded, or
    #Gaussian mixture distribution if poor metabolizers included.
    #Set the mean of the regular metabolizer distribution:
    indiv_tmp[,Clint.mu:=Clint]
    if (poormetab) #if poor metabolizers are included:
    {
      #Assume that 5% of the population has 10% the metabolism:
      indiv_tmp[rbinom(n=nsamp,size=1,prob=0.05)==1,Clint.mu:=Clint.mu/10]
    }
    #Draw Clint from a normal distribution with mean = measured Clint, and
    #coefficient of variation given by clint.pop.cv.
    indiv_tmp[,Clint:=truncnorm::rtruncnorm(n=1,
                                                a=0,
                                                b=Inf,
                                                mean=Clint.mu,
                                                sd=clint.pop.cv*Clint.mu)]
  }
    
  #
  #
  #
  # fup variability Monte Carlo:
  #
  #
  #
  # next, draw Funbound.plasma from either a normal or censored distribution, as
  # long as fup.pop.cv isn't NULL (otherwise, no pop variability for this)
  if (!is.null(fup.pop.cv))
  {
    indiv_tmp[,fup.sd:=fup.pop.cv*fup.mean]
    indiv_tmp[,fup.lod:=fup.lod]
    # add check for fup.pop.cv=NULL, smae for clint
    if (fup.censored.dist)
    { #if user specified to use a censored distribution,
      #then draw from a normal distribution, left-censored at the specified LOD.
      indiv_tmp[, Funbound.plasma:=r_left_censored_norm(n=1,
                                                          mean=fup.mean,
                                                          sd=fup.sd,
                                                          lod=fup.lod)]
    } else { #if user specified to use a non-censored distribution
      #Draw Funbound.plasma from a normal distribution, truncated at 0 and 1.
      indiv_tmp[, Funbound.plasma:=truncnorm::rtruncnorm(n=1,
                                                             a=0,
                                                             b=1,
                                                             mean=fup.mean,
                                                             sd=fup.sd)] 
    }
  } else {
    indiv_tmp[,Funbound.plasma:=fup.mean]
  }

  #Enforce a minimum Funbound.plasma unless set to zero:
  indiv_tmp[Funbound.plasma<minimum.Funbound.plasma,
    Funbound.plasma:=minimum.Funbound.plasma]
  
  return(indiv_tmp)
}
