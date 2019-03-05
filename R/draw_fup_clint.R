#'Draw Funbound.plasma and Clint from censored or non-censored distributions.
#'
#'Given a CAS in the HTTK data set, a virtual population from HTTK-Pop, some
#'user specifications on the assumed distributions of Funbound.plasma and Clint,
#'draw "individual" values of Funbound.plasma and Clint from those
#'distributions.
#'
#'@export
#'
#'@param this.chem The CAS number of one of the HTTK chemicals (see 
#'  \code{\link[httk]{get_cheminfo}}).
#' @param parameters A list of chemical-specific model parameters containing at least
#' Funbound.plasma, Clint, and Fhep.assay.correction. 
#'@param nsamp The number of samples to draw.
#'@param sigma.factor The coefficient of variance to assume. Default 0.3.
#'@param poormetab Logical. Whether to include poor metabolizers in the Clint
#'  distribution or not.
#'@param fup.censor Logical. Whether to draw \code{Funbound.plasma} from a
#'  censored distribution or not.
#'@param Clint.vary Logical, default TRUE. Whether to treat \code{Clint} as
#'  fixed at its measured value (FALSE), or as a random variable (TRUE).
#'@param lod The average limit of detection for \code{Funbound.plasma}, below
#'  which distribution will be censored if fup.censor is TRUE. Default 0.01.
#' @param clint.pvalue.threshold Hepatic clearance for chemicals where the in vitro 
#' clearance assay result has a p-values greater than the threshold are set to zero.
#' @param adjusted.Funbound.plasma Uses adjusted Funbound.plasma when set to TRUE.
#'
#'@return A data.table with three columns:
#'  \code{Funbound.plasma} and \code{Clint}, containing the sampled values, and
#'  \code{Fhep.assay.correction}, containing the value for fraction unbound in
#'  hepatocyte assay.
#'
#' @author Caroline Ring and John Wambaugh

draw_fup_clint <- function(this.chem=NULL,
                           parameters=NULL,
                           nsamp,
                           fup.meas.cv=0.4,
                           clint.meas.cv=0.3,                           
                           fup.pop.cv=0.3,
                           clint.pop.cv=0.3,
                           poormetab,
                           fup.lod=0.01,
                           fup.censored.dist=FALSE,
                           adjusted.Funbound.plasma=T,
                           clint.pvalue.threshold=0.05)
{
  #R CMD CHECK throws notes about "no visible binding for global variable", for
  #each time a data.table column name is used without quotes. To appease R CMD
  #CHECK, a variable has to be created for each of these column names and set to
  #NULL. Note that within the data.table, these variables will not be NULL! Yes,
  #this is pointless and annoying.
  Clint.mu<-Clint.sd<-unadjusted.Funbound.plasma<-Flipid<-physiology.data<-NULL
  Parameter<-Funbound.plasma.adjustment<-fup.mean<-X<-NULL
  #End R CMD CHECK appeasement.

  #To get the measured values of Funbound.plasma and Clint, get the HTTK default
  #parameter set for the steady-state model, which contains the measured values
  #of Funbound.plasma and Clint
  if (!is.null(this.chem))
  {
    parameters<-httk::parameterize_steadystate(chem.cas=this.chem,
                                      species='Human',
                                      adjusted.Funbound.plasma=adjusted.Funbound.plasma,
                                      clint.pvalue.threshold=clint.pvalue.threshold)
  } else if (any(!(c("Funbound.plasma","Clint","Dow74","Fhep.assay.correction")%in%names(parameters))))
  {
    stop("Funbound.plasma, Clint, Dow74, and Fhep.assay.correction needed in draw_fup_clint.")
  }
  
  # Initalize the data.table:
  indiv_tmp <- data.table(Funbound.plasma=rep(parameters[["Funbound.plasma"]],nsamp),
    Clint=rep(parameters[["Clint"]],nsamp),
    Dow74=rep(parameters[["Dow74"]],nsamp))

  #
  #
  #
  # MEASUREMENT UNCERTAINTY:
  #
  #
  #
  # Hepatocyte clearance assay uncertainty...
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
  else if (regexpr(",",parameters$Clint)!=-1)
  {
    temp <- strsplit(parameters$Clint,",")
    Clint <- as.numeric(temp[[1]][1])
    Clint.l95 <- as.numeric(temp[[1]][2])
    Clint.u95 <- as.numeric(temp[[1]][3])
    Clint.pvalue <- as.numeric(temp[[1]][4])
  # If we don't have that, we use the default coefficient of variation to
  # generate confidence limits:
  } else {
    Clint <- parameters$Clint
    # zero doesn't behave well in a log-normal distribution (negatives too):
    Clint.l95 <- max(Clint*(1 - clint.meas.cv*1.96),1)  
    Clint.u95 <- Clint*(1 + clint.meas.cv*1.96)
    Clint.pvalue <- 0
  }

  #Assign the HTTK default value for fraction unbound in hepatocyte assay to all
  #individuals (this is chemical-specific and does not vary between individuals).
  Fhep.assay.correction <- parameters$Fhep.assay.correction

  # Now do the uncertainty Monte Carlo analysis -- draw a series of plausible 
  # "true" values for Clint that are consistent with the measurment .
  # If a credible interval was specified for Clint, draw from that interval:
  if (!is.null(Clint.u95))
  {
    if (Clint.u95>0& Clint>0)
    {
      # Optimize to find parameters for a log-normal distribution that have
      # the least squares difference using the three quantiles (median, l95, u95)
      clint.fit <- optim(c(log(Clint),clint.meas.cv), function(x) (0.95-
                                              plnorm(Clint.u95,x[1],x[2])+
                                              plnorm(Clint.l95,x[1],x[2]))^2+
                                              (Clint-qlnorm(0.5,x[1],x[2]))^2)
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

  # fup uncertainty Monte Carlo:
  # If the default CV is set to NULL, we just use the point estimate with no
  # uncertainty:
  if (is.null(fup.meas.cv))
  {
    Funbound.plasma <- parameters$Funbound.plasma
    Funbound.plasma.l95 <- NULL
    Funbound.plasma.u95 <- NULL
  }
  # We need to determine what sort of information we have been provided about
  # measurment uncertainty. We first check for a comma separated list with a
  # median, lower, and upper 95th credible interval limits:
  else if (regexpr(",",parameters$Funbound.plasma)!=-1)
  {
    temp <- strsplit(parameters$Funbound.plasma,",")
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
  }
  # Otherwise, check to see if fup credible interval was provided:
  if (!is.null(Funbound.plasma.u95))
  {
    # Use optim to estimate parameters for a beta distribution (alpha and beta)
    # such that the median and 95% credible interval approximate the given values:
    ppb.fit <- optim(c(2,(1-Funbound.plasma)/Funbound.plasma*2), function(x) (0.95-
      pbeta(Funbound.plasma.u95,x[1],x[2])+
      pbeta(Funbound.plasma.l95,x[1],x[2]))^2+
      (Funbound.plasma-qbeta(0.5,x[1],x[2]))^2,
      method="BFGS")
  # We are drawing new values for the unadjusted Fup:
    indiv_tmp[, unadjusted.Funbound.plasma:=rbeta(n=nsamp,ppb.fit$par[1],ppb.fit$par[2])]
    indiv_tmp[, Funbound.plasma.adjustment:=1 / (Dow74 * Flipid + 1 / unadjusted.Funbound.plasma)/unadjusted.Funbound.plasma]
    indiv_tmp[, fup.mean:=unadjusted.Funbound.plasma*Funbound.plasma.adjustment]
  # Otherwise use point estimate:
  } else {
    #if measured Funbound.plasma > 1,
    #then set the distribution mean to 1.
    indiv_tmp[, fup.mean := min(1,
                    Funbound.plasma)] 
  }

  #
  #
  #
  # POPULATION VARIABILITY:
  #
  #
  # Clint variability:
  #do not sample Clint if measured value is zero,
  #or if user said not to vary Clint.
  if (!is.null(clint.pop.cv) & Clint>0)
  {
    #Draw Clint from a normal distribution if poor metabolizers excluded, or
    #Gaussian mixture distribution if poor metabolizers included.
    if (poormetab){ #if poor metabolizers are included:
      #Assume that 5% of the population has 10% the metabolism; i.e., assume a
      #Gaussian mixture distribution

      #Gaussian mixture distribution: first, draw whether each individual will
      #come from the "regular metabolizers" distribution (with 95% probability),
      #or the "poor metabolizers" distribution (with 5% probability)
      components <- sample(1:2,
                           prob=c(0.05,0.95),
                           size=nsamp,
                           replace=TRUE)
      #Set the means of the two distributions. Poor metabolizers distribution
      #has mean 10% of regular metabolizers distribution.
      # Check if clint is a distribution (median,low95,high95,pvalue):
      if (nchar(Clint) - nchar(gsub(",","",Clint))==3) 
      {
        indiv_tmp[,Clint.mu:=as.numeric(strsplit(Clint,",")[[1]][1])]
      } else indiv_tmp[,Clint.mu:=Clint]
      indiv_tmp[rbinom(n=nsamp,size=1,prob=0.05)==1,.(Clint.mu=Clint.mu/10)]

      #Set the standard deviations of the two distributions.
      #Both have a coefficient of variation given by *.pop.cv:
      indiv_tmp[,Clint.sd:=clint.pop.cv*Clint.mu]

      #Now draw from the "regular" or "poor metabolizers" distributions as
      #assigned earlier for each individual.
      indiv_tmp[,Clint:=truncnorm::rtruncnorm(n=1,
                                                  a=0,
                                                  b=Inf,
                                                  mean=Clint.mu,
                                                  sd=Clint.sd)]
    } else{ #if poor metabolizers were excluded
      #Draw Clint from a normal distribution with mean = measured Clint, and
      #coefficient of variation given by clint.pop.cv.
      indiv_tmp[,Clint:=truncnorm::rtruncnorm(n=1,
                                                  a=0,
                                                  b=Inf,
                                                  mean=Clint,
                                                  sd=clint.pop.cv*Clint)]
    }
  } else { #if either measured Clint is zero or the user said don't vary Clint
    #just set Clint to its measured value, for all individuals.
    if (is.null(Clint.u95)) indiv_tmp[, Clint:=parameters$Clint]
  }
  
  # fup variability:
  # next, draw Funbound.plasma from either a normal or censored distribution, as
  # long as fup.pop.cv isn't NULL (otherwise, no pop variability for this
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
  }

  return(indiv_tmp)
}
