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





#' Draw Funbound.plasma and Clint from censored or non-censored distributions.
#' 
#' Given a CAS in the HTTK data set, a virtual population from HTTK-Pop, some
#' user specifications on the assumed distributions of Funbound.plasma and
#' Clint, draw "individual" values of Funbound.plasma and Clint from those
#' distributions.
#' 
#' 
#' @param this.chem The CAS number of one of the HTTK chemicals (see
#' \code{\link[httk]{get_cheminfo}}).
#' @param parameters A list of chemical-specific model parameters containing at
#' least Funbound.plasma, Clint, and Fhep.assay.correction.
#' @param nsamp The number of samples to draw.
#' @param sigma.factor The coefficient of variance to assume. Default 0.3.
#' @param poormetab Logical. Whether to include poor metabolizers in the Clint
#' distribution or not.
#' @param fup.censor Logical. Whether to draw \code{Funbound.plasma} from a
#' censored distribution or not.
#' @param Clint.vary Logical, default TRUE. Whether to treat \code{Clint} as
#' fixed at its measured value (FALSE), or as a random variable (TRUE).
#' @param lod The average limit of detection for \code{Funbound.plasma}, below
#' which distribution will be censored if fup.censor is TRUE. Default 0.01.
#' @param adjusted.Funbound.plasma Uses adjusted Funbound.plasma when set to
#' TRUE.
#' @param clint.pvalue.threshold Hepatic clearance for chemicals where the in
#' vitro clearance assay result has a p-values greater than the threshold are
#' set to zero.
#' @param this.chem The CAS number of one of the HTTK chemicals (see
#' \code{\link[httk]{get_cheminfo}}).
#' @param parameters A list of chemical-specific model parameters containing at
#' least Funbound.plasma, Clint, and Fhep.assay.correction.
#' @param nsamp The number of samples to draw.
#' @param sigma.factor The coefficient of variance to assume. Default 0.3.
#' @param poormetab Logical. Whether to include poor metabolizers in the Clint
#' distribution or not.
#' @param fup.censor Logical. Whether to draw \code{Funbound.plasma} from a
#' censored distribution or not.
#' @param Clint.vary Logical, default TRUE. Whether to treat \code{Clint} as
#' fixed at its measured value (FALSE), or as a random variable (TRUE).
#' @param lod The average limit of detection for \code{Funbound.plasma}, below
#' which distribution will be censored if fup.censor is TRUE. Default 0.01.
#' @param adjusted.Funbound.plasma Uses adjusted Funbound.plasma when set to
#' TRUE.
#' @param clint.pvalue.threshold Hepatic clearance for chemicals where the in
#' vitro clearance assay result has a p-values greater than the threshold are
#' set to zero.
#' @return A data.table with three columns: \code{Funbound.plasma} and
#' \code{Clint}, containing the sampled values, and
#' \code{Fhep.assay.correction}, containing the value for fraction unbound in
#' hepatocyte assay.
#' 
#' A data.table with three columns: \code{Funbound.plasma} and \code{Clint},
#' containing the sampled values, and \code{Fhep.assay.correction}, containing
#' the value for fraction unbound in hepatocyte assay.
#' @author Caroline Ring and John Wambaugh Draw Funbound.plasma and Clint from
#' censored or non-censored distributions.
#' 
#' Given a CAS in the HTTK data set, a virtual population from HTTK-Pop, some
#' user specifications on the assumed distributions of Funbound.plasma and
#' Clint, draw "individual" values of Funbound.plasma and Clint from those
#' distributions.
#' 
#' Caroline Ring and John Wambaugh
#' @export draw_fup_clint
draw_fup_clint <- function(this.chem=NULL,
                           parameters=NULL,
                           nsamp,
                           sigma.factor=0.3,
                           poormetab,
                           fup.censor,
                           Clint.vary=TRUE,
                           lod=0.01,
                           adjusted.Funbound.plasma=T,
                           clint.pvalue.threshold=0.05){
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
  } else if (any(!(c("Funbound.plasma","Clint","Fhep.assay.correction")%in%names(parameters))))
  {
    stop("Funbound.plasma, Clint, and Fhep.assay.correction needed in draw_fup_clint.")
  }
  if (regexpr(",",parameters$Funbound.plasma)!=-1)
  {
    temp <- strsplit(parameters$Funbound.plasma,",")
    Funbound.plasma <- as.numeric(temp[[1]][1])
    Funbound.plasma.l95 <- as.numeric(temp[[1]][2])
    Funbound.plasma.u95 <- as.numeric(temp[[1]][3])
    Dow74 <- as.numeric(temp[[1]][4])
  } else {
    Funbound.plasma <- parameters$Funbound.plasma
    Funbound.plasma.l95 <- NULL
    Funbound.plasma.u95 <- NULL
    Dow74 <- NULL
  }
  if (regexpr(",",parameters$Clint)!=-1)
  {
    temp <- strsplit(parameters$Clint,",")
    Clint <- as.numeric(temp[[1]][1])
    Clint.l95 <- as.numeric(temp[[1]][2])
    Clint.u95 <- as.numeric(temp[[1]][3])
    Clint.pvalue <- as.numeric(temp[[1]][4])
  } else {
    Clint <- parameters$Clint
    Clint.l95 <- NULL
    Clint.u95 <- NULL
    Clint.pvalue <- NULL
  }
  Fhep.assay.correction <- parameters$Fhep.assay.correction

  #Initialize the data table
  #indiv_tmp <- data.table(junk=rep(NA, nsamp))

  #Assign the HTTK default value for fraction unbound in hepatocyte assay to all
  #individuals.

  indiv_tmp <- data.table("Fhep.assay.correction"=rep(Fhep.assay.correction,nsamp),
                          "X"=1:nsamp)

  # If a credible interval was specified for Clint, draw from that interval:
  if (!is.null(Clint.u95))
  {
    if (Clint.u95>0& Clint>0)
    {
      clint.fit <- optim(c(1,1), function(x) (0.95-plnorm(Clint.u95,x[1],x[2])+plnorm(Clint.l95,x[1],x[2]))^2+(Clint-qlnorm(0.5,x[1],x[2]))^2)
      indiv_tmp[,Clint:=rlnorm(n=nsamp,clint.fit$par[1],clint.fit$par[2])]
    } else if (Clint.u95>0)
    {
      # Assume that the minimum non-zero Clint is 1
      # Assume that since the median is zero but the u95 is not, that there is an exponential distribution:
      # 97.5% of clearance values will be below Clint.u95:
      indiv_tmp[,Clint:=exp(runif(n=nsamp,log(1),(log(Clint.u95)-log(1))/0.975))]
    } else indiv_tmp[,Clint:=Clint]
# the Bayesian "p-value" here reflects how often there is no clearance:
    indiv_tmp[as.logical(rbinom(n=nsamp,1,Clint.pvalue)),Clint:=0] 
  } else indiv_tmp[,Clint:=Clint]

  #do not sample Clint if measured value is zero,
  #or if user said not to vary Clint.
  if (Clint.vary & Clint>0)
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
      indiv_tmp[,Clint.mu:=Clint]
      indiv_tmp[rbinom(n=nsamp,size=1,prob=0.05)==1,.(Clint.mu=Clint.mu/10)]

      #Set the standard deviations of the two distributions.
      #Both have a coefficient of variation given by sigma.factor.
      indiv_tmp[,Clint.sd:=sigma.factor*Clint.mu]

      #Now draw from the "regular" or "poor metabolizers" distributions as
      #assigned earlier for each individual.
      indiv_tmp[,Clint:=truncnorm::rtruncnorm(n=1,
                                                  a=0,
                                                  b=Inf,
                                                  mean=Clint.mu,
                                                  sd=Clint.sd)]
    } else{ #if poor metabolizers were excluded
      #Draw Clint from a normal distribution with mean = measured Clint, and
      #coefficient of variation given by sigma.factor.
      indiv_tmp[,Clint:=truncnorm::rtruncnorm(n=1,
                                                  a=0,
                                                  b=Inf,
                                                  mean=Clint,
                                                  sd=sigma.factor*Clint)]
    }
  } else { #if either measured Clint is zero or the user said don't vary Clint
    #just set Clint to its measured value, for all individuals.
    if (is.null(Clint.u95)) indiv_tmp[, Clint:=parameters$Clint]
  }


  # Check to see if fup credible interval was provided:
  if (!is.null(Funbound.plasma.u95))
  {
    # Use optim to estimate alpha and beta such that the median and 95% credible interval approximate the estimate from MCMC:
    ppb.fit <- optim(c(2,(1-Funbound.plasma)/Funbound.plasma*2), function(x) (0.95-pbeta(Funbound.plasma.u95,x[1],x[2])+pbeta(Funbound.plasma.l95,x[1],x[2]))^2+(Funbound.plasma-qbeta(0.5,x[1],x[2]))^2,method="BFGS")
  # We are drawing new values for the unadjusted Fup:
    indiv_tmp[, unadjusted.Funbound.plasma:=rbeta(n=nsamp,ppb.fit$par[1],ppb.fit$par[2])]
    indiv_tmp[, Flipid:=subset(physiology.data,Parameter=='Plasma Effective Neutral Lipid Volume Fraction')[,which(colnames(physiology.data) == 'Human')]]
    indiv_tmp[, Funbound.plasma.adjustment:=1 / (Dow74 * Flipid + 1 / unadjusted.Funbound.plasma)/unadjusted.Funbound.plasma]
    indiv_tmp[, fup.mean:=unadjusted.Funbound.plasma*Funbound.plasma.adjustment]

  } else {
    indiv_tmp[, fup.mean := min(1,
                    Funbound.plasma),.(X)] #if measured Funbound.plasma > 1,
    #then set the distribution mean to 1.
  }

  #next, draw Funbound.plasma from either a normal or censored distribution.
  if (fup.censor){ #if user specified to use a censored distribution,
    #then draw from a normal distribution, left-censored at the specified LOD.
    indiv_tmp[, Funbound.plasma:=r_left_censored_norm(n=1,
                                                        mean=fup.mean,
                                                        sd=sigma.factor*fup.mean,
                                                        lod=lod),.(X)]
  }else{ #if user specified to use a non-censored distribution
    #Draw Funbound.plasma from a normal distribution, truncated at 0 and 1.
    indiv_tmp[, Funbound.plasma:=truncnorm::rtruncnorm(n=1,
                                                           a=0,
                                                           b=1,
                                                           mean=fup.mean,
                                                           sd=sigma.factor*fup.mean)] #draw from trunc normal dist
    #Any samples that were below specified LOD, fix them at LOD/2.
#    indiv_tmp[Funbound.plasma<lod,
#                  Funbound.plasma:=lod/2]
  }
  #indiv_tmp[, junk:=NULL] #remove initialization column
  return(indiv_tmp)
}
