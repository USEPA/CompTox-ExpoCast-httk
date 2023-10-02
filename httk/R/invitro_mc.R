#' Monte Carlo for in vitro toxicokinetic parameters including uncertainty and variability.
#' 
#' @description
#' Given a CAS in the HTTK data set, a virtual population from HTTK-Pop, some
#' user specifications on the assumed distributions of Funbound.plasma and
#' Clint, draw "individual" values of Funbound.plasma and Clint from those
#' distributions. The methodology for this function was developed and described
#' by Wambaugh et al. (2019) (\doi{10.1093/toxsci/kfz205}).
#'
#' @details
#' The Monte Carlo methods used here were recently updated and described by
#' Breen et al. (submitted).
#' 
#' @param parameters.dt A data table of physiological and chemical-specific parameters
#' 
#' @param parameters A list of chemical-specific model parameters containing at
#' least Funbound.plasma, Clint, and Fhep.assay.correction.
#' 
#' @param samples The number of samples to draw.
#' 
#' @param fup.meas.mc Logical -- should we perform measurment (uncertainty)
#' Monte Carlo for \code{Funbound.plasma} values (Default TRUE). If FALSE, 
#' the user may choose to provide columns for "unadjusted.Funbound.plasma" or
#' "fup.mean" from their own methods. 
#' 
#' @param fup.pop.mc Logical -- should we perform population (variability)
#' Monte Carlo for \code{Funbound.plasma} values (Default TRUE)
#' 
#' @param clint.meas.mc Logical -- should we perform measurment (uncertainty)
#' Monte Carlo for \code{Clint} values (Default TRUE)
#' 
#' @param clint.pop.mc Logical -- should we perform population (variability)
#' Monte Carlo for \code{Clint} values (Default TRUE)
#' 
#' @param fup.meas.cv Coefficient of variation of distribution of measured
#' \code{Funbound.plasma} values. 
#' 
#' @param clint.meas.cv Coefficient of variation of distribution of measured 
#' \code{Clint} values.
#' 
#' @param fup.pop.cv Coefficient of variation of distribution of population
#' \code{Funbound.plasma} values.
#' 
#' @param clint.pop.cv Coefficient of variation of distribution of population
#' \code{Clint} values.
#' 
#' @param poormetab Logical. Whether to include poor metabolizers in the Clint
#' distribution or not.
#' 
#' @param fup.lod The average limit of detection for \code{Funbound.plasma}, below
#' which distribution will be censored if fup.censored.dist is TRUE. Default 0.01.
#' 
#' @param fup.censored.dist Logical. Whether to draw \code{Funbound.plasma} from a
#' censored distribution or not.
#' 
#' @param adjusted.Funbound.plasma Uses Pearce et al. (2017) lipid binding adjustment
#' for Funbound.plasma when set to TRUE (Default).
#' 
#' @param adjusted.Clint Uses Kilford et al. (2008) hepatocyte incubation
#' binding adjustment for Clint when set to TRUE (Default).
#' 
#' @param clint.pvalue.threshold Hepatic clearance for chemicals where the in
#' vitro clearance assay result has a p-values greater than the threshold are
#' set to zero.
#' 
#' @param minimum.Funbound.plasma Monte Carlo draws less than this value are set 
#' equal to this value (default is 0.0001 -- half the lowest measured Fup in our
#' dataset).
#' 
#' @param caco2.meas.sd Standard deviation of the measured oral absorption - numeric value (Default 0.3).
#' 
#' @param caco2.pop.sd Standard deviation of the population level oral absorption - numeric value (Default 0.3).
#' 
#' @param Caco2.Fabs = TRUE uses Caco2.Pab to calculate
#' fabs.oral, otherwise fabs.oral = \code{Fabs}. 
#' 
#' @param Caco2.Fgut = TRUE uses Caco2.Pab to calculate 
#' fgut.oral, otherwise fgut.oral = \code{Fgut}. 
#' 
#' @param keepit100 = TRUE overwrites Fabs and Fgut with 1 (i.e. 100 percent) regardless of other settings.
#' 
#' @return A data.table with three columns: \code{Funbound.plasma} and
#' \code{Clint}, containing the sampled values, and
#' \code{Fhep.assay.correction}, containing the value for fraction unbound in
#' hepatocyte assay.
#'
#' @author Caroline Ring and John Wambaugh
#'
#' @references
#' \insertRef{wambaugh2019assessing}{httk} 
#'
#' \insertRef{breen2022simulating}{httk} 
#' 
#' \insertRef{kilford2008hepatocellular}{httk} 
#' @examples
#' \donttest{
#' #Simply generate a virtual population of 100 individuals,
#' #using the direct-resampling method
#' set.seed(42)
#'
#' # Pull mean vchemical=specific values:
#' chem.props <- parameterize_pbtk(chem.name="bisphenolaf")
#'
#' # Convert to data.table with one row per sample:
#' parameters.dt <- monte_carlo(chem.props,samples=100)
#'
#' # Use httk-pop to generate a population:
#' pop <- httkpop_generate(method='direct resampling', nsamp=100)
#'
#' # Overwrite parameters specified by httk-pop:
#' parameters.dt[,names(pop):=pop]
#' # Vary in vitro parameters:
#' parameters.dt <- invitro_mc(parameters.dt,samples=100)
#' }
#'
#' @keywords monte-carlo in-vitro
#'
#' @import stats
#'
#' @export invitro_mc

invitro_mc <- function(parameters.dt=NULL,
                           samples,
                           fup.meas.mc = TRUE,
                           fup.pop.mc = TRUE,
                           clint.meas.mc = TRUE,
                           clint.pop.mc = TRUE,
                           fup.meas.cv=0.4,
                           clint.meas.cv=0.3,                           
                           fup.pop.cv=0.3,
                           clint.pop.cv=0.3,
                           caco2.meas.sd = 0.3,
                           caco2.pop.sd = 0.3,
                           Caco2.Fgut = TRUE,
                           Caco2.Fabs = TRUE,
                           keepit100 = FALSE,
                           poormetab=TRUE,
                           fup.lod=0.01,
                           fup.censored.dist=FALSE,
                           adjusted.Funbound.plasma=TRUE,
                           adjusted.Clint=TRUE,
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
  Funbound.plasma.dist<-fup.sd<-Fhep.assay.correction <- NULL
  PoorMetabolizer <- NULL
  Caco2.Pab.dist <- Caco2.Pab.mu <- Fabs <- Fgut <- Fabsgut <- NULL
  #End R CMD CHECK appeasement.

  # Are we doing clint measurmement Monte Carlo?
  if (is.null(clint.meas.cv))
  {
    clint.meas.mc <- FALSE
  }
  # Are we doing clint population Monte Carlo?
  if (is.null(clint.pop.cv))
  {
    clint.pop.mc <- FALSE
  }
  # Are we doing fup measurmement Monte Carlo?
  if (is.null(clint.meas.cv))
  {
    fup.meas.mc <- FALSE
  }
  # Are we doing fup population Monte Carlo?
  if (is.null(fup.pop.cv))
  {
    fup.pop.mc <- FALSE
  }
  
  


  if (!("Funbound.plasma") %in% names(parameters.dt))
    stop("Funbound.plasma needed in invitro_mc.")

  if (any(!(c(
    "Clint",
    "pKa_Donor",
    "pKa_Accept",
    "Pow") %in% names(parameters.dt))) &
    !("Dow74" %in% names(parameters.dt)))
    stop("Either Dow74 or Clint, pKa_Donor, pKa_Accept and Pow needed in invitro_mc.")

  if (!"Dow74"%in%names(parameters.dt))
  {
      pKa_Donor <- parameters.dt[["pKa_Donor"]]
      pKa_Accept <- parameters.dt[["pKa_Accept"]]
      Pow <- parameters.dt[["Pow"]] # Octanol:water partition coeffiecient
      ion <- calc_ionization(pH=7.4,pKa_Donor=pKa_Donor,pKa_Accept=pKa_Accept)
      dow <- Pow * (ion$fraction_neutral + 0.001 * ion$fraction_charged + ion$fraction_zwitter)
      parameters.dt[,Dow74:=dow]
  }

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
  # If the clint.meas.mc == FALSE then we just use the point estimate with no
  # uncertainty:
  if (!clint.meas.mc)
  {         
    Clint <- parameters.dt[["Clint"]]
    Clint.l95 <- NULL
    Clint.u95 <- NULL
    Clint.pvalue <- 0
    parameters.dt[,Clint:=Clint]
  } else {
    
    # We need to determine what sort of information we have been provided about
    # measurment uncertainty. We first check for a comma separated list with a
    # median, lower, and upper 95th credible interval limits:
  
    if (all(!is.na(parameters.dt$Clint.dist)))
    {
      if (nchar(parameters.dt$Clint.dist[1]) -
        nchar(gsub(",","",parameters.dt$Clint.dist[1]))!=3) 
      {
        stop("Clint distribution should be four values (median,low95th,high95th,pValue) separated by commas.")
      }
      temp <- strsplit(parameters.dt$Clint.dist,",")
      Clint <- as.numeric(temp[[1]][1])
      Clint.l95 <- as.numeric(temp[[1]][2])
      Clint.u95 <- as.numeric(temp[[1]][3])
      Clint.pvalue <- as.numeric(temp[[1]][4])
    # If we don't have that, we use the default coefficient of variation to
    # generate confidence limits:
    } else {
      Clint <- parameters.dt[["Clint"]]
      Clint.l95 <- sapply(Clint*(1 - clint.meas.cv*1.96),function(x) max(x,10^-3))
      Clint.u95 <- Clint*(1 + clint.meas.cv*1.96)
      Clint.pvalue <- 0 # Currently we don't story the pvalue in parameters.dt
    }
    
  # Shrink it down if we don't have unique values:
    if (all(c(length(unique(Clint))==1,
      length(unique(Clint.l95))==1,
      length(unique(Clint.u95))==1,
      length(unique(Clint.pvalue))==1)))
    {
      Clint <- Clint[1]
      Clint.l95 <- Clint.l95[1]
      Clint.u95 <- Clint.u95[1]
      Clint.pvalue <- Clint.pvalue[1]
    }
    
    # Now do the uncertainty Monte Carlo analysis -- draw a series of plausible 
    # "true" values for Clint that are consistent with the measurment .
    # If a credible interval was specified for Clint, draw from that interval.
    # First check if at least the upper limit on the 95 percent interval is non-zero:
    if (Clint.u95>0)
    {
      # Check if the median is zero:
      if (Clint == 0)
      {
        # Use our custom median 0 non-zero upper 95th function:
        parameters.dt[, Clint := rmed0non0u95(
            n=samples,
            x.u95 = Clint.u95,
            x.min = 0,
            x.LOD = Clint.u95/1e2)]
      } else {
        # Optimize to find parameters.dt for a log-normal distribution that have
        # the least squares difference using the three quantiles (median, l95, u95)
        clint.fit <- suppressWarnings(optim(c(log(Clint),clint.meas.cv), function(x) (0.95-
                                                plnorm(Clint.u95,x[1],x[2])+
                                                plnorm(Clint.l95,x[1],x[2]))^2+
                                                (Clint-qlnorm(0.5,x[1],x[2]))^2))
        parameters.dt[,Clint:=rlnorm(n=samples,clint.fit$par[1],clint.fit$par[2])]
      }
      # Check to see if we should enforce some zero entries:
      N.clint.zero <- dim(parameters.dt[Clint==0,])[1]
      N.clint.zero.req <- round(Clint.pvalue*samples)
      if (N.clint.zero < N.clint.zero.req)
      {
        # Pick the N.clint.zero.req lowest Clints:
        thresh <- sort(parameters.dt[,Clint,with=TRUE])[N.clint.zero.req]
        # Change some non-zero Clints to zero:
        parameters.dt[Clint <= thresh, Clint:=0] 
      }
    } else {
      # All quantiles are zero:
      parameters.dt[,Clint:=0]
    }
  }

  # Determine the value for fraction unbound in hepatocyte assay (depends on
  # phys-chem but does not vary biologically):
  # First check that this model has phys-chem parameters:
  if (all(c("Pow","pKa_Donor","pKa_Accept")%in%colnames(parameters.dt)))
    parameters.dt[,Fhep.assay.correction:=calc_hep_fu(parameters=parameters.dt)]
  if (adjusted.Clint)
  {
    # Correct for fraction of chemical unbound in in vitro hepatocyte assay:
    parameters.dt[, Clint := apply_clint_adjustment(
                               Clint,
                               Fu_hep=Fhep.assay.correction,
                               suppress.messages=TRUE)]
  }
  #
  #
  #
  # fup uncertainty Monte Carlo:
  #
  #
  #
  # If the fup.meas.mc == FALSE we just use the point estimate with no
  # uncertainty simulation:
  if (!fup.meas.mc)
  {
    Funbound.plasma <- parameters.dt$Funbound.plasma
    Funbound.plasma.l95 <- NULL
    Funbound.plasma.u95 <- NULL
    if (!"unadjusted.Funbound.plasma" %in% colnames(parameters.dt))
    {
      parameters.dt[, unadjusted.Funbound.plasma:=Funbound.plasma]
    }
  } else {
    # We need to determine what sort of information we have been provided about
    # measurment uncertainty. 
    #
    # We first check for a comma separated list with a
    # median, lower, and upper 95th credible interval limits:
    if(!is.na(parameters.dt$Funbound.plasma.dist[1]))
    {
      # Check formatting:
      if (nchar(parameters.dt$Funbound.plasma.dist[1]) - 
        nchar(gsub(",","",parameters.dt$Funbound.plasma.dist[1]))!=2)
      {
        stop("Funbound.plasma distribution should be three values (median,low95th,high95th) separated by commas.")
      }
      # Split into quantiles:
      temp <- strsplit(parameters.dt$Funbound.plasma.dist,",")
      Funbound.plasma <- as.numeric(temp[[1]][1])
      Funbound.plasma.l95 <- as.numeric(temp[[1]][2])
      Funbound.plasma.u95 <- as.numeric(temp[[1]][3])
    # If we don't have actual quantiles, use the coefficient of
    # variation (fup.meas.cv), to approximate confidence limits:
    } else {
      Funbound.plasma <- parameters.dt$Funbound.plasma
      Funbound.plasma.l95 <- sapply(Funbound.plasma*(1-fup.meas.cv*1.96),
        function(x) max(x,0))
      Funbound.plasma.u95 <- sapply(Funbound.plasma*(1+fup.meas.cv*1.96),
        function(x) min(x,1))
    }
  
   # For computational efficiency, shrink the median and quantiles down to a 
   # single value if there is only one value for each quantile
   if (all(c(length(unique(Funbound.plasma))==1,
      length(unique(Funbound.plasma.l95))==1,
      length(unique(Funbound.plasma.u95))==1)))
    {
      Funbound.plasma <- Funbound.plasma[1]
      Funbound.plasma.l95 <- Funbound.plasma.l95[1]
      Funbound.plasma.u95 <- Funbound.plasma.u95[1]
    }
    
    # Check to see if median fup was below lod:
    if (Funbound.plasma==0)
    {
       # Is the upper bound non-zero?
       if (Funbound.plasma.u95 > minimum.Funbound.plasma)
       {
          # If so, make the median LOD and the upper 95th quantile match the measured value:
          parameters.dt[, unadjusted.Funbound.plasma := rmed0non0u95(
            n=samples,
            x.u95 = Funbound.plasma.u95,
            x.min = minimum.Funbound.plasma,
            x.LOD = fup.lod)]
       } else {
         # Otherwise, assign values between the minimum and the lod:
         parameters.dt[, unadjusted.Funbound.plasma := runif(
           n=samples, 
           minimum.Funbound.plasma,
           fup.lod)]
       }
    } else {
      # Median Fup is non-zero.
      # Check to see if lower bound of confidence/credible interval is 1:
      if (Funbound.plasma.l95 == 1)
      {
        # If so all values set to 1:
        parameters.dt[, unadjusted.Funbound.plasma:=1]  
      } else {
        # Lower bound is not 1.
        # Check to see if median fup was below minimum allowed value:
        if (Funbound.plasma<minimum.Funbound.plasma)
        {
          # Is the upper bound non-zero?
          if (Funbound.plasma.u95 > minimum.Funbound.plasma)
          {
            # If so, make the median LOD and the upper 95th quantile match the measured value:
            parameters.dt[, unadjusted.Funbound.plasma := rmed0non0u95(
              n=samples,
              x.u95 = Funbound.plasma.u95,
              x.min = minimum.Funbound.plasma,
              x.LOD = fup.lod)]
          } else {
            # Otherwise, assign the minimum to all samples:
            parameters.dt[, unadjusted.Funbound.plasma := minimum.Funbound.plasma]
          }
        } else {
          # Median is above minimum allowed value

          # Initial conditions for optimizer depend on how close median is to 1:
          # If the median is one we have a special case:
          Funbound.med.is.one <- Funbound.plasma == 1
          if (Funbound.plasma < 0.99)
          {
            # Decent guess at initial values:
            initial.values <- c(2,
              (1-Funbound.plasma)/Funbound.plasma*2)
          } else {
            # temporarily reduce median to just less than 1:
            if (Funbound.med.is.one) Funbound.plasma <- 1 - 10^-3 
            # more likely to convege if we start with a distribution skewed toward 1
            initial.values <- c(2,1)
          }
          
          # Use optim to estimate parameters for a beta distribution (alpha and beta)
          # such that the median and 95% credible interval approximate the given values:
          ppb.fit <- suppressWarnings(optim(initial.values, 
            function(x) (0.95-
            pbeta(Funbound.plasma.u95,x[1],x[2])+
            pbeta(Funbound.plasma.l95,x[1],x[2]))^2+
            (Funbound.plasma-qbeta(0.5,x[1],x[2]))^2,
            method="BFGS"))
          # We are drawing new values for the unadjusted Fup:
          parameters.dt[, unadjusted.Funbound.plasma:=rbeta(n=samples,
            ppb.fit$par[1],
            ppb.fit$par[2])]
            
          # Check to see if we need to adjust for median = 1:
          if (Funbound.med.is.one)
          {
            # Assign median its old value
            Funbound.plasma <- 1
            # Set the highest 50% to 1:
            med.val <- median(parameters.dt[, unadjusted.Funbound.plasma, with=TRUE])
            parameters.dt[unadjusted.Funbound.plasma > med.val, 
              unadjusted.Funbound.plasma := 1]
          }
        }
      } 
    }
    #if measured Funbound.plasma > 1, then set it to 1
    parameters.dt[unadjusted.Funbound.plasma>1, unadjusted.Funbound.plasma := 1]
  }
  
  # Check to see if we are adjusting for differences between in vitro and 
  # physiological lipid partitioning (Pearce, 2017):
  if (adjusted.Funbound.plasma)
  {
    # We need the fraction of lipid in plasma:
    Flipid <-subset(httk::physiology.data,
               Parameter=='Plasma Effective Neutral Lipid Volume Fraction')[,
               which(colnames(httk::physiology.data) == 'Human')]

    if (all(c("Pow","pKa_Donor","pKa_Accept") %in% names(parameters.dt)) | 
        ("Dow74" %in% names(parameters.dt)))
    {
      # put the unadjusted fup where calc_fup_correction will look for it:
      parameters.dt[, Funbound.plasma:=unadjusted.Funbound.plasma]
      parameters.dt[, Funbound.plasma.adjustment:=
        calc_fup_correction(
          parameters = parameters.dt)]
    } else stop("Missing phys-chem parameters in invitro_mc for calc_fup_correction.") 
  } else {
    parameters.dt[, Funbound.plasma.adjustment:=1]
  }
  
  # Check that user didn't provide fup.mean:
  if (fup.meas.mc | !("fup.mean" %in% colnames(parameters.dt)))
  {
    # After uncertainty simulation (if any) values become population means:
    parameters.dt[, fup.mean:=
      unadjusted.Funbound.plasma*Funbound.plasma.adjustment]
  }

  #
  #
  #
  # Caco-2 uncertainty Monte Carlo:
  #
  #
  #
  # If the default CV is set to NULL, we just use the point estimate with no
  # uncertainty:
  if(keepit100 == FALSE & 
     (Caco2.Fgut == TRUE | Caco2.Fabs == TRUE))
  {
    if (is.null(caco2.meas.sd))
    {
      Caco2.Pab <- parameters.dt$Caco2.Pab
      Caco2.Pab.l95 <- NULL
      Caco2.Pab.u95 <- NULL
      parameters.dt[, Caco2.Pab := Caco2.Pab]
      # We need to determine what sort of information we have been provided about
      # measurment uncertainty. We first check for a comma separated list with a
      # median, lower, and upper 95th credible interval limits:
    } else if(all(!is.na(parameters.dt$Caco2.Pab.dist)))
    {
      if (any(nchar(parameters.dt$Caco2.Pab.dist) - 
          nchar(gsub(",","",parameters.dt$Caco2.Pab.dist[1]))!=2))
      {
        stop("Caco2.Pab distribution should be three values (median,low95th,high95th) separated by commas.")
      }
      temp <- strsplit(parameters.dt$Caco2.Pab.dist,",")
      Caco2.Pab <- as.numeric(temp[[1]][1])
      Caco2.Pab.l95 <- as.numeric(temp[[1]][2])
      Caco2.Pab.u95 <- as.numeric(temp[[1]][3])
      
    # Shrink it down if all the values are the same
      if (all(c(length(unique(Caco2.Pab))==1,
        length(unique(Caco2.Pab.l95))==1,
        length(unique(Caco2.Pab.u95))==1)))
      {
        Caco2.Pab <- Caco2.Pab[1]
        Caco2.Pab.l95 <- Caco2.Pab.l95[1]
        Caco2.Pab.u95 <- Caco2.Pab.u95[1]
      }      
      
      caco2.fit <- suppressWarnings(optim(c(Caco2.Pab, caco2.meas.sd), 
                     function(x) 
                       # 97.5% of values should be less than the u95
                       (0.975 - pnorm(Caco2.Pab.u95, x[1], x[2]))^2 +
                       # 2.5% of values should be less than the l96
                       (0.025 - pnorm(Caco2.Pab.l95, x[1], x[2]))^2 +
                       # The median should be the median:
                       (Caco2.Pab - qnorm(0.5, x[1], x[2]))^2))
      parameters.dt[, Caco2.Pab := rtnorm(n = samples, 
                                     caco2.fit$par[1], 
                                     caco2.fit$par[2],
                                     lower=0)]
      
      # If we don't have that, we use the default coefficient of variation to
      # generate confidence limits:
      
    } else if(!is.null(caco2.meas.sd)) {
      Caco2.Pab <- parameters.dt$Caco2.Pab
      
      # Shrink it down if all the values are the same
      if (length(unique(Caco2.Pab))==1)
      {
        Caco2.Pab <- Caco2.Pab[1]
      }   
      
      caco2.fit <- suppressWarnings(optim(Caco2.Pab, 
                                          function(x) (Caco2.Pab - qnorm(0.5, x[1], abs(caco2.meas.sd)))^2))
      caco2.fit$par[2] <- abs(caco2.meas.sd)
      parameters.dt[, Caco2.Pab := rnorm(n = samples, caco2.fit$par[1], caco2.fit$par[2])]
      
    } 
    
    # Store NA so data.table doesn't convert everything to text:
    parameters.dt[, Caco2.Pab.dist := NA]
  }
  
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
  if (clint.pop.mc & Clint>0)
  {
    #Draw Clint from a normal distribution if poor metabolizers excluded, or
    #Gaussian mixture distribution if poor metabolizers included.
    #Set the mean of the regular metabolizer distribution:
    parameters.dt[,Clint.mu:=Clint]
    parameters.dt[,PoorMetabolizer := "N"]
    if (poormetab) #if poor metabolizers are included:
    {
      #Assume that 5% of the population has 10% the metabolism:
      parameters.dt[rbinom(n=samples,size=1,prob=0.05)==1, 
        PoorMetabolizer := "Y"]
      parameters.dt[PoorMetabolizer == "Y", Clint.mu:=Clint.mu/10]
    }
    #Draw Clint from a normal distribution with mean = measured Clint, and
    #coefficient of variation given by clint.pop.cv.
    parameters.dt[,Clint:=truncnorm::rtruncnorm(n=1,
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
  # long as fup.pop.mc isn't FALSE (otherwise, no pop variability for this)
  if (fup.pop.mc)
  {
    parameters.dt[,fup.sd:=fup.pop.cv*fup.mean]
    parameters.dt[,fup.lod:=fup.lod]
    # add check for fup.pop.cv=NULL, smae for clint
    if (fup.censored.dist)
    { #if user specified to use a censored distribution,
      #then draw from a normal distribution, left-censored at the specified LOD.
      parameters.dt[, Funbound.plasma:=r_left_censored_norm(n=1,
                                                          mean=fup.mean,
                                                          sd=fup.sd,
                                                          lod=fup.lod)]
      # Enforce maximum value:
      parameters.dt[Funbound.plasma>1, Funbound.plasma:=1]                                                   
    } else { #if user specified to use a non-censored distribution
      #Draw Funbound.plasma from a normal distribution, truncated at 0 and 1.
      parameters.dt[, Funbound.plasma:=truncnorm::rtruncnorm(n=1,
                                                             a=0,
                                                             b=1,
                                                             mean=fup.mean,
                                                             sd=fup.sd)] 
    }
  } else {
  # No population variability simulation:
    parameters.dt[,Funbound.plasma:=fup.mean]
  }

  #Enforce a minimum Funbound.plasma :
  parameters.dt[Funbound.plasma<minimum.Funbound.plasma,
    Funbound.plasma:=minimum.Funbound.plasma]

  #
  #
  #
  # Caco2.Pab variability Monte Carlo:
  #
  #
  #
  #do not sample if user said not to vary Caco2.Pab.
  if(keepit100 == FALSE & 
     (Caco2.Fgut == TRUE | Caco2.Fabs == TRUE))
  {
    if (!is.null(caco2.pop.sd))
    {
      #Draw Pab from a normal distribution if poor metabolizers excluded, or
      #Gaussian mixture distribution if poor metabolizers included.
      #Set the mean of the regular metabolizer distribution:
      parameters.dt[, Caco2.Pab.mu := Caco2.Pab]
      
      #Draw Pab from a normal distribution with mean = measured Clint, and
      #coefficient of variation given by clint.pop.cv.
      # We use truncnorm::rtruncnorm becase mean can be a vector:
      parameters.dt[, Caco2.Pab := 10^sapply(log10(Caco2.Pab.mu),
                                             rnorm,n=1,
                                             sd=caco2.pop.sd)
        ]
    }
  } else if (keepit100 == TRUE)
  {
    parameters.dt[,Fabs:=1]
    parameters.dt[,Fgut:=1]
  }
  
  # Make sure Fabsgut gets recalculated:
  parameters.dt[, Fabsgut := NA]

# set precision:
  cols <- colnames(parameters.dt)
  parameters.dt[ , (cols) := lapply(.SD, set_httk_precision), .SDcols = cols]
 
   return(parameters.dt)
}
