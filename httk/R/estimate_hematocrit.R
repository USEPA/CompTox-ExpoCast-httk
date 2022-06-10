#'Generate hematocrit values for a virtual population
#'
#'Predict hematocrit from age using smoothing splines and kernel density
#'estimates of residual variability fitted to NHANES data, for a given
#'combination of gender and NHANES race/ethnicity category.
#'
#'This function should usually not be called directly by the user. It is used by
#'\code{httkpop_generate()} in "virtual-individuals" mode, after drawing gender,
#'NHANES race/ethnicity category, and age from their NHANES
#'proportions/distributions.
#'
#'@param gender Gender for which to generate hematocrit values ("Male" or
#'  "Female")
#'@param reth NHANES race/ethnicity category for which to generate serum
#'  creatinine values ("Mexican American", "Non-Hispanic Black", "Non-Hispanic
#'  White", "Other", or "Other Hispanic")
#'@param age_months vector of ages in months for individuals for whom to
#'  generate hematocrit values (between 0-959 months)
#'@param age_years Vector of ages in years for individuals for whom to generate
#'  hematocrit values (corresponding to age_months)
#'
#'@return A vector of numeric generated hematocrit values (blood percentage red
#'  blood cells by volume).
#'
#'@keywords httk-pop
#'
#'@author Caroline Ring
#'
#'@references Ring, Caroline L., et al. "Identifying populations sensitive to
#'  environmental chemicals by simulating toxicokinetic variability."
#'  Environment International 106 (2017): 105-118
#'@import stats

estimate_hematocrit <- function(gender,
                                reth,
                                age_years,
                                age_months,
                                nhanes_mec_svy){
  
  #R CMD CHECK throws notes about "no visible binding for global variable", for
  #each time a data. table column name is used without quotes. To appease R CMD
  #CHECK, a variable has to be created for each of these column names and set to
  #NULL. Note that within the data.table, these variables will not be NULL! Yes,
  #this is pointless and annoying.
  g <- r <- loghctresid <- seqn <- wtmec6yr <- NULL
  #End R CMD CHECK appeasement.
  
  hematocrit <- rep(NA_real_, length(age_months))
  grname <- unique(paste(gender, reth))
  if (any(age_years>=1)){
    n <- sum(age_years>=1)
    #calculate NHANES residuals
    nhanes_sub <- nhanes_mec_svy$variables[riagendr %in% gender &
                                             ridreth1 %in% reth &
                                             is.finite(lbxhct),
                                           .(ridexagm, lbxhct, wtmec6yr)]
    w <- nhanes_sub[, wtmec6yr/sum(wtmec6yr)]
    #fit smoothing spline
    splinefit <- smooth.spline(x = nhanes_sub$ridexagm,
                               y = log(nhanes_sub$lbxhct),
                               w = w)
    loghctresid <- residuals(splinefit)
    
    #sample from centers
    centers_samp <- sample(x = loghctresid,
                           size = n,
                           replace = TRUE,
                           prob = w)
    
    #get optimal bandwidth
    #h <- ks::hpi(x = loghctresid)
    h <- hct_h[[grname]]
    
    #now sample from kernels around these centers
    resids_samp <- rnorm(n = n,
                         mean = centers_samp,
                         sd = h)
    
    #predicted values for sampled individuals
    log_hematocrit_pred <- predict(splinefit, 
                                    x=age_months[age_years>=1])$y
    
   hematocrit[age_years>=1] <- exp(log_hematocrit_pred +
                                     resids_samp)
}

#For infants under 1 year, sample hematocrit from log-normal distributions
#based on reference ranges by age
  hematocrit[age_years<1] <- hematocrit_infants(age_months=age_months[age_years<1])

return(hematocrit)
}
