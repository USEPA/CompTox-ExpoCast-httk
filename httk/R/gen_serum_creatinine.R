#' Generate serum creatinine values for a virtual population.
#'
#' Predict serum creatinine from age using smoothing splines and kernel density
#'estimates of residual variability fitted to NHANES data,, for a given
#'combination of gender and NHANES race/ethnicity category.
#'
#' This function should usually not be called directly by the user. It is used
#' by \code{httkpop_generate()} in "virtual-individuals" mode, after drawing
#' gender, NHANES race/ethnicity category, and age from their NHANES
#' proportions/distributions.
#'
#'@param gender Gender for which to generate serum creatinine values ("Male" or
#'  "Female")
#'@param reth NHANES race/ethnicity category for which to generate serum
#'  creatinine values ("Mexican American", "Non-Hispanic Black", "Non-Hispanic
#'  White", "Other", or "Other Hispanic")
#'@param age_months vector of ages in months for individuals for whom to
#'  generate serum creatinine values (between 0-959 months)
#'@param age_years Vector of ages in years for individuals for whom to generate
#'  serum creatinine values (corresponding to age_months)
#' @param nhanes_mec_svy \code{surveydesign} object created from
#'  \code{\link{mecdt}} using \code{\link[survey]{svydesign}} (this is done in
#'  \code{\link{httkpop_generate}})
#'@return A vector of numeric generated serum creatinine values (mg/dL).
#'
#'@keywords httk-pop
#'
#'@author Caroline Ring
#'
#'@references 
#' \insertRef{ring2017identifying}{httk} 
#'@import stats

gen_serum_creatinine <- function(gender,
                                 reth,
                                 age_years,
                                 age_months,
                                 nhanes_mec_svy){
  #R CMD CHECK throws notes about "no visible binding for global variable", for
  #each time a data. table column name is used without quotes. To appease R CMD
  #CHECK, a variable has to be created for each of these column names and set to
  #NULL. Note that within the data.table, these variables will not be NULL! Yes,
  #this is pointless and annoying.
 riagendr <- ridreth1 <- lbxscr <- ridexagm <- wtmec6yr <- NULL
  #End of R CMD CHECK appeasement.
  
  
  #initialize serum creatinine vector
  serum_creat <- rep(NA_real_, length(age_months))
  grname <- unique(paste(gender, reth))
  
  if (any(age_years>=12)){
    n <- sum(age_years>=12)
    
    
    #calculate NHANES residuals
    nhanes_sub <- nhanes_mec_svy$variables[riagendr %in% gender &
                                             ridreth1 %in% reth &
                                             is.finite(lbxscr),
                                           .(ridexagm, lbxscr, wtmec6yr)]
    w <- nhanes_sub[, wtmec6yr/sum(wtmec6yr)]
    #fit smoothing spline
    splinefit <- smooth.spline(x = nhanes_sub$ridexagm,
                               y = log(nhanes_sub$lbxscr),
                               w = w)
    logscresid <- residuals(splinefit)
    
    
    #sample from centers
    centers_samp <- sample(x = logscresid,
                           size = n,
                           replace = TRUE,
                           prob = w)
    
    #get optimal bandwidth
    h <- scr_h[[grname]]
    
    #sample from normal distirbution with optimal bandwidth for this gender/reth
    resids_samp <- rnorm(n =n,
                         mean = centers_samp,
                         sd = h)
    #predicted values for sampled individuals
    log_serum_creat_pred <- predict(splinefit, 
                                    x=age_months[age_years>=12])$y
    
    serum_creat[age_years>=12] <- exp(log_serum_creat_pred + resids_samp)
  }
  
  return(serum_creat)
}