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
#'
#'@return A vector of numeric generated serum creatinine values (mg/dL).
#'
#'@keywords httk-pop
#'
#'@author Caroline Ring
#'
#'@references Ring, Caroline L., et al. "Identifying populations sensitive to
#'  environmental chemicals by simulating toxicokinetic variability."
#'  Environment International 106 (2017): 105-118
#'@import stats

gen_serum_creatinine <- function(gender,
                                 reth,
                                 age_years,
                                 age_months){
  #R CMD CHECK throws notes about "no visible binding for global variable", for
  #each time a data. table column name is used without quotes. To appease R CMD
  #CHECK, a variable has to be created for each of these column names and set to
  #NULL. Note that within the data.table, these variables will not be NULL! Yes,
  #this is pointless and annoying.
  g <- r <- seqn <- logscresid <- wtmec6yr <- NULL
  #End of R CMD CHECK appeasement.
  

  #initialize serum creatinine vector
  serum_creat <- rep(NA_real_, length(age_months))
  grname <- unique(paste(gender, reth))

  if (any(age_years>=12)){
    n <- sum(age_years>=12)
   log_serum_creat_pred <- predict(scr_spline[[grname]], 
                                        x=age_months[age_years>=12])$y
   
   #centers for KDE sampling
   kde_centers_gr <- kde_centers[g %in% gender &
                                   r %in% reth &
                                   is.finite(logscresid),
                                 .(seqn,
                                   logscresid)]
   #merge in weights
   kde_centers_gr <- nhanes_mec_svy$variables[kde_centers_gr, on = "seqn"][, .(seqn,
                                                             wtmec6yr,
                                                             logscresid)]
   
   #sample from centers
   centers_samp <- sample(kde_centers_gr$logscresid,
                        size = n,
                        replace = TRUE,
                        prob = kde_centers_gr$wtmec6yr/sum( kde_centers_gr$wtmec6yr))
   
   #sample from normal distirbution with optimal bandwidth for this gender/reth
   resids <- rnorm(n =n,
                   mean = centers_samp,
                   sd = scr_h[[grname]])
   
   serum_creat[age_years>=12] <- exp(log_serum_creat_pred + resids)
  }
 
  return(serum_creat)
}