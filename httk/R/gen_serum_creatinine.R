#'Predict GFR.
#'
#'Predict serum creatinine using smoothing splines and kernel density estimates of residual variability
#'
#'@param serumcreat.dt A data.table with columns \code{gender}, \code{reth}, 
#'  \code{age_years}, \code{age_months}, \code{BSA_adj}.
#'  
#'@return The same data.table with a \code{serum_creat} column added, containing 
#' spline-interpolated serum creatinine values.
#'
#'@keywords httk-pop
#'
#'@author Caroline Ring
#'
#'@references Ring, Caroline L., et al. "Identifying populations sensitive to 
#'environmental chemicals by simulating toxicokinetic variability." Environment 
#'International 106 (2017): 105-118
#' @import stats
#' @export gen_serum_creatinine
gen_serum_creatinine <- function(gender,
                                 reth,
                                 age_years,
                                 age_months){
  #R CMD CHECK throws notes about "no visible binding for global variable", for
  #each time a data. table column name is used without quotes. To appease R CMD
  #CHECK, a variable has to be created for each of these column names and set to
  #NULL. Note that within the data.table, these variables will not be NULL! Yes,
  #this is pointless and annoying.
  id <- age_years <- log_serum_creat <- sc_spline <- age_months <- NULL
  sc_kde <- serum_creat <- gfr_est <- gender <- reth <- BSA_adj <- NULL
  #End of R CMD CHECK appeasement.
  

  #initialize serum creatinine vector
  serum_creat <- vector(mode = "numeric", length = length(age_months))
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