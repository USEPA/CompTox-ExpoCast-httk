#' Predict hematocrit using smoothing spline.
#' 
#' Using precalculated smoothing splines on NHANES log hematocrit vs. age in 
#' months (and KDE residuals) by gender and race/ethnicity, generate hematocrit 
#' values for individuals specified by age, gender, and race/ethnicity.
#' 
#' @param hcttmp_dt A data.table with columns \code{age_years},
#'   \code{age_months}, \code{gender}, \code{reth}.
#'   
#' @return The same data.table with a \code{hematocrit} column added.
#'
#' @keywords httk-pop
#'
#' @author Caroline Ring
#'
#' @references Ring, Caroline L., et al. "Identifying populations sensitive to 
#'environmental chemicals by simulating toxicokinetic variability." Environment 
#'International 106 (2017): 105-118
#' @import stats
#' @export estimate_hematocrit

estimate_hematocrit <- function(gender,
                                reth,
                                age_years,
                                age_months){
  
  #R CMD CHECK throws notes about "no visible binding for global variable", for
  #each time a data. table column name is used without quotes. To appease R CMD
  #CHECK, a variable has to be created for each of these column names and set to
  #NULL. Note that within the data.table, these variables will not be NULL! Yes,
  #this is pointless and annoying.
  id <- age_years <- log_hematocrit <- hct_spline <- age_months <- NULL
  hct_kde <- hematocrit <- gender <- reth <- NULL
  #End R CMD CHECK appeasement.
  
  hematocrit <- vector(mode = "numeric", length = length(age_months))
  grname <- unique(paste(gender, reth))
  if (any(age_years>=1)){
    n <- sum(age_years>=1)
    #predict conditional mean from age using spline
    log_hematocrit <- predict(scr_spline[[grname]],
                              x = age_months[age_years>=1])$y
    #draw residuals from KDE
    #first take the relevant centers
    kde_centers_gr <- kde_centers[g %in% gender &
                                    r %in% reth &
                                    is.finite(loghctresid),
                                  .(seqn,
                                    loghctresid)]
    #get corresponding weights
    w <- nhanes_mec_svy$variables[kde_centers_gr[, .(seqn)], ][, wtmec6yr]
    #sample centers according to these weights
    centers_samp <- sample(kde_centers_gr$loghctresid,
                           size = n,
                           replace = TRUE,
                           prob = w/sum(w))
    #now sample from kernels around these centers
    resids_samp <- rnorm(n = n,
                         mean = centers_samp,
                         sd = hct_h[[grname]])
    
   hematocrit[age_years>=1] <- exp(log_hematocrit +
                                     resids_samp)
}

#For infants under 1 year, sample hematocrit from log-normal distributions
#based on reference ranges by age
  hematocrit[age_years<1] <- hematocrit_infants(age_months=age_months[age_years<1])

return(hematocrit)
}
