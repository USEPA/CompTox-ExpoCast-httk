#' Generate heights and weights for a virtual population.
#' 
#' Generate heights and weights for a virtual population.
#' 
#' @param hbw_dt A data.table describing the virtual population by race,
#' gender, and age (in years and months). Must have variables \code{gender},
#' \code{reth}, \code{age}, and \code{age.years}.
#' @return The same data.table with two new variables added: \code{weight} and
#' \code{height}. Respectively, these give individual body weights in kg, and
#' individual heights in cm.
#'
#' @author Caroline Ring
#'
#' @references Ring, Caroline L., et al. "Identifying populations sensitive to
#' environmental chemicals by simulating toxicokinetic variability."
#' Environment International 106 (2017): 105-118
#'
#' @keywords httk-pop
#'
#' @import stats
#' @importFrom mvtnorm rmvnorm
#'
#' @export gen_height_weight
gen_height_weight <- function(gender,
                              reth,
                              age_months){
  
  #R CMD CHECK throws notes about "no visible binding for global variable", for
  #each time a data.table column name is used without quotes. To appease R CMD
  #CHECK, a variable has to be created for each of these column names and set to
  #NULL. Note that within the data.table, these variables will not be NULL! Yes,
  #this is pointless and annoying.
  mean_logh <- g <- gender <- r <- reth <- height_spline <- NULL
  age_months <- mean_logbw <- weight_spline <- hw_kde <- nkde <- NULL
  id <- weight <- NULL
  logbw_resid <-   height <- logh_resid <- NULL
  #End R CMD CHECK appeasement.
  
   n <- length(age_months)
   gr <- unique(paste(gender, reth))
   
  #Get "mean" heights (cm) and bodyweights (kg) for each individual
  #Subset dt by gender and race/ethnicity,
  #then predict mean heights and BWs from ages
  #using the spline for that gender/race combination
 mean_logh <- predict(height_spline[[grname]], 
                            x=age_months)$y
  mean_logbw <- predict(weight_spline[[grname]], 
                             x=age_months)$y
  
  #Draw log BW and log height residuals from the 2-d KDE
  #Procedure: 
  #for each individual of a gender/race combination,
  #resample a center point from the KDE (one of the original data points),
  #using the sample weights used to construct the KDE in the first place,
  #and randomly draw a value from the 2-D distribution about that center point,
  #using the optimal bandwidth matrix calculated when constructing the KDE.
  kde_centers_gr <- kde_centers[g==gender &
                                  r==reth &
                                  is.finite(logwresid) &
                                  is.finite(loghresid),
                                .(seqn,
                                  logwresid,
                                  loghresid)]
  #merge in wtmec6yr
  kde_centers_gr <- nhanes_mec_svy$variables[kde_centers_gr,
                           on = "seqn"][,
                                        .(seqn,
                                          logwresid,
                                          loghresid,
                                          wtmec6yr)]
  
  #get residuals: draw from the multivariate normal dist 
  #with centers randomly chosen from the original residuals,
  #and the optimal bandwidth matrix
  centers_id <- sample(nrow(kde_centers_gr),
                       size = n,
                       replace = TRUE,
                       prob = kde_centers_gr$wtmec6yr/sum(kde_centers_gr$wtmec6yr))
  centers <- kde_centers_gr[centers_id,
                            .(logwresid,
                              loghresid)]
  H <- hw_H[[grname]] #KDE bandwidth for this gender & race combination
  
  resids <- centers[,
                    c("logw_resid",
                      "logh_resid"):=rmvnorm(n = 1,
                    mean = c(logwresid, loghresid),
                    sigma = H),
                    by = 1:nrow(centers)]
  
   weight <- exp(mean_logbw + resids$logw_resid)
   height <- exp(mean_logh + resids$logh_resid)

  return(list(weight=weight,
              height = height))
}
