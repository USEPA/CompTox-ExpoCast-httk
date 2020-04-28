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
gen_height_weight <- function(hbw_dt){
  
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
  
  hbw_dt <- data.table::copy(hbw_dt)
  #Get "mean" heights (cm) and bodyweights (kg) for each individual
  #Subset dt by gender and race/ethnicity,
  #then predict mean heights and BWs from ages
  #using the spline for that gender/race combination
  hbw_dt[, 
         mean_logh:=predict(spline_heightweight[g==gender & r==reth, 
                                                height_spline][[1]], 
                            x=age_months)$y, 
         by=list(gender, reth)]
  hbw_dt[, 
         mean_logbw:=predict(spline_heightweight[g==gender & r==reth, 
                                                 weight_spline][[1]], 
                             x=age_months)$y, 
         by=list(gender,reth)]
  
  #Draw log BW and log height residuals from the 2-d KDE
  #Procedure: 
  #for each individual of a gender/race combination,
  #resample a center point from the KDE (one of the original data points),
  #using the sample weights used to construct the KDE in the first place,
  #and randomly draw a value from the 2-D distribution about that center point,
  #using the optimal bandwidth matrix calculated when constructing the KDE.
  spline_kde <- spline_heightweight[, list(g,r,hw_kde, nkde)]
  setnames(spline_kde,
           c('g', 'r'),
           c('gender', 'reth'))
  
  hbw_dt[, id:=1:nrow(hbw_dt)]
  
  hbw_dt <- merge(hbw_dt, spline_kde,
                  by=c('gender', 'reth'))
  #get residuals: draw from the multivariate normal dist 
  #with centers randomly chosen from the original residuals,
  #and the optimal bandwidth matrix
  hbw_dt[, 
         c('logbw_resid',
           'logh_resid'):=as.data.frame(hw_kde[[1]]$x[sample(unique(nkde),
                                                                    size=length(id),
                                                                    prob=hw_kde[[1]]$w), ] +
                                               mvtnorm::rmvnorm(n=length(id),
                                                                mean=c(0,0),
                                                                sigma=hw_kde[[1]]$H)),
         by=list(gender, reth)]
  
  hbw_dt[, weight:=exp(mean_logbw+logbw_resid)]
  hbw_dt[, height:=exp(mean_logh+logh_resid)]
  
  #Remove the temporary columns
  hbw_dt[, id:=NULL]
  hbw_dt[, hw_kde:=NULL]
  hbw_dt[, nkde:=NULL]
  return(hbw_dt)
}
