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
gen_serum_creatinine <- function(serumcreat.dt){
  #R CMD CHECK throws notes about "no visible binding for global variable", for
  #each time a data. table column name is used without quotes. To appease R CMD
  #CHECK, a variable has to be created for each of these column names and set to
  #NULL. Note that within the data.table, these variables will not be NULL! Yes,
  #this is pointless and annoying.
  id <- age_years <- log_serum_creat <- sc_spline <- age_months <- NULL
  sc_kde <- serum_creat <- gfr_est <- gender <- reth <- BSA_adj <- NULL
  #End of R CMD CHECK appeasement.
  
  serumcreat.dt <- data.table::copy(serumcreat.dt) #to avoid altering the original item
  #Take a list of sample individuals with gender, race, age_years, height, BSA in cm^2
  #Draw their serum creatinine levels from the appropriate KDE distribution
  serumcreat.dt[, id:=1:nrow(serumcreat.dt)]
  
  serumcreat.tmp <- merge(serumcreat.dt, 
                      spline_serumcreat,
                      by=c('gender', 'reth'))
  
  if (serumcreat.tmp[, any(age_years>=12)]){
    serumcreat.tmp[age_years>=12, 
               log_serum_creat:=predict(sc_spline[[1]], #spline
                                        x=age_months)$y + 
                 rfun(n=sum(age_years>=12), #residual variability KDE
                      fhat=sc_kde[[1]]),
               by=list(gender, reth)] #Note: separate spline for each gender and reth
    
    serumcreat.dt <- merge(serumcreat.dt,
                       serumcreat.tmp[, list(id, log_serum_creat)],
                       by='id')
    serumcreat.dt[, id:=NULL]
    
    serumcreat.dt[age_years>=12, 
              serum_creat:=exp(log_serum_creat)]
  }else{ #if no one over age 12, don't assign any serum creatinine values
    serumcreat.dt[age_years<12, 
                  serum_creat:=NA_real_]
  }
  
  return(serumcreat.dt)
}