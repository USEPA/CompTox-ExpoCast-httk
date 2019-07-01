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

estimate_hematocrit <- function(hcttmp_dt){
  
  #R CMD CHECK throws notes about "no visible binding for global variable", for
  #each time a data. table column name is used without quotes. To appease R CMD
  #CHECK, a variable has to be created for each of these column names and set to
  #NULL. Note that within the data.table, these variables will not be NULL! Yes,
  #this is pointless and annoying.
  id <- age_years <- log_hematocrit <- hct_spline <- age_months <- NULL
  hct_kde <- hematocrit <- gender <- reth <- NULL
  #End R CMD CHECK appeasement.
  
  hcttmp_dt <- data.table::copy(hcttmp_dt) #to avoid altering original object
  
  hcttmp_dt[, id:=1:nrow(hcttmp_dt)]
  
  hcttmp_tmp <- merge(hcttmp_dt, 
                      spline_hematocrit,
                      by=c('gender', 'reth'))
  
  if (hcttmp_tmp[, any(age_years>=1)]){
    hcttmp_tmp[age_years>=1, 
               log_hematocrit:=predict(hct_spline[[1]], #Hct value predicted from age...
                                       x=age_months)$y + 
                 rfun(n=sum(age_years>=1), #...plus residual predicted from KDE of NHANES resids
                      fhat=hct_kde[[1]]),
               by=list(gender, reth)]
  
  hcttmp_dt <- merge(hcttmp_dt,
                     hcttmp_tmp[, list(id, log_hematocrit)],
                     by='id')
  hcttmp_dt[, id:=NULL]
  
  hcttmp_dt[age_years>=1, hematocrit:=exp(log_hematocrit)]
}

#For infants under 1 year, sample hematocrit from log-normal distributions
#based on reference ranges by age
if (nrow(hcttmp_dt[age_years<1,])>0) {
  hcttmp_dt[age_years<1,
            hematocrit:=hematocrit_infants(age_months=age_months)]
}

hcttmp_dt[, log_hematocrit:=NULL] #Remove the temp log hematocrit variable

return(hcttmp_dt)
}
