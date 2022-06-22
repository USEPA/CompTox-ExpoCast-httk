#' Predict hematocrit in infants under 1 year old.
#' 
#' For infants under 1 year, hematocrit was not measured in NHANES. Assume a
#' log-normal distribution where plus/minus 1 standard deviation of the
#' underlying normal distribution is given by the reference range. Draw
#' hematocrit values from these distributions by age.
#' 
#' \tabular{cc}{ 
#' Age \tab Reference range\cr 
#' <1 month \tab 31-49\cr 
#' 1-6 months \tab 29-42\cr 
#' 7-12 months \tab 33-38 
#' }
#' 
#' @param age_months Vector of ages in months; all must be <= 12.
#'   
#' @return Vector of hematocrit percentages corresponding to the input vector
#'   of ages.
#'
#'@keywords httk-pop
#'
#'@author Caroline Ring
#'
#'@references Ring, Caroline L., et al. "Identifying populations sensitive to 
#'environmental chemicals by simulating toxicokinetic variability." Environment 
#'International 106 (2017): 105-118
#' @import stats

hematocrit_infants <- function(age_months){

#For infants under 1 year,
#hematocrit was not measured in NHANES.
#Assume a log-normal distribution where plus/minus 1 standard deviation
#of the underlying normal distribution is given by the reference range
  #The 1-sd range is based on the fact that for 1year olds,
  #the NHANES data shows approximately plus and minus 1 sd
  #within 33-38% hematocrit, which is the reference range for that age.
  
  loghct <- rep(NA, length(age_months))
  
  if (length(loghct[age_months<1])>0){
  #For infants under 1 month old
  #reference range 31-49
  sig.tmp <- (log(49)-log(31))/2
  mu.tmp <- log(49) - sig.tmp
  loghct[age_months<1] <- rnorm(n=length(loghct[age_months<1]),
                                mean=mu.tmp,
                                sd=sig.tmp)
  }
  
  if (length(loghct[age_months>=1 &
                      age_months<=6])>0){
  #For infants 1-6 months old
  sig.tmp <- (log(42)-log(29))/2
  mu.tmp <- log(42) - sig.tmp
  loghct[age_months>=1 &
        age_months<=6] <- rnorm(n=length(loghct[age_months>=1 &
                                               age_months<=6]),
                               mean=mu.tmp,
                               sd=sig.tmp)
  }
  
  if (length(loghct[age_months>6 &
                      age_months<12])>0){
  #For infants 7-12 months old
  sig.tmp <- (log(38)-log(33))/2
  mu.tmp <- log(38) - sig.tmp
  loghct[age_months>6 &
        age_months<12] <- rnorm(n=length(loghct[age_months>6 &
                                               age_months<12]),
                                mean=mu.tmp,
                                sd=sig.tmp)
  }
  
  hct <- exp(loghct)
  
  return(hct)
}

