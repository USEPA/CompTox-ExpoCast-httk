#'Categorize kidney function by GFR.
#'
#'For adults: 
#'In general GFR > 60 is considered normal 
#'15 < GFR < 60 is considered kidney disease 
#'GFR < 15 is considered kidney failure 
#'
#'These values can also be used for children 2 years old and greater (see PEDIATRICS IN
#'REVIEW Vol. 29 No. 10 October 1, 2008 pp. 335-341 (doi:
#'10.1542/pir.29-10-335))
#'
#'@param age_years Vector of ages in years.
#'@param age_months Vector of ages in months.
#'@param gfr_est Vector of estimated GFR values in mL/min/1.73m^2.
#'  
#'@return Vector of GFR categories: 'Normal', 'Kidney Disease', 'Kidney
#'  Failure'.
#'
#'@keywords httk-pop
#'
#'@author Caroline Ring
#'
#'@references 
#' \insertRef{ring2017identifying}{httk} 
#' @export get_gfr_category

get_gfr_category <- function(age_years,
                             age_months,
                             gfr_est){
  #For adults:
  # #In general GFR > 60 is considered normal
  # #15 < GFR < 60 is considered kidney disease
  # #GFR < 15 is considered kidney failure
  
  gfrcat <- rep(NA, length(age_years))
  gfrcat[(age_years>=2 & gfr_est>=60) |
                  (age_months>6 & 
                     age_months<24 &
                     gfr_est>=50) |
                  (age_months>1 &
                     age_months<=6 
                   & gfr_est>=40) |
                  (age_months<=1 & gfr_est>=25)] <- 'Normal'
  gfrcat[gfr_est>15 & ((age_years>=2 & gfr_est<60) |
                  (age_months>6 & 
                     age_months<24 &
                     gfr_est<50) |
                  (age_months>1 &
                     age_months<=6 
                   & gfr_est<40) |
                  (age_months<=1 & gfr_est<25))] <- 'Kidney Disease'
  gfrcat[gfr_est<=15] <- 'Kidney Failure'
  
  return(gfrcat)
}
