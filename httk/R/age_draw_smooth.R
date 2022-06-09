#' Draws ages from a smoothed distribution for a given gender/race combination
#' 
#' Draws ages from a smoothed distribution for a given gender/race combination
#' 
#' 
#' @param g Gender. Either 'Male' or 'Female'.
#' @param r Race/ethnicity. One of 'Mexican American', 'Other Hispanic',
#' 'Non-Hispanic Black', 'Non-Hispanic White', 'Other'.
#' @param nsamp Number of ages to draw.
#' @param agelim_months Two-element numeric vector giving the minimum and
#' maximum ages in months to include.
#' @return A named list with members 'ages_months' and 'ages_years', each
#' numeric of length \code{nsamp}, giving the sampled ages in months and years.
#' @author Caroline Ring
#' @references Ring, Caroline L., et al. "Identifying populations sensitive to
#' environmental chemicals by simulating toxicokinetic variability."
#' Environment International 106 (2017): 105-118
#' @keywords httk-pop
#' @import stats
#' @export age_draw_smooth
age_draw_smooth <- function(g, r, nsamp, agelim_months){
  #R CMD CHECK throws notes about "no visible binding for global variable", for 
  #each time a data.table column name is used without quotes. To appease R CMD 
  #CHECK, a variable has to be created for each of these column names and set to
  #NULL. Note that within the data.table, these variables will not be NULL! Yes,
  #this is pointless and annoying.
  gender <- reth <- smth <- NULL
  physiology.data <- physiology.data
  #End R CMD CHECK appeasement.

  indiv_ages_m <- vector()
  indiv_ages_y <- vector()
  while(length(indiv_ages_m)<nsamp){
    smooth_x <- DT_age[g==g & r==r,
                           x]
    smooth_y <- DT_age[g==g & r==r,
                       y]
    age_dist <- approx(smooth_x,
                       smooth_y,
                       xout=0:959,
                       rule=2,
                       method='linear')$y
    
      tmp.m <- sample(x=0:959,
                         size=nsamp,
                         replace=TRUE,
                         prob=age_dist)
      
    indiv_ages_m <- c(indiv_ages_m, 
                    tmp.m[tmp.m>=min(agelim_months) &
                            tmp.m<max(agelim_months)])
  }
  #We may have overshot, so just take the first nsamp
  #Round them to whole numbers
  indiv_ages_m <- round(indiv_ages_m[1:nsamp])
  indiv_ages_y <- floor(indiv_ages_m/12) #convert ages in months to years
#indiv_ages_y <- indiv_ages_m/12
  return(list(ages_months=indiv_ages_m,
              ages_years=indiv_ages_y))
}
