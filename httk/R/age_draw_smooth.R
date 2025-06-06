#' Draws ages from a smoothed distribution for a given gender/race combination
#' 
#' This function should usually not be called directly by the user. It is used by
#'\code{httkpop_generate()} in "virtual-individuals" mode.
#' 
#' @param gender Gender. Either 'Male' or 'Female'.
#' @param reth Race/ethnicity. One of 'Mexican American', 'Other Hispanic',
#' 'Non-Hispanic Black', 'Non-Hispanic White', 'Other'.
#' @param nsamp Number of ages to draw.
#' @param agelim_months Two-element numeric vector giving the minimum and
#' maximum ages in months to include.
#' @param nhanes_mec_svy \code{surveydesign} object created from
#'  \code{\link{mecdt}} using \code{\link[survey]{svydesign}} (this is done in
#'  \code{\link{httkpop_generate}})
#' @return A named list with members 'ages_months' and 'ages_years', each
#' numeric of length \code{nsamp}, giving the sampled ages in months and years.
#' @author Caroline Ring
#' @references 
#' \insertRef{ring2017identifying}{httk} 
#' @keywords httk-pop
#' @import stats
#' @export age_draw_smooth
age_draw_smooth <- function(gender, reth, nsamp, agelim_months, nhanes_mec_svy){
  #R CMD CHECK throws notes about "no visible binding for global variable", for
  #each time a data.table column name is used without quotes. To appease R CMD
  #CHECK, a variable has to be created for each of these column names and set to
  #NULL. Note that within the data.table, these variables will not be NULL! Yes,
  #this is pointless and annoying.
  riagendr <- ridreth1 <- NULL
  #End R CMD CHECK appeasement.
  
  nhanes_sub <- subset(nhanes_mec_svy,
                       riagendr %in% gender &
                         ridreth1 %in% reth)
  age_smooth <- svysmooth(~ridexagm,
                          nhanes_sub,
                          ngrid = 1024)$ridexagm
  indiv_ages_m <- rep(-99, nsamp)
  
  badind <- which(indiv_ages_m<min(agelim_months) |
                    indiv_ages_m>max(agelim_months))
  n <- length(badind)
  
  while(n>0){
    
    age_dist <- approx(age_smooth$x,
                       age_smooth$y,
                       xout=0:959,
                       rule=2,
                       method='linear')$y
    
      indiv_ages_m[badind] <- sample(x=0:959,
                         size=n,
                         replace=TRUE,
                         prob=age_dist)
      #update badind
      badind <- which(indiv_ages_m<min(agelim_months) |
                        indiv_ages_m>max(agelim_months))
      n <- length(badind)
  }
  
  indiv_ages_y <- floor(indiv_ages_m/12) #convert ages in months to years
  return(list(ages_months=indiv_ages_m,
              ages_years=indiv_ages_y))
}
