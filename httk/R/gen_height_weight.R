#'Generate heights and weights for a virtual population.
#'
#'Predict height and weight from age using smoothing splines, and then add
#'residual variability from a 2-D KDE, both fitted to NHANES data, for a given
#'combination of gender and NHANES race/ethnicity category.
#'
#'This function should usually not be called directly by the user. It is used by
#'\code{httkpop_generate()} in "virtual-individuals" mode, after drawing gender,
#'NHANES race/ethnicity category, and age from their NHANES
#'proportions/distributions.
#'
#'@param gender Gender for which to calculate height/weight ("Male" or "Female")
#'@param reth NHANES race/ethnicity category for which to calculate
#'  height/weight ("Mexican American", "Non-Hispanic Black", "Non-Hispanic
#'  White", "Other", or "Other Hispanic")
#'@param age_months vector of ages in months for individuals for whom to
#'  calculate height/weight (between 0-959 months)
#'@return A list containing two named elements, \code{weight} and \code{height},
#'  each of which is a numeric vector. \code{weight} gives individual body
#'  weights in kg, and \code{height} gives individual heights in cm,
#'  corresponding to each item in the input \code{age_months}.
#'
#'@author Caroline Ring
#'
#'@references Ring, Caroline L., et al. "Identifying populations sensitive to
#'  environmental chemicals by simulating toxicokinetic variability."
#'  Environment International 106 (2017): 105-118
#'
#'@keywords httk-pop
#'
#'@import stats
#'@importFrom mvtnorm rmvnorm
#'  
gen_height_weight <- function(gender,
                              reth,
                              age_months,
                              nhanes_mec_svy){
  
  #R CMD CHECK throws notes about "no visible binding for global variable", for
  #each time a data.table column name is used without quotes. To appease R CMD
  #CHECK, a variable has to be created for each of these column names and set to
  #NULL. Note that within the data.table, these variables will not be NULL! Yes,
  #this is pointless and annoying.
  g <- r <- seqn <- logwresid <- loghresid <- wtmec6yr <- NULL
  #End R CMD CHECK appeasement.
  
   n <- length(age_months)
   grname <- unique(paste(gender, reth))
   
  #Get "mean" heights (cm) and bodyweights (kg) for each individual
  #Subset dt by gender and race/ethnicity,
  #then predict mean heights and BWs from ages
  #using the spline for that gender/race combination
  
  #Draw log BW and log height residuals from the 2-d KDE
  #calculate NHANES residuals
  nhanes_sub <- nhanes_mec_svy$variables[riagendr %in% gender &
                                           ridreth1 %in% reth &
                                           is.finite(bmxwt) &
                                           is.finite(bmxhtlenavg),
                                         .(ridexagm, bmxwt, bmxhtlenavg, wtmec6yr)]
  
  w <- nhanes_sub[, wtmec6yr/sum(wtmec6yr)]
  #fit smoothing spline
 height_spline <- smooth.spline(x = nhanes_sub$ridexagm,
                             y = log(nhanes_sub$bmxhtlenavg),
                             w = w)
 weight_spline <- smooth.spline(x = nhanes_sub$ridexagm,
                                y = log(nhanes_sub$bmxwt),
                                w = w)
 
  logw_resid <- resid(weight_spline)
  logh_resid <- resid(height_spline)
  centers <- cbind(logw_resid, logh_resid)
  
  
  #sample from centers
  centers_id_samp <- sample(x = nrow(centers),
                         size = n,
                         replace = TRUE,
                         prob = w)
  centers_samp <- centers[centers_id_samp, ]
  #handle the case where n=1
  if(n==1){
    centers_samp <- matrix(centers_samp,
                           nrow = 1,
                           ncol = 2)
  }
  
  #get optimal bandwidth
  #H <- ks::Hpi(x = centers)
  H <- hw_H[[grname]]

  
  resids_samp <- t(apply(centers_samp,
                  1,
                  function(this_center) rmvnorm(n = 1,
                    mean = this_center,
                    sigma = H)
                  )
                  )
  
  mean_logbw <- predict(weight_spline,
                        age_months)$y
  mean_logh <- predict(height_spline,
                       age_months)$y
  
   weight <- exp(mean_logbw + resids_samp[,1])
   height <- exp(mean_logh + resids_samp[,2])

  return(list(weight=weight,
              height = height))
}
