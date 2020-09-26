#'Given vectors of age, BMI, recumbent length, weight, and gender,
#'categorizes weight classes using CDC and WHO categories.
#'
#'@export
#'
#'@param age_years A vector of
#'  ages in years.
#'@param age_months A vector of ages in months.
#'@param bmi A vector of BMIs.
#'@param recumlen A vector of heights or recumbent lengths in cm.
#'@param weight A vector of body weights in kg.
#'@param gender A vector of genders (as 'Male' or 'Female').
#'  
#'@return A character vector of weight classes. Each element will be one of
#'  'Underweight', 'Normal', 'Overweight', or 'Obese'.
#'
#'@keywords httk-pop
#'
#'@author Caroline Ring
#'
#'@references Ring, Caroline L., et al. "Identifying populations sensitive to 
#'environmental chemicals by simulating toxicokinetic variability." Environment 
#'International 106 (2017): 105-118
#' @import stats
#' @export get_weight_class

get_weight_class <- function(age_years,
                             age_months,
                             bmi,
                             recumlen,
                             weight,
                             gender){
  
  #R CMD CHECK throws notes about "no visible binding for global variable", for
  #each time a data.table column name is used without quotes. To appease R CMD
  #CHECK, a variable has to be created for each of these column names and set to
  #NULL. Note that within the data.table, these variables will not be NULL! Yes,
  #this is pointless and annoying.
  P5 <- P85 <- P95 <- P2.3 <- P97.7 <- NULL
  Sex <- Length <- NULL
  #End R CMD CHECK appeasement.
  
  # For adults, these are the BMI ranges:
  # #BMI < 18.5 = underweight
  # #18.5 < BMI < 24.9 = Normal
  # #25.0 < BMI < 29.9 = Overweight
  # #BMI > 30 = Obese
  
  weight_class <- rep(NA, length(gender))
  
  weight_class[age_years>18] <- as.character(cut(bmi[age_years>18], 
                                   breaks=c(0,18.5,25.0,30,Inf),
                                   labels=c('Underweight',
                                            'Normal',
                                            'Overweight',
                                            'Obese')))
  
  #if (any(age_years>=2 & age_years<=18])){
    #For children ages 2 to 18, 
    #weight class depends on the BMI-for-age percentile.
    #<5th percentile = underweight
    #5-85th percentiles = normal weight
    #85-95th percentile = overweight
    #>=95th percentile = obese

    weight_class[age_years>=2 &
                   age_years<=18 &
                   bmi < bmiage[list(gender, 
                                  age_months),
                                P5]] <- 'Underweight'
    weight_class[age_years>=2 &
                   age_years<=18 &
                   bmi>= bmiage[list(gender, 
                                  age_months),
                                P5] &
                   bmi < bmiage[list(gender, 
                                  age_months),
                                P85]] <- 'Normal'
    weight_class[age_years>=2 &
                   age_years<=18 &
                   bmi>= bmiage[list(gender, 
                                  age_months),
                                P85] &
                   bmi <= bmiage[list(gender, 
                                  age_months),
                                P95]] <- 'Overweight'
    weight_class[age_years>=2 &
                   age_years<=18 &
                   bmi > bmiage[list(gender, 
                                  age_months),
                                P95]] <- 'Obese'
  #}
  

    #BMI is effectively meaningless for infants under 2 years old.
    #Instead, use WHO weight-for-length charts.
    #Ogden et al. 2010 (https://jama.jamanetwork.com/article.aspx?articleid=185233)
    #use 95th percentile as their only cut point for infants.
    #so lump 'Overweight' and 'Obese' together and just use 97.7th percentile.
  
  #Inititalize weight for recumlen percentile vectors
  wfl_P2.3 <- rep(NA, length(weight_class))
  wfl_P97.7 <- wfl_P2.3
  
  for (g in c('Male', 'Female')){
    if (any(age_years<2 & gender==g)){
      #Interpolate values for 2.3rd and 97.7th percentile
    wfl_P2.3[gender==g &
               age_years<2] <- spline(x=wfl[Sex==g,
                                            Length],
                                      y=wfl[Sex==g,
                                            P2.3],
                                      xout=recumlen[gender==g &
                                                    age_years<2])$y
    wfl_P97.7[gender==g &
               age_years<2] <- spline(x=wfl[Sex==g,
                                            Length],
                                      y=wfl[Sex==g,
                                            P97.7],
                                      xout=recumlen[gender==g &
                                                    age_years<2])$y
  }
  }
  
  weight_class[age_years<2 &
                 weight<wfl_P2.3] <- 'Underweight'
  weight_class[age_years<2 &
                 weight>=wfl_P2.3 &
                 weight<wfl_P97.7] <- 'Normal'
  weight_class[age_years<2 &
                 weight>=wfl_P97.7] <- 'Obese'
    
  return(weight_class)
}
