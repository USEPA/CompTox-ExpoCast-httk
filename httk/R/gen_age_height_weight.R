#' Generate demographic parameters for a virtual population
#'
#' Generate gender, NHANES race/ethnicity category, ages, heights, and weights
#'for a virtual population, based on NHANES data.
#'
#' This function should usually not be called directly by the user. It is used by
#'\code{httkpop_generate()} in "virtual-individuals" mode.
#'
#'@param nsamp The desired number of individuals in the virtual population.
#'  \code{nsamp} need not be provided if \code{gendernum} is provided.
#'@param gendernum Optional: A named list giving the numbers of male and female
#'  individuals to include in the population, e.g. \code{list(Male=100,
#'  Female=100)}. Default is NULL, meaning both males and females are included,
#'  in their proportions in the NHANES data. If both \code{nsamp} and
#'  \code{gendernum} are provided, they must agree (i.e., \code{nsamp} must be
#'  the sum of \code{gendernum}).
#'@param agelim_years Optional: A two-element numeric vector giving the minimum
#'  and maximum ages (in years) to include in the population. Default is
#'  c(0,79). If \code{agelim_years} is provided and \code{agelim_months} is not,
#'  \code{agelim_years} will override the default value of \code{agelim_months}.
#'@param agelim_months Optional: A two-element numeric vector giving the minimum
#'  and maximum ages (in months) to include in the population. Default is c(0,
#'  959), equivalent to the default \code{agelim_years}. If \code{agelim_months}
#'  is provided and \code{agelim_years} is not, agelim_months will override the
#'  default values of \code{agelim_years}.
#'@param reths Optional: a character vector giving the races/ethnicities to
#'  include in the population. Default is \code{c('Mexican American','Other
#'  Hispanic','Non-Hispanic White','Non-Hispanic Black','Other')}, to include
#'  all races and ethnicities in their proportions in the NHANES data.
#'  User-supplied vector must contain one or more of these strings.
#'@param weight_category Optional: The weight categories to include in the
#'  population. Default is \code{c('Underweight', 'Normal', 'Overweight',
#'  'Obese')}. User-supplied vector must contain one or more of these strings.
#'
#'@return A data.table containing variables \describe{
#'  \item{\code{gender}}{Gender of each virtual individual}
#'  \item{\code{reth}}{Race/ethnicity of each virtual individual}
#'  \item{\code{age_months}}{Age in months of each virtual individual}
#'  \item{\code{age_years}}{Age in years of each virtual individual}
#'  \item{\code{weight}}{Body weight in kg of each virtual individual}
#'  \item{\code{height}}{Height in cm of each virtual individual} }
#'
#'
#'@keywords httk-pop
#'
#'@author Caroline Ring
#'
#'@references Ring, Caroline L., et al. "Identifying populations sensitive to
#'  environmental chemicals by simulating toxicokinetic variability."
#'  Environment International 106 (2017): 105-118
#'
#'  importFrom survey svymean
#'
#'@export gen_age_height_weight

gen_age_height_weight <- function(nsamp=NULL, 
                                  gendernum=NULL, 
                                  reths, 
                                  weight_category,
                                  agelim_years,
                                  agelim_months){
  
  #R CMD CHECK throws notes about "no visible binding for global variable", for 
  #each time a data.table column name is used without quotes. To appease R CMD 
  #CHECK, a variable has to be created for each of these column names and set to
  #NULL. Note that within the data.table, these variables will not be NULL! Yes,
  #this is pointless and annoying.
  ridexagm <- ridreth1 <- weight_class <- bmxhtlenavg <- bmxwt <- NULL
  ridexagy <-  lbxhct <- lbxscr <- riagendr <- gender <-  reth <- NULL
  ctcol <- bmi <- weight <- height <- age_years <- NULL
  age_months <-  mean_logh <- spline_heightweight <- g <- r <- NULL
  height_spline <- mean_logbw <- weight_spline <- hw_kde <- NULL
  nkde <- id <- logbw_resid_mean <- logh_resid_mean <- NULL
  logbw_resid <-   logh_resid <- NULL
  #End R CMD CHECK appeasement
  
  if (is.null(gendernum)){
    #No gender specified, so include both,
    #at their proportions in the population
    #Take the relevant user-specified subset of the data
    exam_svy_sub <- subset(nhanes_mec_svy,
      ridexagm>=min(agelim_months) & 
        ridexagm<max(agelim_months) &
        ridreth1 %in% reths &
        weight_class %in% weight_category &
        is.finite(bmxhtlenavg) &
        is.finite(bmxwt) &
        (ridexagy<1 |
           (ridexagy>=1 & !is.na(lbxhct))
        ) &
        (ridexagy<12 |
           (ridexagy>=12 & !is.na(lbxscr))
        )
    )
    
    #Get the race and sex proportions for the user-specified subset
    #This takes care of conditional race/sex proportions
    reth.sex.prop <- survey::svymean(~interaction(ridreth1,
                                                  riagendr), 
                                     exam_svy_sub, 
                                     na.rm=TRUE)
    #Sample from these race and sex proportions
    reth.sex.ind <- sample(x=seq_along(reth.sex.prop),
                           size=nsamp,
                           replace=TRUE,
                           prob=reth.sex.prop)
    #This gives a vector of numbers representing
    #race/sex combinations as listed in reth.sex.prop
    
    #convert numbers into race/sex combination strings
    reth.sex.names <- names(reth.sex.prop)[reth.sex.ind]
    #split strings into actual race/sex values
    reth.sex.names.sub <- gsub(reth.sex.names, 
                               pattern='interaction\\(ridreth1, riagendr\\)([^\\.]+)\\.([^\\.]+)', 
                               replacement='\\1.\\2', 
                               perl=TRUE)
    reth.sex.names.split <- strsplit(reth.sex.names.sub,
                                     split='.', 
                                     fixed=TRUE)
    race <- sapply(reth.sex.names.split, 
                   function(x) x[1])
    sex <- sapply(reth.sex.names.split, 
                  function(x) x[2])
    #Create the data table characterizing each individual by race and sex
    gen_dt <- data.table(reth=race,
                         gender=sex)
  } else{ #if gender numbers were specified, use those
    #Create the data table characterizing each individual by gender first
    gen_dt <- data.table(gender=c(rep('Male', 
                                      gendernum$Male),
                                  rep('Female', 
                                      gendernum$Female)))
    
    #Now assign race
    #First take subsets of NHANES exam population by sex and
    #by age range
    if(gendernum$Male>0){
      exam_svy_sub_male <- subset(nhanes_mec_svy,
        ridexagm>=min(agelim_months) & 
          ridexagm<max(agelim_months) &
          ridreth1 %in% reths &
          weight_class %in% weight_category &
          riagendr=='Male' &
          is.finite(bmxhtlenavg) &
          is.finite(bmxwt) &
          (ridexagy<1 |
             (ridexagy>=1 & !is.na(lbxhct))
          ) &
          (ridexagy<12 |
             (ridexagy>=12 & !is.na(lbxscr))
          )
      )
      
      reth.prop_male <- survey::svymean(~ridreth1, 
                                        exam_svy_sub_male,
                                        na.rm=TRUE)
      
      #Sample from these race and sex proportions
      reth.ind_male <- sample(x=seq_along(reth.prop_male),
                              size=gendernum$Male,
                              replace=TRUE,
                              prob=reth.prop_male)
      
      #convert numbers into race strings
      reth.names_male <- names(reth.prop_male)[reth.ind_male]
      
      reth_male <- gsub(x=reth.names_male, 
                        pattern='ridreth1',
                        replacement='')
      
      #Assign to individuals in data table
      gen_dt[gender=='Male', reth:=reth_male]
    }
    if (gendernum$Female>0){
      exam_svy_sub_female <- subset(nhanes_mec_svy,
        ridexagm>=min(agelim_months) & 
          ridexagm<max(agelim_months) &
          ridreth1 %in% reths &
          riagendr == 'Female' &
          weight_class %in% weight_category &
          is.finite(bmxhtlenavg) &
          is.finite(bmxwt) &
          (ridexagy<1 |
             (ridexagy>=1 & !is.na(lbxhct))
          ) &
          (ridexagy<12 |
             (ridexagy>=12 & !is.na(lbxscr))
          )
      )
      reth.prop_female <- survey::svymean(~ridreth1, 
                                          exam_svy_sub_female,
                                          na.rm=TRUE)
      
      reth.ind_female <- sample(x=seq_along(reth.prop_female),
                                size=gendernum$Female,
                                replace=TRUE,
                                prob=reth.prop_female)
      reth.names_female <- names(reth.prop_female)[reth.ind_female]
      reth_female <- gsub(x=reth.names_female, 
                          pattern='ridreth1',
                          replacement='')
      gen_dt[gender=='Female', reth:=reth_female]
    }
  }
  #Generate ages
  
  #Draw ages from smoothed age distributions by gender and race.
  #Discard any that fall outside the user-specified age range.
  gen_dt[, ctcol:=1] #Add tmp variable to count items in each group
  gen_dt[, c('age_months','age_years'):= age_draw_smooth(gender=gender, 
                                                         reth=reth, 
                                                         nsamp=.N, 
                                                         agelim_months=agelim_months), 
         by=list(gender,reth)]
  
  #Draw height and body weight
  gen_dt[, c("weight",
             "height"):=gen_height_weight(gender = gender,
                                          reth = reth,
                                          age_months = age_months),
         by = list(gender, reth)]
  
  if (length(setdiff(c('Underweight', 
                       'Normal',
                       'Overweight',
                       'Obese'), weight_category))>0){  #If weight class needs to be checked
    gen_dt[, bmi:=weight/((height/100)^2)]
    gen_dt[, weight_class:=get_weight_class(age_years=age_years,
                                           age_months=age_months,
                                            bmi=bmi,
                                            gender=gender,
                                            recumlen=height,
                                            weight=weight)]
    #Rejection sampling for weight class
    rct <- 1
    while(nrow(gen_dt[!(weight_class %in% weight_category),])>0){
      #If not in the right weight class, then redraw age, height, and body weight
      #redraw age for those not in selected weight classes
      gen_dt[!(weight_class %in% 
                 weight_category),
             c('age_months','age_years'):= age_draw_smooth(gender=gender, 
                                                           reth=reth, 
                                                           nsamp=.N, 
                                                           agelim_months=agelim_months), 
             by=list(gender,reth)] #Now age will be updated, but weight_category isn't yet, so we can still use it
    
      #redraw height & weight for those not in selected weight classes
      gen_dt[!(weight_class %in% weight_category),
             c("weight",
               "height"):=gen_height_weight(gender = gender,
                                            reth = reth,
                                            age_months = age_months),
             by = list(gender, reth)]
    
      #recalc BMI
      gen_dt[, bmi:=weight/((height/100)^2)]
      
      #recalc weight class
      gen_dt[, weight_class:=get_weight_class(age_years=age_years,
                                              age_months=age_months,
                                              bmi=bmi,
                                              gender=gender,
                                              recumlen=height,
                                              weight=weight)] #get new weight classes based on new ages, heights, weights
    rct <- rct+1
    }
  }
  #Now gen_dt should have individual genders, race/ethnicity, age, height, and weight.
  return(gen_dt[, list(gender, 
                    reth, 
                    age_months, 
                    age_years, 
                    weight, 
                    height)])
}
