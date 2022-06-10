#'Inner loop function called by \code{httkpop_direct_resample}.
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
#' @param gfr_resid_var Logical value indicating whether or not to include
#' residual variability when generating GFR values. (Default is TRUE, passed from
#' 'httkpop_direct_resample'.)
#' @param ckd_epi_race_coeff Logical value indicating whether or not to use the
#' "race coefficient" from the CKD-EPI equation when estimating GFR values.
#' (Default is FALSE, passed from 'httkpop_direct_resample'.)
#' @param nhanes_mec_svy \code{surveydesign} object created from
#'  \code{\link{mecdt}} using \code{\link[survey]{svydesign}} (this is done in
#'  \code{\link{httkpop_generate}})
#'
#'@return A data.table where each row represents an individual, and
#'  each column represents a demographic, anthropometric, or physiological
#'  parameter.
#'
#'@keywords httk-pop monte-carlo
#'
#'@author Caroline Ring
#'
#'@references Ring, Caroline L., et al. "Identifying populations sensitive to 
#'environmental chemicals by simulating toxicokinetic variability." Environment 
#'International 106 (2017): 105-118
#'
#'@import survey
#'
#'@export httkpop_direct_resample_inner
httkpop_direct_resample_inner <- function(nsamp,
                                          gendernum,
                                          agelim_months,
                                          agelim_years,
                                          reths,
                                          weight_category,
                                          gfr_resid_var,
                                          ckd_epi_race_coeff,
                                          nhanes_mec_svy){
  
  #R CMD CHECK throws notes about "no visible binding for global variable", for
  #each time a data.table column name is used without quotes. To appease R CMD
  #CHECK, a variable has to be created for each of these column names and set to
  #NULL. Note that within the data.table, these variables will not be NULL! Yes,
  #this is pointless and annoying.
  ridexagm <- ridreth1 <- weight_class <- bmxhtlenavg <- bmxwt <- NULL
  ridexagy <- lbxhct <- lbxscr <- wtmec6yr <- seqn <- riagendr <- NULL
  age_years <- gfr_est <- serum_creat <- reth <- BSA_adj <- hematocrit <- NULL
  age_months <- gender <- NULL
  #End R CMD CHECK appeasement.
  
  if (is.null(gendernum)){
    #No gender specified, so include both,
    #at their proportions in the population
    #Take the relevant user-specified subset of the data  
    #First, take the user-specified subset of the NHANES data:
    #specified age limits, genders, and race/ethnicities.
    #Also specify that height and weight must be non-NA.
    #Also specify that serum creatinine and hematocrit must be non-NA,
    #if they were measured.
    mec_svy_sub <- subset(nhanes_mec_svy,
                          ridexagm>=min(agelim_months) &
                            ridexagm<max(agelim_months) &
                            ridreth1 %in% reths &
                            weight_class %in% 
                            weight_category &
                            is.finite(bmxhtlenavg) &
                            is.finite(bmxwt) &
                            (ridexagy<1 |
                               (ridexagy>=1 & 
                                  !is.na(lbxhct))
                            ) &
                            (ridexagy<12 |
                               (ridexagy>=12 & 
                                  !is.na(lbxscr))
                            )
    )
    
    #Now, sample (with replacement) a population of nsamp people
    inner_dt <- mec_svy_sub$variables[sample(x=1:nrow(mec_svy_sub$variables),
                                             size=nsamp,
                                             replace=TRUE,
                                             prob=mec_svy_sub$variables[,
                                                                        wtmec6yr]), #normalize to sum to nrows
                                      list(seqn,
                                        riagendr,
                                        ridreth1,
                                        ridexagy,
                                        ridexagm,
                                        bmxhtlenavg,
                                        bmxwt,
                                        lbxscr,
                                        lbxhct)]
    setnames(inner_dt, 
             names(inner_dt),
             c('seqn',
               'gender',
               'reth',
               'age_years',
               'age_months',
               'height',
               'weight',
               'serum_creat',
               'hematocrit'))
  } else{ #if gendernum was specified, 
    #do separately for males and females
    if (gendernum$Male>0){
      mec_svy_sub_male <- subset(nhanes_mec_svy,
                                 ridexagm>=min(agelim_months) & 
                                   ridexagm<max(agelim_months) &
                                   ridreth1 %in% reths &
                                   riagendr == 'Male' &
                                   weight_class %in%
                                   weight_category &
                                   is.finite(bmxhtlenavg) &
                                   is.finite(bmxwt) &
                                   (ridexagy<1 |
                                      (ridexagy>=1 & 
                                         !is.na(lbxhct))
                                   ) &
                                   (ridexagy<12 |
                                      (ridexagy>=12 & 
                                         !is.na(lbxscr))
                                   )
      )
      
      #Now, sample (with replacement) a population of gendernum$Male males
      inner_dt_male <- mec_svy_sub_male$variables[sample(x=seq_along(mec_svy_sub_male$prob),
                                                         size=gendernum$Male,
                                                         replace=TRUE,
                                                         prob=mec_svy_sub_male$variables[,
                                                                                         wtmec6yr]),
                                                  list(seqn,
                                                    riagendr,
                                                    ridreth1,
                                                    ridexagy,
                                                    ridexagm,
                                                    bmxhtlenavg,
                                                    bmxwt,
                                                    lbxscr,
                                                    lbxhct)]
      data.table::setnames(inner_dt_male, 
                           names(inner_dt_male),
                           c('seqn',
                             'gender',
                             'reth',
                             'age_years',
                             'age_months',
                             'height',
                             'weight',
                             'serum_creat',
                             'hematocrit'))
    }
    
    if (gendernum$Female>0){
      mec_svy_sub_female <- subset(nhanes_mec_svy,
                                   ridexagm>=min(agelim_months) & 
                                     ridexagm<max(agelim_months) &
                                     ridreth1 %in% reths &
                                     riagendr == 'Female' &
                                     weight_class %in% 
                                     weight_category &
                                     is.finite(bmxhtlenavg) &
                                     is.finite(bmxwt) &
                                     (ridexagy<1 |
                                        (ridexagy>=1 &
                                           !is.na(lbxhct))
                                     ) &
                                     (ridexagy<12 |
                                        (ridexagy>=12 &
                                           !is.na(lbxscr))
                                     )
      )
      
      #Now, sample (with replacement) a population of gendernum$Female females
      inner_dt_female <- mec_svy_sub_female$variables[sample(x=seq_along(mec_svy_sub_female$prob),
                                                             size=gendernum$Female,
                                                             replace=TRUE,
                                                             prob=mec_svy_sub_female$variables[,
                                                                                               wtmec6yr]),
                                                      list(seqn,
                                                        riagendr,
                                                        ridreth1,
                                                        ridexagy,
                                                        ridexagm,
                                                        bmxhtlenavg,
                                                        bmxwt,
                                                        lbxscr,
                                                        lbxhct)]
      data.table::setnames(inner_dt_female, 
                           names(inner_dt_female),
                           c('seqn',
                             'gender',
                             'reth',
                             'age_years',
                             'age_months',
                             'height',
                             'weight',
                             'serum_creat',
                             'hematocrit'))
    }
    
    if (gendernum$Male>0 & gendernum$Female>0){
      inner_dt <- rbind(inner_dt_male,
                        inner_dt_female)
    } else if (gendernum$Female==0){
      inner_dt <- data.table::copy(inner_dt_male)
    } else if (gendernum$Male==0){
      inner_dt <- data.table::copy(inner_dt_female)
    }
  }
  
  #Compute tissue masses and flows
  inner_dt <- tissue_masses_flows(tmf_dt=inner_dt)
  #Calculate GFR:
  #for people over 18,
  #Estimate GFR from serum creatinine using CKD-EPI equation
  inner_dt <- estimate_gfr(inner_dt,
                           gfr_resid_var = gfr_resid_var,
                           ckd_epi_race_coeff = ckd_epi_race_coeff)
  #Hematocrit: was not measured for infants < 1 year old;
  #instead, sample hematocrit from log-normal distributions
  if (min(agelim_years)<1) {
    inner_dt[age_years<1,
             hematocrit:=hematocrit_infants(age_months=age_months)]
  }
  
  #And we're done with the inner loop.
  return(inner_dt)
}
