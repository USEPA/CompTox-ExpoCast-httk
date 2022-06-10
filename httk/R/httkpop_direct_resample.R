#' Generate a virtual population by directly resampling the NHANES data.
#' 
#' Generate a virtual population by directly resampling the NHANES data.
#' 
#' 
#' @param nsamp The desired number of individuals in the virtual population.
#' \code{nsamp} need not be provided if \code{gendernum} is provided.
#' @param gendernum Optional: A named list giving the numbers of male and
#' female individuals to include in the population, e.g. \code{list(Male=100,
#' Female=100)}. Default is NULL, meaning both males and females are included,
#' in their proportions in the NHANES data. If both \code{nsamp} and
#' \code{gendernum} are provided, they must agree (i.e., \code{nsamp} must be
#' the sum of \code{gendernum}).
#' @param agelim_years Optional: A two-element numeric vector giving the
#' minimum and maximum ages (in years) to include in the population. Default is
#' c(0,79). If \code{agelim_years} is provided and \code{agelim_months} is not,
#' \code{agelim_years} will override the default value of \code{agelim_months}.
#' @param agelim_months Optional: A two-element numeric vector giving the
#' minimum and maximum ages (in months) to include in the population. Default
#' is c(0, 959), equivalent to the default \code{agelim_years}. If
#' \code{agelim_months} is provided and \code{agelim_years} is not,
#' agelim_months will override the default values of \code{agelim_years}.
#' @param weight_category Optional: The weight categories to include in the
#' population. Default is \code{c('Underweight', 'Normal', 'Overweight',
#' 'Obese')}. User-supplied vector must contain one or more of these strings.
#' @param gfr_category The kidney function categories to include in the
#' population. Default is \code{c('Normal','Kidney Disease', 'Kidney Failure')}
#' to include all kidney function levels.
#' @param reths Optional: a character vector giving the races/ethnicities to
#' include in the population. Default is \code{c('Mexican American','Other
#' Hispanic','Non-Hispanic White','Non-Hispanic Black','Other')}, to include
#' all races and ethnicities in their proportions in the NHANES data.
#' User-supplied vector must contain one or more of these strings.
#' @param gfr_resid_var Logical value indicating whether or not to include
#' residual variability when generating GFR values. (Default is TRUE.)
#' @param ckd_epi_race_coeff Logical value indicating whether or not to use the
#' "race coefficient" from the CKD-EPI equation when estimating GFR values.
#' (Default is FALSE.)
#' @return A data.table where each row represents an individual, and each
#' column represents a demographic, anthropometric, or physiological parameter.
#'
#' @author Caroline Ring
#'
#' @references Ring, Caroline L., et al. "Identifying populations sensitive to
#' environmental chemicals by simulating toxicokinetic variability."
#' Environment International 106 (2017): 105-118
#'
#' @keywords httk-pop monte-carlo
#'
#' @export httkpop_direct_resample
httkpop_direct_resample <- function(nsamp=NULL,
                                    gendernum=NULL,
                                    agelim_years=c(0,79), 
                                    agelim_months=c(0,959),
                                    weight_category=c('Underweight',
                                                      'Normal',
                                                      'Overweight',
                                                      'Obese'), 
                                    gfr_category=c('Normal',
                                                   'Kidney Disease', 
                                                   'Kidney Failure'),
                                    reths=c('Mexican American',
                                            'Other Hispanic',
                                            'Non-Hispanic White',
                                            'Non-Hispanic Black',
                                            'Other'),
                                    gfr_resid_var = TRUE,
                                    ckd_epi_race_coeff = FALSE,
                                    nhanes_mec_svy){
  
  #R CMD CHECK throws notes about "no visible binding for global variable", for
  #each time a data.table column name is used without quotes. To appease R CMD
  #CHECK, a variable has to be created for each of these column names and set to
  #NULL. Note that within the data.table, these variables will not be NULL! Yes,
  #this is pointless and annoying.
  bmi_adj <- weight_adj <- height <- weight_class <- NULL
  age_years <- age_months <- gender <- gfr_class <- gfr_est <- NULL
  Adipose_mass <- org_flow_check <- NULL
  #End R CMD CHECK appeasement.
  
  
  #Draw an initial virtual population
  indiv_dt <- httkpop_direct_resample_inner(nsamp=nsamp,
                                            gendernum=gendernum,
                                            agelim_months=agelim_months,
                                            agelim_years=agelim_years,
                                            reths=reths,
                                            weight_category=weight_category,
                                            gfr_resid_var = gfr_resid_var,
                                            ckd_epi_race_coeff = ckd_epi_race_coeff,
                                            nhanes_mec_svy)
  #Compute BMI
  indiv_dt[, bmi_adj:=weight_adj/((height/100)^2)]
  #Assign weight class
  indiv_dt[, weight_class:=get_weight_class(age_years=age_years,
                                            age_months=age_months,
                                            bmi=bmi_adj,
                                            gender=gender,
                                            recumlen=height,
                                            weight=weight_adj)]
  #Check on GFR category
  indiv_dt[, gfr_class:=get_gfr_category(age_years=age_years,
                                         age_months=age_months,
                                         gfr_est=gfr_est)]
  
  #Rejection sampling
  #Rejection conditions:
  reject_cond <- indiv_dt[, 
                          #if weight class not in specified weight categories
                          !(weight_class %in% weight_category) | 
                            #if kidney function class not in specified kidney
                            #function categories
                            !(gfr_class %in% gfr_category) | 
                            #if adipose is <10% of body weight
                            Adipose_mass/weight_adj<0.1 |
                            #if sum of organ flows >= 100% of CO
                            org_flow_check>=1 |
                            #if height > 244 cm (8 feet)
                            height>244 |
                            #if weight > 453 kg (about 1000 lbs)
                            weight_adj>453]
  rct <- 1
  while(sum(reject_cond)>0){ #while any individuals meet rejection conditions
    #redraw rejected individuals;
    #keep their genders the same.
    indiv_tmp <- httkpop_direct_resample_inner(nsamp=sum(reject_cond),
                                               gendernum=list(Male=nrow(indiv_dt[gender=='Male' &
                                                                                   reject_cond,]),
                                                              Female=nrow(indiv_dt[gender=='Female' &
                                                                                     reject_cond,])),
                                               agelim_months=agelim_months,
                                               agelim_years=agelim_years,
                                               reths=reths,
                                               weight_category=weight_category,
                                               gfr_resid_var = gfr_resid_var,
                                               ckd_epi_race_coeff = ckd_epi_race_coeff,
                                               nhanes_mec_svy)
    #Recalculate BMI for the newly redrawn individuals
    indiv_tmp[, bmi_adj:=weight_adj/((height/100)^2)]
    #Check on weight class
    indiv_tmp[, weight_class:=get_weight_class(age_years=age_years,
                                               age_months=age_months,
                                               bmi=bmi_adj,
                                               gender=gender,
                                               recumlen=height,
                                               weight=weight_adj)]
    #Check on GFR category
    indiv_tmp[, gfr_class:=get_gfr_category(age_years=age_years,
                                            age_months=age_months,
                                            gfr_est=gfr_est)]
    #Replace the rejected individuals with the newly redrawn ones
    indiv_dt <- rbind(indiv_tmp,
                      indiv_dt[!reject_cond,])
    #Check rejection conditions again to see if any individuals still need to be
    #redrawn
    reject_cond <- indiv_dt[, !(weight_class %in% weight_category) |
                              !(gfr_class %in% gfr_category) |
                              Adipose_mass/weight_adj<0.1 |
                              org_flow_check>=1 |
                              height>244 |
                              weight_adj>453]   
    #if so, repeat loop....
    rct <- rct+1
  }
  
  #If any column names have spaces,
  #replace them with underscores _ for ease of use later
  setnames(indiv_dt, 
                       names(indiv_dt), 
                       gsub(names(indiv_dt), 
                            pattern=' ', 
                            replacement='_'))
  
  #Final recomputation of BMI
  indiv_dt[, bmi_adj:=weight_adj/((height/100)^2)]
  #Final recomputation of weight class
  indiv_dt[, weight_class:=get_weight_class(age_years=age_years,
                                            age_months=age_months,
                                            bmi=bmi_adj,
                                            gender=gender,
                                            recumlen=height,
                                            weight=weight_adj)]
  #Final recomputation of GFR class
  indiv_dt[, gfr_class:=get_gfr_category(age_years=age_years,
                                         age_months=age_months,
                                         gfr_est=gfr_est)]
  
  #And we're done!
  return(indiv_dt)
}
