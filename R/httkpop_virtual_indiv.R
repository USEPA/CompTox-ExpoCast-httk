#'Generate a virtual population by the virtual individuals method.
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
#'@param gfr_category The kidney function categories to include in the 
#'  population. Default is \code{c('Normal','Kidney Disease', 'Kidney Failure')}
#'  to include all kidney function levels.
#'
#'@return A data.table where each row represents an individual, and
#'  each column represents a demographic, anthropometric, or physiological
#'  parameter.
#'
#' @keywords httk-pop monte-carlo
#'
#'@author Caroline Ring
#'
#'@references Ring, Caroline L., et al. "Identifying populations sensitive to 
#'environmental chemicals by simulating toxicokinetic variability." Environment 
#'International 106 (2017): 105-118
#' @export httkpop_virtual_indiv
httkpop_virtual_indiv<- function(nsamp=NULL,
                                 gendernum=NULL,
                                 agelim_years=NULL, 
                                 agelim_months=NULL,
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
                                         'Other')) {
  
  #R CMD CHECK throws notes about "no visible binding for global variable", for
  #each time a data.table column name is used without quotes. To appease R CMD
  #CHECK, a variable has to be created for each of these column names and set to
  #NULL. Note that within the data.table, these variables will not be NULL! Yes,
  #this is pointless and annoying.
  bmi_adj <- weight_adj <- height <- weight_class <- NULL
  age_years <- age_months <- gender <- gfr_class <- gfr_est <- NULL
  Adipose_mass <- org_flow_check <- NULL
  #End R CMD CHECK appeasement.
  
  #Generate age, height, and weight.
  indiv_dt <- gen_age_height_weight(nsamp=nsamp,
                                    gendernum=gendernum,
                                    agelim_months=agelim_months,
                                    agelim_years=agelim_years,
                                    weight_category=weight_category,
                                    reths=reths)
  #Generate tissue masses and flows.
  indiv_dt <- tissue_masses_flows(tmf_dt=indiv_dt)
  
  #Estimate GFR (including draw individual values of serum creatinine)
  indiv_dt<-estimate_gfr(gfrtmp.dt=indiv_dt)
  
  #Compute BMI using adjusted individual weights
  indiv_dt[, bmi_adj:=weight_adj/((height/100)^2)]
  #Get weight classes for each individual,
  #based on age and BMI or gender/height/weight
  indiv_dt[, weight_class:=get_weight_class(age_years=age_years,
                                            age_months=age_months,
                                            bmi=bmi_adj,
                                            gender=gender,
                                            recumlen=height,
                                            weight=weight_adj)]
  
  #get kidney function category for each individual,
  #based on age and GFR
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
    #Redraw age, height, and weight for these individuals
    #(keep their genders the same)
    indiv_tmp<-gen_age_height_weight(nsamp=sum(reject_cond),
                                     gendernum=list(
                                       Male=nrow(indiv_dt[gender=='Male' &
                                                            reject_cond,]),
                                       Female=nrow(indiv_dt[gender=='Female' &
                                                              reject_cond,])
                                     ),
                                     agelim_months=agelim_months,
                                     agelim_years=agelim_years,
                                     weight_category=weight_category,
                                     reths=reths)
    #Recompute tissue masses and flows using the new age, height, and weight
    #values
    indiv_tmp<-tissue_masses_flows(tmf_dt=indiv_tmp)
    #Recompute GFR using the new age values
    indiv_tmp<-estimate_gfr(gfrtmp.dt=indiv_tmp)
    #Recompute BMI using the new height, adjusted weight values
    indiv_tmp[, bmi_adj:=weight_adj/((height/100)^2)]
    #Recompute weight class using the new values
    indiv_tmp[, weight_class:=get_weight_class(age_years=age_years,
                                               age_months=age_months,
                                               bmi=bmi_adj,
                                               gender=gender,
                                               recumlen=height,
                                               weight=weight_adj)]
    #Recompute kidney function class using the new values
    indiv_tmp[, gfr_class:=get_gfr_category(age_years=age_years,
                                            age_months=age_months,
                                            gfr_est=gfr_est)]
    #Replace the rejected individuals with the newly redrawn individuals
    indiv_dt <- rbind(indiv_tmp,
                      indiv_dt[!reject_cond,],
                      fill=TRUE)
    #Now, re-check rejection conditions to see whether any individuals need to
    #be redrawn
    reject_cond <- indiv_dt[, !(weight_class %in% weight_category) |
                              !(gfr_class %in% gfr_category) |
                              Adipose_mass/weight_adj<0.1 |
                              org_flow_check>=1 |
                              height>244 |
                              weight_adj>453]
    #if so, repeat loop...
    rct <- rct + 1
  }
  
  #Once rejection sampling is completed,
  #draw hematocrit values
  indiv_dt <- estimate_hematocrit(hcttmp_dt=indiv_dt)
  
  #Replace any spaces in column names with underscores,
  #for ease of use later
  data.table::setnames(indiv_dt, 
                       names(indiv_dt), 
                       gsub(names(indiv_dt), 
                            pattern=' ', 
                            replacement='_'))
  #And we're done!
  return(indiv_dt)
}
