#' Generate a virtual population for PBTK
#'
#' Generate a virtual population characterized by demographic, anthropometric, and physiological parameters relevant to PBTK.
#' 
#' Demographic and anthropometric (body measures) variables, along with serum creatinine and hematocrit,
#' are generated from survey data from the Centers for Disease Control's National Health and Nutrition Examination Survey (NHANES).
#' Those data are stored in the object \code{nhanes_mec_svy} (a \code{survey.design} object, see package \code{survey}).
#' With \code{method = "d"}, these variables will be sampled with replacement directly from NHANES data. Each NHANES respondent's likelihood of being sampled is given by their sample weight.
#' With \code{method = "v"}, these variables will be sampled from distributions fitted to NHANES data.
#' Tissue masses and flows are generated based on demographic, body measures, and serum creatinine values, using regression equations from the literature and/or allometric scaling based on height. 
#' Extensive details about how each of these parameters are generated are available in the supplemental material of Ring et al. (2017) (see References for full citation).
#' 
#' 
#' @section Demographic variables:
#'   \tabular{rrr}{
#'   \strong{Name} \tab \strong{Definition} \tab \strong{Units} \cr
#'   \code{seqn} \tab NHANES unique identifier (only included if \code{method = "direct resampling"}) \tab NA \cr
#'   \code{gender} \tab Sex: "Male" or "Female" \tab NA \cr
#'   \code{reth} \tab Race/ethnicity: "Non-Hispanic Black", "Non-Hispanic white", "Mexican American", "Other Hispanic", or "Other". \tab NA \cr
#'   \code{age_years} \tab Age (0-79 years) \tab years \cr
#'   \code{age_months} \tab Age (0-959 months) \tab months
#'   }
#' @section Body measures and laboratory measurements:
#'   \tabular{rrr}{
#'    \strong{Name} \tab \strong{Definition} \tab \strong{Units} \cr
#'   \code{height} \tab Height \tab cm \cr
#'   \code{weight} \tab Body weight \tab kg \cr
#'   \code{serum_creat} \tab Serum creatinine \tab mg/dL \cr
#'   \code{hematocrit} \tab Hematocrit (percentage by volume of red blood cells in blood) \tab \% 
#'   }
#' @section Tissue masses:
#'   \tabular{rrr}{
#'    \strong{Name} \tab \strong{Definition} \tab \strong{Units} \cr
#'   \code{Blood_mass} \tab Mass of blood \tab kg \cr
#'   \code{Brain_mass} \tab Mass of brain \tab kg \cr
#'   \code{Gonads_mass} \tab Mass of gonads \tab kg \cr
#'   \code{Heart_mass} \tab Mass of heart \tab kg \cr
#'   \code{Kidneys_mass} \tab Mass of kidneys \tab kg \cr
#'   \code{Large_intestine_mass} \tab Mass of large intestine \tab kg \cr
#'   \code{Liver_mass} \tab Mass of liver \tab kg \cr
#'   \code{Lung_mass} \tab Mass of lungs \tab kg \cr
#'   \code{Muscle_mass} \tab Mass of skeletal muscle \tab kg \cr
#'   \code{Pancreas_mass} \tab Mass of pancreas \tab kg \cr
#'   \code{Skeleton_mass} \tab Mass of skeleton (including bone, red and yellow marrow, cartilage, periarticular tissue) \tab kg \cr
#'   \code{Skin_mass} \tab Mass of skin \tab kg \cr
#'   \code{Small_intestine_mass} \tab Mass of small intestine \tab kg \cr
#'   \code{Spleen_mass} \tab Mass of spleen \tab kg \cr
#'   \code{Stomach_mass} \tab Mass of stomach tissue \tab kg \cr
#'   \code{Other_mass} \tab Mass of GI tract contents (1.4\% of body weight) and tissues not otherwise enumerated (3.3\% of body weight). \tab kg \cr
#'   \code{org_mass_sum} \tab Sum of the above tissue masses.  A check to ensure this is less than body weight. \tab kg \cr
#'   \code{Adipose_mass} \tab Mass of adipose tissue. Assigned as \code{weight - org_mass_sum}. \tab kg
#'   }
#'   
#' @section Tissue flows:
#' \tabular{rrr}{
#'  \strong{Name} \tab \strong{Definition} \tab \strong{Units} \cr
#'   \code{Adipose_flow} \tab Blood flow to adipose tissue \tab L/h \cr
#'   \code{Brain_flow} \tab Blood flow to brain tissue \tab L/h \cr
#'   \code{CO} \tab Cardiac output \tab L/h \cr
#'   \code{Gonads_flow} \tab Blood flow to gonads tissue \tab L/h \cr
#'   \code{Heart_flow} \tab Blood flow to heart tissue \tab L/h \cr
#'   \code{Kidneys_flow} \tab Blood flow to kidneys tissue (not for glomerular filtration!) \tab L/h \cr
#'   \code{Large_intestine_flow} \tab Blood flow to large intestine tissue \tab L/h \cr
#'   \code{Liver_flow} \tab Blood flow to liver tissue \tab L/h \cr
#'   \code{Lung_flow} \tab Blood flow to lung tissue \tab L/h \cr
#'   \code{Muscle_flow} \tab Blood flow to skeletal muscle tissue \tab L/h \cr
#'   \code{Pancreas_flow} \tab Blood flow to pancreas tissue \tab L/h \cr
#'   \code{Skeleton_flow} \tab Blood flow to skeleton \tab L/h \cr
#'   \code{Skin_flow} \tab Blood flow to skin \tab L/h \cr
#'   \code{Small_intestine_flow} \tab Blood flow to small intestine \tab L/h \cr
#'   \code{Spleen_flow} \tab Blood flow to spleen \tab L/h \cr
#'   \code{Stomach_flow} \tab Blood flow to stomach \tab L/h \cr
#'   \code{org_flow_check} \tab Sum of blood flows as a fraction of cardiac output (CO). A check to make sure this is less than 1. \tab Unitless fraction
#'   }
#'   
#' @section Adjusted variables:
#' \tabular{rrr}{
#'  \strong{Name} \tab \strong{Definition} \tab \strong{Units} \cr
#'   \code{weight_adj} \tab Adjusted body weight: Sum of all tissue masses. \tab kg \cr
#'   \code{BSA_adj} \tab Adjusted body surface area, based on \code{height} and \code{weight_adj}. \tab cm^2 \cr
#'   \code{million.cells.per.gliver} \tab Hepatocellularity \tab 1e6 cells/g liver \cr
#'   \code{gfr_est} \tab Glomerular filtration rate (GFR) estimated using either the CKD-EPI equation (for adults) or a body-surface-area-based equation (for children). \tab mL/min/1.73 m^2 body surface area \cr
#'   \code{bmi_adj} \tab Body mass index (BMI), adjusted to match \code{weight_adj} and \code{height}. \tab kg/m^2 \cr
#'   \code{weight_class} \tab Weight category based on \code{bmi_adj}: "Underweight" (BMI < 18.5), "Normal" (18.5 < BMI < 24.9), "Overweight" (25.0 < BMI < 29.9), or "Obese" (BMI >= 30) \tab Unitless category \cr
#'   \code{gfr_class} \tab Kidney function category based on GFR: "Normal" (GFR >=60 mL/min/1.73 m^2), "Kidney Disease" (15 <= GFR <= 60), or "Kidney Failure" (GFR < 15). \tab Unitless category
#'   }
#'
#' @param method The population-generation method to use. Either "virtual
#'   individuals" or "direct resampling." Short names may be used: "d" or "dr"
#'   for "direct resampling", and "v" or "vi" for "virtual individuals".
#' @param nsamp The desired number of individuals in the virtual population.
#'   \code{nsamp} need not be provided if \code{gendernum} is provided.
#' @param gendernum Optional: A named list giving the numbers of male and female
#'   individuals to include in the population, e.g. \code{list(Male=100,
#'   Female=100)}. Default is NULL, meaning both males and females are included,
#'   in their proportions in the NHANES data. If both \code{nsamp} and
#'   \code{gendernum} are provided, they must agree (i.e., \code{nsamp} must be
#'   the sum of \code{gendernum}).
#' @param agelim_years Optional: A two-element numeric vector giving the minimum
#'   and maximum ages (in years) to include in the population. Default is
#'   c(0,79). If only a single value is provided, both minimum and maximum ages
#'   will be set to that value; e.g. \code{agelim_years=3} is equivalent to
#'   \code{agelim_years=c(3,3)}. If \code{agelim_years} is provided and
#'   \code{agelim_months} is not, \code{agelim_years} will override the default
#'   value of \code{agelim_months}.
#' @param agelim_months Optional: A two-element numeric vector giving the
#'   minimum and maximum ages (in months) to include in the population. Default
#'   is c(0, 959), equivalent to the default \code{agelim_years}. If only a
#'   single value is provided, both minimum and maximum ages will be set to that
#'   value; e.g. \code{agelim_months=36} is equivalent to
#'   \code{agelim_months=c(36,36)}. If \code{agelim_months} is provided and
#'   \code{agelim_years} is not, \code{agelim_months} will override the default
#'   values of \code{agelim_years}.
#' @param weight_category Optional: The weight categories to include in the
#'   population. Default is \code{c('Underweight', 'Normal', 'Overweight',
#'   'Obese')}. User-supplied vector must contain one or more of these strings.
#' @param gfr_category The kidney function categories to include in the
#'   population. Default is \code{c('Normal','Kidney Disease', 'Kidney
#'   Failure')} to include all kidney function levels.
#' @param reths Optional: a character vector giving the races/ethnicities to
#'   include in the population. Default is \code{c('Mexican American','Other
#'   Hispanic','Non-Hispanic White','Non-Hispanic Black','Other')}, to include
#'   all races and ethnicities in their proportions in the NHANES data.
#'   User-supplied vector must contain one or more of these strings.
#' @param gfr_resid_var TRUE to add residual variability to GFR predicted from
#'   serum creatinine; FALSE to not add residual variability
#' @param ckd_epi_race_coeff TRUE to use the CKD-EPI equation as originally
#'   published (with a coefficient changing predicted GFR for individuals
#'   identified as "Non-Hispanic Black"); FALSE to set this coefficient to 1.
#' @return A data.table where each row represents an individual, and each column
#'   represents a demographic, anthropometric, or physiological parameter.
#'   Details of the parameters returned and their units are in the following tables.
#'
#'
#' @author Caroline Ring
#'
#' @references 
#'\insertRef{ring2017identifying}{httk} 
#'
#' @keywords httk-pop monte-carlo
#'
#' @examples
#'
#' \donttest{
#' #Simply generate a virtual population of 100 individuals,
#'  #using the direct-resampling method
#'  set.seed(42)
#' httkpop_generate(method='direct resampling', nsamp=100)
#' 
#' #Generate a population using the virtual-individuals method,
#' #including 80 females and 20 males,
#' #including only ages 20-65,
#' #including only Mexican American and
#' #Non-Hispanic Black individuals,
#' #including only non-obese individuals
#' set.seed(42)
#' mypop <- httkpop_generate(method = 'virtual individuals',
#'                           gendernum=list(Female=80,
#'                           Male=20),
#'                           agelim_years=c(20,65),
#'                           reths=c('Mexican American',
#'                           'Non-Hispanic Black'),
#'                           weight_category=c('Underweight',
#'                           'Normal',
#'                           'Overweight'))
#' # Including a httkpop.dt argument will overwrite the number of sample and
#' # the httkpop on/off logical switch:
#' samps1 <- create_mc_samples(chem.name="bisphenola",
#'                            httkpop=FALSE,
#'                            httkpop.dt=mypop)
#' samps2 <- create_mc_samples(chem.name="bisphenola",
#'                            httkpop.dt=mypop)
#' samps3 <- create_mc_samples(chem.name="bisphenola",
#'                            httkpop=FALSE)
#' # Now run calc_mc_oral equiv on the same pop for two different chemcials:
#' calc_mc_oral_equiv(conc=10,
#'                    chem.name="bisphenola",
#'                    httkpop.dt=mypop,
#'                    return.samples=TRUE)
#' calc_mc_oral_equiv(conc=2,
#'                    chem.name="triclosan",
#'                    httkpop.dt=mypop,
#'                    return.samples=TRUE)
#' }
#'
#'
#'
#' @export httkpop_generate
httkpop_generate <- function(method,
                             nsamp=NULL,
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
                                     'Other'),
                             gfr_resid_var = TRUE,
                             ckd_epi_race_coeff = FALSE){

# Error checking:
  if (is.null(method)) stop(
"Specify method as \"virtual individuals\" (\"v\" or \"vi\") or \"direct\n\
resampling\" (\"dr\" or \"d\").")
  else if(!method %in% c('direct resampling',
                          'virtual individuals',
                          'v',
                          'vi',
                          'direct resampling',
                          'dr',
                          'd')) stop( 
"Specify method as \"virtual individuals\" (\"v\" or \"vi\") or \"direct\n\
resampling\" (\"dr\" or \"d\").")

  #Check age limits
  if (is.null(agelim_years) & is.null(agelim_months)){
    #Use default values
    agelim_years <- c(0,79)
    agelim_months <- c(max(0, 
                           12*min(agelim_years)),
                       12*(max(agelim_years)+1)-1)
  }else if (is.null(agelim_months) & !is.null(agelim_years)){
    #if agelim_years provided but not agelim_months, then set agelim_months from
    #agelim_years
    agelim_months <- c(max(0, 
                           12*min(agelim_years)),
                       12*(max(agelim_years)+1)-1)
  } else if (is.null(agelim_years) & !is.null(agelim_months)){
    #if agelim_months provided but not agelim_years, then set agelim_years from 
    #agelim_months
    #if only one value for agelim_months is provided, use it for both max and min
    if (length(agelim_months)==1) agelim_months <- rep(agelim_months,2)
    agelim_years <- agelim_months/12
  } else if (!is.null(agelim_years) & !is.null(agelim_months)){
    #if agelim_months and agelim_years are both provided but do not agree, 
    #then throw an error and stop
    if (length(agelim_months)==1) agelim_months <- rep(agelim_months,2)
    if (length(agelim_years)==1) agelim_years <- rep(agelim_years,2)
    if (!all(agelim_years == agelim_months/12)){
      if (all(agelim_years==floor(agelim_months/12))){
        warning("agelim_months and agelim_years agree, 
                but agelim_years is in whole years and agelim_months is not. 
                Using agelim_months.")
        agelim_years <- agelim_months/12
      }else{
        stop("agelim_years and agelim_months were both provided, but do not agree. 
             Please provide only one of agelim_years or agelim_months.")
      }
    }
  }
  
  #If both nsamp and gendernum are NULL, stop with error
  if (is.null(nsamp) & is.null(gendernum)){
    stop(paste('Either nsamp must be provided,',
               'or gendernum must be provided as a numeric list',
               'with names Male and Female giving',
               'the number of individuals of each',
               'gender in the population to be generated.'))
  }
  #Check nsamp argument
  if (!is.null(nsamp)){
    #it should be numeric
    if (!is.numeric(nsamp)) stop('nsamp must be a positive integer.')
    #it should have length 1
    if (length(nsamp)>1) stop('nsamp must be a positive integer.')
    #it should be positive and an integer
    if (nsamp!=floor(nsamp) | 
          nsamp<=0) stop('nsamp must be a positive integer.')
  }
  
  #Check gendernum argument
  if (!is.null(gendernum)){
    #it should be a list
    if (!is.list(gendernum)){
      stop(paste('gendernum must be either NULL or a numeric list',
                 'with names Male and Female,',
                 'giving the number of individuals of each gender',
                 'in the population to be sampled. ',
                 'Here, gendernum was not a list.'))
    }
    #Its names must be "Male" and "Female" (in either order)
    if (!identical(sort(names(gendernum)),
                   sort(c('Male', 'Female')))){
      stop(paste('gendernum must be either NULL or a numeric list',
                 'with names Male and Female,',
                 'giving the number of individuals of each gender',
                 'in the population to be sampled. ',
                 'Here, gendernum did not have names Male and Female.'))
    }
    
    #Its members should be numeric
    if (!all(sapply(gendernum, is.numeric))){
      stop(paste('gendernum must be either NULL or a numeric list',
                 'with names Male and Female,',
                 'giving the number of individuals of each gender',
                 'in the population to be sampled. ',
                 'Here, not all members of gendernum were numeric.'))
    }
    #Its members must have length 1
    if (!all(sapply(gendernum, 
                    function(x) length(x)==1))){
      stop(paste('gendernum must be either NULL or a numeric list',
                 'with names Male and Female,',
                 'giving the number of individuals of each gender',
                 'in the population to be sampled. ',
                 'Here, not all members of gendernum had length 1.'))
    }
    #Its members must be positive integers
    if (!all(sapply(gendernum, 
                    function(x) x==floor(x) &
                      x>=0))){
      stop(paste('gendernum must be either NULL or a numeric list',
                 'with names Male and Female,',
                 'giving the number of individuals of each gender',
                 'in the population to be sampled. ',
                 'Here, not all members of gendernum were positive integers.'))
    }
    #Its members cannot both be zero
    if (all(gendernum==0)) {
      stop('At least one element of gendernum must be greater than zero.')
    }
    
    #Check nsamp argument
    if (!is.null(nsamp)){
      #it must agree with gendernum if both are provided
      if (Reduce('+', gendernum)!=nsamp){
        stop('If both gendernum and nsamp are provided, ',
             'the elements of gendernum must sum to nsamp.')
      }
    } else{ #if gendernum is provided but nsamp is not,
      #use the sum of gendernum as nsamp
      nsamp <- Reduce('+', gendernum)
    }
  }
  
  #Check weight_category argument
  if (!all(weight_category %in% c('Underweight',
                                  'Normal',
                                  'Overweight',
                                  'Obese'))) {
    stop("User-provided weight_category must be a subset of c('Underweight',
    'Normal',
    'Overweight',
    'Obese'")
  }
  
  #Check reths argument
  if(!all(reths %in% c('Mexican American',
                       'Other Hispanic',
                       'Non-Hispanic White',
                       'Non-Hispanic Black',
                       'Other'))){
    stop("User-provided reths must be a subset of c('Mexican American',
                           'Other Hispanic',
                           'Non-Hispanic White',
                           'Non-Hispanic Black',
                           'Other')")
  }
  
  #Check gfr_category argument
  if(!all(gfr_category %in% c('Normal',
                              'Kidney Disease', 
                              'Kidney Failure'))){
    stop("User-provded gfr_category must be a subset of c('Normal',
                                            'Kidney Disease', 
                                            'Kidney Failure')")
  }
  
  #Handle short versions of method names
  if (tolower(method)=='d' | tolower(method)=='dr') method <- 'direct resampling'
  if (tolower(method)=='v' | tolower(method)=='vi') method <- 'virtual individuals'
  
  #Create survey::svydesign object
  nhanes_mec_svy <- survey::svydesign(data = mecdt, #referring to mecdt data set
                                      strata=~sdmvstra, #masked stratification variable name
                                      id=~sdmvpsu, #masked PSU (cluster) variable name
                                      weights=~wtmec6yr, #use examination sample weights
                                      nest=TRUE) #nest means masked PSUs are reused within each stratum.
  
  #Now actually do the generating.
  if (method == 'virtual individuals'){
    indiv_dt <- httkpop_virtual_indiv(nsamp=nsamp,
                                      gendernum=gendernum,
                                      agelim_years=agelim_years, 
                                      agelim_months=agelim_months,
                                      weight_category=weight_category, 
                                      gfr_category=gfr_category,
                                      reths=reths,
                                      gfr_resid_var = gfr_resid_var,
                                      ckd_epi_race_coeff = ckd_epi_race_coeff,
                                      nhanes_mec_svy = nhanes_mec_svy)
  } else if (method == 'direct resampling'){
    indiv_dt <- httkpop_direct_resample(nsamp=nsamp,
                                        gendernum=gendernum,
                                        agelim_years=agelim_years, 
                                        agelim_months=agelim_months,
                                        weight_category=weight_category, 
                                        gfr_category=gfr_category,
                                        reths=reths,
                                        gfr_resid_var = gfr_resid_var,
                                        ckd_epi_race_coeff = ckd_epi_race_coeff,
                                        nhanes_mec_svy = nhanes_mec_svy)
  }
  
  # set precision:
  cols <- colnames(indiv_dt)
  indiv_dt[ , (cols) := lapply(.SD, set_httk_precision), .SDcols = cols]
  
  return(indiv_dt)
}
