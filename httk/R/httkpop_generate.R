#declare names of data variables as global
#Effectively they are global anyway, but R CMD CHECK wants it explicit

if (getRversion() >= "2.15.1") {
  utils::globalVariables(c('age_dist_smooth', 'bmiage', 'spline_heightweight',
                  'nhanes_mec_svy', 'spline_hematocrit', 'mcnally_dt', 
                  'spline_serumcreat', 'wfl'))
}





#' Generate a virtual population
#' 
#' Generate a virtual population
#' 
#' 
#' @param method The population-generation method to use. Either "virtual
#' individuals" or "direct resampling." Short names may be used: "d" or "dr"
#' for "direct resampling", and "v" or "vi" for "virtual individuals".
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
#' c(0,79). If only a single value is provided, both minimum and maximum ages
#' will be set to that value; e.g. \code{agelim_years=3} is equivalent to
#' \code{agelim_years=c(3,3)}. If \code{agelim_years} is provided and
#' \code{agelim_months} is not, \code{agelim_years} will override the default
#' value of \code{agelim_months}.
#' @param agelim_months Optional: A two-element numeric vector giving the
#' minimum and maximum ages (in months) to include in the population. Default
#' is c(0, 959), equivalent to the default \code{agelim_years}. If only a
#' single value is provided, both minimum and maximum ages will be set to that
#' value; e.g. \code{agelim_months=36} is equivalent to
#' \code{agelim_months=c(36,36)}. If \code{agelim_months} is provided and
#' \code{agelim_years} is not, \code{agelim_months} will override the default
#' values of \code{agelim_years}.
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
#' @examples
#' 
#' \donttest{
#' #Simply generate a virtual population of 100 individuals,
#'  #using the direct-resampling method
#'  set.seed(42)
#' httkpop_generate(method='direct resampling', nsamp=100)
#' #Generate a population using the virtual-individuals method,
#' #including 80 females and 20 males,
#' #including only ages 20-65,
#' #including only Mexican American and 
#'  #Non-Hispanic Black individuals,
#'  #including only non-obese individuals
#' httkpop_generate(method = 'virtual individuals',
#' gendernum=list(Female=80, 
#' Male=20),
#' agelim_years=c(20,65), 
#' reths=c('Mexican American', 
#' 'Non-Hispanic Black'),
#' weight_category=c('Underweight',
#' 'Normal',
#' 'Overweight'))
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
                                     'Other')){

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
  
  #Now actually do the generating.
  if (method == 'virtual individuals'){
    indiv_dt <- httkpop_virtual_indiv(nsamp=nsamp,
                                      gendernum=gendernum,
                                      agelim_years=agelim_years, 
                                      agelim_months=agelim_months,
                                      weight_category=weight_category, 
                                      gfr_category=gfr_category,
                                      reths=reths)
  } else if (method == 'direct resampling'){
    indiv_dt <- httkpop_direct_resample(nsamp=nsamp,
                                        gendernum=gendernum,
                                        agelim_years=agelim_years, 
                                        agelim_months=agelim_months,
                                        weight_category=weight_category, 
                                        gfr_category=gfr_category,
                                        reths=reths)
  }
  return(indiv_dt)
}
