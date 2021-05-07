#' Monte Carlo for pharmacokinetic models
#' 
#' This function performs Monte Carlo to assess uncertainty and variability for
#' toxicokinetic models. 
#' 
#' @param parameters These parameters that are also listed in either
#' cv.params or censored.params are sampled using Monte Carlo.
#' @param cv.params The parameters listed in cv.params are sampled from a
#' normal distribution that is truncated at zero. This argument should be a
#' list of coefficients of variation (cv) for the normal distribution. Each
#' entry in the list is named for a parameter in "parameters". New values are
#' sampled with mean equal to the value in "parameters" and standard deviation
#' equal to the mean times the cv.
#' @param censored.params The parameters listed in censored.params are sampled
#' from a normal distribution that is censored for values less than the limit
#' of detection (specified separately for each parameter). This argument should
#' be a list of sub-lists. Each sublist is named for a parameter in "params"
#' and contains two elements: "cv" (coefficient of variation) and "LOD" (limit
#' of detection), below which parameter values are censored. New values are
#' sampled with mean equal to the value in "params" and standard deviation
#' equal to the mean times the cv. Censored values are sampled on a uniform
#' distribution between 0 and the limit of detection.
#' @param samples This argument is the number of samples to be generated for
#' calculating quantiles.
#'
#' @author John Wambaugh
#'
#' @return 
#' A data.table with a row for each individual in the sample and a column for
#' each parater in the model.
#' 
#' @references 
#' Pearce, Robert G., et al. "Httk: R package for high-throughput 
#' toxicokinetics." Journal of statistical software 79.4 (2017): 1.
#'
#' @keywords Monte-Carlo
#'
#' @examples
#' 
#' #Example based on Pearce et al. (2017):
#' 
#' # Set up means:
#' params <- parameterize_pbtk(chem.name="zoxamide")
#' # Nothing changes:
#' monte_carlo(params)
#' 
#' vary.params <- NULL
#' for (this.param in names(params)[!(names(params) %in%
#'   c("Funbound.plasma", "pKa_Donor", "pKa_Accept" )) &
#'   !is.na(as.numeric(params))]) vary.params[this.param] <- 0.2
#' # Most everything varies with CV of 0.2:
#' monte_carlo(
#'   parameters=params, 
#'   cv.params = vary.params)
#' 
#' censored.params <- list(Funbound.plasma = list(cv = 0.2, lod = 0.01))
#' # Fup is censored below 0.01:
#' monte_carlo(
#'   parameters=params, 
#'   cv.params = vary.params,
#'   censored.params = censored.params)
#' 
#' @import stats data.table
#' 
#' @export monte_carlo
monte_carlo <- function(
                 parameters,
                 cv.params=NULL,
                 censored.params=NULL,
                 samples=1000)
{

# Create a data table with the same parameters in every row:  
  MC.matrix <- as.data.table(parameters)[rep(1,samples)]

# Number of different results to be obtained for the different paramters:
  sample.vec <- rep(NA,samples)
# Any parameter given in cv.params is sampled from a normal distribution 
# truncated at zero:
  for (this.param in names(cv.params))
  {
    if (!(this.param %in% names(parameters))) 
      stop(paste("Cannot find cv.params parameter",
        this.param,
        "in parameter list."))
    if (parameters[[this.param]]>0) 
      MC.matrix[,this.param] <- 
      rtnorm(
        samples,
        mean=parameters[[this.param]],
        sd=parameters[[this.param]]*cv.params[[this.param]],
        lower=0)
    else 
    {
      MC.matrix[,this.param] <- 0  
      warning(paste(
        this.param,
        "has mean of zero, yielding SD of zero for fixed cv.\n\
Parameter value fixed at zero."))
    }
  }
  
# Any parameter given in censored parameters is sampled from a censored distribution:
  for (this.param in names(censored.params))
  {
    if (!(this.param %in% names(parameters))) 
      stop(paste("Cannot find censored.params parameter",
        this.param,"in parameter list."))
    if (!("cv" %in% names(censored.params[[this.param]]))) 
      stop(paste("cv (coefficient of variation) must be specified for parameter",
        this.param))
    if (!("lod" %in% names(censored.params[[this.param]]))) 
      stop(paste("lod (limit of detection) must be specified for parameter",
        this.param))
    if(this.param %in% c('Funbound.plasma','Fhep.assay.correction'))  upper <- 1
    else upper <- Inf
    MC.matrix[,this.param] <- r_left_censored_norm(samples,
      mean=parameters[[this.param]],
      sd=parameters[[this.param]]*censored.params[[this.param]]$cv,
      lod=censored.params[[this.param]]$lod,
      upper=upper)
  }

  return(MC.matrix)
}


