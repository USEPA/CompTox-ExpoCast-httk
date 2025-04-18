#' Get timeseries containing the change of each of the input parameters.
#' 
#' @description
#' The \code{deSolve} package uses timeseries as forcing functions. In lieu of hard-
#' coding time evolution of parameters into a model, these timeseries may be used
#' to change the value of parameters in time. The function \code{get_input_parm_timeseries}
#' queries a virutal population and non-parametrically produces timeseries that 
#' preserve the percentile score of the given starting values. 
#' 
#' @details
#' For each time-dependent model, there should be a function (such as \code{\link{gen_nhanes_params_pbtk_growthcurve}})
#' that determines the model parameter values for each individual in the NHANES dataset.
#' The resulting value are used to form the non-parametric regression curve. 
#'
#' @param model The name of a model which can accept timeseries as forcing functions.
#' 
#' @param chem.cas Chemical Abstract Services Registry Number (CAS-RN) -- the 
#' chemical must be identified by either CAS, name, or DTXISD
#' 
#' @param chem.name Chemical name (spaces and capitalization ignored) --  the 
#' chemical must be identified by either CAS, name, or DTXISD
#' 
#' @param dtxsid EPA's 'DSSTox Structure ID (\url{https://comptox.epa.gov/dashboard})   
#' -- the chemical must be identified by either CAS, name, or DTXSIDs
#' 
#' @param initial.params The values for each parameter at the beginning of the simulation.
#' All compiled parameters should be present. The output of the parameterize_<model>
#' function is appropriate for \code{initial.params}.
#' 
#' @param initial.percentiles If \code{initial.params} are not provided, \code{initial.percentiles}
#' will designate a starting value for each parameter according to the corresponding 
#' percentile within the NHANES population. Values should be between zero and one. If 
#' neither \code{initial.params} nor \code{initial.percentiles} are provided, the initial
#' value for the parameter is taken to be the median of the population value. 
#' 
#' @param start.age The age in months of the individual at the beginning of the simulation. 
#' Used for determining the percentile score of the initial parameter values when producing
#' the timeseries determining parameter changes. 
#' 
#' @param days The length of the simulation in days. Equivalent to the \code{days}
#' parameter in \code{\link{solve_model}}.
#' 
#' The next four parameters play the same role here as in \code{\link{httkpop_generate}}:
#' the user may restrict the data available to the non-parametric regression by 
#' specifying demographics.
#' 
#' @param gender Optional: The gender categories to include in the popualtion; 
#'  default \code{c("Female", "Male")}.
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
#' 
#' @param bandwidth Dictates the length of time centered around the present to use
#' when calculating non-parametric regressions. 
#' 
#' @param get.median.param.vals Return, instead of the timeseries, the median values
#' for the dynamic model parameters at the given start age.
#' 
#' @param input.param.dir The path to the \code{input_params_data_files} directory,
#' which is used to store all \code{input_param} data files. If \code{input_params_data_files}
#' does not exist, this function will create it in the specified path. Default \code{NULL}, 
#' in which case the present working directory is used as default.
#'
#' @return A list of two-column matrices indexed by names of compiled parameters for
#' the designated model. The first column contains a list of times (in days) and the 
#' second the total change in that parameter from the initial value.
#'
#' @author Colin Thomson
#'
#' @seealso \code{\link{solve_pbtk_growthcurve}}
#'
#' @seealso \code{\link{gen_input_params}}
#'
#' @keywords lifestage 
#'
#' @examples
#' \donttest{
#' 
#' params <- parameterize_pbtk(chem.name = 'Bisphenol A')
#' ts <- get_input_param_timeseries('pbtk_lifestage"',
#'                                  chem.name = 'Bisphenol A',
#'                                  initial.params = params,
#'                                  start.age = 600, # age fifty
#'                                  days = 365,
#'                                  weight_category=c('Underweight',
#'                                                    'Normal',
#'                                                    'Overweight'))
#' 
#' }
#' 
#' @export get_input_param_timeseries
#'
#' @useDynLib httk

get_input_param_timeseries <- function(model,
                                       chem.cas = NULL,
                                       chem.name = NULL,
                                       dtxsid = NULL,
                                       initial.params = NULL,
                                       initial.percentiles = NULL,
                                       start.age = 360, # months
                                       days = 10, # days
                                       gender=c('Male',
                                                'Female'),
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
                                       bandwidth = 12,
                                       get.median.param.vals = FALSE,
                                       input.param.dir = NULL) 
  {
  
  # We need to describe the chemical to be simulated one way or another:
  if (is.null(chem.cas) & 
      is.null(chem.name) & 
      is.null(dtxsid)) 
    stop('chem.name, chem.cas, or dtxsid must be specified.')
  
  # Look up the chemical name/CAS, depending on what was provide:
  out <- get_chem_id(
    chem.cas=chem.cas,
    chem.name=chem.name,
    dtxsid=dtxsid)
  chem.cas <- out$chem.cas
  chem.name <- out$chem.name                                
  dtxsid <- out$dtxsid
  
  # Get the names of the dynamic parameters from the input.var.names list, 
  #   which have the form d_<param>, where <param> is the solver name. 
  param.names <- substring(model.list[[model]]$input.var.names, 3)
  
  # # Set httk directory
  # this.func.dir <- utils::getSrcDirectory(get_input_param_timeseries)
  # httk.dir <- substr(this.func.dir, 1, nchar(this.func.dir)-2)
  
  # param.data.file stores the parameter values for the generated population
  if (is.null(input.param.dir)) {
    input.param.dir <- getwd()
  } else if (!file.exists(input.param.dir)) {
    stop("Specified directory for storing input parameters does not exist.")
  }
  
  storage.dir <- file.path(input.param.dir, "input_param_data_files")
  if (!file.exists(storage.dir)) {
    dir.create(storage.dir)
  }
  param.data.file <- file.path(storage.dir, paste("input_params_",
                                                  model,
                                                  "_",
                                                  chem.cas,
                                                  ".Rds",
                                                  sep = ""))
  
  # param.data.file <- paste(httk.dir,"/data/input_params_", model, "_", chem.cas,
  #                          ".Rds", sep="")
  if (!file.exists(param.data.file)){
    do.call(gen_input_params, list(model = model, 
                                   chem.cas = chem.cas,
                                   input.param.dir = input.param.dir))
  }
  pop.data <- readRDS(param.data.file)
  # Restrict based on user-supplied demographics
  pop.data <- pop.data[gender %in% gender &
                         weight_class %in% weight_category &
                         gfr_class %in% gfr_category &
                         reth %in% reths,]
  pop.age <- pop.data[["age_months"]] 
  
  timeseries.list <- list() # The list of two-column matrices to be passed to 
  # deSolve. The first column  of each matrix is a list of times, time.vals, and
  # the second column a list of values.
  
  # timeseries.list will ultimately be returned with time units of days; that
  # is the unit of time within the simulation. For ease in the nonparametric
  # regression, however, the time.vals will be units of months.
  
  time.vals <- seq(start.age+1, ceiling(start.age+days*12/365))
  
  for (param in param.names) {
    if (param %in% names(pop.data)) {
      pop.param <- pop.data[[param]]
    } else {
      do.call(param.gen.function, list(chem.cas.list=c(chem.cas)))
      pop.data <- readRDS(param.data.file)
      if (param %in% names(pop.data)) {
        pop.param <- pop.data[[param]]
      } else { # There is a problem generating the data for this compound
        stop(paste("Data for", param, "is not available.",
                   "Check", param.data.file, "data file."))
      }
    }
    
    # The non-parametric regression works as follows:
    #   First, the population values of the parameter of interest are ordered
    #   This ordering is used to re-order the relative weights of individuals
    #     in the population.
    #   The newly ordered weights form an empirical cumulative density function
    #   From this empirical CDF, the percentile or value for the parameter can
    #     be determined.
    
    # Relevant indices: indices of individuals that fall within bandwidth window
    rel.inds <- abs(pop.age - start.age) < bandwidth/2
    
    
    param.timeseries <- c()
    order.param <- order(pop.param[rel.inds])
    sorted.param <- pop.param[rel.inds][order.param]

    
    density.weights <- dbeta((pop.age[rel.inds] - start.age + 
                                bandwidth/2)/bandwidth,
                             shape1 = 3, shape2 = 3)#*pop.rep.weight
    ordered.weights <- density.weights[order.param]
    cumsum.ordered.weights <- cumsum(ordered.weights)/sum(ordered.weights)

    if (!is.null(initial.params[[model.list[[model]]$Rtosolvermap[[param]]]])) {
      # Use the Rtosolver map to extract initial value of parameter
      initial.param <- initial.params[[
        model.list[[model]]$Rtosolvermap[[param]]
      ]]

      if (initial.param >= max(pop.param[rel.inds])) {
        # Default value for which.max in this case is to return an index of 1,
        #  resulting in param.percentile = 0 when it should be 1.
        param.percentile <- 0.9999
      } else{
        param.percentile <- cumsum.ordered.weights[which.max(sorted.param > 
                                                             initial.param)]
      }
    } else {
      if (!is.null(initial.percentiles)) {
        param.percentile <- initial.percentiles[[
          model.list[[model]]$Rtosolvermap[[param]]
        ]]
        # Make sure percentile is between zero and one
        param.percentile <- min(1, max(0, param.percentile))
      } else { # No initial value provided; use median
        param.percentile <- 0.5
      }
      
      initial.param <- sorted.param[which.max(cumsum.ordered.weights > 
                                                param.percentile)]
      initial.params[[
        model.list[[model]]$Rtosolvermap[[param]]]] <- initial.param
      message(paste("Initial value of", param, "taken to be", initial.param))
    }
    
    for (time in time.vals) {
      rel.inds <- abs(pop.age - time) < bandwidth/2
      order.param <- order(pop.param[rel.inds])
      sorted.param <- pop.param[rel.inds][order.param]

      density.weights <- dbeta((pop.age[rel.inds] - time +
                                  bandwidth/2)/bandwidth,
                               shape1 = 3, shape2 = 3)#*pop.rep.weight
      ordered.weights <- density.weights[order.param]
      cumsum.ordered.weights <- cumsum(ordered.weights)/sum(ordered.weights)


      ## This may need additional safeguarding: if this line throws a
      # "replacement has length 0" error, the bandwidth-length interval centered
      # at the individual's age has moved outside the ages of the population.
      # Check to make sure the inputs_params_<model>_<CAS>.Rds file has a range
      # of ages for all start ages of interest.
      param.timeseries[time-start.age] <- tryCatch(
        { sorted.param[which.max(cumsum.ordered.weights > param.percentile)] },
        error = function(cond) {
          message(paste("Error in calculating timeseries for:", param))
          message("Here's the original error message:")
          message(conditionMessage(cond))
          message(paste("Errors are common when the population generated",
                        "by gen_input_params does not cover the age range",
                        "for the individual in the current simulation."))
          # Choose a return value in case of error
          -999
        }
      )
    }
    
    # Construct the time series: the time in hours in one column and the change
    # in parameter value in the other. (1 month = 365/12 days)
    param.timeseries <- param.timeseries - initial.param
    timeseries.list[[paste("d", param, sep="_")]] <- 
      rbind(c(0, 0), 
            matrix(c((time.vals - start.age)*365/12, param.timeseries), 
                   ncol = 2)
    )

  } # end loop over all parameters
  
  if (get.median.param.vals) {
    return(initial.params)
  } else {
    return(timeseries.list)
  }
}