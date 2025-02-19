#' Calculate the fractional contributions to total clearance
#' 
#' Steady-state clearance is a function of multiple processes. For example,
#' meabolism in the liver and glomerular filtration in the kidney. This function
#' takes a list of parameters potentially impacting total clearance and
#' iteratively sets all but one of the paramters to zero. This allows
#' calculation of the fration of total clearance driven by that parameter.
#' 
#' @param fraction.params A vector of character strings identifying the
#' parameters whose fractional contributions are to be calculated. Defaults to
#' 'Qfgr' and 'Qtotal.liverc'.
#'
#' @param model Model used in calculation, for example'pbtk' for the multiple compartment
#'  model,'3compartment' for the three compartment model, and '1compartment' for
#'  the one compartment model. Defaults to '3compartmentss'.
#'
#' @param chem.cas Chemical Abstract Services Registry Number (CAS-RN) -- if
#'  parameters is not specified then the chemical must be identified by either
#'  CAS, name, or DTXISD
#' 
#' @param chem.name Chemical name (spaces and capitalization ignored) --  if
#'  parameters is not specified then the chemical must be identified by either
#'  CAS, name, or DTXISD
#' 
#' @param dtxsid EPA's 'DSSTox Structure ID (\url{https://comptox.epa.gov/dashboard})  
#'  -- if parameters is not specified then the chemical must be identified by 
#' either CAS, name, or DTXSIDs
#' 
#' @param parameters Parameters from the appropriate parameterization function
#' for the model indicated by argument model
#'
#' @param suppress.messages Whether or not the output message is suppressed.
#'
#' @param restrictive.clearance Protein binding not taken into account (set to 1)
#'  in liver clearance if FALSE.
#'
#' @param species Species desired (either "Rat", "Rabbit", "Dog", "Mouse", or
#' default "Human").
#' 
#' @param default.to.human (Logical) Substitutes missing rat values with
#' human values if TRUE. (Not applicable for `calc_fabs.oral`.)
#' (Defaults to `FALSE`.)
#'
#' @param parameterize.args Named list of any additional arguments passed to
#'  model parameterization function (other than the already-named arguments).
#'  Default `list()` to pass no additional arguments.
#'
#' @return A numeric fraction unpbound in plasma between zero and one
#'
#' @author John Wambaugh 
#'
#' @references 
#' 
#' @keywords parameters
#'
#' @export calc_clearance_frac
#'
calc_clearance_frac <- function(
                                fraction.params=c("Qtotal.liverc","Qgfrc"),
                                chem.cas=NULL,
                                chem.name=NULL,
                                dtxsid = NULL,
                                parameters=NULL,
                                species="human",
                                default.to.human = FALSE,
                                suppress.messages=FALSE,
                                model="3compartmentss",
                                restrictive.clearance = TRUE,
                                parameterize.args = list(),
                                analytic_css.args = list()
                                )
{
  if (is.null(model)) stop("Model must be specified.")
# We need to know model-specific information (from modelinfo_[MODEL].R]) 
  model <- tolower(model)
  if (!(model %in% names(model.list)))            
  {
    stop(paste("Model",model,"not available. Please select from:",
      paste(names(model.list),collapse=", ")))
  } 
  parameterize_function <- model.list[[model]]$parameterize.func
    
# We need to describe the chemical to be simulated one way or another:
  if (is.null(chem.cas) & 
      is.null(chem.name) & 
      is.null(dtxsid) &
      is.null(parameters)) 
    stop('parameters, chem.name, chem.cas, or dtxsid must be specified.')
  
  ### MODEL PARAMETERS FOR R

# Make sure we have all the parameters necessary to describe the chemical (we don't
# necessarily need all parameters associated with a given model to do this:)
  if (is.null(parameters))
  {
# Look up the chemical name/CAS/dtxsid, depending on what was provided:
    out <- get_chem_id(
            chem.cas=chem.cas,
            chem.name=chem.name,
            dtxsid=dtxsid)
    chem.cas <- out$chem.cas
    chem.name <- out$chem.name                                
    dtxsid <- out$dtxsid  

  # pass chemical information plus formal argument parameterize.args to the
  # parameterization function specified by the appropriate modelinfo file:
    parameters <- do.call(what=parameterize_function, 
      args=purrr::compact(c(list(
        chem.cas=chem.cas,
        chem.name=chem.name,
        dtxsid=dtxsid,
        species=species,
        suppress.messages=suppress.messages),
      parameterize.args)))
 
  } else {
    model_param_names <- model.list[[model]]$param.names 
    if (!all(model_param_names %in% names(parameters)))
    {
      stop(paste("Missing parameters:",
        paste(model_param_names[which(!model_param_names %in% 
        names(parameters))], collapse=', '),
        ". Use parameters from ", parameterize_function, ".", sep="")) 
    }
  }
  
  # Check that all parameters in fraction.params are actual parameters:
  if (any(!(fraction.params %in% names(parameters))))
  {
      stop(paste("Parameters:",
        paste(fraction.params[!(fraction.params %in% names(parameters))], 
              collapse=', '),
        " are not model parameters for model ", model,".", sep="")) 
  }
    
  # We will use Css = f_bio / Cl to get Cl:
  
  # Get total Css
  Clearance.total <- do.call(calc_total_clearance,
                     args=purrr::compact(c(list(
                                                parameters =
                                                  parameters,
                                                suppress.messages =
                                                  suppress.messages,
                                                restrictive.clearance =
                                                  restrictive.clearance
                                                ),
                                            analytic_css.args)))
  
  clearance.fractions <- list()
  # Now loop over fraction params:
  for (this.param in fraction.params)
  {
    these.params <- parameters
# Set other parameters to zero:
    for (other.param in fraction.params)
      if (other.param != this.param)
        these.params[[other.param]] <- 0
    
    this.clearance <- do.call(calc_total_clearance,
                     args=purrr::compact(c(list(
                                                parameters =
                                                  these.params,
                                                suppress.messages =
                                                  suppress.messages,
                                                restrictive.clearance =
                                                  restrictive.clearance
                                                ),
                                            analytic_css.args)))
                                            
    clearance.fractions[this.param] <- this.clearance / Clearance.total
  }
  
# Cannot guarantee arbitrary precision:
  Css <- set_httk_precision(clearance.fractions)
  
  return(clearance.fractions)
}