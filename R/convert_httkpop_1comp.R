#'Converts HTTK-Pop physiology into parameters relevant to the one
#' compartment model
#'
#' @param parameters.dt Data table returned by \code{\link{create_mc_samples}}
#' @param httkpop.dt Data table returned by \code{\link{httkpop_generate}}
#' @param ... Additional arguments passed to \code{\link{propagate_invitrouv_1comp}}
#'
#' @return A data.table whose columns are the parameters of the HTTK model
#'  specified in \code{model}.
#'
#' @author Caroline Ring, John Wambaugh, and Greg Honda
#'
#' @references Ring, Caroline L., et al. "Identifying populations sensitive to 
#'environmental chemicals by simulating toxicokinetic variability." Environment 
#'International 106 (2017): 105-118
#'
#' @keywords httk-pop 1compartment
convert_httkpop_1comp <- function(
                             parameters.dt,
                             httkpop.dt,
                             ...)
{
  return(propagate_invitrouv_1comp(parameters.dt,...))
}