#'Converts HTTK-Pop physiology into parameters relevant to the three
#' compartment steady-state model
#'
#'@param parameters.dt A data.table containing the physiological
#'  parameters as expected by HTTK (from \code{\link{httkpop_bio}}) and
#'  \code{Funbound.plasma} and \code{Clint} values (from
#'  \code{\link{draw_fup_clint}}).
#'
#'@return A data.table whose columns are the parameters of the HTTK model
#'  specified in \code{model}.
#'
#' @author Caroline Ring, John Wambaugh, and Greg Honda
#'
#'@references Ring, Caroline L., et al. "Identifying populations sensitive to 
#'environmental chemicals by simulating toxicokinetic variability." Environment 
#'International 106 (2017): 105-118
#'
#' @keywords httk-pop 3oompss  monte-carlo
#''
#' @import utils data.table
#'
#' @export convert_httkpop_3compss 
convert_httkpop_3compss <- function(
                             parameters.dt,
                             httkpop.dt,
                             ...)
{
  return(parameters.dt)
}