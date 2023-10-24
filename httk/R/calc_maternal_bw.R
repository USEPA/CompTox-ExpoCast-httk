#' Calculate maternal body weight
#' 
#' This function initializes the parameters needed in the functions
#' solve_fetal_pbtk by calling solve_pbtk and adding additional parameters.
#' 
#'   BW <- params$pre_pregnant_BW + 
#'    params$BW_cubic_theta1 * tw + 
#'    params$BW_cubic_theta2 * tw^2 + 
#'    params$BW_cubic_theta3 * tw^3
#'
#' @param week Gestational week
#'
#' @return \item{BW}{Maternal Body Weight, kg.}
#'
#' @references 
#' \insertRef{kapraun2019empirical}{httk} 
#'
#' @keywords Parameter
#' 
#' @author John Wambaugh
#' 
#' @export calc_maternal_bw
calc_maternal_bw <- function(
  week = 12)
{
  return(calc_fetal_phys(week)$BW)
}