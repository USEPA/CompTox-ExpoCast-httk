#'Predict GFR in children.
#'
#'BSA-based equation from Johnson et al. 2006, Clin Pharmacokinet 45(9) 931-56. 
#'Used in Wetmore et al. 2014.
#'
#'@param BSA Vector of body surface areas in m^2.
#'  
#'@return Vector of GFRs in mL/min/1.73m^2.
#'
#'@keywords httk-pop
#'
#'@author Caroline Ring
#'
#' @references
#' \insertRef{ring2017identifying}{httk}
#' @export estimate_gfr_ped

estimate_gfr_ped <- function(BSA){
  #BSA-based equation from Johnson et al. 2006, Clin Pharmacokinet 45(9) 931-56.
  #Used in Wetmore et al. 2014.
  #Note: Provide BSA in m^2
  gfr <- (-6.1604*BSA^2) + (99.054*BSA) - 17.74 #This gfr is in mL/min
  #For consistency with adult numbers, normalize to mL/min/1.73m^2 BSA
  gfr <- gfr/(BSA/1.73)
  return(gfr) 
}
