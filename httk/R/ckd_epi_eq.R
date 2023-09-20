#'CKD-EPI equation for GFR.
#'
#'Predict GFR from serum creatinine, gender, and age.
#'
#'From Levey AS, Stevens LA, Schmid CH, Zhang YL, Castro AF, Feldman HI, et al. A new
#'equation to estimate glomerular filtration rate. Ann Intern Med 2009;
#'150(9):604-612. doi:10.7326/0003-4819-150-9-200905050-00006
#'
#'@param scr Vector of serum creatinine values in mg/dL.
#'@param gender Vector of genders (either 'Male' or 'Female').
#'@param age_years Vector of ages in years.
#'@param reth Vector of races/ethnicities. Not used unless ckd_epi_race_coeff is TRUE.
#'@param ckd_epi_race_coeff Whether to use the "race coefficient" in the CKD-EPI equation. Default is FALSE.
#'  
#'@return Vector of GFR values in mL/min/1.73m^2.
#'
#'@keywords httk-pop
#'
#'@author Caroline Ring
#'
#' @references 
#' \insertRef{ring2017identifying}{httk} 
#'
#'@export ckd_epi_eq

ckd_epi_eq <- function(scr, gender, reth, age_years, ckd_epi_race_coeff = FALSE){
  kappa <- rep(NA, length(scr))
  kappa[gender=='Female'] <- 0.7
  kappa[gender=='Male'] <- 0.9
  alph <- rep(NA, length(scr))
  alph[gender=='Female'] <- -0.329
  alph[gender=='Male'] <- -0.411
  genfact <- rep(1,length(scr))
  genfact[gender=='Female'] <- 1.018
  rethfact <- rep(1,length(scr))
  #setting the "race" factor to 1 per Eneanya et al. 2019;
  #Anker et al. 2016; Peralta et al. 2010; Grubb et al. 2020
  if(isTRUE(ckd_epi_race_coeff)){
    rethfact[reth=='Non-Hispanic Black'] <- 1.159
  }
 
  
  gfr.est <- 141 * 
    pmin(scr/kappa, 1)^alph * 
    pmax(scr/kappa,1)^(-1.209) *
    0.993^age_years * genfact * rethfact
  
  return(gfr.est)
}
