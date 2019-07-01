#' Return the assumptions used in Honda et al. 2019
#' 
#' This function returns four of the better performing sets of assumptions evaluated in Honda et al. 2019 
#' (https://doi.org/10.1371/journal.pone.0217564).These include four different combinations of hepatic clearance assumption, in vivo bioactivity assumption, 
#' and relevant tissue assumption. Generally, this function is not called directly by the user, but instead
#' called by setting the IVIVE option in calc_mc_oral_equiv, calc_mc_css, and calc_analytic functions. Currently, these IVIVE option 
#' is not implemented the solve_1comp etc. functions.
#' 
#' @param method This is set to one of "Honda1", "Honda2", "Honda3", or "Honda4".
#' @param tissue This is only relevant to "Honda4" and indicates the relevant tissue compartment.
#' @return A list of tissue, bioactive.free.invivo, and restrictive.clearance assumptions.
#' 
#' @details
#' "Honda1" - tissue = NULL, restrictive.clearance = TRUE, bioactive.free.invivo = TRUE
#'       This assumption assumes restrictive hepatic clearance, and treats the free concentration in plasma as 
#'       the bioactive concentration in vivo. This option must be used in combination with the concentration in vitro 
#'       predicted by armitage_eval(), otherwise the result will be the same as "Honda2". This option corresponds to the result 
#'       in Figure 8 panel c) restrictive, mean free plasma conc., Armitage in Honda et al. 2019.
#' "Honda2" - tissue = NULL, restrictive.clearance = TRUE, bioactive.free.invivo = TRUE
#'       This assumption assumes restrictive hepatic clearance, and treats the free concentration in plasma as 
#'       the bioactive concentration in vivo. This option corresponds to the result 
#'       in Figure 8 panel b) restrictive, mean free plasma conc. in Honda et al. 2019.
#' "Honda3" - tissue = NULL, restrictive.clearance = TRUE, bioactive.free.invivo = TRUE
#'       This assumption assumes restrictive hepatic clearance, and treats the free concentration in plasma as 
#'       the bioactive concentration in vivo. This option corresponds to the result 
#'       in Figure 8 panel a) restrictive, mean total plasma conc. in Honda et al. 2019.
#' "Honda4" - tissue = tissue, restrictive.clearance = FALSE, bioactive.free.invivo = TRUE
#'       This assumption assumes restrictive hepatic clearance, and treats the free concentration in plasma as 
#'       the bioactive concentration in vivo. The input tissue should be relevant to the in vitro assay endpoint used as input or that
#'       the result is being compared to. This option corresponds to the result 
#'       in Figure 8 panel d) nonrestrictive, mean tissue conc. in Honda et al. 2019.
#'       
#' @examples
#' honda.ivive(method = "Honda1", tissue = NULL)
#' 
#' @author Greg Honda and John Wambaugh
#' 
#' @keywords Solve
#' 
#' @references Honda, Gregory S., et al. "Using the Concordance of In Vitro and 
#' In Vivo Data to Evaluate Extrapolation Assumptions." 2019. PLoS ONE 14(5): e0217564.
#' 
#' @export honda.ivive
#' 
#' 
honda.ivive <- function(method="Honda1",tissue="liver"){
  #R CMD CHECK throws notes about "no visible binding for global variable", for
  #each time a data.table column name is used without quotes. To appease R CMD
  #CHECK, a variable has to be created for each of these column names and set to
  #NULL. Note that within the data.table, these variables will not be NULL! Yes,
  #this is pointless and annoying.
  restrictive.clearance<-bioactive.free.invivo<-concentration<-NULL
  #End R CMD CHECK appeasement.

  if (is.null(tissue)) tissue <- "liver"

  if (tolower(method)==tolower("Honda1"))
  {
    tissue <- NULL
    restrictive.clearance <- T
    bioactive.free.invivo <- T
    concentration <- "plasma"
    
    warning("Argument method =\"Honda1\" uses plasma concentration, restrictive clearance, and treats the unbound invivo concentration as bioactive.
            For IVIVE, any input nominal concentration in vitro should be converted to cfree.invitro using armitage_eval(),
            otherwise performance will be the same as \"Honda2\". Use show_honda.ivive() to print summary of Honda et al. 2019 results.")
  } else if (tolower(method)==tolower("Honda2"))
  {
    tissue <- NULL
    restrictive.clearance <- T
    bioactive.free.invivo <- T
    concentration <- "plasma"
    
    warning("Argument method =\"Honda2\" uses plasma concentration, restrictive clearance, and treats the unbound concentration as bioactive.")
  } else if (tolower(method)==tolower("Honda3"))
  {
    tissue <- NULL
    restrictive.clearance <- T
    bioactive.free.invivo <- F
    concentration <- "plasma"
    
    warning("Argument method =\"Honda3\" uses plasma concentration, restrictive clearance, and treats the total concentration as bioactive.
            This is equivalent to the default httk assumptions.")
    
  } else if (tolower(method)==tolower("Honda4"))
  {
    tissue <- tolower(tissue)
    restrictive.clearance <- F
    bioactive.free.invivo <- F
    concentration <- "tissue"
    
    warning(paste("Argument method =\"Honda4\" uses target tissue (",tissue,") concentration, non-restrictive clearance, and treats the total concentration as bioactive.",sep=""))
    
  }else stop("Only four sets of IVIVE assumptions that performed well in Honda et al. (2019) are included: \"Honda1\" through \"Honda4\". The use of max (peak) concentration can not be currently be calculated with calc_analytic_css.")

  return(list("tissue" = tissue,
              "bioactive.free.invivo" = bioactive.free.invivo,
              "restrictive.clearance" = restrictive.clearance,
              "concentration" = concentration))
}