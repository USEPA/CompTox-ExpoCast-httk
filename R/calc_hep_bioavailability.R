#' Calculate first pass metabolism
#'
#' For models that don't described first pass blood flow from the gut, need to
#' cacluate a hepatic bioavailability, that is, the fraction of chemical 
#' systemically available after metabolism during the first pass through the 
#' liver (Rowland, 1973).
  
#' @param chem.cas Chemical Abstract Services Registry Number (CAS-RN) -- if
#'  parameters is not specified then the chemical must be identified by either
#'  CAS, name, or DTXISD
#' @param chem.name Chemical name (spaces and capitalization ignored) --  if
#'  parameters is not specified then the chemical must be identified by either
#'  CAS, name, or DTXISD
#' @param dtxsid EPA's 'DSSTox Structure ID (http://comptox.epa.gov/dashboard)  
#'  -- if parameters is not specified then the chemical must be identified by 
#' either CAS, name, or DTXSIDs
#' @param parameters Parameters from the appropriate parameterization function
#' for the model indicated by argument model
#'@param httk.pop.biomets A data.table containing the physiological
#'  parameters as expected by HTTK (from \code{\link{httkpop_bio}}) and
#'  \code{Funbound.plasma} and \code{Clint} values (from
#'  \code{\link{draw_fup_clint}}).
#'@param model Which HTTK model to use. 
#' @param adjusted.Funbound.plasma Uses adjusted Funbound.plasma when set to TRUE.
#' @param regression Whether or not to use the regressions in calculating partition 
#' coefficients.
#' @param well.stirred.correction Uses correction in calculation of hepatic clearance 
#' for well-stirred model if TRUE for hepatic.model well-stirred. This assumes 
#' clearance re/lative to amount unbound in whole blood instead of plasma, but 
#' converted to use with plasma concentration.
#' @param restrictive.clearance Protein binding not taken into account (set to 1) in 
#' liver clearance if FALSE.
#' @param concentration Blood, plasma, or tissue concentration. 
#' @param clint.pvalue.threshold Hepatic clearance for chemicals where the in vitro 
#' clearance assay result has a p-values greater than the threshold are set to zero.
#'
#'@return A data.table whose columns are the parameters of the HTTK model
#'  specified in \code{model}.
#'
#' @author John Wambaugh
#'
#'@references Rowland, Malcolm, Leslie Z. Benet, and Garry G. Graham. 
#'"Clearance concepts in pharmacokinetics." Journal of pharmacokinetics and 
#'biopharmaceutics 1.2 (1973): 123-136.
#'
#' @keywords physiology 
#'
#' @import utils
#'                                                                 
#' @export calc_hep_bioavailability 
#'
calc_hep_bioavailability <- function(
                         chem.cas=NULL,
                         chem.name=NULL,
                         dtxsid = NULL,
                         parameters=NULL,
                         restrictive.clearance=T)
{
# We need to describe the chemical to be simulated one way or another:
  if (is.null(chem.cas) & 
      is.null(chem.name) & 
      is.null(dtxsid) &
      is.null(parameters)) 
    stop('Parameters, chem.name, chem.cas, or dtxsid must be specified.')

  if (is.null(parameters))
  {
    parameters <- parameterize_pbtk(
                    chem.cas=chem.cas,
                    chem.name=chem.name,
                    dtxsid=dtxsid)
  }
  
  if (!all(c("Qtotal.liverc","Funbound.plasma","Clmetabolismc","Rblood2plasma") 
    %in% names(parameters))) 
    stop("Missing needed parameters in calc_hepatic_bioavailability.")

  if (restrictive.clearance) return(parameters$Qtotal.liverc / 
    (parameters$Qtotal.liverc + 
    parameters$Funbound.plasma * 
      parameters$Clmetabolismc / 
      parameters$Rblood2plasma))
  else return(parameters$Qtotal.liverc / 
    (parameters$Qtotal.liverc + 
      parameters$Clmetabolismc / 
      parameters$Rblood2plasma))
}