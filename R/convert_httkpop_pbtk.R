#'Converts HTTK-Pop physiology into parameters relevant to the default
#' PBTK model
#'
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
#' @author Caroline Ring, John Wambaugh, and Greg Honda
#'
#'@references Ring, Caroline L., et al. "Identifying populations sensitive to 
#'environmental chemicals by simulating toxicokinetic variability." Environment 
#'International 106 (2017): 105-118
#'
#' @keywords httk-pop pbtk
#' @import utils
#' @export convert_httkpop_pbtk 
convert_httkpop_pbtk <- function(
                             parameters.dt,
                             httkpop.dt,
                             ...)
{
      calc_hep_params <- c(as.list(parameters.dt[, list(Clint,
                                                   Funbound.plasma,
                                                   Fhep.assay.correction,
                                                   million.cells.per.gliver,
                                                   BW,
                                                   Vliverc,
                                                   Qtotal.liverc)]),
                           liver.density=1.05,
                           Dn=0.17)

      #Call HTTK function to compute total hepatic clearance, using unscaled
      #hepatic model.
      parameters.dt[,
                  Clmetabolismc:=httk::calc_hep_clearance(hepatic.model="unscaled",
                                                              parameters=calc_hep_params,
                                                              suppress.messages=TRUE,
                                                              clint.pvalue.threshold=clint.pvalue.threshold)]

  return(parameters.dt)
}