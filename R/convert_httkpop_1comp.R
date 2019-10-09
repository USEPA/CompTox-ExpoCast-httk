#'Converts HTTK-Pop physiology into parameters relevant to the one
#' compartment model
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
#' @keywords httk-pop 1compartment
#' @import utils
#' @export convert_httkpop_1comp
convert_httkpop_1comp <- function(
                             parameters.dt,
                             httkpop.dt,
                             ...)
{
      #for 1-compartment model, don't need to compute total hepatic clearance,
      #but do need to compute volume of distribution and elimination rate.

      #HTTK contains a function to compute volume of distribution, but it pulls
      #Funbound.plasma from its table of default values, meaning we can't give
      #that function our vector of individual Funbound.plasma values. So
      #instead, I've re-implemented the Vdist equation here.

      #To compute volume of distribution, need to get volume of red blood cells.
      #Can compute that from plasma volume and hematocrit.

      parameters.dt[, RBC.vol:=plasma.vol/
                    (1 - hematocrit)*
                    hematocrit]
      #Compute Vdist, volume of distribution
      parameters.dt[,Vdist:=plasma.vol +
                    RBC.vol*
                    PCs[["Krbc2pu"]]*
                    Funbound.plasma+
                    Krest2pu*
                    vol.restc*
                    Funbound.plasma]
      #Compute kelim: Elimination rate, units of 1/h. First make a list of the
      #parameters that HTTK uses to calculate kelim. Each list element will be a
      #vector of the values for each individual.
      calc_elim_params <- c(as.list(parameters.dt[,
                                                list(Vdist,
                                                  Clint,
                                                  Funbound.plasma,
                                                  Qtotal.liverc,
                                                  Qgfrc,
                                                  BW,
                                                  million.cells.per.gliver,
                                                  Rblood2plasma,
                                                  Vliverc,
                                                  Fhep.assay.correction,
                                                  liver.density)]))
      #Call HTTK function to calculate total elimination rate. This one is OK
      #because it uses the vector of Funbound.plasma that we give it.
      ke <- httk::calc_elimination_rate(parameters=calc_elim_params,
                                        chem.cas=this.chem,
                                        suppress.messages=TRUE,
                                        adjusted.Funbound.plasma=adjusted.Funbound.plasma,regression=regression,
                                        well.stirred.correction=well.stirred.correction,
                                        restrictive.clearance=restrictive.clearance,
                                        clint.pvalue.threshold=clint.pvalue.threshold)
      #Add kelim to the population data.table.
      parameters.dt[, kelim:=ke]

  return(parameters.dt)
}