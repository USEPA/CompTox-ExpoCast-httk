#' Calculates the half-life for a one compartment model.
#' 
#' This function calculates the half life from the three compartment
#' steady state model where elimination is entirely due to metabolism by the
#' liver and glomerular filtration in the kidneys.
#' 
#' Half life is calculated by dividing the natural-log of 2 by the elimination
#' rate from the one compartment model.
#' 
#' @param chem.cas Either the cas number or the chemical name must be specified.
#' @param chem.name Either the chemical name or the cas number must be specified.
#' @param dtxsid EPA's 'DSSTox Structure ID (\url{https://comptox.epa.gov/dashboard})  
#' the chemical must be identified by either CAS, name, or DTXSIDs
#' @param parameters Chemical parameters from parameterize_steadystate or
#' 1compartment function, overrides chem.name and chem.cas.
#' @param species Species desired (either "Rat", "Rabbit", "Dog", "Mouse", or
#' default "Human").
#' @param suppress.messages Whether or not the output message is suppressed.
#' @param default.to.human Substitutes missing animal values with human values
#' if true.
#' @param restrictive.clearance In calculating elimination rate, protein
#' binding is not taken into account (set to 1) in liver clearance if FALSE.
#' @param adjusted.Funbound.plasma Uses adjusted Funbound.plasma when set to
#' TRUE along with partition coefficients calculated with this value.
#' @param regression Whether or not to use the regressions in calculating
#' partition coefficients.
#' @param well.stirred.correction Uses correction in calculation of hepatic
#' clearance for -stirred model if TRUE.  This assumes clearance relative
#' to amount unbound in whole blood instead of plasma, but converted to use
#' with plasma concentration.
#' @param clint.pvalue.threshold Hepatic clearance for chemicals where the in
#' vitro clearance assay result has a p-values greater than the threshold are
#' set to zero.
#' @param minimum.Funbound.plasma Monte Carlo draws less than this value are set 
#' equal to this value (default is 0.0001 -- half the lowest measured Fup in our
#' dataset).
#' 
#' @return \item{Half life}{Units of h.}
#' @seealso [calc_elimination_rate()] for the elimination rate calculation
#' 
#' @author Sarah E. Davidson
#' 
#' @keywords Parameter  1compartment
#' @examples
#' 
#' calc_half_life(chem.name="Bisphenol A")
#' calc_half_life(chem.name="Bisphenol A",species="Rat")
#' calc_half_life(chem.cas="80-05-7")
#' 
#' @export calc_half_life
calc_half_life <- function (chem.cas = NULL, chem.name = NULL, dtxsid = NULL,
                            parameters = NULL, species = "Human",
                            suppress.messages = FALSE,
                            default.to.human = FALSE, restrictive.clearance = TRUE,
                            adjusted.Funbound.plasma = TRUE,
                            regression = TRUE, well.stirred.correction = TRUE,
                            clint.pvalue.threshold = 0.05,
                            minimum.Funbound.plasma = 1e-04){
  
  elim_rate <- calc_elimination_rate(chem.cas = chem.cas,
                                     chem.name = chem.name,
                                     dtxsid = dtxsid,
                                     parameters = parameters,
                                     species = species,
                                     suppress.messages = suppress.messages,
                                     default.to.human = default.to.human,
                                     restrictive.clearance = restrictive.clearance,
                                     adjusted.Funbound.plasma = adjusted.Funbound.plasma,
                                     regression = regression,
                                     well.stirred.correction = well.stirred.correction,
                                     clint.pvalue.threshold = clint.pvalue.threshold,
                                     minimum.Funbound.plasma = minimum.Funbound.plasma)
  
  half_life <- log(2)/elim_rate # calculate the elimination rate
  
  if(!suppress.messages)cat(paste(toupper(substr(species,1,1)),substr(species,2,nchar(species)),sep=''),"half life returned in units of h.\n")
  
  return(set_httk_precision(as.numeric(half_life)))
}
