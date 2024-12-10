#' Calculate the elimination rate for a one compartment model
#' 
#' This function calculates an elimination rate from the three compartment
#' steady state model where elimination is entirely due to metablism by the
#' liver and glomerular filtration in the kidneys.
#' 
#' Elimination rate calculated by dividing the total clearance (using the
#' default -stirred hepatic model) by the volume of distribution. When
#' species is specified as rabbit, dog, or mouse, the function uses the
#' appropriate physiological data(volumes and flows) but substitues human
#' fraction unbound, partition coefficients, and intrinsic hepatic clearance.
#' 
#' @param chem.cas Either the cas number or the chemical name must be
#' specified. 
#' 
#' @param chem.name Either the chemical name or the cas number must be
#' specified. 
#' 
#' @param dtxsid EPA's 'DSSTox Structure ID (\url{https://comptox.epa.gov/dashboard})  
#' the chemical must be identified by either CAS, name, or DTXSIDs
#' 
#' @param parameters Chemical parameters from parameterize_steadystate or
#' 1compartment function, overrides chem.name and chem.cas.
#'
#' @param model The model used to calculate total clearance (defaults to "3compartmentss")
#'
#' @param species Species desired (either "Rat", "Rabbit", "Dog", "Mouse", or
#' default "Human").
#' 
#' @param suppress.messages Whether or not the output message is suppressed.
#' 
#' @param default.to.human Substitutes missing animal values with human values
#' if true.
#' 
#' @param restrictive.clearance In calculating elimination rate, protein
#' binding is not taken into account (set to 1) in liver clearance if FALSE.
#' 
#' @param adjusted.Funbound.plasma Uses adjusted Funbound.plasma when set to
#' TRUE along with partition coefficients calculated with this value.
#' 
#' @param adjusted.Clint Uses Kilford et al. (2008) hepatocyte incubation
#' binding adjustment for Clint when set to TRUE (Default).
#' 
#' @param regression Whether or not to use the regressions in calculating
#' partition coefficients.
#' 
#' @param well.stirred.correction Uses correction in calculation of hepatic
#' clearance for -stirred model if TRUE.  This assumes clearance relative
#' to amount unbound in whole blood instead of plasma, but converted to use
#' with plasma concentration.
#' 
#' @param clint.pvalue.threshold Hepatic clearance for chemicals where the in
#' vitro clearance assay result has a p-values greater than the threshold are
#' set to zero.
#' 
#' @param minimum.Funbound.plasma Monte Carlo draws less than this value are set 
#' equal to this value (default is 0.0001 -- half the lowest measured Fup in our
#' dataset).
#' 
#' @param class.exclude Exclude chemical classes identified as outside of 
#' domain of applicability by relevant modelinfo_[MODEL] file (default TRUE).
#' 
#' @return \item{Elimination rate}{Units of 1/h.}
#' 
#' @seealso \code{\link{calc_total_clearance}} for calculation of total clearance
#' 
#' @seealso \code{\link{calc_vdist}} for calculation of volume of distribution
#' 
#' @author John Wambaugh
#' 
#' @references 
#' Schmitt, Walter. "General approach for the calculation of tissue 
#' to plasma partition coefficients." Toxicology in vitro 22.2 (2008): 457-467.
#'
#' \insertRef{dawson2021qsar}{httk} 
#'
#' \insertRef{kilford2008hepatocellular}{httk} 
#'
#' @keywords Parameter  1compartment
#' 
#' @examples
#' 
#' calc_elimination_rate(chem.name="Bisphenol A")
#'\dontrun{
#' calc_elimination_rate(chem.name="Bisphenol A",species="Rat")
#' calc_elimination_rate(chem.cas="80-05-7")
#'}
#'
#' @export calc_elimination_rate
calc_elimination_rate <- function(chem.cas=NULL,
                                  chem.name=NULL,
                                  dtxsid=NULL,
                                  parameters=NULL,
                                  model="3compartmentss",
                                  species="Human",
                                  suppress.messages=TRUE,
                                  default.to.human=FALSE,
                                  class.exclude=TRUE,
                                  restrictive.clearance=TRUE,
                                  adjusted.Funbound.plasma=TRUE,
                                  adjusted.Clint=TRUE,
                                  regression=TRUE,
                                  well.stirred.correction=TRUE,
                                  clint.pvalue.threshold=0.05,
                                  minimum.Funbound.plasma=0.0001)
{
  if ('Vdist' %in% names(parameters))
  {
    Vd <- parameters[['Vdist']]
  } else {
      Vd <- calc_vdist(chem.cas=chem.cas,
                       chem.name=chem.name,
                       dtxsid=dtxsid,
                       parameters=parameters,
                       species=species,
                       suppress.messages=suppress.messages,
                       default.to.human=default.to.human,
                       class.exclude=class.exclude,
                       adjusted.Funbound.plasma=adjusted.Funbound.plasma,
                       regression=regression,
                       minimum.Funbound.plasma=minimum.Funbound.plasma) 
  } # L/kgBW 

  clearance <- calc_total_clearance(chem.name=chem.name,
                                    chem.cas=chem.cas,
                                    dtxsid=dtxsid,
                                    species=species,
                                    parameters=parameters,
                                    model=model,
                                    suppress.messages=suppress.messages,
                                    default.to.human=default.to.human,
                                    class.exclude=class.exclude,
                                    restrictive.clearance=restrictive.clearance,
                                    adjusted.Funbound.plasma=adjusted.Funbound.plasma,
                                    clint.pvalue.threshold=clint.pvalue.threshold,
                                    well.stirred.correction=well.stirred.correction,
                                    minimum.Funbound.plasma=minimum.Funbound.plasma) #L/h/kgBW


  if (!suppress.messages) cat(paste(
      toupper(substr(species,1,1)),
      substr(species,2,nchar(species)),sep=''),
      "elimination rate returned in units of 1/h.\n")

  return(set_httk_precision(as.numeric(clearance/Vd)))
}
