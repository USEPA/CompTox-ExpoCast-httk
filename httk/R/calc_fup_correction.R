#' Calculate the correction for lipid binding in plasma binding assay
#' 
#' Pearce et al. (2017) compared predicted and observed partition coefficients
#' for a range of compounds. Based on observed discrepancies they 
#' argued that there should be greater lipid binding in plasma (in vivo) 
#' for some lipophilic un-ionized compounds than is observed in vitro.
#' Following Schmitt (2008), the lipid binding in tissue is assumed to follow
#' the distribution coefficient 
#' \deqn{D = K_{nL} = P_{ow}*(F_{neutral} + \alpha*F_{charged})}
#' and the fractional volume of lipid in
#' plasma. The error in \eqn{K_{tissue:plasma}} prediction for these compounds
#' has been hypothesized to be lower due to insufficient lipid
#' binding in the in vitro protein binding assay (Poulin and Haddad (2012)).
#' This deficiency in the assay leads to a much higher
#' unbound concentration relative to the bound concentration
#' for lipophilic compounds and thus an overestimation of \eqn{f_up}
#' and \eqn{K_{tissue:plasma}}. To adjust for this we recalculated the plasma:water
#' partition coefficient (\eqn{1/f_{up}}) to account for neutral lipid
#' binding in plasma, assuming that some lipid is absent from
#' the in vitro assay:
#' \deqn{f^{corrected}_{up} = \frac{1}{K_{nL}^{pl}*F_{lipid} + \frac{1}{f^{in vitro}_{up}}}}
#' 
#' @param chem.cas Chemical Abstract Services Registry Number (CAS-RN) -- if
#'  parameters is not specified then the chemical must be identified by either
#'  CAS, name, or DTXISD
#' 
#' @param chem.name Chemical name (spaces and capitalization ignored) --  if
#'  parameters is not specified then the chemical must be identified by either
#'  CAS, name, or DTXISD
#' 
#' @param dtxsid EPA's 'DSSTox Structure ID (\url{https://comptox.epa.gov/dashboard})  
#'  -- if parameters is not specified then the chemical must be identified by 
#' either CAS, name, or DTXSIDs
#' 
#' @param parameters Parameters from the appropriate parameterization function
#' for the model indicated by argument model
#' 
#' @param Vr Rratio of cell volume to incubation volume. Default is taken from 
#  Wetmore et al. (2015)
#' 
#' @param pH pH of the incupation medium.
#'
#' @return A numeric fraction unpbound in plasma between zero and one
#'
#' @author John Wambaugh 
#'
#' @references 
#' Pearce, Robert G., et al. "Evaluation and calibration of high-throughput 
#' predictions of chemical distribution to tissues." Journal of pharmacokinetics 
#' and pharmacodynamics 44.6 (2017): 549-565.
#' 
#' Poulin, Patrick, and Sami Haddad. "Advancing prediction of tissue 
#' distribution and volume of distribution of highly lipophilic compounds from 
#' a simplified tissue-composition-based model as a mechanistic animal 
#' alternative method." Journal of pharmaceutical sciences 101.6 (2012): 
#' 2250-2261.
#'
#' Schmitt, Walter. "General approach for the calculation of 
#' tissue to plasma partition coefficients." Toxicology in Vitro 22.2 (2008): 
#' 457-467.
#'
#' @keywords in-vitro
#'
#' @seealso \code{\link{adjust_fup}}
#'
#' @export calc_fup_correction
#'
calc_fup_correction <- function(
                 chem.cas=NULL,
                 chem.name=NULL,
                 dtxsid = NULL,
                 parameters=NULL,
                 Flipid = NULL,
                 plasma.pH = 7.4
                 ) 
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
  
  if (!all(c("Pow","pKa_Donor","pKa_Accept") 
    %in% names(parameters))) 
    stop("Missing parameters needed in calc_hep_fu.")            
                 
  Pow <- parameters$Pow
  pKa_Donor <- parameters$pKa_Donor
  pKa_Accept <- parameters$pKa_Accept
  if (!is_base(pH=pH, pKa_Donor=pKa_Donor, pKa_Accept=pKa_Accept))
  {
    logPD <- log10(calc_dow(
               Pow, 
               pH=pH,
               pKa_Donor=pKa_Donor,
               pKa_Accept=pKa_Accept)) 
  } else logPD <- log10(Pow)
  
# Calculate Pearce (2017) in vitro plasma binding correction:
  if (force.human.fup) 
    Flipid <- subset(
                physiology.data,
                Parameter == 'Plasma Effective Neutral Lipid Volume Fraction')[,
                  which(colnames(physiology.data) == 'Human')]
  else Flipid <- subset(
                   physiology.data,
                   Parameter=='Plasma Effective Neutral Lipid Volume Fraction')[,
                     which(tolower(colnames(physiology.data)) == tolower(species))]
  if (!is.null(parameters))
    if ("Flipid" %in% names(parameters))
      Flipid <- parameters$Flipid
        
  
  ion <- calc_ionization(
           pH=plasma.pH,
           pKa_Donor=pKa_Donor,
           pKa_Accept=pKa_Accept)
  dow <- Pow * (ion$fraction_neutral + 
                alpha * ion$fraction_charged + 
                ion$fraction_zwitter)
  fup.corrected <- 1 / ((dow) * Flipid + 1 / fup)
  fup.correction <- fup.corrected/fup
  
  return(set_httk_precision(fup.correction))
}
