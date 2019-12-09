#' Back-calculates the Red Blood Cell to Unbound Plasma Partition Coefficient
#' 
#' Given and observed ratio of chemial concentration in blood to plasma, this
#' function calculates a Red Blood Cell to unbound plasma (Krbc2pu) partition
#' coefficient that would be consistent with that observation.
#' 
#' @param chem.name Either the chemical name or the CAS number must be
#' specified.
#' @param chem.cas Either the CAS number or the chemical name must be
#' specified.
#' @param params Parameters from parameterize_schmitt.
#' @param hematocrit Overwrites default hematocrit value in calculating
#' Rblood2plasma.
#' @param default.to.human Substitutes missing animal values with human values
#' if true.
#' @param species Species desired (either "Rat", "Rabbit", "Dog", "Mouse", or
#' default "Human").
#' @param adjusted.Funbound.plasma Whether or not to use Funbound.plasma
#' adjustment.
#' @param suppress.messages Determine whether to display certain usage
#' feedback.
#'
#' @author John Wambaugh and Robert Pearce
#'
#' @references 
#' Pearce, Robert G., et al. "Evaluation and calibration of high-throughput 
#' predictions of chemical distribution to tissues." Journal of 
#' pharmacokinetics and pharmacodynamics 44.6 (2017): 549-565.
#'
#' Ruark, Christopher D., et al. "Predicting passive and active tissue: plasma 
#' partition coefficients: interindividual and interspecies variability." 
#' Journal of pharmaceutical sciences 103.7 (2014): 2189-2198.
#'
#' @keywords Parameter
#'
#' @export calc_krbc2pu
calc_krbc2pu <- function(
                         Rb2p,
                         Funbound.plasma,
                         hematocrit=NULL,
                         default.to.human=F,
                         species="Human",
                         suppress.messages=T)
{
  physiology.data <- physiology.data

  if (!(species %in% colnames(physiology.data)))
  {
    if (toupper(species) %in% toupper(colnames(physiology.data)))
    {
      phys.species <- colnames(physiology.data)[
        toupper(colnames(physiology.data))==toupper(species)]
    } else stop(paste("Physiological PK data for",species,"not found."))
  } else phys.species <- species

  if (is.null(hematocrit)) 
  {
    hematocrit <- 
      physiology.data[physiology.data$Parameter=="Hematocrit",phys.species]
  }
  
  Krbp2pu <- (Rb2p - 1 + hematocrit)  / Funbound.plasma / hematocrit
    
  return(as.numeric(Krbp2pu))
}
