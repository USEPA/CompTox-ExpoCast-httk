#' Calculate the constant ratio of the blood concentration to the plasma
#' concentration.
#' 
#' This function calculates the constant ratio of the blood concentration to
#' the plasma concentration.
#' 
#' The red blood cell (RBC) parition coefficient as predicted by the Schmitt
#' (2008) method is used in the calculation. The value is calculated with the
#' equation: 1 - hematocrit + hematocrit * Krbc2pu * Funbound.plasma, summing
#' the red blood cell to plasma and plasma:plasma (equal to 1) partition
#' coefficients multiplied by their respective fractional volumes. %%When
#' species is specified as rabbit, dog, or mouse, the function uses the
#' appropriate physiological data(hematocrit and temperature) but substitues
#' human fraction unbound and tissue volumes.  %% ~~ If necessary, more details
#' than the description above ~~
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
#' @author John Wambaugh and Robert Pearce
#' @references Schmitt W. "General approach for the calculation of tissue to
#' plasma partition coefficients." Toxicology In Vitro, 22, 457-467 (2008).
#' @keywords Parameter
#' @examples
#' 
#' calc_rblood2plasma(chem.name="Bisphenol A")
#' calc_rblood2plasma(chem.name="Bisphenol A",species="Rat")
#' 
#' @export calc_rblood2plasma
calc_rblood2plasma <- function(chem.cas=NULL,
                              chem.name=NULL,
                              params=NULL,
                              hematocrit=NULL,
                              default.to.human=F,
                              species="Human",
                              adjusted.Funbound.plasma=T,
                              suppress.messages=F)
{
  physiology.data <- physiology.data

  if (is.null(params)) 
  {
    parameters <- parameterize_schmitt(chem.cas=chem.cas,
                    chem.name=chem.name,
                    default.to.human=default.to.human,
                    species=species,
                    suppress.messages=suppress.messages)
  } else {
    parameters <- params
  }
  
  if (!(species %in% colnames(physiology.data)))
  {
    if (toupper(species) %in% toupper(colnames(physiology.data)))
    {
      phys.species <- colnames(physiology.data)[toupper(colnames(physiology.data))==toupper(species)]
    } else stop(paste("Physiological PK data for",species,"not found."))
  } else phys.species <- species

  if (is.null(hematocrit)) 
  {
    if (is.null(parameters$hematocrit))
    {
      hematocrit <- physiology.data[physiology.data$Parameter=="Hematocrit",phys.species]
    } else {
      hematocrit <- parameters$hematocrit
    }
  }
  
# Predict the PCs for all tissues in the tissue.data table:

  PCs <- predict_partitioning_schmitt(parameters=parameters,
           species=species,
           adjusted.Funbound.plasma=adjusted.Funbound.plasma,
           tissues='red blood cells')  #regression not applied to Krbc2pu
    
  if(adjusted.Funbound.plasma) Rblood2plasma = 1 - hematocrit + hematocrit * PCs[["Krbc2pu"]] * parameters$Funbound.plasma
  else Rblood2plasma = 1 - hematocrit + hematocrit * PCs[["Krbc2pu"]] * parameters$unadjusted.Funbound.plasma
    
  return(as.numeric(Rblood2plasma))
}
