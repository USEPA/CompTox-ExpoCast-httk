# This function calculates the constant ratio of the blood concentration to the plasma concentration. It uses the hematocrit and the red blood cell (RBC) parition coefficient as predicted by the Schmitt (2008) method.

calc_rblood2plasma <- function(chem.cas=NULL,
                              chem.name=NULL,
                              params=NULL,
                              hematocrit=NULL,
                              default.to.human=F,
                              species="Human",
                              adjusted.Funbound.plasma=T)
{
  physiology.data <- physiology.data

  if (is.null(params)) 
  {
    parameters <- parameterize_schmitt(chem.cas=chem.cas,chem.name=chem.name,default.to.human=default.to.human,species=species)
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