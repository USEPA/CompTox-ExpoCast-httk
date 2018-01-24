available_rblood2plasma <- function(chem.cas=NULL,chem.name=NULL,species='Human',adjusted.Funbound.plasma=T)
{
  chem.physical_and_invitro.data <- chem.physical_and_invitro.data
  Rblood2plasma <- get_rblood2plasma(chem.name=chem.name,chem.cas=chem.cas,species=species) 
  if(tolower(species) != 'human' & is.na(Rblood2plasma)){
    Rblood2plasma <- get_rblood2plasma(chem.cas=chem.cas,chem.name=chem.name,species='Human')
    if(!is.na(Rblood2plasma)) warning('Human in vivo Rblood2plasma substituted.')
  }else if(!is.na(Rblood2plasma)) warning(paste(species,'in vivo Rblood2plasma returned.'))
  if(is.na(Rblood2plasma)){
    if(is.null(chem.cas)){
      out <- get_chem_id(chem.cas=chem.cas,chem.name=chem.name)
      chem.cas <- out$chem.cas
    }
    if(chem.cas %in% get_cheminfo(species=species,model='schmitt')){
      Rblood2plasma <- calc_rblood2plasma(chem.cas=chem.cas,species=species,adjusted.Funbound.plasma=adjusted.Funbound.plasma)
      warning(paste(species,'Rblood2plasma calculated with calc_rblood2plasma.')) 
    }else if(chem.cas %in% get_cheminfo(species='Human',model='schmitt')){
      Rblood2plasma <- calc_rblood2plasma(chem.cas=chem.cas,species=species,default.to.human=T,adjusted.Funbound.plasma=adjusted.Funbound.plasma)
      warning(paste(species,'Rblood2plasma calculated with Human Funbound.plasma.'))
    }else{
      Rblood2plasma.data <- chem.physical_and_invitro.data[,'Human.Rblood2plasma']
      Rblood2plasma <- mean(Rblood2plasma.data[which(!is.na(Rblood2plasma.data))])
      warning('Average in vivo Human Rblood2plasma substituted.')
    }
  }
  return(Rblood2plasma)
}