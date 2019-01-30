#Written by Robert Pearce
# This function attempts to retrieve a measured species- and chemical-specific 
# blood:plasma concentration ratio.
get_rblood2plasma <- function(chem.name=NULL,chem.cas=NULL,species='Human',default.to.human=F){
  chem.physical_and_invitro.data <- chem.physical_and_invitro.data
  if (is.null(chem.name) & is.null(chem.cas)) return(NA)
  
  CAS <- NULL
  # Look up the chemical name/CAS, depending on what was provide:
  if(is.null(chem.cas)){
    out <- get_chem_id(chem.cas=chem.cas,chem.name=chem.name)
    chem.cas <- out$chem.cas
  }
  species.Rblood2plasma <- paste0(toupper(substr(species,1,1)),substr(species,2,nchar(species)),'.Rblood2plasma')
  if(! species.Rblood2plasma %in% colnames(chem.physical_and_invitro.data)) Rblood2plasma <- NA
  else Rblood2plasma <- subset(chem.physical_and_invitro.data,CAS == chem.cas)[,species.Rblood2plasma]
  if(default.to.human & is.na(Rblood2plasma) & tolower(species) != 'human'){
    Rblood2plasma <- subset(chem.physical_and_invitro.data,CAS == chem.cas)[,'Human.Rblood2plasma']
    if(!is.na(Rblood2plasma)) warning('Human in vivo Rblood2plasma substituted for missing value.')
  }
  return(Rblood2plasma)
}