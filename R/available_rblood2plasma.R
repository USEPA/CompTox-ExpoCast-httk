#Written by Robert Pearce
# Either retrieves a measured blood:plasma concentration ratio from the
# chem.physical_and_invitro.data table or calculates it using the red blood cell
# partition coefficient predicted with Schmitt's method






#' Find the best available ratio of the blood to plasma concentration constant.
#' 
#' 
#' 
#' @param chem.cas Either the CAS number or the chemical name must be
#' specified. %% ~~Describe \code{pred} here~~}
#' 
#' \itemchem.nameEither the chemical name or the CAS number must be specified.
#' %% ~~Describe \code{obs} here~~}
#' 
#' \itemspeciesSpecies desired (either "Rat", "Rabbit", "Dog", "Mouse", or
#' default "Human"). %% ~~Describe \code{ssparams.var.inv} here~~}
#' 
#' \itemadjusted.Funbound.plasmaWhether or not to use Funbound.plasma
#' adjustment if calculating Rblood2plasma.
#' 
#' This function finds the best available constant ratio of the blood
#' concentration to the plasma concentration, using get_rblood2plasma and
#' calc_rblood2plasma.
#' 
#' If available, in vivo data (from chem.physical_and_invitro.data) for the
#' given species is returned, substituting the human in vivo value when missing
#' for other species.  In the absence of in vivo data, the value is calculated
#' with calc_rblood2plasma for the given species. If Funbound.plasma is
#' unvailable for the given species, the human Funbound.plasma is substituted.
#' If none of these are available, the mean human Rblood2plasma from
#' chem.physical_and_invitro.data is returned.  %% ~~ If necessary, more
#' details than the description above ~~
#' 
#' available_rblood2plasma(chem.name="Bisphenol
#' A",adjusted.Funbound.plasma=FALSE)
#' available_rblood2plasma(chem.name="Bisphenol A",species="Rat")
#' 
#' Robert Pearce
#' 
#' Parameter
#' @export available_rblood2plasma
available_rblood2plasma <- function(chem.cas=NULL,chem.name=NULL,species='Human',adjusted.Funbound.plasma=T)
{
  chem.physical_and_invitro.data <- chem.physical_and_invitro.data
  Rblood2plasma <- get_rblood2plasma(chem.name=chem.name,chem.cas=chem.cas,species=species) 
  if (tolower(species) != 'human' & is.na(Rblood2plasma)){
    Rblood2plasma <- get_rblood2plasma(chem.cas=chem.cas,chem.name=chem.name,species='Human')
    if(!is.na(Rblood2plasma)) warning('Human in vivo Rblood2plasma substituted.')
  }else if(!is.na(Rblood2plasma)) warning(paste(species,'in vivo Rblood2plasma returned.'))
  if(is.na(Rblood2plasma))
  {
    if (is.null(chem.cas)& is.null(chem.name))
    {
      Rblood2plasma.data <- chem.physical_and_invitro.data[,'Human.Rblood2plasma']
      Rblood2plasma <- mean(Rblood2plasma.data[which(!is.na(Rblood2plasma.data))])
      warning(paste('Average in vivo Human Rblood2plasma (',
                    signif(Rblood2plasma,3),
                    ') substituted.',sep=""))

    } else {
      if(is.null(chem.cas)) 
      {
        out <- get_chem_id(chem.cas=chem.cas,chem.name=chem.name)
        chem.cas <- out$chem.cas
      }
      if (chem.cas %in% get_cheminfo(species=species,model='schmitt'))
      {
        Rblood2plasma <- calc_rblood2plasma(chem.cas=chem.cas,species=species,adjusted.Funbound.plasma=adjusted.Funbound.plasma)
        warning(paste(species,'Rblood2plasma calculated with calc_rblood2plasma.')) 
      } else if (chem.cas %in% get_cheminfo(species='Human',model='schmitt')) {
        Rblood2plasma <- calc_rblood2plasma(chem.cas=chem.cas,species=species,default.to.human=T,adjusted.Funbound.plasma=adjusted.Funbound.plasma)
        warning(paste(species,'Rblood2plasma calculated with Human Funbound.plasma.'))
      } else {
        Rblood2plasma.data <- chem.physical_and_invitro.data[,'Human.Rblood2plasma']
        Rblood2plasma <- mean(Rblood2plasma.data[which(!is.na(Rblood2plasma.data))])
        warning(paste('Average in vivo Human Rblood2plasma (',
                      signif(Rblood2plasma,3),
                      ') substituted.',sep=""))
      }
    }
  }
  return(Rblood2plasma)
}
