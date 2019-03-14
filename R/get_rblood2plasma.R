#Written by Robert Pearce
# This function attempts to retrieve a measured species- and chemical-specific 
# blood:plasma concentration ratio.




#' Get ratio of the blood concentration to the plasma concentration.
#' 
#' This function retrieves the in vivo ratio of the blood concentration to the
#' plasma concentration.
#' 
#' A value of NA is returned when the requested value is unavailable.  Values
#' are retrieved from chem.physical_and_invitro.data. %% ~~ If necessary, more
#' details than the description above ~~
#' 
#' @param chem.name Either the chemical name or the CAS number must be
#' specified. %% ~~Describe \code{obs} here~~
#' @param chem.cas Either the CAS number or the chemical name must be
#' specified. %% ~~Describe \code{pred} here~~
#' @param species Species desired (either "Rat", "Rabbit", "Dog", "Mouse", or
#' default "Human"). %% ~~Describe \code{ssparams.var.inv} here~~
#' @param default.to.human Substitutes missing animal values with human values
#' if true.
#' @author Robert Pearce
#' @keywords Parameter
#' @examples
#' 
#' get_rblood2plasma(chem.name="Bisphenol A")
#' get_rblood2plasma(chem.name="Bisphenol A",species="Rat")
#' 
#' @export get_rblood2plasma
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
