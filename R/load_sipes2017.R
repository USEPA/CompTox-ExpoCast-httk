# Writen by Robert Pearce and John Wambaugh
# Adds the predicted data from Sipes et al. (2017) for the Tox21 library




#' Load data from Sipes et al 2017.
#' 
#' This function returns an updated version of chem.physical_and_invitro.data
#' that includes data predicted with Simulations Plus' ADMET predictor that was
#' used in Sipes et al. 2017, included in admet.data.
#' 
#' 
#' @param load.image If overwrite=TRUE (DEFAULT)) then the default HTTK
#' chemical data plus the any new data/predictions from Sipes et al. (2017)
#' will be quickly loaded. This is the same as load.image=F, but much faster,
#' however any other data added by the user will be deleted.
#' @param overwrite Only matters if load.image=FALSE. If overwrite=TRUE then
#' existing data in chem.physical_and_invitro.data will be replaced by any
#' data/predictions in Sipes et al. (2017) that is for the same chemical and
#' property. If overwrite=FALSE (DEFAULT) then new data for the same chemical
#' and property are ignored.  Funbound.plasma values of 0 (below limit of
#' detection) are overwritten either way.
#' @param target.env The environment where the new
#' chem.physical_and_invitro.data is loaded. Defaults to global environment.
#' @return \item{data.frame}{An updated version of
#' chem.physical_and_invitro.data.}
#' @author Robert Pearce and John Wambaugh
#' @references Sipes, Nisha S., et al. "An intuitive approach for predicting
#' potential human health risk with the Tox21 10k library." Environmental
#' Science & Technology 51.18 (2017): 10786-10796.
#' @examples
#' 
#' \dontrun{
#' chem.physical_and_invitro.data <- load_sipes2017()
#' chem.physical_and_invitro.data <- load_sipes2017(overwrite=T) 
#' }                        
#' 
#' @export load_sipes2017
load_sipes2017 <- function(load.image=T,overwrite=F,target.env=.GlobalEnv)
{
  if (load.image)
  {
#    assignInMyNamespace("chem.physical_and_invitro.data", sipes2017.table)
    assign("chem.physical_and_invitro.data", sipes2017.table,envir=target.env)
#    chem.physical_and_invitro.data <<- sipes2017.table
  } else {
    assign("chem.physical_and_invitro.data", add_chemtable(sipes2017,
                                                  current.table=chem.physical_and_invitro.data,
                                                  data.list=list(Compound='Compound', 
                                                    CAS='CAS', 
                                                    MW = 'MW', 
                                                    logP = 'logP',
                                                    pKa_Donor = 'pKa_Donor', 
                                                    pKa_Accept = 'pKa_Accept',
                                                    Funbound.plasma = 'Human.Funbound.plasma', 
                                                    Clint = 'Human.Clint', 
                                                    SMILES.desalt = 'SMILES.desalt'),
                                                  reference = 'Sipes 2017', 
                                                  species= 'Human', 
                                                  overwrite=overwrite),
                                                  envir=target.env)
  }
}
