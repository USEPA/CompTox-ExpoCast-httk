#' Load data from Pradeep et al. 2020.
#' 
#' This function returns an updated version of chem.physical_and_invitro.data
#' that includes data predicted with Support Vector Machine and Random Forest 
#' models developed and presented in Pradeep et al. 2020, included in
#' pradeep2020.
#' 
#' @param overwrite Only matters if load.image=FALSE. If overwrite=TRUE then
#' existing data in chem.physical_and_invitro.data will be replaced by any
#' data/predictions in Pradeep et al. (2020) that is for the same chemical and
#' property. If overwrite=FALSE (DEFAULT) then new data for the same chemical
#' and property are ignored.  Funbound.plasma values of 0 (below limit of
#' detection) are overwritten either way.
#' @param target.env The environment where the new
#' chem.physical_and_invitro.data is loaded. Defaults to global environment.
#' @return \item{data.frame}{An updated version of
#' chem.physical_and_invitro.data.}
#' @author Sarah E. Davidson
#' @references
#' \insertRef{pradeep2020chemstr}{httk}
#' 
#' @examples
#' 
#' \dontrun{
#' chem.physical_and_invitro.data <- load_pradeep2020()
#' chem.physical_and_invitro.data <- load_pradeep2020(overwrite=TRUE) 
#' }                        
#' 
#' @export load_pradeep2020
load_pradeep2020 <- function(overwrite=FALSE,target.env=.GlobalEnv)
{
  cat(paste("Loading predictions from Pradeep et al. (2020) for",
            dim(httk::pradeep2020)[1],"chemicals.\n"))
  cat(paste("Existing data are",
            ifelse(overwrite,""," not"),
            " being overwritten.\n",sep=""))
  cat("Please wait...\n")
  assign("chem.physical_and_invitro.data", add_chemtable(httk::pradeep2020,
                                                         current.table=chem.physical_and_invitro.data,
                                                         data.list=list(
                                                           CAS = 'CASRN',
                                                           DTXSID='DTXSID',
                                                           Funbound.plasma = 'Consensus (SVM,RF)',
                                                           Clint = 'pred_clint_rf'),
                                                         reference = 'Pradeep 2020', 
                                                         species= 'Human', 
                                                         overwrite=overwrite),
         envir=target.env)
}