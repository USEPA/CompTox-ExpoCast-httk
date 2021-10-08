#' Load data from Pradeep et al. 2020.
#' 
#' This function returns an updated version of chem.physical_and_invitro.data
#' that includes data predicted with ____ models developed and
#' presented in Pradeep et al. 2020, included in pradeep2020.
#' 
#' @param overwrite Only matters if load.image=FALSE. If overwrite=TRUE then
#' existing data in chem.physical_and_invitro.data will be replaced by any
#' data/predictions in Pradeep et al. (2020) that is for the same chemical and
#' property. If overwrite=FALSE (DEFAULT) then new data for the same chemical
#' and property are ignored.  Funbound.plasma values of 0 (below limit of
#' detection) are overwritten either way.
#' @param exclude_oad Include the chemicals only within the applicability domain.
#' If exlude_oad=TRUE (DEFAULT) chemicals outside the applicability domain do not
#' have their predicted values loaded.
#' @param target.env The environment where the new
#' chem.physical_and_invitro.data is loaded. Defaults to global environment.
#' @return \item{data.frame}{An updated version of
#' chem.physical_and_invitro.data.}
#' @author Sarah E. Davidson
#' @references _________________
#' 
#' @examples
#' 
#' \dontrun{
#' chem.physical_and_invitro.data <- load_pradeep2020()
#' chem.physical_and_invitro.data <- load_pradeep2020(overwrite=TRUE) 
#' }                        
#' 
#' @export load_pradeep2020
load_pradeep2020 <- function(overwrite=FALSE,exclude_oad=TRUE,target.env=.GlobalEnv)
{
  cat(paste("Chemicals outside the applicabilty domain are",
            ifelse(exclude_oad,""," not"),
            " excluded in the data load.\n",sep=""))
  if(exclude_oad){
    tmp_pradeep2020 <- pradeep2020 %>% dplyr::filter(Outlier==0) %>% dplyr::filter(AD_out==0) %>% 
      dplyr::select(`CASRN`,`QSAR Clint`,`QSAR Fup`) %>% as.data.frame()
  }else{
    tmp_pradeep2020 <- pradeep2020 %>% dplyr::select(`CASRN`,`QSAR Clint`,`QSAR Fup`) %>% 
      as.data.frame()
  }
  cat(paste("Loading predictions from Dawson et al. (2021) for",
            dim(tmp_pradeep2020)[1],"chemicals.\n"))
  cat(paste("Existing data are",
            ifelse(overwrite,""," not"),
            " being overwritten.\n",sep=""))
  cat("Please wait...\n")
  assign("chem.physical_and_invitro.data", add_chemtable(tmp_pradeep2020,
                                                         current.table=chem.physical_and_invitro.data,
                                                         data.list=list(
                                                           CAS='CASRN', # 'CAS',
                                                           Funbound.plasma = 'QSAR Fup', # 'Human.Funbound.plasma',
                                                           Clint = 'QSAR Clint'), # 'Human.Clint'),
                                                         reference = 'Dawson 2021', 
                                                         species= 'Human', 
                                                         overwrite=overwrite),
         envir=target.env)
}