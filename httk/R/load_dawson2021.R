#' Load data from Dawson et al. 2021.
#' 
#' This function returns an updated version of chem.physical_and_invitro.data
#' that includes data predicted with Random Forest QSAR models developed and
#' presented in Dawson et al. 2021, included in dawson2021.
#' 
#' @param overwrite Only matters if load.image=FALSE. If overwrite=TRUE then
#' existing data in chem.physical_and_invitro.data will be replaced by any
#' data/predictions in Dawson et al. (2021) that is for the same chemical and
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
#' @references Dawson, Daniel E. et al. "Designing QSARs for parameters
#' of high-throughput toxicokinetic models using open-source descriptors."
#' Environmental Science & Technology____. (2021):______.
#' 
#' @examples
#' 
#' \dontrun{
#' chem.physical_and_invitro.data <- load_dawson2021()
#' chem.physical_and_invitro.data <- load_dawson2021(overwrite=TRUE) 
#' }                        
#' 
#' @export load_dawson2021
load_dawson2021 <- function(overwrite=FALSE,exclude_oad=TRUE,target.env=.GlobalEnv)
{
  cat(paste("Chemicals outside the applicabilty domain are",
            ifelse(exclude_oad,""," not"),
            " excluded in the data load.\n",sep=""))
  if(exclude_oad){
    tmp_dawson2021 <- dawson2021 %>% dplyr::filter(Outlier==0) %>% dplyr::filter(AD_out==0) %>% 
      dplyr::select(`CASRN`,`QSAR Clint`,`QSAR Fup`) %>% as.data.frame()
  }else{
    tmp_dawson2021 <- dawson2021 %>% dplyr::select(`CASRN`,`QSAR Clint`,`QSAR Fup`) %>% 
      as.data.frame()
  }
  cat(paste("Loading predictions from Dawson et al. (2021) for",
            dim(tmp_dawson2021)[1],"chemicals.\n"))
  cat(paste("Existing data are",
            ifelse(overwrite,""," not"),
            " being overwritten.\n",sep=""))
  cat("Please wait...\n")
  assign("chem.physical_and_invitro.data", add_chemtable(tmp_dawson2021,
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