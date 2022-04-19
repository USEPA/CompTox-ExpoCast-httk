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
#' 
#' @param exclude_oad Include the chemicals only within the applicability domain.
#' If exlude_oad=TRUE (DEFAULT) chemicals outside the applicability domain do not
#' have their predicted values loaded.
#' 
#' @param target.env The environment where the new
#' chem.physical_and_invitro.data is loaded. Defaults to global environment.
#' 
#' @return \item{data.frame}{An updated version of
#' chem.physical_and_invitro.data.}
#' 
#' @author Sarah E. Davidson
#' 
#' @references
#' \insertRef{dawson2021qsar}{httk}
#' 
#' @examples
#' 
#' \dontrun{
#' chem.physical_and_invitro.data <- load_dawson2021()
#' chem.physical_and_invitro.data <- load_dawson2021(overwrite=TRUE) 
#' }                        
#' 
#' @importFrom Rdpack reprompt
#' 
#' @export load_dawson2021
load_dawson2021 <- function(overwrite=FALSE,exclude_oad=TRUE,target.env=.GlobalEnv)
{
  #R CMD CHECK throws notes about "no visible binding for global variable", for
  #each time a data.table column name is used without quotes. To appease R CMD
  #CHECK, a variable has to be created for each of these column names and set to
  #NULL. Note that within the data.table, these variables will not be NULL! Yes,
  #this is pointless and annoying.
  `Clint QSAR AD Outlier` <- `Fup QSAR AD Outlier` <- `CASRN` <- NULL
  `QSAR_Clint` <- `QSAR_Fup` <- NULL
  #End R CMD CHECK appeasement.

  cat(paste("Chemicals outside the applicabilty domain are",
            ifelse(exclude_oad,""," not"),
            " excluded in the data load.\n",sep=""))
  if(exclude_oad){
    tmp_dawson2021 <- httk::dawson2021 %>%
      dplyr::filter(`Clint QSAR AD Outlier`==0) %>%
      dplyr::filter(`Fup QSAR AD Outlier`==0) %>% 
      dplyr::select(`CASRN`,`QSAR_Clint`,`QSAR_Fup`) %>%
      as.data.frame()
  }else{
    tmp_dawson2021 <- httk::dawson2021 %>%
      dplyr::select(`CASRN`,`QSAR_Clint`,`QSAR_Fup`) %>% 
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
                                                           Funbound.plasma = 'QSAR_Fup', # 'Human.Funbound.plasma',
                                                           Clint = 'QSAR_Clint'), # 'Human.Clint'),
                                                         reference = 'Dawson 2021', 
                                                         species= 'Human', 
                                                         overwrite=overwrite),
         envir=target.env)
}