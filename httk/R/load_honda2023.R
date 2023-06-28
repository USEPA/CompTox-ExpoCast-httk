#' Load Caco2 QSPR predictions from Honda et al. 2023
#' 
#' This function returns an updated version of 
#' \code{\link{chem.physical_and_invitro.data}}
#' that includes Caco2 Pab predictions from the Random Forest quantitative 
#' structure-property relationship (QSPR) models developed and
#' presented in Honda et al. 2023, included in table \code{\link{honda2023}}.
#' 
#' Note that because Pab is not required for most HTTK models, changing
#' the number of chemicals for which a value is available will not change the
#' number of chemicals which are listed with the \code{\link{get_cheminfo}}
#' command. Use the command \code{\link{reset_httk}} to return to the initial 
#' (measured only) \code{\link{chem.physical_and_invitro.data}} (for all
#' parameters).
#' 
#' @param overwrite Only matters if load.image=FALSE. If overwrite=TRUE then
#' existing data in chem.physical_and_invitro.data will be replaced by any
#' prediction in Honda et al. (2023) that is for the same chemical and
#' property. If overwrite=FALSE (DEFAULT) then new data for the same chemical
#' and property are ignored.
#' 
#' @param exclude_oad Include the chemicals only within the applicability domain.
#' If exclude_oad=TRUE (DEFAULT) chemicals outside the applicability domain do not
#' have their predicted values loaded.
#' 
#' @param target.env The environment where the new
#' \code{\link{chem.physical_and_invitro.data}} is loaded. Defaults to global environment.
#' 
#' @return \item{data.frame}{An updated version of
#' \code{\link{chem.physical_and_invitro.data}}.}
#' 
#' @seealso \code{\link{reset_httk}}
#' @seealso \code{\link{get_cheminfo}}
#' 
#' @author John Wambaugh
#' 
#' @examples
#' 
#' \dontrun{
#' # For chemicals with Honda et al. (2023) Caco2 Pab QSPR predictions, 
#' # add them to our chemical information wherever measured values are 
#' # unavailable:
#' load_honda2023()
#' 
#' # Or, for chemicals with Honda et al. (2023) QSPR predictions, add them to
#' # our chemical information but overwrite measured values where we had them:
#' load_honda2023(overwrite=TRUE) 
#'
#' # Now let us reset the chemical data to the initial version:
#' reset_httk()
#' }                        
#' 
#' @export load_honda2023
load_honda2023 <- function(
    overwrite=FALSE,
    exclude_oad=TRUE,
    target.env=.GlobalEnv)
{
  #R CMD CHECK throws notes about "no visible binding for global variable", for
  #each time a data.table column name is used without quotes. To appease R CMD
  #CHECK, a variable has to be created for each of these column names and set to
  #NULL. Note that within the data.table, these variables will not be NULL! Yes,
  #this is pointless and annoying.

  #End R CMD CHECK appeasement.

  cat(paste("Chemicals outside the applicabilty domain are",
            ifelse(exclude_oad,""," not"),
            " excluded in the prediction load.\n",sep=""))
  if (exclude_oad)
  {
    tmp_honda2023 <- subset(httk::honda2023.qspr, Pab.Pred.AD == 1)
  } else {
    tmp_honda2023 <- httk::honda2023.qspr
  }
  cat(paste("Loading Caco2 PAb predictions from Honda et al. (2023) for",
            dim(tmp_honda2023)[1],"chemicals.\n"))
  cat(paste("Existing data are",
            ifelse(overwrite,""," not"),
            " being overwritten.\n",sep=""))
  cat("Please wait...\n")
  assign("chem.physical_and_invitro.data", 
         add_chemtable(tmp_honda2023,
                       current.table=chem.physical_and_invitro.data,
                       data.list=list(
                         DTXSID='DTXSID',
                         CAS="CAS",
                         Caco2.Pab="Pab.Quant.Pred"),
                       reference = 'HondaUnpublished',
                       species="Human", 
                       overwrite=overwrite),
         envir=target.env)
}