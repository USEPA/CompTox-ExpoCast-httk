#' Load CLint and Fup QSPR predictions predictions from Pradeep et al. 2020.
#' 
#' This function returns an updated version of 
#' \code{\link{chem.physical_and_invitro.data}}
#' that includes quantitative 
#' structure-property relationship (QSPR) predictions from
#' Support Vector Machine and Random Forest 
#' models developed and presented in Pradeep et al. 2020, included in
#' \code{\link{pradeep2020}}.
#' 
#' Because Clint and Fup are the only measurements required for many HTTK models,
#' changing the number of chemicals for which a value is available will change 
#' the number of chemicals which are listed with the \code{\link{get_cheminfo}}
#' command. Use the command \code{\link{reset_httk}} to return to the initial 
#' (measured only) \code{\link{chem.physical_and_invitro.data}} (for all
#' parameters).
#' 
#' @param overwrite Only matters if load.image=FALSE. If overwrite=TRUE then
#' existing data in chem.physical_and_invitro.data will be replaced by any
#' predictions in Pradeep et al. (2020) that is for the same chemical and
#' property. If overwrite=FALSE (DEFAULT) then new data for the same chemical
#' and property are ignored.  Funbound.plasma values of 0 (below limit of
#' detection) are overwritten either way.
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
#' @author Sarah E. Davidson
#' 
#' @references
#' \insertRef{pradeep2020chemstr}{httk}
#' 
#' @examples
#' 
#' \donttest{
#' # Count how many chemicals for which HTTK is available without the QSPR:
#' num.chems <- length(get_cheminfo())
#' print(num.chems)
#' 
#' # For chemicals with Pradeep et al. (2020) Clint and Fup QSPR predictions, 
#' # add them to our chemical information wherever measured values are 
#' # unavailable:
#' load_pradeep2020()
#' 
#' # Or, for chemicals with Pradeep et al. (2020) QSPR predictions, add them to
#' # our chemical information but overwrite measured values where we had them:
#' load_pradeep2020(overwrite=TRUE) 
#' 
#' # Let's see how many chemicals we have now with the Pradeep et al. (2020)
#' # predictions data loaded:
#' length(get_cheminfo())
#'
#' # Now let us reset the chemical data to the initial version:
#' reset_httk()
#'
#' # We should be back to our original number:
#' num.chems == length(get_cheminfo())
#' }                        
#' 
#' @export load_pradeep2020
load_pradeep2020 <- function(
    overwrite=FALSE,
    target.env=.GlobalEnv)
{
  cat(paste("Loading CLint and Fup predictions from Pradeep et al. (2020) for",
            dim(httk::pradeep2020)[1],"chemicals.\n"))
  cat(paste("Existing data are",
            ifelse(overwrite,""," not"),
            " being overwritten.\n",sep=""))
  cat("Please wait...\n")
  assign("chem.physical_and_invitro.data", 
         add_chemtable(httk::pradeep2020,
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