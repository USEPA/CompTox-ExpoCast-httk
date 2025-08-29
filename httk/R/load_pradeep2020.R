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
#' @param chem_include A vector of CAS numbers indicating only the chemicals to
#' be included in the loading process. If set to `NULL` all applicable chemicals are
#' loaded. (Default is `NULL`.)
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
#' 
#' # Demonstrate loading data for specific chemicals:
#' #
#' # Find chemicals with a clint and no fup:
#' subset(chem.physical_and_invitro.data,!is.na(Human.Clint) & Human.Funbound.plasma==0)$CAS
#' chem1 <- "101-05-3"
#' chem2 <- "2971-36-0"
#' # Take a look at what parameterize_steadystate gives (working from a default fup of 0.005):
#' a1 <- parameterize_steadystate(chem.cas=chem1)
#' a2 <- parameterize_steadystate(chem.cas=chem2)
#' 
#' # load Pradeep for this chemical:
#' load_pradeep2020(chem_include=chem1)
#' # Check values, only fup for the first chemical should change:
#' a3 <- parameterize_steadystate(chem.cas=chem1)
#' a4 <- parameterize_steadystate(chem.cas=chem2)
#' # All tests should be true:
#' a1[["Clint"]] == a3[["Clint"]]
#' a1[["Funbound.plasma"]] != a3[["Funbound.plasma"]]
#' a2[["Clint"]] == a4[["Clint"]]
#' a2[["Funbound.plasma"]] == a4[["Funbound.plasma"]]
#' 
#' # load Pradeep for this chemical, but allow it to overwrite the clint:
#' load_pradeep2020(chem_include=chem1, overwrite=TRUE)
#' # Check values, both clint and fup for the first chemical should change:
#' a5 <- parameterize_steadystate(chem.cas=chem1)
#' a6 <- parameterize_steadystate(chem.cas=chem2)
#' # All tests should be true:
#' a1[["Clint"]] != a5[["Clint"]]
#' a1[["Funbound.plasma"]] != a5[["Funbound.plasma"]]
#' a2[["Clint"]] == a6[["Clint"]]
#' a2[["Funbound.plasma"]] == a6[["Funbound.plasma"]]
#' 
#' # Load Pradeep for all chemicals, fup should change for second chemical:
#' load_pradeep2020()
#' a7 <- parameterize_steadystate(chem.cas=chem1)
#' a8 <- parameterize_steadystate(chem.cas=chem2)
#' # All tests should be true:
#' a1[["Clint"]] != a7[["Clint"]]
#' a1[["Funbound.plasma"]] != a7[["Funbound.plasma"]]
#' a2[["Clint"]] == a8[["Clint"]]
#' a2[["Funbound.plasma"]] != a8[["Funbound.plasma"]]
#' 
#' # load Pradeep for this chemical, but allow it to overwrite all clints:
#' load_pradeep2020(overwrite=TRUE)
#' # Both clint and fup should now be changed for second chemical:
#' a9 <- parameterize_steadystate(chem.cas=chem1)
#' a10 <- parameterize_steadystate(chem.cas=chem2)
#' # All tests should be true:
#' a1[["Clint"]] != a9[["Clint"]]
#' a1[["Funbound.plasma"]] != a9[["Funbound.plasma"]]
#' a2[["Clint"]] != a10[["Clint"]]
#' a2[["Funbound.plasma"]] != a10[["Funbound.plasma"]]
#' }                        
#' 
#' @export load_pradeep2020
load_pradeep2020 <- function(
    overwrite=FALSE,
    chem_include = NULL,
    target.env=.GlobalEnv)
{
  tmp_pradeep2020 <- httk::pradeep2020
  # check whether there is any information on specific chemicals to include
  if(!is.null(chem_include)){
    tmp_pradeep2020 <- httk_chem_subset(tmp_pradeep2020,chem_include = chem_include)
  }
  
  cat(paste("Loading CLint and Fup predictions from Pradeep et al. (2020) for",
            dim(tmp_pradeep2020)[1],"chemicals.\n"))
  cat(paste("Existing data are",
            ifelse(overwrite,""," not"),
            " being overwritten.\n",sep=""))
  cat("Please wait...\n")
  assign("chem.physical_and_invitro.data", 
         add_chemtable(tmp_pradeep2020,
                       current.table=chem.physical_and_invitro.data,
                       data.list=list(
                         CAS = 'CASRN',
                         DTXSID='DTXSID',
                         Funbound.plasma = 'Consensus (SVM,RF)',
                         Clint = 'pred_clint_rf'),
                       reference = 'Pradeep 2020', 
                       species= 'Human', 
                       overwrite=overwrite,
                       suppress.messages=TRUE),
         envir=target.env)
}