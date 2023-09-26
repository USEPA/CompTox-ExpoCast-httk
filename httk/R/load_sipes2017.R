#' Load CLint and Fup QSPR predictions from Sipes et al 2017.
#' 
#' This function returns an updated version of 
#' \code{\link{chem.physical_and_invitro.data}}
#' that includes quantitative 
#' structure-property relationship (QSPR) predictions from Simulations Plus' 
#' ADMET predictor as 
#' used in Sipes et al. 2017, included in \code{\link{sipes2017}}.
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
#' predictions in Sipes et al. (2017) that is for the same chemical and
#' property. If overwrite=FALSE (DEFAULT) then new data for the same chemical
#' and property are ignored. Funbound.plasma values of 0 (below limit of
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
#' @author Robert Pearce and John Wambaugh
#' 
#' @references Sipes, Nisha S., et al. "An intuitive approach for predicting
#' potential human health risk with the Tox21 10k library." Environmental
#' Science & Technology 51.18 (2017): 10786-10796.
#'
#' @examples
#' 
#' \donttest{
#' # Count how many chemicals for which HTTK is available without the QSPR:
#' num.chems <- length(get_cheminfo())
#' print(num.chems)
#' 
#' # For chemicals with Sipes et al. (2017) Clint and Fup QSPR predictions, 
#' # add them to our chemical information wherever measured values are 
#' # unavailable:
#' load_sipes2017()
#'
#' # Here's a chemical we didn't have before (this one is a good test since the 
#' # logP is nearly 10 and it probably wouldn't work in vitro):
#' calc_css(chem.cas="26040-51-7")
#'
#' # Let's see how many chemicals we have now with the Sipes et al. (2017) 
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
#' @export load_sipes2017
load_sipes2017 <- function(
    overwrite=FALSE,
    target.env=.GlobalEnv)
{
  cat(paste("Loading CLint and Fup predictions from Sipes et al. (2017) for",
    dim(httk::sipes2017)[1],"chemicals.\n"))
  cat(paste("Existing data are",
    ifelse(overwrite,""," not"),
    " being overwritten.\n",sep=""))
  cat("Please wait...\n")
  assign("chem.physical_and_invitro.data", 
         add_chemtable(httk::sipes2017,
                       current.table=chem.physical_and_invitro.data,
                       data.list=list(
                         CAS='CAS', 
                         Funbound.plasma = 'Human.Funbound.plasma', 
                         Clint = 'Human.Clint'),
                       reference = 'Sipes 2017', 
                       species= 'Human', 
                       overwrite=overwrite),
         envir=target.env)
}