#' Load data from Sipes et al 2017.
#' 
#' This function returns an updated version of chem.physical_and_invitro.data
#' that includes data predicted with Simulations Plus' ADMET predictor that was
#' used in Sipes et al. 2017, included in admet.data.
#' 
#' @param overwrite Only matters if load.image=FALSE. If overwrite=TRUE then
#' existing data in chem.physical_and_invitro.data will be replaced by any
#' data/predictions in Sipes et al. (2017) that is for the same chemical and
#' property. If overwrite=FALSE (DEFAULT) then new data for the same chemical
#' and property are ignored.  Funbound.plasma values of 0 (below limit of
#' detection) are overwritten either way.
#' @param target.env The environment where the new
#' chem.physical_and_invitro.data is loaded. Defaults to global environment.
#' 
#' @return \item{data.frame}{An updated version of
#' chem.physical_and_invitro.data.}
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
#' num.chems <- length(get_cheminfo())
#' load_sipes2017()
#'
#' #We should have the ADMet Predicted chemicals from Sipes et al. (2017),
#' #this one is a good test since the logP is nearly 10
#' calc_css(chem.cas="26040-51-7")
#'
#' #Let's see how many chemicals we have now with the Sipes (2017) data loaded:
#' length(get_cheminfo())
#'
#' #Now let us reset
#' reset_httk()
#'
#' # We should be back to our original number:
#' num.chems == length(get_cheminfo())
#' }                        
#' 
#' @export load_sipes2017
load_sipes2017 <- function(overwrite=FALSE,target.env=.GlobalEnv)
{
  cat(paste("Loading predictions from Sipes et al. (2017) for",
    dim(sipes2017)[1],"chemicals.\n"))
  cat(paste("Existing data are",
    ifelse(overwrite,""," not"),
    " being overwritten.\n",sep=""))
  cat("Please wait...\n")
  assign("chem.physical_and_invitro.data", add_chemtable(sipes2017,
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