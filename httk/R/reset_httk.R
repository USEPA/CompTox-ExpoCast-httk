#' Reset HTTK to Default Data Tables
#' 
#' This function returns an updated version of chem.physical_and_invitro.data
#' that includes data predicted with Simulations Plus' ADMET predictor that was
#' used in Sipes et al. 2017, included in admet.data.
#' 
#' @param target.env The environment where the new
#' chem.physical_and_invitro.data is loaded. Defaults to global environment.
#' 
#' @return \item{data.frame}{The package default version of 
#' chem.physical_and_invitro.data.}
#' 
#' @author John Wambaugh
#' 
#' @examples
#' 
#' \donttest{
#' chem.physical_and_invitro.data <- load_sipes2017()
#' reset_httk()
#' }                        
#' 
#' @export reset_httk
reset_httk <- function(target.env=.GlobalEnv)
{
  assign("chem.physical_and_invitro.data", 
    httk::chem.physical_and_invitro.data, envir=target.env)
}
