#' Calculate first pass metabolism
#'
#' For models that don't described first pass blood flow from the gut, need to
#' cacluate a hepatic bioavailability, that is, the fraction of chemical 
#' systemically available after metabolism during the first pass through the 
#' liver (Rowland, 1973).
#'
#' @param chem.cas Chemical Abstract Services Registry Number (CAS-RN) -- if
#'  parameters is not specified then the chemical must be identified by either
#'  CAS, name, or DTXISD
#' @param chem.name Chemical name (spaces and capitalization ignored) --  if
#'  parameters is not specified then the chemical must be identified by either
#'  CAS, name, or DTXISD
#' @param dtxsid EPA's 'DSSTox Structure ID (\url{https://comptox.epa.gov/dashboard})  
#'  -- if parameters is not specified then the chemical must be identified by 
#' either CAS, name, or DTXSIDs
#' @param parameters Parameters from the appropriate parameterization function
#' for the model indicated by argument model
#' @param restrictive.clearance Protein binding not taken into account (set to 1) in 
#' liver clearance if FALSE.
#' @param flow.34 A logical constraint
#'
#'@return A data.table whose columns are the parameters of the HTTK model
#'  specified in \code{model}.
#'
#' @author John Wambaugh
#'
#'@references Rowland, Malcolm, Leslie Z. Benet, and Garry G. Graham. 
#'"Clearance concepts in pharmacokinetics." Journal of pharmacokinetics and 
#'biopharmaceutics 1.2 (1973): 123-136.
#'
#' @keywords physiology 
#'
#' @import utils
#'                                                                 
#' @export calc_hep_bioavailability 
#'
calc_hep_bioavailability <- function(
                         chem.cas=NULL,
                         chem.name=NULL,
                         dtxsid = NULL,
                         parameters=NULL,
                         restrictive.clearance=T,
                         flow.34=T)
{
# We need to describe the chemical to be simulated one way or another:
  if (is.null(chem.cas) & 
      is.null(chem.name) & 
      is.null(dtxsid) &
      is.null(parameters)) 
    stop('Parameters, chem.name, chem.cas, or dtxsid must be specified.')

  if (is.null(parameters))
  {
    parameters <- parameterize_pbtk(
                    chem.cas=chem.cas,
                    chem.name=chem.name,
                    dtxsid=dtxsid)
  }
  
  if (!all(c("Qtotal.liverc","Funbound.plasma","Clmetabolismc","Rblood2plasma") 
    %in% names(parameters))) 
    stop("Missing needed parameters in calc_hepatic_bioavailability.")

  if (flow.34 & !("BW" %in% names(parameters))) 
    stop("flow.34=TRUE and missing BW in calc_hepatic_bioavailability.")
  else {
    #converting from L/h/kg^3/4 to L/h/kg
    parameters$Qtotal.liverc <- parameters$Qtotal.liverc/parameters$BW^0.25 
  }

  if (restrictive.clearance) 
  {
    bioavail <- (parameters$Qtotal.liverc / 
    (parameters$Qtotal.liverc + 
    parameters$Funbound.plasma * 
      parameters$Clmetabolismc / 
      parameters$Rblood2plasma))
  } else {
    bioavail <- (parameters$Qtotal.liverc / 
    (parameters$Qtotal.liverc + 
      parameters$Clmetabolismc / 
      parameters$Rblood2plasma))
  }
      
  return(set_httk_precision(bioavail))
}