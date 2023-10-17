#' Calculate first pass heaptic metabolism
#'
#' For models that don't described first pass blood flow from the gut, need to
#' cacluate a hepatic bioavailability, that is, the fraction of chemical 
#' systemically available after metabolism during the first pass through the 
#' liver (Rowland, 1973 Equation 29, where k21 is blood flow through the liver
#' and k23 is clearance from the liver in Figure 1 in that paper).
#'
#' @param chem.cas Chemical Abstract Services Registry Number (CAS-RN) -- if
#'  parameters is not specified then the chemical must be identified by either
#'  CAS, name, or DTXISD
#'
#' @param chem.name Chemical name (spaces and capitalization ignored) --  if
#'  parameters is not specified then the chemical must be identified by either
#'  CAS, name, or DTXISD
#'
#' @param dtxsid EPA's 'DSSTox Structure ID (\url{https://comptox.epa.gov/dashboard})  
#'  -- if parameters is not specified then the chemical must be identified by 
#' either CAS, name, or DTXSIDs
#'
#' @param parameters Parameters from the appropriate parameterization function
#' for the model indicated by argument model
#'
#' @param restrictive.clearance Protein binding not taken into account (set to 1) in 
#' liver clearance if FALSE.
#'
#' @param flow.34 A logical constraint
#'
#' @param species Species desired (either "Rat", "Rabbit", "Dog", "Mouse", or
#' default "Human").
#'
#' @param suppress.messages Whether or not to suppress the output message.
#'
#'@return A data.table whose columns are the parameters of the HTTK model
#'  specified in \code{model}.
#'
#' @author John Wambaugh
#'
#'@references Rowland, Malcolm, Leslie Z. Benet, and Garry G. Graham. 
#' \insertRef{rowland1973clearance}{httk}
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
                         restrictive.clearance=TRUE,
                         flow.34=TRUE,
                         suppress.messages=FALSE,
                         species="Human")
{
# We need to describe the chemical to be simulated one way or another:
  if (is.null(chem.cas) & 
      is.null(chem.name) & 
      is.null(dtxsid) &
      is.null(parameters)) 
    stop('Parameters, chem.name, chem.cas, or dtxsid must be specified.')

  # Required parameters
  req.param <- c("BW", "Qtotal.liverc", "Clmetabolismc", "Funbound.plasma", 
                 "Rblood2plasma")
  
  # Qtotal.liverc is a total blood flow for simpler models without explicit
  # first-pass hepatic metabolism. However, if we arecalling this function from
  # a more complicated model we can still calculate Qtotal.liverc if we have 
  # the appropriate other parameters:
  if (!is.null(parameters))
    if (all(c("Qcardiacc","Qliverf","Qgutf") %in% names(parameters)))
    {
      parameters["Qtotal.liverc"] <- parameters[["Qcardiacc"]] * (
        parameters[["Qliverf"]] + parameters[["Qgutf"]])
    }  
  
  if (is.null(parameters) | !all(req.param %in% names(parameters)))
  {
# If we don't have parameters we can call parameterize_steadystate using the
# chemical identifiers. Then that function will build up a list of parameters
# so that when it calls this function a second time (recursively) we WILL have
# a list of parameters defined. The key is to make sure that the call to this
# function in parameterize_steadystate occurs after all parameters needed by
# this function are defined (or we get stuck in a recursive loop).
    parameters <- do.call(parameterize_steadystate,
                          args=purrr::compact(c(list(
                            chem.cas=chem.cas,
                            chem.name=chem.name,
                            dtxsid=dtxsid,
                            suppress.messages=suppress.messages))))
  }
  
  if (!"Clmetabolismc" %in% names(parameters))
    parameters[["Clmetabolismc"]] <- calc_hep_clearance(parameters=parameters,
                                                           hepatic.model='unscaled',
                                                           suppress.messages=TRUE)#L/h/kg body weight
  
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