#' Get ratio of the blood concentration to the plasma concentration.
#' 
#' This function attempts to retrieve a measured species- and chemical-specific 
#' blood:plasma concentration ratio from \code{\link{chem.phys_and_invitro.data}}.
#' If not available, Rblood2plasma is returned as NA.
#' 
#' @param chem.cas Chemical Abstract Services Registry Number (CAS-RN) -- if
#'  parameters is not specified then the chemical must be identified by either
#'  CAS, name, or DTXISD
#'  
#' @param chem.name Chemical name (spaces and capitalization ignored) --  if
#'  parameters is not specified then the chemical must be identified by either
#'  CAS, name, or DTXISD
#'  
#' @param dtxsid EPA's DSSTox Structure ID (\url{https://comptox.epa.gov/dashboard})  
#'  -- if parameters is not specified then the chemical must be identified by 
#' either CAS, name, or DTXSIDs
#' 
#' @param species Species desired (either "Rat", "Rabbit", "Dog", "Mouse", or
#' default "Human"). 
#' 
#' @param default.to.human Substitutes missing animal values with human values
#' if true.
#' 
#' @param suppress.messages Whether or not the output message is suppressed.
#'
#' @return
#' A numeric value for the steady-state ratio of chemical concentration in blood
#' to plasma
#' 
#' @author Robert Pearce, Annabel Meade
#'
#' @keywords Parameter in-vitro
#'
#' @examples
#' 
#' get_rblood2plasma(chem.name="Bisphenol A")
#' get_rblood2plasma(chem.name="Bisphenol A",species="Rat")
#' 
#' @seealso \code{\link{chem.phys_and_invitro.data}}
#' 
#' @export get_rblood2plasma


get_rblood2plasma <- function(
                       chem.name=NULL,
                       chem.cas=NULL,
                       dtxsid=NULL,
                       species='Human',
                       default.to.human=FALSE,
                       suppress.messages=FALSE)
{
# We need to describe the chemical to be simulated one way or another:
  if (is.null(chem.cas) & 
      is.null(chem.name) & 
      is.null(dtxsid))
    stop('chem.name, chem.cas, or dtxsid must be specified.')

# Look up the chemical name/CAS, depending on what was provide:
  if (any(is.null(chem.cas),is.null(chem.name),is.null(dtxsid)))
  {
    out <- get_chem_id(
          chem.cas=chem.cas,
          chem.name=chem.name,
          dtxsid=dtxsid)
  chem.cas <- out$chem.cas
  chem.name <- out$chem.name                                
  dtxsid <- out$dtxsid  
  }
  
  Rblood2plasma <- try(
    get_invitroPK_param(
      "Rblood2plasma",
      species,
      chem.cas=chem.cas,
      chem.name=chem.name,
      dtxsid=dtxsid),
    silent=TRUE)
  if (is(Rblood2plasma,"try-error") & default.to.human){
    Rblood2plasma <- try(
      get_invitroPK_param(
        "Rblood2plasma",
        "Human",
        chem.cas=chem.cas,
        chem.name=chem.name,
        dtxsid=dtxsid),
      silent=TRUE)
    if (!suppress.messages) 
      warning(paste(species,"coerced to Human for Rblood2plasma."))
  }
  
  # If we couldn't retrieve Rblood2plasma and Rblood2plasma wasn't provided as a parameter:
  if (is(Rblood2plasma,"try-error")) 
    Rblood2plasma = NA 
  
  return(Rblood2plasma)
}
