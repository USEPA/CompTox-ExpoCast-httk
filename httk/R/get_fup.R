#' Retrieve and parse fraction unbound in plasma
#' 
#' This function retrieves the chemical- and species-specific fraction
#' unbound in plasma (\ifelse{html}{\out{f<sub>up</sub>}}{\eqn{f_{up}}}) 
#' from \code{\link{chem.physical_and_invitro.data}}. 
#' If that parameter is described by a distribution (that is, a median, 
#' lower-, and upper-95th percentile separated by commas) this 
#' function splits those quantiles into separate values.
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
#' @param default.to.human Substitutes missing fraction of unbound plasma with
#' human values if true.
#' 
#' @param force.human.fup If a non-human species value (matching argument
#' species) is available, it is ignored and the human fraction unbound is
#' returned
#' 
#' @param suppress.messages Whether or not the output message is suppressed.
#' 
#' @param minimum.Funbound.plasma \ifelse{html}{\out{f<sub>up</sub>}}{\eqn{f_{up}}}
#' is not allowed to drop below this value (default is 0.0001).
#' 
#' @return list containing:
#' \item{Funbound.plasma.point}{Point estimate (central tendency) of the Unbound fraction in plasma}
#' \item{Funbound.plasma.dist}{Quantiles of a distribution (median, lower and upper 95th percentiles) for the unbound fraction}
#'
#' @author John Wambaugh
#'
#' @keywords Parameter in-vitro
#'
#' @seealso \code{\link{chem.physical_and_invitro.data}}
get_fup <- function(chem.cas=NULL,
                    chem.name=NULL,
                    dtxsid = NULL,
                    species="Human",
                    default.to.human=FALSE,
                    force.human.fup=FALSE,
                    suppress.messages=FALSE,
                    minimum.Funbound.plasma=0.0001)
{
 # We need to describe the chemical to be simulated one way or another:
  if (is.null(chem.cas) & 
      is.null(chem.name) & 
      is.null(dtxsid)) 
    stop('chem.name, chem.cas, or dtxsid must be specified.')
    
  # Look up the chemical name/CAS, depending on what was provided:
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

  # unitless fraction of chemical unbound with plasma
  fup.db <- try(
    get_invitroPK_param(
      "Funbound.plasma",
      species,
      chem.cas=chem.cas,
      chem.name=chem.name,
      dtxsid=dtxsid),
    silent=TRUE)
  if ((is(fup.db,"try-error") & default.to.human) || force.human.fup) 
  {
    fup.db <- try(
      get_invitroPK_param(
        "Funbound.plasma",
        "Human",
        chem.cas=chem.cas,
        chem.name=chem.name,
        dtxsid=dtxsid),
      silent=TRUE)
    if (!suppress.messages) 
      warning(paste(species,"coerced to Human for protein binding data."))

    # If we couldn't retrieve fup.db and fup wasn't provided as a parameter:
    if (is(fup.db,"try-error")) 
      stop("Missing protein binding data for given species. Set default.to.human to true to substitute human value.")
  }

  # Check if fup is a point value or a distribution, if a distribution, use the median:
  if (nchar(fup.db) - nchar(gsub(",","",fup.db))==2) 
  {
    fup.point <- as.numeric(strsplit(fup.db,",")[[1]][1])
    fup.dist <- fup.db
    if (!suppress.messages) 
      warning("Fraction unbound is provided as a distribution.")
  } else {
    fup.point <- fup.db
    fup.dist <- NA 
  }
  
  # "0" is a special value that indicates a below limit of detection measurment
  # If species-specific fup is 0 try the human value:  
  if (fup.point == 0 & tolower(species)!="human" & default.to.human) 
  {
    if (!suppress.messages) 
      warning(paste("Fraction unbound below limit of detection for",species,"replaced with human value."))
     fup.db <- try(
                get_invitroPK_param(
                  "Funbound.plasma",
                  "Human",
                  chem.cas=chem.cas,
                  chem.name=chem.name,
                  dtxsid=dtxsid),
                silent=TRUE)
  # Check if fup is a point value or a distribution, if a distribution, use the median:
    if (nchar(fup.db) - nchar(gsub(",","",fup.db))==2) 
    {
      fup.point <- as.numeric(strsplit(fup.db,",")[[1]][1])
      fup.dist <- fup.db
      if (!suppress.messages) 
        warning("Fraction unbound is provided as a distribution.")
    } else {
      fup.point <- fup.db
      fup.dist <- NA 
    }
  }

  # Check if still 0:
  if (fup.point == 0)
  {
    if (!suppress.messages) 
      warning("Fraction unbound below limit of detection.")
  } else if (fup.point < minimum.Funbound.plasma) { 
   # Restrict the value of fup:
   fup.point <- minimum.Funbound.plasma
  }
  
  return(list(Funbound.plasma.point = set_httk_precision(fup.point),
              Funbound.plasma.dist = set_httk_precision(fup.dist)))
}
