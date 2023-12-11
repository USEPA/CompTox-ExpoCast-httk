#' Retrieve in vitro measured Caco-2 membrane permeabilit
#' 
#' This function checks for chemical-specific in vitro measurements of the 
#' Caco-2 membrane permeability 
#' in the \code{\link{chem.physical_and_invitro.data}} table. If no value is 
#' available argument \code{Caco2.Pab.default} is returned.
#' 
#' @param chem.cas Chemical Abstract Services Registry Number (CAS-RN) -- the 
#' chemical must be identified by either CAS, name, or DTXISD
#' 
#' @param chem.name Chemical name (spaces and capitalization ignored) --  the 
#' chemical must be identified by either CAS, name, or DTXISD
#' 
#' @param dtxsid EPA's DSSTox Structure ID (\url{https://comptox.epa.gov/dashboard})  
#' -- the chemical must be identified by either CAS, name, or DTXSIDs
#' 
#' @param Caco2.Pab.default sets the default value for  Caco2.Pab if Caco2.Pab is 
#' unavailable. 
#' 
#' @param suppress.messages Whether or not the output message is suppressed.
#' 
#' @author John Wambaugh
#'
#' @keywords Parameter oral_bioavailability 
#'
#' @export get_caco2
get_caco2 <- function(
    chem.cas=NULL,
    chem.name=NULL,
    dtxsid = NULL,
    Caco2.Pab.default = 1.6,
    suppress.messages=FALSE)
{
  # We need to describe the chemical to be simulated one way or another:
  if (is.null(chem.cas) & 
      is.null(chem.name) & 
      is.null(dtxsid)) 
    stop('chem.name, chem.cas, or dtxsid must be specified.')
  
  # Look up the chemical name/CAS, depending on what was provide:
  chem.ids <- get_chem_id(
    chem.cas=chem.cas,
    chem.name=chem.name,
    dtxsid=dtxsid)
  chem.cas <- chem.ids$chem.cas
  chem.name <- chem.ids$chem.name                                
  dtxsid <- chem.ids$dtxsid
  
  out <- list()

  out[["Caco2.Pab"]] <- Caco2.Pab.default
  out[["Caco2.Pab.dist"]] <- NA

    # Caco-2 Pab:
    Caco2.Pab.db <- try(get_invitroPK_param(
      "Caco2.Pab", 
      species = "Human", 
      chem.cas=chem.cas,
      chem.name=chem.name,
      dtxsid=dtxsid), 
      silent = TRUE)
    if (is(Caco2.Pab.db,"try-error"))
    {  
      Caco2.Pab.db <- as.character(Caco2.Pab.default)
      if (!suppress.messages) warning(paste0(
        "Default value of ", 
        Caco2.Pab.default, 
        " used for Caco2 permeability."))
    }
    # Check if Caco2 a point value or a distribution, if a distribution, use the median:
    if (nchar(Caco2.Pab.db) - nchar(gsub(",","",Caco2.Pab.db)) == 2) 
    {
      Caco2.Pab.dist <- Caco2.Pab.db
      Caco2.Pab.point <- as.numeric(strsplit(Caco2.Pab.db,",")[[1]][1])
      if (!suppress.messages) warning("Clint is provided as a distribution.")
    } else {
      Caco2.Pab.point <- as.numeric(Caco2.Pab.db)
      Caco2.Pab.dist <- NA
    }
    
    out[["Caco2.Pab"]] <- Caco2.Pab.point
    out[["Caco2.Pab.dist"]] <- Caco2.Pab.dist

      # Set a reasonable precision:
  out <- lapply(out, set_httk_precision)
  
  return(out)
}