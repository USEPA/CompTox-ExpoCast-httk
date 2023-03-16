#' get_fabsgut
#' 
#' This function uses Caco2 permeability to estimate oral absorption fraction.
#' 
#' @param chem.cas Chemical Abstract Services Registry Number (CAS-RN) -- the 
#' chemical must be identified by either CAS, name, or DTXISD
#' @param chem.name Chemical name (spaces and capitalization ignored) --  the 
#' chemical must be identified by either CAS, name, or DTXISD
#' @param dtxsid EPA's DSSTox Structure ID (\url{http://comptox.epa.gov/dashboard})  
#' -- the chemical must be identified by either CAS, name, or DTXSIDs
#' @param species Species desired (either "Rat", "Rabbit", "Dog", "Mouse", or
#' default "Human").
#' @param default.to.human Substitutes missing rat values with human values if
#' true.
#' @param Caco2.default sets the default value for  Caco2.Pab if Caco2.Pab is 
#' unavailable. 
#' @param Caco2.Fabs = TRUE uses Caco2.Pab to calculate
#' fabs.oral, otherwise fabs.oral = \code{Fabs}. 
#' @param Caco2.Fgut = TRUE uses Caco2.Pab to calculate  fgut.oral, otherwise 
#' fgut.oral = \code{Fgut}. 
#' @param overwrite.invivo = TRUE overwrites Fabs and Fgut in vivo values from 
#' literature with Caco2 derived values if available. 
#' @param keepit100 TRUE overwrites Fabs and Fgut with 1 (i.e. 100 percent) 
#' regardless of other settings.
#' 
#' @author Greg Honda and John Wambaugh
#'
#' @keywords Parameter oral_bioavailability 
#'
#' @export get_fabsgut
get_fabsgut <- function(
  Params=NULL,
  chem.cas=NULL,
  chem.name=NULL,
  dtxsid = NULL,
  species = "Human",
  default.to.human = FALSE,
  Caco2.Pab.default = "1.6",
  Caco2.Fgut = TRUE,
  Caco2.Fabs = TRUE,
  overwrite.invivo = FALSE,
  keepit100 = FALSE,
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
  
  if(keepit100 == TRUE)
  {
    out[["Fabs"]] <- 1
    out[["Fgut"]] <- 1
    out[["Fabsgut"]] <- 1
    out[["Caco2.Pab"]] <- 10
    out[["Caco2.Pab.dist"]] <- NA
  } else {
    # Caco-2 Pab:
    Caco2.Pab.db <- try(get_invitroPK_param(
        "Caco2.Pab", 
        species = "Human", 
        chem.cas=chem.cas,
        chem.name=chem.name,
        dtxsid=dtxsid), 
      silent = TRUE)
    if (is(Caco2.Pab.db,"try-error")){  
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
    
    # Select Fabs, optionally overwrite based on Caco2.Pab
    Fabs <- try(get_invitroPK_param("Fabs",species,chem.cas=chem.cas),
                silent=TRUE)
    if (is(Fabs,"try-error") | overwrite.invivo == TRUE){
      if (overwrite.invivo == TRUE | 
        (Caco2.Fabs == TRUE & is(Fabs,"try-error")))
      {
        out[["Fabs"]] <- 1
        # Caco2 is a human cell line
        # only calculable for human, assume the same across species
        Fabs <- calc_fabs.oral(
          Params = c(out, Params), 
          chem.cas = chem.cas,
          chem.name = chem.name,
          dtxsid = dtxsid,
          species = "Human") 
      } else {
        Fabs <- 1
      }
    }
    
    Fgut <- try(get_invitroPK_param("Fgut",species,chem.cas=chem.cas),
                silent=TRUE)
    if (is(Fgut,"try-error") | overwrite.invivo == TRUE)
    {
      if (overwrite.invivo == TRUE | 
        (Caco2.Fgut == TRUE & is(Fgut,"try-error")))
      {
        out[["Fgut"]] <- 1
        Fgut <- calc_fgut.oral(
          Params = c(out, Params), 
          chem.cas = chem.cas,
          chem.name = chem.name,
          dtxsid = dtxsid,
          species = species) 
      }else{
        Fgut <- 1
      }
    }
    out[['Fabsgut']] <- Fabs*Fgut
    out[['Fabs']] <- Fabs
    out[['Fgut']] <- Fgut
  }
  
  return(out)
}