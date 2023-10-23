#' Retrieve or calculate fraction of chemical absorbed from the gut
#' 
#' This function checks for chemical-specific in vivo measurements of the 
#' fraction absorbed from the
#' gut in the \code{\link{chem.physical_and_invitro.data}} table. If in vivo
#' data are unavailable (or \code{keepit100 == TRUE}) we attempt to use 
#' in vitro Caco-2 membrane permeability to predict the fractions according to
#' \code{\link{calc_oral_bioavailability}}.
#' 
#' @param parameters A list of the parameters (Caco2.Pab, Funbound.Plasma, Rblood2plasma,
#' Clint, BW, Qsmallintestine, Fabs, Fgut) used in the calculation, either supplied by user
#' or calculated in parameterize_steady_state.                                                                               
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
#' @param Caco2.Pab.default sets the default value for  Caco2.Pab if Caco2.Pab is 
#' unavailable. 
#' @param Caco2.Fabs = TRUE uses Caco2.Pab to calculate
#' fabs.oral, otherwise fabs.oral = \code{Fabs}. 
#' @param Caco2.Fgut = TRUE uses Caco2.Pab to calculate  fgut.oral, otherwise 
#' fgut.oral = \code{Fgut}. 
#' @param overwrite.invivo = TRUE overwrites Fabs and Fgut in vivo values from 
#' literature with Caco2 derived values if available. 
#' @param keepit100 TRUE overwrites Fabs and Fgut with 1 (i.e. 100 percent) 
#' regardless of other settings.
#' @param suppress.messages Whether or not the output message is suppressed.
#' 
#' @author Greg Honda and John Wambaugh
#'
#' @keywords Parameter oral_bioavailability 
#'
#' @export get_fabsgut
get_fabsgut <- function(
    parameters=NULL,
    chem.cas=NULL,
    chem.name=NULL,
    dtxsid = NULL,
    species = "Human",
    default.to.human = FALSE,
    Caco2.Pab.default = 1.6,
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

  if (is.null(parameters))
  {
    # By using parameterize_steadystate to define the parameters vector we avoid
    # a recursive loop (that is, the next time this function is called parameters
    # will already be defined).
    parameters <- parameterize_steadystate(chem.cas = chem.cas,
                                           chem.name = chem.name,
                                           dtxsid = dtxsid)  
  }
  
  out <- list()
  
  # Start with dummy values (100 percent)
  out[["Fabs"]] <- 1
  out[["Fgut"]] <- 1
  out[["Fabsgut"]] <- 1
  
  # Retrieve the chemical-specific Caco-2 value:
  out <- c(out, get_caco2(chem.cas=chem.cas,
                          chem.name=chem.name,
                          dtxsid=dtxsid,
                          Caco2.Pab.default = Caco2.Pab.default,
                          suppress.messages = suppress.messages))

    # Get the in vivo measured systemic oral bioavailability if
    # available, optionally overwriting based on Caco2.Pab
    Fbio <- try(get_invitroPK_param("Foral",species,chem.cas=chem.cas),
                silent=TRUE)
    if (!is(Fbio,"try-error") & !overwrite.invivo)
    {
      # Correct for hepatic first-pass metabolism:
      Fhep <- try(get_invitroPK_param("Fhep",species,chem.cas=chem.cas),
                silent=TRUE)
      if (is(Fhep,"try-error") | overwrite.invivo == TRUE)
      {
        if (!is.null(parameters[['hepatic.bioavailability']]))
        {
          Fhep <- parameters[['hepatic.bioavailability']] 
        } else {
          Fhep <- calc_hep_bioavailability(parameters = parameters, 
                                          chem.cas = chem.cas,
                                          chem.name = chem.name,
                                          dtxsid = dtxsid,
                                          species = species, 
                                          suppress.messages = suppress.messages)
        }
      }
      Fabsgut <- Fbio/Fhep
    } else Fabsgut <- NA
    
    # Get the fraction absorbed from the gut, preferring in vivo measured data if
    # available, otherwise attempt to use Caco2.Pab
    Fabs <- try(get_invitroPK_param("Fabs",species,chem.cas=chem.cas),
                silent=TRUE)
    if (is(Fabs,"try-error") | overwrite.invivo == TRUE){
      if (overwrite.invivo  | 
          (Caco2.Fabs & is(Fabs,"try-error")))
      {
        out[["Fabs"]] <- 1
        # Caco2 is a human cell line
        # only calculable for human, assume the same across species
        Fabs <- calc_fabs.oral(
          parameters = c(out, parameters), 
          chem.cas = chem.cas,
          chem.name = chem.name,
          dtxsid = dtxsid,
          species = species,
          suppress.messages=suppress.messages) 
      } else {
        Fabs <- 1
      }
    }
    
    # We have a hard time with Fgut, if we don't have it measured we first
    # try to set it with Fabsgut/Fabs:
    Fgut <- try(get_invitroPK_param("Fgut",species,chem.cas=chem.cas),
                silent=TRUE)
    if (is(Fgut,"try-error") | overwrite.invivo == TRUE)
    {
      if (overwrite.invivo | 
          (Caco2.Fgut & is(Fgut,"try-error")))
      {
        Fgut <- calc_fgut.oral(
          parameters = c(out, parameters), 
          chem.cas = chem.cas,
          chem.name = chem.name,
          dtxsid = dtxsid,
          species = species,
          suppress.messages=suppress.messages) 
      } else if (!is.na(Fabsgut) & !overwrite.invivo) {
        Fgut <- Fabsgut/Fabs
      } else {
        Fgut <- 1
      }
    }
    
    out[['Fabsgut']] <- Fabs*Fgut
    out[['Fabs']] <- Fabs
    out[['Fgut']] <- Fgut

  # Require that values are <= 1:
  out[names(out) %in% c("Fabsgut", "Fabs", "Fgut")] <- 
    lapply(out[names(out) %in% c("Fabsgut", "Fabs", "Fgut")], 
           function(x) suppressWarnings(ifelse(x>1, 1.0, x)))

  # Set a reasonable precision:
  out <- lapply(out, set_httk_precision)
  
  return(out)
}