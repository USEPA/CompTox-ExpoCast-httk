#' Retrieve or calculate fraction of chemical absorbed from the gut
#' 
#' This function checks for chemical-specific in vivo measurements of the 
#' fraction absorbed from the
#' gut in the \code{\link{chem.physical_and_invitro.data}} table. If in vivo
#' data are unavailable (or \code{keepit100 == TRUE}) we attempt to use 
#' in vitro Caco-2 membrane permeability to predict the fractions according to
#' \code{\link{calc_fbio.oral}}.
#' 
#' @param parameters A list of the parameters (Caco2.Pab, Funbound.Plasma, Rblood2plasma,
#' Clint, BW, Qsmallintestine, Fabs, Fgut) used in the calculation, either supplied by user
#' or calculated in parameterize_steady_state.                                                                               
#' @param chem.cas Chemical Abstract Services Registry Number (CAS-RN) -- the 
#' chemical must be identified by either CAS, name, or DTXISD
#' @param chem.name Chemical name (spaces and capitalization ignored) --  the 
#' chemical must be identified by either CAS, name, or DTXISD
#' @param dtxsid EPA's DSSTox Structure ID (\url{https://comptox.epa.gov/dashboard})  
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
#' @export get_fbio
get_fbio <- function(
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
  out[["kgutabs"]] <- 2.18 # Wambaugh et al. (2018)
  
  # Retrieve the chemical-specific Caco-2 value:
  Caco2.Pab <- get_caco2(chem.cas=chem.cas,
                          chem.name=chem.name,
                          dtxsid=dtxsid,
                          Caco2.Pab.default = Caco2.Pab.default,
                          suppress.messages = suppress.messages)
  out <- c(out, Caco2.Pab)
  parameters <- c(parameters, Caco2.Pab)
  
  # Only bother with the remaining code if keepit100=FALSE
  if (!keepit100)
  {
    caco2.vals <- calc_fbio.oral(parameters = parameters,
                                 chem.cas=chem.cas,
                                 chem.name=chem.name,
                                 dtxsid=dtxsid,
                                 species = species,
                                 default.to.human = default.to.human,
                                 suppress.messages = suppress.messages)


    # Attempt to use the in vivo measured hepatic bioavailability (first-pass
    # hepatic metbaolism):
    Fhep <- try(get_invitroPK_param("Fhep",species,chem.cas=chem.cas),
              silent=TRUE)
    # If we don't have an in vivo value or are overwriting it:
    if (is(Fhep,"try-error") | overwrite.invivo == TRUE)
    {
      # Do we already have Fhep?
      if (!is.null(parameters[['hepatic.bioavailability']]))
      {
        Fhep <- parameters[['hepatic.bioavailability']]
      # If not, calculate it:
      } else {
        Fhep <- calc_hep_bioavailability(parameters = parameters, 
                                        chem.cas = chem.cas,
                                        chem.name = chem.name,
                                        dtxsid = dtxsid,
                                        species = species, 
                                        suppress.messages = suppress.messages)
      }
    }

    # Get the in vivo measured systemic oral bioavailability if
    # available, optionally overwriting based on Caco2.Pab
    Fbio <- try(get_invitroPK_param("Foral",species,chem.cas=chem.cas),
                silent=TRUE)
    # Because we model Fbio as Fabs*Fgut*Fhep we calculate Fabs*Fgut from Fbio:
    if (!is(Fbio,"try-error") & !overwrite.invivo)
    {
      # Correct for hepatic first-pass metabolism:
      # Fabsgut is the product Fabs*Fgut (without Fhep):
      Fabsgut <- Fbio/Fhep
    } else if (is(Fbio,"try-error") | !overwrite.invivo) 
    {
      Fbio <- caco2.vals[["fbio.oral"]]
      # Set to NA so we will calculate later using Fabs and Fgut:
      Fabsgut <- NA
    }
      
    # Get the fraction absorbed from the gut, preferring in vivo measured data if
    # available, otherwise attempt to use Caco2.Pab
    Fabs <- try(get_invitroPK_param("Fabs",species,chem.cas=chem.cas),
                silent=TRUE)
    # If we don't have an in vivo value or are overwriting it:
    if (is(Fabs,"try-error") | overwrite.invivo == TRUE)
    {
      Fabs <- caco2.vals[["fabs.oral"]]
    }
      
    # We have a hard time with Fgut, if we don't have it measured we first
    # try to set it with Fabsgut/Fabs:
    Fgut <- try(get_invitroPK_param("Fgut",species,chem.cas=chem.cas),
                silent=TRUE)
    # If we don't have an in vivo value of Fgut but we do have an in vivo 
    # estimate of Fabsgut, can use Fabs to calculate this:
    if (is(Fgut,"try-error") & !is.na(Fabsgut) & !overwrite.invivo) 
    {
      Fgut <- Fabsgut/Fabs 
    # If we don't have an in vivo value or are overwriting it:
    } else if (is(Fgut,"try-error") | overwrite.invivo == TRUE)
    {
      Fgut <- caco2.vals[["fgut.oral"]]
    }
     
    # If we don't have an in vivo Fabsgut then calculate it: 
    if (is.na(Fabsgut)) Fabsgut <- Fabs*Fgut
    
    out[['Fabsgut']] <- Fabsgut 
    out[['Fabs']] <- Fabs
    out[['Fgut']] <- Fgut
  
    # Require that values are <= 1:
    out[names(out) %in% c("Fabsgut", "Fabs", "Fgut")] <- 
      lapply(out[names(out) %in% c("Fabsgut", "Fabs", "Fgut")], 
             function(x) suppressWarnings(ifelse(x>1, 1.0, x)))

    # Get the in vivo measured absorption rate, defaulting to average value
    # from Wambaugh et al. (2018):
    kgutabs <- try(get_invitroPK_param("kgutabs",
                                       species,
                                       chem.cas=chem.cas),
                   silent=TRUE)
    # If we have an in vivo value and overwrite invivo = FALSE: 
    if (is(kgutabs,"try-error") | overwrite.invivo == TRUE)
    {
      kgutabs <- caco2.vals[["kgutabs"]]
    }
    
    out[["kgutabs"]] <- kgutabs
  }

  # Set a reasonable precision:
  out <- lapply(out, set_httk_precision)
      
  return(out)
}