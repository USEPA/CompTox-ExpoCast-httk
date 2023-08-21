#' Retrieve or caclulate fraction of chemical absorbed from the gut
#' 
#' This function tetrieves or caclulates fraction of chemical absorbed from the gut.
#' We assume that systemic oral bioavailability (\ifelse{html}{\out{F<sub>bio</sub>}}{\eqn{F_{bio}}})
#' consists of three components: 
#' 1) the fraction of chemical absorbed from intestinal lumen into enterocytes 
#' (\ifelse{html}{\out{F<sub>abs</sub>}}{\eqn{F_{abs}}}, 
#' 2) the fraction surviving intestinal metabolism 
#' (\ifelse{html}{\out{F<sub>gut</sub>}}{\eqn{F_{gut}}}), and 
#' 3) the fraction surviving first-pass hepatic metabolism 
#' (\ifelse{html}{\out{F<sub>hep</sub>}}{\eqn{F_{hep}}}). This function returns
#' \ifelse{html}{\out{F<sub>abs</sub>*F<sub>gut</sub>}}{\eqn{F_{abs}*F_{gut}}}
#' 
#' We model systemic oral bioavailability as 
#' \ifelse{html}{\out{F<sub>bio</sub>=F<sub>abs</sub>*F<sub>gut</sub>*F<sub>hep</sub>}}{\eqn{F_{bio}=F_{abs}*F_{gut}*F_{hep}}}.
#' \ifelse{html}{\out{F<sub>hep</sub>}}{\eqn{F_{hep}}}
#' is estimated from in vitro TK data using 
#' \code{\link{calc_hep_bioavailability}}.
#' If \ifelse{html}{\out{F<sub>bio</sub>}}{\eqn{F_{bio}}}
#' has been measured in vivo and is found in
#' table \code{\link{chem.physical_and_invitro.data)}} then we set 
#' \ifelse{html}{\out{F<sub>abs</sub>*F<sub>gut</sub>}}{\eqn{F_{abs}*F_{git}}} 
#' to the measured value divided by 
#' \ifelse{html}{\out{F<sub>hep</sub>}}{\eqn{F_{hep}}} 
#' Otherwise, if Caco2 membrane permeability data or predictions
#' are available \ifelse{html}{\out{F<sub>abs</sub>}}{\eqn{F_{abs}}} is estimated
#' using \code{\link{calc_fgut.oral}}.
#' Intrinsic hepatic metabolism is used to very roughly estimate
#' \ifelse{html}{\out{F<sub>gut</sub>}}{\eqn{F_{gut}}}
#' using \code{\link{calc_fgut.oral}}.
#' If argument keepit100 is used then there is complete absorption from the gut
#' (that is, \ifelse{html}{\out{F<sub>abs</sub>=F<sub>gut</sub>=1}}{\eqn{F_{abs}=F_{gut}=1}}). 
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
    
    # Get the in vivo measured sysemtic oral bioavailability if
    # available, optionally overwriting based on Caco2.Pab
    Fbio <- try(get_invitroPK_param("Fbio",species,chem.cas=chem.cas),
                silent=TRUE)
    if (!is(Fbio,"try-error") & !overwrite.invivo)
    {
      # Correct for hepatic first-pass metabolism:
      Fhep <- calc_hep_bioavailability(dtxsid=dtxsid, species=species)
      Fabsgut <- Fbio/Fhep
    } else Fabsgut <- NA
    
    # Get the fraction absorbed from the gut, preferring in vivo measured data if
    # available, otherwise attempt to use Caco2.Pab
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
    
    # We have a hard time with Fgut, if we don't have it measured we first
    # try to set it with Fabsgut/Fabs:
    Fgut <- try(get_invitroPK_param("Fgut",species,chem.cas=chem.cas),
                silent=TRUE)
    if (is(Fgut,"try-error") | overwrite.invivo == TRUE)
    {
      if (overwrite.invivo == TRUE | 
          (Caco2.Fgut == TRUE & is(Fgut,"try-error")))
      {
        Fgut <- calc_fgut.oral(
          Params = c(out, Params), 
          chem.cas = chem.cas,
          chem.name = chem.name,
          dtxsid = dtxsid,
          species = species) 
      } else if (!is.na(Fgutabs) & !overwrite.invivo) {
        Fgut <- Fabsgut/Fabs
      } else {
        Fgut <- 1
      }
    }
    
    out[['Fabsgut']] <- Fabs*Fgut
    out[['Fabs']] <- Fabs
    out[['Fgut']] <- Fgut
  }
  
  return(set_httk_precision(out))
}