#' Parameterize_SteadyState
#' 
#' These functions calculate the fraction absorbed (calc_fabs.oral), the fraction
#' surviving first pass gut metabolism (calc_fgut.oral), and the oral bioavailability
#' (calc_fbio.oral). Note that the first pass hepatic clearance is calculated within the
#' parameterization and other functions. Options are set in the list Caco2.options.
#' 
#' @param Params A list of the parameters (Caco2.Pab, Funbound.Plasma, Rblood2plasma,
#' Clint, BW, Qsmallintestine, Fabs, Fgut) used in the calculation, either supplied by user
#' or calculated in parameterize_steady_state.
#' @param chem.cas Either the chemical name or the CAS number must be
#' specified.
#' @param chem.name Either the chemical name or the CAS number must be
#' specified.
#' @param species Species desired (either "Rat", "Rabbit", "Dog", "Mouse", or
#' default "Human").
#' @param clint.pvalue.threshold Hepatic clearances with clearance assays
#' having p-values greater than the threshold are set to zero.
#' @param default.to.human Substitutes missing rat values with human values if
#' true.
#' @param human.clint.fup Uses human hepatic intrinsic clearance and fraction
#' of unbound plasma in calculation of partition coefficients for rats if true.
#' @param adjusted.Funbound.plasma Returns adjusted Funbound.plasma when set to
#' TRUE.
#' @param restrictive.clearance In calculating hepatic.bioavailability, protein
#' binding is not taken into account (set to 1) in liver clearance if FALSE.
#' @param minimum.Funbound.plasma Monte Carlo draws less than this value are set 
#' equal to this value (default is 0.0001 -- half the lowest measured Fup in our
#' dataset).
#' @param Caco2.options A list of options to use when working with Caco2 apical to
#' basolateral data \code{Caco2.Pab}, default is Caco2.options = list(Caco2.default = 2,
#' Caco2.Fabs = TRUE, Caco2.Fgut = TRUE, overwrite.invivo = FALSE, keepit100 = FALSE). Caco2.default sets the default value for 
#' Caco2.Pab if Caco2.Pab is unavailable. Caco2.Fabs = TRUE uses Caco2.Pab to calculate
#' fabs.oral, otherwise fabs.oral = \code{Fabs}. Caco2.Fgut = TRUE uses Caco2.Pab to calculate 
#' fgut.oral, otherwise fgut.oral = \code{Fgut}. overwrite.invivo = TRUE overwrites Fabs and Fgut in vivo values from literature with 
#' Caco2 derived values if available. keepit100 = TRUE overwrites Fabs and Fgut with 1 (i.e. 100 percent) regardless of other settings.
#' 
#'
#' @return \item{fbio.oral}{Oral bioavailability, the fraction of oral dose 
#' reaching systemic distribution in the body.} \item{fabs.oral}{Fraction of dose absorbed, 
#' i.e. the fraction of the dose that enters the gutlumen.} \item{fgut.oral}{Fraction of 
#' chemical surviving first pass metabolism in the gut.} \item{fhep.oral}{Fraction of chemical
#' surviving first pass hepatic clearance.}
#' @author Gregory Honda
#' @keywords Parameter
#' @examples
#' 
#'  fbio1 <- calc_fbio.oral(chem.cas='80-05-7',
#'                          Caco2.options = list(Caco2.Pab.default = 2,
#'                                               Caco2.Fabs = TRUE,
#'                                               Caco2.Fgut = TRUE))
#'  fbio2 <- calc_fbio.oral(chem.cas='80-05-7',
#'                          Caco2.options = list(Caco2.Pab.default = 2,
#'                                               Caco2.Fabs = FALSE,
#'                                               Caco2.Fgut = FALSE))
#' 
#' @export calc_fbio.oral
#' @export calc_fabs.oral
#' @export calc_fgut.oral
#' 
calc_fbio.oral <- function(Params = NULL,
  chem.cas = NULL,
  chem.name = NULL,
  dtxsid = NULL,
  species = "Human",
  default.to.human = F,
  suppress.messages = F,
  Caco2.Pab.default = "1.6",
  Caco2.Fgut = TRUE,
  Caco2.Fabs = TRUE,
  overwrite.invivo = FALSE,
  keepit100 = FALSE,
  ...
  )
{
  
  # Initialize parameters if null
  if(is.null(Params)){
  # We need to describe the chemical to be simulated one way or another:
    if (is.null(chem.cas) & 
        is.null(chem.name) & 
        is.null(dtxsid)) 
      stop('chem.name, chem.cas, or dtxsid must be specified.')
  
  # Look up the chemical name/CAS, depending on what was provide:
      out <- get_chem_id(
              chem.cas=chem.cas,
              chem.name=chem.name,
              dtxsid=dtxsid)
      chem.cas <- out$chem.cas
      chem.name <- out$chem.name                                
      dtxsid <- out$dtxsid
    
    Params <- do.call(parameterize_steadystate, args=purrr::compact(c(list(
      chem.cas = chem.cas,
      chem.name = chem.name,
      dtxsid = dtxsid,
      species = species,
      default.to.human = default.to.human,
      suppress.messages = suppress.messages,
      Caco2.options = list(
        Caco2.Pab.default = Caco2.Pab.default,
        Caco2.Fgut = Caco2.Fgut,
        Caco2.Fabs = Caco2.Fabs,
        overwrite.invivo = overwrite.invivo,
        keepit100 = keepit100)),
      ...)))
  }

  if(keepit100 == TRUE){
    fabs.oral <- 1
    fgut.oral <- 1
  }else{
    if (overwrite.invivo == TRUE | 
      (Caco2.Fabs == TRUE & 
        class(try(get_invitroPK_param("Fabs",species,chem.cas=chem.cas),silent=T)) == "try-error"))
    {
      fabs.oral <- calc_fabs.oral(Params = Params) # Determine Fabs.oral
    } else {
      fabs.oral <- Params$Fabs
    }
    
    if (overwrite.invivo == TRUE | 
      (Caco2.Fgut == TRUE & 
        class(try(get_invitroPK_param("Fgut",species,chem.cas=chem.cas),silent=T)) == "try-error"))
    {
      fgut.oral <- calc_fgut.oral(Params = Params) # Determine Fgut.oral
    } else {
      fgut.oral <- Params$Fgut
    }
  }
  
  fhep.oral <- Params$hepatic.bioavailability # Determine Fhep.oral
  fbio.oral <- fabs.oral*fhep.oral*fgut.oral # Determine Fbio.oral
  
  return(list("fbio.oral" = fbio.oral,
              "fabs.oral" = fabs.oral,
              "fgut.oral" = fgut.oral,
              "fhep.oral" = fhep.oral
              ))

}

# Calculate the fraction absorbed in the gut
calc_fabs.oral <- function(Params = NULL,
  chem.cas = NULL,
  chem.name = NULL,
  dtxsid = NULL,
  species = "Human",
  default.to.human = F,
  suppress.messages = F,
  Caco2.Pab.default = "1.6",
  Caco2.Fgut = TRUE,
  Caco2.Fabs = TRUE,
  overwrite.invivo = FALSE,
  keepit100 = FALSE,
  ...
  )
{
  # Required parameters
  req.param <- c("Caco2.Pab", "Fabs")
  
  # Header initialization  
  if(is.null(Params) | !all(req.param %in% names(Params))){
  # We need to describe the chemical to be simulated one way or another:
      if (is.null(chem.cas) & 
          is.null(chem.name) & 
          is.null(dtxsid)) 
        stop('chem.name, chem.cas, or dtxsid must be specified.')
    
    # Look up the chemical name/CAS, depending on what was provide:
        out <- get_chem_id(
                chem.cas=chem.cas,
                chem.name=chem.name,
                dtxsid=dtxsid)
        chem.cas <- out$chem.cas
        chem.name <- out$chem.name                                
        dtxsid <- out$dtxsid
      
    Params <- do.call(parameterize_steadystate, args=purrr::compact(c(list(
      chem.cas = chem.cas,
      chem.name = chem.name,
      dtxsid = dtxsid,
      species = species,
      default.to.human = default.to.human,
      suppress.messages = suppress.messages,
      Caco2.options = list(
        Caco2.Pab.default = Caco2.Pab.default,
        Caco2.Fgut = Caco2.Fgut,
        Caco2.Fabs = Caco2.Fabs,
        overwrite.invivo = overwrite.invivo,
        keepit100 = keepit100)),
      ...)))
  }
  
  # Detetermine Fabs.oral based on Caco2 data, or keep as Fabs
  if(Caco2.Fabs == TRUE){
    peffh <- 10^(0.4926 * Params$Caco2.Pab - 0.1454) # Yang 2007 for Caco2 pH7.4
    permh <- 0.66 * peffh * 3.6
    fabs.oral <- 1 - (1 + 0.54 * peffh)^-7
  }else{
    fabs.oral <- Params$Fabs
  }
  return(as.numeric(fabs.oral))
  
}

# Calculate the fraction of chemical surviving first pass metabolism in the gut
calc_fgut.oral <- function(Params = NULL,
  chem.cas = NULL,
  chem.name = NULL,
  dtxsid = NULL,
  species = "Human",
  default.to.human = F,
  suppress.messages = F,
  Caco2.Pab.default = "1.6",
  Caco2.Fgut = TRUE,
  Caco2.Fabs = TRUE,
  overwrite.invivo = FALSE,
  keepit100 = FALSE,
  ...
  )
{
  
  if(Caco2.Fgut == TRUE){
    # Required parameters
    req.param <- c("BW", "Clint", "Caco2.Pab", "Fgut", "Funbound.plasma", "Rblood2plasma")
    
    # Header initialization  
    if(is.null(Params) | !all(req.param %in% names(Params))){
# We need to describe the chemical to be simulated one way or another:
      if (is.null(chem.cas) & 
          is.null(chem.name) & 
          is.null(dtxsid)) 
        stop('chem.name, chem.cas, or dtxsid must be specified.')
    
    # Look up the chemical name/CAS, depending on what was provide:
        out <- get_chem_id(
                chem.cas=chem.cas,
                chem.name=chem.name,
                dtxsid=dtxsid)
        chem.cas <- out$chem.cas
        chem.name <- out$chem.name                                
        dtxsid <- out$dtxsid

    Params <- do.call(parameterize_steadystate, args=purrr::compact(c(list(
      chem.cas = chem.cas,
      chem.name = chem.name,
      dtxsid = dtxsid,
      species = species,
      default.to.human = default.to.human,
      suppress.messages = suppress.messages,
      Caco2.options = list(
        Caco2.Pab.default = Caco2.Pab.default,
        Caco2.Fgut = Caco2.Fgut,
        Caco2.Fabs = Caco2.Fabs,
        overwrite.invivo = overwrite.invivo,
        keepit100 = keepit100)),
      ...)))
    }
    
    clu_hep <- Params$Clint*Params$BW # L/h for 70 kg human
    clu_gut <- clu_hep/100 # approximate ratio of cyp abundances
    
    if(tolower(species) == "rat"){
      peffh <- (10^(0.6532 * Params$Caco2.Pab - 0.3036)) # peff dimensional 10-4 cm/s
      peffr <- peffh/3.6 # Fagerhol 1996
      permr <- 71/(100^2)*peffr*3.6
      fgut.oral <- 0.65/(0.65+Params$Funbound.plasma*clu_gut/Params$Rblood2plasma*(1+.65/permr))
      
    }else{
      
      # Calculate Qvilli based on Qsmallintestine, or use default for ~70 kg human
      if(!is.null(Params$Qsmallintestine)){
        Qvilli <- (18/37.5)*Params$Qsmallintestine
      }else{
        Qvilli <- 18 # L/h blood flow to microvilli of intestinal lumen
      }
      
      peffh <- (10^(0.6532 * Params$Caco2.Pab - 0.3036)) # peff dimensional 10-4 cm/s
      permh <- 0.66*peffh*3.6 # L/h, 0.66 m2 area intestine
      fgut.oral <- Qvilli/(Qvilli+Params$Funbound.plasma*clu_gut/Params$Rblood2plasma*(1+Qvilli/permh))
      
    }
  }else{
    # if Caco2.options$Fgut.oral == FALSE, return 1
    fgut.oral <- Params$Fgut
  }
  return(as.numeric(fgut.oral))
 
}





















