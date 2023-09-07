#' Caco2 Oral Fraction Calculators
#' 
#' Caco2 is related to effective permeability based on Yang et al. (2007)
#' These functions calculate the fraction absorbed (calc_fabs.oral -- 
#' Darwich et al. (2010)), the fraction
#' surviving first pass gut metabolism (calc_fgut.oral), and the oral bioavailability
#' (calc_fbio.oral). Note that the first pass hepatic clearance is calculated within the
#' parameterization and other functions. using \code{\link{calc_hep_bioavailability}} 
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
#' @references 
#' Darwich, A. S., Neuhoff, S., Jamei, M., & Rostami-Hodjegan, A. (2010). 
#' Interplay of metabolism and transport in determining oral drug absorption 
#' and gut wall metabolism: a simulation assessment using the 
#' "Advanced Dissolution, Absorption, Metabolism (ADAM)" model. Curr Drug 
#' Metab, 11(9), 716-729. 
#'
#' Yang, J., Jamei, M., Yeo, K. R., Tucker, G. T., & Rostami-Hodjegan, A. 
#' (2007). Prediction of intestinal first-pass drug metabolism. Curr Drug 
#' Metab, 8(7), 676-684. 
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
  default.to.human = FALSE,
  suppress.messages = FALSE,
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

  if(keepit100){
    fabs.oral <- 1
    fgut.oral <- 1
  }else{
    if (overwrite.invivo | 
      (Caco2.Fabs & 
        class(try(get_invitroPK_param("Fabs",species,chem.cas=chem.cas),silent=T)) == "try-error"))
    {
      fabs.oral <- calc_fabs.oral(Params = Params) # Determine Fabs.oral
    } else {
      fabs.oral <- Params$Fabs
    }
    
    if (overwrite.invivo | 
      (Caco2.Fgut & 
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

#' @describeIn calc_fbio.oral Calculate the fraction absorbed in the gut (Darwich et al., 2010)
calc_fabs.oral <- function(Params = NULL,
  chem.cas = NULL,
  chem.name = NULL,
  dtxsid = NULL,
  species = "Human",
  default.to.human = FALSE,
  suppress.messages = FALSE,
  Caco2.Pab.default = "1.6",
  Caco2.Fgut = TRUE,
  Caco2.Fabs = TRUE,
  overwrite.invivo = FALSE,
  keepit100 = FALSE,
  ...
  )
{
  # Required parameters
  req.param <- c("Caco2.Pab")
  
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
  if(Caco2.Fabs){
    # Yang 2007 equation 10 for Caco2 pH7.4
    peff <- 10^(0.4926 * log10(Params$Caco2.Pab) - 0.1454) 
    # Fagerholm 1996 -- Yang et al. (2007) equation 14
    if (tolower(species) %in% c("rat"))
      peff <- max((peff - 0.03)/3.6, 0)
    # Darwich et al. (2010) Equation 3
    fabs.oral <- 1 - (1 + 0.54 * peff)^-7
  }else{
    fabs.oral <- 1
  }
  
  return(as.numeric(fabs.oral))
}

#' @describeIn calc_fbio.oral Calculate the fraction of chemical surviving first pass metabolism in the gut
# Is this the Yang et al. (2007) QGut Model?
calc_fgut.oral <- function(Params = NULL,
  chem.cas = NULL,
  chem.name = NULL,
  dtxsid = NULL,
  species = "Human",
  default.to.human = FALSE,
  suppress.messages = FALSE,
  Caco2.Pab.default = "1.6",
  Caco2.Fgut = TRUE,
  Caco2.Fabs = TRUE,
  overwrite.invivo = FALSE,
  keepit100 = FALSE,
  ...
  )
{
  
  if (Caco2.Fgut)
  {
    # Required parameters
    req.param <- c("BW", "Clint", "Caco2.Pab", "Funbound.plasma", "Rblood2plasma")
    
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
    
    # Scale up from in vitro Clint to a whole liver clearance:
    clu_hep <- calc_hep_clearance(parameters=Params,
                             hepatic.model='unscaled',
                             suppress.messages=TRUE) #L/h/kg body weight
    clu_hep <- clu_hep*Params$BW # L/h 
    clu_gut <- clu_hep/100 # approximate ratio of cyp abundances
    
    # Yang et al. (2007) equation 10 (our data is at pH 7.4):
    peff <- (10^(0.4926 * Params$Caco2.Pab - 0.1454)) # peff dimensional 10-4 cm/s
    
    if(tolower(species) == "rat")
    {
      # Fagerholm 1996 -- Yang et al. (2007) equation 14
      peff <- max((peff - 0.03)/3.6, 0)
      Asmallintestine <- 71/(100^2) # m2 Ref?
# IF not a rat, we assume human:
    } else {
      if (tolower(species) != "human") 
        if (!suppress.messages)
          warning("Human intestinal permeability and microvilli blood flow used to calculate fraction absorbed by gut")
      
      # m2 area intestine -- Yang et al. (2007)
      Asmallintestine <- 0.66/70*Params$BW
    }
    
    # Calculate Qvilli based on Qsmallintestine
    # Tateishi 1997 -- Human Qsmallintestine = 38 ml/min/100 g
    # Weight small intestine -- 1.5875 kg -- 15.875 100 g
    # Fraction of blood flow to gut going to small intestine:
    # For 70 kg human Qtotal.liver = 86.96 L/h (parameterize_pbtk)
    # and Qgut = 68.99 L/h (parameterize_pbtk)
    Qsmallintestinehuman <- 38.7 /1000*60*15.8757 # L/h
    Qsmallintestinef <- Qsmallintestinehuman / 68.99 # Works out to roughly 53%
    if(!is.null(Params$Qtotal.liverc)){
      this.Qsmallintestinehuman <- Qsmallintestinef*68.99/86.96*Params$Qtotal.liverc*Params$BW^(3/4)
      Qvilli <- 18/Qsmallintestinehuman*this.Qsmallintestinehuman
    } else if(!is.null(Params$Qgutf)){
      this.Qsmallintestinehuman <- Qsmallintestinef*Params$Qgutf*Params$Qcardiacc*Params$BW^(3/4)
      Qvilli <- 18/Qsmallintestinehuman*this.Qsmallintestinehuman
    } else {
      warning("Because model used does not provide Qtotal.liver or Qgutf, the Yang et al. (2007) value of 18 L/h was used to calculate fraction absorbed by gut")
      Qvilli <- 18 # L/h blood flow to microvilli of intestinal lumen
    }    
    # permeability clearance (CLperm) Yang et al. (2007) equation 8:
    CLperm <- peff * Asmallintestine * 1000/10^4/100*3600 # 10^-4 cm / s * m2 * 1000 L / m^3 / 10^4 * 1 m / 100 cm * 3600 s / h = L / h
    # Qgut "in terms of fundamental parameters" -- Yang et al. (2007) equation 6:
    Qgut <- Qvilli * CLperm / (Qvilli + CLperm)
    # Qgut Model -- Yang et al. (2007) equation 5:
 #   fgut.oral <- Qgut / (Qgut + Params$Funbound.plasma*clu_gut/Params$Rblood2plasma) 
 # Metabolism of chemical in enterocyte, not blood:
   fgut.oral <- Qgut / (Qgut + Params$Funbound.plasma*clu_gut) 
    # Add competitive process (clearance of the gut lumen):
    fgut.oral <- fgut.oral*(Qgut/(0.5+Qgut))
  } else {
    # if Caco2.options$Fgut.oral == FALSE, return 1
    fgut.oral <- 1
  }
  return(set_httk_precision(as.numeric(fgut.oral)))
 
}
