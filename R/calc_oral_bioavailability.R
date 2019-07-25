#' Parameterize_SteadyState
#' 
#' These functions calculate the fraction absorbed (calc_fabs.oral), the fraction
#' surviving first pass gut metabolism (calc_fgut.oral), and the oral bioavailability
#' (calc_fbio.oral). Note that the first pass hepatic clearance is calculated within the
#' parameterization and other functions. Options are set in the list Caco2.options.
#' 
#' @param Params A list of the parameters (Caco2.Pab, Funbound.Plasma, Rblood2plasma,
#' cl_us, BW, Qsmallintestine, Fgutabs) used in the calculation, either supplied by user
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
#' basolateral data \item{Caco2.Pab}, default is Caco2.options = list(Caco2.default = 2,
#' Caco2.Fabs = TRUE, Caco2.Fgut = TRUE). Caco2.default sets the default value for 
#' Caco2.Pab if Caco2.Pab is unavailable. Caco2.Fabs = TRUE uses Caco2.Pab to calculate
#' fabs.oral, otherwise fabs.oral = \item {Fgutabs}. Caco2.Fgut = TRUE uses Caco2.Pab to calculate 
#' fgut.oral, otherwise fgut.oral = 1.
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
                           species = "Human",
                           clint.pvalue.threshold = 0.05,
                           default.to.human = F,
                           human.clint.fup = F,
                           adjusted.Funbound.plasma = T,
                           restrictive.clearance = T,
                           fup.lod.default = 0.005,
                           suppress.messages = F,
                           minimum.Funbound.plasma = 0.0001,
                           Caco2.options = list(Caco2.Pab.default = 1.6,
                                                Caco2.Fgut = TRUE,
                                                Caco2.Fabs = TRUE,
                                                overwrite.invivo = FALSE)
){
  # Initialize parameters if null
  if(is.null(Params)){
    out <- get_chem_id(chem.cas=chem.cas,chem.name=chem.name)
    chem.cas <- out$chem.cas
    chem.name <- out$chem.name
    Params <- parameterize_steadystate(chem.cas = chem.cas,
                                       chem.name = chem.name,
                                       species = species,
                                       clint.pvalue.threshold = clint.pvalue.threshold,
                                       default.to.human = default.to.human,
                                       human.clint.fup = human.clint.fup,
                                       adjusted.Funbound.plasma = adjusted.Funbound.plasma,
                                       restrictive.clearance = restrictive.clearance,
                                       fup.lod.default = fup.lod.default,
                                       suppress.messages = suppress.messages,
                                       minimum.Funbound.plasma = minimum.Funbound.plasma,
                                       Caco2.options = list(Caco2.Pab.default = Caco2.options$Caco2.Pab.default,
                                                            Caco2.Fgut = FALSE,
                                                            Caco2.Fabs = FALSE,
                                                            ovewrite.invivo = FALSE))
  }

  fabs.oral <- calc_fabs.oral(Params = Params) # Determine Fabs.oral
  fhep.oral <- Params$hepatic.bioavailability # Determine Fhep.oral
  fgut.oral <- calc_fgut.oral(Params = Params) # Determine Fgut.roal
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
                           species = "Human",
                           clint.pvalue.threshold=0.05,
                           default.to.human=F,
                           human.clint.fup=F,
                           adjusted.Funbound.plasma=T,
                           restrictive.clearance=T,
                           fup.lod.default=0.005,
                           suppress.messages=F,
                           minimum.Funbound.plasma=0.0001,
                           Caco2.options = list(Caco2.Pab.default = 1.6,
                                                Caco2.Fgut = TRUE,
                                                Caco2.Fabs = TRUE,
                                                overwrite.invivo = FALSE)
){
  # Required parameters
  req.param <- c("Caco2.Pab", "Fgutabs")
  
  # Header initialization  
  if(is.null(Params) | !all(req.param %in% names(Params))){
    out <- get_chem_id(chem.cas=chem.cas,chem.name=chem.name)
    chem.cas <- out$chem.cas
    chem.name <- out$chem.name
    Params <- parameterize_steadystate(chem.cas = chem.cas,
                                chem.name = chem.name,
                                species = species,
                                clint.pvalue.threshold = clint.pvalue.threshold,
                                default.to.human = default.to.human,
                                human.clint.fup = human.clint.fup,
                                adjusted.Funbound.plasma = adjusted.Funbound.plasma,
                                restrictive.clearance = restrictive.clearance,
                                fup.lod.default = fup.lod.default,
                                suppress.messages = suppress.messages,
                                minimum.Funbound.plasma = minimum.Funbound.plasma,
                                Caco2.options = list(Caco2.Pab.default = Caco2.options$Caco2.Pab.default,
                                                     Caco2.Fgut = FALSE,
                                                     Caco2.Fabs = FALSE,
                                                     overwrite.invivo = FALSE))
  }
  
  # Detetermine Fabs.oral based on Caco2 data, or keep as Fgutabs
  if(Caco2.options$Caco2.Fabs == TRUE){
    peffh <- 10^(0.4926 * Params$Caco2.Pab - 0.1454) # Yang 2007 for Caco2 pH7.4
    permh <- 0.66 * peffh * 3.6
    fabs.oral <- 1 - (1 + 0.54 * peffh)^-7
  }else{
    fabs.oral <- Params$Fabsgut
  }
  return(as.numeric(fabs.oral))
  
}

# Calculate the fraction of chemical surviving first pass metabolism in the gut
calc_fgut.oral <- function(Params = NULL,
                           chem.cas = NULL,
                           chem.name = NULL,
                           species = "Human",
                           clint.pvalue.threshold=0.05,
                           default.to.human=F,
                           human.clint.fup=F,
                           adjusted.Funbound.plasma=T,
                           restrictive.clearance=T,
                           fup.lod.default=0.005,
                           suppress.messages=F,
                           minimum.Funbound.plasma=0.0001,
                           Caco2.options = list(Caco2.Pab.default = 1.6,
                                                Caco2.Fgut = TRUE,
                                                Caco2.Fabs = TRUE,
                                                overwrite.invivo = FALSE)
){
  
  if(Caco2.options$Caco2.Fgut == TRUE){
    # Required parameters
    req.param <- c("BW", "cl_us", "Caco2.Pab", "Funbound.plasma", "Rblood2plasma")
    
    # Header initialization  
    if(is.null(Params) | !all(req.param %in% names(Params))){
      out <- get_chem_id(chem.cas=chem.cas,chem.name=chem.name)
      chem.cas <- out$chem.cas
      chem.name <- out$chem.name
      Params <- parameterize_steadystate(chem.cas = chem.cas,
                                         chem.name = chem.name,
                                         species = species,
                                         clint.pvalue.threshold = clint.pvalue.threshold,
                                         default.to.human = default.to.human,
                                         human.clint.fup = human.clint.fup,
                                         adjusted.Funbound.plasma = adjusted.Funbound.plasma,
                                         restrictive.clearance = restrictive.clearance,
                                         fup.lod.default = fup.lod.default,
                                         suppress.messages = suppress.messages,
                                         minimum.Funbound.plasma = minimum.Funbound.plasma,
                                         Caco2.options = list(Caco2.Pab.default = Caco2.options$Caco2.Pab.default,
                                                              Caco2.Fgut = FALSE,
                                                              Caco2.Fabs = FALSE,
                                                              overwrite.invivo = FALSE))
    }
    
    
    clu_hep <- Params$cl_us*Params$BW # L/h for 70 kg human
    clu_gut <- clu_hep/100 # approximate ratio of cyp abundances
    fup <- Params$Funbound.plasma
    Rb2p <- Params$Rblood2plasma
    
    if(tolower(species) == "rat"){
      peffh <- (10^(0.6532 * Params$Caco2.Pab - 0.3036)) # peff dimensional 10-4 cm/s
      peffr <- peffh/3.6 # Fagerhol 1996
      permr <- 71/(100^2)*peffr*3.6
      fgut.oral <- 0.65/(0.65+fup*clu_gut/Rb2p*(1+.65/permr))
      
    }else{
      
      # Calculate Qvilli based on Qsmallintestine, or use default for ~70 kg human
      if(!is.null(Params$Qsmallintestine)){
        Qvilli <- (18/37.5)*Params$Qsmallintestine
      }else{
        Qvilli <- 18 # L/h blood flow to microvilli of intestinal lumen
      }
      
      peffh <- (10^(0.6532 * Params$Caco2.Pab - 0.3036)) # peff dimensional 10-4 cm/s
      permh <- 0.66*peffh*3.6 # L/h, 0.66 m2 area intestine
      fgut.oral <- Qvilli/(Qvilli+fup*clu_gut/Rb2p*(1+Qvilli/permh))
      
    }
  }else{
    # if Caco2.options$Fgut.oral == FALSE, return 1
    fgut.oral <- 1
  }
  return(as.numeric(fgut.oral))
  
}





















