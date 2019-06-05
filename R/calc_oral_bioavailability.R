#' Parameterize_SteadyState
#' 
#' This function initializes the parameters needed in the functions
#' calc_mc_css, calc_mc_oral_equiv, and calc_analytic_css for the three
#' compartment steady state model ('3compartmentss').  
#' 
#' 
#' @param chem.name Either the chemical name or the CAS number must be
#' specified.
#' @param chem.cas Either the chemical name or the CAS number must be
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
#'
#' @return \item{Clint}{Hepatic Intrinsic Clearance, uL/min/10^6 cells.}
#' \item{Fgutabs}{Fraction of the oral dose absorbed, i.e. the fraction of the
#' dose that enters the gutlumen.} \item{Funbound.plasma}{Fraction of plasma
#' that is not bound.} \item{Qtotal.liverc}{Flow rate of blood exiting the
#' liver, L/h/kg BW^3/4.} \item{Qgfrc}{Glomerular Filtration Rate, L/h/kg
#' BW^3/4, volume of fluid filtered from kidney and excreted.} \item{BW}{Body
#' Weight, kg} \item{MW}{Molecular Weight, g/mol}
#' \item{million.cells.per.gliver}{Millions cells per gram of liver tissue.}
#' \item{Vliverc}{Volume of the liver per kg body weight, L/kg BW.}
#' \item{liver.density}{Liver tissue density, kg/L.}
#' \item{Fhep.assay.correction}{The fraction of chemical unbound in hepatocyte
#' assay using the method of Kilford et al. (2008)}
#' \item{hepatic.bioavailability}{Fraction of dose remaining after first pass
#' clearance, calculated from the corrected well-stirred model.}
#' @author Gregory Honda
#' @keywords Parameter
#' @examples
#' 
#'  parameters <- parameterize_steadystate(chem.name='Bisphenol-A',species='Rat')
#'  parameters <- parameterize_steadystate(chem.cas='80-05-7')
#' 
#' @export parameterize_fbio
#' @export calc_fbio.oral
#' @export calc_fabs.oral
#' @export calc_hepatic.bioavailability
#' @export calc_fgut.oral
#' 
calc_fbio.oral <- function(Params = NULL,
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
                           Caco2.options = list(Caco2.Pab.default = 2,
                                                Caco2.Fgut = TRUE,
                                                Caco2.Fabs = TRUE)
){
  # Header initialization  
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
                                                            Caco2.Fabs = FALSE))
  }

  fabs.oral <- calc_fabs.oral(Params = Params)  
  fhep.oral <- Params$hepatic.bioavailability
  fgut.oral <- calc_fgut.oral(Params = Params)
  fbio.oral <- fabs.oral*fhep.oral*fgut.oral
  
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
                           Caco2.options = list(Caco2.Pab.default = 2,
                                                Caco2.Fgut = TRUE,
                                                Caco2.Fabs = TRUE)
){
  # Required parameters
  req.param <- c("Caco2.Pab")
  
  # Header initialization  
  if(is.null(Params) || all(!req.param %in% names(Params))){
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
                                                     Caco2.Fabs = FALSE))
  }
  
  
  peffh <- 10^(0.6532 * Params$Caco2.Pab - 0.3036)
  permh <- 0.66 * peffh * 3.6
  fabs.oral <- 1 - (1 + 0.54 * peffh)^-7
  return(fabs.oral)
  
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
                           Caco2.options = list(Caco2.Pab.default = 2,
                                                Caco2.Fgut = TRUE,
                                                Caco2.Fabs = TRUE)
){
  # Required parameters
  req.param <- c("BW", "cl_us", "Caco2.Pab")
  
  # Header initialization  
  if(is.null(Params) || all(!req.param %in% names(Params))){
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
                                                     Caco2.Fabs = FALSE))
  }
  
  clu_hep <- Params$cl_us*Params$BW # L/h for 70 kg human
  clu_gut <- clu_hep/100 # approximate ratio of cyp abundances
  
  if(tolower(species) == "rat"){
    peffh <- (10^(0.6532 * Params$Caco2.Pab - 0.3036)) # peff dimensional 10-4 cm/s
    peffr <- peffh/3.6 # Fagerhol 1996
    permr <- 71/(100^2)*peffr*3.6
    fgut.oral <- 0.65/(0.65+clu_gut*(1+.65/permr))
  
  }else{
    peffh <- (10^(0.6532 * Params$Caco2.Pab - 0.3036)) # peff dimensional 10-4 cm/s
    permh <- 0.66*peffh*3.6 # L/h, 0.66 m2 area intestine
    fgut.oral <- 18/(18+clu_gut*(1+18/permh))
    
  }

  return(fgut.oral)
  
}






























