#' Calculate Monte Carlo Oral Equivalent Dose
#' 
#' This functions converts a chemical plasma concetration to an oral equivalent
#' dose using a concentration obtained from calc_mc_css.  
#' 
#' 
#' All arguments after httkpop only apply if httkpop is set to TRUE and species
#' to "Human".
#' 
#' When species is specified as rabbit, dog, or mouse, the function uses the
#' appropriate physiological data(volumes and flows) but substitues human
#' fraction unbound, partition coefficients, and intrinsic hepatic clearance.
#' 
#' Tissue concentrations are calculated for the pbtk model with oral infusion
#' dosing.  All tissues other than gut, liver, and lung are the product of the
#' steady state plasma concentration and the tissue to plasma partition
#' coefficient.
#' 
#' The six sets of plausible \emph{in vitro-in vivo} extrpolation (IVIVE)
#' assumptions identified by Honda et al. (submitted) are: \tabular{lrrrr}{
#' \tab \emph{in vivo} Conc. \tab Metabolic Clearance \tab Bioactive Chemical
#' Conc. \tab TK Statistic Used* \cr Honda1 \tab Veinous (Plasma) \tab
#' Restrictive \tab Free \tab Mean Conc. \cr Honda2 \tab Veinous \tab
#' Restrictive \tab Free \tab Max Conc. \cr Honda3 \tab Veinous \tab
#' Non-restrictive \tab Total \tab Mean Conc. \cr Honda4 \tab Veinous \tab
#' Non-restrictive \tab Total \tab Max Conc. \cr Honda5 \tab Target Tissue \tab
#' Non-restrictive \tab Total \tab Mean Conc. \cr Honda6 \tab Target Tissue
#' \tab Non-restrictive \tab Total \tab Max Conc. \cr } *Assumption is
#' currently ignored because analytical steady-state solutions are currently
#' used by this function.
#' 
#' @param conc Bioactive in vitro concentration in units of uM. 
#' @param chem.name Either the chemical name or the CAS number must be
#' specified. 
#' @param chem.cas Either the CAS number or the chemical name must be
#' specified. 
#' @param suppress.messages Suppress text messages. 
#' @param input.units Units of given concentration, default of uM but can also
#' be mg/L.
#' @param output.units Units of dose, default of 'mgpkgpday' for mg/kg BW/ day or
#' 'umolpkgpday' for umol/ kg BW/ day.
#' @param which.quantile Which quantile from Monte Carlo steady-state
#' simulation (calc_mc_css) is requested. Can be a vector. Note that 95th
#' concentration quantile is the same population as the 5th dose quantile. 
#' @param species Species desired (either "Rat", "Rabbit", "Dog", "Mouse", or
#' default "Human").  
#' @param return.samples Whether or not to return the vector containing the
#' samples from the simulation instead of the selected quantile.
#' @param restrictive.clearance Protein binding not taken into account (set to
#' 1) in liver clearance if FALSE.
#' @param bioactive.free.invivo If TRUE, then only the free (unbound) fraction of
#' chemical is considered to be bioactive. If FALSE, the total chemical
#' concentration is used for IVIVE. (Default TRUE)
#' @param tk.statistic.used Theoreticially either the "mean" or "max"imum
#' (peak) concetrations might be used for IVIVE with some models. Defaults to
#' "mean". Meaningless for the steady-state model (Argument is currently
#' ignored because analytic steady-state solutions are used by this function.).
#' @param tissue Desired steady state tissue conentration.
#' @param IVIVE Honda et al. (submitted) identified six plausible sets of
#' assumptions for \emph{in vitro-in vivo} extrapolation (IVIVE) assumptions.
#' Argument may be set to "Honda1" through "Honda6". If used, this function
#' overwrites the tissue, restrictive.clearance, and bioactive.free.invivo arguments.
#' See Details below for more information.
#' @param ... Additional parameters passed to calc_mc_css for httkpop and
#' variance of parameters.
#' @return Equivalent dose in specified units, default of mg/kg BW/day.
#' @author John Wambaugh
#' @keywords Steady State Monte Carlo
#' @examples
#' 
#' 
#' \dontrun{
#' calc_mc_oral_equiv(0.1,chem.cas="34256-82-1",which.quantile=c(0.05,0.5,0.95),
#'                    method='vi',samples=100,tissue='brain')
#' }
#' 
#' @export calc_mc_oral_equiv
calc_mc_oral_equiv <- function(conc,
                               chem.name=NULL,
                               chem.cas=NULL,
                               which.quantile=0.95,
                               species="Human",
                               input.units='uM',
                               output.units='mgpkgpday',
                               suppress.messages=F,
                               return.samples=F,
                               restrictive.clearance=T,
                               bioactive.free.invivo=F,
                               tk.statistic.used="mean",
                               tissue=NULL,
                               IVIVE=NULL,
                               ...)
{
  if(!(tolower(input.units) %in% c('um','mg/l'))) stop("Input units can only be uM or mg/L.")
  
  if (!(tk.statistic.used %in% c("mean","max"))) stop ("tk.statistic.used for IVIVE must be either \"mean\" or \"max\"imum concentrtation.")
  
  if (!is.null(IVIVE)) 
  {
    out <- honda.ivive(method=IVIVE,tissue=tissue)
    bioactive.free.invivo <- out[["bioactive.free.invivo"]]
    restrictive.clearance <- out[["restrictive.clearance"]]
    tissue <- out[["tissue"]]

  }
  
  Css <- try(calc_mc_css(daily.dose=1,
                         chem.name=chem.name,
                         chem.cas=chem.cas,
                         which.quantile=which.quantile,
                         species=species,
                         output.units=input.units,
                         suppress.messages=T,
                         restrictive.clearance=restrictive.clearance,
                         bioactive.free.invivo = bioactive.free.invivo,
                         tissue=tissue,
                         tk.statistic.used=tk.statistic.used,
                         return.samples=return.samples,
                         ...))
                         
  dose <- conc/Css  
  
  # Do we use the free concentration in the plasma or the total?
  if(bioactive.free.invivo) 
  {
    params <- parameterize_steadystate(chem.name=chem.name,chem.cas=chem.cas,species=species)
    dose <- dose/params[["Funbound.plasma"]]
  } 
  if(tolower(output.units) == 'umolpkgpday'){
    if(is.null(chem.cas)) chem.cas <- get_chem_id(chem.name=chem.name)[['chem.cas']]
    MW <- get_physchem_param("MW",chem.CAS=chem.cas)
    dose <- dose /1000 / MW * 1000000 
  }else if(tolower(output.units) != 'mgpkgpday') stop("Output units can only be in mgpkgpday or mol.")
  if(!suppress.messages & !return.samples){
    cat(input.units,"concentration converted to",output.units,"dose for",which.quantile,"quantile.\n")
  }
	if (class(Css) == "try-error"){
    return(NA)
  }else{
    return(dose)
  }
  
}
