#' Calculate Monte Carlo Oral Equivalent Dose
#' 
#' This functions converts a chemical plasma concetration to an oral equivalent
#' dose using a concentration obtained from \code{\link{calc_mc_css}}.  
#' 
#' 
#' All arguments after httkpop only apply if httkpop is set to TRUE and species
#' to "Human".
#' 
#' When species is specified as rabbit, dog, or mouse, the function uses the
#' appropriate physiological data(volumes and flows) but substitutes human
#' fraction unbound, partition coefficients, and intrinsic hepatic clearance.
#' 
#' Tissue concentrations are calculated for the pbtk model with oral infusion
#' dosing.  All tissues other than gut, liver, and lung are the product of the
#' steady state plasma concentration and the tissue to plasma partition
#' coefficient.
#' 
#' The six sets of plausible \emph{in vitro-in vivo} extrapolation (IVIVE)
#' assumptions identified by Honda et al. (2019) are: \tabular{lrrrr}{
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
#' @param dtxsid EPA's 'DSSTox Structure ID (\url{https://comptox.epa.gov/dashboard})  
#' the chemical must be identified by either CAS, name, or DTXSIDs
#' @param suppress.messages Suppress text messages. 
#' @param input.units Units of given concentration, default of uM but can also
#' be mg/L.
#' @param output.units Units of dose, default of 'mgpkgpday' for mg/kg BW/ day or
#' 'umolpkgpday' for umol/ kg BW/ day.
#' @param which.quantile Which quantile from Monte Carlo steady-state
#' simulation (\code{\link{calc_mc_css}}) is requested. Can be a vector. Note that 95th
#' concentration quantile is the same population as the 5th dose quantile. 
#' @param species Species desired (either "Rat", "Rabbit", "Dog", "Mouse", or
#' default "Human").  
#' @param return.samples Whether or not to return the vector containing the
#' samples from the simulation instead of the selected quantile.
#' @param restrictive.clearance Protein binding not taken into account (set to
#' 1) in liver clearance if FALSE.
#' @param bioactive.free.invivo If FALSE (default), then the total concentration is treated
#' as bioactive in vivo. If TRUE, the the unbound (free) plasma concentration is treated as 
#' bioactive in vivo. Only works with tissue = NULL in current implementation.
#' @param tissue Desired steady state tissue conentration.
#' @param concentration Desired concentration type, 'blood','tissue', or default 'plasma'.
#' @param IVIVE Honda et al. (2019) identified six plausible sets of
#' assumptions for \emph{in vitro-in vivo} extrapolation (IVIVE) assumptions.
#' Argument may be set to "Honda1" through "Honda6". If used, this function
#' overwrites the tissue, restrictive.clearance, and plasma.binding arguments.
#' See Details below for more information.
#' @param ... Additional parameters passed to \code{\link{calc_mc_css}} for httkpop and
#' variance of parameters.
#' @return Equivalent dose in specified units, default of mg/kg BW/day.
#' @author John Wambaugh
#'
#' @references Wetmore, Barbara A., et al. "Incorporating high-throughput 
#' exposure predictions with dosimetry-adjusted in vitro bioactivity to inform 
#' chemical toxicity testing." Toxicological Sciences 148.1 (2015): 121-136.
#'
#' Ring, Caroline L., et al. "Identifying populations sensitive to
#' environmental chemicals by simulating toxicokinetic variability."
#' Environment international 106 (2017): 105-118. 
#' 
#' Honda, Gregory S., et al. "Using the Concordance of In Vitro and 
#' In Vivo Data to Evaluate Extrapolation Assumptions." 2019. PLoS ONE 14(5): e0217564.
#' 
#' Rowland, Malcolm, Leslie Z. Benet, and Garry G. Graham. "Clearance concepts in 
#' pharmacokinetics." Journal of pharmacokinetics and biopharmaceutics 1.2 (1973): 123-136.
#' 
#' @keywords Monte-Carlo Steady-State
#'
#' @examples
#' 
#' 
#' \dontrun{
#' calc_mc_oral_equiv(0.1,chem.cas="34256-82-1",which.quantile=c(0.05,0.5,0.95),
#'        tissue='brain')
#' }
#' 
#' @export calc_mc_oral_equiv
calc_mc_oral_equiv <- function(conc,
                               chem.name=NULL,
                               chem.cas=NULL,
                               dtxsid=NULL,
                               which.quantile=0.95,
                               species="Human",
                               input.units='uM',
                               output.units='mgpkgpday',
                               suppress.messages=F,
                               return.samples=F,
                               concentration = "plasma",
                               restrictive.clearance=T,
                               bioactive.free.invivo = F,
                               tissue=NULL,
                               IVIVE=NULL,
                               ...)
{
  if(!(tolower(input.units) %in% c('um','mg/l'))) stop("Input units can only be uM or mg/L.")
  
  if (!is.null(IVIVE)) 
  {
    out <- honda.ivive(method=IVIVE,tissue=tissue)
    bioactive.free.invivo <- out[["bioactive.free.invivo"]]
    restrictive.clearance <- out[["restrictive.clearance"]]
    tissue <- out[["tissue"]]
    concentration <- out[["concentration"]]
  }
  
  if((bioactive.free.invivo == TRUE & !is.null(tissue)) | 
     (bioactive.free.invivo == TRUE & tolower(concentration) != "plasma")
  ){
    stop("Option bioactive.free.invivo only works with tissue = NULL and concentration = \"plasma\".\n
         Ctissue * Funbound.plasma is not a relevant concentration.\n
         Cfree_blood should be the same as Cfree_plasma = Cplasma*Funbound.plasma.")
  }
  
  if(!is.null(tissue) & tolower(concentration) != "tissue"){
    concentration <- "tissue"
    warning("Tissue selected. Overwriting option for concentration with \"tissue\".")
  }
  
  
  #R CMD CHECK throws notes about "no visible binding for global variable", for
  #each time a data.table column name is used without quotes. To appease R CMD
  #CHECK, a variable has to be created for each of these column names and set to
  #NULL. Note that within the data.table, these variables will not be NULL! Yes,
  #this is pointless and annoying.
  well.stirred.correction <- adjusted.Funbound.plasma <- NULL
  #End R CMD CHECK appeasement.
  
  Css <- try(calc_mc_css(chem.name=chem.name,
                         chem.cas=chem.cas,
                         dtxsid=dtxsid,
                         which.quantile=which.quantile,
                         species=species,
                         output.units=input.units,
                         suppress.messages=T, 
                         calc.analytic.css.arg.list=
                           list(concentration = concentration,
                           restrictive.clearance=restrictive.clearance,
                           bioactive.free.invivo = bioactive.free.invivo,
                           tissue = tissue,IVIVE=IVIVE,
                           well.stirred.correction=well.stirred.correction,
                           adjusted.Funbound.plasma=adjusted.Funbound.plasma),
                         return.samples=return.samples,
                         ...))
                         
  if (class(Css) == "try-error")
  {
    return(NA)
  }
  
  #
  # The super-impressive, and complimacated reverse dosimetry IVIVE calculation: 
  #
  # uM to mg/kg/day:
  #
  dose <- conc/Css  

  if(tolower(output.units) == 'umolpkgpday'){
    if(is.null(chem.cas)) chem.cas <- get_chem_id(chem.name=chem.name)[['chem.cas']]
    MW <- get_physchem_param("MW",chem.cas=chem.cas)
    dose <- dose /1000 / MW * 1000000 
  }else if(tolower(output.units) != 'mgpkgpday') stop("Output units can only be in mgpkgpday or mol.")
  if(!suppress.messages & !return.samples){
    cat(input.units,"concentration converted to",output.units,"dose for",which.quantile,"quantile.\n")
  }

  return(set_httk_precision(dose))
}
