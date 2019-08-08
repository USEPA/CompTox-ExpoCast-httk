#' Find the monte carlo steady state concentration.
#' 
#' This function finds the analytical steady state plasma concentration(from
#' calc_analytic_css) using a monte carlo simulation (monte_carlo).
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
#' @param chem.cas Either the CAS number, parameters, or the chemical name must
#' be specified. 
#' @param chem.name Either the chemical parameters, name, or the CAS number
#' must be specified. 
#' @param parameters Parameters from parameterize_steadystate. Not used with
#' httkpop model.
#' @param daily.dose Total daily dose, mg/kg BW/day.
#' @param which.quantile Which quantile from Monte Carlo simulation is
#' requested. Can be a vector.
#' @param species Species desired (either "Rat", "Rabbit", "Dog", "Mouse", or
#' default "Human").  Species must be set to "Human" to run httkpop model. 
#' @param output.units Plasma concentration units, either uM or default mg/L.
#' @param suppress.messages Whether or not to suppress output message.
#' @param model Model used in calculation: 'pbtk' for the multiple compartment
#' model,'3compartment' for the three compartment model, '3compartmentss' for
#' the three compartment steady state model, and '1compartment' for one
#' compartment model.  This only applies when httkpop=TRUE and species="Human",
#' otherwise '3compartmentss' is used.
#' @param censored.params The parameters listed in censored.params are sampled
#' from a normal distribution that is censored for values less than the limit
#' of detection (specified separately for each paramter). This argument should
#' be a list of sub-lists. Each sublist is named for a parameter in
#' "parameters" and contains two elements: "CV" (coefficient of variation) and
#' "LOD" (limit of detection, below which parameter values are censored. New
#' values are sampled with mean equal to the value in "parameters" and standard
#' deviation equal to the mean times the CV.  Censored values are sampled on a
#' uniform distribution between 0 and the limit of detection. Not used with
#' httkpop model.
#' @param vary.params The parameters listed in vary.params are sampled from a
#' normal distribution that is truncated at zero. This argument should be a
#' list of coefficients of variation (CV) for the normal distribution. Each
#' entry in the list is named for a parameter in "parameters". New values are
#' sampled with mean equal to the value in "parameters" and standard deviation
#' equal to the mean times the CV. Not used with httkpop model.
#' @param fup.meas.cv Coefficient of variation of distribution of measured
#' \code{Funbound.plasma} values. 
#' @param clint.meas.cv Coefficient of variation of distribution of measured 
#' \code{Clint} values.
#' @param fup.pop.cv Coefficient of variation of distribution of population
#' \code{Funbound.plasma} values.
#' @param clint.pop.cv Coefficient of variation of distribution of population
#' \code{Clint} values.
#' @param samples Number of samples generated in calculating quantiles.
#' @param return.samples Whether or not to return the vector containing the
#' samples from the simulation instead of the selected quantile.
#' @param default.to.human Substitutes missing rat values with human values if
#' true.
#' @param tissue Desired steady state tissue conentration.
#' @param well.stirred.correction If TRUE (default) then the well-stirred
#' correction (Rowland et al., 1973) is used in the calculation of hepatic
#' clearance for the models that do not include flows for first-pass metabolism
#' (currently, 1compartment and 3compartmentss). This assumes clearance
#' relative to amount unbound in whole blood instead of plasma, but converted
#' for use with plasma concentration.
#' @param adjusted.Funbound.plasma Uses adjusted Funbound.plasma when set to
#' TRUE along with partition coefficients calculated with this value.
#' @param regression Whether or not to use the regressions in calculating
#' partition coefficients.
#' @param clint.pvalue.threshold Hepatic clearance for chemicals where the in
#' vitro clearance assay result has a p-values greater than the threshold are
#' set to zero.
#' @param restrictive.clearance Protein binding not taken into account (set to
#' 1) in liver clearance if FALSE.
#' @param bioactive.free.invivo If FALSE (default), then the total concentration is treated
#' as bioactive in vivo. If TRUE, the the unbound (free) plasma concentration is treated as 
#' bioactive in vivo. Only works with tissue = NULL in current implementation.
#' @param concentration Desired concentration type, 'blood','tissue', or default 'plasma'.
#' @param IVIVE Honda et al. (2019) identified six plausible sets of
#' assumptions for \emph{in vitro-in vivo} extrapolation (IVIVE) assumptions.
#' Argument may be set to "Honda1" through "Honda6". If used, this function
#' overwrites the tissue, restrictive.clearance, and bioactive.free.invivo arguments.
#' See Details below for more information.
#' @param httkpop Whether or not to use population generator and sampler from
#' httkpop.  This is overwrites censored.params and vary.params and is only for
#' human physiology.  Species must also be set to 'Human'.
#' @param poormetab TRUE (include poor metabolizers) or FALSE (exclude poor
#' metabolizers)
#' @param fup.censored.dist Logical. Whether to draw \code{Funbound.plasma} from a
#' censored distribution or not.
#' @param fup.lod The average limit of detection for Funbound.plasma. if
#' \code{fup.censor == TRUE}, the \code{Funbound.plasma} distribution will be
#' censored below \code{lod/2}. Default value is 0.01.
#' @param method The population-generation method to use. Either "virtual
#' individuals" or "direct resampling" (default). Short names may be used: "d"
#' or "dr" for "direct resampling", and "v" or "vi" for "virtual individuals".
#' @param gendernum Optional: A named list giving the numbers of male and
#' female individuals to include in the population, e.g. \code{list(Male=100,
#' Female=100)}. Default is NULL, meaning both males and females are included,
#' in their proportions in the NHANES data. If both \code{nsamp} and
#' \code{gendernum} are provided, they must agree (i.e., \code{nsamp} must be
#' the sum of \code{gendernum}).
#' @param agelim_years Optional: A two-element numeric vector giving the
#' minimum and maximum ages (in years) to include in the population. Default is
#' c(0,79). If only a single value is provided, both minimum and maximum ages
#' will be set to that value; e.g. \code{agelim_years=3} is equivalent to
#' \code{agelim_years=c(3,3)}. If \code{agelim_years} is provided and
#' \code{agelim_months} is not, \code{agelim_years} will override the default
#' value of \code{agelim_months}.
#' @param agelim_months Optional: A two-element numeric vector giving the
#' minimum and maximum ages (in months) to include in the population. Default
#' is c(0, 959), equivalent to the default \code{agelim_years}. If only a
#' single value is provided, both minimum and maximum ages will be set to that
#' value; e.g. \code{agelim_months=36} is equivalent to
#' \code{agelim_months=c(36,36)}. If \code{agelim_months} is provided and
#' \code{agelim_years} is not, \code{agelim_months} will override the default
#' values of \code{agelim_years}.
#' @param weight_category Optional: The weight categories to include in the
#' population. Default is \code{c('Underweight', 'Normal', 'Overweight',
#' 'Obese')}. User-supplied vector must contain one or more of these strings.
#' @param gfr_category The kidney function categories to include in the
#' population. Default is \code{c('Normal','Kidney Disease', 'Kidney Failure')}
#' to include all kidney function levels.
#' @param reths Optional: a character vector giving the races/ethnicities to
#' include in the population. Default is \code{c('Mexican American','Other
#' Hispanic','Non-Hispanic White','Non-Hispanic Black','Other')}, to include
#' all races and ethnicities in their proportions in the NHANES data.
#' User-supplied vector must contain one or more of these strings.
#' @param physiology.matrix A data table generated by
#' \code{httkpop_generate()}.
#' @param parameter.matrix A data table generated by \code{get_httk_params()}.
#' @param Caco2.options A list of options to use when working with Caco2 apical to
#' basolateral data \code{Caco2.Pab}, default is Caco2.options = list(Caco2.default = 2,
#' Caco2.Fabs = TRUE, Caco2.Fgut = TRUE, overwrite.invivo = FALSE, keepit100 = FALSE). Caco2.default sets the default value for 
#' Caco2.Pab if Caco2.Pab is unavailable. Caco2.Fabs = TRUE uses Caco2.Pab to calculate
#' fabs.oral, otherwise fabs.oral = \code{Fabs}. Caco2.Fgut = TRUE uses Caco2.Pab to calculate 
#' fgut.oral, otherwise fgut.oral = \code{Fgut}. overwrite.invivo = TRUE overwrites Fabs and Fgut in vivo values from literature with 
#' Caco2 derived values if available. keepit100 = TRUE overwrites Fabs and Fgut with 1 (i.e. 100 percent) regardless of other settings.
#'
#' @param ... Additional parameters passed to calc_analytic_css
#' 
#' @author Caroline Ring, Robert Pearce, and John Wambaugh
#' @references Wambaugh, John F., et al. "Toxicokinetic triage for 
#' environmental chemicals." Toxicological Sciences 147.1 (2015): 55-67.
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
#' @keywords Monte Carlo Steady State
#'
#' @examples
#' 
#' \dontrun{
#'  calc_mc_css(chem.name='Bisphenol A',output.units='uM',method='vi',
#'              samples=100,return.samples=TRUE)
#'  calc_mc_css(chem.name='2,4-d',which.quantile=.9,httkpop=FALSE,tissue='heart')
#' 
#'  calc_mc_css(chem.cas = "80-05-7", daily.dose = 1, which.quantile = 0.5,
#'              censored.params = list(Funbound.plasma = list(cv = 0.1, 
#'                                                           lod = 0.005)),
#'              vary.params = list(BW = 0.15, Vliverc = 0.15, Qgfrc = 0.15,
#'                                Qtotal.liverc = 0.15, 
#'                                million.cells.per.gliver = 0.15, Clint = 0.15),
#'              output.units = "uM", samples = 2000)
#' 
#'  params <- parameterize_pbtk(chem.cas="80-05-7")
#'  calc_mc_css(parameters=params,model="pbtk")
#' }
#'
#' @import stats
#' @export calc_mc_css
calc_mc_css <- function(chem.cas=NULL,
                        chem.name=NULL,
                        parameters=NULL,
                        daily.dose=1,
                        which.quantile=0.95,
                        species="Human",
                        output.units="mg/L",
                        suppress.messages=F,
                        model='3compartmentss',
                        censored.params=list(Funbound.plasma=list(cv=0.3,lod=0.01)),
                        vary.params=list(BW=0.3,
                          Vliverc=0.3,
                          Qgfrc=0.3,
                          Qtotal.liverc=0.3,
                          million.cells.per.gliver=0.3,
                          Caco2.Pab = 0.3,
                          Clint=0.3),
                        fup.meas.cv=0.4,
                        caco2.meas.sd = 0.3,
                        clint.meas.cv=0.3,
                        fup.pop.cv=0.3,
                        caco2.pop.sd = 0.3,
                        clint.pop.cv=0.3,
                        samples=1000,
                        return.samples=F,
                        default.to.human=F,
                        tissue=NULL,
                        well.stirred.correction=T,
                        adjusted.Funbound.plasma=T,
                        regression=T,
                        clint.pvalue.threshold=0.05,
                        restrictive.clearance = T,
                        bioactive.free.invivo = FALSE,
                        concentration = "plasma",
                        IVIVE=NULL,
                        httkpop=T,
                        poormetab=T,
                        fup.censored.dist=FALSE,
                        fup.lod=0.01,
                        method='direct resampling',
                        gendernum=NULL,
                        agelim_years=NULL,
                        agelim_months=NULL,
                        weight_category =  c("Underweight", "Normal", "Overweight", "Obese"),
                        gfr_category = c("Normal", "Kidney Disease", "Kidney Failure"),
                        reths = c("Mexican American", "Other Hispanic", "Non-Hispanic White","Non-Hispanic Black", "Other"),
                        physiology.matrix=NULL,
                        parameter.matrix=NULL,
                        Caco2.options = list(Caco2.Pab.default = "1.6",
                                             Caco2.Fgut = TRUE,
                                             Caco2.Fabs = TRUE,
                                             overwrite.invivo = FALSE,
                                             keepit100 = FALSE),
                        ...)
{
  if (!(model %in% c("pbtk","1compartment","3compartment","3compartmentss"))) stop("Model must be either \"pbtk\", \"1compartment\", \"3compartmentss\", or \"3compartment\".")

  if(!all(c("Caco2.Pab.default", "Caco2.Fgut", "Caco2.Fabs", "overwrite.invivo", "keepit100")%in% names(unlist(Caco2.options)))){
    Caco2.options <- ammend.httk.option.list(httk.option.list = Caco2.options)
  }
  
  if (!is.null(IVIVE)) 
  {
    out <- honda.ivive(method=IVIVE,tissue=tissue)
    restrictive.clearance <- out[["restrictive.clearance"]]
    tissue <- out[["tissue"]]
    bioactive.free.invivo <- out[["bioactive.free.invivo"]]
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
 
  css_apply <- function(params)
  {
    params <- as.list(params)
    css <- calc_analytic_css(parameters=params,
                             model=model,
                             daily.dose=daily.dose,
                             suppress.messages=T,
                             output.units=output.units,
                             chem.cas=chem.cas,
                             chem.name=chem.name,
                             well.stirred.correction=well.stirred.correction,
                             adjusted.Funbound.plasma=adjusted.Funbound.plasma,
                             regression=regression,
                             restrictive.clearance=restrictive.clearance,
                             bioactive.free.invivo = bioactive.free.invivo,
                             Caco2.options = Caco2.options,
                             IVIVE = NULL,
                             tissue=tissue,
                             concentration = concentration,
                             clint.pvalue.threshold = 0.05,
                             ...)
    return(css)
  }
  if (httkpop == T & tolower(species) == 'human')
  {
    if (!is.null(parameter.matrix)) css.list <- apply(parameter.matrix,1,css_apply)
    else
    {
      if (is.null(chem.cas) & is.null(chem.name) & is.null(parameters)) stop('Must specify chem.cas, chem.name, or parameters.')
      if (is.null(parameters))
      {
        out <- get_chem_id(chem.cas=chem.cas,chem.name=chem.name)
        this.chem <- out$chem.cas
      } else this.chem <- NULL
      if (is.null(physiology.matrix))
      {
        nsamp <- samples
        if(is.null(method)) stop('Specify method as \"virtual individuals\" (\"v\" or \"vi\") or \"direct resampling\" (\"dr\" or \"d\").')
        else if(! method %in% c('direct resampling','virtual individuals','v','vi','direct resampling','dr','d')) stop('Specify method as \"virtual individuals\" (\"v\" or \"vi\") or \"direct resampling\" (\"dr\" or \"d\").')
        physiology.matrix <- httkpop_generate(method=method,
                                              nsamp=nsamp,
                                              gendernum=gendernum,
                                              agelim_years=agelim_years,
                                              agelim_months=agelim_months,
                                              weight_category=weight_category,
                                              gfr_category=gfr_category,
                                              reths=reths)
      } 
      parameter.matrix <- get_httk_params(physiology.matrix,
                                          model=model,
                                          chemcas=this.chem,
                                          parameters=parameters,
                                          poormetab=poormetab,
                                          fup.meas.cv=fup.meas.cv,
                                          clint.meas.cv=clint.meas.cv,
                                          caco2.meas.sd = caco2.meas.sd,
                                          fup.pop.cv=fup.pop.cv,
                                          clint.pop.cv=clint.pop.cv,
                                          caco2.pop.sd = caco2.pop.sd,
                                          fup.lod=fup.lod,
                                          fup.censored.dist=fup.censored.dist,
                                          well.stirred.correction=well.stirred.correction,
                                          adjusted.Funbound.plasma=adjusted.Funbound.plasma,
                                          regression=regression,
                                          restrictive.clearance=restrictive.clearance,
                                          Caco2.options = Caco2.options,
                                          concentration = concentration,
                                          clint.pvalue.threshold=clint.pvalue.threshold)
      css.list <- apply(parameter.matrix,1,css_apply) 
    }
    if (return.samples) out <- css.list
    else out <- quantile(css.list,which.quantile,na.rm=T)       
  } else {
    if (is.null(chem.cas) & is.null(chem.name) & is.null(parameters)) stop('Must specify chem.cas, chem.name, or parameters.')
    if (is.null(parameters))
    {
        parameters <- parameterize_steadystate(chem.cas=chem.cas,
                                               chem.name=chem.name,
                                               species=species,
                                               default.to.human=default.to.human,
                                               adjusted.Funbound.plasma=adjusted.Funbound.plasma)
    } else {
      if (!all(param.names.3compss %in% names(parameters)))
      {
        stop(paste("Missing parameters:",
                   paste(param.names.3compss[which(!param.names.3compss %in% names(parameters))],
                     collapse=', '),
                   ".  Use parameters from parameterize_steadystate."))
      }
    }
    if (well.stirred.correction & !'Rblood2plasma' %in% names(parameters)){
      parameters[['Rblood2plasma']] <- 
        available_rblood2plasma(chem.name=chem.name,chem.cas=chem.cas,
                                species=species,
                                adjusted.Funbound.plasma=
                                  adjusted.Funbound.plasma)
    }
    
    out <- monte_carlo(params=parameters,
                        censored.params=censored.params,
                        which.quantile=which.quantile,
                        cv.params=vary.params,
                        samples=samples,model='3compartmentss',
                        daily.dose=daily.dose,
                        output.units=output.units,
                        tissue=tissue,
                        IVIVE=IVIVE,
                        chem.name=chem.name,
                        chem.cas=chem.cas,
                        adjusted.Funbound.plasma=adjusted.Funbound.plasma,
                        regression=regression,
                        well.stirred.correction=well.stirred.correction,
                        suppress.messages=T,
                        return.samples=return.samples,
                        restrictive.clearance=restrictive.clearance,
                       bioactive.free.invivo = bioactive.free.invivo,
                        species=species)

    if(httkpop==T) warning('httkpop model only available for human and thus not used.  Set species=\"Human\" to run httkpop model.')   
  }  
  if(!suppress.messages & !return.samples){
    if(is.null(chem.cas) & is.null(chem.name)){
      if(is.null(tissue)) cat("Plasma concentration returned in",output.units,"units.\n")
      else cat(paste(toupper(substr(tissue,1,1)),substr(tissue,2,nchar(species)),sep=''),"concentration returned in",output.units,"units.\n")
    }else{
      if(is.null(tissue))cat(paste(toupper(substr(species,1,1)),substr(species,2,nchar(species)),sep=''),"plasma concentration returned in",output.units,"units for",which.quantile,"quantile.\n") 
      else cat(paste(toupper(substr(species,1,1)),substr(species,2,nchar(species)),sep=''),tissue,"concentration returned in",output.units,"units.\n")
    }
  }
  print(out)
  return(as.numeric(out))
}
