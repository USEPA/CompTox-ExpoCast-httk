#' Find the monte carlo steady state concentration.
#' 
#' This function finds the analytical steady state plasma concentration(from
#' calc_analytic_css) using a monte carlo simulation (monte_carlo).
#' 
#' All arguments after httkpop only apply if httkpop is set to TRUE and species
#' to "Human".
#' 
#' When species is specified as rabbit, dog, or mouse, the function uses the
#' appropriate physiological data (volumes and flows) but substitutes human
#' fraction unbound, partition coefficients, and intrinsic hepatic clearance.
#' 
#' Tissue concentrations are calculated for the pbtk model with a default oral
#' infusion dosing. All tissues other than gut, liver, and lung are the product
#' of the steady state plasma concentration and the tissue to plasma partition
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
#' @param chem.cas Chemical Abstract Services Registry Number (CAS-RN) -- if
#'  parameters is not specified then the chemical must be identified by either
#'  CAS, name, or DTXISD
#' @param chem.name Chemical name (spaces and capitalization ignored) --  if
#'  parameters is not specified then the chemical must be identified by either
#'  CAS, name, or DTXISD
#' @param dtxsid EPA's DSSTox Structure ID (\url{https://comptox.epa.gov/dashboard})  
#'  -- if parameters is not specified then the chemical must be identified by 
#' either CAS, name, or DTXSIDs
#' @param parameters Parameters from the appropriate parameterization function
#' for the model indicated by argument model
#' @param samples Number of samples generated in calculating quantiles.
#' @param which.quantile Which quantile from Monte Carlo simulation is
#' requested. Can be a vector.
#' @param species Species desired (either "Rat", "Rabbit", "Dog", "Mouse", or
#' default "Human").  Species must be set to "Human" to run httkpop model.
#' @param suppress.messages Whether or not to suppress output message.
#' @param model Model used in calculation: 'pbtk' for the multiple compartment
#' model,'3compartment' for the three compartment model, '3compartmentss' for
#' the three compartment steady state model, and '1compartment' for one
#' compartment model.  This only applies when httkpop=TRUE and species="Human",
#' otherwise '3compartmentss' is used.
#' @param httkpop Whether or not to use population generator and sampler from
#' httkpop.  This is overwrites censored.params and vary.params and is only for
#' human physiology.  Species must also be set to 'Human'.
#' @param invitrouv Logical to indicate whether to include in vitro parameters
#' in uncertainty and variability analysis
#' @param calcrb2p Logical determining whether or not to recalculate the 
#' chemical ratio of blood to plasma
#' @param censored.params The parameters listed in censored.params are sampled
#' from a normal distribution that is censored for values less than the limit
#' of detection (specified separately for each parameter). This argument should
#' be a list of sublists. Each sublist is named for a parameter in
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
#' @param return.samples Whether or not to return the vector containing the
#' samples from the simulation instead of the selected quantile.
#' @param tissue Desired steady state tissue concentration.
#' @param output.units Plasma concentration units, either uM or default mg/L.
#' @param invitro.mc.arg.list List of additional parameters passed to 
#' \code{\link{invitro_mc}}
#' @param httkpop.generate.arg.list Additional parameters passed to 
#' \code{\link{httkpop_generate}}.
#' @param convert.httkpop.arg.list Additional parameters passed to the 
#' convert_httkpop_* function for the model.
#' @param parameterize.arg.list Additional parameters passed to the 
#' parameterize_* function for the model.
#' @param calc.analytic.css.arg.list Additional parameters passed to 
#' \code{\link{calc_analytic_css}}.  
#' @author Caroline Ring, Robert Pearce, and John Wambaugh
#'
#' @references 
#' Wambaugh, John F., et al. "Toxicokinetic triage for 
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
#' @keywords Monte-Carlo Steady-State
#'
#' @examples
#' 
#' \dontrun{
#'  set.seed(1234)
#'  calc_mc_css(chem.name='Bisphenol A',output.units='uM',
#'              samples=100,return.samples=TRUE)
#'              
#'  set.seed(1234)
#'  calc_mc_css(chem.name='2,4-d',which.quantile=.9,httkpop=FALSE,tissue='heart')
#' 
#'  set.seed(1234)
#'  calc_mc_css(chem.cas = "80-05-7", which.quantile = 0.5,
#'              output.units = "uM", samples = 2000,
#'              httkpop.generate.arg.list=list(method='vi', gendernum=NULL, 
#'              agelim_years=NULL, agelim_months=NULL, weight_category = 
#'              c("Underweight", "Normal", "Overweight", "Obese")))
#' 
#'  params <- parameterize_pbtk(chem.cas="80-05-7")
#'  set.seed(1234)
#'  calc_mc_css(parameters=params,model="pbtk")
#'
#'  set.seed(1234)
#'  # Standard HTTK Monte Carlo:
#'  NSAMP = 500
#'  calc_mc_css(chem.cas="90-43-7",model="pbtk",samples=NSAMP)
#'  set.seed(1234)
#   HTTK Monte Carlo with no measurement uncertainty (pre v1.10.0):
#'  calc_mc_css(chem.cas="90-43-7",
#'  model="pbtk",
#'  samples=NSAMP,
#'  invitro.mc.arg.list = list(adjusted.Funbound.plasma = T,
#'    poormetab = T, 
#'    fup.censored.dist = FALSE, 
#'    fup.lod = 0.01, 
#'    fup.meas.cv = 0.0, 
#'    clint.meas.cv = 0.0, 
#'    fup.pop.cv = 0.3, 
#'    clint.pop.cv = 0.3))
#'  set.seed(1234)
#'  # HTTK Monte Carlo with no HTTK-Pop physiological variability):
#'  calc_mc_css(chem.cas="90-43-7",model="pbtk",samples=NSAMP,httkpop=F)
#'  set.seed(1234)
#'  # HTTK Monte Carlo with no in vitro uncertainty and variability):
#'  calc_mc_css(chem.cas="90-43-7",model="pbtk",samples=NSAMP,invitrouv=F)
#'  set.seed(1234)
#'  # HTTK Monte Carlo with no HTTK-Pop and no in vitro uncertainty and variability):
#'  calc_mc_css(chem.cas="90-43-7",model="pbtk",samples=NSAMP,httkpop=F,invitrouv=F)
#'  # Should be the same as the mean result:
#'  calc_analytic_css(chem.cas="90-43-7",model="pbtk",output.units="mg/L")
#'  set.seed(1234)
#'  # HTTK Monte Carlo using basic Monte Carlo sampler:
#'  calc_mc_css(chem.cas="90-43-7",
#'  model="pbtk",
#'  samples=NSAMP,
#'  httkpop=F,
#'  invitrouv=F,
#'  vary.params=list(Pow=0.3))
#' }
#'
#' @import stats
#' @export calc_mc_css
calc_mc_css <- function(chem.cas=NULL,
                        chem.name=NULL,
                        dtxsid = NULL,
                        parameters=NULL,
                        samples=1000,
                        which.quantile=0.95,
                        species="Human",
                        suppress.messages=F,
                        model='3compartmentss',
                        httkpop=T,
                        invitrouv=T,
                        calcrb2p=T,
                        censored.params=list(),
                        vary.params=list(),
                        return.samples=F,
                        tissue=NULL,
                        output.units="mg/L",
                        invitro.mc.arg.list=list(
                          adjusted.Funbound.plasma=T,
                          poormetab=T,
                          fup.censored.dist=FALSE,
                          fup.lod=0.01,
                          fup.meas.cv=0.4,
                          clint.meas.cv=0.3,
                          fup.pop.cv=0.3,
                          clint.pop.cv=0.3),
                        httkpop.generate.arg.list=list(
                          method='direct resampling',
                          gendernum=NULL,
                          agelim_years=NULL,
                          agelim_months=NULL,
                          weight_category =  c(
                            "Underweight", 
                            "Normal", 
                            "Overweight", 
                            "Obese"),
                          gfr_category = c(
                            "Normal", 
                            "Kidney Disease", 
                            "Kidney Failure"),
                          reths = c(
                            "Mexican American", 
                            "Other Hispanic", 
                            "Non-Hispanic White",
                            "Non-Hispanic Black", 
                            "Other")),
                        convert.httkpop.arg.list=list(),
                        parameterize.arg.list=list(
                          default.to.human=F,
                          clint.pvalue.threshold=0.05,
                          restrictive.clearance = T,
                          regression=T),
                        calc.analytic.css.arg.list=list(
                          well.stirred.correction=T,
                          adjusted.Funbound.plasma=T,
                          regression=T,
                          IVIVE = NULL,
                          tissue=tissue,
                          restrictive.clearance = T,
                          bioactive.free.invivo = FALSE)) 
{
# We need to describe the chemical to be simulated one way or another:
  if (is.null(chem.cas) & 
      is.null(chem.name) & 
      is.null(dtxsid) &
      is.null(parameters)) 
    stop('Parameters, chem.name, chem.cas, or dtxsid must be specified.')

  if (is.null(model)) stop("Model must be specified.")
  
  #Appease R CMD check --as-cran variable binding:
  Css <- NULL
  
  
# We need to know model-specific information (from modelinfo_[MODEL].R]) 
# to set up the solver:
  model <- tolower(model)
  if (!(model %in% names(model.list)))            
  {
    stop(paste("Model",model,"not available. Please select from:",
      paste(names(model.list),collapse=", ")))
  } 
    
#
#
# CREATE A TABLE OF PARAMETER VALUES WHERE EACH ROW IS A SEPARATE SET OF 
# VALUES FOR WHICH Css SHOULD BE CALCULATED
#
#
  parameter.dt <- create_mc_samples(
                        chem.cas=chem.cas,
                        chem.name=chem.name,
                        dtxsid = dtxsid,
                        parameters=parameters,
                        samples=samples,
                        species=species,
                        suppress.messages=suppress.messages,
                        model=model,
                        httkpop=httkpop,
                        invitrouv=invitrouv,
                        calcrb2p=calcrb2p,
                        censored.params=censored.params,
                        vary.params=vary.params,
                        return.samples=F,
                        invitro.mc.arg.list=invitro.mc.arg.list,
                        httkpop.generate.arg.list=httkpop.generate.arg.list,
                        convert.httkpop.arg.list=convert.httkpop.arg.list,
                        parameterize.arg.list=parameterize.arg.list)

#
# HERE LIES THE ACTUAL MONTE CARLO STEP:
#
# Calculate CSS for each row in the parameter matrix (each row corresponds to
# a different individual):
#

  parameter.dt[,Css:= do.call(calc_analytic_css,
                            args=c(list(parameters=.SD,
                              model=model,
                              suppress.messages=T,
                              chem.cas=chem.cas,
                              chem.name=chem.name,
                              dtxsid=dtxsid,
                              output.units=output.units,
                              clint.pvalue.threshold=
                                parameterize.arg.list$clint.pvalue.threshold),
                              calc.analytic.css.arg.list))]

  css.list <- parameter.dt$Css 
  
  if (!return.samples) out <- quantile(css.list,which.quantile,na.rm=T)  
  else out <- css.list  

  if (!suppress.messages)
  {
    if (!return.samples)
    {
      if (is.null(tissue)) cat(paste(toupper(substr(species,1,1)),
        substr(species,2,nchar(species)),sep=''),
        "plasma concentration returned in",
        output.units,
        "units for",
        which.quantile,
        "quantile.\n") 
        else cat(paste(toupper(substr(species,1,1)),
          substr(species,2,nchar(species)),sep=''),
          tissue,
          "concentration returned in",
          output.units,
          "units for",
          which.quantile,
          "quantile.\n")    
    } else {
      if (is.null(tissue)) cat(paste(toupper(substr(species,1,1)),
        substr(species,2,nchar(species)),sep=''),
        "plasma concentration returned in",
        output.units,
        "units.\n") 
      else cat(paste(toupper(substr(species,1,1)),
        substr(species,2,nchar(species)),sep=''),
        tissue,
        "concentration returned in",
        output.units,
        "units.\n") 
    }
  }

# Cannot guarantee arbitrary precision:
  out <- set_httk_precision(out)
  
  return(out)
}
