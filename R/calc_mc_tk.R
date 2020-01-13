#' Conduct multiple TK simulations using Monte Carlo
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
#' overwrites the tissue, restrictive.clearance, and plasma.binding arguments.
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
#' @param ... Additional parameters passed to calc_analytic_css
#' 
#' @author  John Wambaugh
#'
#' @keywords Monte-Carlo dynamic simulation
#'
#' @examples
#' 
#' \dontrun{
#' chemname="Abamectin"
#' times<- c(0,0.25,0.5,0.75,1,1.5,2,2.5,3,4,5)
#' age.ranges <- seq(6,80,by=10)
#' forward <- NULL
#' for (age.lower in age.ranges)
#' {
#'   label <- paste("Ages ",age.lower,"-",age.lower+4,sep="")
#'   set.seed(1234)
#'   forward[[label]] <- calc_mc_tk(
#'                         chem.name=chemname,
#'                         samples=NSAMP,
#'                         httkpop.generate.arg.list=list(
#'                           method="d",
#'                           agelim_years = c(age.lower, age.lower+9)),
#'                         solvemodel.arg.list = list(
#'                           times=times))
#' }
#' 
#' }
#'
#' @export calc_mc_tk
calc_mc_tk<- function(chem.cas=NULL,
                        chem.name=NULL,
                        dtxsid = NULL,
                        parameters=NULL,
                        samples=1000,
                        which.quantile=0.95,
                        species="Human",
                        suppress.messages=F,
                        model="pbtk",
                        httkpop=T,
                        invitrouv=T,
                        calcrb2p=T,
                        censored.params=list(),
                        vary.params=list(),
                        return.samples=F,
                        tissue=NULL,
                        httkpop.matrix=NULL,
                        output.units="mg/L",
                        solvemodel.arg.list=list(
                          times=c(0,0.25,0.5,0.75,1,1.5,2,2.5,3,4,5)),
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
                        return.all.sims=FALSE)
{
# We need to describe the chemical to be simulated one way or another:
  if (is.null(chem.cas) & 
      is.null(chem.name) & 
      is.null(dtxsid) &
      is.null(parameters)) 
    stop('Parameters, chem.name, chem.cas, or dtxsid must be specified.')

# We need to know model-specific information (from modelinfo_[MODEL].R]) 
# to set up the solver:
  model <- tolower(model)
  if (!(model %in% names(model.list)))            
  {
    stop(paste("Model",model,"not available. Please select from:",
      paste(names(model.list),collapse=", ")))
  }
  
  if (is.null(model.list[[model]]$solve.func)) 
    stop(paste("Kinetic model solver not available for model ",model,".",sep="")) 

#
#
# CREATE A TABLE OF PARAMETER VALUES WHERE EACH ROW IS A SEPARATE SET OF 
# VALUES FOR WHICH Css SHOULD BE CALCULATEDL
#
#
  parameter.dt <- create_mc_samples(
                        chem.cas=chem.cas,
                        chem.name=chem.name,
                        dtxsid = dtxsid,
                        parameters=parameters,
                        samples=1000,
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
  model.out <- list()
  for (i in 1:nrow(parameter.dt)) 
   model.out[[i]] <- do.call(model.list[[model]]$solve.func,args=c(list(
     parameters=as.list(parameter.dt[i,])),
     suppress.messages=T,
     solvemodel.arg.list))

  means <- Reduce("+",model.out)/length(model.out)
  sds <- (purrr::reduce(model.out,
    function(x,y) (y-means)^2)/(length(model.out)-1))^(1/2)

  out <- list(means=means,sds=sds)
  if (return.all.sims) out <- list(stats=out,sims=model.out)
  return(out)
}
