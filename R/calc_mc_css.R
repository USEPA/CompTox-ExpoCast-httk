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
#' @param chem.cas Chemical Abstract Services Registry Number (CAS-RN) -- if
#'  parameters is not specified then the chemical must be identified by either
#'  CAS, name, or DTXISD
#' @param chem.name Chemical name (spaces and capitalization ignored) --  if
#'  parameters is not specified then the chemical must be identified by either
#'  CAS, name, or DTXISD
#' @param dtxsid EPA's 'DSSTox Structure ID (http://comptox.epa.gov/dashboard)  
#'  -- if parameters is not specified then the chemical must be identified by 
#' either CAS, name, or DTXSIDs
#' @param parameters Parameters from the appropriate parameterization function
#' for the model indicated by argument model
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
#' @author Caroline Ring, Robert Pearce, and John Wambaugh
#'
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
#' @keywords Monte-Carlo Steady-State
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
                        dtxsid = NULL,
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
                          Clint=0.3),
                        fup.meas.cv=0.4,
                        clint.meas.cv=0.3,
                        fup.pop.cv=0.3,
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
                        httkpop.matrix=NULL,
                        physiology.matrix=NULL,
                        parameter.matrix=NULL,
                        ...)
{
# Define a local function for running calc_analytic_css:
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
                             IVIVE = NULL,
                             tissue=tissue,
                             restrictive.clearance = restrictive.clearance,
                             bioactive.free.invivo = bioactive.free.invivo,
                             concentration = concentration,
                             clint.pvalue.threshold = 0.05,
                             ...)
    return(css)
  }

# We need to describe the chemical to be simulated one way or another:
  if (is.null(chem.cas) & 
      is.null(chem.name) & 
      is.null(dtxsid) &
      is.null(parameters)) 
    stop('Parameters, chem.name, chem.cas, or dtxsid must be specified.')

if (is.null(model)) stop("Model must be specified.")
# We need to know model-specific information (from modelinfo_[MODEL].R]) 
# to set up the solver:
  model <- tolower(model)
  if (!(model %in% names(model.list)))            
  {
    stop(paste("Model",model,"not available. Please select from:",
      paste(names(model.list),collapse=", ")))
  } else {
#Depending on model, choose which parameters are not to be Monte Carlo sampled
    noMC.names <- model.list[[model]]$noMC.params
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

  if (is.null(parameters))
  {
#Depending on model, choose the function in HTTK that will return the default
#HTTK parameters for this chemical
  paramfun <- model.list[[model]]$parameterize.func
  parameters.mean <- do.call(getFromNamespace(paramfun, "httk"),
                       args=c(list(chem.cas=chem.cas,
                           chem.name=chem.name,
                           dtxsid=dtxsid),
                           ...))
  } else parameters.mean <- parameters 
  
  if (is.null(physiology.matrix))
  {
    nsamp <- samples
    if (httkpop=T & tolower(species)=="human")
    {
      if (is.null(method)) stop(
"Specify method as \"virtual individuals\" (\"v\" or \"vi\") or \"direct\n\
resampling\" (\"dr\" or \"d\").")
      else if(!method %in% c('direct resampling',
                              'virtual individuals',
                              'v',
                              'vi',
                              'direct resampling',
                              'dr',
                              'd')) stop( 
"Specify method as \"virtual individuals\" (\"v\" or \"vi\") or \"direct\n\
resampling\" (\"dr\" or \"d\").")
      if (is.null(httkpop.matrix))
        httkpop.matrix <- httkpop_generate(method=method,
                                           nsamp=nsamp,
                                           gendernum=gendernum,
                                           agelim_years=agelim_years,
                                           agelim_months=agelim_months,
                                           weight_category=weight_category,
                                           gfr_category=gfr_category,
                                           reths=reths)

#Depending on model, choose the function in HTTK that will return the default
#HTTK parameters for this chemical
      converthttkfun <- model.list[[model]]$converthttk.func
      physiology.matrix <- convert_httkpop(
                             chem.cas=chem.cas,
                             chem.name=chem.name,
                             dtxsid=dtxsid,
                             parameters=parameters,
                             httk.pop.biomets=httkpop.matrix,
                             model=model)
    } else {
      if(httkpop==T) 
        warning('httkpop model only available for human and thus not used.\n\
Set species=\"Human\" to run httkpop model.')   

      physiology.matrix <- monte_carlo(parameters.mean,
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
                      species=species)
    }
  }  

#
# PERFORM MONTE CARLO ON THE IN VITRO PARAMETERS
#
#Next add chemical-specific Funbound.plasma and CLint values
#Just cbind them together for now
  parameter.matrix <- cbind(physiology.matrix,
                draw_invitro(this.chem=chemcas,
                  parameters=parameters,
                  nsamp=nrow(indiv_bio),
                  poormetab=poormetab,
                  fup.meas.cv=fup.meas.cv,
                  clint.meas.cv=clint.meas.cv,
                  fup.pop.cv=fup.pop.cv,
                  clint.pop.cv=clint.pop.cv,
                  fup.censored.dist=fup.censored.dist,
                  fup.lod=fup.lod,
                  adjusted.Funbound.plasma=adjusted.Funbound.plasma,
                  clint.pvalue.threshold=clint.pvalue.threshold))

#
# UPDATE THE PARTITION COEFFICIENTS IF NEEDED
#
# For models with tissue-to-plasma partition coefficients we neeed to calculate
# them for each individual because each individual has a different 
# Funbound.plasma value:
  if (model.table[model]$ComputePCforMC)
  {
    if (apply())

    #First, get the default parameters used for the Schmitt method of estimating
    #partition coefficients.
    if (!is.null(this.chem))
    {
      pschmitt <- httk::parameterize_schmitt(chem.cas=this.chem,
                                           species='Human')
    } else {
      pschmitt <- parameters[param.names.schmitt[param.names.schmitt%in%names(parameters)]]
      pschmitt.chemindependent <- httk::parameterize_schmitt(chem.cas="80-05-7",species="Human")
      pschmitt <- c(pschmitt,pschmitt.chemindependent[c("Fprotein.plasma","plasma.pH","alpha")])
#      pschmitt[["MA"]] <- NA
    }
    #next, replace the single default value for Funbound.plasma with the vector
    #of Funbound.plasma values from the virtual population data.table.
    pschmitt$Funbound.plasma<-parameters.dt[, Funbound.plasma]

    #Now, predict the partitioning coefficients using Schmitt's method. The
    #result will be a list of numerical vectors, one vector for each
    #tissue-to-plasma partitioning coefficient, and one element of each vector
    #for each individual. The list element names specify which partition
    #coefficient it is, e.g. Kliver2plasma, Kgut2plasma, etc.
    PCs <- httk::predict_partitioning_schmitt(parameters=pschmitt,
                                              chem.cas=this.chem,
                                              species='Human',
                                              adjusted.Funbound.plasma=adjusted.Funbound.plasma,
                                              regression=regression)

  # List all tissues for which HTTK has human tissue information. 
  # This will be used in lumping.  
    tissuenames <- sort(unique(subset(httk::tissue.data,Species=="Human")$Tissue))
  # We don't use these tissues for lumping:
    tissuenames <- tissuenames[!(tissuenames %in% c("red blood cells"))]

# Lump the tissues, depending on model. tissues is a list of all the 
# unlumped compartments, all other tissues will be lumped into a "rest" 
# compartment.
    tissue.list <- model.table[model]$tissues
    if (!is.null(tissue.list))
    {
# Check to make sure that all the requested tissues are available:
      if (any(!(tissue.list %in% tissuenames))) stop(paste(
                                                  "Requested tissue(s)",
                                                  paste(tissue.list[
                                                    !(tissue.list %in% 
                                                    tissuenames)],
                                                    collapse=", "),
                                                  "are not in tissue.data."))
    
    #Now get the list of tissues to be lumped: that is, everything in
    #tissuenames that wasn't in the list of compartments for this model.
    rest.tissues <- tissuenames[!(tissuenames %in%
                                    c(tissue.list,
                                      'red blood cells'))]
    #Lump the volumes by simply summing them.
    vol.restc <- parameters.dt[,
                             Reduce('+', .SD),
                             .SDcols=paste0('V',
                                            rest.tissues,
                                            'c')]
    #Lump the flows by summing them.
    flow.restf <- parameters.dt[,
                              Reduce('+', .SD),
                              .SDcols=paste0('Q',
                                             rest.tissues,
                                             'f')]
    #Lumped partition coefficient: sum partition coefficients of rest.tissues,
    #weighted by their volumes; then divide by total lumped volume.
    Krest2pu <- Reduce('+',
                           lapply(as.list(rest.tissues),
                                  function(x) PCs[[paste0('K',
                                                          x,
                                                          '2pu')]]*
                                    unlist(parameters.dt[,
                                                       paste0('V',
                                                              x,
                                                              'c'),
                                                       with=FALSE])
                           )
    )/vol.restc

    #Add lumped volumes, flows, and partition coefficients to population
    #data.table
    parameters.dt[, Vrestc:=vol.restc]
    parameters.dt[, Qrestf:=flow.restf]
    parameters.dt[, Krest2pu:=Krest2pu]
    parameters.dt[, Krbc2pu:=PCs[['Krbc2pu']]]

    if (!(length(tissue.list)==0)){
      #For enumerated tissue compartments (if any), add their partitition
      #coefficients to the population data.table as well.

      #First get the vector of partition coefficient names
      #(names of elements of PCs)
      knames <- paste0('K',
                       tissue.list,
                       '2pu')
      #Then add them to the population data.table. data.table syntax: wrap
      #vector of column names in parentheses to assign to multiple columns at
      #once
      parameters.dt[, (knames):=PCs[knames]]
    }

    if (!"Rblood2plasma" %in% colnames(parameters.dt))
    {
      #For 1 compartment, 3 compartment, or PBTK models: Calculate Rblood2plasma
      #based on hematocrit and Krbc2plasma. This is the ratio of chemical in blood
      #vs. in plasma.
      if (is.null(parameters))
      {
        Rblood2plasma <- get_rblood2plasma(chem.cas=this.chem,species='Human')
      } else {
        Rblood2plasma <- calc_rblood2plasma(params=pschmitt,species="Human")
      }
      parameters.dt[,Rblood2plasma:=Rblood2plasma]
    }
    parameters.dt[is.na(Rblood2plasma),
                Rblood2plasma:=(1-
                                  hematocrit +
                                  hematocrit*
                                  Krbc2pu*
                                  Funbound.plasma)]
  }




#
# HERE LIES THE ACTUAL MONTE CARLO STEP:
#
# Calculate CSS for each row in the parameter matrix (each row corresponds to
# a different individual):
#
  css.list <- apply(parameter.matrix,1,css_apply) 
  
  if (!suppress.messages & !return.samples)
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
    out <- quantile(css.list,which.quantile,na.rm=T)     
  } else {
    out <- css.list  
    if (!suppress.messages) 
    {
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
      out <- css.list
    }
  }
}
