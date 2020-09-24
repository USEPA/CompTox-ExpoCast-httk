#Initialize model.list to be fleshed out in the model info files and elsewhere. 
model.list <- list()

#'Calculate the analytic steady state concentration.
#'
#'This function calculates the analytic steady state plasma or venous blood 
#'concentrations as a result of infusion dosing for the three compartment and 
#'multiple compartment PBTK models.
#'
#'
#'@param chem.name Either the chemical name, CAS number, or the parameters must 
#'be specified.
#'@param chem.cas Either the chemical name, CAS number, or the parameters must 
#'be specified.
#' @param dtxsid EPA's DSSTox Structure ID (\url{https://comptox.epa.gov/dashboard})  
#' the chemical must be identified by either CAS, name, or DTXSIDs
#'@param parameters Chemical parameters from parameterize_pbtk (for model = 
#''pbtk'), parameterize_3comp (for model = '3compartment), 
#'parmeterize_1comp(for model = '1compartment') or parameterize_steadystate 
#'(for model = '3compartmentss'), overrides chem.name and chem.cas.
#'@param species Species desired (either "Rat", "Rabbit", "Dog", "Mouse", or
#' default "Human").
#'@param daily.dose Total daily dose, mg/kg BW.
#'@param output.units Units for returned concentrations, defaults to uM 
#'(specify units = "uM") but can also be mg/L.
#'@param model Model used in calculation, 'pbtk' for the multiple compartment 
#'model,'3compartment' for the three compartment model, '3compartmentss' for 
#'the three compartment steady state model, and '1compartment' for one 
#'compartment model.
#'@param concentration Desired concentration type, 'blood','tissue', or default 'plasma'.
#'@param suppress.messages Whether or not the output message is suppressed.
#'@param tissue Desired tissue conentration (defaults to whole body 
#'concentration.)
#'@param restrictive.clearance If TRUE (default), then only the fraction of
#' chemical not bound to protein is available for metabolism in the liver. If 
#' FALSE, then all chemical in the liver is metabolized (faster metabolism due
#' to rapid off-binding). 
#'@param bioactive.free.invivo If FALSE (default), then the total concentration is treated
#' as bioactive in vivo. If TRUE, the the unbound (free) plasma concentration is treated as 
#' bioactive in vivo. Only works with tissue = NULL in current implementation.
#'@param IVIVE Honda et al. (2019) identified four plausible sets of 
#'assumptions for \emph{in vitro-in vivo} extrapolation (IVIVE) assumptions. 
#'Argument may be set to "Honda1" through "Honda4". If used, this function 
#'overwrites the tissue, restrictive.clearance, and bioactive.free.invivo arguments. 
#'See Details below for more information.
#'@param parameterize.args List of arguments passed to model's associated
#' parameterization function, including default.to.human, 
#' adjusted.Funbound.plasma, regression, and minimum.Funbound.plasma. The 
#' default.to.human argument substitutes missing animal values with human values
#' if true, adjusted.Funbound.plasma returns adjusted Funbound.plasma when set 
#' to TRUE along with parition coefficients calculated with this value, 
#' regression indicates whether or not to use the regressions in calculating
#' partition coefficients, and minimum.Funbound.plasma is the value to which
#' Monte Carlo draws less than this value are set (default is 0.0001 -- half
#' the lowest measured Fup in our dataset).
#'@param ... Additional parameters passed to parameterize function if 
#'parameters is NULL.
#'  
#'@return Steady state concentration
#'
#'@details Concentrations are calculated for the specifed model with constant 
#'oral infusion dosing.  All tissues other than gut, liver, and lung are the 
#'product of the steady state plasma concentration and the tissue to plasma 
#'partition coefficient. 
#'\tabular{lrrrr}{
#' \tab \emph{in vivo} Conc. \tab Metabolic Clearance \tab Bioactive Chemical Conc. \tab TK Statistic Used* \cr
#'Honda1 \tab Veinous (Plasma) \tab Restrictive \tab Free \tab Mean Conc. \cr
#'Honda2 \tab Veinous \tab Restrictive \tab Free \tab Max Conc. \cr
#'Honda3 \tab Veinous \tab Non-restrictive \tab Total \tab Mean Conc. \cr
#'Honda4 \tab Veinous \tab Non-restrictive \tab Total \tab Max Conc. \cr
#'Honda5 \tab Target Tissue \tab Non-restrictive \tab Total \tab Mean Conc. \cr
#'Honda6 \tab Target Tissue \tab Non-restrictive \tab Total \tab Max Conc. \cr
#'}
#'*Assumption is currently ignored because analytical steady-state solutions are currently used by this function.
#'  
#'@examples 
#'calc_analytic_css(chem.name='Bisphenol-A',output.units='mg/L',
#'                  model='3compartment',concentration='blood')
#' 
#'calc_analytic_css(chem.name='Bisphenol-A',tissue='liver',species='rabbit',
#'                  parameterize.args = list(
#'                                 default.to.human=TRUE,
#'                                 adjusted.Funbound.plasma=TRUE,
#'                                 regression=TRUE,
#'                                 minimum.Funbound.plasma=1e-4),daily.dose=2)
#' 
#'calc_analytic_css(chem.name="bisphenol a",model="1compartment")
#' 
#'calc_analytic_css(chem.cas="80-05-7",model="3compartmentss")
#' 
#'params <- parameterize_pbtk(chem.cas="80-05-7") 
#' 
#'calc_analytic_css(parameters=params,model="pbtk")
#'
#'@author Robert Pearce, John Wambaugh, and Greg Honda
#'
#'@keywords Solve
#'
#'@references Honda, Gregory S., et al. "Using the Concordance of In Vitro and 
#'In Vivo Data to Evaluate Extrapolation Assumptions." 2019. PLoS ONE 14(5): e0217564.
#'
#'@export calc_analytic_css
#'@import methods
calc_analytic_css <- function(chem.name=NULL,
                              chem.cas = NULL,
                              dtxsid = NULL,
                              parameters=NULL,
                              species="human",
                              daily.dose=1,
                              output.units='uM',
                              model = 'pbtk',
                              concentration='plasma',
                              suppress.messages=F,
                              tissue=NULL,
                              restrictive.clearance = T,
                              bioactive.free.invivo = F,
                              IVIVE=NULL,
                              parameterize.args = list(
                                default.to.human=F,
                                adjusted.Funbound.plasma=T,
                                regression=T,
                                minimum.Funbound.plasma=1e-4),
                              ...)
{  
  if (is.null(model)) stop("Model must be specified.")
# We need to know model-specific information (from modelinfo_[MODEL].R]) 
# to set up the solver:
  model <- tolower(model)
  if (!(model %in% names(model.list)))            
  {
    stop(paste("Model",model,"not available. Please select from:",
      paste(names(model.list),collapse=", ")))
  } 
  parameterize_function <- model.list[[model]]$parameterize.func
      
# We need to describe the chemical to be simulated one way or another:
  if (is.null(chem.cas) & 
      is.null(chem.name) & 
      is.null(dtxsid) &
      is.null(parameters)) 
    stop('parameters, chem.name, chem.cas, or dtxsid must be specified.')
  
### MODEL PARAMETERS FOR R

# Make sure we have all the parameters necessary to describe the chemical (we don't
# necessarily need all parameters associated with a given model to do this:)
  if (is.null(parameters))
  {
# Look up the chemical name/CAS/dtxsid, depending on what was provided:
    out <- get_chem_id(
            chem.cas=chem.cas,
            chem.name=chem.name,
            dtxsid=dtxsid)
    chem.cas <- out$chem.cas
    chem.name <- out$chem.name                                
    dtxsid <- out$dtxsid  
    
    
    parameterize.args <- c(parameterize.args,list(
      chem.cas=chem.cas,
      chem.name=chem.name,
      species=species,
      suppress.messages=suppress.messages))
# Make sure all the arguments are used by the function:
    parameterize.args <- parameterize.args[names(parameterize.args) %in% 
      methods::formalArgs(parameterize_function)]
    parameters <- do.call(parameterize_function, parameterize.args) 
  } else {
    model_param_names <- model.list[[model]]$param.names 
    if (!all(model_param_names %in% names(parameters)))
    {
      stop(paste("Missing parameters:",
        paste(model_param_names[which(!model_param_names %in% 
        names(parameters))],collapse=', '),
        ". Use parameters from",parameterize_function,".",sep="")) 
    }
  }

# If argument IVIVE is set, change arguments to match Honda et al. (2019) 
# IVIVE parameters:
  if (!is.null(IVIVE)) 
  {
    out <- honda.ivive(method=IVIVE, tissue=tissue)
    restrictive.clearance <- out[["restrictive.clearance"]]
    tissue <- out[["tissue"]]
    bioactive.free.invivo <- out[["bioactive.free.invivo"]]
    concentration <- out[["concentration"]]
  }
    
# If the hepatic metabolism is not slowed by plasma protein binding (non-
# restrictive clearance)  
  if (!restrictive.clearance) parameters$Clmetabolismc <- 
    parameters$Clmetabolismc / parameters$Funbound.plasma
  
# If there is not an explicit liver we need to include a factor for first-
# pass metabolism:
  if (!is.null(model.list[[model]]$do.first.pass))
    if (model.list[[model]]$do.first.pass)
  {
    parameters$Fgutabs <- parameters$Fgutabs * parameters$hepatic.bioavailability
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
  
# Check that the output units are ones we can work with:
  good.units <- c("uM","mg/L")
  if (!(tolower(output.units) %in% tolower(good.units))) 
  {
    stop(paste("Do not know how to calculate units",output.units,
      ". Please select from: ",paste(good.units,collapse=", ")))
  }
  
# Convert to hourly dose:
  hourly.dose <- daily.dose / 24 # mg/kg/h

# Retrieve the molecular weight:
  MW <- parameters[['MW']]
  
  if (model %in% names(model.list))            
  {
    Css <- do.call(model.list[[model]]$analytic.css.func,c(list(
      chem.cas = chem.cas,
      chem.name = chem.name,
      dtxsid = dtxsid,
      parameters=parameters,
      hourly.dose=hourly.dose,
      concentration=concentration,
      suppress.messages=suppress.messages,
      tissue=tissue,
      restrictive.clearance=restrictive.clearance,
      bioactive.free.invivo = bioactive.free.invivo),
      list(...)))
  } else {
    stop(paste("Model",model,"not available. Please select from:",
      paste(names(model.list),collapse=", ")))
  }

# Convert to uM if requested
  if (tolower(output.units)=='um')
  { 
    Css <- Css / 1e3 / MW * 1e6 # mg/L -> uM
  }

#User message:
  if (!suppress.messages)
  {
    if (tolower(concentration)=="plasma") concentration <- "Plasma"
    else if (tolower(concentration)=="blood") concentration <- "Blood"
    else if (tolower(concentration) == "tissue") concentration <- "Tissue"
    if(is.null(tissue)) cat(paste(concentration,"concentration returned in",output.units,"units.\n"))
    else cat(paste(concentration,"concentration for",tissue,"returned in",output.units,"units.\n"))
  }
  
# Cannot guarantee arbitrary precision:
  Css <- set_httk_precision(Css)
  
  return(as.numeric(Css))
}