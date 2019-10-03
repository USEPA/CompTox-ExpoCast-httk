model.list <- list()
#'Calculate the analytic steady state concentration.
#'
#'This function calculates the analytic steady state plasma or venous blood 
#'concentrations as a result of infusion dosing for the three compartment and 
#'multiple compartment PBTK models.
#'
#'@export
#'
#'@param chem.name Either the chemical name, CAS number, or the parameters must 
#'be specified.
#'@param chem.cas Either the chemical name, CAS number, or the parameters must 
#'be specified.
#'@param parameters Chemical parameters from parameterize_pbtk (for model = 
#''pbtk'), parameterize_3comp (for model = '3compartment), 
#'parmeterize_1comp(for model = '1compartment') or parameterize_steadystate 
#'(for model = '3compartmentss'), overrides chem.name and chem.cas.
#'@param daily.dose Total daily dose, mg/kg BW.
#'@param output.units Units for returned concentrations, defaults to uM 
#'(specify units = "uM") but can also be mg/L.
#'@param model Model used in calculation, 'pbtk' for the multiple compartment 
#'model,'3compartment' for the three compartment model, '3compartmentss' for 
#'the three compartment steady state model, and '1compartment' for one 
#'compartment model.
#'@param concentration Desired concentration type, 'blood','tissue', or default 'plasma'.
#'@param suppress.messages Whether or not the output message is suppressed.
#'@param recalc.blood2plasma Recalculates the ratio of the amount of chemical 
#'in the blood to plasma using the input parameters. Use this if you have 
#''altered hematocrit, Funbound.plasma, or Krbc2pu.
#'@param tissue Desired tissue conentration (defaults to whole body 
#'concentration.)
#'@param IVIVE Honda et al. (2019) identified four plausible sets of 
#'assumptions for \emph{in vitro-in vivo} extrapolation (IVIVE) assumptions. 
#'Argument may be set to "Honda1" through "Honda4". If used, this function 
#'overwrites the tissue, restrictive.clearance, and bioactive.free.invivo arguments. 
#'See Details below for more information.
#'@param restrictive.clearance If TRUE (default), then only the fraction of
#' chemical not bound to protein is available for metabolism in the liver. If 
#' FALSE, then all chemical in the liver is metabolized (faster metabolism due
#' to rapid off-binding). 
#'@param bioactive.free.invivo If FALSE (default), then the total concentration is treated
#' as bioactive in vivo. If TRUE, the the unbound (free) plasma concentration is treated as 
#' bioactive in vivo. Only works with tissue = NULL in current implementation.
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
#'calc_analytic_css(chem.name='Bisphenol-A',tissue='liver',species='rabbit',
#'                  default.to.human=TRUE,daily.dose=2)
#'calc_analytic_css(chem.name="bisphenol a",model="1compartment")
#'calc_analytic_css(chem.cas="80-05-7",model="3compartmentss")
#'params <- parameterize_pbtk(chem.cas="80-05-7") 
#'calc_analytic_css(parameters=params,model="pbtk")
#'
#'@author Robert Pearce, John Wambaugh, and Greg Honda
#'
#'@keywords Solve
#'
#' @references Honda, Gregory S., et al. "Using the Concordance of In Vitro and 
#' In Vivo Data to Evaluate Extrapolation Assumptions." 2019. PLoS ONE 14(5): e0217564.
#''
#' @export calc_analytic_css
calc_analytic_css <- function(chem.name=NULL,
                              chem.cas = NULL,
                              parameters=NULL,
                              species="human",
                              daily.dose=1,
                              output.units='uM',
                              model = 'pbtk',
                              concentration='plasma',
                              suppress.messages=F,
                              recalc.blood2plasma=F,
                              tissue=NULL,
                              restrictive.clearance = T,
                              bioactive.free.invivo = F,
                              IVIVE=NULL,
                              ...)
{
#  tissue.data <- tissue.data
#  physiology.data <- physiology.data
#  Tissue <- Species <- variable <- NULL
  
# Check that chemical info in specified somehow:
  if(is.null(chem.cas) & is.null(chem.name) & is.null(parameters)) stop('Must specify chem.cas, chem.name, or parameters.')
  
### MODEL PARAMETERS FOR R

# Make sure we have all the parameters necessary to describe the chemical (we don't
# necessarily need all parameters associated with a given model to do this:)
  if (is.null(parameters))
  {
  # name of function that generates the model parameters:
    parameterize_function <- model.list[[model]]$parameterize.func
    
    parameters <- do.call(parameterize_function,list(
      chem.cas=chem.cas,
      chem.name=chem.name,
      species=species,
      default.to.human=default.to.human,
      suppress.messages=suppress.messages,
      adjusted.Funbound.plasma=adjusted.Funbound.plasma,
      regression=regression,
      minimum.Funbound.plasma=minimum.Funbound.plasma)) 
  } else {
    if (!all(compiled_param_names %in% names(parameters)))
    {
      stop(paste("Missing parameters:",
        paste(compiled_param_names[which(!compiled_param_names %in% 
        names(parameters))],collapse=', '),
        ". Use parameters from",parameterize_function,".",sep="")) 
    }
  }
  
  # Rblood2plasma depends on hematocrit, Krbc2pu, and Funbound.plasma. If those
  # have been updated by Monte Carlo it may be that Rblood2plasma needs to be
  # recalculated:
  if (recalc.blood2plasma) parameters[['Rblood2plasma']] <- 
    1 - 
    parameters[['hematocrit']] + 
    parameters[['hematocrit']] * parameters[['Krbc2pu']] * 
    parameters[['Funbound.plasma']]
  Rblood2plasma <- parameters[["Rblood2plasma"]]
  
  # The unscald hepatic clearance depens on many parameters that could be changed
  # by Monte Carlo simulation. This code recalculates it if needed:
  if (recalc.clearance)
  {
  # Do we have all the parameters we need?:
    if (!all(model.list[[model]]$param.names%in%names(parameters)))
    {
      if (is.null(chem.name) & is.null(chem.cas)) 
        stop('Chemical name or CAS must be specified to recalculate hepatic clearance.')
      ss.params <- parameterize_steadystate(chem.name=chem.name,chem.cas=chem.cas)
    }
    ss.params[[names(ssparams) %in% names(parameters)]] <- 
      parameters[[names(ssparams) %in% names(parameters)]]
    parameters[['Clmetabolismc']] <- calc_hepatic_clearance(parameters=ss.params,
      hepatic.model='unscaled',
      suppress.messages=T)
  }
  
  # If the hepatic metabolism is now slowed by plasma protein binding (non-
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
      parameters=parameters,
      hourly.dose=hourly.dose,
      concentration=concentration,
      suppress.messages=suppress.messages,
      recalc.blood2plasma=recalc.blood2plasma,
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
  
  return(as.numeric(Css))
}
