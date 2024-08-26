#Initialize model.list to be fleshed out in the model info files and elsewhere. 
model.list <- list()

#'Calculate the analytic steady state plasma concentration.
#'
#' @description
#' This function calculates the analytic steady state plasma or venous blood 
#'concentrations as a result of infusion dosing for the three compartment and 
#'multiple compartment PBTK models.
#'
#'
#'@param chem.name Either the chemical name, CAS number, or the parameters must 
#'be specified.
#'
#'@param chem.cas Either the chemical name, CAS number, or the parameters must 
#'be specified.
#'
#' @param dtxsid EPA's DSSTox Structure ID (\url{https://comptox.epa.gov/dashboard})  
#' the chemical must be identified by either CAS, name, or DTXSIDs
#'
#' @param parameters Chemical parameters from parameterize_pbtk (for model = 
#' 'pbtk'), parameterize_3comp (for model = '3compartment), 
#' parameterize_1comp(for model = '1compartment') or parameterize_steadystate 
#' (for model = '3compartmentss'), overrides chem.name and chem.cas.
#'
#' @param species Species desired (either "Rat", "Rabbit", "Dog", "Mouse", or
#' default "Human").
#'
#' @param route Route of exposure (either "oral", "iv", or "inhalation"
#' default "oral").
#'
#' @param daily.dose Total daily dose, mg/kg BW.
#'
#' @param output.units Units for returned concentrations, defaults to uM 
#' (specify units = "uM") but can also be mg/L.
#'
#' @param model Model used in calculation,'gas_pbtk' for the gas pbtk model, 
#' 'pbtk' for the multiple compartment model,
#' '3compartment' for the three compartment model, '3compartmentss' for 
#' the three compartment steady state model, and '1compartment' for one 
#' compartment model.
#'
#' @param suppress.messages Whether or not the output message is suppressed.
#'
#' @param tissue Desired steady state tissue concentration. Default is of NULL
#' typically gives whole body plasma concentration.
#'
#' @param concentration Desired concentration type: 'blood','tissue', or default 
#' 'plasma'. In the case that the concentration is for plasma, selecting "blood"
#' will use the blood:plasma ratio to estimate blood concentration. In the case
#' that the argument 'tissue' specifies a particular tissue of the body, 
#' concentration defaults to 'tissue' -- that is, the concentration in the 
#' If cocentration is set to 'blood' or 'plasma' and 'tissue' specifies a
#' specific tissue then the value returned is for the plasma or blood in that
#' specific tissue.
#'
#'@param restrictive.clearance If TRUE (default), then only the fraction of
#' chemical not bound to protein is available for metabolism in the liver. If 
#' FALSE, then all chemical in the liver is metabolized (faster metabolism due
#' to rapid off-binding). 
#'
#'@param bioactive.free.invivo If FALSE (default), then the total concentration is treated
#' as bioactive in vivo. If TRUE, the the unbound (free) plasma concentration is treated as 
#' bioactive in vivo. Only works with tissue = NULL in current implementation.
#'
#' @param Caco2.options A list of options to use when working with Caco2 apical to
#' basolateral data \code{Caco2.Pab}, default is Caco2.options = list(Caco2.Pab.default = 1.6,
#' Caco2.Fabs = TRUE, Caco2.Fgut = TRUE, overwrite.invivo = FALSE, keepit100 = FALSE). Caco2.Pab.default sets the default value for 
#' Caco2.Pab if Caco2.Pab is unavailable. Caco2.Fabs = TRUE uses Caco2.Pab to calculate
#' fabs.oral, otherwise fabs.oral = \code{Fabs}. Caco2.Fgut = TRUE uses Caco2.Pab to calculate 
#' fgut.oral, otherwise fgut.oral = \code{Fgut}. overwrite.invivo = TRUE overwrites Fabs and Fgut in vivo values from literature with 
#' Caco2 derived values if available. keepit100 = TRUE overwrites Fabs and Fgut with 1 (i.e. 100 percent) regardless of other settings.
#' See \code{\link{get_fbio}} for further details.
#'
#'@param IVIVE Honda et al. (2019) identified four plausible sets of 
#'assumptions for \emph{in vitro-in vivo} extrapolation (IVIVE) assumptions. 
#'Argument may be set to "Honda1" through "Honda4". If used, this function 
#'overwrites the tissue, restrictive.clearance, and bioactive.free.invivo arguments. 
#'See Details below for more information.
#'
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
#'
#' @param dose The amount of chemial to which the individual is exposed.
#'
#' @param dose.units The units associated with the dose received.
#'
#'@param ... Additional parameters passed to parameterize function if 
#'parameters is NULL.
#'  
#'@return Steady state plasma concentration in specified units
#'
#'@details Concentrations are calculated for the specifed model with constant 
#'oral infusion dosing.  All tissues other than gut, liver, and lung are the 
#'product of the steady state plasma concentration and the tissue to plasma 
#'partition coefficient. 
#'
#' Only four sets of IVIVE assumptions that performed well in Honda et al. 
#' (2019) are currently included in \code{\link{honda.ivive}}:
#' "Honda1" through "Honda4". The use of max (peak) 
#' concentration can not be currently be calculated with \code{\link{calc_analytic_css}}. 
#' The httk default settings correspond to "Honda3":
#' 
#'\tabular{lrrrrr}{
#' \tab \emph{In Vivo} Conc. \tab Metabolic Clearance \tab Bioactive Chemical Conc. \emph{In Vivo} \tab TK Statistic Used* \tab Bioactive Chemical Conc. \emph{In Vitro} \cr
#'Honda1 \tab Veinous (Plasma) \tab Restrictive \tab Free \tab Mean Conc. In Vivo \tab Free Conc. In Vitro \cr
#'Honda2 \tab Veinous \tab Restrictive \tab Free \tab Mean Conc. In Vivo \tab Nominal Conc. In Vitro \cr
#'Honda3 \tab Veinous \tab Restrictive \tab Total \tab Mean Conc. In Vivo \tab Nominal Conc. In Vitro \cr
#'Honda4 \tab Target Tissue \tab Non-restrictive \tab Total \tab Mean Conc. In Vivo \tab Nominal Conc. In Vitro \cr
#'}
#'
#' "Honda1" uses plasma concentration, restrictive clearance, and treats the 
#' unbound invivo concentration as bioactive. For IVIVE, any input nominal 
#' concentration in vitro should be converted to cfree.invitro using 
#' \code{\link{armitage_eval}}, otherwise performance will be the same as 
#' "Honda2". 
#'  
#'@examples 
#'calc_analytic_css(chem.name='Bisphenol-A',output.units='mg/L',
#'                  model='3compartment',concentration='blood')
#' 
#' \donttest{
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
#' # Try various chemicals with differing parameter sources/issues:
#' calc_analytic_css(chem.name="Betaxolol")
#' calc_analytic_css(chem.name="Tacrine",model="pbtk")
#' calc_analytic_css(chem.name="Dicofol",model="1compartment")
#' calc_analytic_css(chem.name="Diflubenzuron",model="3compartment")
#' calc_analytic_css(chem.name="Theobromine",model="3compartmentss")
#'}
#'
#' @seealso \code{\link{calc_css}}
#'
#'@author Robert Pearce, John Wambaugh, Greg Honda, Miyuki Breen
#'
#'@keywords Solve
#'
#' @references 
#' \insertRef{honda2019using}{httk}
#'
#'@export calc_analytic_css
#'@import methods
calc_analytic_css <- function(chem.name=NULL,
                              chem.cas = NULL,
                              dtxsid = NULL,
                              parameters=NULL,
                              species="human",
                              daily.dose=NULL,
                              dose=1,
                              dose.units="mg/kg/day",
                              route="oral",
                              output.units='uM',
                              model = 'pbtk',
                              concentration='plasma',
                              suppress.messages=FALSE,
                              tissue=NULL,
                              restrictive.clearance = TRUE,
                              bioactive.free.invivo = FALSE,
                              IVIVE=NULL,
                              Caco2.options = list(),
                              parameterize.args = list(),
                              ...)
{  
  if (!is.null(daily.dose))
  {
     warning("calc_analytic_css deprecated argument daily.dose replaced with new argument dose, value given assigned to dose")
     dose <- daily.dose
  }
  
  # scale_dosing does not handle time so we do that here:
  if (regexpr("/h", dose.units)!=-1) 
  {
    dose <- dose * 24
    dose.units <- gsub("/h", "", dose.units)
  }
  if (regexpr("/day", dose.units)!=-1) 
  {
    dose.units <- gsub("/day", "", dose.units)
  }
    
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
  compartment_state <- model.list[[model]]$compartment.state
  dose.var <- model.list[[model]]$routes[[route]][["entry.compartment"]]
    
  # Build dosing list according to route:
  available.routes <- names(model.list[[model]]$routes)  
  if (!route %in% available.routes)
  {
    stop(paste("Route", 
               route, 
               "not available for model", 
               model,
               ". Please select from:",
               paste(available.routes,collapse=", ")))
  }

# We need to describe the chemical to be simulated one way or another:
  if (is.null(chem.cas) & 
      is.null(chem.name) & 
      is.null(dtxsid) &
      is.null(parameters)) 
    stop('parameters, chem.name, chem.cas, or dtxsid must be specified.')
  
  # Error handling for tissue argument:
  if (!is.null(tissue))
  {
    if (is.null(model.list[[model]]$alltissues))
    {
      stop(paste("Tissues are not available for model", model))
    }
    if (!(tissue %in% model.list[[model]]$alltissues))
    {
      stop(paste("Tissue", tissue, "not available for model", model))
    }
  }  
  
  # Error handling for concentration arugment:
  if (!(concentration %in% c("blood","tissue","plasma")))
  {
    stop("Concentration must be one of blood, tissue, or plasma")
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
  
  if ((bioactive.free.invivo == TRUE & !is.null(tissue)) | 
     (bioactive.free.invivo == TRUE & tolower(concentration) != "plasma")
     )
  {
    stop("Option bioactive.free.invivo only works with tissue = NULL and concentration = \"plasma\".\n
         Ctissue * Funbound.plasma is not a relevant concentration.\n
         Cfree_blood should be the same as Cfree_plasma = Cplasma*Funbound.plasma.")
  }  
    
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

  # pass chemical information plus formal argument parameterize.args to the
  # parameterization function specified by the appropriate modelinfo file:
    parameters <- do.call(what=parameterize_function, 
      args=purrr::compact(c(list(
        chem.cas=chem.cas,
        chem.name=chem.name,
        dtxsid=dtxsid,
        species=species,
        Caco2.options=Caco2.options,
        suppress.messages=suppress.messages),
      parameterize.args)))
 
  } else {
    model_param_names <- model.list[[model]]$param.names 
    if (!all(model_param_names %in% names(parameters)))
    {
      stop(paste("Missing parameters:",
        paste(model_param_names[which(!model_param_names %in% 
        names(parameters))],collapse=', '),
        ". Use parameters from ",parameterize_function,".",sep="")) 
    }
  }
    
# If there is not an explicit liver we need to include a factor for first-
# pass metabolism:
  if (!is.null(model.list[[model]]$do.first.pass))
    if (model.list[[model]]$do.first.pass)
  {
    parameters$Fbio.oral <- parameters$Fabsgut * parameters$hepatic.bioavailability
  }
 
  # Check if value "all" is used for a state (that is, all compartments are
  # the same state of matter):
  if (all(tolower(compartment_state[[1]])%in%"all"))
  {
    entry.compartment.state <- names(compartment_state)[1]
  } else {
    # Check whether the dose.var is in the compartment.state list & if so which
    entry.state.check <- unlist(lapply(compartment_state,function(x){dose.var%in%x}))
    if(any(entry.state.check)){
      entry.compartment.state <- names(compartment_state)[which(entry.state.check==TRUE)]
    }else{
      stop(paste0("Entry compartment state is not specified in the model.list for ",model,"."))
    }
  }           
    
  # Scale dose by body weight if necessary:
  if (regexpr("/kg", dose.units)!=-1) 
  {
    dose <- dose*parameters[["BW"]]
    dose.units <- gsub("/kg", "", dose.units)
  }

  if (route == "oral")
  {
    dosing = list(daily.dose = dose)
  } else if (route == "inhalation") {
    dosing = list(Cinhppmv = dose)
  } else stop(paste("Do not know how to handle steady-state dosing for route",
                    route))
    
  if (model %in% names(model.list))            
  {
      Css <- do.call(model.list[[model]]$analytic.css.func,
        args=purrr::compact(c(list(
          chem.cas = chem.cas,
          chem.name = chem.name,
          dtxsid=dtxsid,
          parameters=parameters,
          dosing=dosing,
          dose.units=dose.units,
          route=route,
          concentration=concentration,
          suppress.messages=suppress.messages,
          tissue=tissue,
          restrictive.clearance=restrictive.clearance,
          bioactive.free.invivo = bioactive.free.invivo,
          Caco2.options = Caco2.options),
          list(...))))
  } else {
    stop(paste("Model",model,"not available. Please select from:",
               paste(names(model.list),collapse=", ")))
  }

  # Check modelinfo file:
  if (is.null(model.list[[model]]$steady.state.units))
    stop(paste("steady.state.units not set for model",model))

  # Convert units:
  if (tolower(model.list[[model]]$steady.state.units) != 
    tolower(output.units))
  Css <- Css * convert_units(model.list[[model]]$steady.state.units,
                 output.units, 
                 chem.cas = chem.cas,
                 chem.name = chem.name,
                 dtxsid=dtxsid,
                 parameters = parameters)

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