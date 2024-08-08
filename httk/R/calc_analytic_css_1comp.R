#' Calculate the analytic steady state concentration for the one compartment model.
#'
#' This function calculates the analytic steady state plasma or venous blood 
#' concentrations as a result of infusion dosing.
#' 
#' @param chem.name Either the chemical name, CAS number, or the parameters must 
#' be specified.
#' 
#' @param chem.cas Either the chemical name, CAS number, or the parameters must 
#' be specified.
#' 
#' @param dtxsid EPA's 'DSSTox Structure ID (\url{https://comptox.epa.gov/dashboard})  
#' the chemical must be identified by either CAS, name, or DTXSIDs
#' 
#' @param parameters Chemical parameters from parameterize_pbtk (for model = 
#' 'pbtk'), parameterize_3comp (for model = '3compartment), 
#' parameterize_1comp(for model = '1compartment') or parameterize_steadystate 
#' (for model = '3compartmentss'), overrides chem.name and chem.cas.
#' 
#' @param hourly.dose Hourly dose rate mg/kg BW/h.
#' 
#' @param concentration Desired concentration type, 'blood' or default 'plasma'.
#' 
#' @param suppress.messages Whether or not the output message is suppressed.
#' 
#' @param recalc.blood2plasma Recalculates the ratio of the amount of chemical 
#' in the blood to plasma using the input parameters. Use this if you have 
#' altered hematocrit, Funbound.plasma, or Krbc2pu.
#' 
#' @param tissue Desired tissue conentration (defaults to whole body 
#' concentration.)
#' 
#' @param restrictive.clearance If TRUE (default), then only the fraction of
#' chemical not bound to protein is available for metabolism in the liver. If 
#' FALSE, then all chemical in the liver is metabolized (faster metabolism due
#' to rapid off-binding). 
#' 
#' @param bioactive.free.invivo If FALSE (default), then the total concentration is treated
#' as bioactive in vivo. If TRUE, the the unbound (free) plasma concentration is treated as 
#' bioactive in vivo. Only works with tissue = NULL in current implementation.
#' 
#' @param dosing List of dosing metrics used in simulation, which includes
#' the namesake entries of a model's associated dosing.params. For steady-state
#' calculations this is likely to be either "daily.dose" for oral exposures or
#' "Cinhaled" for inhalation.
#'
#' @param dose.units The units associated with the dose received.
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
#' @param ... Additional parameters passed to parameterize function if 
#' parameters is NULL.
#'  
#' @return Steady state plasma concentration in mg/L units
#'
#' @seealso \code{\link{calc_analytic_css}}
#'
#' @seealso \code{\link{parameterize_1comp}}
#'
#' @author Robert Pearce and John Wambaugh
#'
#' @keywords 1compartment
calc_analytic_css_1comp <- function(chem.name=NULL,
                                   chem.cas = NULL,
                                   dtxsid = NULL,
                                   parameters=NULL,
                                   dosing=list(daily.dose=1),
                                   hourly.dose = NULL,
                                   dose.units = "mg",
                                   concentration='plasma',
                                   suppress.messages=FALSE,
                                   recalc.blood2plasma=FALSE,
                                   tissue=NULL,
                                   restrictive.clearance=TRUE,
                                   bioactive.free.invivo = FALSE,
                                   Caco2.options = list(),
                                   ...)
{
  if (!is.null(hourly.dose))
  {
     warning("calc_analytic_css_3compss deprecated argument hourly.dose replaced with new argument dose, value given assigned to dose")
     dosing <- list(daily.dose = 24*hourly.dose)
  }
  
# Load from modelinfo file:
  THIS.MODEL <- "1compartment"
  param.names <- model.list[[THIS.MODEL]]$param.names
  param.names.schmitt <- model.list[["schmitt"]]$param.names
  parameterize_function <- model.list[[THIS.MODEL]]$parameterize.func
  
# We need to describe the chemical to be simulated one way or another:
  if (is.null(chem.cas) & 
      is.null(chem.name) & 
      is.null(dtxsid) &
      is.null(parameters)) 
    stop('parameters, chem.name, chem.cas, or dtxsid must be specified.')

# Look up the chemical name/CAS, depending on what was provide:
  if (is.null(parameters))
  {
    out <- get_chem_id(
            chem.cas=chem.cas,
            chem.name=chem.name,
            dtxsid=dtxsid)
    chem.cas <- out$chem.cas
    chem.name <- out$chem.name                                
    dtxsid <- out$dtxsid  

    parameters <- do.call(what=parameterize_function, 
                          args=purrr::compact(c(
                            list(chem.cas=chem.cas,
                                 chem.name=chem.name,
                                 suppress.messages=suppress.messages,
                                 Caco2.options = Caco2.options,
                                 restrictive.clearance = restrictive.clearance
                                 ),
                            ...)))
      
    if (recalc.blood2plasma) 
    {
      warning("Argument recalc.blood2plasma=TRUE ignored because parameters is NULL.")
    }
  } else {
    if (!all(param.names %in% names(parameters))) 
    {
      stop(paste("Missing parameters:",
                 paste(param.names[which(!param.names %in% names(parameters))],
                   collapse=', '),
                 ".  Use parameters from parameterize_1comp."))
    }
    if (!restrictive.clearance) 
    {
      warning("Argument restrictive.clearance=FALSE ignored by model 1comp when parameters!=NULL.") 
    }
  }

  
  # one compartment Css is dose.rate / clearance:

  # Dose rate:
  hourly.dose <- dosing[["daily.dose"]] /
                   parameters[["BW"]] /
                   24 *
                   convert_units(MW = parameters[["MW"]],
                                 dose.units,
                                 "mg") # mg/kg/h

  Css <- hourly.dose / parameters[["kelim"]] / parameters[["Vdist"]]
  # Convert to plasma concentration:
  Css <- Css/parameters[["Rblood2plasma"]]
  
# Check to see if a specific tissue was asked for:
  if (!is.null(tissue))
  {
# Need to convert to Schmitt parameters:
    #The parameters used in predict_partitioning_schmitt may be a compound
    #data.table/data.frame or list object, however, depending on the source 
    #of the parameters. In calc_mc_css, for example, parameters is received 
    #as a "data.table" object. Screen for processing appropriately.
    if (any(class(parameters) == "data.table")){
      pcs <- predict_partitioning_schmitt(parameters =
            parameters[, param.names.schmitt[param.names.schmitt %in% 
                                             names(parameters)], with = F])
    }else if (is(parameters,"list")) {
      pcs <- predict_partitioning_schmitt(parameters =
         parameters[param.names.schmitt[param.names.schmitt %in% 
                                    names(parameters)]])
    }else stop('httk is only configured to process parameters as objects of 
               class list or class compound data.table/data.frame.')
      
    if (!paste0('K',tolower(tissue)) %in% 
      substr(names(pcs),1,nchar(names(pcs))-3))
    {
      stop(paste("Tissue",tissue,"is not available."))
    }

    Css <- Css * 
      pcs[[names(pcs)[substr(names(pcs),2,nchar(names(pcs))-3)==tissue]]] * 
      parameters$Funbound.plasma   
  }
  
  if(tolower(concentration) != 'tissue'){
    if (tolower(concentration)=='blood')
    {
      Css <- Css * parameters[['Rblood2plasma']]
      
    }else if(bioactive.free.invivo == TRUE & tolower(concentration) == 'plasma'){
      
      Css <- Css * parameters[['Funbound.plasma']]
      
    } else if (tolower(concentration)!='plasma') 
    {
      stop("Only blood and plasma concentrations are calculated.")
    }
  }
  
  return(Css)
}
