#'Calculate the analytic steady state concentration for the one compartment model.
#'
#'This function calculates the analytic steady state plasma or venous blood 
#'concentrations as a result of infusion dosing.
#'
#'@param chem.name Either the chemical name, CAS number, or the parameters must 
#'be specified.
#'@param chem.cas Either the chemical name, CAS number, or the parameters must 
#'be specified.
#'@param parameters Chemical parameters from parameterize_pbtk (for model = 
#''pbtk'), parameterize_3comp (for model = '3compartment), 
#'parmeterize_1comp(for model = '1compartment') or parameterize_steadystate 
#'(for model = '3compartmentss'), overrides chem.name and chem.cas.
#'@param hourly.dose Hourly dose rate mg/kg BW/h.
#'@param concentration Desired concentration type, 'blood' or default 'plasma'.
#'@param suppress.messages Whether or not the output message is suppressed.
#'@param recalc.blood2plasma Recalculates the ratio of the amount of chemical 
#'in the blood to plasma using the input parameters. Use this if you have 
#''altered hematocrit, Funbound.plasma, or Krbc2pu.
#'@param bioactive.free.invivo If FALSE (default), then the total concentration is treated
#' as bioactive in vivo. If TRUE, the the unbound (free) plasma concentration is treated as 
#' bioactive in vivo. Only works with tissue = NULL in current implementation.
#'@param tissue Desired tissue conentration (defaults to whole body 
#'concentration.)
#'@param restrictive.clearance If TRUE (default), then only the fraction of
#' chemical not bound to protein is available for metabolism in the liver. If 
#' FALSE, then all chemical in the liver is metabolized (faster metabolism due
#' to rapid off-binding). 
#'@param ... Additional parameters passed to parameterize function if 
#'parameters is NULL.
#'  
#'@return Steady state concentration in uM units
#'
#'@author Robert Pearce and John Wambaugh
calc_analytic_css_1comp <- function(chem.name=NULL,
                                   chem.cas = NULL,
                                   parameters=NULL,
                                   hourly.dose=1/24,
                                   concentration='plasma',
                                   suppress.messages=F,
                                   recalc.blood2plasma=F,
                                   tissue=NULL,
                                   restrictive.clearance=T,
                                   bioactive.free.invivo = F,
                                   Caco2.options = list(Caco2.Pab.default = "1.6",
                                                        Caco2.Fgut = TRUE,
                                                        Caco2.Fabs = TRUE,
                                                        overwrite.invivo = FALSE),
                                   ...)
{
  if (is.null(chem.cas) & is.null(chem.name) & is.null(parameters))
  {
    stop('Parameters, chem.name, or chem.cas must be specified.')
  }
  if(is.null(parameters))
  {
    parameters <- parameterize_1comp(chem.cas=chem.cas,
                                    chem.name=chem.name,
                                    suppress.messages=suppress.messages,
                                    restrictive.clearance=restrictive.clearance,
                                    Caco2.options = Caco2.options,
                                    ...)
    if (recalc.blood2plasma) 
    {
      warning("Argument recalc.blood2plasma=TRUE ignored because parameters is NULL.")
    }
  } else {
    if (!all(param.names.1comp %in% names(parameters))) 
    {
      stop(paste("Missing parameters:",
                 paste(param.names.1comp[which(!param.names.1comp %in% names(parameters))],
                   collapse=', '),
                 ".  Use parameters from parameterize_1comp."))
    }
    if (!restrictive.clearance) 
    {
      warning("Argument restrictive.clearance=FALSE ignored by model 1comp when parameters!=NULL.") 
    }
  }
  parameters$Fabsgut <- parameters$Fabsgut * parameters$hepatic.bioavailability
  
  hourly.dose <- hourly.dose * parameters$Fabsgut
  Css <- hourly.dose / parameters$kelim / parameters$Vdist

# Check to see if a specific tissue was asked for:
  if (!is.null(tissue))
  {
# Need to convert to Schmitt parameters:
    pcs <- predict_partitioning_schmitt(parameters = parameters[param.names.schmitt[param.names.schmitt %in% names(parameters)]])
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
      
    }else if(bioactive.free.invivo == T & tolower(concentration) == 'plasma'){
      
      Css <- Css * parameters[['Funbound.plasma']]
      
    } else if (tolower(concentration)!='plasma') 
    {
      stop("Only blood and plasma concentrations are calculated.")
    }
  }
  
  return(Css)
}
