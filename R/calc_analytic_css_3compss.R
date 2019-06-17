#'Calculate the analytic steady state concentration for the three oompartment
#'steady-state model
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
calc_analytic_css_3compss <- function(chem.name=NULL,
                                   chem.cas = NULL,
                                   parameters=NULL,
                                   hourly.dose=1/24,
                                   concentration='plasma',
                                   suppress.messages=F,
                                   recalc.blood2plasma=F,
                                   tissue=NULL,
                                   restrictive.clearance=T,
                                   bioactive.free.invivo = F,
                                   Caco2.options = list(Caco2.Pab.default = 2,
                                                        Caco2.Fgut = TRUE,
                                                        Caco2.Fabs = TRUE),
                                   ...)
{
  if (is.null(chem.cas) & is.null(chem.name) & is.null(parameters))
  {
    stop('Parameters, chem.name, or chem.cas must be specified.')
  }
  if (is.null(parameters))
  {
    parameters <- parameterize_steadystate(chem.cas=chem.cas,
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
    if (!all(param.names.3compss %in% names(parameters)))
    {
      stop(paste("Missing parameters:",
                 paste(param.names.3compss[which(!param.names.3compss %in% names(parameters))],
                   collapse=', '),
                 ".  Use parameters from parameterize_steadystate."))
    }
  }
  if (any(parameters$Funbound.plasma == 0)) 
  {
    stop('Fraction unbound plasma cannot be zero.')
  }
#  if (is.na(parameters$hepatic.bioavailability)) browser() 
  if (recalc.blood2plasma) 
  {
    parameters$Rblood2plasma <- calc_rblood2plasma(chem.cas=chem.cas,
                                                   params=parameters,
                                                   hematocrit=parameters$hematocrit)
  }

  Fup <- parameters$Funbound.plasma
  Rb2p <- parameters$Rblood2plasma 
  BW <- parameters$BW
  # Total blood flow (gut plus arterial) into liver:
  Qtotalliver <- parameters$Qtotal.liverc/BW^0.25 #L/h/kg BW

# Scale up from in vitro Clint to a whole liver clearance:
  cl <- calc_hepatic_clearance(parameters=parameters,
          hepatic.model='unscaled',
          suppress.messages=T)#L/h/kg body weight
  if (!restrictive.clearance) cl <- cl*Fup

# Calculate steady-state plasma Css, Pearce et al. (2017) equation section 2.2:
  Css <- parameters$Fgutabs * 
    parameters$hepatic.bioavailability *
    hourly.dose / (
    parameters$Qgfrc/BW^0.25 * Fup + 
    Qtotalliver*Fup*cl /
    (Qtotalliver + Fup*cl/Rb2p))
    
# Check to see if a specific tissue was asked for:
  if (!is.null(tissue))
  {
# We need logP, which currently isn't one of the 3compss parameters, so unless
# the user gives chem.name/chem.cas, we can't run:
    if (is.null(chem.cas) & is.null(chem.name) & !("Pow" %in% names(parameters)))
      stop("Either chem.cas or chem.name must be specified to give tissue concs with this model. Try model=\"pbtk\".")
# Need to convert to 3compartmentss parameters:
    pcs <- predict_partitioning_schmitt(chem.cas=chem.cas)
    if (!paste0('K',tolower(tissue)) %in% 
      substr(names(pcs),1,nchar(names(pcs))-3))
    {
      stop(paste("Tissue",tissue,"is not available."))
    }

    Css <- Css * pcs[[names(pcs)[substr(names(pcs),2,nchar(names(pcs))-3)==tissue]]] * Fup   
  }

  if (tolower(concentration)=='blood')
  {
    Css <- Css * Rb2p
  }else if(bioactive.free.invivo == T & tolower(concentration) == 'plasma'){
    
    Css <- Css * Fup
    
  }else if (tolower(concentration)!='plasma') stop("Only blood and plasma concentrations are calculated.")      

  return(Css)
}
