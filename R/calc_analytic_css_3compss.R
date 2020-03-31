#'Calculate the analytic steady state concentration for the three oompartment
#'steady-state model
#'
#'This function calculates the analytic steady state plasma or venous blood 
#'concentrations as a result of infusion dosing.
#'
#'@param chem.name Either the chemical name, CAS number, or the parameters must 
#' be specified.
#'@param chem.cas Either the chemical name, CAS number, or the parameters must 
#' be specified.
#' @param dtxsid EPA's 'DSSTox Structure ID (\url{http://comptox.epa.gov/dashboard})   
#' the chemical must be identified by either CAS, name, or DTXSIDs
#'@param parameters Chemical parameters from parameterize_pbtk (for model = 
#' 'pbtk'), parameterize_3comp (for model = '3compartment), 
#' parmeterize_1comp(for model = '1compartment') or parameterize_steadystate 
#' (for model = '3compartmentss'), overrides chem.name and chem.cas.
#'@param hourly.dose Hourly dose rate mg/kg BW/h.
#'@param concentration Desired concentration type, 'blood' or default 'plasma'.
#'@param suppress.messages Whether or not the output message is suppressed.
#'@param recalc.blood2plasma Recalculates the ratio of the amount of chemical 
#' in the blood to plasma using the input parameters. Use this if you have 
#' 'altered hematocrit, Funbound.plasma, or Krbc2pu.
#'@param tissue Desired tissue concentration (defaults to whole body 
#'concentration.)
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
#'@return Steady state concentration in uM units
#'
#'@author Robert Pearce and John Wambaugh
#'@keywords 3compss
calc_analytic_css_3compss <- function(chem.name=NULL,
                                   chem.cas = NULL,
                                   dtxsid = NULL,
                                   parameters=NULL,
                                   hourly.dose=1/24,
                                   concentration='plasma',
                                   suppress.messages=F,
                                   recalc.blood2plasma=F,
                                   tissue=NULL,
                                   restrictive.clearance=T,
                                   bioactive.free.invivo = FALSE,
                                   ...)
{

  param.names.3compss <- model.list[["3compartmentss"]]$param.names
  param.names.schmitt <- model.list[["schmitt"]]$param.names
    
# We need to describe the chemical to be simulated one way or another:
  if (is.null(chem.cas) & 
      is.null(chem.name) & 
      is.null(dtxsid) &
      is.null(parameters)) 
    stop('parameters, chem.name, chem.cas, or dtxsid must be specified.')

# Look up the chemical name/CAS, depending on what was provided:
  if (is.null(parameters))
  {
    out <- get_chem_id(
            chem.cas=chem.cas,
            chem.name=chem.name,
            dtxsid=dtxsid)
    chem.cas <- out$chem.cas
    chem.name <- out$chem.name                                
    dtxsid <- out$dtxsid  

    parameters <- parameterize_steadystate(
                                    chem.cas=chem.cas,
                                    chem.name=chem.name,
                                    dtxsid=dtxsid,
                                    suppress.messages=suppress.messages,
                                    restrictive.clearance=restrictive.clearance,
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
                                                   parameters=parameters,
                                                   hematocrit=parameters$hematocrit)
  }

  Fup <- parameters$Funbound.plasma
  Rb2p <- parameters$Rblood2plasma 
  BW <- parameters$BW
  # Total blood flow (gut plus arterial) into liver:
  Qtotalliver <- parameters$Qtotal.liverc/BW^0.25 #L/h/kg BW

# Scale up from in vitro Clint to a whole liver clearance:
  cl <- calc_hep_clearance(parameters=parameters,
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
    if (is.null(chem.cas) & is.null(chem.name) & is.null(dtxsid) & !any(c("Pow", "MA", "pKa_Accept", "pKa_Donor") %in% names(parameters)))
      stop("Either chem.cas, chem.name, or dtxsid must be specified to give tissue concs with this model. Try model=\"pbtk\".")
# Need to convert to 3compartmentss parameters:
    if(!all(c("Pow", "MA", "pKa_Accept", "pKa_Donor") %in% names(parameters))){
      parameters <- add_schmitt.param_to_3compss(parameters = parameters, chem.cas = chem.cas, chem.name = chem.name)
    }
    pcs <- predict_partitioning_schmitt(parameters = parameters[param.names.schmitt[param.names.schmitt %in% names(parameters)]])
    if (!paste0('K',tolower(tissue)) %in% 
      substr(names(pcs),1,nchar(names(pcs))-3))
    {
      stop(paste("Tissue",tissue,"is not available."))
    }

    Css <- Css * pcs[[names(pcs)[substr(names(pcs),2,nchar(names(pcs))-3)==tissue]]] * Fup   
  }

  if(tolower(concentration) != "tissue"){
    
    if (tolower(concentration)=='blood')
    {
      Css <- Css * Rb2p
      
    }else if(bioactive.free.invivo == T & tolower(concentration) == 'plasma'){
      
      Css <- Css * parameters[['Funbound.plasma']]
      
    } else if (tolower(concentration)!='plasma') stop("Only blood and plasma concentrations are calculated.")      
  }
  return(Css)
}




# Add some parameters to the output from parameterize_steady_state so that predict_partitioning_schmitt can run without reparameterizing
add_schmitt.param_to_3compss <- function(parameters = NULL, chem.cas = NULL, chem.name = NULL){
  
  if ((is.null(chem.cas) & is.null(chem.name) & is.null(dtxsid)))
    stop("Either chem.cas or chem.name must be specified to give tissue concs with this model. Try model=\"pbtk\".")
  if (is.null(parameters))
    stop("Must have input parameters to add Schmitt input to.")
  # Need to convert to 3compartmentss parameters:
  temp.params <- get_physchem_param(chem.cas = chem.cas, chem.name = chem.name,
                                    dtxsid = dtxsid, param = c("logP", "logMA", "pKa_Accept","pKa_Donor"))
  if(!"Pow" %in% names(parameters)){
    parameters[["Pow"]] <- 10^temp.params[["logP"]]
  }
  if(!"MA" %in% names(parameters)){
    parameters[["MA"]] <- 10^temp.params[["logMA"]]
  }
  if(!"pKa_Accept" %in% names(parameters)){
    parameters[["pKa_Accept"]] <- temp.params[["pKa_Accept"]]
  }
  if(!"pKa_Donor" %in% names(parameters)){
    parameters[["pKa_Donor"]] <- temp.params[["pKa_Donor"]]
  }
  return(parameters)
}
