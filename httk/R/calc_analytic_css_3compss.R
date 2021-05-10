#'Calculate the analytic steady state concentration for the three compartment
#'steady-state model
#'
#'This function calculates the analytic steady state plasma or venous blood 
#'concentrations as a result of infusion dosing.
#'
#'@param chem.name Either the chemical name, CAS number, or the parameters must 
#' be specified.
#'@param chem.cas Either the chemical name, CAS number, or the parameters must 
#' be specified.
#' @param dtxsid EPA's 'DSSTox Structure ID (\url{https://comptox.epa.gov/dashboard})   
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
                                   suppress.messages=FALSE,
                                   recalc.blood2plasma=FALSE,
                                   tissue=NULL,
                                   restrictive.clearance=TRUE,
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

# Expand on any provided chemical identifiers if possible (if any but not
# all chemical descriptors are NULL):
  chem_id_list  = list(chem.cas, chem.name, dtxsid)
  if (any(lapply(chem_id_list, is.null)) &
      !all(lapply(chem_id_list, is.null))){
  out <- get_chem_id(
    chem.cas=chem.cas,
    chem.name=chem.name,
    dtxsid=dtxsid)
  chem.cas <- out$chem.cas
  chem.name <- out$chem.name                                
  dtxsid <- out$dtxsid  
  }
  
# Fetch some parameters using parameterize_steadstate, if needed:
  if (is.null(parameters))
  {
    if (recalc.blood2plasma) 
    {
      warning("Argument recalc.blood2plasma=TRUE ignored because parameters is NULL.")
    }
    
    parameters <- parameterize_steadystate(
                                    chem.cas=chem.cas,
                                    chem.name=chem.name,
                                    dtxsid=dtxsid,
                                    suppress.messages=suppress.messages,
                                    restrictive.clearance=restrictive.clearance,
                                    ...)

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
          suppress.messages=TRUE)#L/h/kg body weight
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
    # We need logP, the pKa's, and membrane affinity, which currently isn't one 
    # of the 3compss parameters, so unless the user provides these parameters,
    # they need to give a chemical identifier like chem.name/chem.cas/dtxsid, or
    # we can't find them in the chem.physical_and_invitro.data set and run:
    if (!any(c("Pow", "MA", "pKa_Accept", "pKa_Donor") %in% 
             names(parameters))) {
      #We do a lookup of these needed parameters using a targeted version of 
      #get_physchem_param for the 3 compss model, add_schmitt.param_to_3compss
      #(function definition nested at bottom):
        parameters <- add_schmitt.param_to_3compss(parameters = parameters,
           chem.cas = chem.cas, chem.name = chem.name, dtxsid = dtxsid)
    }

    #The parameters used in predict_partitioning_schmitt may be a compound
    #data.table/data.frame or list object, however, depending on the source 
    #of the parameters. In calc_mc_css, for example, parameters is received 
    #as a "data.table" object. Screen for processing appropriately, and 
    #pass our parameters to predict_partitioning_schmitt so we can get
    #the needed pc's.
    if (any(class(parameters) == "data.table")){
      pcs <- predict_partitioning_schmitt(parameters =
          parameters[, param.names.schmitt[param.names.schmitt %in% 
          names(parameters)], with = F])
    }else if (class(parameters) == "list") {
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


# Add some parameters to the output from parameterize_steady_state so that
# predict_partitioning_schmitt can run without reparameterizing
add_schmitt.param_to_3compss <- function(parameters = NULL, chem.cas = NULL,
                                         chem.name = NULL, dtxsid = NULL){
  
  if ((is.null(chem.cas) & is.null(chem.name) & is.null(dtxsid)))
    stop("Either chem.cas, chem.name, or dtxsid must be specified to give 
          tissue concs with this model. Try model=\"pbtk\".")
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
