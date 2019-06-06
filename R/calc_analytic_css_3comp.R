#'Calculate the analytic steady state concentration for model 3comp
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
calc_analytic_css_3comp <- function(chem.name=NULL,
                                   chem.cas = NULL,
                                   parameters=NULL,
                                   hourly.dose=1/24,
                                   concentration='plasma',
                                   suppress.messages=F,
                                   recalc.blood2plasma=F,
                                   tissue=NULL,
                                   restrictive.clearance=T,
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
    parameters <- parameterize_3comp(chem.cas=chem.cas,
                                    chem.name=chem.name,
                                    suppress.messages=suppress.messages,
                                    ...)
    if (recalc.blood2plasma) 
    {
      warning("Argument recalc.blood2plasma=TRUE ignored because parameters is NULL.")
    }
  } else {
    if (!all(param.names.3comp %in% names(parameters)))
    {
      stop(paste("Missing parameters:",
           paste(param.names.3comp[which(!param.names.3comp %in% names(parameters))],
             collapse=', '),
           ".  Use parameters from parameterize_3comp."))
    }
    if (any(param.names.pbtk[which(!param.names.pbtk %in% param.names.3comp)] 
      %in% names(parameters)))
    {
      stop("Parameters are from parameterize_pbtk. Use parameters from parameterize_3comp instead.")
    }
    if (recalc.blood2plasma) parameters[['Rblood2plasma']] <- 1 - 
      parameters[['hematocrit']] + 
      parameters[['hematocrit']] * parameters[['Krbc2pu']] * parameters[['Funbound.plasma']]
  }

  hourly.dose <- hourly.dose * parameters$Fgutabs
  fup <- parameters$Funbound.plasma
  Rblood2plasma <- parameters$Rblood2plasma
  Clmetabolism <- parameters$Clmetabolismc
  if (!restrictive.clearance) Clmetabolism <- Clmetabolism / fup
  Css <- hourly.dose * parameters[['BW']]^0.25  / 
    (Clmetabolism * parameters[['BW']]^0.25 + 
    parameters$Qgfrc * (parameters$Qliverf + 
    parameters$Qgutf) * parameters$Qcardiacc / 
    ((parameters$Qliverf + parameters$Qgutf) * parameters$Qcardiacc + 
    fup * parameters$Qgfrc / parameters$Rblood2plasma)) / fup

# Check to see if a specific tissue was asked for:
  if (!is.null(tissue))
  {
# Need to convert to 3compartmentss parameters:
    pcs <- predict_partitioning_schmitt(chem.cas=chem.cas,
      parameters=c(parameters[param.names.3compss%in%names(parameters)],
      Dow74=NA,
      hepatic.bioavailability=NA,
      Qtotal.liverc=(parameters$Qgutf+parameters$Qliverf)*parameters$Qcardiacc),
                                        ...)
    if (!paste0('K',tolower(tissue)) %in% 
      substr(names(pcs),1,nchar(names(pcs))-3))
    {
      stop(paste("Tissue",tissue,"is not available."))
    }
# Tissues with sources (gut) or sinks (liver,kidney) need to be calculated
# taking the change of mass into account:
    if (tissue == 'gut')
    {
      Qgut <- parameters$Qgutf * parameters$Qcardiacc / parameters$BW^0.25
      Css <- parameters[['Kgut2pu']] * fup * 
        (Css + dose / (Qgut * Rblood2plasma))
    } else if (tissue == 'liver') {
      Qliver <- (parameters$Qgutf + parameters$Qliverf) * parameters$Qcardiacc / 
        parameters$BW^0.25
      Clmetabolism <- parameters$Clmetabolismc
      if (!restrictive.clearance) Clmetabolism <- Clmetabolism / fup
      Css <- parameters[['Kliver2pu']] * fup * (hourly.dose + 
        Qliver * Css * Rblood2plasma) / 
        (Clmetabolism * fup + Qliver * Rblood2plasma)
    } else {
      Css <- Css * pcs[[names(pcs)[substr(names(pcs),2,nchar(names(pcs))-3)==tissue]]] * fup   
    }
  }
  
  if (tolower(concentration)=='blood')
  {
     Css <- Css * parameters[['Rblood2plasma']]
  } else if (tolower(concentration)!='plasma') stop("Only blood and plasma concentrations are calculated.")

  return(Css)
}
