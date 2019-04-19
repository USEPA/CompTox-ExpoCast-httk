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
#'@param concentration Desired concentration type, 'blood' or default 'plasma'.
#'@param suppress.messages Whether or not the output message is suppressed.
#'@param recalc.blood2plasma Recalculates the ratio of the amount of chemical 
#'in the blood to plasma using the input parameters. Use this if you have 
#''altered hematocrit, Funbound.plasma, or Krbc2pu.
#'@param tissue Desired tissue conentration (defaults to whole body 
#'concentration.)
#'@param IVIVE Honda et al. (submitted) identified six plausible sets of 
#'assumptions for \emph{in vitro-in vivo} extrapolation (IVIVE) assumptions. 
#'Argument may be set to "Honda1" through "Honda6". If used, this function 
#'overwrites the tissue, restrictive.clearance, and plasma.binding arguments. 
#'See Details below for more information.
#'@param restrictive.clearance If TRUE (default), then only the fraction of
#' chemical not bound to protein is available for metabolism in the liver. If 
#' FALSE, then all chemical in the liver is metabolized (faster metabolism due
#' to rapid off-binding). 
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
#'@author Robert Pearce and John Wambaugh
#'
#'@keywords Solve
#'
#'@references Honda, Gregory S., et al. "Using the Concordance of In Vitro and 
#'In Vivo Data to Evaluate Extrapolation Assumptions", submitted.
model.list <- list()
calc_analytic_css <- function(chem.name=NULL,
                               chem.cas = NULL,
                               parameters=NULL,
                               daily.dose=1,
                               output.units='uM',
                               model = 'pbtk',
                               concentration='plasma',
                               suppress.messages=F,
                               recalc.blood2plasma=F,
                               tissue=NULL,
                               restrictive.clearance=T,
                               IVIVE=NULL,
                               ...)
{
#  tissue.data <- tissue.data
#  physiology.data <- physiology.data
#  Tissue <- Species <- variable <- NULL
  
# Check that chemical info in specified somehow:
  if(is.null(chem.cas) & is.null(chem.name) & is.null(parameters)) stop('Must specify chem.cas, chem.name, or parameters.')
  
# If argument IVIVE is set, change arguments to match Honda et al. (2019) 
# IVIVE parameters:
  if (!is.null(IVIVE)) 
  {
    out <- honda.ivive(method=IVIVE,tissue=tissue)
    restrictive.clearance <- out[["restrictive.clearance"]]
    tissue <- out[["tissue"]]
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

# Retrieve the molecular weight if the parameters argument isn't given:'
  if (is.null(parameters) | !is.null(tissue))
  {
    if(is.null(chem.cas))
    {
      out <- get_chem_id(chem.name=chem.name)
      chem.cas <- out$chem.cas
    }
    MW <- get_physchem_param('MW',chem.CAS=chem.cas)
  }else{
    MW <- parameters[['MW']]
  }
  
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
      restrictive.clearance=restrictive.clearance),
      list(...)))
  } else {
    stop(paste("Model",model,"not available. Please select from:",
      paste(names(model.list),collapse=", ")))
  }

# Convert to uM if requested
  if (tolower(output.units)=='um')
  { 
    Css <- Css / 1000 / MW * 100000 # mg/L -> uM
  }

#User message:
  if (!suppress.messages)
  {
    if (tolower(concentration)=="plasma") concentration <- "Plasma"
    else if (tolower(concentration)=="blood") concentration <- "Blood"
    if(is.null(tissue)) cat(paste(concentration,"concentration returned in",output.units,"units.\n"))
    else cat(paste(concentration,"for",tissue,"returned in",output.units,"units.\n"))
  }
  
  return(as.numeric(Css))
}


model.list[["pbtk"]]$analytic.css.func <- "calc_analytic_css_pbtk"
#'Calculate the analytic steady state concentration for model pbtk.
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
calc_analytic_css_pbtk <- function(chem.name=NULL,
                                   chem.cas = NULL,
                                   parameters=NULL,
                                   hourly.dose=1/24,
                                   concentration='plasma',
                                   suppress.messages=F,
                                   recalc.blood2plasma=F,
                                   tissue=NULL,
                                   restrictive.clearance=T,
                                   ...)
{
  if (is.null(chem.cas) & is.null(chem.name) & is.null(parameters))
  {
    stop('Parameters, chem.name, or chem.cas must be specified.')
  }
  if (is.null(parameters))
  {
    parameters <- parameterize_pbtk(chem.cas=chem.cas,
                                    chem.name=chem.name,
                                    suppress.messages=suppress.messages,
                                    ...) 
    if (recalc.blood2plasma) 
    {
      warning("Argument recalc.blood2plasma=TRUE ignored because parameters is NULL.")
    }
  } else {
    if (!all(param.names.pbtk %in% names(parameters)))
    {
      stop(paste("Missing parameters:",
           paste(param.names.pbtk[which(!param.names.pbtk %in% names(parameters))],
             collapse=', '),
           ".  Use parameters from parameterize_pbtk.")) 
    }
    if (recalc.blood2plasma) {
      parameters[['Rblood2plasma']] <- 1 - 
        parameters[['hematocrit']] + 
        parameters[['hematocrit']] * parameters[['Krbc2pu']] * parameters[['Funbound.plasma']]
    }
  }
  
  Qcardiac <-  parameters[["Qcardiacc"]] / parameters[['BW']]^0.25  
  Qgfr <-  parameters[["Qgfrc"]] / parameters[['BW']]^0.25    
  Clmetabolism <-  parameters[["Clmetabolismc"]]  
  Kliver2pu <- parameters[['Kliver2pu']]
  
  Qgut <- parameters[["Qgutf"]] * Qcardiac
  Qliver <- parameters[["Qliverf"]] * Qcardiac
  Qkidney <- parameters[['Qkidneyf']] * Qcardiac
  Qrest <- Qcardiac-Qgut-Qliver-Qkidney
  Rblood2plasma <- parameters[['Rblood2plasma']]
  fup <- parameters[["Funbound.plasma"]]
  if (!restrictive.clearance) Clmetabolism <- Clmetabolism / fup
  
  hourly.dose <- hourly.dose * parameters$Fgutabs
  
# Calculate steady-state plasma Css:
  Css <- (hourly.dose * (Qliver + Qgut) / 
         (fup * Clmetabolism / Rblood2plasma + (Qliver + Qgut))) / 
         (Qcardiac - (Qliver + Qgut)**2 /
         (fup * Clmetabolism / Rblood2plasma + (Qliver + Qgut)) - 
         Qkidney**2 / (Qgfr * fup / Rblood2plasma + Qkidney) - Qrest)

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
      Css <- parameters[['Kgut2pu']] * parameters[['Funbound.plasma']] * 
        (Css + dose / (Qgut * parameters[['Rblood2plasma']]))
    } else if (tissue == 'liver') {
      Qliver <- (parameters$Qgutf + parameters$Qliverf) * parameters$Qcardiacc / 
        parameters$BW^0.25
      Clmetabolism <- parameters$Clmetabolismc
      if (!restrictive.clearance) Clmetabolism <- Clmetabolism / fup
      Css <- parameters[['Kliver2pu']] * fup * (hourly.dose + 
        Qliver * Css * Rblood2plasma) / 
        (Clmetabolism * fup + Qliver * Rblood2plasma)
    } else if(tissue == 'kidney') {
      Qkidney <- parameters$Qkidneyf * parameters$Qcardiacc / parameters$BW^0.25
      Css <- parameters[['Kkidney2pu']] * fup * Qkidney * Css * Rblood2plasma /
        (Qkidney * Rblood2plasma + parameters$Qgfrc * fup)
# All other tissues are proportional based on the partition coefficient:
    } else {
      Css <- Css * pcs[[names(pcs)[substr(names(pcs),2,nchar(names(pcs))-3)==tissue]]] * fup   
    }
  }

  if (tolower(concentration)=='plasma')
  {
    Css <- Css / Rblood2plasma
    concentration <- "Plasma"
  } else if (tolower(concentration)=='blood')
  {
    concentration <- "Blood"
  } else {
    stop("Only blood and plasma concentrations are calculated.")
  }
  
  return(Css)
}

model.list[["3compartmentss"]]$analytic.css.func <- "calc_analytic_css_3compss"
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
  if (is.na(parameters$hepatic.bioavailability)) browser() 
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

    Css <- Css * pcs[[names(pcs)[substr(names(pcs),2,nchar(names(pcs))-3)==tissue]]] * Fup   
  }

  if (tolower(concentration)=='blood')
  {
    Css <- Css * Rb2p
  } else if (tolower(concentration)!='plasma') stop("Only blood and plasma concentrations are calculated.")      

  return(Css)
}


model.list[["1compartment"]]$analytic.css.func <- "calc_analytic_css_1comp"
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
  parameters$Fgutabs <- parameters$Fgutabs * parameters$hepatic.bioavailability
  
  hourly.dose <- hourly.dose * parameters$Fgutabs
  Css <- hourly.dose / parameters$kelim / parameters$Vdist

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

    Css <- Css * 
      pcs[[names(pcs)[substr(names(pcs),2,nchar(names(pcs))-3)==tissue]]] * 
      parameters$Funbound.plasma   
  }
  
  if (tolower(concentration)=='blood')
  {
    Css <- Css * parameters[['Rblood2plasma']]
  } else if (tolower(concentration)!='plasma') 
  {
    stop("Only blood and plasma concentrations are calculated.")
  }
  
  return(Css)
}

model.list[["3compartment"]]$analytic.css.func <- "calc_analytic_css_3comp"
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
