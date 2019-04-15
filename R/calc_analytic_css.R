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
#'@param species Species desired (either "Rat", "Rabbit", "Dog", "Mouse", or 
#'default "Human"). 
#'@param suppress.messages Whether or not the output message is suppressed.
#'@param concentration Desired concentration type, 'blood' or default 'plasma'.
#'@param recalc.blood2plasma Recalculates the ratio of the amount of chemical 
#'in the blood to plasma using the input parameters, calculated with hematocrit,
#'Funbound.plasma, and Krbc2pu.
#'@param default.to.human Substitutes missing rat values with human values if 
#'true.
#'@param tissue Desired tissue conentration, overwrites concentration 
#'argument.
#'@param adjusted.Funbound.plasma Uses adjusted Funbound.plasma when set to 
#'TRUE along with partition coefficients calculated with this value.
#'@param regression}{Whether or not to use the regressions in calculating 
#'partition coefficients.
#'@param well.stirred.correction Uses correction in calculation of hepatic 
#'clearance for well-stirred model if TRUE for model 1compartment and 
#'3compartmentss. This assumes clearance relative to amount unbound in whole 
#'blood instead of plasma, but converted to use with plasma concentration.
#'@param IVIVE Honda et al. (submitted) identified six plausible sets of 
#'assumptions for \emph{in vitro-in vivo} extrapolation (IVIVE) assumptions. 
#'Argument may be set to "Honda1" through "Honda6". If used, this function 
#'overwrites the tissue, restrictive.clearance, and plasma.binding arguments. 
#'See Details below for more information.
#'@param restrictive.clearance If TRUE (default), then only the fraction of
#' chemical not bound to protein is available for metabolism in the liver. If 
#' FALSE, then all chemical in the liver is metabolized (faster metabolism due
#' to rapid off-binding). 
#'@param ... Additional parameters passed to parameterize functions if parameters is NULL.
#'  
#'@return Steady state concentration                                                               %'      #'
#'@details Tissue concentrations are calculated for the pbtk model with oral 
#'infusion dosing.  All tissues other than gut, liver, and lung are the product
#'of the steady state plasma concentration and the tissue to plasma partition 
#'coefficient. 
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

 calc_analytic_css <- function(chem.name=NULL,
                               chem.cas = NULL,
                               parameters=NULL,
                               daily.dose=1,
                               output.units='uM',
                               model = 'pbtk',
                               species='Human',
                               concentration='plasma',
                               suppress.messages=F,
                               recalc.blood2plasma=F,
                               default.to.human=F,
                               tissue=NULL,
                               well.stirred.correction=T,
                               adjusted.Funbound.plasma=T,
                               regression=T,
                               restrictive.clearance=T,
                               IVIVE=NULL,
                               ...)
{
  tissue.data <- tissue.data
  physiology.data <- physiology.data
  Tissue <- Species <- variable <- NULL
  
  if(is.null(chem.cas) & is.null(chem.name) & is.null(parameters)) stop('Must specify chem.cas, chem.name, or parameters.')
  
  if (!is.null(IVIVE)) 
  {
    out <- honda.ivive(method=IVIVE,tissue=tissue)
    restrictive.clearance <- out[["restrictive.clearance"]]
    tissue <- out[["tissue"]]
  }
     
  dose <- daily.dose
  good.units <- c("uM","mg/L")
  if (!(tolower(output.units) %in% tolower(good.units))) stop(paste("Do not know how to calculate units",output.units,". Please select from: ",paste(good.units,collapse=", ")))
  dose <- dose / 24 
  if(is.null(parameters) | !is.null(tissue)){
    if(is.null(chem.cas)){
      out <- get_chem_id(chem.name=chem.name)
      chem.cas <- out$chem.cas
    }
    MW <- get_physchem_param('MW',chem.CAS=chem.cas)
  }else{
    MW <- parameters[['MW']]
  }
  if(!is.null(tissue)){
    concentration <- 'plasma'    
    if(tissue %in% c('gut','liver')){
      if(tolower(output.units) == 'um'){
        output.units <- 'mg/l'
        unit.change <- T
      }else unit.change <- F
    }
  }
  if (tolower(output.units)=='um')
  { 
       dose <- dose / 1000 / MW * 100000 # Analytic solution is linear with dose so okay to change here
  } else if(tolower(output.units) != 'mg/l') stop('Output.units can only be uM or mg/L.')
    
  if(tolower(model)=='pbtk')
  {
     if(is.null(parameters))
     {
       parameters <- parameterize_pbtk(chem.cas=chem.cas,
                                       species=species,
                                       default.to.human=default.to.human,
                                       adjusted.Funbound.plasma=adjusted.Funbound.plasma,
                                       regression=regression,
                                       ...)
       user.params <- F 
     } else {
       if (!all(param.names.pbtk %in% names(parameters))) 
       {
         stop(paste("Missing parameters:",
                    paste(param.names.pbtk[which(!param.names.pbtk %in% names(parameters))],
                      collapse=', '),
                    ".  Use parameters from parameterize_pbtk."))
       }
       if (recalc.blood2plasma) parameters[['Rblood2plasma']] <- 1 - parameters[['hematocrit']] + parameters[['hematocrit']] * parameters[['Krbc2pu']] * parameters[['Funbound.plasma']]
       user.params <- T
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
     fub <- parameters[["Funbound.plasma"]]
     if(!restrictive.clearance) Clmetabolism <- Clmetabolism / fub

     dose <- dose * parameters$Fgutabs
     
     Css <- (dose * (Qliver + Qgut) / (fub * Clmetabolism / Rblood2plasma + (Qliver + Qgut))) / (Qcardiac - (Qliver + Qgut)**2 /(fub * Clmetabolism / Rblood2plasma + (Qliver + Qgut)) - Qkidney**2 / (Qgfr * fub / Rblood2plasma + Qkidney) - Qrest)
   if (tolower(concentration)=='plasma')
    {
      Css <- Css / parameters[['Rblood2plasma']]
    } else if (tolower(concentration)!='blood') stop("Only blood and plasma concentrations are calculated.")
  }
  else if (tolower(model)=='3compartmentss')
  {
    if (is.null(parameters))
    {
      parameters <- parameterize_steadystate(chem.cas=chem.cas,
                                             species=species,
                                             default.to.human=default.to.human,
                                             adjusted.Funbound.plasma=adjusted.Funbound.plasma,
                                             restrictive.clearance=restrictive.clearance,
                                             ...)
      user.params <- F
    } else {
      if (!all(param.names.3compss %in% names(parameters)))
      {
        stop(paste("Missing parameters:",
                   paste(param.names.3compss[which(!param.names.3compss %in% names(parameters))],
                     collapse=', '),
                   ".  Use parameters from parameterize_steadystate."))
      }
      user.params <- T
    }
    if (any(parameters$Funbound.plasma == 0)) 
    {
      stop('Fraction unbound plasma cannot be zero.  Use calc_mc_css or get_wetmore_css to predict steady state for this chemical with three compartment steady state model.')
    }
    if (is.null(parameters[["Rblood2plasma"]])|recalc.blood2plasma) 
    {
      parameters$Rblood2plasma <- calc_rblood2plasma(chem.cas=chem.cas,
                                                     params=parameters,
                                                     hematocrit=parameters$hematocrit)
    }

    Fup <- parameters$Funbound.plasma
    Rb2p <- parameters$Rblood2plasma 
    cl <- calc_hepatic_clearance(parameters=parameters,
                                 chem.cas=chem.cas,
                                 chem.name=chem.name,
                                 species=species,
                                 well.stirred.correction=well.stirred.correction,
                                 suppress.messages=T,
                                 restrictive.clearance=restrictive.clearance)
    Qliver <- parameters$Qtotal.liverc / parameters$BW^.25
#      Rb2p <- available_rblood2plasma(chem.name=chem.name,chem.cas=chem.cas,species=species,adjusted.Funbound.plasma=adjusted.Funbound.plasma)
    if (restrictive.clearance) {
      parameters[['hepatic.bioavailability']] <- Qliver / (Qliver +  Fup * cl / Rb2p)
    } else {
      parameters[['hepatic.bioavailability']] <- Qliver / (Qliver + cl / Rb2p) 
    }
    dose <- dose * parameters$Fgutabs * parameters$hepatic.bioavailability

    Css <- dose/(parameters$Qgfrc/parameters[['BW']]^.25 * Fup + cl) 
    if (tolower(concentration)=='blood')
    {
#       if(is.null(chem.name) & is.null(chem.cas)) stop("Enter chem.name or chem.cas with appropriate species and default.to.human options for desired concentration.")
#        Rb2p <- available_rblood2plasma(chem.cas=chem.cas,species=species,adjusted.Funbound.plasma=adjusted.Funbound.plasma)
      Css <- Css * Rb2p
    } else if (tolower(concentration)!='plasma') stop("Only blood and plasma concentrations are calculated.")      
  } else if(tolower(model) == '1compartment') 
  {
    if(is.null(parameters))
    {
      parameters <- parameterize_1comp(chem.cas=chem.cas,
                                       species=species,
                                       default.to.human=default.to.human,
                                       adjusted.Funbound.plasma=adjusted.Funbound.plasma,
                                       regression=regression,
                                       restrictive.clearance=restrictive.clearance,
                                       ...)
      user.params <- F
    } else {
      if (!all(param.names.1comp %in% names(parameters))) 
      {
        stop(paste("Missing parameters:",
                   paste(param.names.1comp[which(!param.names.1comp %in% names(parameters))],
                     collapse=', '),
                   ".  Use parameters from parameterize_1comp."))
      } 
      user.params <- T            
    }
    parameters$Fgutabs <- parameters$Fgutabs * parameters$hepatic.bioavailability
    
    dose <- dose * parameters$Fgutabs
    Css <- dose / parameters$kelim / parameters$Vdist
    if (tolower(concentration)=='blood')
    {
      Css <- Css * parameters[['Rblood2plasma']]
    } else if (tolower(concentration)!='plasma') stop("Only blood and plasma concentrations are calculated.")
  } else if (tolower(model) == '3compartment')
  {
    if (is.null(parameters))
    {
      parameters <- parameterize_3comp(chem.cas=chem.cas,species=species,default.to.human=default.to.human,adjusted.Funbound.plasma=adjusted.Funbound.plasma,regression=regression,...)
      user.params <- F
    } else {

      if (!all(param.names.3comp %in% names(parameters)))
      {
        stop(paste("Missing parameters:",
             paste(param.names.3comp[which(!param.names.3comp %in% names(parameters))],
               collapse=', '),
             ".  Use parameters from parameterize_3comp."))
      }
      if(any(param.names.pbtk[which(!param.names.pbtk %in% param.names.3comp)] %in% names(parameters)))
      {
        stop("Parameters are from parameterize_pbtk.  Use parameters from parameterize_3comp.")
      }
      if(recalc.blood2plasma) parameters[['Rblood2plasma']] <- 1 - parameters[['hematocrit']] + parameters[['hematocrit']] * parameters[['Krbc2pu']] * parameters[['Funbound.plasma']]
      user.params <- T
    }

    dose <- dose * parameters$Fgutabs
    fub <- parameters$Funbound.plasma
    Clmetabolism <- parameters$Clmetabolismc
    if(!restrictive.clearance) Clmetabolism <- Clmetabolism / fub
    Css <- dose * parameters[['BW']]^0.25  / (Clmetabolism * parameters[['BW']]^0.25  + parameters$Qgfrc * (parameters$Qliverf + parameters$Qgutf) * parameters$Qcardiacc / ((parameters$Qliverf + parameters$Qgutf) * parameters$Qcardiacc + fub * parameters$Qgfrc / parameters$Rblood2plasma)) / fub
    if (tolower(concentration)=='blood')
    {
       Css <- Css * parameters[['Rblood2plasma']]
    } else if (tolower(concentration)!='plasma') stop("Only blood and plasma concentrations are calculated.")
  } else stop("Model must be either \"pbtk\", \"1compartment\", \"3compartmentss\", or \"3compartment\".")
    
  if(!is.null(tissue)){
    if(model == '1compartment'){ 
      if(!'BW' %in% names(parameters) & tolower(tissue) %in% c('gut','kidney','liver')){
        parameters[['BW']] <- physiology.data[physiology.data[,'Parameter']=='Average BW',paste0(toupper(substr(species,1,1)), tolower(substr(species,2,nchar(species))))]
        if(user.params) warning(paste(species,'body weight used in calculating',tissue,'concentration.  Species of parameters should match species argument of input function, or BW should be included with input parameters.')) 
      }
      if(!'Funbound.plasma' %in% names(parameters)){
        if(is.null(chem.name) & is.null(chem.cas)) stop("Include Funbound.plasma in parameters or enter chem.name or chem.cas with appropriate species and default.to.human options for desired tissue concentration.")
        parameters <- c(parameters,parameterize_schmitt(chem.cas=chem.cas,default.to.human=default.to.human,species=species)['Funbound.plasma'])
      }  
    } 
    if(!paste0('K',tolower(tissue)) %in% substr(names(parameters),1,nchar(names(parameters))-3)){
      if(is.null(chem.name) & is.null(chem.cas)) stop("Enter chem.name or chem.cas with appropriate species and default.to.human options for desired tissue concentration.")
      pcs <- predict_partitioning_schmitt(chem.cas=chem.cas,species=species,default.to.human=default.to.human,tissues=tissue)
      parameters <- c(parameters,pcs[!substr(names(pcs),1,nchar(names(pcs))-3) %in% substr(names(parameters),1,nchar(names(parameters))-3)])
    }
     if(!'Rblood2plasma' %in% names(parameters) & tolower(tissue) %in% c('gut','kidney','liver')) parameters[['Rblood2plasma']] <- available_rblood2plasma(chem.cas=chem.cas,chem.name=chem.name,species=species,adjusted.Funbound.plasma=adjusted.Funbound.plasma)    
    if(tissue == 'gut'){
      if(!all(c('Qcardiacc','Qgutf') %in% names(parameters))){
        Qgut <- as.numeric(subset(tissue.data,Tissue == 'gut' & tolower(Species) == tolower(species) & variable == 'Flow (mL/min/kg^(3/4))')[,'value']) * 60 / 1000 / parameters$BW^.25
        if(user.params) warning(paste(species,'gut flow used in calculating',tissue,'concentration.  Species of parameters should match species argument of input function.'))
      }else Qgut <- parameters$Qgutf * parameters$Qcardiacc / parameters$BW^0.25
      Css <- parameters[['Kgut2pu']] * parameters[['Funbound.plasma']] * (Css + dose / (Qgut * parameters[['Rblood2plasma']]))
      if(unit.change){
        Css <- Css / parameters$MW * 1000
        output.units <- 'uM'
      }
    }else if(tissue == 'liver'){
      if('Qtotal.liverc' %in% names(parameters)) Qliver <- parameters$Qtotal.liverc / parameters$BW^0.25
      else if(all(c('Qgutf','Qliverf','Qcardiacc') %in% names(parameters))) Qliver <- (parameters$Qliverf + parameters$Qgutf) * parameters$Qcardiacc / parameters$BW^0.25
      else{
        Qliver <- as.numeric(subset(tissue.data,Tissue == 'liver' & tolower(Species) == tolower(species) & variable == 'Flow (mL/min/kg^(3/4))')[,'value']) * 60 / 1000 / parameters$BW^.25
        if(user.params) warning(paste(species,'liver flow used in calculating',tissue,'concentration.  Species of parameters should match species argument of input function.'))
      }
      if('Clint' %in% names(parameters)) parameters[['Clmetabolismc']] <- calc_hepatic_clearance(parameters=parameters,hepatic.model='unscaled',suppress.messages=T)
      else if(!'Clmetabolismc' %in% names(parameters)){ 
        if(is.null(chem.name) & is.null(chem.cas)) stop("Include Clmetabolismc (unscaled) in parameters or enter chem.name or chem.cas with appropriate species for desired tissue concentration.")
        parameters[['Clmetabolismc']] <- calc_hepatic_clearance(chem.cas=chem.cas,species=species,hepatic.model='unscaled',suppress.messages=T)
      }
      fub <- parameters[['Funbound.plasma']]
      Clmetabolism <- parameters$Clmetabolismc
      if(!restrictive.clearance) Clmetabolism <- Clmetabolism / fub
      Css <- parameters[['Kliver2pu']] * fub * (dose + Qliver * Css * parameters[['Rblood2plasma']]) / (Clmetabolism * fub + Qliver * parameters[['Rblood2plasma']])
      if(unit.change){
        Css <- Css / parameters$MW * 1000
        output.units <- 'uM'
      }
    }else if(tissue == 'kidney'){
      if(!all(c('Qcardiacc','Qkidneyf') %in% names(parameters))){
        Qkidney <- as.numeric(subset(tissue.data,Tissue == 'kidney' & tolower(Species) == tolower(species) & variable == 'Flow (mL/min/kg^(3/4))')[,'value']) * 60 / 1000 / parameters$BW^0.25 
        if(user.params) warning(paste(species,'kidney flow used in calculating',tissue,'concentration.  Species of parameters should match species argument of input function.'))
      }else Qkidney <- parameters$Qkidneyf * parameters$Qcardiacc / parameters$BW^0.25
      if(! 'Qgfrc' %in% names(parameters)){
        parameters[['Qgfrc']] <-  physiology.data[physiology.data[,'Parameter']=='GFR',paste0(toupper(substr(species,1,1)), tolower(substr(species,2,nchar(species))))] / 1000 * 60 / parameters$BW^0.25
        if(user.params) warning(paste(species,'glomerular filtration used in calculating',tissue,'concentration.  Species of parameters should match species argument of input function, or Qgfrc should be included with input parameters.'))
      }
      Css <- parameters[['Kkidney2pu']] * parameters[['Funbound.plasma']] * Qkidney * Css * parameters[['Rblood2plasma']] /(Qkidney * parameters[['Rblood2plasma']] + parameters$Qgfrc * parameters[['Funbound.plasma']])
    }else Css <- Css * parameters[[names(parameters)[substr(names(parameters),2,nchar(names(parameters))-3)==tissue]]] * parameters[['Funbound.plasma']]   
  }
    
  if(!suppress.messages){
    if(user.params){
      if((model == 'pbtk' | model == '3compartment') & !recalc.blood2plasma) warning('Rblood2plasma not recalculated.  Set recalc.blood2plasma to TRUE if desired.')
      if(is.null(tissue))cat(paste(toupper(substr(concentration,1,1)),substr(concentration,2,nchar(concentration)),sep=''),"concentration returned in",output.units,"units.\n")
      else cat(paste(toupper(substr(tissue,1,1)),substr(tissue,2,nchar(concentration)),sep=''),"concentration returned in",output.units,"units.\n")
    }else{
      if(is.null(tissue)) cat(paste(toupper(substr(species,1,1)),substr(species,2,nchar(species)),sep=''),concentration,"concentration returned in",output.units,"units.\n")
      else cat(paste(toupper(substr(species,1,1)),substr(species,2,nchar(species)),sep=''),tissue,"concentration returned in",output.units,"units.\n")
    }
  }    
       

 #  Css <- dose * Rblood2plasma * (Qliver + Qgut) * (Qkidney * Rblood2plasma  + Qgfr * fub)  / ((##(Rblood2plasma * Qkidney + Qgfr * fub) * (Qcardiac - Qrest) - Qkidney**2 * Rblood2plasma) * ((Qliver + Qgut) * Rblood2plasma +  Clmetabolism * fub * Kliver2plasma) - (Qliver + Qgut)**2 * (Qkidney *Rblood2plasma + Qgfr * fub)*Rblood2plasma)
  
    #Css <- dose/(QGFRc*fub+calc_Hepatic_Clearance(Params))
    return(as.numeric(Css))
}