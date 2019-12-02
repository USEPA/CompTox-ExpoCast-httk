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
                               restrictive.clearance=T,...)
{
  tissue.data <- tissue.data
  physiology.data <- physiology.data
  Tissue <- Species <- variable <- NULL
  
  if(is.null(chem.cas) & is.null(chem.name) & is.null(parameters)) stop('Must specify chem.cas, chem.name, or parameters.')
  
  dose <- daily.dose
  good.units <- c("uM","mg/L")
  if (!(tolower(output.units) %in% tolower(good.units))) stop(paste("Do not know how to calculate units",output.units,". Please select from: ",paste(good.units,collapse=", ")))
  dose <- dose / 24 
  if(is.null(parameters) | !is.null(tissue)){
    if(is.null(chem.cas)){
      out <- get_chem_id(chem.name=chem.name)
      chem.cas <- out$chem.cas
    }
    MW <- get_physchem_param('MW',chem.cas=chem.cas)
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
     fup <- parameters[["Funbound.plasma"]]
     if(!restrictive.clearance) Clmetabolism <- Clmetabolism / fup

     dose <- dose * parameters$Fgutabs
     
     Css <- (dose * (Qliver + Qgut) / (fup * Clmetabolism / Rblood2plasma + (Qliver + Qgut))) / (Qcardiac - (Qliver + Qgut)**2 /(fup * Clmetabolism / Rblood2plasma + (Qliver + Qgut)) - Qkidney**2 / (Qgfr * fup / Rblood2plasma + Qkidney) - Qrest)
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
    fup <- parameters$Funbound.plasma
    Clmetabolism <- parameters$Clmetabolismc
    if(!restrictive.clearance) Clmetabolism <- Clmetabolism / fup
    Css <- dose * parameters[['BW']]^0.25  / (Clmetabolism * parameters[['BW']]^0.25  + parameters$Qgfrc * (parameters$Qliverf + parameters$Qgutf) * parameters$Qcardiacc / ((parameters$Qliverf + parameters$Qgutf) * parameters$Qcardiacc + fup * parameters$Qgfrc / parameters$Rblood2plasma)) / fup
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
      fup <- parameters[['Funbound.plasma']]
      Clmetabolism <- parameters$Clmetabolismc
      if(!restrictive.clearance) Clmetabolism <- Clmetabolism / fup
      Css <- parameters[['Kliver2pu']] * fup * (dose + Qliver * Css * parameters[['Rblood2plasma']]) / (Clmetabolism * fup + Qliver * parameters[['Rblood2plasma']])
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
       

 #  Css <- dose * Rblood2plasma * (Qliver + Qgut) * (Qkidney * Rblood2plasma  + Qgfr * fup)  / ((##(Rblood2plasma * Qkidney + Qgfr * fup) * (Qcardiac - Qrest) - Qkidney**2 * Rblood2plasma) * ((Qliver + Qgut) * Rblood2plasma +  Clmetabolism * fup * Kliver2plasma) - (Qliver + Qgut)**2 * (Qkidney *Rblood2plasma + Qgfr * fup)*Rblood2plasma)
  
    #Css <- dose/(QGFRc*fup+calc_Hepatic_Clearance(Params))
    return(as.numeric(Css))
}
