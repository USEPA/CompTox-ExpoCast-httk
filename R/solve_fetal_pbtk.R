#' Solve_fetal_PBTK
#' 
#' This function solves for the amounts or concentrations in uM of a chemical
#' in different tissues as functions of time based on the dose and dosing
#' frequency.
#' 
#' Note that the model parameters have units of hours while the model output is
#' in days. Dose is in mg, not scaled for body weight.
#' 
#' Default NULL value for doses.per.day solves for a single dose.
#' 
#' The compartments used in this model are the gutlumen, gut, liver, kidneys,
#' veins, arteries, lungs, and the rest of the body.
#' 
#' The extra compartments include the amounts or concentrations metabolized by
#' the liver and excreted by the kidneys through the tubules.
#' 
#' AUC is the area under the curve of the plasma concentration.
#' 
#' When species is specified as rabbit, dog, or mouse, the function uses the
#' appropriate physiological data (volumes and flows) but substitues human
#' fraction unbound, partition coefficients, and intrinsic hepatic clearance.
#' 
#' @param chem.name Either the chemical name, CAS number, or the parameters
#' must be specified.
#' @param chem.cas Either the chemical name, CAS number, or the parameters must
#' be specified.
#' @param times Optional time sequence for specified number of days.  Dosing
#' sequence begins at the beginning of times.
#' @param parameters Chemical parameters from parameterize_pbtk function,
#' overrides chem.name and chem.cas.
#' @param days Length of the simulation.
#' @param tsteps The number time steps per hour.
#' @param daily.dose Total daily dose, mg.
#' @param dose Amount of a single dose, mg.  Overwrites daily.dose.
#' @param doses.per.day Number of doses per day.
#' @param initial.values Vector containing the initial concentrations or
#' amounts of the chemical in specified tissues with units corresponding to
#' output.units.  Defaults are zero.
#' @param plots Plots all outputs if true.
#' @param suppress.messages Whether or not the output message is suppressed.
#' @param species Species desired (either "Rat", "Rabbit", "Dog", "Mouse", or
#' default "Human").
#' @param iv.dose Simulates a single i.v. dose if true.
#' @param output.units Desired units (either "mg/L", "mg", "umol", or default
#' "uM").
#' @param method Method used by integrator (deSolve).
#' @param rtol Argument passed to integrator (deSolve).
#' @param atol Argument passed to integrator (deSolve).
#' @param default.to.human Substitutes missing animal values with human values
#' if true (hepatic intrinsic clearance or fraction of unbound plasma).
#' @param recalc.blood2plasma Recalculates the ratio of the amount of chemical
#' in the blood to plasma using the input parameters, calculated with
#' hematocrit, Funbound.plasma, and Krbc2pu.
#' @param recalc.clearance Recalculates the the hepatic clearance
#' (Clmetabolism) with new million.cells.per.gliver parameter.
#' @param dosing.matrix Vector of dosing times or a matrix consisting of two
#' columns or rows named "dose" and "time" containing the time and amount, in
#' mg/kg BW, of each dose.
#' @param adjusted.Funbound.plasma Uses adjusted Funbound.plasma when set to
#' TRUE along with partition coefficients calculated with this value.
#' @param regression Whether or not to use the regressions in calculating
#' partition coefficients.
#' @param begin.css Begin at steady state concentration in mother.
#' @param restrictive.clearance Protein binding not taken into account (set to
#' 1) in liver clearance if FALSE.
#' @param ... Additional arguments passed to the integrator.
#' @return A matrix of class deSolve with a column for time(in days), each
#' compartment, the area under the curve, and plasma concentration and a row
#' for each time point.
#' 
#' @author John Wambaugh and Robert Pearce
#' @keywords Solve
#' @examples
#' 
#' 
#' 
#' @export solve_fetal_pbtk
#' @useDynLib httk
#' @import deSolve
solve_fetal_pbtk <- function(chem.name = NULL,
                    chem.cas = NULL,
                    times=NULL,
                    parameters=NULL,
                    days=10,
                    tsteps = 4, # tsteps is number of steps per hour
                    daily.dose = 1,
                    dose = NULL, # Assume dose is mg, consistent with output units
                    doses.per.day=NULL,
                    initial.values=NULL,
                    plots=F,
                    suppress.messages=F,
                    species="Human",
                    iv.dose=F,
                    output.units='uM',
                    method="lsoda",rtol=1e-8,atol=1e-12,
                    begin.css=F,
                    default.to.human=F,
                    recalc.blood2plasma=F,
                    recalc.clearance=F,
                    dosing.matrix=NULL,
                    adjusted.Funbound.plasma=T,
                    regression=T,
                    restrictive.clearance = T,
                    ...)
{
  Aart <- Agut <- Agutlumen <- Alung <- Aliver <- Aven <- Arest <- Akidney <- Cgut <- Vgut <- Cliver <- Vliver <- Cven <- Vven <- Clung <- Vlung <- Cart <- Vart <- Crest <- Vrest <- Ckidney <- Vkidney <- NULL
  if(is.null(chem.cas) & is.null(chem.name) & is.null(parameters)) stop('Parameters, chem.name, or chem.cas must be specified.')
  if(is.null(parameters)){
    parameters <- parameterize_fetal_pbtk(chem.cas=chem.cas,chem.name=chem.name,species=species,default.to.human=default.to.human,suppress.messages=suppress.messages,
                                    adjusted.Funbound.plasma=adjusted.Funbound.plasma,regression=regression) 
  }else{
    name.list <- c("pre_pregnant_BW","Clmetabolismc","Funbound.plasma","Fgutabs","Fhep.assay.correction","hematocrit","Kgut2pu","kgutabs","Kkidney2pu","Kliver2pu","Klung2pu","Krbc2pu","Krest2pu","million.cells.per.gliver","MW","Qgfrc","Rblood2plasma","Vartc","Vgutc","Vkidneyc","Vliverc","Vlungc","Vvenc","Kfplacenta2pu","Kplacenta2pu","Vthyroidc","Kfthyroid2pu","Kthyroid2pu","Kfliver2pu","Kfbrain2pu","Kfkidney2pu","Kfrest2pu","Kfgut2pu","Kflung2pu","Vfgutc")
  if(!all(name.list %in% names(parameters)))stop(paste("Missing parameters:",paste(name.list[which(!name.list %in% names(parameters))],collapse=', '),".  Use parameters from parameterize_fetal_pbtk.")) 
  }
  if(is.null(times)) times <- round(seq(85,85 + days, 1/(24*tsteps)),8)
  start <- times[1]
  end <- times[length(times)]
  if(start < 85) stop('Cannot simulate before day 85.')

    if(iv.dose) parameters$Fgutabs <- 1
    if(is.null(dosing.matrix)){ 
      if(is.null(dose)){
        if(!is.null(doses.per.day)){
          dose <- daily.dose / doses.per.day * parameters$Fgutabs
        }else dose <- daily.dose * parameters$Fgutabs
      }else dose <- dose * parameters$Fgutabs
    }else{
      if(!is.null(dim(dosing.matrix))){
        rc <- which(dim(dosing.matrix) == 2)
        if(rc == 1){
          if(is.null(rownames(dosing.matrix)) | any(!(rownames(dosing.matrix) %in% c('time','dose')))) stop('dosing.matrix must have column or row names of \"time\" and \"dose\" or be a vector of times.')
          dosing.times <- dosing.matrix['time',]
          dose.vector <- dosing.matrix['dose',] * parameters$Fgutabs
        }else{
          if(is.null(colnames(dosing.matrix)) | any(!(colnames(dosing.matrix) %in% c('time','dose')))) stop('dosing.matrix must have column or row names of \"time\" and \"dose\" or be a vector of times.')
          dosing.times <- dosing.matrix[,'time']
          dose.vector <- dosing.matrix[,'dose'] * parameters$Fgutabs   
        }
      }else{
        if(is.null(dose)) stop("Argument dose must be entered to overwrite daily.dose when a time vector is entered into dosing.matrix.")
        dosing.times <- dosing.matrix
        dose.vector <- rep(dose * parameters$Fgutabs,length(dosing.matrix))
      } 
      if(start == dosing.times[1]){
        dose <- dose.vector[[1]]
        dosing.times <- dosing.times[2:length(dosing.times)]
        dose.vector <- dose.vector[2:length(dose.vector)]
      }else dose <- 0  
    } 

  lastchar <- function(x){substr(x, nchar(x), nchar(x))}
  firstchar <- function(x){substr(x, 1,1)}

  #Rblood2plasma <- parameters[["Rblood2plasma"]]
  #hematocrit <- parameters[["hematocrit"]]
   #dose converted to umoles/day  from  mg/kg BW/day 
   
     if(tolower(output.units)=='um' |  tolower(output.units) == 'mg/l') use.amounts <- F
     if(tolower(output.units)=='umol' |  tolower(output.units) == 'mg') use.amounts <- T
  
     if(tolower(output.units)=='um' | tolower(output.units) == 'umol'){
       dose <- as.numeric(dose / 1000 / parameters[["MW"]] * 1000000)
       if(!is.null(dosing.matrix)) dose.vector <- as.numeric(dose.vector  / 1000 / parameters[["MW"]] * 1000000)
     }else if(!(tolower(output.units) == 'mg/l' | tolower(output.units) == 'mg')) stop('Output.units can only be uM, umol, mg, or mg/L.')
   
  


  fBW <- 0.00003107 * exp(0.8137/0.06458 * (1 - exp(-0.06458 * start / 7)))
BW <- parameters$pre_pregnant_BW + 0.006514 * exp(0.6797/0.08698 * (1 - exp(-0.08698 * start/7)))
Vplacenta <- 0.317 * fBW^0.582
Vfart <- 0.16 * 80/1000 * fBW 
Vfven <- 0.595 * 80/1000 *  fBW
Vfkidney <- 0.01082 * exp(0.4673/0.052 * (1 - exp(-0.052 *  start/7)))/1000
Vfthyroid <- 0.002091 * exp(0.3137/0.0367 * (1 - exp(-0.0367 *  start/7)))/1000
Vfliver <- 0.1528 * exp(0.3478/0.04058 * (1 - exp(-0.04058 *  start/7)))/1000
Vfbrain <- 0.2912 * exp(0.4244/0.05078 * (1 - exp(-0.05078 *  start/7)))/1000
Vflung <- 0.02611 * exp(0.5898/0.07125 * (1 - exp(-0.07125 *  start/7)))/1000
  scaled.volumes <- names(parameters)[firstchar(names(parameters))=="V"&lastchar(names(parameters))=="c"]
        
  for (this.vol in scaled.volumes)
  {
      if(substr(this.vol,2,2) == 'f'){
        eval(parse(text=paste(substr(this.vol,1,nchar(this.vol)-1), '<-', parameters[[this.vol]],'*', fBW)))
      }else eval(parse(text=paste(substr(this.vol,1,nchar(this.vol)-1), '<-', parameters[[this.vol]],'*', parameters[["pre_pregnant_BW"]]))) # L 
  }
  Vfrest <- fBW - (Vfart + Vfven + Vfkidney + Vfthyroid + Vfliver + Vfbrain + Vfgut + Vflung)
  Vrest <- BW - (fBW + Vplacenta) - (Vthyroid + Vkidney + Vgut + Vliver + Vlung + Vart + Vven)
  
  if (use.amounts)
  {
    CompartmentsToInitialize <-c("Agutlumen","Aart","Aven","Alung","Agut","Aliver","Akidney","Arest","Athyroid","Aplacenta","Afliver","Afven","Afart","Afrest","Afthyroid","Afkidney",'Aflung','Afgut','Afbrain')
  } else {
    CompartmentsToInitialize <-c("Agutlumen","Cart","Cven","Clung","Cgut","Cliver","Ckidney","Crest","Cthyroid","Cplacenta","Cfliver","Cfven","Cfart","Cfrest","Cfthyroid","Cfkidney",'Cflung','Cfgut','Cfbrain')
  }

  
  if(begin.css){
    if(is.null(chem.cas) & is.null(chem.name)) stop("Must enter chem.cas or chem.name to begin at steady state.")
    parms <- c(parameterize_pbtk(chem.name=chem.name,chem.cas=chem.cas,species=species)[c('Qcardiacc','Qgutf','Qkidneyf','Qliverf','Vrestc')],parameters[c("Clmetabolismc","Funbound.plasma","Fgutabs","Fhep.assay.correction","hematocrit","Kgut2pu","kgutabs","Kkidney2pu","Kliver2pu","Klung2pu","Krbc2pu","Krest2pu","million.cells.per.gliver","MW" ,"Qgfrc","Rblood2plasma","Vartc","Vgutc","Vkidneyc","Vliverc","Vlungc","Vvenc")])
    parms$BW <- parameters[['pre_pregnant_BW']]
    daily.dose <- daily.dose / parms$BW
    css.days <- calc_css(parameters=parms,daily.dose=daily.dose,doses.per.day=doses.per.day,suppress.messages=T)[['the.day']]
    out <- solve_pbtk(parameters=parms, daily.dose=daily.dose,doses.per.day=doses.per.day, days = css.days,tsteps=tsteps,output.units=output.units,suppress.messages=T,...)
    if(use.amounts){
      initial.values[c("Agutlumen","Aart","Aven","Alung","Agut","Aliver","Akidney","Arest")] <- out[dim(out)[1],c("Agutlumen","Aart","Aven","Alung","Agut","Aliver","Akidney","Arest")]
    }else  initial.values[c("Agutlumen","Cart","Cven","Clung","Cgut","Cliver","Ckidney","Crest")] <- out[dim(out)[1],c("Agutlumen","Cart","Cven","Clung","Cgut","Cliver","Ckidney","Crest")]
  }
  
  for (this.compartment in CompartmentsToInitialize){
  # If the compartment has a value specified in the list initial.values, then set it to that value:
    if (this.compartment %in% names(initial.values))
    {
      eval(parse(text=paste(this.compartment,"<-",initial.values[[this.compartment]]))) 
    }
  # Otherwise set the value to zero:
    else eval(parse(text=paste(this.compartment,"<- 0")))
  }

  if (use.amounts) 
  {
    if(iv.dose){
   # if('Aven' %in% names(initial.values)) Aserum <- Aven / Ratioblood2plasma * (1 - hematocrit)
    #if('Aserum' %in% names(initial.values)) Aven <- Aserum * Ratioblood2plasma / (1 - hematocrit) 
      state <- c(Aart = Aart,Agut = Agut,Agutlumen = Agutlumen,Alung = Alung,Aliver = Aliver,
               Aven = Aven + dose,Arest = Arest,Akidney = Akidney,Athyroid=Athyroid,Aplacenta=Aplacenta,Afliver=Afliver,Afven=Afven,Afart=Afart,Afrest=Afrest,Afthyroid=Afthyroid,Afkidney=Afkidney,Afbrain=Afbrain,Aflung=Aflung,Afgut=Afgut,Atubules = 0,Ametabolized = 0,AUC=0,fAUC=0)
    }else{
      state <- c(Aart = Aart,Agut = Agut,Agutlumen = Agutlumen + dose,Alung = Alung,Aliver = Aliver,
               Aven = Aven,Arest = Arest,Akidney = Akidney,Athyroid=Athyroid,Aplacenta=Aplacenta,Afliver=Afliver,Afven=Afven,Afart=Afart,Afrest=Afrest,Afthyroid=Afthyroid,Afkidney=Afkidney,Afbrain=Afbrain,Aflung=Aflung,Afgut=Afgut,Atubules = 0,Ametabolized = 0,AUC=0,fAUC=0)
    }
  }else{
    if(iv.dose){
      state <- c(Agutlumen = Agutlumen,Agut = Cgut * Vgut,Aliver = Cliver * Vliver,Aven = Cven * Vven + dose,Alung = Clung * Vlung,Aart = Cart * Vart,Arest = Crest * Vrest,Akidney = Ckidney * Vkidney,Athyroid=Cthyroid * Vthyroid,Aplacenta=Cplacenta*parameters$Vplacenta,Afliver=Cfliver*Vfliver,Afven=Cfven*Vfven,Afart=Cfart*Vfart,Afrest=Cfrest*Vfrest,Afthyroid=Cfthyroid*Vfthyroid,Afkidney=Cfkidney*Vfkidney,Afbrain=Cfbrain * Vfbrain,Aflung=Cflung * Vflung,Afgut= Cfgut * Vfgut,Atubules = 0,Ametabolized = 0,AUC=0,fAUC=0)
    }else{
    #if('Cven' %in% names(initial.values)) Cserum <- Cven / Ratioblood2plasma
    #if('Cserum' %in% names(initial.values)) Cven <- Cserum * Ratioblood2plasma
      state <- c(Agutlumen = Agutlumen + dose,Agut = Cgut * Vgut,Aliver = Cliver * Vliver,Aven = Cven * Vven,Alung = Clung * Vlung,Aart = Cart * Vart,Arest = Crest * Vrest,Akidney = Ckidney * Vkidney,Athyroid=Cthyroid * Vthyroid,Aplacenta=Cplacenta* parameters$Vplacenta,Afliver=Cfliver*Vfliver,Afven=Cfven*Vfven,Afart=Cfart*Vfart,Afrest=Cfrest*Vfrest,Afthyroid=Cfthyroid*Vfthyroid,Afkidney=Cfkidney*Vfkidney,Afbrain=Cfbrain * Vfbrain,Aflung=Cflung * Vflung,Afgut= Cfgut * Vfgut,Atubules = 0,Ametabolized = 0,AUC=0,fAUC=0)
    }
  }       
  
  if(recalc.blood2plasma) parameters[['Rblood2plasma']] <- 1 - parameters[['hematocrit']] + parameters[['hematocrit']] * parameters[['Krbc2pu']] * parameters[['Funbound.plasma']]
  
  if(recalc.clearance){
    if(is.null(chem.name) & is.null(chem.cas)) stop('Chemical name or CAS must be specified to recalculate hepatic clearance.')
    ss.params <- parameterize_steadystate(chem.name=chem.name,chem.cas=chem.cas)
    ss.params[['million.cells.per.gliver']] <- parameters[['million.cells.per.gliver']]
    parameters[['Clmetabolismc']] <- calc_hepatic_clearance(parameters=ss.params,hepatic.model='unscaled',suppress.messages=T)
  } 
  if(!restrictive.clearance) parameters$Clmetabolismc <- parameters$Clmetabolismc / parameters$Funbound.plasma
  
  #Parameter bridging between R and .C solver file naming conventions
  parameters[['Fraction_unbound_plasma']] <- parameters[['Funbound.plasma']]
  parameters[['Ratioblood2plasma']] <- parameters[['Rblood2plasma']]
  parameters <- initParmsfetalpbtk(parameters[param.names.fetal.pbtk.solver])

  
  state <-initStatesfetalpbtk(parameters,state)
  
   
     

  if(is.null(dosing.matrix)){
    if(is.null(doses.per.day)){
      out <- ode(y = state, times = times,func="derivsfetalpbtk", parms=parameters, method=method,rtol=rtol,atol=atol,dllname="httk",initfunc="initmodfetalpbtk", nout=length(Outputsfetus),outnames=Outputsfetus,...)
    }else{
      dosing <- seq(start + 1/doses.per.day,end-1/doses.per.day,1/doses.per.day)
      length <- length(dosing)
      if(iv.dose) eventdata <- data.frame(var=rep('Aven',length),time = round(dosing,8),value = rep(dose,length), method = rep("add",length))
      else eventdata <- data.frame(var=rep('Agutlumen',length),time = round(dosing,8),value = rep(dose,length), method = rep("add",length))
      times <- sort(c(times,dosing + 1e-8,start + 1e-8))
      out <- ode(y = state, times = times, func="derivsfetalpbtk", parms = parameters,method=method,rtol=rtol,atol=atol, dllname="httk",initfunc="initmodfetalpbtk", nout=length(Outputsfetus),outnames=Outputsfetus,events=list(data=eventdata),...)
    }      
  }else{
    if(iv.dose) eventdata <- data.frame(var=rep('Aven',length(dosing.times)),time = dosing.times,value = dose.vector, method = rep("add",length(dosing.times)))                          
    else eventdata <- data.frame(var=rep('Agutlumen',length(dosing.times)),time = dosing.times,value = dose.vector, method = rep("add",length(dosing.times)))
    times <- sort(c(times,dosing.times + 1e-8,start + 1e-8))
    out <- ode(y = state, times = times, func="derivsfetalpbtk", parms = parameters,method=method,rtol=rtol,atol=atol, dllname="httk",initfunc="initmodfetalpbtk", nout=length(Outputsfetus),outnames=Outputsfetus,events=list(data=eventdata),...)                                
  }
  
  colnames(out)[[which(colnames(out)=='Aserum')]] <- 'Aplasma'
  colnames(out)[[which(colnames(out)=='Cserum')]] <- 'Cplasma'
  colnames(out)[[which(colnames(out)=='Afserum')]] <- 'Afplasma'
  colnames(out)[[which(colnames(out)=='Cfserum')]] <- 'Cfplasma'
  
  if(plots==T)
  {
    if(use.amounts){
      plot(out,select=c(CompartmentsToInitialize,"Ametabolized","Atubules","Aplasma","Afplasma","AUC","fAUC"))
    }else{
      plot(out,select=c(CompartmentsToInitialize,"Ametabolized","Atubules","Cplasma","Cfplasma","AUC","fAUC"))
    }
    
  }
    if(use.amounts){
      out <- out[,c("time",CompartmentsToInitialize,"Ametabolized","Atubules","Aplasma","Afplasma","AUC","fAUC")]
    }else{
      out <- out[,c("time",CompartmentsToInitialize,"Ametabolized","Atubules","Cplasma","Cfplasma","AUC","fAUC")]
    }
  class(out) <- c('matrix','deSolve')
  
  if(!suppress.messages){
    if(is.null(chem.cas) & is.null(chem.name)){
      cat("Values returned in",output.units,"units.\n")
      if(!recalc.blood2plasma) warning('Rblood2plasma not recalculated.  Set recalc.blood2plasma to TRUE if desired.') 
      if(!recalc.clearance) warning('Clearance not recalculated.  Set recalc.clearance to TRUE if desired.') 
    }else cat(paste(toupper(substr(species,1,1)),substr(species,2,nchar(species)),sep=''),"values returned in",output.units,"units.\n")
    if(tolower(output.units) == 'mg'){
      cat("AUC is area under plasma concentration in mg/L * days units with Rblood2plasma =",parameters[['Ratioblood2plasma']],".\n")
    }else if(tolower(output.units) == 'umol'){
      cat("AUC is area under plasma concentration in uM * days units with Rblood2plasma =",parameters[['Ratioblood2plasma']],".\n")
    }else cat("AUC is area under plasma concentration curve in",output.units,"* days units with Rblood2plasma =",parameters[['Ratioblood2plasma']],".\n")
  }
    
  return(out) 
}
