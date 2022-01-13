#' Solve_dermal_PBTK
#' 
#' This function solves for the amounts or concentrations of a chemical in
#' different tissues as functions of time based on the dose and dosing
#' frequency. 
#' 
#' Model units are the same as media concentration, units/L or units when
#' use.amounts=T.
#' 
#' New doses replace rather than add to previous doses. A concentration of 0 in
#' dosing.matrix switches off the dosing/diffusion between the media and
#' exposed skin.
#' 
#' Note that the model parameters have units of hours while the model output is
#' in days.
#' 
#' Default NULL value for doses.per.day solves for a single dose.
#' 
#' The compartments used in this model are the gutlumen, gut, liver, kidneys,
#' veins, arteries, lungs, skin, exposed skin, media, and the rest of the body.
#' 
#' The extra compartments include the amounts or concentrations metabolized by
#' the liver and excreted by the kidneys through the tubules.
#' 
#' AUC is the area under the curve of the plasma concentration.
#' 
#' When species is specified as rabbit, dog, or mouse, the function uses the
#' appropriate physiological data(volumes and flows) but substitues human
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
#' @param concentration Concentration of media at dosing times, units/L.
#' @param doses.per.day Number of doses per day.
#' @param initial.values Vector containing the initial concentrations or
#' amounts of the chemical in specified tissues with units corresponding to
#' output.units.  Defaults are zero.
#' @param plots Plots all outputs if true.
#' @param suppress.messages Whether or not the output message is suppressed.
#' @param species Species desired (either "Rat", "Rabbit", "Dog", "Mouse", or
#' default "Human").
#' @param use.amounts Return outputs as amounts instead of concentrations.
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
#' @param dosing.matrix Matrix consisting of three columns named
#' "concentration", "Vmedia", and "time" containing the dosing times, days,
#' with the applied concentration, units/L, and the volume of the applied
#' media, L.
#' @param adjusted.Funbound.plasma Uses adjusted Funbound.plasma when set to
#' TRUE along with partition coefficients calculated with this value.
#' @param regression Whether or not to use the regressions in calculating
#' partition coefficients.
#' @param restrictive.clearance Protein binding not taken into account (set to
#' 1) in liver clearance if FALSE.
#' @param depth depth of skin, cm, used in calculating Kp.
#' @param skin.pH pH of dermis/skin, used in calculating Kp and Kskin2media.
#' @param vmax.km Whether or not to use Michaelis-Menten kinetics
#' @param ... Additional arguments passed to the integrator.
#' @return A matrix of class deSolve with a column for time(in days), each
#' compartment, the area under the curve, and plasma concentration and a row
#' for each time point.
#' 
#' @author John Wambaugh and Robert Pearce
#' @keywords Solve
#' @examples
#' 
#' Vmedia <- c(.1,.2,.3)
#' time <- c(0,.5,3)
#' concentration <- c(2,0,3)
#' dosing.matrix <- cbind(time,concentration,Vmedia)
#' out <- solve_dermal_pbtk(chem.name='bisphenola',dosing.matrix=dosing.matrix,plots=T)
#' 
#' parameters <- parameterize_dermal_pbtk(chem.name='bisphenola',depth=1)
#' parameters$Fskinexposed <- 0.25
#' parameters$Kp <- 2
#' parameters$Fdermabs <- 0.5
#' parameters$Vmedia <- 1
#' out <- solve_dermal_pbtk(parameters=parameters,concentration=100,plots=T)
#' 
#' @export solve_dermal_pbtk
#' @useDynLib httk
#' @import deSolve
solve_dermal_pbtk <- function(chem.name = NULL,
                    chem.cas = NULL,
                    times=NULL,
                    parameters=NULL,
                    days=10,
                    tsteps = 4, # tsteps is number of steps per hour
                    concentration = 1, 
                    doses.per.day=NULL,
                    initial.values=NULL,
                    plots=F,
                    suppress.messages=F,
                    species="Human",
                    use.amounts=F,
                 #   output.units='uM',
                    method="lsoda",rtol=1e-8,atol=1e-12,
                    default.to.human=F,
                    recalc.blood2plasma=F,
                    recalc.clearance=F,
                    dosing.matrix=NULL,
                    adjusted.Funbound.plasma=T,
                    regression=T,
                    restrictive.clearance = T,
                    depth=0.3,skin.pH=7,
                    vmax.km=F,...)
{
  Aart <- Agut <- Agutlumen <- Alung <- Aliver <- Aven <- Arest <- Akidney <- Cgut <- Vgut <- Cliver <- Vliver <- Cven <- Vven <- Clung <- Vlung <- Cart <- Vart <- Crest <- Vrest <- Ckidney <- Vkidney <- NULL
  if(is.null(chem.cas) & is.null(chem.name) & is.null(parameters)) stop('Parameters, chem.name, or chem.cas must be specified.')
  if(is.null(parameters)){
    parameters <- parameterize_dermal_pbtk(chem.cas=chem.cas,chem.name=chem.name,species=species,default.to.human=default.to.human,suppress.messages=suppress.messages,
                                    adjusted.Funbound.plasma=adjusted.Funbound.plasma,regression=regression,depth=depth,skin.pH=skin.pH,vmax.km=vmax.km) 
  }else{
    #name.list <- c("BW","Clmetabolismc","Funbound.plasma","Fgutabs","Fhep.assay.correction","hematocrit","Kgut2pu","kgutabs","Kkidney2pu","Kliver2pu","Klung2pu","Krbc2pu","Krest2pu","million.cells.per.gliver","MW","Qcardiacc" ,"Qgfrc","Qgutf","Qkidneyf","Qliverf","Rblood2plasma","Vartc","Vgutc","Vkidneyc","Vliverc","Vlungc","Vrestc","Vvenc")
  #if(!all(name.list %in% names(parameters)))stop(paste("Missing parameters:",paste(name.list[which(!name.list %in% names(parameters))],collapse=', '),".  Use parameters from parameterize_dermal_pbtk.")) 
  if(parameters$Vskinc * parameters$BW - parameters$totalSA * parameters$depth * 0.001 * parameters$Fskinexposed < 0) stop('Exposed volume of skin is larger than skin volume.')
  }
  if(is.null(times)) times <- round(seq(0, days, 1/(24*tsteps)),8)
  start <- times[1]
  end <- times[length(times)]          
    
    
    if(is.null(dosing.matrix)){ 
      dose <- concentration * parameters$Fdermabs * parameters$Vmedia #concentration in units/L ---was in mg/L
    }else{
      if(any(!(colnames(dosing.matrix) %in% c('time','concentration','Vmedia')))) stop('dosing.matrix must have column names of \"time\", \"Vmedia\", and \"concentration\" or be a vector of times.')
      dosing.times <- dosing.matrix[,'time']
      dose.vector <- dosing.matrix[,'concentration'] * dosing.matrix[,'Vmedia'] * parameters$Fdermabs
      Vmedia <- dosing.matrix[,'Vmedia']
      switch <- dosing.matrix[,c('time','concentration')]
      switch[which(switch[,'concentration'] != 0),'concentration'] <- 1
      colnames(switch) <- c('times','y')
      V0 <- Vmedia[1] 
      if(start == dosing.times[1]){
        dose <- dose.vector[1]
        dosing.times <- dosing.times[-1]
        dose.vector <- dose.vector[-1]
        FVmedia <- Vmedia[-1] / V0 
      }else{
        dose <- 0 
        switch <- rbind(c(0,1),switch)
        FVmedia <- Vmedia / V0
      }
    } 

  lastchar <- function(x){substr(x, nchar(x), nchar(x))}
  firstchar <- function(x){substr(x, 1,1)}

   #dose converted to umoles/day  from  mg/kg BW/day                     
     #if(tolower(output.units)=='um' |  tolower(output.units) == 'mg/l') use.amounts <- F
     #if(tolower(output.units)=='umol' |  tolower(output.units) == 'mg') use.amounts <- T
     
  
   #  if(tolower(output.units)=='um' | tolower(output.units) == 'umol'){
    #   dose <- as.numeric(dose / 1000 / parameters[["MW"]] * 1000000)
     #  if(!is.null(dosing.matrix)) dose.vector <- as.numeric(dose.vector / 1000 / parameters[["MW"]] * 1000000)
 #    }else if(tolower(output.units) == 'mg/l' | tolower(output.units) == 'mg'){
  #     dose <- dose * bw.multiplier
   #    if(!is.null(dosing.matrix)) dose.vector <-  dose.vector * bw.multiplier
    # }else stop('Output.units can only be uM, umol, mg, or mg/L.')
   
  
parameters[["MW"]] <- NULL

  
  scaled.volumes <- names(parameters)[firstchar(names(parameters))=="V"&lastchar(names(parameters))=="c"]
        
  for (this.vol in scaled.volumes)
  {
      eval(parse(text=paste(substr(this.vol,1,nchar(this.vol)-1), '<-', parameters[[this.vol]],'*', parameters[["BW"]]))) # L 
  }
  Vskinexposed <- parameters$Fskinexposed * parameters$totalSA * parameters$depth * 0.001  
  
  if (use.amounts) CompartmentsToInitialize <- c("Agutlumen","Aart","Aven","Alung","Agut","Aliver","Akidney","Arest","Askin","Askinexposed","Amedia")
  else CompartmentsToInitialize <- c("Agutlumen","Cart","Cven","Clung","Cgut","Cliver","Ckidney","Crest","Cskin","Cskinexposed","Cmedia")

  for (this.compartment in CompartmentsToInitialize)
  {
  # If the compartment has a value specified in the list initial.values, then set it to that value:
    if (this.compartment %in% names(initial.values))
    {
      eval(parse(text=paste(this.compartment,"<-",initial.values[[this.compartment]])))
      
    }
  # Otherwise set the value to zero:
    else eval(parse(text=paste(this.compartment,"<- 0")))
  }
  

   if (use.amounts){ 
     state <- c(Aart = Aart,Agut = Agut,Agutlumen = Agutlumen,Alung = Alung,Aliver = Aliver,
               Aven = Aven,Arest = Arest,Akidney = Akidney,Askin=Askin,Askinexposed = Askinexposed, Amedia = dose,Atubules = 0,Ametabolized = 0,AUC=0)#,Vmedia=V0)
  }else{
    state <- c(Agutlumen = Agutlumen,Agut = Cgut * Vgut,Aliver = Cliver * Vliver,Aven = Cven * Vven,Alung = Clung * Vlung,Aart = Cart * Vart,Arest = Crest * Vrest,Akidney = Ckidney * Vkidney,Askin = Cskin * Vskin, Askinexposed = Cskinexposed * Vskinexposed, Amedia = dose,Atubules = 0,Ametabolized = 0,AUC=0)#,Vmedia=V0)
  }    
  
  if(recalc.blood2plasma) parameters[['Rblood2plasma']] <- 1 - parameters[['hematocrit']] + parameters[['hematocrit']] * parameters[['Krbc2pu']] * parameters[['Funbound.plasma']]
  
  if(recalc.clearance & !vmax.km){
    if(is.null(chem.name) & is.null(chem.cas)) stop('Chemical name or CAS must be specified to recalculate hepatic clearance.')
    ss.params <- parameterize_steadystate(chem.name=chem.name,chem.cas=chem.cas)
    ss.params[['million.cells.per.gliver']] <- parameters[['million.cells.per.gliver']]
    parameters[['Clmetabolismc']] <- calc_hepatic_clearance(parameters=ss.params,hepatic.model='unscaled',suppress.messages=T)
  } 
  if(!restrictive.clearance & !vmax.km) parameters$Clmetabolismc <- parameters$Clmetabolismc / parameters$Funbound.plasma
 
 
    parameters[['Fraction_unbound_plasma']] <- parameters[['Funbound.plasma']]
    if(is.null(dosing.matrix)) parameters$V0 <- parameters$Vmedia
    else parameters$V0 <- V0
  if(vmax.km){
    parameters <- initparms_skin_mm(parameters[!(names(parameters) %in% c("Fhep.assay.correction","million.cells.per.gliver","Fgutabs","Funbound.plasma","Krbc2pu","Fdermabs","Vmedia"))]) 
    state <-initState_skin_mm(parameters,state)
  
    if(is.null(dosing.matrix)){
      if(is.null(doses.per.day)){
        Forc <- list(cbind(times=0,y=1),cbind(times=0,y=1))
        out <- ode(y = state, times = times,func="derivs_skin_mm", parms=parameters, method=method,rtol=rtol,atol=atol,dllname="httk",initfunc="initmod_skin_mm", nout=length(Outputs_skin_mm),outnames=Outputs_skin_mm,initforc= "initforc_skin_mm",forcing=Forc,fcontrol=list(method='constant',rule=2,f=0),...)
      }else{
        dosing <- seq(start + 1/doses.per.day,end-1/doses.per.day,1/doses.per.day)
        length <- length(dosing)
        eventdata <- data.frame(var=rep('Amedia',length),time = round(dosing,8),value = rep(dose,length), method = rep("replace",length))
        times <- sort(c(times,dosing + 1e-8,1e-8))
         Forc <- list(cbind(times=0,y=1),cbind(times=0,y=1))
        out <- ode(y = state, times = times, func="derivs_skin_mm", parms = parameters,method=method,rtol=rtol,atol=atol, dllname="httk",initfunc="initmod_skin_mm", nout=length(Outputs_skin_mm),outnames=Outputs_skin_mm,events=list(data=eventdata),initforc= "initforc_skin_mm",forcing=Forc,fcontrol=list(method='constant',rule=2,f=0),...)
      }      
    }else{
      eventdata <- data.frame(var=rep('Amedia',length(dosing.times)),time = dosing.times,value = dose.vector, method = rep("replace",length(dosing.times)))
      times <- sort(c(times,dosing.times + 1e-8,1e-8))
      Forc <- list(forcing=rbind(c(0,1),cbind(times=dosing.times,y=FVmedia)),switch=switch)
      out <- ode(y = state, times = times, func="derivs_skin_mm", parms = parameters,method=method,rtol=rtol,atol=atol, dllname="httk",initfunc="initmod_skin_mm", nout=length(Outputs_skin_mm),outnames=Outputs_skin_mm,events=list(data=eventdata),initforc= "initforc_skin_mm",forcing=Forc,fcontrol=list(method='constant',rule=2,f=0),...)                                
    }
  }else{
    parameters <- initparms_skin(parameters[!(names(parameters) %in% c("Fhep.assay.correction","million.cells.per.gliver","Fgutabs","Funbound.plasma","Krbc2pu","Fdermabs","Vmedia"))]) 
    state <-initState_skin(parameters,state)
  
    if(is.null(dosing.matrix)){
      if(is.null(doses.per.day)){
        Forc <- list(cbind(times=0,y=1),cbind(times=0,y=1))
        out <- ode(y = state, times = times,func="derivs_skin", parms=parameters, method=method,rtol=rtol,atol=atol,dllname="httk",initfunc="initmod_skin", nout=length(Outputs_skin),outnames=Outputs_skin,initforc= "initforc_skin",forcing=Forc,fcontrol=list(method='constant',rule=2,f=0),...)
      }else{
        dosing <- seq(start + 1/doses.per.day,end-1/doses.per.day,1/doses.per.day)
        length <- length(dosing)
        eventdata <- data.frame(var=rep('Amedia',length),time = round(dosing,8),value = rep(dose,length), method = rep("replace",length))
        times <- sort(c(times,dosing + 1e-8,1e-8))
         Forc <- list(cbind(times=0,y=1),cbind(times=0,y=1))
        out <- ode(y = state, times = times, func="derivs_skin", parms = parameters,method=method,rtol=rtol,atol=atol, dllname="httk",initfunc="initmod_skin", nout=length(Outputs_skin),outnames=Outputs_skin,events=list(data=eventdata),initforc= "initforc_skin",forcing=Forc,fcontrol=list(method='constant',rule=2,f=0),...)
      }      
    }else{
      eventdata <- data.frame(var=rep('Amedia',length(dosing.times)),time = dosing.times,value = dose.vector, method = rep("replace",length(dosing.times)))
      times <- sort(c(times,dosing.times + 1e-8,1e-8))
      Forc <- list(forcing=rbind(c(0,1),cbind(times=dosing.times,y=FVmedia)),switch=switch)
      out <- ode(y = state, times = times, func="derivs_skin", parms = parameters,method=method,rtol=rtol,atol=atol, dllname="httk",initfunc="initmod_skin", nout=length(Outputs_skin),outnames=Outputs_skin,events=list(data=eventdata),initforc= "initforc_skin",forcing=Forc,fcontrol=list(method='constant',rule=2,f=0),...)                                
    }
  }
  
  if(plots==T)
  {
    if(use.amounts){
      plot(out,select=c(CompartmentsToInitialize,"Ametabolized","Atubules","Aplasma","AUC"))
    }else{
      plot(out,select=c(CompartmentsToInitialize,"Ametabolized","Atubules","Cplasma","AUC"))
    }
    
  }
    if(use.amounts){
      out <- out[,c("time",CompartmentsToInitialize,"Ametabolized","Atubules","Aplasma","AUC")]
    }else{
      out <- out[,c("time",CompartmentsToInitialize,"Ametabolized","Atubules","Cplasma","AUC")]
    }
  class(out) <- c('matrix','deSolve')
  
  if(!suppress.messages){
  #  if(is.null(chem.cas) & is.null(chem.name)){
      if(use.amounts) cat("Values returned in amounts matching units from media concentration, units/L.\n")
      else cat("Values returned in units/L, same as media concentration.\n")
      if(!recalc.blood2plasma) warning('Rblood2plasma not recalculated.  Set recalc.blood2plasma to TRUE if desired.') 
      if(!recalc.clearance) warning('Clearance not recalculated.  Set recalc.clearance to TRUE if desired.') 
   # }else cat(paste(toupper(substr(species,1,1)),substr(species,2,nchar(species)),sep=''),"values returned in",output.units,"units.\n")
#    if(tolower(output.units) == 'mg'){
#      cat("AUC is area under plasma concentration in mg/L * days units with Rblood2plasma =",parameters[['Rblood2plasma']],".\n")
 #   }else if(tolower(output.units) == 'umol'){
  #    cat("AUC is area under plasma concentration in uM * days units with Rblood2plasma =",parameters[['Rblood2plasma']],".\n")
   # }else cat("AUC is area under plasma concentration curve in",output.units,"* days units with Rblood2plasma =",parameters[['Rblood2plasma']],".\n")
  }
    
  return(out) 
}
