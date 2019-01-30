solve_gas_pbtk <- function(chem.name = NULL,
                    chem.cas = NULL,
                    times=NULL,
                    parameters=NULL,
                    days=10,
                    tsteps = 4, # tsteps is number of steps per hour
                    initial.values=NULL,
                    plots=F,
                    suppress.messages=F,
                    species="Human",
                    method="lsoda",rtol=1e-8,atol=1e-12,
                    default.to.human=F,
                    recalc.blood2plasma=F,
                    recalc.clearance=F,
                    dosing.matrix=NULL,
                    adjusted.Funbound.plasma=T,
                    regression=T,        
                    use.amounts=F,                    
                    restrictive.clearance = T,
                    air.concentration=1,
                    period = 1,
                    exposure = .5,
		    vmax.km=F,
		    vmax = 0,
		    km = 1,
                    ...)
{
  Aart <- Agut <- Agutlumen <- Alung <- Aliver <- Aven <- Arest <- Akidney <- Cgut <- Vgut <- Cliver <- Vliver <- Cven <- Vven <- Clung <- Vlung <- Cart <- Vart <- Crest <- Vrest <- Ckidney <- Vkidney <- Calv <- Cendexh <- Cmixexh <- NULL
  if(is.null(chem.cas) & is.null(chem.name) & is.null(parameters)) stop('Parameters, chem.name, or chem.cas must be specified.')
  if(is.null(parameters)){
    parameters <- parameterize_gas_pbtk(chem.cas=chem.cas,chem.name=chem.name,species=species,default.to.human=default.to.human,suppress.messages=suppress.messages,
                                    adjusted.Funbound.plasma=adjusted.Funbound.plasma,regression=regression, vmax.km=vmax.km, vmax = vmax, km = km)                                  
  }else{
    name.list <- c("BW","Clmetabolismc","Funbound.plasma","Fgutabs","Fhep.assay.correction","hematocrit","Kgut2pu","kgutabs","Kkidney2pu","Kliver2pu","Klung2pu","Krbc2pu","Krest2pu","million.cells.per.gliver","MW","Qcardiacc" ,"Qgfrc","Qgutf","Qkidneyf","Qliverf","Rblood2plasma","Vartc","Vgutc","Vkidneyc","Vliverc","Vlungc","Vrestc","Vvenc")
  if(!all(name.list %in% names(parameters)))stop(paste("Missing parameters:",paste(name.list[which(!name.list %in% names(parameters))],collapse=', '),".  Use parameters from parameterize_pbtk.")) 
  }
  if(is.null(times)) times <- round(seq(0, days, 1/(24*tsteps)),8)
  start <- times[1]
  end <- times[length(times)]
  
  lastchar <- function(x){substr(x, nchar(x), nchar(x))}
  firstchar <- function(x){substr(x, 1,1)}

  #Rblood2plasma <- parameters[["Rblood2plasma"]]
  #hematocrit <- parameters[["hematocrit"]]
   #dose converted to umoles/day  from  mg/kg BW/day 
  
  
  parameters[["MW"]] <- NULL

  scaled.volumes <- names(parameters)[firstchar(names(parameters))=="V"&lastchar(names(parameters))=="c"]
        
  for (this.vol in scaled.volumes)
  {
      eval(parse(text=paste(substr(this.vol,1,nchar(this.vol)-1), '<-', parameters[[this.vol]],'*', parameters[["BW"]]))) # L 
  }
  
  
  if (use.amounts) CompartmentsToInitialize <-c("Agutlumen","Aart","Aven","Alung","Agut","Aliver","Akidney","Arest")
  else CompartmentsToInitialize <-c("Agutlumen","Cart","Cven","Clung","Cgut","Cliver","Ckidney","Crest")
  

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
  

   if (use.amounts) {
    state <- c(Aart = Aart,Agut = Agut,Agutlumen = Agutlumen,Alung = Alung,Aliver = Aliver,
               Aven = Aven,Arest = Arest,Akidney = Akidney,Atubules = 0,Ametabolized = 0,AUC=0)
  }else{
    state <- c(Agutlumen = Agutlumen,Agut = Cgut * Vgut,Aliver = Cliver * Vliver,Aven = Cven * Vven,Alung = Clung * Vlung,Aart = Cart * Vart,Arest = Crest * Vrest,Akidney = Ckidney * Vkidney,Atubules = 0,Ametabolized = 0,AUC=0)
  }    
  
  if(recalc.blood2plasma) parameters[['Rblood2plasma']] <- 1 - parameters[['hematocrit']] + parameters[['hematocrit']] * parameters[['Krbc2pu']] * parameters[['Funbound.plasma']]
  
  if(recalc.clearance & !vmax.km){
    if(is.null(chem.name) & is.null(chem.cas)) stop('Chemical name or CAS must be specified to recalculate hepatic clearance.')
    ss.params <- parameterize_steadystate(chem.name=chem.name,chem.cas=chem.cas)
    ss.params[['million.cells.per.gliver']] <- parameters[['million.cells.per.gliver']]
    parameters[['Clmetabolismc']] <- calc_hepatic_clearance(parameters=ss.params,hepatic.model='unscaled',suppress.messages=T)
  } 
  if(!restrictive.clearance) parameters$Clmetabolismc <- parameters$Clmetabolismc / parameters$Funbound.plasma
  
  parameters[['Fraction_unbound_plasma']] <- parameters[['Funbound.plasma']]
  	parameters <- initParms_inh(parameters[!(names(parameters) %in% c("Fhep.assay.correction","Krbc2pu","million.cells.per.gliver","Fgutabs","Funbound.plasma"))])
  	state <-initStates_inh(parameters,state) 
     
  	forcing <- function(mag, Period, start, ExpDuration, times, vmax, km) {
  		Nrep <- ceiling(max(times) / Period) 
  		times <- rep(c(start, ExpDuration), Nrep) + rep(Period * (0:(Nrep - 1)), rep(2, Nrep))
  		y  <- rep(c(mag,0), Nrep)
 		 cbind(times,y)
 		 }
  	Forc <- list(forcing(air.concentration, period, 0,exposure, times))  # possible to have more than one in list
  	if(!is.null(dosing.matrix)) Forc <- dosing.matrix
  	out <- ode(y = state, times = times, func="derivs_inh", parms = parameters,method=method,rtol=rtol,atol=atol, dllname="httk",initfunc="initmod_inh", nout=length(Outputs_inh),outnames=Outputs_inh,initforc= "initforc_inh",forcing=Forc,fcontrol=list(method='constant',rule=2,f=0),...)   


  if(plots==T)
  {
    if(use.amounts){
      plot(out,select=c(CompartmentsToInitialize,"Ametabolized","Atubules","Aplasma","AUC"))
    }else{
      plot(out,select=c(CompartmentsToInitialize,"Ametabolized","Atubules","Cplasma","AUC"))
    }
    
  }
    if(use.amounts){
      out <- out[,c("time",CompartmentsToInitialize,"Ametabolized","Atubules","Aplasma","AUC","Cendexh","Cmixexh")]
    }else{
      out <- out[,c("time",CompartmentsToInitialize,"Ametabolized","Atubules","Cplasma","AUC","Cendexh","Cmixexh")]
    }
  class(out) <- c('matrix','deSolve')
  
  if(!suppress.messages){
    if(is.null(chem.cas) & is.null(chem.name)){
      cat("Values returned in same units as air concentration(units/L).\n")
      if(!recalc.blood2plasma) warning('Rblood2plasma not recalculated.  Set recalc.blood2plasma to TRUE if desired.') 
      if(!recalc.clearance) warning('Clearance not recalculated.  Set recalc.clearance to TRUE if desired.') 
    }else cat(paste(toupper(substr(species,1,1)),substr(species,2,nchar(species)),sep=''),"values returned in same units as air concentration(units/L).\n")
      cat("AUC is area under plasma concentration in units/L * days units with Rblood2plasma =",parameters[['Rblood2plasma']],".\n")
  }
    
  return(out) 
}