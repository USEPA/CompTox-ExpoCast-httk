#' Solve one compartment TK model
#' 
#' This function solves for the amount or concentration of a chemical in plasma
#' for a one compartment model as a function of time based on the dose and
#' dosing frequency. 
#' 
#' Note that the model parameters have units of hours while the model output is
#' in days.
#' 
#' Default value of NULL for doses.per.day solves for a single dose.
#' 
#' When species is specified as rabbit, dog, or mouse, the function uses the
#' appropriate physiological data(volumes and flows) but substitues human
#' fraction unbound, partition coefficients, and intrinsic hepatic clearance.
#' 
#' AUC is area under plasma concentration curve.
#' 
#' Model Figure 
#' \if{html}{\figure{1comp.png}{options: width="60\%" alt="Figure: One
#' Compartment Model Schematic"}}
#' \if{latex}{\figure{1comp.pdf}{options: width=12cm alt="Figure: One
#' Compartment Model Schematic"}}
#' 
#' 
#' 
#' @param chem.name Either the chemical name, CAS number, or the parameters
#' must be specified.
#' @param chem.cas Either the chemical name, CAS number, or the parameters must
#' be specified.
#' @param times Optional time sequence for specified number of days.
#' @param parameters Chemical parameters from parameterize_1comp function,
#' overrides chem.name and chem.cas.
#' @param days Length of the simulation.
#' @param tsteps The number time steps per hour.
#' @param daily.dose Total daily dose, mg/kg BW.
#' @param dose Amount of a single dose, mg/kg BW.  Overwrites daily.dose.
#' @param doses.per.day Number of doses per day.
#' @param species Species desired (either "Rat", "Rabbit", "Dog", or default
#' "Human").
#' @param iv.dose Simulates a single i.v. dose if true.
#' @param output.units Desired units (either "mg/L", "mg", "umol", or default
#' "uM").
#' @param initial.values Vector containing the initial concentrations or
#' amounts of the chemical in specified tissues with units corresponding to
#' output.units.  Defaults are zero.
#' @param suppress.messages Whether or not the output message is suppressed.
#' @param plots Plots all outputs if true.
#' @param method Method used by integrator (deSolve).
#' @param rtol Argument passed to integrator (deSolve).
#' @param atol Argument passed to integrator (deSolve).
#' @param default.to.human Substitutes missing rat values with human values if
#' true.
#' @param dosing.matrix Vector of dosing times or a matrix consisting of two
#' columns or rows named "dose" and "time" containing the time and amount, in
#' mg/kg BW, of each dose.
#' @param recalc.elimination Whether or not to recalculate the elimination
#' rate.
#' @param adjusted.Funbound.plasma Uses adjusted Funbound.plasma when set to
#' TRUE along with volume of distribution calculated with this value.
#' @param regression Whether or not to use the regressions in calculating
#' partition coefficients in volume of distribution calculation.
#' @param restrictive.clearance In calculating elimination rate, protein
#' binding is not taken into account (set to 1) in liver clearance if FALSE.
#' @param well.stirred.correction Uses correction in calculation of hepatic
#' clearance for well-stirred model if TRUE.  This assumes clearance relative
#' to amount unbound in whole blood instead of plasma, but converted to use
#' with plasma concentration.
#' @param minimum.Funbound.plasma Monte Carlo draws less than this value are set 
#' equal to this value (default is 0.0001 -- half the lowest measured Fup in our
#' dataset).
#' @param ... Additional arguments passed to the integrator.
#' @return A matrix with a column for time(in days) and a column for the
#' compartment and the area under the curve (concentration only).
#' @author Robert Pearce
#' @references Pearce, Robert G., et al. "Httk: R package for high-throughput
#' toxicokinetics." Journal of statistical software 79.4 (2017): 1.
#' @keywords Solve
#' @examples
#' 
#' solve_1comp(chem.name='Bisphenol-A',days=1)
#' params <- parameterize_1comp(chem.cas="80-05-7")
#' solve_1comp(parameters=params)
#' @import deSolve 
#' @export solve_1comp
#' @useDynLib httk

solve_1comp <- function(chem.name=NULL,
                        chem.cas=NULL,
                        times=NULL,
                        parameters=NULL,
                        daily.dose=1,
                        dose = NULL,
                        doses.per.day=NULL,
                        days=10,
                        tsteps=4,
                        suppress.messages=F,
                        species='Human',
                        output.units='uM',
                        plots=F,
                        initial.values=NULL,
                        iv.dose = F,
                        method="lsoda",rtol=1e-8,atol=1e-12,
                        default.to.human=F,
                        dosing.matrix = NULL,
                        recalc.elimination=F,
                        adjusted.Funbound.plasma=T,
                        regression=T,
                        restrictive.clearance=T,
                        well.stirred.correction=T,
                        minimum.Funbound.plasma=0.0001,                        
                        ...)
{     
   Agutlumen <- Acompartment <- Ccompartment <- NULL 
  if (is.null(chem.cas) & is.null(chem.name) & is.null(parameters)) 
    stop('Parameters, chem.name, or chem.cas must be specified.')
  if (is.null(parameters))
  {
   parameters <- parameterize_1comp(chem.name=chem.name,
                   chem.cas=chem.cas,
                   species=species,
                   default.to.human=default.to.human,
                   adjusted.Funbound.plasma=adjusted.Funbound.plasma,
                   regression=regression,
                   restrictive.clearance=restrictive.clearance,
                   well.stirred.correction=well.stirred.correction,
                   suppress.messages=suppress.messages,
                   minimum.Funbound.plasma=minimum.Funbound.plasma) 
  } else {
     if (!all(param.names.1comp %in% names(parameters)))
       stop(paste("Missing parameters:",
       paste(param.names.1comp[which(!param.names.1comp %in% names(parameters))],
       collapse=', '),".  Use parameters from parameterize_1comp."))
  }
  Rb2p <- parameters[['Rblood2plasma']]
  BW <- parameters[['BW']]
   
  parameters$Fgutabs <- parameters$Fgutabs * parameters$hepatic.bioavailability

  if(is.null(times)) times <- round(seq(0, days, 1/(24*tsteps)),8)
  start <- times[1]
  end <- times[length(times)] 
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
   
  if(tolower(output.units)=='um' |  tolower(output.units) == 'mg/l') use.amounts <- F
  if(tolower(output.units)=='umol' |  tolower(output.units) == 'mg') use.amounts <- T 
   
  if(tolower(output.units)=='um' | tolower(output.units) == 'umol')
  {
    dose <- as.numeric(dose / 1000 / parameters[["MW"]] * 1000000)
    if(!is.null(dosing.matrix)) dose.vector <- as.numeric(dose.vector / 1000 / parameters[["MW"]] * 1000000)
  } else if(!(tolower(output.units) == 'mg/l' | tolower(output.units) == 'mg'))
  {
    stop('Output.units can only be uM, umol, mg, or mg/L.')

  }
  
   if (use.amounts)
  {
    CompartmentsToInitialize <-c("Agutlumen","Acompartment")
  } else {
    CompartmentsToInitialize <-c("Agutlumen","Ccompartment")
  }

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

  if(use.amounts)
  {
    if(iv.dose)
    {
      state <- c(Agutlumen=Agutlumen,Acompartment = Acompartment + dose ,Ametabolized = 0,AUC=0)
    }else{
      state <- c(Agutlumen= Agutlumen + dose,Acompartment = Acompartment ,Ametabolized = 0,AUC=0)
    }
  }else{
    if(iv.dose)
    {
      state <- c(Agutlumen=Agutlumen,Acompartment = dose + Ccompartment * parameters[['Vdist']],Ametabolized = 0,AUC=0)
    } else{
      state <- c(Agutlumen= Agutlumen + dose,Acompartment = Ccompartment * parameters[['Vdist']],Ametabolized = 0,AUC=0)
    }
  }
  
  if(recalc.elimination){
  parameters$kelim <- calc_elimination_rate(parameters=parameters,chem.cas=chem.cas,chem.name=chem.name,species=species,suppress.messages=T,default.to.human=default.to.human)

  }
  parameters[['ke']] <- parameters[['kelim']]   
  parameters[['vdist']] <- parameters[['Vdist']]
  parameters <- initparms1comp(parameters[param.names.1comp.solver])
  
  state <-initState1comp(parameters,state)
  

  if(is.null(dosing.matrix)){
    if(is.null(doses.per.day)){
      out <- ode(y = state, times = times,func="derivs1comp", parms=parameters, method=method,rtol=rtol,atol=atol, dllname="httk",initfunc="initmod1comp", nout=length(Outputs1comp),outnames=Outputs1comp,...)
    }else{
      dosing <- seq(start + 1/doses.per.day,end-1/doses.per.day,1/doses.per.day)
      length <- length(dosing)
      if(iv.dose) eventdata <- data.frame(var=rep('Acompartment',length),time = round(dosing,8),value = rep(dose,length), method = rep("add",length))           
      else eventdata <- data.frame(var=rep('Agutlumen',length),time = round(dosing,8),value = rep(dose,length), method = rep("add",length)) 
      times <- sort(c(times,dosing + 1e-8,start + 1e-8))               
      out <- ode(y = state, times = times, func="derivs1comp", parms = parameters, method=method,rtol=rtol,atol=atol, dllname="httk",initfunc="initmod1comp", nout=length(Outputs1comp),outnames=Outputs1comp,events=list(data=eventdata),...)
    }
  }else{
    if(iv.dose) eventdata <- data.frame(var=rep('Acompartment',length(dosing.times)),time = dosing.times,value = dose.vector, method = rep("add",length(dosing.times)))
    else eventdata <- data.frame(var=rep('Agutlumen',length(dosing.times)),time = dosing.times,value = dose.vector, method = rep("add",length(dosing.times)))                          
    times <- sort(c(times,dosing.times + 1e-8,start + 1e-8))
    out <- ode(y = state, times = times, func="derivs1comp", parms = parameters, method=method,rtol=rtol,atol=atol, dllname="httk",initfunc="initmod1comp", nout=length(Outputs1comp),outnames=Outputs1comp,events=list(data=eventdata),...)
  }
   
  if(use.amounts) out[,c('Agutlumen','Acompartment','Ametabolized')] <- out[,c('Agutlumen','Acompartment','Ametabolized')] * BW
  else out[,c('Agutlumen','Ametabolized')] <- out[,c('Agutlumen','Ametabolized')] * BW

 if(plots==T)
  {
    graphics::plot(out,select=c(CompartmentsToInitialize,"Ametabolized","AUC"))
  }
  
  out <- out[,c("time",CompartmentsToInitialize,"Ametabolized","AUC")]
  class(out) <- c('matrix','deSolve')
  
  if(!suppress.messages){
    if(is.null(chem.cas) & is.null(chem.name)){
      if(use.amounts){
        cat("Values returned in",output.units," units.\n")
      }else{
        if(tolower(output.units) == 'um'){
          out.amount <- 'umol'
        }else out.amount <- 'mg'
        cat("Amounts returned in",out.amount," and concentration returned in",output.units,"units.\n")
      }
    }else{
      if(use.amounts){  
        cat(paste(toupper(substr(species,1,1)),substr(species,2,nchar(species)),sep=''),"values returned in",output.units,"units.\n")
      }else{
        if(tolower(output.units) == 'um'){
          out.amount <- 'umol'
        }else out.amount <- 'mg'
        cat(paste(toupper(substr(species,1,1)),substr(species,2,nchar(species)),sep=''),"amounts returned in",out.amount,"and concentration returned in",output.units,"units.\n")
      }
    }
    if(tolower(output.units) == 'mg'){
      cat("AUC is area under compartment concentration in mg/L * days units with Rblood2plasma =",Rb2p,".\n")
    }else if(tolower(output.units) == 'umol'){
      cat("AUC is area under compartment concentration in uM * days units with Rblood2plasma =",Rb2p,".\n")
    }else cat("AUC is area under plasma concentration curve in",output.units,"* days units with Rblood2plasma =",Rb2p,".\n")
  }

  return(out)
}