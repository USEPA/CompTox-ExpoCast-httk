#' Solve_PBTK
#' 
#' This function solves for the amounts or concentrations in uM of a chemical
#' in different tissues as functions of time based on the dose and dosing
#' frequency. 
#' 
#' Note that the model parameters have units of hours while the model output is
#' in days.
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
#' Model Figure 
#' \if{html}{\figure{pbtk.png}{options: width="60\%" alt="Figure: PBTK Model
#' Schematic"}}
#' \if{latex}{\figure{pbtk.pdf}{options: width=12cm alt="Figure: PBTK Model
#' Schematic"}}
#' 
#' When species is specified as rabbit, dog, or mouse, the function uses the
#' appropriate physiological data(volumes and flows) but substitues human
#' fraction unbound, partition coefficients, and intrinsic hepatic clearance.
#' 
#' 
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
#' @param tsteps The number of time steps per hour.
#' @param daily.dose Total daily dose, mg/kg BW.
#' @param dose Amount of a single dose, mg/kg BW.  Overwrites daily.dose.
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
#' @param restrictive.clearance Protein binding not taken into account (set to
#' 1) in liver clearance if FALSE.
#' @param minimum.Funbound.plasma Monte Carlo draws less than this value are set 
#' equal to this value (default is 0.0001 -- half the lowest measured Fup in our
#' dataset).
#' @param ... Additional arguments passed to the integrator.
#' @return A matrix of class deSolve with a column for time(in days), each
#' compartment, the area under the curve, and plasma concentration and a row
#' for each time point.
#' @author John Wambaugh and Robert Pearce
#' @references Pearce, Robert G., et al. "Httk: R package for high-throughput
#' toxicokinetics." Journal of statistical software 79.4 (2017): 1.
#' @keywords Solve
#' @examples
#' 
#' 
#' solve_pbtk(chem.name='Bisphenol-A',dose=.5,days=1,doses.per.day=2,tsteps=2)
#' out <- solve_pbtk(chem.name='bisphenola',dose=0,output.units='mg', 
#'                   plots=TRUE,initial.values=c(Agut=200))
#' params <- parameterize_pbtk(chem.cas="80-05-7")
#' solve_pbtk(parameters=params)
#'                   
#' \dontrun{
#' parameters <- parameterize_pbtk(chem.name = "triclosan", species = "rat")
#' parameters["Funbound.plasma"] <- 0.1
#' out <- solve_pbtk(parameters=parameters)
#' 
#' library("ggplot2")
#' out <- solve_pbtk(chem.name = "Bisphenol A", days = 50, doses.per.day = 3)
#' plot.data <- as.data.frame(out)
#' css <- calc_analytic_css(chem.name = "Bisphenol A")
#' c.vs.t <- ggplot(plot.data,aes(time, Cplasma)) + geom_line() +
#' geom_hline(yintercept = css) + ylab("Plasma Concentration (uM)") +
#' xlab("Day") + theme(axis.text = element_text(size = 16), axis.title =
#' element_text(size = 16), plot.title = element_text(size = 17)) +
#' ggtitle("Bisphenol A")
#' print(c.vs.t)
#' }
#' 
#' @import deSolve 
#' @export solve_pbtk
#' @useDynLib httk
solve_pbtk <- function(chem.name = NULL,
                    chem.cas = NULL,
                    times=NULL,
                    parameters=NULL,
                    days=10,
                    tsteps = 4, # tsteps is number of steps per hour
                    daily.dose = 1,
                    dose = NULL, # Assume dose is in mg/kg BW/day  
                    doses.per.day=NULL,
                    initial.values=NULL,
                    plots=F,
                    suppress.messages=F,
                    species="Human",
                    iv.dose=F,
                    output.units='uM',
                    method="lsoda",rtol=1e-8,atol=1e-12,
                    default.to.human=F,
                    recalc.blood2plasma=F,
                    recalc.clearance=F,
                    dosing.matrix=NULL,
                    adjusted.Funbound.plasma=T,
                    regression=T,
                    restrictive.clearance = T,
                    minimum.Funbound.plasma=0.0001,
                    ...)
{
  Aart <- Agut <- Agutlumen <- Alung <- Aliver <- Aven <- Arest <- NULL
  Akidney <- Cgut <- Vgut <- Cliver <- Vliver <- Cven <- Vven <- Clung <- NULL
  Vlung <- Cart <- Vart <- Crest <- Vrest <- Ckidney <- Vkidney <- NULL
  
  if (is.null(chem.cas) & is.null(chem.name) & is.null(parameters)) 
    stop('Parameters, chem.name, or chem.cas must be specified.')
  if (is.null(parameters)){
    parameters <- parameterize_pbtk(
                    chem.cas=chem.cas,
                    chem.name=chem.name,
                    species=species,
                    default.to.human=default.to.human,
                    suppress.messages=suppress.messages,
                    adjusted.Funbound.plasma=adjusted.Funbound.plasma,
                    regression=regression,
                    minimum.Funbound.plasma=minimum.Funbound.plasma) 
  }else{
#    name.list <- c("BW","Clmetabolismc","Funbound.plasma","Fgutabs","Fhep.assay.correction","hematocrit","Kgut2pu","kgutabs","Kkidney2pu","Kliver2pu","Klung2pu","Krbc2pu","Krest2pu","million.cells.per.gliver","MW","Qcardiacc" ,"Qgfrc","Qgutf","Qkidneyf","Qliverf","Rblood2plasma","Vartc","Vgutc","Vkidneyc","Vliverc","Vlungc","Vrestc","Vvenc")
  if(!all(param.names.pbtk %in% names(parameters)))stop(paste("Missing parameters:",paste(param.names.pbtk[which(!param.names.pbtk %in% names(parameters))],collapse=', '),".  Use parameters from parameterize_pbtk.")) 
  }
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

  lastchar <- function(x){substr(x, nchar(x), nchar(x))}
  firstchar <- function(x){substr(x, 1,1)}

  #Rblood2plasma <- parameters[["Rblood2plasma"]]
  #hematocrit <- parameters[["hematocrit"]]
   #dose converted to umoles/day  from  mg/kg BW/day 
   
     if(tolower(output.units)=='um' |  tolower(output.units) == 'mg/l') use.amounts <- F
     if(tolower(output.units)=='umol' |  tolower(output.units) == 'mg') use.amounts <- T
  
     if(tolower(output.units)=='um' | tolower(output.units) == 'umol'){
       dose <- as.numeric(dose * parameters[["BW"]] / 1000 / parameters[["MW"]] * 1000000)
       if(!is.null(dosing.matrix)) dose.vector <- as.numeric(dose.vector * parameters[["BW"]] / 1000 / parameters[["MW"]] * 1000000)
     }else if(tolower(output.units) == 'mg/l' | tolower(output.units) == 'mg'){
       dose <- dose * parameters[['BW']]
       if(!is.null(dosing.matrix)) dose.vector <-  dose.vector * parameters[['BW']]
     }else stop('Output.units can only be uM, umol, mg, or mg/L.')
   
  
parameters[["MW"]] <- NULL

  
  scaled.volumes <- names(parameters)[firstchar(names(parameters))=="V"&lastchar(names(parameters))=="c"]
        
  for (this.vol in scaled.volumes)
  {
      eval(parse(text=paste(substr(this.vol,1,nchar(this.vol)-1), '<-', parameters[[this.vol]],'*', parameters[["BW"]]))) # L 
  }
  
  
  if (use.amounts)
  {
    CompartmentsToInitialize <-c("Agutlumen","Aart","Aven","Alung","Agut","Aliver","Akidney","Arest")
  } else {
    CompartmentsToInitialize <-c("Agutlumen","Cart","Cven","Clung","Cgut","Cliver","Ckidney","Crest")
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
  

   if (use.amounts) 
  {
    if(iv.dose){
      state <- c(Aart = Aart,Agut = Agut,Agutlumen = Agutlumen,Alung = Alung,Aliver = Aliver,
               Aven = Aven + dose,Arest = Arest,Akidney = Akidney,Atubules = 0,Ametabolized = 0,AUC=0)
    }else{
      state <- c(Aart = Aart,Agut = Agut,Agutlumen = Agutlumen + dose,Alung = Alung,Aliver = Aliver,
               Aven = Aven,Arest = Arest,Akidney = Akidney,Atubules = 0,Ametabolized = 0,AUC=0)
    }
  }else{
    if(iv.dose){
      state <- c(Agutlumen = Agutlumen,Agut = Cgut * Vgut,Aliver = Cliver * Vliver,Aven = Cven * Vven + dose,Alung = Clung * Vlung,Aart = Cart * Vart,Arest = Crest * Vrest,Akidney = Ckidney * Vkidney,Atubules = 0,Ametabolized = 0,AUC=0)
    }else{
      state <- c(Agutlumen = Agutlumen + dose,Agut = Cgut * Vgut,Aliver = Cliver * Vliver,Aven = Cven * Vven,Alung = Clung * Vlung,Aart = Cart * Vart,Arest = Crest * Vrest,Akidney = Ckidney * Vkidney,Atubules = 0,Ametabolized = 0,AUC=0)
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
  
  parameters[['Fraction_unbound_plasma']] <- parameters[['Funbound.plasma']]
# Here we remove model parameters that are not needed by the C solver:
  parameters <- pbtk.initparms(parameters[param.names.pbtk.solver])

  state <-pbtk.initState(parameters,state)

  if(is.null(dosing.matrix)){
    if(is.null(doses.per.day)){
      out <- ode(y = state, 
        times = times,
# This is the derivatives function specific to this model:
        func="derivspbtk", 
        parms=parameters,
        method=method,
        rtol=rtol,
        atol=atol,
# This is the httk.dll file containing all the .C code:
        dllname="httk",
        initfunc="initmodpbtk", 
# Here we make sure we get the number of outputs from the model that we expect:
        nout=length(pbtk.Outputs),
# Here we assign them names (order matters!)
        outnames=pbtk.Outputs,
        ...)
    }else{
      dosing <- seq(start + 1/doses.per.day,end-1/doses.per.day,1/doses.per.day)
      length <- length(dosing)
      if(iv.dose) eventdata <- data.frame(var=rep('Aven',length),
        time = round(dosing,8),
        value = rep(dose,length), 
        method = rep("add",length))
      else eventdata <- data.frame(var=rep('Agutlumen',length),
        time = round(dosing,8),
        value = rep(dose,length), 
        method = rep("add",length))
      times <- sort(c(times,dosing + 1e-8,start + 1e-8))
      out <- ode(y = state, 
        times = times, 
# This is the derivatives function specific to this model:
        func="derivspbtk", 
        parms = parameters,
        method=method,
        rtol=rtol,
        atol=atol, 
# This is the httk.dll file containing all the .C code:
        dllname="httk",
        initfunc="initmodpbtk", 
# Here we make sure we get the number of outputs from the model that we expect:
        nout=length(pbtk.Outputs),
# Here we assign them names (order matters!)
        outnames=pbtk.Outputs,
        events=list(data=eventdata),
        ...)
    }      
  }else{
    if(iv.dose) eventdata <- data.frame(var=rep('Aven',length(dosing.times)),time = dosing.times,value = dose.vector, method = rep("add",length(dosing.times)))                          
    else eventdata <- data.frame(var=rep('Agutlumen',length(dosing.times)),time = dosing.times,value = dose.vector, method = rep("add",length(dosing.times)))
    times <- sort(c(times,dosing.times + 1e-8,start + 1e-8))
    out <- ode(y = state, 
      times = times, 
# This is the derivatives function specific to this model:
      func="derivspbtk", 
      parms = parameters,
      method=method,
      rtol=rtol,
      atol=atol, 
# This is the httk.dll file containing all the .C code:
      dllname="httk",
      initfunc="initmodpbtk", 
# Here we make sure we get the number of outputs from the model that we expect:
      nout=length(pbtk.Outputs),
# Here we assign them names (order matters!)
      outnames=pbtk.Outputs,
      events=list(data=eventdata),...)                                
  }
  
  
  if(plots==T)
  {
    if(use.amounts){
      graphics::plot(out,select=c(CompartmentsToInitialize,"Ametabolized","Atubules","Aplasma","AUC"))
    }else{
      graphics::plot(out,select=c(CompartmentsToInitialize,"Ametabolized","Atubules","Cplasma","AUC"))
    }
    
  }
    if(use.amounts){
      out <- out[,c("time",CompartmentsToInitialize,"Ametabolized","Atubules","Aplasma","AUC")]
    }else{
      out <- out[,c("time",CompartmentsToInitialize,"Ametabolized","Atubules","Cplasma","AUC")]
    }
  class(out) <- c('matrix','deSolve')
  
  if(!suppress.messages){
    if(is.null(chem.cas) & is.null(chem.name)){
      cat("Values returned in",output.units,"units.\n")
      if(!recalc.blood2plasma) warning('Rblood2plasma not recalculated.  Set recalc.blood2plasma to TRUE if desired.') 
      if(!recalc.clearance) warning('Clearance not recalculated.  Set recalc.clearance to TRUE if desired.') 
    }else cat(paste(toupper(substr(species,1,1)),substr(species,2,nchar(species)),sep=''),"values returned in",output.units,"units.\n")
    if(tolower(output.units) == 'mg'){
      cat("AUC is area under plasma concentration in mg/L * days units with Rblood2plasma =",parameters[['Rblood2plasma']],".\n")
    }else if(tolower(output.units) == 'umol'){
      cat("AUC is area under plasma concentration in uM * days units with Rblood2plasma =",parameters[['Rblood2plasma']],".\n")
    }else cat("AUC is area under plasma concentration curve in",output.units,"* days units with Rblood2plasma =",parameters[['Rblood2plasma']],".\n")
  }
    
  return(out) 
}