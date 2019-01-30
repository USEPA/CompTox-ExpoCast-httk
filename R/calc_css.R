# Written by Robert Pearce and John Wambaugh
calc_css <- function(parameters=NULL,
                    chem.name=NULL,
                    chem.cas=NULL, 
                    species='Human',
                    f = .01,
                    daily.dose=1,
                    doses.per.day=3,
                    days = 21,
                    output.units = "uM",
                    concentration='plasma',
                    suppress.messages=F,
                    model='pbtk',
                    default.to.human=F,
                    f.change = 0.00001,
                    adjusted.Funbound.plasma=T,
                    regression=T,
                    well.stirred.correction=T,
                    restrictive.clearance=T,
                    ...)
{
  
  if(is.null(parameters)){
    if(tolower(model)=='pbtk'){
      parameters <- parameterize_pbtk(chem.cas=chem.cas,chem.name=chem.name,species=species,default.to.human=default.to.human,adjusted.Funbound.plasma=adjusted.Funbound.plasma,regression=regression)
    }else if(tolower(model)=='3compartment'){
      parameters <- parameterize_3comp(chem.cas=chem.cas,chem.name=chem.name,species=species,default.to.human=default.to.human,adjusted.Funbound.plasma=adjusted.Funbound.plasma,regression=regression)
    }else if(tolower(model)=='1compartment'){
      parameters <- parameterize_1comp(chem.cas=chem.cas,chem.name=chem.name,species=species,default.to.human=default.to.human,adjusted.Funbound.plasma=adjusted.Funbound.plasma,regression=regression,well.stirred.correction=well.stirred.correction,restrictive.clearance=restrictive.clearance)
    }
  } 

  css <- calc_analytic_css(parameters=parameters,daily.dose=daily.dose,concentration='plasma',model=model,suppress.messages=T,adjusted.Funbound.plasma=adjusted.Funbound.plasma,regression=regression,well.stirred.correction=well.stirred.correction,restrictive.clearance=restrictive.clearance) 
  target.conc <- (1 - f) * css 

  # Initially simulate for a time perioud of length "days":
  if(tolower(model) == 'pbtk'){
    out <- solve_pbtk(parameters=parameters, daily.dose=daily.dose,doses.per.day=doses.per.day,days = days,suppress.messages=T,restrictive.clearance=restrictive.clearance,...)
    Final_Conc <- out[dim(out)[1],c("Agutlumen","Cart","Cven","Clung","Cgut","Cliver","Ckidney","Crest")]
  }else if(tolower(model) =='3compartment'){
    out <- solve_3comp(parameters=parameters, daily.dose=daily.dose,doses.per.day=doses.per.day, days = days,suppress.messages=T,restrictive.clearance=restrictive.clearance,...)
    Final_Conc <- out[dim(out)[1],c("Agutlumen","Cgut","Cliver","Crest")]
  }else if(tolower(model)=='1compartment'){
    out <- solve_1comp(parameters=parameters,daily.dose=daily.dose,doses.per.day=doses.per.day, days = days,suppress.messages=T,...)
    Final_Conc <- out[dim(out)[1],c("Agutlumen","Ccompartment")]
  }else stop('The model options are only: 1compartment, 3compartment, and pbtk.')
  
  total.days <- days
  additional.days <- days

  while(all(out[,"Cplasma"] < target.conc) & 
       ((out[match((additional.days - 1),out[,'time']),'Cplasma']-
        out[match((additional.days - 2),out[,'time']),'Cplasma'])/
        out[match((additional.days - 2),out[,'time']),'Cplasma'] > f.change))
  {
    if(additional.days < 1000)
    {
      additional.days <- additional.days * 5
    }#else{
    #  additional.days <- additional.days * 3
    #}
    total.days <- total.days + additional.days
    
  if(tolower(model) == 'pbtk'){
    out <- solve_pbtk(parameters=parameters,initial.values = Final_Conc, daily.dose=daily.dose,doses.per.day=doses.per.day, days = additional.days,suppress.messages=T,restrictive.clearance=restrictive.clearance,...)
    Final_Conc <- out[dim(out)[1],c("Agutlumen","Cart","Cven","Clung","Cgut","Cliver","Ckidney","Crest")]
  }else if(tolower(model) =='3compartment'){
    out <- solve_3comp(parameters=parameters,initial.values = Final_Conc, daily.dose=daily.dose,doses.per.day=doses.per.day, days = additional.days,suppress.messages=T,restrictive.clearance=restrictive.clearance,...)
    Final_Conc <- out[dim(out)[1],c("Agutlumen","Cgut","Cliver","Crest")]
  }else if(tolower(model)=='1compartment'){
    out <- solve_1comp(parameters=parameters,daily.dose=daily.dose,doses.per.day=doses.per.day, days = additional.days,suppress.messages=T,initial.values=Final_Conc,...)
    Final_Conc <- out[dim(out)[1],c('Agutlumen','Ccompartment')]
  }
  
    if(total.days > 36500) break 
  }
  
# Calculate the day Css is reached:
  if (total.days < 36500)
  {
    # The day the simulation started:
    sim.start.day <- total.days - additional.days
    # The day the current simulation reached Css:
    if(any(out[,"Cplasma"] >= target.conc))
    {
      sim.css.day <- floor(min(out[out[,"Cplasma"]>=target.conc,"time"]))
    } else {
      sim.css.day <- additional.days
    }
    # The overall day the simulation reached Css:
    css.day <- sim.start.day+sim.css.day
    # Fraction of analytic Css achieved:
    last.day.subset<-subset(out,out[,"time"]<(sim.css.day+1) &
                                out[,"time"]>=(sim.css.day))
    frac_achieved <- as.numeric(mean(last.day.subset[,"Cplasma"])/css)
  } else{ 
   if(!suppress.messages)cat("Analytic css not reached after 100 years.")
   css.day  <- 36500
   frac_achieved <- as.numeric(max(subset(out[,concentration]))/css)  
  }     
   
  if (tolower(output.units) == tolower("mg/L")) 
  {
      out[,'AUC'] <- out[,'AUC']/1e+06 * parameters[["MW"]] * 1000
      css <- css /1e+06 * parameters[["MW"]] * 1000
      if(tolower(model)=='1compartment'){
        out[,'Ccompartment'] <- out[,'Ccompartment']/1e+06 * parameters[["MW"]] * 1000
      }else{  
        out[,'Cplasma'] <- out[,'Cplasma']/1e+06 * parameters[["MW"]] * 1000
      }
  } else if (tolower(output.units) != tolower("uM")) stop("Currently can only return units of mg/L and uM")
  
  if(tolower(concentration)=='plasma'){
    if(tolower(model)=='1compartment'){
      max=as.numeric(max(out[,'Ccompartment']))
    }else{
      max=as.numeric(max(out[,'Cplasma']))
    }
    avg=as.numeric(out[dim(out)[1],'AUC'] - out[match(additional.days-1,out[,'time']),'AUC'])
  }else if(tolower(concentration)=='blood'){
    if(tolower(model)=='pbtk'){
      max=as.numeric(max(out[,'Cven']))
    }else if(tolower(model) == '3compartment'){
      max=as.numeric(max(out[,'Cplasma'] * parameters[['Rblood2plasma']]))
    }else{
      max=as.numeric(max(out[,'Ccompartment'] * parameters[['Rblood2plasma']]))
    }   
   avg=as.numeric((out[dim(out)[1],'AUC'] - out[match(additional.days-1,out[,'time']),'AUC'])*parameters[['Rblood2plasma']])
  }else stop("Only blood and plasma concentrations are calculated.")
  if(!suppress.messages){
    if(is.null(chem.cas) & is.null(chem.name)){
      cat(paste(toupper(substr(concentration,1,1)),substr(concentration,2,nchar(concentration)),sep=''),"concentrations returned in",output.units,"units.\n")
    }else cat(paste(toupper(substr(species,1,1)),substr(species,2,nchar(species)),sep=''),concentration,"concentrations returned in",output.units,"units.\n")
  }
  

  return(list(avg=avg,
    frac=frac_achieved, 
    max=max,
    the.day =as.numeric(css.day)))
}