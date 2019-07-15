#' Find the steady state concentration and the day it is reached.
#' 
#' This function finds the day a chemical comes within the specified range of
#' the analytical steady state venous blood or plasma concentration(from
#' calc_analytic_css) for the multiple compartment, three compartment, and one
#' compartment models, the fraction of the true steady state value reached on
#' that day, the maximum concentration, and the average concentration at the
#' end of the simulation.
#' 
#' 
#' @param chem.name Either the chemical name, CAS number, or parameters must be
#' specified. 
#' @param chem.cas Either the chemical name, CAS number, or parameters must be
#' specified. 
#' @param f Fractional distance from the final steady state concentration that
#' the average concentration must come within to be considered at steady state.
#' 
#' @param parameters Chemical parameters from parameterize_pbtk function,
#' overrides chem.name and chem.cas.
#' @param species Species desired (either "Rat", "Rabbit", "Dog", "Mouse", or
#' default "Human").
#' @param daily.dose Total daily dose, mg/kg BW.
#' @param doses.per.day Number of doses per day.
#' @param days Initial number of days to run simulation that is multiplied on
#' each iteration.
#' @param output.units Units for returned concentrations, defaults to uM
#' (specify units = "uM") but can also be mg/L.
#' @param concentration Desired concentration type, 'blood' or default
#' 'plasma'.
#' @param suppress.messages Whether or not to suppress messages.
#' @param model Model used in calculation, 'pbtk' for the multiple compartment
#' model,'3compartment' for the three compartment model, and '1compartment' for
#' the one compartment model.
#' @param default.to.human Substitutes missing animal values with human values
#' if true (hepatic intrinsic clearance or fraction of unbound plasma).
#' @param f.change Fractional change of daily steady state concentration
#' reached to stop calculating.
#' @param adjusted.Funbound.plasma Uses adjusted Funbound.plasma when set to
#' TRUE along with partition coefficients calculated with this value.
#' @param regression Whether or not to use the regressions in calculating
#' partition coefficients.
#' @param well.stirred.correction Uses correction in calculation of hepatic
#' clearance for well-stirred model if TRUE for model 1compartment elimination
#' rate.  This assumes clearance relative to amount unbound in whole blood
#' instead of plasma, but converted to use with plasma concentration.
#' @param restrictive.clearance Protein binding not taken into account (set to
#' 1) in liver clearance if FALSE.
#' @param ... Additional arguments passed to model solver (default of
#' solve_pbtk).
#' @return \item{frac}{Ratio of the mean concentration on the day steady state
#' is reached (baed on doses.per.day) to the analytical Css (based on infusion
#' dosing).} \item{max}{The maximum concentration of the simulation.}
#' \item{avg}{The average concentration on the final day of the simulation.}
#' \item{the.day}{The day the average concentration comes within 100 * p
#' percent of the true steady state concentration.}
#' @author Robert Pearce, John Wambaugh
#' @keywords Steady State
#' @examples
#' 
#' calc_css(chem.name='Bisphenol-A',doses.per.day=5,f=.001,output.units='mg/L')
#' \dontrun{
#' parms <- parameterize_3comp(chem.name='Bisphenol-A')
#' parms$Funbound.plasma <- .07
#' calc_css(parms,concentration='blood',model='3compartment')
#' 
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
#' 
#' days <- NULL
#' avg <- NULL
#' max <- NULL
#' for(this.cas in get_cheminfo()){
#' css.info <- calc_css(chem.cas = this.cas, doses.per.day = 1,suppress.messages=T)
#' days[[this.cas]] <- css.info[["the.day"]]
#' avg[[this.cas]] <- css.info[["avg"]]
#' max[[this.cas]] <- css.info[["max"]]
#' }
#' days.data <- as.data.frame(days)
#' hist <- ggplot(days.data, aes(days)) +
#' geom_histogram(fill = "blue", binwidth = 1/6) + scale_x_log10() +
#' ylab("Number of Chemicals") + xlab("Days") + theme(axis.text =
#' element_text(size = 16), axis.title = element_text(size = 16))
#' print(hist)
#' avg.max.data <- as.data.frame(cbind(avg, max))
#' avg.vs.max <- ggplot(avg.max.data, aes(avg, max)) + geom_point() +
#' geom_abline() + scale_x_log10() + scale_y_log10() +
#' xlab("Average Concentration at Steady State (uM)") +
#' ylab("Max Concentration at Steady State (uM)") +
#' theme(axis.text = element_text(size = 16),
#' axis.title = element_text(size = 16))
#' print(avg.vs.max)
#' }
#' 
#' @export calc_css
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
   frac_achieved <- as.numeric(max(out[,"Cplasma"])/css)  
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
