#' Find the steady state concentration and the day it is reached.
#' 
#' This function finds the day a chemical comes within the specified range of
#' the analytical steady state venous blood or plasma concentration(from
#' calc_analytic_css) for the multiple compartment, three compartment, and one
#' compartment models, the fraction of the true steady state value reached on
#' that day, the maximum concentration, and the average concentration at the
#' end of the simulation.
#' 
#' @param chem.name Either the chemical name, CAS number, or parameters must be
#' specified. 
#' @param chem.cas Either the chemical name, CAS number, or parameters must be
#' specified. 
#' @param dtxsid EPA's DSSTox Structure ID (\url{https://comptox.epa.gov/dashboard})   
#' the chemical must be identified by either CAS, name, or DTXSIDs
#' @param parameters Chemical parameters from parameterize_pbtk function,
#' overrides chem.name and chem.cas.
#' @param species Species desired (either "Rat", "Rabbit", "Dog", "Mouse", or
#' default "Human").
#' @param f Fractional distance from the final steady state concentration that
#' the average concentration must come within to be considered at steady state.
#' @param daily.dose Total daily dose, mg/kg BW.
#' @param doses.per.day Number of doses per day.
#' @param days Initial number of days to run simulation that is multiplied on
#' each iteration.
#' @param output.units Units for returned concentrations, defaults to uM
#' (specify units = "uM") but can also be mg/L.
#' @param suppress.messages Whether or not to suppress messages.
#' @param tissue Desired tissue concentration (defaults to whole body 
#' concentration.)
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
#' @param dosing The dosing object for more complicated scenarios. Defaults to
#' repeated \code{daily.dose} spread out over \code{doses.per.day}
#' @param ... Additional arguments passed to model solver (default of
#' \code{\link{solve_pbtk}}).
#'
#' @return \item{frac}{Ratio of the mean concentration on the day steady state
#' is reached (baed on doses.per.day) to the analytical Css (based on infusion
#' dosing).} \item{max}{The maximum concentration of the simulation.}
#' \item{avg}{The average concentration on the final day of the simulation.}
#' \item{the.day}{The day the average concentration comes within 100 * p
#' percent of the true steady state concentration.}
#'
#' @author Robert Pearce, John Wambaugh
#'
#' @keywords Steady-State
#'
#' @examples
#' 
#' calc_css(chem.name='Bisphenol-A',doses.per.day=5,f=.001,output.units='mg/L')
#' 
#' parms <- parameterize_3comp(chem.name='Bisphenol-A')
#' parms$Funbound.plasma <- .07
#' calc_css(parameters=parms,model='3compartment')
#' 
#' out <- solve_pbtk(chem.name = "Bisphenol A",
#'   days = 50,                                   
#'   daily.dose=1,
#'   doses.per.day = 3)
#' plot.data <- as.data.frame(out)
#' 
#' css <- calc_analytic_css(chem.name = "Bisphenol A")
#' library("ggplot2")
#' c.vs.t <- ggplot(plot.data,aes(time, Cplasma)) + geom_line() +
#' geom_hline(yintercept = css) + ylab("Plasma Concentration (uM)") +
#' xlab("Day") + theme(axis.text = element_text(size = 16), axis.title =
#' element_text(size = 16), plot.title = element_text(size = 17)) +
#' ggtitle("Bisphenol A")
#'
#' print(c.vs.t)
#' 
#' @export calc_css
calc_css <- function(chem.name=NULL,
                    chem.cas=NULL, 
                    dtxsid=NULL,
                    parameters=NULL,
                    species='Human',
                    f = .01,
                    daily.dose=1,
                    doses.per.day=3,
                    days = 21,
                    output.units = "uM",
                    suppress.messages=FALSE,
                    tissue="plasma",
                    model='pbtk',
                    default.to.human=FALSE,
                    f.change = 0.00001,
                    adjusted.Funbound.plasma=TRUE,
                    regression=TRUE,
                    well.stirred.correction=TRUE,
                    restrictive.clearance=TRUE,
                    dosing=NULL,
                    ...)
{
  # We need to describe the chemical to be simulated one way or another:
  if (is.null(chem.cas) & 
      is.null(chem.name) & 
      is.null(dtxsid) &
      is.null(parameters)) 
    stop('Parameters, chem.name, chem.cas, or dtxsid must be specified.')
  
# We need to know model-specific information (from modelinfo_[MODEL].R]) 
# to set up the solver:
  if (is.null(model)) stop("Model must be specified.")
  model <- tolower(model)
  if (!(model %in% names(model.list)))            
  {
    stop(paste("Model",model,"not available. Please select from:",
      paste(names(model.list),collapse=", ")))
  } else {
    #Set more convenient names for various model-related variables (e.g. route
    #of exposure) stored in the list of lists, model.list, compiled in the
    #various models' associated "modelinfo_xxx.R" files:
    #
    # name of function that generates the model parameters:
    parameterize_function <- model.list[[model]]$parameterize.func
    # the names of the state variables of the model (so far, always in units of 
    # amounts)
    state.vars <- model.list[[model]]$default.monitor.vars
    state.vars <- state.vars[!(state.vars %in% c(
      "Atubules",
      "Ametabolized",
      "AUC",
      "Cplasma"))]
  }   

  # We only want to call the parameterize function once:
  if (is.null(parameters))
  {
    parameters <- do.call(parameterize_function,list(
      chem.cas=chem.cas,
      chem.name=chem.name,
      dtxsid=dtxsid,
      species=species,
      default.to.human=default.to.human,
      suppress.messages=suppress.messages,
      adjusted.Funbound.plasma=adjusted.Funbound.plasma,
      regression=regression)) 
  }

  if (is.null(dosing))
  {
    dosing <- list(
      initial.dose=0,
      dosing.matrix=NULL,
      daily.dose=daily.dose,
      doses.per.day=doses.per.day
    )
  }
  
  # We need to find out what concentrations (roughly) we should reach before
  # stopping:
  css <- calc_analytic_css(
    parameters=parameters,
    daily.dose=daily.dose,
    concentration='plasma',
    model=model,
    output.units = output.units,
    suppress.messages=TRUE,
    adjusted.Funbound.plasma=adjusted.Funbound.plasma,
    regression=regression,
    well.stirred.correction=well.stirred.correction,
    restrictive.clearance=restrictive.clearance) 
  target.conc <- (1 - f) * css 

  # Initially simulate for a time period of length "days":
  out <- solve_model(parameters=parameters,
    model=model, 
    dosing=dosing,
    suppress.messages=TRUE,
    days=days,
    output.units = output.units,
    restrictive.clearance=restrictive.clearance,
    ...)
  Final_Conc <- out[dim(out)[1],state.vars]
  total.days <- days
  additional.days <- days

  # For the 3-compartment model:  
  colnames(out)[colnames(out)=="Csyscomp"]<-"Cplasma"

  target <- paste("C",tissue,sep="") 
  if (!(target %in% colnames(out))) stop(paste(
    "Requested tissue",tissue,"is not an output of model",model))
    
  while(all(out[,"Cplasma"] < target.conc) & 
       ((out[match((additional.days - 1),out[,'time']),target]-
        out[match((additional.days - 2),out[,'time']),target])/
        out[match((additional.days - 2),out[,'time']),target] > f.change))
  {
    if(additional.days < 1000)
    {
      additional.days <- additional.days * 5
    }#else{
    #  additional.days <- additional.days * 3
    #}
    total.days <- total.days + additional.days
    
    out <- solve_model(parameters=parameters,
      model=model,
      initial.values = Final_Conc,  
      dosing=dosing,
      days = additional.days,
      suppress.messages=TRUE,
      restrictive.clearance=restrictive.clearance,
      ...)
    Final_Conc <- out[dim(out)[1],state.vars]
  
    if(total.days > 36500) break 
  }
  
# Calculate the day Css is reached:
  if (total.days < 36500)
  {
    # The day the simulation started:
    sim.start.day <- total.days - additional.days
    # The day the current simulation reached Css:
    if(any(out[,target] >= target.conc))
    {
      sim.css.day <- floor(min(out[out[,target]>=target.conc,"time"]))
    } else {
      sim.css.day <- additional.days
    }
    # The overall day the simulation reached Css:
    css.day <- sim.start.day+sim.css.day
    # Fraction of analytic Css achieved:
    last.day.subset<-subset(out,out[,"time"]<(sim.css.day+1) &
                                out[,"time"]>=(sim.css.day))
    frac_achieved <- as.numeric(mean(last.day.subset[,target])/css)
  } else{ 
   if(!suppress.messages)cat("Analytic css not reached after 100 years.")
   css.day  <- 36500
   frac_achieved <- as.numeric(max(out[,target])/css)  
  }     
  
  max.conc <- as.numeric(max(out[,target]))
  avg.conc <- as.numeric(out[dim(out)[1],'AUC'] - out[match(additional.days-1,out[,'time']),'AUC']) 
   
  return(list(
    avg=set_httk_precision(avg.conc),
    frac=set_httk_precision(frac_achieved), 
    max=set_httk_precision(max.conc),
    the.day =set_httk_precision(as.numeric(css.day))))
}
