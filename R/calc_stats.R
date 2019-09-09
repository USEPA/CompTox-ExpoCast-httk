#' Calculate the statistics.
#' 
#' This function calculates the area under the curve, the mean, and the peak values
#' for the venous blood or plasma concentration of a specified chemical or all
#' chemicals if none is specified for the multiple compartment model with a
#' given number of days, dose, and number of doses per day.
#' 
#' Default value of 0 for doses.per.day solves for a single dose.
#' 
#' When species is specified as rabbit, dog, or mouse, the function uses the
#' appropriate physiological data(volumes and flows) but substitues human
#' fraction unbound, partition coefficients, and intrinsic hepatic clearance.
#' 
#' 
#' @param days Length of the simulation.
#' @param chem.name Name of desired chemical.
#' @param chem.cas CAS number of desired chemical.
#' @param parameters Chemical parameters from parameterize_pbtk function,
#' overrides chem.name and chem.cas.
#' @param stats Desired values (either 'AUC', 'mean', 'peak', or a vector
#' containing any combination).
#' @param daily.dose Total daily dose, mg/kg BW.
#' @param dose Amount of a single dose, mg/kg BW.  Overwrites daily.dose.
#' @param species Species desired (either "Rat", "Rabbit", "Dog", "Mouse", or
#' default "Human").
#' @param exclude.fup.zero Whether or not to exclude chemicals with a fraction
#' of unbound plasma equal to zero or include them with a value of 0.005, only
#' used when chem.name, chem.cas, and parameters are not specified.
#' @param doses.per.day Number of doses per day.
#' @param output.units Desired units (either "mg/L", "mg", "umol", or default
#' "uM").
#' @param model Model used in calculation, 'pbtk' for the multiple compartment
#' model,'3compartment' for the three compartment model, '3compartmentss' for
#' the three compartment steady state model, and '1compartment' for one
#' compartment model.
#' @param concentration Desired concentration type, 'blood' or default
#' 'plasma'.
#' @param default.to.human Substitutes missing animal values with human values
#' if true (hepatic intrinsic clearance or fraction of unbound plasma).
#' @param suppress.messages Whether to suppress output message.
#' @param ... Arguments passed to solve function.
#' @return \item{AUC}{Area under the plasma concentration curve.}
#' \item{mean}{The area under the curve divided by the number of days.}
#' \item{peak}{The highest concentration.}
#' @author John Wambaugh and Robert Pearce
#' @keywords Solve Statistics
#' @examples
#' 
#' calc_stats(chem.name='Bisphenol-A',days=100,stats='mean',model='3compartment')
#' calc_stats(chem.name='Bisphenol-A',days=100,stats=c('peak','mean'),species='Rat')
#' \dontrun{
#' all.peak.stats <- calc_stats(days=10, doses.per.day = 3, stats = "peak")
#' }
#' triclosan.stats <- calc_stats(days=10, chem.name = "triclosan")
#' 
#' @export calc_stats
calc_stats <-function(days,chem.name=NULL,chem.cas=NULL,parameters=NULL,stats=c("AUC","peak","mean"),species='Human',exclude.fup.zero=F,daily.dose=1,dose=NULL,doses.per.day=NULL,output.units='uM',concentration='plasma',model='pbtk',default.to.human=F,suppress.messages=F,...)
{
  AUC <- NULL
  peak <- NULL
  mean <- NULL
  out <- NULL
  
  if(is.null(chem.name) & is.null(chem.cas) & is.null(parameters)){
    for(this.CAS in get_cheminfo(species=species,model=model)){
      stat <- calc_chem_stats(chem.cas=this.CAS,days=days,stats=stats,species=species,dose=dose,daily.dose=daily.dose,doses.per.day=doses.per.day,concentration=concentration,output.units=output.units,model=model,default.to.human=default.to.human,suppress.messages=T,...)

      if(length(stat)==1){
        out[this.CAS] <-  stat 
      }else{
        AUC[this.CAS] <- stat[["AUC"]]
        peak[this.CAS] <- stat[["peak"]] 
        mean[this.CAS] <- stat[["mean"]] 
      }
    }
    if(length(stat)!=1){
      if(!is.null(AUC) & !is.null(peak) & is.null(mean)) out <- list(AUC=AUC,peak=peak)
      else if(!is.null(AUC) & is.null(peak) & !is.null(mean)) out <- list(AUC=AUC,mean=mean)
      else if(is.null(AUC) & !is.null(peak) & !is.null(mean)) out <- list(mean=mean,peak=peak)
      else out <- list(AUC=AUC,peak=peak,mean=mean)
    }
    if(!suppress.messages){
      cat(paste(toupper(substr(species,1,1)),substr(species,2,nchar(species)),sep=''),concentration,"concentrations returned in",output.units,"units.\n")
      if('auc' %in% tolower(stats)) cat("AUC is area under plasma concentration curve in",output.units,"* days units.\n")
    }
  }else{
    out <- calc_chem_stats(chem.name=chem.name,chem.cas=chem.cas,parameters=parameters,days=days,stats=stats,species=species,daily.dose=daily.dose,dose=dose,doses.per.day=doses.per.day,concentration=concentration,output.units=output.units,model=model,default.to.human=default.to.human,suppress.messages=suppress.messages,...)
  } 
  return(out)
}
