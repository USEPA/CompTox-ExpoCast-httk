#' Calculate toxicokinetic summary statistics.
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
#' @param dtxsid EPA's DSSTox Structure ID (\url{https://comptox.epa.gov/dashboard})  
#' the chemical must be identified by either CAS, name, or DTXSIDs
#' @param parameters Chemical parameters from parameterize_pbtk function,
#' overrides chem.name and chem.cas.
#' @param route String specification of route of exposure for simulation:
#' "oral", "iv", "inhalation", ...
#' @param stats Desired values (either 'AUC', 'mean', 'peak', or a vector
#' containing any combination).
#' @param daily.dose Total daily dose, mg/kg BW.
#' @param dose Amount of a single dose at time zero, mg/kg BW. 
#' @param species Species desired (either "Rat", "Rabbit", "Dog", "Mouse", or
#' default "Human").
#' @param doses.per.day Number of doses per day.
#' @param output.units Desired units (either "mg/L", "mg", "umol", or default
#' "uM").
#' @param model Model used in calculation, 'pbtk' for the multiple compartment
#' model,'3compartment' for the three compartment model, '3compartmentss' for
#' the three compartment steady state model, and '1compartment' for one
#' compartment model.
#' @param concentration Desired concentration type, 'blood' or default
#' 'plasma'.
#' @param tissue Desired steady state tissue conentration.
#' @param default.to.human Substitutes missing animal values with human values
#' if true (hepatic intrinsic clearance or fraction of unbound plasma).
#' @param adjusted.Funbound.plasma Uses adjusted Funbound.plasma when set to
#' TRUE along with partition coefficients calculated with this value.
#' @param regression Whether or not to use the regressions in calculating
#' partition coefficients.
#' @param restrictive.clearance Protein binding not taken into account (set to
#' 1) in liver clearance if FALSE.
#' @param ... Additional arguments passed to the integrator.
#' @param suppress.messages Whether to suppress output message.
#' @param ... Arguments passed to solve function.
#' @return \item{AUC}{Area under the plasma concentration curve.}
#' \item{mean.conc}{The area under the curve divided by the number of days.}
#' \item{peak.conc}{The highest concentration.}
#' @author Robert Pearce and John Wambaugh 
#' @keywords Solve Statistics
#' @examples
#' 
#' calc_tkstats(chem.name='Bisphenol-A',days=100,stats='mean',model='3compartment')
#' 
#' \donttest{
#' calc_tkstats(chem.name='Bisphenol-A',days=100,stats=c('peak','mean'),species='Rat')
#' 
#' triclosan.stats <- calc_tkstats(days=10, chem.name = "triclosan")
#' }
#' 
#' @export calc_tkstats
calc_tkstats <-function(
               chem.name=NULL,
               chem.cas=NULL,
               dtxsid=NULL,
               parameters=NULL,
               route="oral",
               stats=c("AUC","peak","mean"),
               species='Human',
               days=28,
               daily.dose=1,
               dose=NULL,
               doses.per.day=1,
               output.units='uM',
               concentration='plasma',
               tissue='plasma',
               model='pbtk',
               default.to.human=FALSE,
               adjusted.Funbound.plasma=TRUE,
               regression=TRUE,
               restrictive.clearance = T,
               suppress.messages=FALSE,
               ...)
{
### ERROR CHECKING

# Check that this model is available in this distribution of HTTK:
  if (!(model %in% names(model.list)))            
    stop(paste("Model",model,"not available. Please select from:",
      paste(names(model.list),collapse=", ")))

# Currently we only calculate three stats:  
  valid.stats <- c("AUC","mean","peak")
  if (any(!(tolower(stats) %in% tolower(valid.stats))))
     stop(paste("calc_tkstats cannot calculate",
     stats[!(stats %in% valid.stats)],
     ". Valid stats are:",
     paste(valid.stats,collapse=" "),"."))

# Stats for all chemicals in HTTK:  
  if(is.null(chem.name) & 
     is.null(chem.cas) & 
     is.null(dtxsid) & 
     is.null(parameters))
  {
    AUC <- NULL
    peak.conc <- NULL
    mean.conc <- NULL
    out <- NULL
    for (this.CAS in sort(get_cheminfo(species=species,
                      model=model)))
    {
      cat(paste(this.CAS,"\n"))
      stat <- calc_tkstats(chem.cas=this.CAS,
                days=days,
                stats=stats,
                species=species,
                dose=dose,
                daily.dose=daily.dose,
                doses.per.day=doses.per.day,
                concentration=concentration,
                output.units=output.units,
                model=model,
                default.to.human=default.to.human,
                suppress.messages=TRUE,
                ...)

      if (length(stat)==1)
      {
        out[this.CAS] <-  stat 
      } else {
        AUC[this.CAS] <- stat[["AUC"]]
        peak.conc[this.CAS] <- stat[["peak.conc"]] 
        mean.conc[this.CAS] <- stat[["mean.conc"]] 
      }
    }
    if (length(stat)!=1)
    {
      out <- list()
      if (!is.null(AUC)) out[["AUC"]] <- AUC
      if (!is.null(peak.conc)) out[["peak.conc"]] <- peak.conc
      if (!is.null(mean.conc)) out[["mean.conc"]] <- mean.conc
    }
    if (!suppress.messages)
    {
      cat(paste(toupper(substr(species,1,1)),
            substr(species,2,nchar(species)),sep=''),
        concentration,"concentrations returned in",output.units,"units.\n")
      if ('auc' %in% tolower(stats)) 
        cat("AUC is area under plasma concentration curve in",
          output.units, "* days units.\n")
    }
# Stats for a particular chemical:    
  } else {
    dosing <- list(
        initial.dose=dose,
        dosing.matrix=NULL,
        daily.dose=daily.dose,
        doses.per.day=doses.per.day)
        
    PKtimecourse <- solve_model(
                      chem.name=chem.name,
                      chem.cas=chem.cas,
                      dtxsid=dtxsid,
                      parameters=parameters,
                      model=model,
                      route=route,
                      days = days,
                      species=species,
                      dosing=dosing,
                      suppress.messages=TRUE,
                      output.units=output.units,
                      ...)
    
    # For the 3-compartment model:  
    colnames(PKtimecourse)[colnames(PKtimecourse)=="Csyscomp"]<-"Cplasma"
      
    out <- list()
  
    if (any(c("mean","peak") %in% tolower(stats)))
    {
      # Which column do we want peak and mean from:
      tissue <- paste("C",tolower(tissue),sep="")
      if (!(tissue %in% colnames(PKtimecourse)))
        stop(tissue,"is not a column output by model",model)
    }
  
    if ("auc" %in% tolower (stats) &
      !("AUC" %in% colnames(PKtimecourse)))
      stop("AUC is not a column output by model",model)
    
    # If mean is requested, calculate it last in case AUC is also requested:
    if ("mean" %in% stats) stats <- c(stats[stats!="mean"],"mean")
    for (this.stat in stats)
    {
      if (tolower(this.stat) == "auc") 
        out[["AUC"]] <- set_httk_precision(as.numeric(
          PKtimecourse[dim(PKtimecourse)[1],'AUC']))
      if (tolower(this.stat) == "peak") 
        out[["peak"]] <- calc_timecourse_peak(PKtimecourse[,c("time",tissue)])
      if (tolower(this.stat) == "mean")
      {
        if (!is.null(out[["AUC"]])) out[["mean"]] <- signif(out[["AUC"]]/days,4)
        else out[["mean"]] <- 
          set_httk_precision(as.numeric(PKtimecourse[dim(PKtimecourse)[1],'AUC']/days)) 
      }
    }
  
    # We need to have the blood:plasma ratio:
    if (is.null(parameters[['Rblood2plasma']]))
    {
      parameters[['Rblood2plasma']] <- available_rblood2plasma(
        chem.name=chem.name,
        chem.cas=chem.cas,
        dtxsid=dtxsid,
        species=species,
        adjusted.Funbound.plasma=adjusted.Funbound.plasma,
        suppress.messages=TRUE)
    }
  
    # Blood or plasma concentration:
    if (tolower(concentration)=='blood')
    {
      if (length(out) == 1)
      {
        out <- out * parameters[['Rblood2plasma']]
      } else {
        for (this.stat in stats)
        {
          out[[this.stat]] <- set_httk_precision(out[[this.stat]] *  
            parameters[['Rblood2plasma']])
        }
      }
    } else if (tolower(concentration) != 'plasma') { 
      stop("Only blood and plasma concentrations are calculated.")
    }
  
    if (!suppress.messages)
    {
      if (is.null(chem.cas) & is.null(chem.name))
      {
        cat(paste(toupper(substr(concentration,1,1)),
          substr(concentration,2,nchar(concentration)),sep=''),
          "values returned in",output.units,"units.\n")
      } else {
        cat(paste(toupper(substr(species,1,1)),
          substr(species,2,nchar(species)),sep=''),
          concentration,"concentrations returned in",output.units,"units.\n")
      }
      if ('AUC' %in% stats) cat("AUC is area under plasma concentration curve in",
        output.units,"* days units with Rblood2plasma =",
        parameters[['Rblood2plasma']],".\n")
    }    
  }
  
  # If only one stat was asked for, don't return a list, return just the first entry in the list:
  if (length(out) == 1) out <- out[[1]]
  
  return(out)
}

#' Calculate toxicokinetic summary statistics (deprecated).
#' 
#' #' This function is included for backward compatibility. It calls
#' \code{\link{calc_tkstats}} which 
#' calculates the area under the curve, the mean, and the peak values
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
#' @param dtxsid EPA's DSSTox Structure ID (\url{https://comptox.epa.gov/dashboard})  
#' the chemical must be identified by either CAS, name, or DTXSIDs
#' @param parameters Chemical parameters from parameterize_pbtk function,
#' overrides chem.name and chem.cas.
#' @param route String specification of route of exposure for simulation:
#' "oral", "iv", "inhalation", ...
#' @param stats Desired values (either 'AUC', 'mean', 'peak', or a vector
#' containing any combination).
#' @param daily.dose Total daily dose, mg/kg BW.
#' @param dose Amount of a single dose at time zero, mg/kg BW. 
#' @param species Species desired (either "Rat", "Rabbit", "Dog", "Mouse", or
#' default "Human").
#' @param doses.per.day Number of doses per day.
#' @param output.units Desired units (either "mg/L", "mg", "umol", or default
#' "uM").
#' @param model Model used in calculation, 'pbtk' for the multiple compartment
#' model,'3compartment' for the three compartment model, '3compartmentss' for
#' the three compartment steady state model, and '1compartment' for one
#' compartment model.
#' @param concentration Desired concentration type, 'blood' or default
#' 'plasma'.
#' @param tissue Desired steady state tissue conentration.
#' @param default.to.human Substitutes missing animal values with human values
#' if true (hepatic intrinsic clearance or fraction of unbound plasma).
#' @param adjusted.Funbound.plasma Uses adjusted Funbound.plasma when set to
#' TRUE along with partition coefficients calculated with this value.
#' @param regression Whether or not to use the regressions in calculating
#' partition coefficients.
#' @param restrictive.clearance Protein binding not taken into account (set to
#' 1) in liver clearance if FALSE.
#' @param ... Additional arguments passed to the integrator.
#' @param suppress.messages Whether to suppress output message.
#' @param ... Arguments passed to solve function.
#' @return \item{AUC}{Area under the plasma concentration curve.}
#' \item{mean.conc}{The area under the curve divided by the number of days.}
#' \item{peak.conc}{The highest concentration.}
#'
#' @author Robert Pearce and John Wambaugh 
#'
#' @keywords Solve Statistics
#' 
#' @export calc_stats
calc_stats <-function(
               chem.name=NULL,
               chem.cas=NULL,
               dtxsid=NULL,
               parameters=NULL,
               route="oral",
               stats=c("AUC","peak","mean"),
               species='Human',
               days=28,
               daily.dose=1,
               dose=NULL,
               doses.per.day=1,
               output.units='uM',
               concentration='plasma',
               tissue='plasma',
               model='pbtk',
               default.to.human=FALSE,
               adjusted.Funbound.plasma=TRUE,
               regression=TRUE,
               restrictive.clearance = T,
               suppress.messages=FALSE,
               ...)
{
  warning("Function \"calc_stats\" has been renamed to \"calc_tkstats\".")
  return(calc_tkstats(
               chem.name=chem.name,
               chem.cas=chem.cas,
               dtxsid=dtxsid,
               parameters=parameters,
               route=route,
               stats=stats,
               species=species,
               days=days,
               daily.dose=daily.dose,
               dose=dose,
               doses.per.day=doses.per.day,
               output.units=output.units,
               concentration=concentration,
               tissue=tissue,
               model=model,
               default.to.human=default.to.human,
               adjusted.Funbound.plasma=adjusted.Funbound.plasma,
               regression=regression,
               restrictive.clearance = restrictive.clearance,
               suppress.messages=suppress.messages,
               ...))
}