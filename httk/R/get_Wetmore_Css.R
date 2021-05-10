#' Get literature Css
#' 
#' This function retrives a steady-state plasma concentration as a result of
#' infusion dosing from the Wetmore et al. (2012) and (2013) publications and
#' other literature. 
#' 
#' @param chem.name Either the chemical name or the CAS number must be
#' specified. 
#' @param chem.cas Either the cas number or the chemical name must be
#' specified. 
#' @param which.quantile Which quantile from the SimCYP Monte Carlo simulation
#' is requested. Can be a vector. 
#' @param species Species desired (either "Rat" or default "Human").
#' @param clearance.assay.conc Concentration of chemical used in measureing
#' intrinsic clearance data, 1 or 10 uM.
#' @param daily.dose Total daily dose infused in units of mg/kg BW/day.
#' Defaults to 1 mg/kg/day.  
#' @param output.units Returned units for function, defaults to mg/L but can
#' also be uM (specify units = "uM"). 
#' @param suppress.messages Whether or not the output message is suppressed.
#'
#' @return A numeric vector with the literature steady-state plasma 
#' concentration (1 mg/kg/day) for the requested quantiles
#'
#' @author John Wambaugh
#'
#' @references Wetmore, B.A., Wambaugh, J.F., Ferguson, S.S., Sochaski, M.A.,
#' Rotroff, D.M., Freeman, K., Clewell, H.J., Dix, D.H., Andersen, M.E., Houck,
#' K.A., Allen, B., Judson, R.S., Sing, R., Kavlock, R.J., Richard, A.M., and
#' Thomas, R.S., "Integration of Dosimetry, Exposure and High-Throughput
#' Screening Data in Chemical Toxicity Assessment," Toxicological Sciences 125
#' 157-174 (2012)
#' 
#' Wetmore, B.A., Wambaugh, J.F., Ferguson, S.S., Li, L., Clewell, H.J. III,
#' Judson, R.S., Freeman, K., Bao, W, Sochaski, M.A., Chu T.-M., Black, M.B.,
#' Healy, E, Allen, B., Andersen M.E., Wolfinger, R.D., and Thomas R.S., "The
#' Relative Impact of Incorporating Pharmacokinetics on Predicting in vivo
#' Hazard and Mode-of-Action from High-Throughput in vitro Toxicity Assays"
#' Toxicological Sciences, 132:327-346 (2013).
#' 
#' Wetmore, B. A., Wambaugh, J. F., Allen, B., Ferguson, S. S., Sochaski, M.
#' A., Setzer, R. W., Houck, K. A., Strope, C. L., Cantwell, K., Judson, R. S.,
#' LeCluyse, E., Clewell, H.J. III, Thomas, R.S., and Andersen, M. E. (2015).
#' "Incorporating High-Throughput Exposure Predictions with Dosimetry-Adjusted
#' In Vitro Bioactivity to Inform Chemical Toxicity Testing" Toxicological
#' Sciences, kfv171.
#'
#' @keywords Literature Monte-Carlo
#'
#' @examples
#' get_lit_css(chem.cas="34256-82-1")
#' 
#' get_lit_css(chem.cas="34256-82-1",species="Rat",which.quantile=0.5)
#' 
#' get_lit_css(chem.cas="80-05-7", daily.dose = 1,which.quantile = 0.5, output.units = "uM")
#' 
#' @export get_lit_css
get_lit_css <- function(chem.cas=NULL,chem.name=NULL,daily.dose=1,which.quantile=0.95,species="Human",clearance.assay.conc=NULL,output.units="mg/L",suppress.messages=FALSE)
{
  Wetmore.data <- Wetmore.data
  if (species == "Human") available.quantiles <- c(0.05,0.5, 0.95)
  else available.quantiles <- 0.5
  if (!all(which.quantile %in% available.quantiles)) stop("Literature only includes 5%, 50%, and 95% quantiles for human and 50% for rat.")
  if (!(tolower(output.units) %in% c("mg/l","um"))) stop("Literature only includes mg/L and uM values for Css")
  out <- get_chem_id(chem.cas=chem.cas,chem.name=chem.name)
  chem.cas <- out$chem.cas
  chem.name <- out$chem.name
    
  this.data <- subset(Wetmore.data,Wetmore.data[,"CAS"]==chem.cas&toupper(Wetmore.data[,"Species"])==toupper(species))
   
    if (!is.null(clearance.assay.conc)) 
    {
      this.data <- subset(this.data,this.data[,"Concentration..uM."]==clearance.assay.conc)[1,]
      if (dim(this.data)[1]!=1) stop(paste("No",clearance.assay.conc,"uM clearance assay data for",chem.name,"in",species))
    }else{
      if(1 %in% this.data[,"Concentration..uM."]){
        this.data <- subset(this.data,this.data[,"Concentration..uM."]== 1)[1,] 
        clearance.assay.conc <- 1
      }else{
        this.data <- this.data[1,]
        clearance.assay.conc <- this.data[,"Concentration..uM."][[1]]
      }
    }
    out <- NULL
    if (tolower(output.units)=="mg/l")
    {
      if (0.05 %in% which.quantile) out <- c(out,daily.dose*this.data[,"Css_lower_5th_perc.mg.L."])
      if (0.5 %in% which.quantile) out <- c(out,daily.dose*this.data[,"Css_median_perc.mg.L."])
      if (0.95 %in% which.quantile) out <- c(out,daily.dose*this.data[,"Css_upper_95th_perc.mg.L."])
    } else if(tolower(output.units) == 'um') {
      if (0.05 %in% which.quantile) out <- c(out,daily.dose*this.data[,"Css_lower_5th_perc.uM."])
      if (0.5 %in% which.quantile) out <- c(out,daily.dose*this.data[,"Css_median_perc.uM."])
      if (0.95 %in% which.quantile) out <- c(out,daily.dose*this.data[,"Css_upper_95th_perc.uM."])
    } else{
     stop('Output.units can only be uM or mg/L.')
    }
  
  if(!suppress.messages){
    cat(paste(toupper(substr(species,1,1)),substr(species,2,nchar(species)),sep=''),"plasma concentrations returned in",output.units,"units.\n")
    cat("Retrieving Css from literature based on ",clearance.assay.conc," uM intrinsic clearance data for the ",which.quantile," quantile in ",species,".\n")
  }
  return(out)
}
