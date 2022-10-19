#' Get Literature Oral Equivalent Dose
#' 
#' This function converts a chemical plasma concetration to an oral equivalent
#' dose using the values from the Wetmore et al. (2012) and (2013) publications
#' and other literature. 
#'
#' 
#' 
#' @param conc Bioactive in vitro concentration in units of specified
#' input.units, default of uM. 
#' @param chem.name Either the chemical name or the CAS number must be
#' specified. 
#' @param chem.cas Either the CAS number or the chemical name must be
#' specified. 
#' @param dtxsid EPA's 'DSSTox Structure ID (\url{https://comptox.epa.gov/dashboard})   
#' the chemical must be identified by either CAS, name, or DTXSIDs
#' @param input.units Units of given concentration, default of uM but can also
#' be mg/L.
#' @param output.units Units of dose, default of 'mg' for mg/kg BW/ day or
#' 'mol' for mol/ kg BW/ day.
#' @param suppress.messages Suppress output messages. 
#' @param which.quantile Which quantile from the SimCYP Monte Carlo simulation
#' is requested. Can be a vector.  Papers include 0.05, 0.5, and 0.95 for
#' humans and 0.5 for rats. 
#' @param species Species desired (either "Rat" or default "Human"). 
#' @param clearance.assay.conc Concentration of chemical used in measureing
#' intrinsic clearance data, 1 or 10 uM.
#' @param ... Additional parameters passed to get_lit_css.
#' @return Equivalent dose in specified units, default of mg/kg BW/day.
#' @author John Wambaugh
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
#' @keywords Literature Monte-Carlo
#' @examples
#' 
#' table <- NULL
#' for(this.cas in sample(get_lit_cheminfo(),50)) table <- rbind(table,cbind(
#' as.data.frame(this.cas),as.data.frame(get_lit_oral_equiv(conc=1,chem.cas=this.cas))))
#' 
#' 
#' 
#' 
#' get_lit_oral_equiv(0.1,chem.cas="34256-82-1")
#' 
#' get_lit_oral_equiv(0.1,chem.cas="34256-82-1",which.quantile=c(0.05,0.5,0.95))
#' 
#' @export get_lit_oral_equiv
get_lit_oral_equiv <- function(
                               conc,
                               chem.name=NULL,
                               chem.cas=NULL,
                               dtxsid=NULL,
                               suppress.messages=FALSE,
                               which.quantile=0.95,
                               species="Human",
                               input.units='uM',
                               output.units='mg',
                               clearance.assay.conc=NULL,
                               ...)
{
  Wetmore.data <- Wetmore.data
  if (is.null(chem.cas)){
    if(!is.null(chem.name)){
      chem.cas <- 
        get_chem_id(chem.name=chem.name)[['chem.cas']]
    }else if(!is.null(dtxsid)){
      chem.cas <- 
        get_chem_id(dtxsid=dtxsid)[['chem.cas']]
    }else{
      stop("Provide at least one chemical identifier either 'chem.cas',
           'chem.name', or 'dtxsid'.")
    }
  } 
  if (tolower(input.units) =='mg/l' | tolower(output.units) == 'mol') {
    MW <- get_physchem_param("MW",chem.cas=chem.cas)
  }   
  # if the user provided concentration is in 'mg/L' units,
  # then convert the concentration from 'mg/L' to 'uM'
  if(tolower(input.units) == 'mg/l'){
    # output units are in 'uM' for 'conc'
    conc <- conc * convert_units(
      input.units = input.units,
      output.units = 'um',
      chem.cas = chem.cas,
      chem.name = chem.name,
      dtxsid = dtxsid
    )
  } else if(tolower(input.units)!='um') stop(
           'Input units can only be in mg/L or uM.')
  if(is.null(clearance.assay.conc)){
    this.data <- subset(Wetmore.data,Wetmore.data[,"CAS"] == 
                        chem.cas&toupper(Wetmore.data[,"Species"]) ==
                        toupper(species))
    this.conc <- this.data[
      which.min(abs(this.data[,'Concentration..uM.'] - conc)),
      'Concentration..uM.']
  } else {
    this.conc <- clearance.assay.conc
  }
  # output units are 'uM/mg/kg/day' for 'Css'
  Css <- try(get_lit_css(
                         daily.dose=1,
                         chem.cas=chem.cas,
                         which.quantile=which.quantile,
                         species=species,
                         clearance.assay.conc=this.conc,
                         suppress.messages=TRUE,
                         output.units='uM',
                         ...))
  # output units are 'mg/kg/day'
  dose <- conc / Css # conc (uM) / Css (uM/mg/kg/day) = dose (mg/kg/day)
  
  # if the user wants the dose returned in 'umol/kg/day' units,
  # then convert the dose from 'mg/kg/day' to 'umol/kg/day'
  if (tolower(output.units) == 'umol') {
    # output units are 'umol/kg/day' for 'dose'
    dose <- dose * convert_units(
      input.units = 'mg',
      output.units = output.units,
      chem.cas = chem.cas,
      chem.name = chem.name,
      dtxsid = dtxsid
    )
  } else if(tolower(output.units) != 'mg') stop(
            "Output units can only be in mg or umol.")
  if (!suppress.messages)
  {
    cat(paste("Retrieving Css from literature based on ",
              this.conc,
              " uM intrinsic clearance data for the ",
              which.quantile,
              " quantile in ",
              species,".\n",
              sep=""))
    cat(paste(
      toupper(substr(species,1,1)),
      substr(species,2,nchar(species)),
      " ",
      input.units,
      " concentration converted to ",
      output.units,
      "/kg bw/day dose.\n",
      sep=""))
   }
   if (is(Css,"try-error")) {
     return(NA)
   } else {
# Converting Css for 1 mg/kg/day to dose needed to produce concentration conc uM:   
     return(set_httk_precision(dose))
   }
}

#' Get Literature Oral Equivalent Dose (deprecated).
#' 
#' This function is included for backward compatibility. It calls
#' \code{\link{get_lit_oral_equiv}} which
#' converts a chemical plasma concetration to an oral equivalent
#' dose using the values from the Wetmore et al. (2012) and (2013) publications
#' and other literature. 
#' 
#' 
#' @param conc Bioactive in vitro concentration in units of specified
#' input.units, default of uM. 
#' @param chem.name Either the chemical name or the CAS number must be
#' specified. 
#' @param chem.cas Either the CAS number or the chemical name must be
#' specified. 
#' @param input.units Units of given concentration, default of uM but can also
#' be mg/L.
#' @param output.units Units of dose, default of 'mg' for mg/kg BW/ day or
#' 'mol' for mol/ kg BW/ day.
#' @param suppress.messages Suppress output messages. 
#' @param which.quantile Which quantile from the SimCYP Monte Carlo simulation
#' is requested. Can be a vector.  Papers include 0.05, 0.5, and 0.95 for
#' humans and 0.5 for rats. 
#' @param species Species desired (either "Rat" or default "Human"). 
#' @param clearance.assay.conc Concentration of chemical used in measureing
#' intrinsic clearance data, 1 or 10 uM.
#' @param ... Additional parameters passed to get_lit_css.
#' @return Equivalent dose in specified units, default of mg/kg BW/day.
#' @author John Wambaugh
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
#' @keywords Literature Monte-Carlo
#' @examples
#' 
#' table <- NULL
#' for(this.cas in sample(get_lit_cheminfo(),50)) table <- rbind(table,cbind(
#' as.data.frame(this.cas),as.data.frame(get_lit_oral_equiv(conc=1,chem.cas=this.cas))))
#' 
#' 
#' 
#' 
#' get_lit_oral_equiv(0.1,chem.cas="34256-82-1")
#' 
#' get_lit_oral_equiv(0.1,chem.cas="34256-82-1",which.quantile=c(0.05,0.5,0.95))
#' 
#' @export get_wetmore_oral_equiv
get_wetmore_oral_equiv <- function(
                               conc,
                               chem.name=NULL,
                               chem.cas=NULL,
                               suppress.messages=FALSE,
                               which.quantile=0.95,
                               species="Human",
                               input.units='uM',
                               output.units='mg',
                               clearance.assay.conc=NULL,
                               ...)
{
  warning("Function \"get_wetmore_oral_equiv\" has been renamed to \"get_lit_oral_equiv\".")
  return(do.call(get_lit_oral_equiv, c(list(                        
                               conc=conc,
                               chem.name=chem.name,
                               chem.cas=chem.cas,
                               suppress.messages=suppress.messages,
                               which.quantile=which.quantile,
                               species=species,
                               input.units=input.units,
                               output.units=output.units,
                               clearance.assay.conc=clearance.assay.conc),
                               ...)))
}