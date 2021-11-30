#'Calculate the analytic steady state concentration for model gas pbpk.
#'
#'This function calculates the analytic steady state plasma and venous blood 
#'concentrations.
#'
#'@param chem.name Either the chemical name, CAS number, or the parameters must 
#' be specified.
#'@param chem.cas Either the chemical name, CAS number, or the parameters must 
#' be specified.
#' @param dtxsid EPA's 'DSSTox Structure ID (\url{http://comptox.epa.gov/dashboard})    
#' the chemical must be identified by either CAS, name, or DTXSIDs
#'@param parameters Chemical parameters from parameterize_gas_pbtk (for model = 
#' 'gas_pbtk'), parameterize_pbtk (for model = 
#' 'pbtk'), parameterize_3comp (for model = '3compartment), 
#' parmeterize_1comp(for model = '1compartment') or parameterize_steadystate 
#' (for model = '3compartmentss'), overrides chem.name and chem.cas.
#' @param exp.conc Specified inhalation exposure concentration for use in assembling
#' 'forcings' data series argument for integrator. Defaults to uM/L 
#' @param period For use in assembling forcing function data series 'forcings'
#' argument, specified in hours
#' @param exp.duration For use in assembling forcing function data 
#' series 'forcings' argument, specified in hours
#'@param concentration Desired concentration type, 'venous', or default 'plasma'.
#'@param suppress.messages Whether or not the output message is suppressed.
#'@param recalc.blood2plasma Recalculates the ratio of the amount of chemical 
#' in the blood to plasma using the input parameters. Use this if you have 
#''altered hematocrit, Funbound.plasma, or Krbc2pu.
#'@param tissue Desired tissue conentration (defaults to whole body 
#' concentration.)
#'@param restrictive.clearance If TRUE (default), then only the fraction of
#' chemical not bound to protein is available for metabolism in the liver. If 
#' FALSE, then all chemical in the liver is metabolized (faster metabolism due
#' to rapid off-binding). 
#'@param bioactive.free.invivo If FALSE (default), then the total concentration is treated
#' as bioactive in vivo. If TRUE, the the unbound (free) plasma concentration is treated as 
#' bioactive in vivo. Only works with tissue = NULL in current implementation.
#' @param vmax Michaelis-Menten vmax value in reactions/min. (Default is 0.)
#' @param km Michaelis-Menten concentration of half-maximal reaction velocity
#' in desired output concentration units. (Default is 1.)
#'@param ... Additional parameters passed to parameterize function if 
#' parameters is NULL.
#'  
#'@return Steady state concentration in uM units
#'
#'@author Miyuki Breen
#'@keywords gas_pbtk
calc_analytic_css_gas <- function(chem.name=NULL,
                                   chem.cas = NULL,
                                   dtxsid = NULL,
                                   parameters=NULL,
                                   exp.conc = 1, #default exposure concentration for forcing data series
                                   period = 24,
                                   exp.duration = 24,
                                   concentration='plasma',
                                   suppress.messages=FALSE,
                                   recalc.blood2plasma=FALSE,
                                   tissue=NULL,
                                   restrictive.clearance=TRUE,
                                   bioactive.free.invivo = FALSE,
                                   vmax = 0,
                                   km = 1,
                                   ...)
{
  #R CMD CHECK throws notes about "no visible binding for global variable", for
  #each time a data.table column name is used without quotes. To appease R CMD
  #CHECK, a variable has to be created for each of these column names and set to
  #NULL. Note that within the data.table, these variables will not be NULL! Yes,
  #this is pointless and annoying.
  # dose <- NULL
  #End R CMD CHECK appeasement.
  
  param.names.gas <- model.list[["gas_pbtk"]]$param.names
  param.names.schmitt <- model.list[["schmitt"]]$param.names
    
# We need to describe the chemical to be simulated one way or another:
  if (is.null(chem.cas) & 
      is.null(chem.name) & 
      is.null(dtxsid) &
      is.null(parameters)) 
    stop('parameters, chem.name, chem.cas, or dtxsid must be specified.')

# Look up the chemical name/CAS, depending on what was provide:
  if (is.null(parameters))
  {
    out <- get_chem_id(
            chem.cas=chem.cas,
            chem.name=chem.name,
            dtxsid=dtxsid)
    chem.cas <- out$chem.cas
    chem.name <- out$chem.name                                
    dtxsid <- out$dtxsid  

    parameters <- parameterize_gas_pbtk(chem.cas=chem.cas,
                                    chem.name=chem.name,
                                    suppress.messages=suppress.messages,
                                    ...) 
    if (recalc.blood2plasma) 
    {
      warning("Argument recalc.blood2plasma=TRUE ignored because parameters is NULL.")
    }
  } else {
    if (!all(param.names.gas %in% names(parameters)))
    {
      stop(paste("Missing parameters:",
           paste(param.names.gas[which(!param.names.gas %in% names(parameters))],
             collapse=', '),
           ".  Use parameters from parameterize_gas_pbtk.")) 
    }
    if (recalc.blood2plasma) {
      parameters[['Rblood2plasma']] <- 1 - 
        parameters[['hematocrit']] + 
        parameters[['hematocrit']] * parameters[['Krbc2pu']] * parameters[['Funbound.plasma']]
    }
  }
  
  Qcardiac <-  parameters[["Qcardiacc"]]/parameters[['BW']]^0.25
  Qgfr <-  parameters[["Qgfrc"]]/parameters[['BW']]^0.25
  Clmetabolism <-  parameters[["Clmetabolismc"]]
  Kliver2pu <- parameters[['Kliver2pu']]
  kUrt <- parameters[['kUrtc']]/parameters[['BW']]^0.25
  Kblood2air <- parameters[['Kblood2air']]
  
  Qgut <- parameters[["Qgutf"]] * Qcardiac
  Qliver <- parameters[["Qliverf"]] * Qcardiac
  Qkidney <- parameters[['Qkidneyf']] * Qcardiac
  Qlung <- parameters[['Qlungf']] * Qcardiac
  Qrest <- Qcardiac-Qgut-Qliver-Qkidney-Qlung
  Qalv <- parameters[['Qalvc']]/parameters[['BW']]^0.25
  Rblood2plasma <- parameters[['Rblood2plasma']]
  fup <- parameters[["Funbound.plasma"]]
    
  if (!restrictive.clearance) Clmetabolism <- Clmetabolism / fup
  
  # set Cinh
  Cinh <- exp.conc * (exp.duration / period)
  
  # calculate Cven_ss
  Css <- Cinh*Kblood2air*(kUrt*Qgfr*Qgut^2*fup + 2*kUrt*Qgfr*Qgut*Qliver*fup
                        + kUrt*Qgfr*Qgut*Qlung*fup + kUrt*Qgfr*Qgut*Qrest*fup
                        + kUrt*Qgfr*Qliver^2*fup + kUrt*Qgfr*Qliver*Qlung*fup
                        + kUrt*Qgfr*Qliver*Qrest*fup + kUrt*Qgfr*Qlung*fup*Clmetabolism
                        + kUrt*Qgfr*Qrest*fup*Clmetabolism + kUrt*Qgut^2*Qkidney*Rblood2plasma
                        + kUrt*Qgut*Qkidney^2*Rblood2plasma + 2*kUrt*Qgut*Qkidney*Qliver*Rblood2plasma
                        + kUrt*Qgut*Qkidney*Qlung*Rblood2plasma + kUrt*Qgut*Qkidney*Qrest*Rblood2plasma
                        + kUrt*Qkidney^2*Qliver*Rblood2plasma + kUrt*Qkidney^2*Clmetabolism*Rblood2plasma
                        + kUrt*Qkidney*Qliver^2*Rblood2plasma + kUrt*Qkidney*Qliver*Qlung*Rblood2plasma
                        + kUrt*Qkidney*Qliver*Qrest*Rblood2plasma + kUrt*Qkidney*Qlung*Clmetabolism*Rblood2plasma
                        + kUrt*Qkidney*Qrest*Clmetabolism*Rblood2plasma - 2*Qalv*Qgfr*Qgut^2*fup
                        - 4*Qalv*Qgfr*Qgut*Qliver*fup - 2*Qalv*Qgfr*Qgut*Qlung*fup
                        - 2*Qalv*Qgfr*Qgut*Qrest*fup - 2*Qalv*Qgfr*Qliver^2*fup
                        - 2*Qalv*Qgfr*Qliver*Qlung*fup - 2*Qalv*Qgfr*Qliver*Qrest*fup
                        - 2*Qalv*Qgfr*Qlung*fup*Clmetabolism - 2*Qalv*Qgfr*Qrest*fup*Clmetabolism
                        - 2*Qalv*Qgut^2*Qkidney*Rblood2plasma - 2*Qalv*Qgut*Qkidney^2*Rblood2plasma
                        - 4*Qalv*Qgut*Qkidney*Qliver*Rblood2plasma - 2*Qalv*Qgut*Qkidney*Qlung*Rblood2plasma
                        - 2*Qalv*Qgut*Qkidney*Qrest*Rblood2plasma - 2*Qalv*Qkidney^2*Qliver*Rblood2plasma
                        - 2*Qalv*Qkidney^2*Clmetabolism*Rblood2plasma - 2*Qalv*Qkidney*Qliver^2*Rblood2plasma
                        - 2*Qalv*Qkidney*Qliver*Qlung*Rblood2plasma - 2*Qalv*Qkidney*Qliver*Qrest*Rblood2plasma
                        - 2*Qalv*Qkidney*Qlung*Clmetabolism*Rblood2plasma
                        - 2*Qalv*Qkidney*Qrest*Clmetabolism*Rblood2plasma)/((2*Qgfr*Qgut^2*fup*Kblood2air
                                                                             + 4*Qgfr*Qgut*Qliver*fup*Kblood2air
                                                                             + 2*Qgfr*Qgut*Qlung*fup*Kblood2air
                                                                             + 2*Qgfr*Qgut*Qrest*fup*Kblood2air
                                                                             - 2*Qgfr*Qgut*fup*Kblood2air*Qcardiac
                                                                             + 2*Qgfr*Qliver^2*fup*Kblood2air
                                                                             + 2*Qgfr*Qliver*Qlung*fup*Kblood2air
                                                                             + 2*Qgfr*Qliver*Qrest*fup*Kblood2air
                                                                             - 2*Qgfr*Qliver*fup*Kblood2air*Qcardiac
                                                                             + 2*Qgfr*Qlung*fup*Clmetabolism*Kblood2air
                                                                             + 2*Qgfr*Qrest*fup*Clmetabolism*Kblood2air
                                                                             - 2*Qgfr*fup*Clmetabolism*Kblood2air*Qcardiac
                                                                             + 2*Qgut^2*Qkidney*Kblood2air*Rblood2plasma
                                                                             + 2*Qgut*Qkidney^2*Kblood2air*Rblood2plasma
                                                                             + 4*Qgut*Qkidney*Qliver*Kblood2air*Rblood2plasma
                                                                             + 2*Qgut*Qkidney*Qlung*Kblood2air*Rblood2plasma
                                                                             + 2*Qgut*Qkidney*Qrest*Kblood2air*Rblood2plasma
                                                                             - 2*Qgut*Qkidney*Kblood2air*Qcardiac*Rblood2plasma
                                                                             + 2*Qkidney^2*Qliver*Kblood2air*Rblood2plasma
                                                                             + 2*Qkidney^2*Clmetabolism*Kblood2air*Rblood2plasma
                                                                             + 2*Qkidney*Qliver^2*Kblood2air*Rblood2plasma
                                                                             + 2*Qkidney*Qliver*Qlung*Kblood2air*Rblood2plasma
                                                                             + 2*Qkidney*Qliver*Qrest*Kblood2air*Rblood2plasma
                                                                             - 2*Qkidney*Qliver*Kblood2air*Qcardiac*Rblood2plasma
                                                                             + 2*Qkidney*Qlung*Clmetabolism*Kblood2air*Rblood2plasma
                                                                             + 2*Qkidney*Qrest*Clmetabolism*Kblood2air*Rblood2plasma
                                                                             - 2*Qkidney*Clmetabolism*Kblood2air*Qcardiac*Rblood2plasma
                                                                             + kUrt*Qgfr*Qgut*fup + kUrt*Qgfr*Qliver*fup + kUrt*Qgfr*fup*Clmetabolism
                                                                             + kUrt*Qgut*Qkidney*Rblood2plasma + kUrt*Qkidney*Qliver*Rblood2plasma
                                                                             + kUrt*Qkidney*Clmetabolism*Rblood2plasma - 2*Qalv*Qgfr*Qgut*fup
                                                                             - 2*Qalv*Qgfr*Qliver*fup - 2*Qalv*Qgfr*fup*Clmetabolism
                                                                             - 2*Qalv*Qgut*Qkidney*Rblood2plasma - 2*Qalv*Qkidney*Qliver*Rblood2plasma
                                                                             - 2*Qalv*Qkidney*Clmetabolism*Rblood2plasma)*Qcardiac)

  if (concentration=="plasma"){   
    # Calculate Cplasma_ss
    Css <- Css/Rblood2plasma
  }
  else if (concentration=="venous"){
    Css <- Css
  }
  return(Css)
}

