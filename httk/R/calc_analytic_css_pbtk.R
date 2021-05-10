#'Calculate the analytic steady state concentration for model pbtk.
#'
#'This function calculates the analytic steady state plasma or venous blood 
#'concentrations as a result of infusion dosing.
#'
#'@param chem.name Either the chemical name, CAS number, or the parameters must 
#' be specified.
#'@param chem.cas Either the chemical name, CAS number, or the parameters must 
#' be specified.
#' @param dtxsid EPA's 'DSSTox Structure ID (\url{https://comptox.epa.gov/dashboard})    
#' the chemical must be identified by either CAS, name, or DTXSIDs
#'@param parameters Chemical parameters from parameterize_pbtk (for model = 
#' 'pbtk'), parameterize_3comp (for model = '3compartment), 
#' parmeterize_1comp(for model = '1compartment') or parameterize_steadystate 
#' (for model = '3compartmentss'), overrides chem.name and chem.cas.
#'@param hourly.dose Hourly dose rate mg/kg BW/h.
#'@param concentration Desired concentration type, 'blood', 'tissue', or default 'plasma'.
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
#'@param ... Additional parameters passed to parameterize function if 
#' parameters is NULL.
#'  
#'@return Steady state concentration in uM units
#'
#'@author Robert Pearce and John Wambaugh
#'@keywords pbtk
calc_analytic_css_pbtk <- function(chem.name=NULL,
                                   chem.cas = NULL,
                                   dtxsid = NULL,
                                   parameters=NULL,
                                   hourly.dose=1/24,
                                   concentration='plasma',
                                   suppress.messages=FALSE,
                                   recalc.blood2plasma=FALSE,
                                   tissue=NULL,
                                   restrictive.clearance=TRUE,
                                   bioactive.free.invivo = FALSE,
                                   ...)
{
  #R CMD CHECK throws notes about "no visible binding for global variable", for
  #each time a data.table column name is used without quotes. To appease R CMD
  #CHECK, a variable has to be created for each of these column names and set to
  #NULL. Note that within the data.table, these variables will not be NULL! Yes,
  #this is pointless and annoying.
  dose <- NULL
  #End R CMD CHECK appeasement.
  
  param.names.pbtk <- model.list[["pbtk"]]$param.names
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

    parameters <- parameterize_pbtk(chem.cas=chem.cas,
                                    chem.name=chem.name,
                                    suppress.messages=suppress.messages,
                                    ...) 
    if (recalc.blood2plasma) 
    {
      warning("Argument recalc.blood2plasma=TRUE ignored because parameters is NULL.")
    }
  } else {
    if (!all(param.names.pbtk %in% names(parameters)))
    {
      stop(paste("Missing parameters:",
           paste(param.names.pbtk[which(!param.names.pbtk %in% names(parameters))],
             collapse=', '),
           ".  Use parameters from parameterize_pbtk.")) 
    }
    if (recalc.blood2plasma) {
      parameters[['Rblood2plasma']] <- 1 - 
        parameters[['hematocrit']] + 
        parameters[['hematocrit']] * parameters[['Krbc2pu']] * parameters[['Funbound.plasma']]
    }
  }
  
  Qcardiac <-  parameters[["Qcardiacc"]] / parameters[['BW']]^0.25  
  Qgfr <-  parameters[["Qgfrc"]] / parameters[['BW']]^0.25    
  Clmetabolism <-  parameters[["Clmetabolismc"]]  
  Kliver2pu <- parameters[['Kliver2pu']]
  
  Qgut <- parameters[["Qgutf"]] * Qcardiac
  Qliver <- parameters[["Qliverf"]] * Qcardiac
  Qkidney <- parameters[['Qkidneyf']] * Qcardiac
  Qrest <- Qcardiac-Qgut-Qliver-Qkidney
  Rblood2plasma <- parameters[['Rblood2plasma']]
  fup <- parameters[["Funbound.plasma"]]
  if (!restrictive.clearance) Clmetabolism <- Clmetabolism / fup
  
  hourly.dose <- hourly.dose * parameters$Fgutabs
  
# Calculate steady-state plasma Css:
  Css <- (hourly.dose * (Qliver + Qgut) / 
         (fup * Clmetabolism / Rblood2plasma + (Qliver + Qgut))) / 
         (Qcardiac - (Qliver + Qgut)**2 /
         (fup * Clmetabolism / Rblood2plasma + (Qliver + Qgut)) - 
         Qkidney**2 / (Qgfr * fup / Rblood2plasma + Qkidney) - Qrest)

# Check to see if a specific tissue was asked for:
  if (!is.null(tissue))
  {
# Need to convert to schmitt parameters:
    #The parameters used in predict_partitioning_schmitt may be a compound
    #data.table/data.frame or list object, however, depending on the source 
    #of the parameters. In calc_mc_css, for example, parameters is received 
    #as a "data.table" object. Screen for processing appropriately.
    if (any(class(parameters) == "data.table")){
      pcs <- predict_partitioning_schmitt(parameters =
        parameters[, param.names.schmitt[param.names.schmitt %in% 
        names(parameters)], with = F])
    }else if (class(parameters) == "list") {
      pcs <- predict_partitioning_schmitt(parameters =
        parameters[param.names.schmitt[param.names.schmitt %in% 
        names(parameters)]])
    }else stop('httk is only configured to process parameters as objects of 
               class list or class compound data.table/data.frame.')
    
    if (!paste0('K',tolower(tissue)) %in% 
      substr(names(pcs),1,nchar(names(pcs))-3))
    {
      stop(paste("Tissue",tissue,"is not available."))
    }
# Tissues with sources (gut) or sinks (liver,kidney) need to be calculated
# taking the change of mass into account:
    if (tissue == 'gut')
    {
      Qgut <- parameters$Qgutf * parameters$Qcardiacc / parameters$BW^0.25
      Css <- parameters[['Kgut2pu']] * parameters[['Funbound.plasma']] * 
        (Css + dose / (Qgut * parameters[['Rblood2plasma']]))
    } else if (tissue == 'liver') {
      Qliver <- (parameters$Qgutf + parameters$Qliverf) * parameters$Qcardiacc / 
        parameters$BW^0.25
      Clmetabolism <- parameters$Clmetabolismc
      if (!restrictive.clearance) Clmetabolism <- Clmetabolism / fup
      Css <- parameters[['Kliver2pu']] * fup * (hourly.dose + 
        Qliver * Css * Rblood2plasma) / 
        (Clmetabolism * fup + Qliver * Rblood2plasma)
    } else if(tissue == 'kidney') {
      Qkidney <- parameters$Qkidneyf * parameters$Qcardiacc / parameters$BW^0.25
      Css <- parameters[['Kkidney2pu']] * fup * Qkidney * Css * Rblood2plasma /
        (Qkidney * Rblood2plasma + parameters$Qgfrc * fup)
# All other tissues are proportional based on the partition coefficient:
    } else {
      Css <- Css * pcs[[names(pcs)[substr(names(pcs),2,nchar(names(pcs))-3)==tissue]]] * fup   
    }
  }

  if(tolower(concentration != "tissue")){
    if (tolower(concentration)=='plasma')
    {
      Css <- Css 
      concentration <- "Plasma"
      
      if(bioactive.free.invivo == T){
        Css <- Css * parameters[['Funbound.plasma']]
      }
      
    } else if (tolower(concentration)=='blood')
    {
      Css <- Css * Rblood2plasma
      concentration <- "Blood"
    } else {
      stop("Only blood and plasma concentrations are calculated.")
    }
  }
  return(Css)
}

