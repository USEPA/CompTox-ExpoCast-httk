#'Calculate the analytic steady state plasma concentration for model pbtk.
#'
#' This function calculates the analytic steady state concentration (mg/L) as a result
#' of oral infusion dosing. Concentrations are returned for plasma by default, but various
#' tissues or blood concentrations can also be given as specified.
#' 
#' The PBTK model (Pearce et al., 2017) predicts the amount of chemical in
#' various tissues of the body. A system of oridinary
#' differential equations describes how the amounts in each tissue change as 
#' a function of time. The analytic steady-state equation was found by 
#' algebraically solving for the tissue concentrations that result in each
#' equation being zero -- thus determining the concentration at which there is no change
#' over time as the result of a fixed infusion dose rate. 
#'
#' The analytical solution is:
#' \deqn{C^{ss}_{ven} = \frac{dose rate * \frac{Q_{liver} + Q_{gut}}{\frac{f_{up}}{R_{b:p}}*Cl_{metabolism} + (Q_{liver}+Q_{gut})}}{Q_{cardiac} - \frac{(Q_{liver} + Q_{gut})^2}{\frac{f_{up}}{R_{b:p}}*Cl_{metabolism} + (Q_{liver}+Q_{gut})} - \frac{(Q_{kidney})^2}{\frac{f_{up}}{R_{b:p}}*Q_{GFR}+Q_{kideny}}-Q_{rest}}}{%
#' C_ven_ss =(dose rate * (Q_liver + Q_gut) / (f_up/Rb2p*Cl_metabolism + (Q_liver + Qgut)))/(Q_cardiac - (Q_liver + Qgut)^2/(f_up/Rb2p*Cl_metabolism + (Q_liver + Qgut)) - (Q_kidney)^2/(f_up/Rb2p*Q_gfr + Q_kidney) - Q_rest)}
#' \deqn{C^{ss}_{plasma} = \frac{C^{ss}_{ven}}{R_{b:p}}}{%
#'       C_ss = C_ven_ss/Rb2p}
#' \deqn{C^{ss}_{tissue} = \frac{K_{tissue:fuplasma}*f_{up}}{R_{b:p}}*C^{ss}_{ven}}{%
#'        C_tissue_ss = K_tissue2fuplasma*f_up*C_ven_ss/Rb2p}
#'  where Q_cardiac is the cardiace output, Q_gfr is the glomerular filtration
#' rate in the kidney, other Q's indicate blood flows to various tissues, 
#' Cl_metabolism is the chemical-specific whole liver metabolism clearance,
#' f_up is the chemical-specific fraction unbound n plasma, R_b2p is the 
#' chemical specific ratio of concentrations in blood:plasma, K_tissue2fuplasma
#' is the chemical- and tissue-specufic equilibrium partition coefficient
#' and dose rate has  units of mg/kg/day. 
#' 
#'@param chem.name Either the chemical name, CAS number, or the parameters must 
#' be specified.
#'@param chem.cas Either the chemical name, CAS number, or the parameters must 
#' be specified.
#' @param dtxsid EPA's 'DSSTox Structure ID (\url{https://comptox.epa.gov/dashboard})    
#' the chemical must be identified by either CAS, name, or DTXSIDs
#'@param parameters Chemical parameters from parameterize_pbtk (for model = 
#' 'pbtk'), parameterize_3comp (for model = '3compartment), 
#' parameterize_1comp(for model = '1compartment') or parameterize_steadystate 
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
#' 
#' @param Caco2.options A list of options to use when working with Caco2 apical to
#' basolateral data \code{Caco2.Pab}, default is Caco2.options = list(Caco2.default = 2,
#' Caco2.Fabs = TRUE, Caco2.Fgut = TRUE, overwrite.invivo = FALSE, keepit100 = FALSE). Caco2.default sets the default value for 
#' Caco2.Pab if Caco2.Pab is unavailable. Caco2.Fabs = TRUE uses Caco2.Pab to calculate
#' fabs.oral, otherwise fabs.oral = \code{Fabs}. Caco2.Fgut = TRUE uses Caco2.Pab to calculate 
#' fgut.oral, otherwise fgut.oral = \code{Fgut}. overwrite.invivo = TRUE overwrites Fabs and Fgut in vivo values from literature with 
#' Caco2 derived values if available. keepit100 = TRUE overwrites Fabs and Fgut with 1 (i.e. 100 percent) regardless of other settings.
#' 
#'@param ... Additional parameters passed to parameterize function if 
#' parameters is NULL.
#'  
#'@return Steady state plasma concentration in mg/L units
#'
#' @seealso \code{\link{calc_analytic_css}}
#'
#' @seealso \code{\link{parameterize_pbtk}}
#'
#'@author Robert Pearce and John Wambaugh
#'
#' @references 
#' \insertRef{pearce2017httk}{httk}
#'
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
                                   Caco2.options = list(),
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
  
  Qcardiac <-  parameters[["Qcardiacc"]] / parameters[['BW']]^0.25 # L/h/kg
  Qgfr <-  parameters[["Qgfrc"]] / parameters[['BW']]^0.25 # L/h/kg   
  Clmetabolism <-  parameters[["Clmetabolismc"]] # L/h/kg
  Kliver2pu <- parameters[['Kliver2pu']]
  
  Qgut <- parameters[["Qgutf"]] * Qcardiac # L/h/kg
  Qliver <- parameters[["Qliverf"]] * Qcardiac # L/h/kg
  Qkidney <- parameters[['Qkidneyf']] * Qcardiac # L/h/kg
  Qrest <- Qcardiac-Qgut-Qliver-Qkidney # L/h/kg
  Rblood2plasma <- parameters[['Rblood2plasma']]
  fup <- parameters[["Funbound.plasma"]]
  if (!restrictive.clearance) Clmetabolism <- Clmetabolism / fup
  
  hourly.dose <- hourly.dose * parameters$Fabsgut
  
# Css for venous blood:
  Cven.ss <- (hourly.dose * (Qliver + Qgut) / 
         (fup * Clmetabolism / Rblood2plasma + (Qliver + Qgut))) / 
         (Qcardiac - 
         (Qliver + Qgut)**2 /
         (fup * Clmetabolism / Rblood2plasma + (Qliver + Qgut)) - 
         Qkidney +
         Qgfr * fup / Rblood2plasma  - 
         Qrest)
# Calculate steady-state plasma Css:
  Css <- Cven.ss / Rblood2plasma

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
    }else if (is(parameters,"list")) {
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

