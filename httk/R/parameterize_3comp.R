#' Parameters for a three-compartment toxicokinetic model (dynamic)
#' 
#' This function generates the chemical- and species-specific parameters needed 
#' for model '3compartment', for example \code{\link{solve_3comp}}. A call is 
#' masde to \code{\link{parameterize_pbtk}} to use Schmitt (2008)'s method
#' as modified by Pearce et al. (2017) to predict partition coefficients based
#' on descriptions in \code{\link{tissue.data}}. Organ volumes and flows are
#' retrieved from table \code{\link{physiology.data}}.
#' 
#' Because this model does not simulate exhalation, inhalation, and other 
#' processes relevant to volatile chemicals, this model is by default 
#' restricted to chemicals with a logHenry's Law Constant less than that of 
#' Acetone, a known volatile chemical. That is, chemicals with logHLC > -4.5 
#' (Log10 atm-m3/mole) are excluded. Volatility is not purely determined by the 
#' Henry's Law Constant, therefore this chemical exclusion may be turned off 
#' with the argument "physchem.exclude = FALSE". Similarly, per- and 
#' polyfluoroalkyl substances (PFAS) are excluded by default because the 
#' transporters that often drive PFAS toxicokinetics are not included in this 
#' model. However, PFAS chemicals can be included with the argument 
#' "class.exclude = FALSE".
#' 
#' @param chem.cas Chemical Abstract Services Registry Number (CAS-RN) -- the 
#' chemical must be identified by either CAS, name, or DTXISD
#' 
#' @param chem.name Chemical name (spaces and capitalization ignored) --  the 
#' chemical must be identified by either CAS, name, or DTXISD
#' 
#' @param dtxsid EPA's 'DSSTox Structure ID (https://comptox.epa.gov/dashboard)  
#' -- the chemical must be identified by either CAS, name, or DTXSIDs
#' 
#' @param species Species desired (either "Rat", "Rabbit", "Dog", "Mouse", or
#' default "Human").
#' 
#' @param default.to.human Substitutes missing animal values with human values
#' if true.
#' 
#' @param class.exclude Exclude chemical classes identified as outside of 
#' domain of applicability by relevant modelinfo_[MODEL] file (default TRUE).
#' 
#' @param physchem.exclude Exclude chemicals on the basis of physico-chemical
#' properties (currently only Henry's law constant) as specified by 
#' the relevant modelinfo_[MODEL] file (default TRUE).
#' 
#' @param force.human.clint.fup Forces use of human values for hepatic
#' intrinsic clearance and fraction of unbound plasma if true.
#' 
#' @param clint.pvalue.threshold Hepatic clearances with clearance assays
#' having p-values greater than the threshold are set to zero.
#' 
#' @param adjusted.Funbound.plasma Uses Pearce et al. (2017) lipid binding adjustment
#' for Funbound.plasma (which impacts partition coefficients) when set to TRUE (Default).
#' 
#' @param adjusted.Clint Uses Kilford et al. (2008) hepatocyte incubation
#' binding adjustment for Clint when set to TRUE (Default).
#' 
#' @param regression Whether or not to use the regressions in calculating
#' partition coefficients.
#' 
#' @param suppress.messages Whether or not the output message is suppressed.
#' 
#' @param restrictive.clearance In calculating hepatic.bioavailability, protein
#' binding is not taken into account (set to 1) in liver clearance if FALSE.
#' 
#' @param minimum.Funbound.plasma Monte Carlo draws less than this value are set 
#' equal to this value (default is 0.0001 -- half the lowest measured Fup in our
#' dataset).
#' 
#' @param Caco2.options A list of options to use when working with Caco2 apical 
#' to basolateral data \code{Caco2.Pab}, default is Caco2.options = 
#' list(Caco2.Pab.default = 1.6, Caco2.Fabs = TRUE, Caco2.Fgut = TRUE, 
#' overwrite.invivo = FALSE, keepit100 = FALSE). Caco2.Pab.default sets the 
#' default value for Caco2.Pab if Caco2.Pab is unavailable. Caco2.Fabs = TRUE 
#' uses Caco2.Pab to calculate fabs.oral, otherwise fabs.oral = \code{Fabs}. 
#' Caco2.Fgut = TRUE uses Caco2.Pab to calculate 
#' fgut.oral, otherwise fgut.oral = \code{Fgut}. overwrite.invivo = TRUE 
#' overwrites Fabs and Fgut in vivo values from literature with 
#' Caco2 derived values if available. keepit100 = TRUE overwrites Fabs and Fgut 
#' with 1 (i.e. 100 percent) regardless of other settings.
#' See \code{\link{get_fbio}} for further details.
#' 
#' @param ... Additional arguments, not currently used.
#'
#' @return 
#' \item{BW}{Body Weight, kg.} 
#' \item{Clmetabolismc}{Hepatic Clearance, L/h/kg BW.} 
#' \item{Fabsgut}{Fraction of the oral dose absorbed, i.e. the 
#' fraction of the dose that enters the gutlumen.} 
#' \item{Funbound.plasma}{Fraction of plasma that is not bound.} 
#' \item{Fhep.assay.correction}{The fraction of chemical unbound in hepatocyte 
#' assay using the method of Kilford et al. (2008)} 
#' \item{hematocrit}{Percent volume of red blood cells in the blood.}
#' \item{Kgut2pu}{Ratio of concentration of chemical in gut tissue to unbound
#' concentration in plasma.} 
#' \item{Kliver2pu}{Ratio of concentration of
#' chemical in liver tissue to unbound concentration in plasma.}
#' \item{Krbc2pu}{Ratio of concentration of chemical in red blood cells to
#' unbound concentration in plasma.} 
#' \item{Krest2pu}{Ratio of concentration of
#' chemical in rest of body tissue to unbound concentration in plasma.}
#' \item{million.cells.per.gliver}{Millions cells per gram of liver tissue.}
#' \item{MW}{Molecular Weight, g/mol.} 
#' \item{Qcardiacc}{Cardiac Output, L/h/kg
#' BW^3/4.} \item{Qgfrc}{Glomerular Filtration Rate, L/h/kg BW^3/4, volume of
#' fluid filtered from kidney and excreted.} 
#' \item{Qgutf}{Fraction of cardiac output flowing to the gut.} 
#' \item{Qliverf}{Fraction of cardiac output flowing to the liver.} 
#' \item{Rblood2plasma}{The ratio of the concentration
#' of the chemical in the blood to the concentration in the plasma.}
#' \item{Vgutc}{Volume of the gut per kg body weight, L/kg BW.}
#' \item{Vliverc}{Volume of the liver per kg body weight, L/kg BW.}
#' \item{Vrestc}{ Volume of the rest of the body per kg body weight, L/kg BW.}
#'
#' @author Robert Pearce and John Wambaugh
#'
#' @references 
#'
#' \insertRef{pearce2017httk}{httk}
#'
#' \insertRef{schmitt2008general}{httk}
#'
#' \insertRef{pearce2017evaluation}{httk}
#'
#' \insertRef{kilford2008hepatocellular}{httk}
#'
#' \insertRef{wambaugh2025simple}{httk}
#'
#' @keywords Parameter 3compartment
#'
#' @seealso \code{\link{solve_3comp}}
#'
#' @seealso \code{\link{calc_analytic_css_3comp}}
#'
#' @seealso \code{\link{parameterize_pbtk}}
#'
#' @seealso \code{\link{apply_clint_adjustment}}
#'
#' @seealso \code{\link{tissue.data}}
#'
#' @seealso \code{\link{physiology.data}}
#'
#' @examples
#' 
#' parameters1 <- parameterize_3comp(chem.name='Bisphenol-A',species='Rat')
#' parameters2 <- parameterize_3comp(chem.cas='80-05-7',
#'                                   species='rabbit',default.to.human=TRUE)
#' # The following will not work because Diquat dibromide monohydrate's 
#' # Henry's Law Constant (-3.912) is higher than that of Acetone (~-4.5):
#' try(parameters3 <- parameterize_3comp(chem.cas = "6385-62-2"))
#' # However, we can turn off checking for phys-chem properties, since we know
#' # that  Diquat dibromide monohydrate is not too volatile:
#' parameters3 <- parameterize_3comp(chem.cas = "6385-62-2",
#'                                   physchem.exclude = FALSE)
#' \donttest{
#' out <- solve_3comp(parameters=parameters1, plots=TRUE)
#' }
#' 
#' @export parameterize_3comp
parameterize_3comp <- function(
                       chem.cas = NULL,
                       chem.name = NULL,
                       dtxsid = NULL,
                       species = "Human",
                       default.to.human = FALSE,
                       class.exclude = TRUE,
                       physchem.exclude = TRUE,
                       force.human.clint.fup = FALSE,
                       clint.pvalue.threshold = 0.05,
                       adjusted.Funbound.plasma = TRUE,
                       adjusted.Clint = TRUE,
                       regression = TRUE,
                       suppress.messages = FALSE,
                       restrictive.clearance = TRUE,
                       minimum.Funbound.plasma = 0.0001,
                       Caco2.options = NULL,
                       ...)
{
  parms <- parameterize_pbtk(
             chem.cas = chem.cas,
             chem.name = chem.name,
             dtxsid = dtxsid,
             species = species,
             default.to.human = default.to.human,
             class.exclude = class.exclude,
             physchem.exclude = physchem.exclude,
             tissuelist = list(
               liver=c("liver"),
               gut=c("gut")),
             force.human.clint.fup = force.human.clint.fup,
             clint.pvalue.threshold = clint.pvalue.threshold,
             adjusted.Funbound.plasma = adjusted.Funbound.plasma,
             adjusted.Clint=adjusted.Clint,
             regression = regression,
             suppress.messages = suppress.messages,
             restrictive.clearance = restrictive.clearance,
             minimum.Funbound.plasma = minimum.Funbound.plasma,
             Caco2.options = Caco2.options,
             ...)
                              
  parms$Qkidneyf <- parms$Vvenc <- parms$Vartc <- NULL
 
  return(lapply(parms[model.list[["3compartment"]]$param.names],
                set_httk_precision))                   
}
