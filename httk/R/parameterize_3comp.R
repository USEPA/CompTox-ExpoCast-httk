#' Parameterize_3comp
#' 
#' This function initializes the parameters needed in the function solve_3comp.
#' 
#' @param chem.cas Chemical Abstract Services Registry Number (CAS-RN) -- the 
#' chemical must be identified by either CAS, name, or DTXISD
#' @param chem.name Chemical name (spaces and capitalization ignored) --  the 
#' chemical must be identified by either CAS, name, or DTXISD
#' @param dtxsid EPA's 'DSSTox Structure ID (https://comptox.epa.gov/dashboard)  
#' -- the chemical must be identified by either CAS, name, or DTXSIDs
#' @param species Species desired (either "Rat", "Rabbit", "Dog", "Mouse", or
#' default "Human").
#' @param default.to.human Substitutes missing animal values with human values
#' if true.
#' @param force.human.clint.fup Forces use of human values for hepatic
#' intrinsic clearance and fraction of unbound plasma if true.
#' @param clint.pvalue.threshold Hepatic clearances with clearance assays
#' having p-values greater than the threshold are set to zero.
#' @param adjusted.Funbound.plasma Returns adjusted Funbound.plasma when set to
#' TRUE along with parition coefficients calculated with this value.
#' @param regression Whether or not to use the regressions in calculating
#' partition coefficients.
#' @param suppress.messages Whether or not the output message is suppressed.
#' @param restrictive.clearance In calculating hepatic.bioavailability, protein
#' binding is not taken into account (set to 1) in liver clearance if FALSE.
#' @param minimum.Funbound.plasma Monte Carlo draws less than this value are set 
#' equal to this value (default is 0.0001 -- half the lowest measured Fup in our
#' dataset).
#'
#' @return 
#' \item{BW}{Body Weight, kg.} 
#' \item{Clmetabolismc}{Hepatic Clearance, L/h/kg BW.} 
#' \item{Fgutabs}{Fraction of the oral dose absorbed, i.e. the 
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
#' @references Pearce, Robert G., et al. "Httk: R package for high-throughput 
#' toxicokinetics." Journal of statistical software 79.4 (2017): 1.
#'
#' Kilford, P. J., Gertz, M., Houston, J. B. and Galetin, A.
#' (2008). Hepatocellular binding of drugs: correction for unbound fraction in
#' hepatocyte incubations using microsomal binding or drug lipophilicity data.
#' Drug Metabolism and Disposition 36(7), 1194-7, 10.1124/dmd.108.020834.
#'
#' @keywords Parameter 3compartment
#'
#' @examples
#' 
#'  parameters <- parameterize_3comp(chem.name='Bisphenol-A',species='Rat')
#'  parameters <- parameterize_3comp(chem.cas='80-05-7',
#'                                   species='rabbit',default.to.human=TRUE)
#'  out <- solve_3comp(parameters=parameters,plots=TRUE)
#' 
#' @export parameterize_3comp
parameterize_3comp<- function(
                       chem.cas = NULL,
                       chem.name = NULL,
                       dtxsid = NULL,
                       species = "Human",
                       default.to.human = F,
                       force.human.clint.fup = F,
                       clint.pvalue.threshold = 0.05,
                       adjusted.Funbound.plasma = T,
                       regression = T,
                       suppress.messages = F,
                       restrictive.clearance = T,
                       minimum.Funbound.plasma = 0.0001)
{
  parms <- parameterize_pbtk(
             chem.cas = chem.cas,
             chem.name = chem.name,
             dtxsid = dtxsid,
             species = species,
             default.to.human = default.to.human,
             tissuelist = list(
               liver=c("liver"),
               gut=c("gut")),
             force.human.clint.fup = force.human.clint.fup,
             clint.pvalue.threshold = clint.pvalue.threshold,
             adjusted.Funbound.plasma =
               adjusted.Funbound.plasma,
             regression = regression,
             suppress.messages = suppress.messages,
             restrictive.clearance = restrictive.clearance,
             minimum.Funbound.plasma = minimum.Funbound.plasma)
                              
  parms$Qkidneyf <- parms$Vvenc <- parms$Vartc <- NULL
 
  return(lapply(parms,set_httk_precision))                          
}
