#' Calculate the total plasma clearance.
#' 
#' This function calculates the total clearance rate for a one compartment model
#' for plasma
#' where clearance is entirely due to metablism by the liver and glomerular
#' filtration in the kidneys, identical to clearance of three compartment
#' steady state model.
#' 
#' @param chem.name Either the chemical name, CAS number, or the parameters
#' must be specified.
#' 
#' @param chem.cas Either the chemical name, CAS number, or the parameters must
#' be specified.
#' 
#' @param dtxsid EPA's 'DSSTox Structure ID (https://comptox.epa.gov/dashboard)  
#' the chemical must be identified by either CAS, name, or DTXSIDs
#' 
#' @param parameters Chemical parameters from parameterize_steadystate
#' function, overrides chem.name and chem.cas.
#'
#' @param model The model used to calculate total clearance (defaults to "3compartmentss")
#' 
#' @param species Species desired (either "Rat", "Rabbit", "Dog", "Mouse", or
#' default "Human"). 
#' 
#' @param suppress.messages Whether or not the output message is suppressed.
#' 
#' @param default.to.human Substitutes missing animal values with human values
#' if true.
#' 
#' @param restrictive.clearance Protein binding is not taken into account (set
#' to 1) in liver clearance if FALSE.
#' 
#' @param ... Additional parameters passed to parameterize_steadystate if
#' parameters is NULL.
#' 
#' @param class.exclude Exclude chemical classes identified as outside of 
#' domain of applicability by relevant modelinfo_[MODEL] file (default TRUE).
#' 
#' @return \item{Total Clearance}{Units of L/h/kg BW.}
#' 
#' @author John Wambaugh
#' 
#' @keywords Parameter 1compartment
#' 
#' @examples
#' 
#' calc_total_clearance(chem.name="Ibuprofen") 
#' 
#' @export calc_total_clearance
calc_total_clearance<- function(chem.cas=NULL,
                                chem.name=NULL,
                                dtxsid=NULL,
                                parameters=NULL,
                                model="3compartmentss",
                                species="Human",
                                suppress.messages=FALSE,
                                default.to.human=FALSE,
                                class.exclude=TRUE,
                                restrictive.clearance=TRUE,
                                ...)

{
# Total clearance is equal to 1/Css, so calculate mg/L Css for 1 mg/kg/day dose:
  clearance <- 1/do.call(calc_analytic_css, args=purrr::compact(c(
                         list(chem.cas=chem.cas, 
                              chem.name=chem.name, 
                              dtxsid=dtxsid,
                              parameters=parameters,
                              model=model,
                              output.units="mg/L",
                              species=species,
                              parameterize.args=list(
                                default.to.human=default.to.human,
                                class.exclude=class.exclude),
                              restrictive.clearance=restrictive.clearance,
                              suppress.messages=suppress.messages),
                         ...))) # mg/kg/day / mg/L -> clearance: L/kg/day

# Convert from 1/day to 1/h:
  clearance <- clearance/24

  if (!suppress.messages)
  {
    cat(paste(toupper(substr(species,1,1)),
      substr(species,2,nchar(species)),sep=''),
      "total clearance returned in units of L/h/kg BW.\n")
  }
  
  return(set_httk_precision(as.numeric(clearance)))
}
