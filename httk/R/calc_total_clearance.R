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
#' @param ... Additional parameters passed to parameterize function if 
#' parameters is NULL.
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
calc_total_clearance <- function(chem.cas=NULL,
                                chem.name=NULL,
                                dtxsid=NULL,
                                parameters=NULL,
                                model="3compartmentss",
                                suppress.messages=FALSE,
                                species="Human",
                                ...
                                )

{
# For oral route of exposure total clearance is equal to f_bio/Css, 
# so calculate mg/L Css for 1 mg/kg/day dose:
  Css <- do.call(calc_analytic_css, 
                 args=purrr::compact(c(
                         list(chem.cas=chem.cas, 
                              chem.name=chem.name, 
                              dtxsid=dtxsid,
                              parameters=parameters,
                              model=model,
                              species=species,
                              output.units="mg/L",
                              suppress.messages=suppress.messages),
                          list(...)
                          )
                          )
                 ) # mg/L / mg/kg/day 
  fbio <- do.call(calc_fbio.oral,
                  args = c(list(chem.cas=chem.cas, 
                               chem.name=chem.name, 
                               dtxsid=dtxsid,
                               parameters=parameters,
                               species=species,
                               suppress.messages=suppress.messages,
                               class.exclude=FALSE,
                               physchem.exclude=FALSE
                               ),
                           list(...)
                           )
                   ) 
  clearance <- fbio$fbio.oral/Css # L / kg / day
  
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
