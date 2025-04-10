#' Calculate the elimination rate for a one compartment model
#' 
#' This function calculates an elimination rate from the three compartment
#' steady state model where elimination is entirely due to metablism by the
#' liver and glomerular filtration in the kidneys.
#' 
#' Elimination rate calculated by dividing the total clearance (using the
#' default -stirred hepatic model) by the volume of distribution. When
#' species is specified as rabbit, dog, or mouse, the function uses the
#' appropriate physiological data(volumes and flows) but substitues human
#' fraction unbound, partition coefficients, and intrinsic hepatic clearance.
#' 
#' @param chem.cas Either the cas number or the chemical name must be
#' specified. 
#' 
#' @param chem.name Either the chemical name or the cas number must be
#' specified. 
#' 
#' @param dtxsid EPA's 'DSSTox Structure ID (\url{https://comptox.epa.gov/dashboard})  
#' the chemical must be identified by either CAS, name, or DTXSIDs
#' 
#' @param parameters Chemical parameters from parameterize_steadystate or
#' 1compartment function, overrides chem.name and chem.cas.
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
#' @return \item{Elimination rate}{Units of 1/h.}
#' 
#' @seealso \code{\link{calc_total_clearance}} for calculation of total clearance
#' 
#' @seealso \code{\link{calc_vdist}} for calculation of volume of distribution
#' 
#' @author John Wambaugh
#' 
#' @references 
#' Schmitt, Walter. "General approach for the calculation of tissue 
#' to plasma partition coefficients." Toxicology in vitro 22.2 (2008): 457-467.
#'
#' \insertRef{dawson2021qsar}{httk} 
#'
#' \insertRef{kilford2008hepatocellular}{httk} 
#'
#' @keywords Parameter  1compartment
#' 
#' @examples
#' 
#' calc_elimination_rate(chem.name="Bisphenol A")
#'\dontrun{
#' calc_elimination_rate(chem.name="Bisphenol A",species="Rat")
#' calc_elimination_rate(chem.cas="80-05-7")
#'}
#'
#' @export calc_elimination_rate
calc_elimination_rate <- function(chem.cas=NULL,
                                  chem.name=NULL,
                                  dtxsid=NULL,
                                  parameters=NULL,
                                  model="3compartmentss",
                                  species="Human",
                                  suppress.messages=TRUE,
                                  ...
                                  )
{
  if ('Vdist' %in% names(parameters))
  {
    Vd <- parameters[['Vdist']]
  } else {
      Vd <- do.call(calc_vdist, 
                    args=c(list(chem.cas=chem.cas,
                                chem.name=chem.name,
                                dtxsid=dtxsid,
                                species=species,
                                parameters=parameters,
                                suppress.messages=suppress.messages
                                ),
                           list(...)
                           )
                    ) 
  } # L/kgBW 

  clearance <- do.call(calc_total_clearance, 
                       args = c(list(chem.name=chem.name,
                                    chem.cas=chem.cas,
                                    dtxsid=dtxsid,
                                    species=species,
                                    parameters=parameters,
                                    model=model,
                                    suppress.messages=suppress.messages
                                    ),
                               list(...)
                               ) 
                       ) #L/h/kgBW
                       
  if (!suppress.messages) cat(paste(
      toupper(substr(species,1,1)),
      substr(species,2,nchar(species)),sep=''),
      "elimination rate returned in units of 1/h.\n")

  return(set_httk_precision(as.numeric(clearance/Vd)))
}
