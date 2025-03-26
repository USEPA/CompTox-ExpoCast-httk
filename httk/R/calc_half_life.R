#' Calculates the half-life for a one compartment model.
#' 
#' This function calculates the half life from the three compartment
#' steady state model where elimination is entirely due to metabolism by the
#' liver and glomerular filtration in the kidneys.
#' 
#' Half life is calculated by dividing the natural-log of 2 by the elimination
#' rate from the one compartment model.
#' 
#' @param chem.cas Either the cas number or the chemical name must be specified.
#' 
#' @param chem.name Either the chemical name or the cas number must be specified.
#' 
#' @param dtxsid EPA's 'DSSTox Structure ID (\url{https://comptox.epa.gov/dashboard})  
#' the chemical must be identified by either CAS, name, or DTXSIDs
#' 
#' @param parameters Chemical parameters from parameterize_steadystate or
#' 1compartment function, overrides chem.name and chem.cas.
#' 
#' @param model The model used to calculate elimination rate (defaults to "3compartmentss")
#'
#' @param species Species desired (either "Rat", "Rabbit", "Dog", "Mouse", or
#' default "Human").
#' 
#' @param suppress.messages Whether or not the output message is suppressed.
#' 
#' @param ... Additional parameters passed to parameterize function if 
#' parameters is NULL.
#'
#' @return \item{Half life}{Units of h.}
#'
#' @seealso \code{\link{calc_elimination_rate}}
#' 
#' @author Sarah E. Davidson
#' 
#' @keywords Parameter  1compartment
#' 
#' @examples
#' calc_half_life(chem.name="Bisphenol A")
#'\donttest{
#' calc_half_life(chem.name="Bisphenol A",species="Rat")
#' 
#' calc_half_life(chem.cas="80-05-7")
#' 
#' # Volatiles are outside the domain of default model:
#' try(calc_half_life(
#'      chem.name="toluene"))
#' 
#' # We can turn off physchem checking:
#' calc_half_life(
#'      chem.name="toluene",
#'      physchem.exclude=FALSE)
#'
#' # Or use an appropriate model for volatiles:
#' calc_half_life(
#'      chem.name="toluene",
#'      model="sumclearances")
#'
#' # PFAS are outside the domain:
#' try(calc_half_life(
#'      dtxsid="DTXSID8031865",
#'      model="sumclearances"))
#' 
#' # Can turn off chemical class checking:
#' calc_half_life(
#'   dtxsid="DTXSID8031865",
#'   model="sumclearances",
#'   class.exclude=FALSE,
#'   suppress.messages=TRUE)
#' 
#' # Change species: 
#' calc_half_life(
#'   dtxsid="DTXSID8031865",
#'   species="rat",
#'   model="sumclearances",
#'   default.to.human=TRUE,
#'   class.exclude=FALSE,
#'   physchem.exclude=FALSE,
#'   suppress.messages=TRUE)
#'}
#'
#' @export calc_half_life
calc_half_life <- function(chem.cas = NULL, 
                            chem.name = NULL, 
                            dtxsid = NULL,
                            parameters = NULL,
                            species="Human",
                            model = "3compartmentss", 
                            suppress.messages = TRUE,
                            ...
                            )
{
  elim_rate <- do.call(calc_elimination_rate, 
                       args=c(list(chem.cas = chem.cas,
                                 chem.name = chem.name,
                                 dtxsid = dtxsid,
                                 parameters = parameters,
                                 model = model,
                                 species = species,
                                 suppress.messages = suppress.messages),
                                 list(...))
                       )
  
  half_life <- log(2)/elim_rate # calculate the elimination rate
  
  if (!suppress.messages) cat(
      paste(toupper(substr(species,1,1)),
            substr(species,2,nchar(species)),sep=''),
            "half life returned in units of h.\n")
  
  return(set_httk_precision(as.numeric(half_life)))
}
