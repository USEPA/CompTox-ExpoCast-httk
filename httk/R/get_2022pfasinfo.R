#' Retrieve chemical information on 2022 EPA PFAS Chemicals
#' 
#' This function is a wrapper for \code{\link{get_cheminfo}} that only lists
#' chemicals from the Smeltz, Kreutz, and Crizer data sets collected on PFAS
#' between 2019 and 2022. Plasma protein binding (fraction unbound) data were
#' collected using ultracentrifugation (UC) instead of rapid equilibrium
#' dialysis. Intrsinsic hepatic clearance (Clint) data were collected with
#' substrate depletion (over time) assays.
#'
#'
#' \strong{Note} that in some cases the \strong{Funbound.plasma} and the 
#' \strong{intrinsic clearance} are
#' \emph{provided as a series of numbers separated by commas}. These values are the 
#' result of Bayesian analysis and characterize a distribution: the first value
#' is the median of the distribution, while the second and third values are the 
#' lower and upper 95th percentile (that is qunatile 2.5 and 97.5) respectively.
#' For intrinsic clearance a fourth value indicating a p-value for a decrease is
#' provided. Typically 4000 samples were used for the Bayesian analusis, such
#' that a p-value of "0" is equivale to "<0.00025". See Wambaugh et al. (2019)
#' for more details. If argument meadian.only == TRUE then only the median is
#' reported for parameters with Bayesian analysis distributions. If the 95% 
#' credible interval is larger than fup.ci.cutoff (defaults
#' to NULL) then the Fup is treated as too uncertain and the value NA is given.
#' 
#' @param info A single character vector (or collection of character vectors)
#' from "Compound", "CAS", "DTXSID, "logP", "pKa_Donor"," pKa_Accept", "MW", "Clint",
#' "Clint.pValue", "Funbound.plasma","Structure_Formula", or "Substance_Type". info="all"
#' gives all information for the model and species.
#' 
#' @param species Species desired (either "Rat", "Rabbit", "Dog", "Mouse", or
#' default "Human").
#' 
#' @param fup.lod.default Default value used for fraction of unbound plasma for
#' chemicals where measured value was below the limit of detection. Default
#' value is 0.0005.
#' 
#' @param model Model used in calculation, 'pbtk' for the multiple compartment
#' model, '1compartment' for the one compartment model, '3compartment' for
#' three compartment model, '3compartmentss' for the three compartment model
#' without partition coefficients, or 'schmitt' for chemicals with logP and
#' fraction unbound (used in predict_partitioning_schmitt).
#' 
#' @param default.to.human Substitutes missing values with human values if
#' true.
#' 
#' @param median.only Use median values only for fup and clint.  Default is FALSE.
#' 
#' @param fup.ci.cutoff Cutoff for the level of uncertainty in fup estimates.
#' This value should be between (0,1). Default is `NULL` specifying no filtering.
#' 
#' @param clint.pvalue.threshold Hepatic clearance for chemicals where the in
#' vitro clearance assay result has a p-values greater than the threshold are
#' set to zero.
#' 
#' @param suppress.messages Whether or not the output messages are suppressed.
#' 
#' @return \item{vector/data.table}{Table (if info has multiple entries) or 
#' vector containing a column for each valid entry 
#' specified in the argument "info" and a row for each chemical with sufficient
#' data for the model specified by argument "model":
#' \tabular{lll}{
#' \strong{Column} \tab \strong{Description} \tab \strong{units} \cr
#' Compound \tab The preferred name of the chemical compound \tab none \cr 
#' CAS \tab The preferred Chemical Abstracts Service Registry Number \tab none \cr  
#' DTXSID \tab DSSTox Structure ID 
#' (\url{http://comptox.epa.gov/dashboard}) \tab none \cr 
#' logP \tab The log10 octanol:water partition coefficient\tab log10 unitless ratio \cr 
#' MW \tab The chemical compound molecular weight \tab g/mol \cr 
#' pKa_Accept \tab The hydrogen acceptor equilibria concentrations 
#' \tab logarithm \cr   
#' pKa_Donor \tab The hydrogen donor equilibria concentrations 
#'  \tab logarithm \cr   
#' [SPECIES].Clint \tab (Primary hepatocyte suspension) 
#' intrinsic hepatic clearance \tab uL/min/10^6 hepatocytes \cr    
#' [SPECIES].Clint.pValue \tab Probability that there is no clearance observed. \tab none \cr  
#' [SPECIES].Funbound.plasma \tab Chemical fraction unbound in presence of 
#' plasma proteins \tab unitless fraction \cr 
#' [SPECIES].Rblood2plasma \tab Chemical concentration blood to plasma ratio \tab unitless ratio \cr  
#' }
#' }
#' 
#' @author John Wambaugh
#' 
#' @keywords Retrieval
#' 
#'@references 
#'
#' @examples
#'
#' library(httk)
#' PFASCssTable <- NULL
#' for (this.id in get_2022pfasinfo(info="dtxsid"))
#' {
#'   PFASCssTable <- rbind(PFASCssTable, data.frame(
#'     DTXSID = this.id,
#'     Css = calc_mc_css(dtxsid=this.id)
#'     ))
#' }
#' 
#' @export get_2022pfasinfo
get_2022pfasinfo <- function(info="CAS",
                         species="Human",
                         fup.lod.default=0.005,
                         model='3compartmentss',
                         default.to.human=FALSE,
                         median.only=FALSE,
                         fup.ci.cutoff=TRUE,
                         clint.pvalue.threshold=0.05,
                         suppress.messages=FALSE,
                         target.env=.GlobalEnv)
{
# Save all the chemcial data:
  full.data <- chem.physical_and_invitro.data 
  
# Reduce to just the new PFAS data:
  assign("chem.physical_and_invitro.data",
    subset(chem.physical_and_invitro.data,
    regexpr("Smeltz",Human.Clint.Reference)!=-1 |
    regexpr("Smeltz",Human.Funbound.plasma.Reference)!=-1 |
    regexpr("Kreutz",Human.Clint.Reference)!=-1 |
    regexpr("Kreutz",Human.Funbound.plasma.Reference)!=-1 |
    regexpr("Crizer",Human.Clint.Reference)!=-1),
    envir=target.env)

# Run get_cheminfo on just the PFAS data:
   out <- get_cheminfo(info=info,
                         species=species,
                         fup.lod.default=fup.lod.default,
                         model=model,
                         default.to.human=default.to.human,
                         median.only=median.only,
                         fup.ci.cutoff=fup.ci.cutoff,
                         clint.pvalue.threshold=clint.pvalue.threshold,
                         suppress.messages=suppress.messages)

# Return the full data before exiting the function
  assign("chem.physical_and_invitro.data",
    full.data,
    envir=target.env)
    
  return(out)
}