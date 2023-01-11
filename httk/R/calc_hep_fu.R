#' Calculate the free chemical in the hepaitic clearance assay
#'
#' This function uses the method from Kilford et al. (2008) to calculate the
#' fraction of unbound chemical in the 
#'  hepatocyte intrinsic clearance assay. The bound chemical is presumed to be
#' unavailable during the performance of the assay, so this fraction can be
#' used to increase the apparent clearance rate to better estimate in vivo 
#' clearance. 
#' For bases, the fraction of chemical unbound in hepatocyte clearance assays 
#' (\ifelse{html}{\out{fu<sub>hep</sub>}}{\eqn{fu_{hep}}}) is calculated in terms of 
#' \ifelse{html}{\out{logP<sub>ow</sub>}}{\eqn{logP_{ow}}}
#' but for neutrual and acidic compounds we use 
#' \ifelse{html}{\out{logD<sub>ow</sub>}}{\eqn{logD_{ow}}} (from \code{\link{calc_dow}}). 
#' Here we denote the appropriate partition coefficient as "logP/D".
#' Kilford et al. (2008) calculates
#' \ifelse{html}{\out{fu<sub>hep</sub> = 1/(1 + 125*V<sub>R</sub>*10^(0.072*logP/D<sup>2</sup> + 0.067*logP/D-1.126))}}{\deqn{fu_{hep} = \frac{1}{1+125*V_{R}*10^{0.072*logP*D^2 + 0.067*logP/D - 1.126}}}}
#'
#' @param chem.cas Chemical Abstract Services Registry Number (CAS-RN) -- if
#'  parameters is not specified then the chemical must be identified by either
#'  CAS, name, or DTXISD
#'
#' @param chem.name Chemical name (spaces and capitalization ignored) --  if
#'  parameters is not specified then the chemical must be identified by either
#'  CAS, name, or DTXISD
#'
#' @param dtxsid EPA's 'DSSTox Structure ID (\url{https://comptox.epa.gov/dashboard})  
#'  -- if parameters is not specified then the chemical must be identified by 
#' either CAS, name, or DTXSIDs
#'
#' @param parameters Parameters from the appropriate parameterization function
#' for the model indicated by argument model
#'
#' @param Vr Ratio of cell volume to incubation volume. Default (0.005) is taken from 
#  Wetmore et al. (2015)
#'
#' @param pH pH of the incupation medium.
#'
#' @return A numeric fraction between zero and one
#'
#' @author John Wambaugh and Robert Pearce
#'
#' @references Kilford, Peter J., et al. "Hepatocellular binding of drugs: 
#' correction for unbound fraction in hepatocyte incubations using microsomal 
#' binding or drug lipophilicity data." Drug Metabolism and Disposition 36.7 
#' (2008): 1194-1197.
#'
#' Wetmore, Barbara A., et al. "Incorporating high-throughput exposure 
#' predictions with dosimetry-adjusted in vitro bioactivity to inform chemical 
#' toxicity testing." Toxicological Sciences 148.1 (2015): 121-136.
#'
#' @keywords in-vitro
#'
#' @seealso \code{\link{adjust_clint}}
#'
#' @import utils
#'
#' @export calc_hep_fu
#'
#'
calc_hep_fu <- function(
                 chem.cas=NULL,
                 chem.name=NULL,
                 dtxsid = NULL,
                 parameters=NULL,
                 Vr=0.005,
                 pH=7.4) 
{
# We need to describe the chemical to be simulated one way or another:
  if (is.null(chem.cas) & 
      is.null(chem.name) & 
      is.null(dtxsid) &
      is.null(parameters)) 
    stop('Parameters, chem.name, chem.cas, or dtxsid must be specified.')

  if (is.null(parameters))
  {
    # Look up the chemical name/CAS, depending on what was provided:
    if (any(is.null(chem.cas),is.null(chem.name),is.null(dtxsid)))
    {
      out <- get_chem_id(
              chem.cas=chem.cas,
              chem.name=chem.name,
              dtxsid=dtxsid)
      chem.cas <- out$chem.cas
      chem.name <- out$chem.name                                
      dtxsid <- out$dtxsid
    }
    # acid dissociation constants
    pKa_Donor <- suppressWarnings(get_physchem_param(
      "pKa_Donor",
      dtxsid=dtxsid,
      chem.name=chem.name,
      chem.cas=chem.cas)) 
    # basic association cosntants
    pKa_Accept <- suppressWarnings(get_physchem_param(
      "pKa_Accept",
      dtxsid=dtxsid,
      chem.name=chem.name,
      chem.cas=chem.cas)) 
    # Octanol:water partition coefficient
    Pow <- 10^get_physchem_param(
      "logP",
      dtxsid=dtxsid,
      chem.name=chem.name,
      chem.cas=chem.cas) 
  } else {
    if (!all(c("Pow","pKa_Donor","pKa_Accept") 
      %in% names(parameters))) 
      stop("Missing parameters needed in calc_hep_fu.")            

    Pow <- parameters$Pow
    pKa_Donor <- parameters$pKa_Donor
    pKa_Accept <- parameters$pKa_Accept
  }

  # Select the appropriate partition coefficient (we treat bases differently):
  if (!is_base(pH=pH, pKa_Donor=pKa_Donor, pKa_Accept=pKa_Accept))
  {
    logPD <- log10(calc_dow(
               Pow, 
               pH=pH,
               pKa_Donor=pKa_Donor,
               pKa_Accept=pKa_Accept)) 
  } else logPD <- log10(Pow)
  
  fu_hep <- 1/(1+ 125*Vr*10^(0.072*logPD^2+0.067*logPD-1.126))
# Vectorized check to keep fu_hep within bounds:
  fu_hep[fu_hep <0 | fu_hep>1] <- 1
  
  return(set_httk_precision(fu_hep))
}
