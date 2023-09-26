#' Correct the measured intrinsive hepatic clearance for fraction free
#'
#' This function uses the free fraction estimated from Kilford et al. (2008) 
#' to increase the in vitro measure intrinsic hepatic clearance. The assumption
#' that chemical that is bound in vitro is not available to be metabolized and
#' therefore the actual rate of clearance is actually faster. Note that in most
#' high throughput TK models included in the package this increase is offset by
#' the assumption of "restrictive clearance" -- that is, the rate of hepatic
#' metabolism is slowed to account for the free fraction of chemical in plasma.
#' This adjustment was made starting in Wetmore et al. (2015) in order to better
#' predict plasma concentrations.
#'
#' @param Clint In vitro measured intrinsic hepatic clearance in units of
#' (ul/min/million hepatocytes).
#'
#' @param Fu_hep Estimated fraction of chemical free for metabolism in the 
#' in vitro assay, estimated by default from the method of Kilford et al. (2008)
#' using \code{\link{calc_hep_fu}}
#'
#' @param Pow The octanal:water equilibrium partition coefficient
#'
#' @param pKa_Donor A string containing hydrogen donor ionization equilibria, 
#' concatenated with commas. Can be "NA" if none exist.
#'
#' @param pKa_Accept A string containing hydrogen acceptance ionization equilibria, 
#' concatenated with commas. Can be "NA" if none exist.
#'
#' @param suppress.messages Whether or not the output message is suppressed.
#'
#' @return Intrinsic hepatic clearance increased to take into account binding
#' in the in vitro assay
#'
#' @author John Wambaugh
#'
#' @references 
#' \insertRef{kilford2008hepatocellular}{httk} 
#' \insertRef{wetmore2015incorporating}{httk}
#'
#' @keywords in-vitro
#'
#' @seealso \code{\link{calc_hep_fu}}
#'
#' @export apply_clint_adjustment
apply_clint_adjustment <- function(Clint, 
                         Fu_hep=NULL,
                         Pow = NULL, 
                         pKa_Donor=NULL, 
                         pKa_Accept=NULL,
                         suppress.messages=FALSE)
{
  if (is.null(Fu_hep))
  {
    Fu_hep <- calc_hep_fu(parameters=list(Pow = Pow,
                                          pKa_Donor = pKa_Donor,
                                          pKa_Accept = pKa_Accept))
  }
  Clint <- Clint / Fu_hep
  
  if (!suppress.messages) 
  {
    warning('Clint adjusted for in vitro partitioning (Kilford, 2008), see calc_hep_fu.')
  }
  
  return(set_httk_precision(Clint))
}