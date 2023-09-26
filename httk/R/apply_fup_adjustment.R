#' Correct the measured fraction unbound in plasma for lipid binding
#'
#' This function uses the lipid binding correction estimated by Pearce
#' et al. (2017) to decrease the fraction unbound in plasma
#' (\ifelse{html}{\out{f<sub>up</sub>}}{\eqn{f_{up}}}). This correction
#' assumes that there is additional in vivo binding to lipid, which
#' has a greater impact on neutral lipophilic compounds. 
#'
#' @param fup In vitro measured fraction unbound in plasma
#'
#' @param fup.correction Estimated correction to account for additional lipid
#' binding in vivo (Pearce et al., 2017) from \code{\link{calc_fup_correction}}
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
#' @param minimum.Funbound.plasma \ifelse{html}{\out{f<sub>up</sub>}}{\eqn{f_{up}}}
#' is not allowed to drop below this value (default is 0.0001).
#'
#' @return Fraction unbound in plasma adjusted to take into account binding
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
#' @seealso \code{\link{calc_fup_correction}}
#'
#' @export apply_fup_adjustment
apply_fup_adjustment <- function(fup, 
                         fup.correction=NULL,
                         Pow = NULL, 
                         pKa_Donor=NULL, 
                         pKa_Accept=NULL,
                         suppress.messages=FALSE,
                         minimum.Funbound.plasma=0.0001)
{
  if (is.null(fup.correction))
  {
    fup.correction <- calc_fup_correction(parameters=list(Pow = Pow,
                                          pKa_Donor = pKa_Donor,
                                          pKa_Accept = pKa_Accept))
  }
  fup.corrected <- max(fup * fup.correction,
                   minimum.Funbound.plasma,
                   na.rm = TRUE) # Enforcing a sanity check on 
                                             # plasma binding
  
  if (!suppress.messages) 
  {
    warning('Fup adjusted for in vivo lipid partitioning (Pearce, 2017), see calc_fup_correction.')
  }
  
  return(set_httk_precision(fup.corrected))
}