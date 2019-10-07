#'Converts HTTK-Pop physiology into parameters relevant to the three
#' compartment steady-state model
#'
#'@param parameters.dt A data.table containing the physiological
#'  parameters as expected by HTTK (from \code{\link{httkpop_bio}}) and
#'  \code{Funbound.plasma} and \code{Clint} values (from
#'  \code{\link{draw_fup_clint}}).
#'
#'@return A data.table whose columns are the parameters of the HTTK model
#'  specified in \code{model}.
#'
#' @author Caroline Ring, John Wambaugh, and Greg Honda
#'
#'@references Ring, Caroline L., et al. "Identifying populations sensitive to 
#'environmental chemicals by simulating toxicokinetic variability." Environment 
#'International 106 (2017): 105-118
#'
#' @keywords httk-pop 3oompss  monte-carlo
#''
#' @import utils data.table
#'
#' @export convert_httkpop_3compss 
convert_httkpop_3compss <- function(
                             parameters.dt,
                             httkpop.dt,
                             ...)
{
#
#    calc_hep_params <- c(as.list(parameters.df[, list(Clint,
#                                                    Funbound.plasma,
#                                                    Fhep.assay.correction,
#                                                    million.cells.per.gliver,
#                                                    BW,
#                                                    Vliverc,
#                                                    Qtotal.liverc)]),
#                         liver.density=1.05,
#                         Dn=0.17)
#    parameters.df[,Rblood2plasma:=available_rblood2plasma(chem.cas=this.chem,
#      species='Human',
#      adjusted.Funbound.plasma=adjusted.Funbound.plasma)]

# For models that don't described first pass blood flow from the gut, need the
# unscaled hepatic clearanxce to cacluate a hepatic bioavailability 
# (Rowland, 1973):      
  parameters.dt[, Clmetabolismc:=
    httk::calc_hepatic_clearance(
      hepatic.model="unscaled",
      parameters=calc_hep_params,
      suppress.messages=TRUE,
      clint.pvalue.threshold=clint.pvalue.threshold)]
    
# For models that don't described first pass blood flow from the gut, need the
# total liver blood flow to cacluate a hepatic bioavailability (Rowland, 1973):
  parameters.dt[, Qlivertot:=Qcardiacc*(Qgutf+Qliverf)*BW^0.75] # L/h
}