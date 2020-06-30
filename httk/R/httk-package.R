#' httkpop: Virtual population generator for HTTK.
#' 
#' The httkpop package generates virtual population physiologies for use in 
#' population TK.
#' 
#' @name httkpop
#' @aliases httkpop httkpop-package
#' @section Main function to generate a population: 
#' If you just want to generate
#'   a table of (chemical-independent) population physiology parameters, use 
#'   \code{\link{httkpop_generate}}.
#'   
#' @section Using HTTK-Pop with HTTK: 
#' To generate a population and then run an 
#'   HTTK model for that population, the workflow is as follows: \enumerate{ 
#'   \item Generate a population using \code{\link{httkpop_generate}}. \item For
#'   a given HTTK chemical and general model, convert the population data to 
#'   corresponding sets of HTTK model parameters using 
#'   \code{\link{httkpop_mc}}.}
#'   
#'   
#' @import data.table
#' @docType package
#'
#'@keywords httk-pop
#'
#'@author Caroline Ring
#'
#'@references Ring, Caroline L., et al. "Identifying populations sensitive to 
#'environmental chemicals by simulating toxicokinetic variability." Environment 
#'International 106 (2017): 105-118
NULL




#' \Sexpr{tools:::Rd_package_title("httk")}
#' 
#' \Sexpr{tools:::Rd_package_description("httk")}
#'  
#' @name httk-package
#' @aliases httk-package httk
#' @docType package
#' @author John Wambaugh, Robert Pearce, Caroline Ring, Gregory Honda, Nisha
#' Sipes, Jimena Davis, Barbara Wetmore, Woodrow Setzer, Mark Sfeir
#' @seealso
#' \href{https://www.epa.gov/chemical-research/computational-toxicology-communities-practice-high-throughput-toxicokinetic-httk-r}{PowerPoint
#' Presentation: High-Throughput Toxicokinetics (HTTK) R package}
#' 
#' \href{https://doi.org/10.18637/jss.v079.i04}{Pearce et al. (2017): httk: R
#' Package for High-Throughput Toxicokinetics}
#' 
#' \href{https://doi.org/10.1093/toxsci/kfv171}{Wetmore et al. (2015):
#' Incorporating High-Throughput Exposure Predictions With Dosimetry-Adjusted
#' In Vitro Bioactivity to Inform Chemical Toxicity Testing}
#' 
#' \href{https://doi.org/10.1093/toxsci/kfv118}{Wambaugh et al. (2015):
#' Toxicokinetic Triage for Environmental Chemicals}
#' 
#' \href{https://doi.org/10.1007/s10928-017-9548-7}{Pearce et al. (2017):
#' Evaluation and calibration of high-throughput predictions of chemical
#' distribution to tissues}
#' 
#' \href{https://doi.org/10.1016/j.envint.2017.06.004}{Ring et al. (2017):
#' Identifying populations sensitive to environmental chemicals by simulating
#' toxicokinetic variability}
#' 
#' \href{https://doi.org/10.1021/acs.est.7b00650}{Sipes et al. (2017): An
#' Intuitive Approach for Predicting Potential Human Health Risk with the Tox21
#' 10k Library}
#' 
#' \href{https://doi.org/10.1093/toxsci/kfy020}{Wambaugh et al. (2018):
#' Evaluating In Vitro-In Vivo Extrapolation of Toxicokinetics}
#'
#' \href{https://doi.org/10.1371/journal.pone.0217564}{Honda et al. (2019):
#' Using the concordance of in vitro and in vivo data to evaluate extrapolation assumptionss}
#' 
#' \href{https://doi.org/10.1093/toxsci/kfz205}{Wambaugh et al. (2019):
#' Assessing Toxicokinetic Uncertainty and Variability in Risk Prioritization}
#' 
#' \href{https://doi.org/10.1038/s41370-020-0238-y}{Linakis et al. (2020):
#' Development and evaluation of a high throughput inhalation model for organic chemicals}
#' 
#' \href{https://www.epa.gov/chemical-research/rapid-chemical-exposure-and-dose-research}{EPA's
#' ExpoCast (Exposure Forecasting) Project}
#' @keywords package
NULL