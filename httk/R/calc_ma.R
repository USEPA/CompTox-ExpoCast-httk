#' Calculate the membrane affinity
#' 
#' Membrane affinity (MA) is the membrane:water partition coefficient. MA
#' chacterizes chemical partitioning into membranes formed
#' from neutral phospholipids (\ifelse{html}{\out{K<sub>nPL</sub>}}{\eqn{K_{nPL}}}). 
#' Pearce et al. (2017) compared five different methods for predicting 
#' membrane affinity using measured data for 59 compounds. The method of
#' Yun and Edgington (2013) was identified as the best: 
#' \ifelse{html}{\out{MA = 10^(1.294 + 0.304 * log<sub>10</sub>(P<sub>ow</sub>)}}{\deqn{MA = 10^(1.294 + 0.304 * log_{10}(P_{ow}))}}
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
#' @param suppress.messages Whether or not the output message is suppressed.
#' 
#' @param pfas.calibration Whether MA for chemicals in class PFAS should be
#' increased using the regression to the Droge (2019) dataset.
#'
#' @return A numeric fraction unpbound in plasma between zero and one
#'
#' @author John Wambaugh 
#'
#' @references 
#' Pearce, Robert G., et al. "Evaluation and calibration of high-throughput 
#' predictions of chemical distribution to tissues." Journal of pharmacokinetics 
#' and pharmacodynamics 44.6 (2017): 549-565.
#' 
#' Yun, Y. E., and A. N. Edginton. "Correlation-based prediction of 
#' tissue-to-plasma partition coefficients using readily available input 
#' parameters." Xenobiotica 43.10 (2013): 839-852.
#'
#' Droge, Steven TJ. "Membrane–water partition coefficients to aid risk 
#' assessment of perfluoroalkyl anions and alkyl sulfates." Environmental 
#' Science & Technology 53.2 (2018): 760-770.
#' 
#' @keywords parameters
#'
#' @export calc_ma
#'
calc_ma <- function(
                 chem.cas=NULL,
                 chem.name=NULL,
                 dtxsid = NULL,
                 parameters=NULL,
                 suppress.messages=FALSE,
                 pfas.calibration=TRUE
                 ) 
{
# We need to describe the chemical to be simulated one way or another:
  if (is.null(chem.cas) & 
      is.null(chem.name) & 
      is.null(dtxsid) &
      is.null(parameters)) 
    stop('Parameters, chem.name, chem.cas, or dtxsid must be specified.')

  if (is.null(parameters))
  {
    Pow <- 10^get_physchem_param("logP",
                                 chem.cas=chem.cas,
                                 dtxsid=dtxsid,
                                 chem.name=chem.name)
  } else {
    if (!("Pow" %in% names(parameters)))
    {
       stop("Pow must be provided if using argument parameters in calc_ma")
    }
    Pow <- parameters$Pow
  }
  
  if (!suppress.messages) warning(
      "Membrane affinity (MA) predicted with method of Yun and Edginton (2013), see calc_ma.\n")  
  
  MA <- 10^(1.294 + 0.304 * log10(Pow))
  
  # Calibration for PFAS based on Droge (2019) data:
  if (pfas.calibration & 
      regexpr("PFAS",get_physchem_param("Chemical.Class",dtxsid=dtxsid)!=-1))
  {
    MA <- 10^(-2.59 + 2.61*log10(MA))
    if (!suppress.messages) warning(
      "Membrane affinity for PFAS increased according to regresion on Droge (2019) data.\n")  
  }
  
  return(set_httk_precision(MA)) 
}
