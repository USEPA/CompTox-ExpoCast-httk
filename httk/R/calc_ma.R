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
#' @return A membrane:unbound fraction in plasma partition coefficient
#'
#' @author John Wambaugh 
#'
#' @references 
#' \insertRef{pearce2017evaluation}{httk}
#' 
#' \insertRef{yun2013correlation}{httk}
#'
#' \insertRef{droge2019pfasmembraneaffinity}{httk}
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

# Look up the chemical name/CAS, depending on what was provided:
  if (is.null(parameters))
  {
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
  } else {
    # Work with local copy of parameters in function(scoping):
    if (is.data.table(parameters)) parameters <- copy(parameters) 
  }

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
      "Membrane affinity (MA) predicted with method of Yun and Edginton (2013), see calc_ma.")  
  
  MA <- 10^(1.294 + 0.304 * log10(Pow))
  
  # Calibration for PFAS based on Droge (2019) data:
  if (pfas.calibration & 
      regexpr("PFAS", get_physchem_param("Chemical.Class",dtxsid=dtxsid))!=-1)
  {
    MA <- 10^(-2.59 + 2.61 * log10(MA))
    if (!suppress.messages) warning(
      "Membrane affinity for PFAS increased according to regression on Droge (2019) data.")  
  }
  
  return(set_httk_precision(MA)) 
}
