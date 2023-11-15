#' Calculate Dermal Equivalent Dose
#' 
#' This functions converts a steady state plasma concetration for a given
#' dermal exposure scenario to an equivalent steady state media concentration
#' for a single dose.
#' 
#' Returned dose is dependent on doses.per.day.
#' 
#' @param conc Bioactive in vitro concentration, arbitrary units.
#' 
#' @param chem.name Either the chemical name or the CAS number must be
#' specified. 
#' 
#' @param chem.cas Either the CAS number or the chemical name must be
#' specified.
#' 
#' @param parameters Parameters from parameterize_dermal_pbtk.
#' 
#' @param days Number of days of simulation.
#' 
#' @param doses.per.day Number of doses per day.
#' 
#' @param skin_depth Skin depth, cm.
#' 
#' @param skin.pH pH of skin/dermis.
#' 
#' @param Vmedia Volume of media, L, used when parameters are not given.
#' 
#' @param Fskinexposed Fraction of total skin exposed, used when parameters are
#' not given.
#' 
#' @param ... Additional parameters passed to solve_dermal_pbtk.
#' 
#' @author Annabel Meade
#' 
#' @return
#' Equivalent dose in specified units, default of mg/kg BW/day.
#' @export calc_dermal_equiv
calc_dermal_equiv <- function(conc,
                              chem.name=NULL,
                              chem.cas=NULL,
                              dtxsid=NULL,
                              parameters=NULL,
                              days=20,
                              doses.per.day=3,
                              skin_depth=0.3,
                              skin.pH=7,
                              Vmedia=0.001,
                              Fskinexposed=0.1,
                              ...)
{
  if(is.null(parameters)){
    parameters <- parameterize_dermal_pbtk(dtxsid=dtxsid,
                                           chem.name=chem.name,
                                           chem.cas=chem.cas,
                                           skin_depth=skin_depth,
                                           skin.pH=skin.pH)
    parameters$Vmedia <- Vmedia
    parameters$Fskinexposed <- Fskinexposed
  }
  out <- solve_dermal_pbtk(dtxsid=dtxsid,
                           chem.name=chem.name,
                           chem.cas=chem.cas,
                           days=days,
                           parameters=parameters,
                           doses.per.day=doses.per.day,
                           ...,
                           suppress.messages=T)
                           
  css <- out[[dim(out)[1],'AUC']] - out[[match(days - 1,out[,'time']),'AUC']]
  dose <- conc/css
  cat('Returned media concentration of same units as conc, units/L.\n')
  
  return(dose)
}
