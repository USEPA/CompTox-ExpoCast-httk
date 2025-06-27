#' Parameterize Armitage In Vitro Distribution Model
#' 
#' @return
#' \tabular{lll}{
#' \strong{Param} \tab \strong{Description} \tab \strong{Units} \cr
#' casrn \tab Chemical Abstracts Service Registry Number \tab character \cr
#' logHenry \tab The log10 Henry's law constant \tab atm*m^3/mol \cr      
#' MP_C \tab The chemical compound's melting point \tab degrees C \cr           
#' MW \tab The chemical compound's molecular weight \tab g/mol \cr          
#' gkow_n \tab The log10 octanol to water (PC) (logP)\tab log10 unitless ratio \cr  
#' pKa_Donor \tab Chemical dissociation equilibrium constant(s); pKa(ie pKa_Donor) = -log10(Ka) \tab unitless \cr  
#' pKa_Accept \tab Chemical association equilibrium constant(s); pKb(ie pKa_Accept) = 14 - pKa  \tab unitless \cr 
#' pH \tab pH where ionization is evaluated (typically assay medium) \tab unitless \cr 
#' gkaw_n \tab The air to water PC (neutral) \tab unitless ratio \cr
#' gswat_n \tab The log10 water solubility at 25C (logWSol) \tab log10 mg/L \cr
#' }
#' 
#' @author Meredith Scherer
#' 
#' @import magrittr
#'
#' @export parameterize_armitage

parameterize_armitage <- function(tcdata = NA,                   #Data.table with casrn
                                  casrn.vector = NA_character_     #CAS number
)
{
  
  
  #### Set tcdata variables ####
  if(all(is.na(tcdata))){
    tcdata <- data.table(casrn = casrn.vector)
  }
  
  #### Call parameterize_IVD ####
  p_Armitage_output<- parameterize_IVD(tcdata)
  
  # Convert from chem.physical_and_invitro.data units to Armitage model units:
  p_Armitage_output[, "gswat_n" := logWSol + log10(MW*1000)] # log10 mol/L to log10 mg/L
  
  return(p_Armitage_output)

}

