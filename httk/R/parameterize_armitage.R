#' Parameterize Armitage In Vitro Distribution Model
#' 
#' @param tcdata A data.table with casrn, nomconc, MP, gkow, gkaw, gswat, sarea,
#' v_total, v_working. Otherwise supply single values to this.params (e.g., this.sarea,
#' this.v_total, etc.). Chemical parameters are taken from 
#' \code{\link{chem.physical_and_invitro.data}}.
#'
#' @param casrn.vector A deprecated argument specifying a single or vector of 
#' Chemical Abstracts Service Registry 
#' Number(s) (CAS-RN) of desired chemical(s).
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
#' @references
#' \insertRef{armitage2014application}{httk}
#' 
#' @import magrittr
#'
#' @export parameterize_armitage

parameterize_armitage <- function(tcdata = NA,                   #Data.table with casrn
                                  casrn.vector = NA_character_     #CAS number
)
{
  #R CMD CHECK throws notes about "no visible binding for global variable", for
  #each time a data.table column name is used without quotes. To appease R CMD
  #CHECK, a variable has to be created for each of these column names and set to
  #NULL. Note that within the data.table, these variables will not be NULL! Yes,
  #this is pointless and annoying.
  logWSol <- MW <- NULL
  #End R CMD CHECK appeasement.         
  
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

