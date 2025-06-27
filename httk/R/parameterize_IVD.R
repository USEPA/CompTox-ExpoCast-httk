#' Parameterize In Vitro Distribution Models
#' 
#' This function collects physicochemical properties from chemicals input by the user for use with armitage.R and kramer.R.
#' 
#' @param casrn.vector For vector or single value, CAS number
#' 
#' @param A data.table with casrn, logHenry, gswat, MP, MW, gkow, pKa_Donor,pKa_Accept, pH, and gkaw_n. 
#' Otherwise chemical parameters are taken from \code{\link{chem.physical_and_invitro.data
#' 
#' @param tcdata A data.table with casrn, nomconc, MP, gkow, gkaw, gswat, sarea,
#' v_total, v_working. Otherwise supply single values to this.params (e.g., this.sarea,
#' this.v_total, etc.). Chemical parameters are taken from 
#' \code{\link{chem.physical_and_invitro.data}}.
#'
#' @param this.ph pH of media
#' 
#' @return
#' \tabular{lll}{
#' \strong{Param} \tab \strong{Description} \tab \strong{Units} \cr
#' casrn \tab Chemical Abstracts Service Registry Number \tab character \cr
#' logHenry \tab The log10 Henry's law constant \tab atm*m^3/mol \cr      
#' gswat \tab The log10 water solubility at 25C (logWSol) \tab log10 mol/L \cr
#' MP_C \tab The chemical compound's melting point \tab degrees C \cr           
#' MW \tab The chemical compound's molecular weight \tab g/mol \cr          
#' gkow_n \tab The log10 octanol to water (PC) (logP)\tab log10 unitless ratio \cr  
#' pKa_Donor \tab Chemical dissociation equilibrium constant(s); pKa(ie pKa_Donor) = -log10(Ka) \tab unitless \cr  
#' pKa_Accept \tab Chemical association equilibrium constant(s); pKb(ie pKa_Accept) = 14 - pKa  \tab unitless \cr 
#' pH \tab pH where ionization is evaluated (typically assay medium) \tab unitless \cr 
#' gkaw_n \tab The air to water PC (neutral) \tab unitless ratio \cr 
#' }
#' 
#' @author Meredith Scherer
#' 
#' @examples
#' 
#' library(httk)
#' 
#' output <- parameterize_IVD(casrn.vector = c("15687-27-1"))
#' print(output)
#' 
#' @import magrittr
#' 
#' @export parameterize_IVD



parameterize_IVD <- function(tcdata = NA, # optionally supply columns logHenry, gswat, MP, MW, gkow, pKa_Donor,pKa_Accept, pH, and gkaw_n
                             casrn.vector = NA_character_, #CAS number
                              this.pH = 7
                             )
{
  #R CMD CHECK throws notes about "no visible binding for global variable", for
  #each time a data.table column name is used without quotes. To appease R CMD
  #CHECK, a variable has to be created for each of these column names and set to
  #NULL. Note that within the data.table, these variables will not be NULL! Yes,
  #this is pointless and annoying.
  sarea <- v_working <- v_total <- cell_yield <- logHenry <- NULL
  casrn <- pH <- Fneutral <- Fcharged <- Fpositive <- Fnegative <- gkow <- NULL
  MP <- NULL
  #End R CMD CHECK appeasement.
    
  #### Set tcdata variables ####
  
  if(all(is.na(tcdata))){
    tcdata <- data.table(casrn = casrn.vector)
  }
  

  
  #### Get PhysChem Parameters ####
  # Check if required phys-chem parameters are provided:
  if(!all(c("logHenry","gswat","MP","MW") %in% names(tcdata))){ #if these columns are not all in tcdata
    # If not, pull them:
    tcdata[, c("logHenry","logWSol","MP","MW") := 
             as.data.frame(get_physchem_param(param = c("logHenry","logWSol","MP","MW"), 
                                              chem.cas = casrn))]
    
  }
  
  
  # Check if logKow is provided (separate so user can enter if desired):
  if(!all(c("gkow") %in% names(tcdata))){ #if this column is not in tcdata
    # If not, pull it:
    tcdata[, c("gkow") := 
             as.data.frame(get_physchem_param(param = c("logP"), 
                                              chem.cas = casrn))] 
    
  } 
  
  
  # Check if pKa_Donor and pKa_Accept are provided:
  if (!all(c("pKa_Donor","pKa_Accept") %in% names(tcdata))){
    # If not present, pull them:
    tcdata[, c("pKa_Donor") := 
             as.data.frame(get_physchem_param(param = c("pKa_Donor"), 
                                              chem.cas = casrn),row.names = casrn)]
    tcdata[, c("pKa_Accept") := 
             as.data.frame(get_physchem_param(param = c("pKa_Accept"), 
                                              chem.cas = casrn),row.names = casrn)]
  }
  
  # Check if pH is provided:
  if (!(c("pH") %in% names(tcdata))){
    # If not present, auto assign:
    tcdata[,pH := this.pH]}
  
  # Calculate the fraction neutral:
  tcdata[, Fneutral := apply(.SD,1,function(x) calc_ionization(
    pH = as.numeric(x["pH"]),
    pKa_Donor = x["pKa_Donor"], 
    pKa_Accept = x["pKa_Accept"])[["fraction_neutral"]])]
  
  # Calculate the fraction charged:
  tcdata[, Fcharged := 1 - Fneutral]
  
  # Calculate the fraction positive:
  tcdata[, Fpositive := apply(.SD,1,function(x) calc_ionization(
    pH = this.pH,    
    pKa_Donor = x["pKa_Donor"], 
    pKa_Accept = x["pKa_Accept"])[["fraction_positive"]])]
  
  # Calculate the fraction negative:
  tcdata[, Fnegative := Fcharged - Fpositive]
  
  # Calculate gkaw for both models:
  if (!(c("gkaw_n") %in% names(tcdata))){
    # If not present, calculate:
    tcdata[, gkaw_n := logHenry - log10(298.15*8.2057338e-5)]} # log10 (atm*m3)/mol to (mol/m3)/(mol/m3) (unitless)
  #using ideal gas constant (R) = 8.2e-5 (m3*atm / (K*mol)) because logHenry is (atm*m3)/mol
  
  # Rename variables for both models
  tcdata[, gkow_n := gkow] %>% 
    .[, MP_C := MP] 
  
  # Delete unnecessary variables (because they were renamed)
  tcdata[, -c(gkow, MP)]
  
  #### Return data table ####
  return(tcdata)
}

