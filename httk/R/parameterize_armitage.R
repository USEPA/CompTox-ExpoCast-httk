#' Parameterize Armitage In Vitro Distribution Model
#' 
#' @param this.well_number For single value, plate format default is 384, used
#' if is.na(tcdata)==TRUE. This value chooses default surface area settings for
#' \code{\link{armitage_estimate_sarea}} based on the number of wells per plate.
#' 
#' @param tcdata A data.table with casrn, nomconc, MP, gkow, gkaw, gswat, sarea,
#' v_total, v_working. Otherwise supply single values to this.params (e.g., this.sarea,
#' this.v_total, etc.). Chemical parameters are taken from 
#' \code{\link{chem.physical_and_invitro.data}}.
#' 
#' @param this.sarea Surface area per well (m^2)
#' 
#' @param this.cell_yield For single value, optionally supply cell_yield,
#' otherwise estimated based on well number
#' 
#' @param casrn.vector A deprecated argument specifying a single or vector of 
#' Chemical Abstracts Service Registry 
#' Number(s) (CAS-RN) of desired chemical(s).
#' 
#' @param nomconc.vector For vector or single value, micromolar (uM = mol/L) nominal 
#' concentration (e.g. AC50 value)
#' 
#' @param this.well_number For single value, plate format default is 384, used
#' if is.na(tcdata)==TRUE. This value chooses default surface area settings for
#' \code{\link{armitage_estimate_sarea}} based on the number of wells per plate.
#' 
#' @return
#' \tabular{lll}{
#' \strong{Param} \tab \strong{Description} \tab \strong{Units} \cr
#' gkaw \tab The air to water PC \tab unitless ratio \cr  
#' logHenry \tab The log10 Henry's law constant \tab atm*m^3/mol \cr  
#' gswat \tab The log10 water solubility at 25C (logWSol) \tab log10 mg/L \cr
#' }
#' 
#' @author Meredith Scherer
#' 
#' @references
#' \insertRef{armitage2014application}{httk}
#' 
#' @export parameterize_armitage
parameterize_armitage <- function(tcdata = NA,                   #Data.table with casrn, nomconc, and well_number
                                  casrn.vector = NA_character_,  #CAS number
                                  nomconc.vector = 1,            #Nominal concentration vector (uM)
                                  this.well_number = 384,        #Number of wells per plate
                                  this.cell_yield = NA_real_,    #Number of cells/well seeded
                                  this.sarea = NA_real_         #Surface area of plastic exposed to medium (m^2)
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
    tcdata <- data.table(casrn = casrn.vector,
                         nomconc = nomconc.vector,
                         well_number = this.well_number,
                         sarea = this.sarea,
                         cell_yield = this.cell_yield)
  }
  

  
  #### Call parameterize_IVD ####
  p_Armitage_output<- parameterize_IVD(tcdata)
  
  
  # Convert from chem.physical_and_invitro.data units to Armitage model units:
  p_Armitage_output[, "gswat_n" := logWSol + log10(MW*1000)] # log10 mol/L to log10 mg/L
  
  return(p_Armitage_output)

}

