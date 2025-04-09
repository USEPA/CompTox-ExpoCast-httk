#' Parameterize Armitage In Vitro Distribution Model
#' 
#' @return
#' \tabular{lll}{
#' \strong{Param} \tab \strong{Description} \tab \strong{Units} \cr
#' gkaw \tab The air to water PC \tab unitless ratio \cr  
#' logHenry \tab The log10 Henry's law constant \tab atm*m^3/mol \cr  
#' gswat \tab The log10 water solubility at 25C (logWSol) \tab log10 mg/L \cr
#' 
#' 
#' @author 
#' 
#' @import magrittr, measurements
#'
#' @export parameterize_armitage

parameterize_armitage <- function(tcdata = NA,                   #Data.table with casrn, nomconc, and well_number
                                casrn.vector = NA_character_,  #CAS number
                                nomconc.vector = 1,            #Nominal concentration vector (uM)
                                this.well_number = 384,        #Number of wells per plate
                                this.logKow = NA_real_,        #Log octanol-water PC (unitless)
                                this.cell_yield = NA_real_,    #Number of cells/well seeded
                                this.sarea = NA_real_,         #Surface area of plastic exposed to medium (m^2)
                                this.prot_conc = 0.21,         #Cell protein concentration (mg protein/million cells)
                                this.option.bottom = TRUE,     #Include the bottom of the well in surface area calculation
                                this.FBSf = NA_real_, # Must be set if not in tcdata, this is the most senstive parameter in the model.
                                this.Tsys = 37,
                                this.Tref = 298.15,
                                this.option.kbsa2 = FALSE,
                                this.option.swat2 = FALSE,
                                this.pseudooct = 0.01, # storage lipid content of cells
                                this.memblip = 0.04, # membrane lipid content of cells
                                this.nlom = 0.20, # structural protein content of cells
                                this.P_nlom = 0.035, # proportionality constant to octanol structural protein
                                this.P_dom = 0.05,# proportionality constant to octanol dom
                                this.P_cells = 1,# proportionality constant to octanol storage-liqid
                                this.csalt = 0.15, # ionic strength of buffer, M = mol/L
                                this.celldensity=1, # kg/L g/mL  mg/uL
                                this.cellmass = 3, #ng/cell
                                this.f_oc = 1, # everything assumed to be like proteins
                                this.conc_ser_alb = 24, # g/L mass concentration of albumin in serum
                                this.conc_ser_lip = 1.9, # g/L mass concentration of lipids in serum
                                this.Vdom = 0, # L the volume of dissolved organic matter (DOM)
                                this.option.plastic = FALSE,   #Automatically set surface area to zero
                                restrict.ion.partitioning = FALSE # Should we restrict the partitioning concentration to neutral only?
)
{

  
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
  
  #merge the two
  #p_Armitage_output <- merge(tcdata, p_IVD_output)
  

  
  # Convert from chem.physical_and_invitro.data units to Armitage model units:
  p_Armitage_output[, "gkaw" := logHenry - log10(298.15*8.2057338e-5)] # log10 atm-m3/mol to (mol/m3)/(mol/m3) (unitless)
  p_Armitage_output[, "gswat" := logWSol + log10(MW*1000)] # log10 mol/L to log10 mg/L 
  
  
  
  return(p_Armitage_output)

}

