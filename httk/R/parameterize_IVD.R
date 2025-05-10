#' Parameterize In Vitro Distribution Models
#' 
#' This function collects information on chemicals and well plate geometry input by the user.
#' 
#' @param casrn.vector For vector or single value, CAS number
#' 
#' @param this.well_number For single value, plate format default is 384, used
#' if is.na(tcdata)==TRUE. This value chooses default surface area settings for
#' \code{\link{armitage_estimate_sarea}} based on the number of plates per well.
#' 
#' @param this.sarea Surface area of plastic exposed to medium (m^2)
#' 
#' @param this.v_total Total volume per well (uL)
#' 
#' @param this.v_working Working volume per well (uL)
#' 
#' @param this.cell_yield Number of cells per well
#' 
#' @param tcdata A data.table with casrn, nomconc, MP, gkow, gkaw, gswat, sarea,
#' v_total, v_working. Otherwise supply single values to this.params (e.g., this.sarea,
#' this.v_total, etc.). Chemical parameters are taken from 
#' \code{\link{chem.physical_and_invitro.data}}.
#' 
#' @param option.bottom If true (default), include the bottom of the well in surface area calculation
#' 
#' @param option.plastic If true (not default), automatically set well surface area to zero
#' 
#' @return
#' \tabular{lll}{
#' \strong{Param} \tab \strong{Description} \tab \strong{Units} \cr
#' casrn \tab Chemical Abstracts Service Registry Number \tab character \cr
#' well_number \tab Number of wells in plate (used to set default surface area) \tab unitless \cr   
#' sarea \tab Surface area of plastic exposed to medium \tab m^2 \cr         
#' v_total \tab Total volume of well \tab uL \cr       
#' v_working \tab Filled volume of well \tab uL \cr     
#' cell_yield \tab Number of cells \tab cells \cr
#' gkow \tab The log10 octanol to water (PC) (logP)\tab log10 unitless ratio \cr     
#' logHenry \tab The log10 Henry's law constant \tab atm*m^3/mol \cr      
#' gswat \tab The log10 water solubility at 25C (logWSol) \tab log10 mol/L \cr
#' MP \tab The chemical compound's melting point \tab degrees C \cr           
#' MW \tab The chemical compound's molecular weight \tab g/mol \cr            
#' }
#' 
#' @author Meredith Scherer
#' 
#' @examples
#' 
#' library(httk)
#' 
#' output <- parameterize_IVD(casrn.vector = c("15687-27-1"), this.well_number = 384)
#' print(output)
#' 
#' @export parameterize_IVD



parameterize_IVD <- function(tcdata = NA, # optionally supply columns v_working, sarea, option.bottom, and option.plastic
                             casrn.vector = NA_character_, #CAS number
                             nomconc.vector = NA_real_,    #nominal concentration
                             this.serum = NA_real_,        #serum concentration
                             this.well_number = 384,       #Number of wells per plate
                             this.sarea = NA_real_,        #Surface area of plastic exposed to medium (m^2)
                             this.cell_yield = NA_real_,   #Number of cells/well seeded
                             this.v_working = NA_real_,    #Volume of medium/well (uL)
                             this.v_total = NA_real_,      #Total volume of well (uL)
                             this.pH = 7, 
                             this.option.bottom = TRUE,    #Include the bottom of the well in surface area calculation
                             this.option.plastic = FALSE,  #Automatically set surface area to zero
                             surface.area.switch = TRUE)   #Calculate surface area of the well (assumes yes)
{
  #do not delete:
  sarea <- v_working <- v_total <- cell_yield <- logHenry <- NULL
  
  
  #### Set tcdata variables ####

  
  if(all(is.na(tcdata))){
    tcdata <- data.table(casrn = casrn.vector,
                         nomconc = nomconc.vector,
                         well_number = this.well_number,
                         sarea = this.sarea,
                         v_total = this.v_total,
                         v_working = this.v_working,
                         cell_yield = this.cell_yield)
  }
  

  

#  
#  if(!all(names(tcdata) %in% c("sarea", "v_total", "v_working", "cell_yield")) | #not all the things we need present
#     any(is.na(tcdata[,.(sarea, v_total, v_working, cell_yield)]))){              #or present and NA
#    
#    if(all(names(tcdata) %in% c("sarea", "v_total", "v_working", "cell_yield")) & #all names are present
#       any(is.na(tcdata[,.(sarea, v_total, v_working, cell_yield)]))){            #and any are nas
#      missing.rows <- which(is.na(tcdata[,sarea]))                                #create a table for those missing rows
#      
#    }else{                                                                        #not all names present or there are some nas
#      missing.rows <- 1:length(tcdata[,casrn])                                    #create a table for the missing rows
#    }
#    
#    if(any(is.na(tcdata[missing.rows, well_number]) & is.na(tcdata[missing.rows, sarea]))){
#      print(paste0("Either well_number or geometry must be defined for rows: ", 
#                   paste(which(tcdata[, is.na(sarea) & is.na(well_number)]),collapse = ",")))
#      stop()
#    }else{
#      temp <- armitage_estimate_sarea(tcdata[missing.rows,], option.bottom)      #get the values for the missing rows
#      
#      #fill in sarea
#      if(!(c("sarea") %in% (names(tcdata))) | (any(is.na(tcdata[missing.rows,.(sarea)])))){ #if surface area isnt in the list or is na
#        tcdata[missing.rows,"sarea"] <- temp[,"sarea"]
#      }
#      
#      #fill in v_total
#      if(!(c("v_total") %in% (names(tcdata))) | (any(is.na(tcdata[missing.rows,.(v_total)])))){ #if v_total isnt in the list or is na
#        tcdata[missing.rows,"v_total"] <- temp[,"v_total"]
#      }
#      
#      #fill in v_working
#      if(!(c("v_working") %in% (names(tcdata))) | (any(is.na(tcdata[missing.rows,.(v_working)])))){ #if v_working isnt in the list or is na
#        tcdata[missing.rows,"v_working"] <- temp[,"v_working"]
#      }
#      
#      #fill in cell_yield
#      if(!(c("cell_yield") %in% (names(tcdata))) | (any(is.na(tcdata[missing.rows,.(cell_yield)])))){ #if cell_yield isnt in the list or is na
#        tcdata[missing.rows,"cell_yield"] <- temp[,"cell_yield"]
#      }
#
#    }
#    
#  }

  
  #### Get PhysChem Parameters ####
  # Check if required phys-chem parameters are provided:
  if(!all(c("gkow","logHenry","gswat","MP","MW") %in% names(tcdata))){ #if these columns are not in tcdata
    # If not, pull them:
    tcdata[, c("gkow","logHenry","logWSol","MP","MW") := 
             as.data.frame(get_physchem_param(param = c("logP","logHenry","logWSol","MP","MW"), 
                                              chem.cas = casrn))] #, row.names = casrn
    
  }
  
  #check for pka donor and acceptor
  if (!all(c("pKa_Donor","pKa_Accept") %in% names(tcdata))){
    # If not present, pull them:
    tcdata[, c("pKa_Donor") := 
             as.data.frame(get_physchem_param(param = c("pKa_Donor"), 
                                              chem.cas = casrn),row.names = casrn)]
    tcdata[, c("pKa_Accept") := 
             as.data.frame(get_physchem_param(param = c("pKa_Accept"), 
                                              chem.cas = casrn),row.names = casrn)]
  }
  
  #check for pH
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
  tcdata[, "gkaw_n" := logHenry - log10(298.15*8.2057338e-5)] # log10 (atm*m3)/mol to (mol/m3)/(mol/m3) (unitless)
  #using ideal gas constant (R) = 8.2e-5 (m3*atm / (K*mol)) because logHenry is (atm*m3)/mol
  
  # Rename variables for both models
  tcdata[, "gkow_n" := gkow] %>% 
    .[,"MP_C":=MP]
  
  #### Return data table ####
  return(tcdata)
}

