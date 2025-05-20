#' Parameterize Kramer IVD Model
#'
#' This function takes inputs from kramer_eval() and calls parameterize_IVD(). Converts units and sets up variables for kramer_eval().
#'
#'
#' @param tcdata A data table with well_number corresponding to plate format,
#' optionally include v_working, sarea, option.bottom, and option.plastic
#'
#' @param this.well_number For single value, plate format default is 384, used
#' if is.na(tcdata)==TRUE
#'
#' @param this.cell_yield For single value, optionally supply cell_yield,
#' otherwise estimated based on well number
#'
#' @param this.v_total For single value, optionally supply total volume,
#' otherwise estimated based on well number (uL)
#'
#' @param this.v_working For single value, optionally supply working volume,
#' otherwise estimated based on well number (uL)
#'
#' @return A data table composed of any input data.table \emph{tcdata}
#' with only the following columns either created or altered by this function:
#' \tabular{ccc}{
#' \strong{Column Name} \tab \strong{Description} \tab \strong{Units} \cr
#' well_number \tab number of wells on plate \tab \cr
#' sarea \tab surface area \tab m^2 \cr
#' cell_yield \tab number of cells \tab cells \cr
#' v_working_m3 \tab working (filled) volume of each well \tab uL \cr
#' v_total_m3 \tab total volume of each well \tab uL \cr
#' }
#'
#' @author Meredith Scherer
#'
#' @import magrittr
#'
#' @export parameterize_kramer

parameterize_kramer <- function(tcdata = NA,                   #Data.table with casrn, nomconc, and well_number
                                casrn.vector = NA_character_,  #CAS number
                                nomconc.vector = 1,            #Nominal concentration vector (uM)
                                this.well_number = 384,        #Number of wells per plate
                                this.BSA = 44,                 #BSA concentration in serum (g/L)
                                this.logKow = NA_real_,        #Log octanol-water PC (unitless)
                                this.logHenry = NA_real_,      #log10 Henry's law constant (atm*m^3/mol)
                                this.serum = NA_real_,         #Concentration of serum in media (%)
                                this.v_total = NA_real_,       #Total volume of well (uL)
                                this.v_working = NA_real_,     #Volume of medium/well (uL)
                                this.cell_yield = NA_real_,    #Number of cells/well seeded
                                this.sarea = NA_real_,         #Surface area of plastic exposed to medium (m^2)
                                this.temp_k = 298.15,          #Temperature (Kelvin)
                                this.prot_conc = 0.21,         #Cell protein concentration (mg protein/million cells)
                                this.option.bottom = TRUE,     #Include the bottom of the well in surface area calculation
                                this.option.plastic = FALSE,   #Automatically set surface area to zero
                                restrict.ion.partitioning = FALSE, # Should we restrict the partitioning concentration to neutral only?
                                surface.area.switch = TRUE       #Calculate surface area of the well (assumes yes)
)



{
  well_number<-nomconc<-serum<-BSA<-BSA_kg<-v_total<-v_working<- NULL
  cell_yield<-prot_conc<-temp_k<-sarea<-casrn <-logHenry <- logWSol<-NULL


  #### Set tcdata variables ####
  if(all(is.na(tcdata))){
    tcdata <- data.table(casrn = casrn.vector,
                         nomconc = nomconc.vector,
                         well_number = this.well_number,
                         sarea = this.sarea,
                         cell_yield = this.cell_yield,
                         v_total = this.v_total,
                         v_working = this.v_working,
                         BSA = this.BSA,
                         serum = this.serum,
                         prot_conc = this.prot_conc,
                         temp_k = this.temp_k,
                         restrict.ion.partitioning = this.restrict.ion.partitioning)
  }


  #### Call parameterize_IVD ####
  p_IVD_output<- parameterize_IVD(tcdata)

  #merge the two
  p_Kramer_output <- merge(tcdata, p_IVD_output)

  #rename serum
  p_Kramer_output[,serum:=FBSf*100] #convert from decimal to percent

  #check for additional parameters we need
  manual.input.list <- list(prot_conc=this.prot_conc, BSA=this.BSA)
  req.list <- c("prot_conc","BSA")

  #fill in the missing parameters with the defaults
  if(!all(req.list%in%names(p_Kramer_output))){
    p_Kramer_output[,req.list[!(req.list %in% names(p_Kramer_output))]] <-
      manual.input.list[!(names(manual.input.list) %in% names(p_Kramer_output))]}

  #### System specific input parameters ####
  p_Kramer_output[, v_total_m3 := (v_total*1.0E-9)]  %>%  #total volume of each well (m^3) #conv_unit(v_total, "ul", "m3")
    .[, v_working_m3 := (v_working*1.0E-9)] %>%           #filled volume of each well (m^3) #conv_unit(v_working, "ul", "m3")
    .[, v_headspace_m3 := (v_total_m3-v_working_m3)] %>%  #volume of headspace per well (m^3)
    .[,BSA_kg:= (BSA/1000)] %>%                           #BSA g/L to kg/L #conv_unit(BSA, "g", "kg")
    .[,BSA2 := BSA_kg*(serum/100)] %>%                                            #concentration of serum constituents (kg/L)
    .[,conc_cell_mg := (cell_yield/1000000)*prot_conc*0.23/v_working_m3] %>%             #concentration cell lipid (mg/m3)
    #0.23 mg lipid per mg protein (estimated by Gülden and Seibert 2002)
    .[,conc_cell := (conc_cell_mg/1e+6)] %>%                      #concentration cell lipid (kg/m3) #conv_unit(conc_cell_mg, "mg", "kg")
    .[,conc_plastic := sarea/v_working_m3]                                               #concentration of plastic (m^2/m^3)


  #### Return data table ####
  return(p_Kramer_output)

}
