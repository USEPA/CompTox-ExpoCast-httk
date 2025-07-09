#' Parameterize Kramer IVD Model
#'
#' This function takes inputs from kramer_eval() and calls parameterize_IVD(). Converts units and sets up variables for kramer_eval().
#'
#'
#' @param tcdata A data table with well_number corresponding to plate format,
#' optionally include v_working, sarea, option.bottom, and option.plastic
#' 
#' @param this.FBSf Fraction fetal bovine serum 
#' 
#' @param this.prot_conc  Cell protein concentration (mg protein/million cells)
#' 
#' @param this.BSA Bovine serum albumin (BSA) concentration in serum (g/L)
#'
#' @param this.v_total Total volume per well (uL)
#'
#' @param casrn.vector A deprecated argument specifying a single or vector of 
#' Chemical Abstracts Service Registry 
#' Number(s) (CAS-RN) of desired chemical(s).
#'
#' @param this.BSA bovine serum albumin concentration (g/L)
#'
#' @param this.v_working Working volume per well (uL)
#' 
#' @param this.sarea Surface area per well (m^2)
#'
#' @param this.cell_yield Number of cells/well seeded
#'
#' @return A data table composed of any input data.table \emph{tcdata}
#' with only the following columns either created or altered by this function:
#' \tabular{ccc}{
#' \strong{Column Name} \tab \strong{Description} \tab \strong{Units} \cr
#' sarea \tab surface area \tab m^2 \cr
#' v_working_m3 \tab working (filled) volume of each well \tab m^3 \cr
#' v_total_m3 \tab total volume of each well \tab m^3 \cr
#' v_headspace_m3 \tab volume of headspace per well \tab m^3 \cr
#' conc_BSA \tab BSA concentration in media \tab kg/L \cr 
#' FBSp \tab Percent fetal bovine serum in media \tab percent \cr
#' conc_cell_mg \tab concentration of cell lipids \tab mg/m^3 \cr
#' conc_cell \tab concentration of cell lipids \tab kg/m^3 \cr 
#' conc_plastic \tab concentration of plastic \tab m2/m^3 \cr 
#' }
#'
#' @author Meredith Scherer
#'
#' @references 
#' \insertRef{kramer2010measuring}{httk}
#'
#' @import magrittr
#'
#' @export parameterize_kramer

parameterize_kramer <- function(tcdata = NA,                   #Data.table with casrn, FBSf
                                casrn.vector = NA_character_,  #CAS number
                                this.FBSf = NA_real_,          #Fraction of serum in media
                                this.BSA = 44,                 #BSA concentration in serum (g/L)
                                this.v_total = NA_real_,       #Total volume of well (uL)
                                this.v_working = NA_real_,     #Volume of medium/well (uL)
                                this.cell_yield = NA_real_,    #Number of cells/well seeded
                                this.sarea = NA_real_,         #Surface area of plastic exposed to medium (m^2)
                                this.prot_conc = 0.21          #Cell protein concentration (mg protein/million cells)
)



{
  #R CMD CHECK throws notes about "no visible binding for global variable", for
  #each time a data.table column name is used without quotes. To appease R CMD
  #CHECK, a variable has to be created for each of these column names and set to
  #NULL. Note that within the data.table, these variables will not be NULL! Yes,
  #this is pointless and annoying.
  well_number<-nomconc<-serum<-BSA<-BSA_kg<-v_total<-v_working<- NULL
  cell_yield<-prot_conc<-temp_k<-sarea<-casrn <-logHenry <- logWSol<-NULL
  this.restrict.ion.partitioning <- FBSf <- v_total_m3 <- v_working_m3 <- NULL
  v_headspace_m3 <- BSA2 <- conc_cell_mg <- conc_cell <- conc_plastic <- NULL
  FBSp <- conc_BSA <- NULL
  #End R CMD CHECK appeasement. 

  #### Set tcdata variables ####
  if(all(is.na(tcdata))){
    tcdata <- data.table(casrn = casrn.vector,
                         sarea = this.sarea,
                         cell_yield = this.cell_yield,
                         v_total = this.v_total,
                         v_working = this.v_working,
                         BSA = this.BSA,
                         FBSf = this.FBSf,
                         prot_conc = this.prot_conc)
  }


  #### Call parameterize_IVD ####
  p_IVD_output<- parameterize_IVD(tcdata)

  #merge the two
  p_Kramer_output <- merge(tcdata, p_IVD_output)

  #calculate FBSp from FBSf
  p_Kramer_output[,FBSp:=FBSf*100] #convert from decimal to percent

  #check for additional parameters we need
  manual.input.list <- list(prot_conc=this.prot_conc, BSA=this.BSA)
  req.list <- c("prot_conc","BSA")

  #fill in the missing parameters with the defaults
  if(!all(req.list%in%names(p_Kramer_output))){
    p_Kramer_output[,req.list[!(req.list %in% names(p_Kramer_output))]] <-
      manual.input.list[!(names(manual.input.list) %in% names(p_Kramer_output))]}

  #### System specific input parameters ####
  p_Kramer_output[, v_total_m3 := (v_total*convert_units("ul", "m3"))]  %>%   #total volume of each well (m^3) 
    .[, v_working_m3 := (v_working*convert_units("ul", "m3"))] %>%            #filled volume of each well (m^3)
    .[, v_headspace_m3 := (v_total_m3-v_working_m3)] %>%                     #volume of headspace per well (m^3)
    .[,conc_BSA := (BSA/1000)*(FBSp/100)] %>%                                #concentration BSA in media (kg/L)
    .[,conc_cell_mg := (cell_yield/1000000)*prot_conc*0.23/v_working_m3] %>% #concentration cell lipid (mg/m3)
    #0.23 mg lipid per mg protein (estimated by Gülden and Seibert 2002)
    .[,conc_cell := (conc_cell_mg*convert_units("mg", "kg"))] %>%                #concentration cell lipid (kg/m3)
    .[,conc_plastic := sarea/v_working_m3]                                   #concentration of plastic (m^2/m^3)


  #### Return data table ####
  return(p_Kramer_output)

}
