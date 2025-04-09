#' Evaluate the Kramer In Vitro Distribution model
#' 
#' Evaluate the Kramer model for chemical distribution \emph{in vitro}. Takes input
#' as data table or vectors of values. Outputs a data table.
#' 
#' @param casrn description
#' 
#' @param nomconc description
#' 
#' @param well_number description
#' 
#' @param tcdata A data.table with casrn, nomconc,v_total, v_working. Otherwise supply single values to this.params (e.g., this.sarea,
#' this.v_total, etc.). Chemical parameters are taken from 
#' \code{\link{chem.physical_and_invitro.data}}.
#' 
#' @param gkow Log octanol to water PC (unitless)
#' 
#' @param Hconst Henry's law constant (atm*m^3/mol)
#' 
#' @param nomconc Nominal test concentration (uM)
#' 
#' @param v_total_m3 Total volume of well (L)
#' 
#' @param v_working_m3 Volume of medium per well (L)
#' 
#' @param cell_yield Number of cells/well seeded (unitless)
#' 
#' @param sarea Surface area of plastic exposed to medium (m^2)
#' 
#' @param temp_k Temperature (Kelvin)
#' 
#' @param prot_conc Cell protein concentration (mg protein/million cells)
#' 
#' @param serum Concentration of serum in media (percent volume/volume)
#' 
#' @param BSA BSA concentration in serum (g/L)
#' 
#' @param option.piechart Option to generate a pie chart to visualize chemical partitioning
#' 
#' @param restrict.ion.partitioning only allow neutral fraction to partition
#'
#' @return
#' \tabular{lll}{
#' \strong{Input Parameter} \tab \strong{Description} \tab \strong{Units} \cr
#' concentration_cells \tab Concentration in cells \tab uM \cr 
#' concentration_medium \tab Concentration in medium \tab uM \cr 
#' concentration_plastic \tab Concentration in plastic \tab umol/m^2 \cr 
#' concentration_air \tab Concentration in headspace \tab uM \cr 
#' 
#' @author Meredith Scherer, adapted from code written by L.S Lautz for A. Punt, N. Kramer
#'
#' @references Kramer, 2010. Measuring, Modeling, and Increasing the Free Concentration of Test Chemicals in Cell Assays. Utrecht University.
#'
#' @import magrittr
#'
#' @examples 
#' 
#' library(httk)
#' ...something here...
#' 
#' @export kramer_eval

##remove this later, just for testing the code :) 
#httk wd:
setwd("C:/Users/mscherer/httk-dev")

#load packages
library(tidyverse)
library(devtools) 
library(measurements) #for unit conversions
library(magrittr)
devtools::load_all("httk")

kramer_eval <- function(chem.cas=NULL,
                        chem.name=NULL,
                        dtxsid = NULL,
                        casrn.vector = NA_character_,  #CAS number
                        nomconc.vector = 1,            #Nominal concentration vector (uM)
                        this.well_number = 384,        #Number of wells per plate
                        tcdata = NA,                   #Data.table with casrn, nomconc, and well_number
                        this.serum = NA_real_,         #Concentration of serum in media (%)
                        this.gKow = NA_real_,          #Log octanol-water PC (unitless)
                        this.Hconst = NA_real_,        #Henry's law constant (atm*m^3/mol)
                        this.BSA = 44,                 #BSA concentration in serum (g/L)
                        this.v_total_m3 = NA_real_,    #Total volume of well (L)
                        this.v_working_m3 = NA_real_,  #Volume of medium/well (L)
                        this.cell_yield = NA_real_,    #Number of cells/well seeded
                        this.L_per_mil_cells = 2.772e-6, #Liters per 1 million cells
                        this.sarea = NA_real_,         #Surface area of plastic exposed to medium (m^2)
                        this.pH = 7,                   #pH of medium
                        this.temp_k = 298.15,          #Temperature (Kelvin)
                        this.prot_conc = 0.21,         #Cell protein concentration (mg protein/million cells)
                        this.option.bottom = TRUE,     #Include the bottom of the well in surface area calculation
                        this.option.plastic = FALSE,   #Automatically set surface area to zero
                        restrict.ion.partitioning = FALSE, #only allow the neutral fraction to partition
                        this.option.piechart = FALSE,   #Do not return pie chart
                        surface.area.switch = TRUE      #Calculate surface area of the well (assumes yes)
    )

  
  
{
  well_number<-nomconc<-serum<-BSA<-v_total<-v_working<-cell_yield<-NULL
  prot_conc<-temp_k<-sarea<-casrn<-NULL
  Fneutral <- ksalt<- csalt<- NULL

  if (all(is.na(tcdata)))
  {
    if (length(casrn.vector) > 1) chem.cas <- casrn.vector
    else if (!is.na(casrn.vector)) chem.cas <- casrn.vector
    
    if (is.null(chem.cas) & 
        is.null(chem.name) & 
        is.null(dtxsid)) 
      stop('chem.name, chem.cas, or dtxsid must be specified.')
    
    out <- get_chem_id(chem.cas=chem.cas,
                       chem.name=chem.name,
                       dtxsid=dtxsid)
    chem.cas <- out$chem.cas
    chem.name <- out$chem.name
    dtxsid <- out$dtxsid
    
    # set basic tcdata variables
    tcdata <- data.table(DTXSID = dtxsid,
                         Compound = chem.name,
                         casrn = chem.cas,
                         nomconc = nomconc.vector,
                         well_number = this.well_number,
                         sarea = this.sarea,
                         cell_yield = this.cell_yield,
                         v_total = this.vol_t,
                         v_working = this.vol_m,
                         BSA = this.BSA,
                         serum = this.serum,
                         prot_conc = this.prot_conc,
                         temp_k = this.temp_k,
                         L_per_mil_cells = this.L_per_mil_cells,
                         option.piechart = this.option.piechart)
  }


  #### Check user input for bare minimum the code needs to run #### 
  # Check CAS and nomconc supplied
  #if(any(is.na(tcdata[,.(casrn, nomconc)]))){
  #  stop("casrn and/or nomconc undefined")
  #}  
  
  # Check serum supplied
  #if(any(is.na(this.serum)) & !"serum" %in% names(tcdata)){
  #  stop("this.serum must be defined or serum must be a column in tcdata")
  #}
  
  #### Parameterize Kramer: ####
  tcdata <- parameterize_kramer(tcdata) #call parameterize_kramer(), overwrite tcdata with the updated variables

  #### Run Kramer Code: ####
  
  ##### Calculations for Partition Coefficients  ##### 
  
  R <- 8.2057338e-5  #Ideal Gas Constant (atm*m^3/mol/K)
  
  tcdata[,Ka := Hconst/(R*temp_k)] %>%    #Ka (air to water PC), unitless
    .[,Ks:= 10^(0.71*gkow+0.42)] %>%    #Ks (bovine serum albumin to water PC), L/kg BSA Endo and Goss 2011
    .[,Kp:=10^(0.97*gkow-6.94)] %>%     #Kp (plastic to water PC), m
    .[,Kc:=10^(1.25*gkow-3.7)]          #Kc (lipid to water PC), m3/kg cell lipid
  

  # Check if we allowed ionized molecules to partition into various in vitro components:
  
  # Check if we allowed ionized molecules to partition into various in vitro
  # components:
  if (restrict.ion.partitioning == FALSE)
  {
    # if not, allow all of the chemical to partition:
    tcdata[, Fneutral := 1]
  }
  
  #reassign values that are corrected for only the neutral fraction partitioning
  #value of alpha defines whether we allow the ionized portion to partition or only the fneutral 
  # (alpha = 1: both charged and neutral partition, alpha=0.0001: only neutral partitions) - MNS
  tcdata[,ksalt:=0.04*gkow+0.114] %>%  # Setschenow Constant, L/mol
    .[,csalt := 0.15 ] %>%  # ionic strength of buffer, mol/L
    .[,Ka := Fneutral*Ka/(10^(-1*ksalt*csalt))] %>% #correct for partitioning  - equivalent to kaw in Armitage
    .[,Ks := Fneutral*Ks/(10^(-1*ksalt*csalt))] %>% #correct for partitioning  - equivalent to kbsa in Armitage
    .[,Kc := Fneutral*Kc/(10^(-1*ksalt*csalt))] #correct for partitioning - equivalent to kcw in Armitage
  #kpl not adjusted in armitage (thus no kp here)

  
  ##### Calculations for Fractions, all unitless  ##### 
  
  tcdata[,frac_free := 1/(1+Ks*BSA2+Kp*conc_plastic+Kc*conc_cell+Ka*(vol_h/vol_m))] %>%  #Fraction free medium
    .[,frac_headspace:= (Ka*frac_free*vol_h)/vol_m] %>%               #Fraction absorbed into headspace
    .[,frac_plastic:= (Kp*frac_free*sarea)/vol_m] %>%                 #Fraction absorbed to plastic
    .[,frac_cells:= Kc*frac_free*conc_cell] %>%                       #Fraction absorbed to cells
    .[,frac_serum:= Ks*frac_free*BSA2] %>%                            #Fraction absorbed to serum
    .[,frac_equilib:= frac_free+frac_serum] %>%                       #Fraction in medium at equilibrium
    .[,mass_balance:= frac_equilib+frac_cells+frac_plastic+frac_headspace] #Mass balance needs to be 1
  
  ##### Calculations for Concentrations  ##### 
  
  tcdata[,system_umol := nomconc*(conv_unit(vol_m, "l", "ml"))] %>% #umol in the system
    .[,cell_umol := system_umol * frac_cells] %>% # umol in cell compartment
    .[,cellcompartment_L := (L_per_mil_cells*cell_yield)/1000000] %>% #volume of cells (liters)
    .[,concentration_cells := cell_umol/cellcompartment_L] %>% #concentration in cells (uM)
    .[,plastic_umol := system_umol * frac_plastic] %>% # umol in plastic compartment
    .[,concentration_plastic := plastic_umol/sarea] %>% #umol/m^2
    .[,air_umol := system_umol * frac_headspace] %>% # umol in headspace compartment
    .[,concentration_air := air_umol/(conv_unit(vol_h, "m3", "l")) ] %>% #concentration in headspace (uM)
    .[,concentration_medium:= nomconc*frac_free]      #concentration in medium (uM)
  
  # Check concentration_medium (umol/L) against water solubility (mol/L)
  tcdata[, "swat_mol" := 10^(logWSol+log10(1+(1-Fneutral/Fneutral)))] %>%  #account for the ionized portion of the chemical in the water solubility from https://docs.chemaxon.com/display/lts-europium/theory-of-aqueous-solubility-prediction.md and arnot email (and unlog it for convenience)
    .[,swat_umol := conv_unit(swat_mol, "mol", "umol")] %>% #convert swat_mol (mol/L) to umol/L
    .[concentration_medium>swat_umol,csat:=1] %>% #medium conc greater than solubility = saturated
    .[concentration_medium<=swat_umol,csat:=0] # medium conc less than solubility = unsaturated
  #csat: Is the solution saturated (yes = 1, no = 0) 
  
  return(tcdata)
  ##### Create data.frame for option.piechart  ##### 
  
  #if option.piechart=TRUE...
  #if(tcdata[option.piechart==TRUE]){

    
  #  for (chemical in tcdata$compound_name){
      
  #    temp_for_piechart<-data.frame(group = c("Free in medium", "Bound in plasma", "Associated with cells", "In headspace", "Soaked to well plastic"), values= t(as.data.frame(tcdata[compound_name==chemical,c("frac_free", "frac_serum", "frac_cells", "frac_headspace", "frac_plastic")])))                                                         #collect data
  #    temp_for_piechart$values<-round(temp_for_piechart$values, digits = 4) #round values
  #    temp_for_piechart$label<-scales::percent(temp_for_piechart$values) #create labels for percentages
      
  #    piechartPlot <- plot.piechart(temp_for_piechart) #call pie chart function
  #    }
  #    
  #}
  
  
}
