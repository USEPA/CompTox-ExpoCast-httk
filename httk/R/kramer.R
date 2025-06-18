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
#' @param gkow_n Log octanol to water PC (unitless)
#' 
#' @param logHenry log10 Henry's law constant (atm*m^3/mol)
#' 
#' @param nomconc Nominal test concentration (uM)
#' 
#' @param v_total Total volume of well (uL)
#' 
#' @param v_working Volume of medium per well (uL)
#' 
#' @param cell_yield Number of cells/well seeded (unitless)
#' 
#' @param sarea Surface area of plastic exposed to medium (m^2)
#' 
#' @param Tsys System temperature (Celcius)
#' 
#' @param Tref Reference temperature (Kelvin)
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
#' }
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
# 


kramer_eval <- function(chem.cas=NULL,
                        chem.name=NULL,
                        dtxsid = NULL,
                        casrn.vector = NA_character_,  #CAS number
                        nomconc.vector = 1,            #Nominal concentration vector (uM)
                        this.well_number = 384,        #Number of wells per plate
                        tcdata = NA,                   #Data.table with casrn, nomconc, and well_number
                        user_assay_parameters = NA,    #Data.table with user-entered assay parameters (optional)
                        this.serum = NA_real_,         #Concentration of serum in media (%)
                        this.gKow = NA_real_,          #Log octanol-water PC (unitless)
                        this.Hconst = NA_real_,        #Henry's law constant (atm*m^3/mol)
                        this.csalt = 0.15,             # Ionic strength of buffer, mol/L
                        this.BSA = 44,                 #BSA concentration in serum (g/L)
                        this.v_total = NA_real_,       #Total volume of well (uL)
                        this.v_working = NA_real_,     #Volume of medium/well (uL)
                        this.cell_yield = NA_real_,    #Number of cells/well seeded
                        this.L_per_mil_cells = 2.772e-6, #Liters per 1 million cells
                        this.sarea = NA_real_,         #Surface area of plastic exposed to medium (m^2)
                        this.Tsys = 37,                #System temperature
                        this.Tref = 298.15,            #Reference temperature (equivalent to 25C)
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
  prot_conc<-temp_k<-sarea<-casrn<- csalt<- NULL
  
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
                         v_total = this.v_total,
                         v_working = this.v_working,
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
  
  #### Call Surface Area Function ####
  
  
  #### Call Surface Area Function ####
  
  #check surface area on/off
  if(surface.area.switch){
    if(!all(names(tcdata) %in% c("sarea", "v_total", "v_working", "cell_yield")) |
       any(is.na(tcdata[,.(sarea, v_total, v_working, cell_yield)]))){
      
      if(all(names(tcdata) %in% c("sarea", "v_total", "v_working", "cell_yield")) &
         any(is.na(tcdata[,.(sarea, v_total, v_working, cell_yield)]))){
        missing.rows <- which(is.na(tcdata[,sarea]))
      }else{
        missing.rows <- 1:length(tcdata[,casrn])
      }
      
      if(c("assay_component_endpoint_name") %in% names(tcdata) & (exists("user_assay_parameters"))){
        #if the code has the assay endpoints labeled and they have been provided
        
        #run the surface area code with the user entered assay parameters
        tcdata <- armitage_estimate_sarea(tcdata[missing.rows,], user_assay_parameters)
        
        #bind the surface area
        #tcdata<-merge(tcdata[missing.rows,], temp)
        
      }else if(c("assay_component_endpoint_name") %in% names(tcdata)){
        #if the code has the assay endpoints labeled but they have not been provided
        
        #run the surface area code and let it provide the standardized assay info (or error out)
        tcdata <- armitage_estimate_sarea(tcdata[missing.rows,])
        
        #bind the surface area
        #tcdata[missing.rows,] <- temp[tcdata[missing.rows,],on=.(assay_component_endpoint_name)]
        #tcdata[missing.rows,] <- temp
        #tcdata<-merge(tcdata[missing.rows,], temp)
        
      }else if(any(is.na(tcdata[missing.rows, well_number])) & !(c("assay_component_endpoint_name") %in% names(tcdata))){
        print(paste0("Either well_number or geometry must be defined for rows: ", 
                     paste(which(tcdata[, is.na(sarea) & is.na(well_number)]),
                           collapse = ",")))
        stop()
      }else{
        
        #run surface area code  
        temp <- armitage_estimate_sarea(tcdata[missing.rows,])
        
        if(any(is.na(tcdata[missing.rows,"sarea"]))){
          tcdata[missing.rows,"sarea"] <- temp[,"sarea"]
        }
        
        if(any(is.na(tcdata[missing.rows,"v_total"]))){
          tcdata[missing.rows,"v_total"] <- temp[,"v_total"]
        }
        
        if(any(is.na(tcdata[missing.rows,"v_working"]))){
          tcdata[missing.rows,"v_working"] <- temp[,"v_working"]
        }
        
        if(any(is.na(tcdata[missing.rows,"cell_yield"]))){
          tcdata[missing.rows,"cell_yield"] <- temp[,"cell_yield"]
        }
        
      }
      
    }
    
  }
  
  #### Parameterize Kramer: ####
  tcdata <- parameterize_kramer(tcdata) #call parameterize_kramer(), overwrite tcdata with the updated variables
  

  #add in the optional parameters:
  manual.input.list <- list(Tsys=this.Tsys, Tref=this.Tref, BSA=this.BSA,
                            L_per_mil_cells=this.L_per_mil_cells,
                            csalt=this.csalt, prot_conc = this.prot_conc)

  check.list <- c("ksalt")

  req.list <- c("Tsys","Tref", "BSA", "L_per_mil_cells", "csalt", "prot_conc")

  if(!all(check.list%in%names(tcdata))){
    tcdata[,check.list[!(check.list %in% names(tcdata))]] <- as.double(NA)}

  if(!all(req.list%in%names(tcdata))){
    tcdata[,req.list[!(req.list %in% names(tcdata))]] <-
      manual.input.list[!(names(manual.input.list) %in% names(tcdata))]}
  
  #Check if we allowed ionized molecules to partition into various in vitro
  # components:
  if (restrict.ion.partitioning == FALSE){
    # if not, allow all of the chemical to partition:
    tcdata[, Fneutral := 1] %>%
      .[, Fcharged := 0] %>%
      .[, Fpositive := 0] %>%
      .[, Fnegative := 0]
    }
  
  #### Run Kramer Code: ####
  
  ##### Calculations for Partition Coefficients  ##### 
  
  tcdata[, BSA2 := (BSA/1000)*(serum/100)] #calculate serum constituents
  
  tcdata[,gkbsa_n:= (0.71*gkow+0.42)] %>%   #Ks (bovine serum albumin to water PC), L/kg BSA Endo and Goss 2011
    .[,gkpl_n:=(0.97*gkow-6.94)] %>%        #Kp (plastic to water PC), m
    .[,gkcw_n:=(1.25*gkow-3.7)]             #Kc (lipid to water PC), m3/kg cell lipid
                                            #Ka (air to water PC), unitless - calculated in p_IVD.R
  
  ### Calculating Ionized Partition Coefficients ###
  # set up scaling factors (used to calculate PCs for the charged portion of the chemical)
  tcdata[,SFmw:=1] %>% # scaling factor for membrane-water (and cell-water)
    .[,SFbsa_acidic:=0] %>% # scaling factor for bsa-water (for acidic chemicals)
    .[,SFbsa_basic:=1] %>% # scaling factor for bsa-water (for basic chemicals)
    .[,SFplw:=3.5] # scaling factor for plastic-water

    # calculate partitioning properties of the charged form of the chemical
  tcdata[,gkcw_i:=(gkcw_n-SFmw)] %>% # gkcw_ionized - uses kmw SF
    .[,gkbsa_i_acidic:=(gkbsa_n-SFbsa_acidic)] %>% # gkbsa_ionized_acidic
    .[,gkbsa_i_basic:=(gkbsa_n-SFbsa_basic)] %>% # gkbsa_ionized_basic
    .[,gkpl_i:=(gkpl_n-SFplw)] # gkpl_ionized
  
  ### Calculate Setschenow salting-out constant (Ksalt) if not provided ###
  tcdata[is.na(ksalt),ksalt:=0.04*gkow_n+0.114] #Ni, N.; Yalkowsky, S. H., Prediction of Setschenow constants 2003

  ### System Temperature Correction ###
  #gkcw_n, gkcw_i, gkbsa_n, gkbsa_i, reference temperature is already 37 C (spLFER equations derived using logKow @ 25C)
  #need to correct gkaw_n (reference temperature is 25 C)

  #Adjust gKaw_n to match system temperature
  R <- 8.3144621  #Ideal Gas Constant units: J/(mol*K)
  
  tcdata[,Tsys:=Tsys+273.15] %>%  #convert from Celcius to Kelvin
    .[,Tcor:=((1/Tsys)-(1/Tref))/(2.303*R)] %>% # calculate temperature correction using van't Hoff approach (2.303 is from ln(10))
    .[,duaw:=60000] %>% # internal energy of phase change for air-water (J/mol)
    .[,gkaw_n_temp := gkaw_n-duaw*Tcor] #correct gkaw for temp

    #convert logged PCs to unlogged versions
  tcdata[,kcw_n := 10^(gkcw_n)] %>%
    .[,kcw_i := 10^(gkcw_i)] %>%
    .[,kbsa_n := 10^(gkbsa_n)] %>%
    .[,kbsa_i_acidic := 10^(gkbsa_i_acidic)] %>%
    .[,kbsa_i_basic := 10^(gkbsa_i_basic)] %>%
    .[,kaw_n := 10^(gkaw_n_temp)] %>%
    .[,kpl_n := 10^(gkpl_n)] %>%
    .[,kpl_i := 10^(gkpl_i)]
  
   ### Calculate pH dependent distribution ratios (DR) ###
  tcdata[Fneutral == 0, Fneutral := 0.00001] %>% #if Fneutral=0, reassign bc we use it to divide
    .[,DR_kcw:= (Fneutral*kcw_n) +(Fcharged*kcw_i)] %>% #DR kcw
    .[,DR_kbsa:= (Fneutral*kbsa_n)+(Fpositive*kbsa_i_basic)+(Fnegative*kbsa_i_acidic)] %>% # DR kbsa (depends on acid/base)
    .[,DR_kaw:= (Fneutral*kaw_n)] %>% # no dependence on ionization: charged form assumed to have negligible vapor pressure
    .[,DR_kpl:= (Fneutral*kpl_n)+(Fcharged*kpl_i)]# DR kpl
  

  ### Adjust DRs to account for salting out ###
  tcdata[,DR_kcw:= DR_kcw / 10^(-1*ksalt*csalt)] %>%
    .[,DR_kbsa:= DR_kbsa / 10^(-1*ksalt*csalt)] %>%
    .[,DR_kaw:= DR_kaw / 10^(-1*ksalt*csalt)] %>%
    .[,DR_kpl:= DR_kpl / 10^(-1*ksalt*csalt)]
  
  #convert DR variable names to og kramer names
  tcdata[,Ka := (DR_kaw)] %>%
    .[,Ks := (DR_kbsa)] %>%
    .[,Kp := (DR_kpl)] %>%
    .[,Kc := (DR_kcw)] 

  
  ##### Calculations for Fractions, all unitless  ##### 
  
  tcdata[,frac_free := 1/(1+Ks*BSA2+Kp*conc_plastic+Kc*conc_cell+Ka*(v_headspace_m3/v_working_m3))] %>%  #Fraction free medium
    .[,frac_headspace:= (Ka*frac_free*v_headspace_m3)/v_working_m3] %>%               #Fraction absorbed into headspace
    .[,frac_plastic:= (Kp*frac_free*sarea)/v_working_m3] %>%                 #Fraction absorbed to plastic
    .[,frac_cells:= Kc*frac_free*conc_cell] %>%                       #Fraction absorbed to cells
    .[,frac_serum:= Ks*frac_free*BSA2] %>%                            #Fraction absorbed to serum
    .[,frac_equilib:= frac_free+frac_serum] %>%                       #Fraction in medium at equilibrium
    .[,mass_balance:= frac_equilib+frac_cells+frac_plastic+frac_headspace] #Mass balance needs to be 1
  
  ##### Calculations for Concentrations  ##### 
  
  tcdata[,system_umol := nomconc*(v_working_m3*1000)] %>% #umol in the system #(conv_unit(v_working_m3, "m3", "l"))
    .[,cell_umol := system_umol * frac_cells] %>% # umol in cell compartment
    .[,cellcompartment_L := (L_per_mil_cells*cell_yield)/1000000] %>% #volume of cells (liters)
    .[,concentration_cells := cell_umol/cellcompartment_L] %>% #concentration in cells (uM)
    .[,plastic_umol := system_umol * frac_plastic] %>% # umol in plastic compartment
    .[,concentration_plastic := plastic_umol/sarea] %>% #umol/m^2
    .[,air_umol := system_umol * frac_headspace] %>% # umol in headspace compartment
    .[,concentration_air := air_umol/(v_headspace_m3 * 1000)] %>% #concentration in headspace (uM) #conv_unit(v_headspace_m3, "m3", "l")
    .[,concentration_medium:= nomconc*frac_free]      #concentration in medium (uM)
  
  # Check concentration_medium (umol/L) against water solubility (mol/L)
  tcdata[, "swat_mol" := 10^(logWSol+log10(1+(1-Fneutral/Fneutral)))] %>%  #account for the ionized portion of the chemical in the water solubility from https://docs.chemaxon.com/display/lts-europium/theory-of-aqueous-solubility-prediction.md and arnot email (and unlog it for convenience)
    .[,swat_umol := (swat_mol * 1000000)] %>% #convert swat_mol (mol/L) to umol/L #conv_unit(swat_mol, "mol", "umol")
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