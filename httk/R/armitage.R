#' Estimate well surface area
#' 
#' Estimate geometry surface area of plastic in well plate based on well plate
#' format suggested values from Corning.  option.plastic == TRUE (default) give
#' nonzero surface area (sarea, m^2) option.bottom == TRUE (default) includes
#' surface area of the bottom of the well in determining sarea.  Optionally
#' include user values for working volume (v_working, m^3) and surface area.
#' 
#' 
#' @param tcdata A data table with well_number corresponding to plate format,
#' optionally include v_working, sarea, option.bottom, and option.plastic OR 
#' with assay_component_endpoint_name corresponding to an entry in invitro.assay.params.
#' 
#' @param this.well_number For single value, plate format default is 384, used
#' if is.na(tcdata)==TRUE
#' 
#' @param this.cell_yield For single value, optionally supply cell_yield,
#' otherwise estimated based on well number
#' 
#' @param this.v_working For single value, optionally supply working volume,
#' otherwise estimated based on well number (m^3)
#' 
#' @param user_assay_parameters option to fill in your own assay parameters (data table)
#' 
#' @return A data table composed of any input data.table \emph{tcdata}
#' with only the following columns either created or altered by this function:  
#' \tabular{ccc}{
#' \strong{Column Name} \tab \strong{Description} \tab \strong{Units} \cr
#' well_number \tab number of wells on plate \tab \cr
#' sarea \tab surface area \tab m^2 \cr
#' cell_yield \tab number of cells \tab cells \cr 
#' v_working \tab working (filled) volume of each well \tab uL \cr
#' v_total \tab total volume of each well \tab uL \cr
#' }
#'
#' @author Greg Honda, Meredith Scherer
#'
#' @references 
#' \insertRef{armitage2014application}{httk} 
#'
#' @import magrittr
#'
#' @export armitage_estimate_sarea
armitage_estimate_sarea <- function(tcdata = NA, # optionally supply columns v_working,sarea, option.bottom, and option.plastic
                                    user_assay_parameters = NA, #option to fill in your own assay parameters (data table)
                                    this.well_number = 384,
                                    this.cell_yield = NA,
                                    this.v_working = NA){
  #R CMD CHECK throws notes about "no visible binding for global variable", for
  #each time a data.table column name is used without quotes. To appease R CMD
  #CHECK, a variable has to be created for each of these column names and set to
  #NULL. Note that within the data.table, these variables will not be NULL! Yes,
  #this is pointless and annoying.
  well_number<-well_desc<-radius<-diam<-v_working<-NULL
  v_working_est<-sysID<-height<-option.bottom<-sarea_c<-option.plastic<-NULL
  sarea<-cell_yield<-cell_yield_est<-NULL
  assay_component_endpoint_name <- NULL
  #End R CMD CHECK appeasement.
  
  if(all(is.na(tcdata))){
    tcdata <- data.table(well_number = this.well_number, cell_yield = this.cell_yield, v_working = this.v_working)
  }
  
  if(!(all(c("option.bottom","option.plastic") %in% names(tcdata)))){
    tcdata[,c("option.bottom","option.plastic")[!(c("option.bottom","option.plastic")%in%names(tcdata))]] <- as.logical(NA)
  }
  
  #if the user is taking advantage of the invitro.assay.params table:
  if(((c("assay_component_endpoint_name") %in% names(tcdata)))){
    
    #but they are missing information in their table or the assay is not in invitro.assay.params nor has it been added to user_assay_parameters
    if(any(is.na(tcdata[,.(assay_component_endpoint_name)])) | 
       (!(all((tcdata[,(assay_component_endpoint_name)]) %in% 
        httk::invitro.assay.params[,(assay_component_endpoint_name)])) &
       !(all((tcdata[,(assay_component_endpoint_name)]) %in% 
         user_assay_parameters[,(assay_component_endpoint_name)])))){
      
      #stop the code and give this error:
      stop("assay_component_endpoint_name must be defined for each row and must be a row in invitro.assay.params")
      
    }
    
    
    #if all the assay names are present, pull surface area and other info from either invitro.assay.params or user_assay_parameters 
    #table and assign to the appropriate assay_component_endpoint_name
    if (all((tcdata[,(assay_component_endpoint_name)]) %in% 
        invitro.assay.params[,(assay_component_endpoint_name)]))
    {
      tcdata <- 
        httk::invitro.assay.params[tcdata,on=.(assay_component_endpoint_name)]
    } else if (all((tcdata[,(assay_component_endpoint_name)]) %in% 
               user_assay_parameters[,(assay_component_endpoint_name)]))
    {
      tcdata <- user_assay_parameters[tcdata,on=.(assay_component_endpoint_name)]
    } else {
      #stop the code and give this error:
      stop("assay_component_endpoint_name present in both invitro.assay.params and user_assay_parameters")
    }
    
    #account for option.plastic and option.bottom
    tcdata[option.bottom==FALSE, sarea := 4*diam*height] %>% #overwrite sarea with the bottom area removed
      .[option.plastic==FALSE, sarea := 0] #overwrite sarea to zero
    
  }
  
  
  #if the user is not taking advantage of the invitro.assay.params table:
  if(!((c("assay_component_endpoint_name") %in% names(tcdata)))){
    
    # 
    # if(!(all(c("sarea","cell_yield","v_working")%in%names(tcdata)))){
    #   tcdata[,c("sarea","cell_yield","v_working")[!(c("sarea","cell_yield","v_working")%in%names(tcdata))]] <- as.double(NA)
    # }
  
    #set up well parameter table
    well.desc.list <- c("flat_bottom","standard","clear_flat_bottom")
    well.number.list <- c(6,12,24,48)
    well.param <- copy(well_param)[well_number %in% well.number.list |
                                     well_desc %in% well.desc.list]
    
    setnames(well.param,c("cell_yield","v_working"),c("cell_yield_est","v_working_est"))
    
    #assign well parameters to each row of tcdata
    tcdata <- well.param[tcdata,on=.(well_number)]
    
    #calculate surface area based on options and assign all variables
    tcdata[,radius:=diam/2] %>%  # mm
      .[is.na(v_working), v_working:=as.numeric(v_working_est)] %>%
      .[sysID %in% c(7,9), height:= v_working/(diam^2)] %>%  #mm for square wells
      .[is.na(option.bottom),option.bottom:=TRUE] %>%
      .[option.bottom==TRUE & (sysID %in% c(7,9)),sarea_c := 4*diam*height+diam^2] %>% #mm2
      .[option.bottom==FALSE & (sysID %in% c(7,9)),sarea_c := 4*diam*height] %>%
      .[!(sysID %in% c(7,9)),height:=v_working/(pi*radius^2)] %>% # for cylindrical wells
      .[option.bottom==TRUE & !(sysID %in% c(7,9)), sarea_c := 2*pi*radius*height+pi*radius^2] %>%  #mm2
      .[option.bottom==FALSE & !(sysID %in% c(7,9)), sarea_c := 2*pi*radius*height] %>%
      .[is.na(option.plastic),option.plastic:=TRUE] %>%
      .[,sarea_c:=sarea_c/1e6] %>% #mm2 to m2
      .[option.plastic==FALSE, sarea_c:=0] %>%
      .[is.na(sarea),sarea:=sarea_c] %>%
      .[is.na(cell_yield),cell_yield:=as.double(cell_yield_est)]
  }
  
  return(tcdata)
}


#' Armitage In Vitro Distribution Model
#' 
#' 
#' Evaluate the Armitage model for chemical distributon \emph{in vitro}. Takes input
#' as data table or vectors of values. Outputs a data table. Updates over
#' the model published in Armitage et al. (2014) include binding to plastic walls
#' and lipid and protein compartments in cells.
#' 
#' @param chem.name A single or vector of name(s)) of desired chemical(s).
#' @param chem.cas A single or vector of Chemical Abstracts Service Registry 
#' Number(s) (CAS-RN) of desired chemical(s).
#' @param dtxsid A single or vector ofEPA's DSSTox Structure ID(s) 
#' (\url{https://comptox.epa.gov/dashboard})  
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
#' @param this.FBSf Fraction fetal bovine serum, must be entered by user.
#' 
#' @param tcdata A data.table with casrn, nomconc, MP, gkow, gkaw, gswat, sarea,
#' v_total, v_working. Otherwise supply single values to this.params (e.g., this.sarea,
#' this.v_total, etc.). Chemical parameters are taken from 
#' \code{\link{chem.physical_and_invitro.data}}.
#' 
#' @param this.sarea Surface area per well (m^2)
#' 
#' @param this.v_total Total volume per well (uL)
#' 
#' @param this.v_working Working volume per well (uL)
#' 
#' @param this.cell_yield Number of cells per well
#' 
#' @param this.Tsys System temperature (degrees C)
#' 
#' @param this.Tref Reference temperature (degrees K)
#' 
#' @param this.option.kbsa2 Use alternative bovine-serum-albumin partitioning
#' model
#' 
#' @param this.option.swat2 Use alternative water solubility correction
#' 
#' @param this.option.kpl2 Use alternative plastic-water partitioning model
#' 
#' @param this.pseudooct Pseudo-octanol cell storage lipid content
#' 
#' @param this.memblip Membrane lipid content of cells
#' 
#' @param this.nlom Structural protein content of cells
#' 
#' @param this.P_nlom Proportionality constant to octanol structural protein
#' 
#' @param this.P_dom Proportionality constant to dissolve organic material
#' 
#' @param this.P_cells Proportionality constant to octanol storage lipid
#' 
#' @param this.csalt Ionic strength of buffer (M = mol/L)
#' 
#' @param this.celldensity Cell density kg/L, g/mL
#' 
#' @param this.cellmass Mass per cell, ng/cell
#'
#' @param this.f_oc Everything assumed to be like proteins
#' 
#' @param this.conc_ser_alb Mass concentration of albumin in serum (g/L)
#' 
#' @param this.conc_ser_lip Mass concentration of lipids in serum (g/L)
#' 
#' @param this.Vdom The volume of dissolved organic matter or DOM (mL)
#' 
#' @param this.pH pH of cell culture
#' 
#' @param this.Vdom 0 ml, the volume of dissolved organic matter (DOM)
#' 
#' @param this.cell_pH 7.4, pH of cell 
#' 
#' @param restrict.ion.partitioning FALSE, Should we restrict the chemical available to partition to only the neutral fraction?
#' 
#' @param surface.area.switch TRUE, automatically calculates surface area, switch to FALSE if user provided
#' 
#' @param user_assay_parameters option to fill in your own assay parameters (data table)
#' 
#' @param this.option.bottom Include the bottom of the well in surface area calculation
#' 
#' @param this.Anionic_VF Anionic phospholipid fraction
#' 
#' @param this.A_Prop_acid Sorption to anionic lipids - acidic chemicals
#' 
#' @param this.A_Prop_base Sorption to anionic lipids - basic chemicals
#' 
#' @param this.Lyso_VF lysosome volume fraction
#' 
#' @param this.Lyso_Diam diameter of lysosome (500 nm)
#' 
#' @param this.Lyso_pH pH of lysosome (5.1)
#'
#' @return
#' \tabular{lll}{
#' \strong{Param} \tab \strong{Description} \tab \strong{Units} \cr
#' casrn \tab Chemical Abstracts Service Registry Number \tab character \cr
#' nomconc \tab Nominal Concentration \tab uM=umol/L \cr       
#' well_number \tab Number of wells in plate (used to set default surface area) \tab unitless \cr   
#' sarea \tab Surface area of well \tab m^2 \cr         
#' v_total \tab Total volume of well \tab uL \cr       
#' v_working \tab Filled volume of well \tab uL \cr     
#' cell_yield \tab Number of cells \tab cells \cr    
#' assay_component_endpoint_name \tab link to invitro.assay.params table \tab character \cr  
#' gkow \tab The log10 octanol to water (PC) (logP)\tab log10 unitless ratio \cr          
#' logHenry \tab The log10 Henry's law constant '\tab log10 unitless ratio \cr      
#' gswat \tab The log10 water solubility (logWSol) \tab log10 mg/L \cr         
#' MP_C \tab The chemical compound melting point \tab degrees Celcius \cr  
#' MP_K \tab The chemical compound melting point \tab degrees Kelvin \cr        
#' MW \tab The chemical compound molecular weight \tab g/mol \cr            
#' gkaw \tab The air to water PC \tab unitless ratio \cr          
#' duow \tab internal energy of phase change for octanol-water \tab J/mol \cr          
#' duaw \tab internal energy of phase change for air-water \tab J/mol \cr          
#' gkmw \tab The log10 membrane to water PC \tab log10 unitless ratio \cr          
#' gkcw \tab The log10 cell/tissue to water PC \tab log10 unitless ratio\cr          
#' gkbsa \tab The log10 bovine serum albumin to water PC \tab log10 unitless ratio \cr         
#' gkpl \tab The log10 plastic to water PC \tab log10 m2/m2 \cr   
#' ksalt \tab Setschenow constant \tab L/mol \cr        
#' Tsys \tab System temperature \tab degrees C \cr          
#' Tref \tab Reference temperature\tab degrees K \cr          
#' option.kbsa2 \tab Use alternative bovine-serum-albumin partitioning model \tab logical \cr  
#' option.swat2 \tab Use alternative water solubility correction \tab logical \cr  
#' option.kpl2 \tab Use alternative plastic-water partitioning model \tab logical \cr 
#' FBSf \tab Fraction fetal bovine serum \tab unitless \cr          
#' pseudooct \tab Pseudo-octanol cell storage lipid content \tab \cr     
#' memblip \tab Membrane lipid content of cells \tab unitless \cr       
#' nlom \tab Structural protein content of cells \tab unitless \cr
#' P_nlom \tab Proportionality constant to octanol structural protein \tab unitless \cr   
#' P_dom \tab Proportionality constant to dissolved organic material (DOM) \tab unitless \cr         
#' P_cells \tab Proportionality constant to octanol storage lipid \tab unitless \cr      
#' Anionic_VF \tab Anionic phospholipid fraction \tab unitless \cr
#' A_Prop_acid \tab Sorption to anionic lipids - acidic chemicals \tab unitless \cr
#' A_Prop_base \tab Sorption to anionic lipids - basic chemicals \tab unitless \cr
#' Lyso_VF \tab Lysosome volume fraction \tab unitless \cr
#' Lyso_Diam \tab Diameter of lysosome \tab nm \cr
#' Lyso_pH \tab pH of lysosome \tab unitless \cr
#' csalt \tab Ionic strength of buffer \tab M=mol/L \cr
#' celldensity \tab Cell density \tab kg/L, g/mL \cr   
#' cellmass \tab Mass per cell \tab ng/cell \cr      
#' f_oc \tab Indicates fraction of dissolved organic matter to be treated like proteins \tab unitless \cr
#' cellwat \tab Fraction of the cell made up of water \tab unitless \cr       
#' Tcor \tab Temperature correction \tab \cr          
#' Vm \tab Volume of media \tab L \cr            
#' Vwell \tab Volume of medium (aqueous phase only) \tab L \cr         
#' Vair \tab Volume of head space \tab L \cr          
#' Vcells \tab Volume of cells/tissue\tab L \cr        
#' Valb \tab Volume of serum albumin \tab L \cr         
#' Vslip \tab Volume of serum lipids \tab L \cr         
#' Vdom \tab Volume of dissolved organic matter\tab L \cr          
#' F_ratio \tab Fugacity ratio \tab unitless \cr       
#' gs1.GSE \tab \tab \cr       
#' s1.GSE \tab \tab \cr        
#' gss.GSE \tab \tab \cr       
#' ss.GSE \tab \tab \cr        
#' kmw \tab The membrane to water PC (i.e., 10^gkmow \tab unitless \cr           
#' kow \tab The octanol to water PC (i.e., 10^gkow) \tab unitless \cr           
#' kaw \tab The air to water PC (i.e., 10^gkaw) \tab unitless \cr           
#' swat \tab The water solubility (i.e., 10^gswat) \tab mg/L \cr         
#' kpl \tab The plastic to water PC (i.e., 10^gkpl) \tab m3/m2 \cr           
#' kcw \tab The cell/tissue to water PC (i.e., 10^gkcw) \tab unitless \cr           
#' kbsa \tab The bovine serum albumin to water PC \tab unitless \cr          
#' swat_L \tab Water solubility limit used for Fugacity ratio calculation \tab \cr        
#' mtot \tab Total micromoles \tab umol \cr          
#' cwat \tab Total concentration in water \tab uM=umol/L \cr          
#' cwat_s \tab Dissolved concentration in water \tab uM=umol/L \cr        
#' csat \tab Is the solution saturated (1/0) \tab logical \cr         
#' activity \tab Chemical activity \tab \cr      
#' cair \tab Concentration in head space\tab uM=umol/L \cr          
#' calb \tab Concentration in serum albumin\tab uM=umol/L \cr          
#' cslip \tab Concentration in serum lipids\tab uM=umol/L \cr         
#' cdom \tab Concentration in dissolved organic matter\tab uM=umol/L \cr          
#' ccells \tab Concentration in cells\tab uM=umol/L \cr        
#' cplastic \tab Concentration in plastic\tab uM=umol/m^2 \cr      
#' mwat_s \tab Mass dissolved in water \tab umols \cr        
#' mair \tab Mass in air/head space \tab umols \cr          
#' mbsa \tab Mass bound to bovine serum albumin \tab umols \cr          
#' mslip \tab Mass bound to serum lipids \tab umols \cr        
#' mdom \tab Mass bound to dissolved organic matter \tab umols \cr          
#' mcells \tab Mass in cells \tab umols \cr        
#' mplastic \tab Mass bond to plastic \tab umols \cr      
#' mprecip \tab Mass precipitated out of solution \tab umols\cr       
#' xwat_s \tab Fraction dissolved in water \tab fraction \cr        
#' xair \tab Fraction in the air \tab fraction \cr          
#' xbsa \tab Fraction bound to bovine serum albumin \tab fraction \cr          
#' xslip \tab Fraction bound to serum lipids \tab fraction \cr         
#' xdom \tab Fraction bound to dissolved organic matter \tab fraction \cr          
#' xcells \tab Fraction within cells \tab fraction \cr        
#' xplastic \tab Fraction bound to plastic \tab fraction \cr     
#' xprecip \tab Fraction precipitated out of solution \tab fraction \cr       
#' eta_free \tab Effective availability ratio \tab fraction \cr      
#' \strong{cfree.invitro} \tab \strong{Free concentration in the in vitro media} (use for Honda1 and Honda2) \tab fraction \cr
#' }
#'
#' @author Greg Honda, Meredith Scherer adapeed from code by James Armitage, Jon Arnot
#'
#' @references
#' \insertRef{armitage2014application}{httk}
#' 
#' \insertRef{honda2019using}{httk} 
#'
#' @import magrittr
#'
#' @examples 
#'
#' library(httk)
#'
#' # Check to see if we have info on the chemical:
#' "80-05-7" %in% get_cheminfo()
#'
#' #We do:
#' temp <- armitage_eval(casrn.vector = c("80-05-7", "81-81-2"), this.FBSf = 0.1,
#' this.well_number = 384, nomconc = 10)
#' print(temp$cfree.invitro)
#'
#' # Check to see if we have info on the chemical:
#' "793-24-8" %in% get_cheminfo()
#' 
#' # Since we don't have any info, let's look up phys-chem from dashboard:
#' cheminfo <- data.frame(
#'   Compound="6-PPD",
#'   CASRN="793-24-8",
#'   DTXSID="DTXSID9025114",
#'   logP=4.27, 
#'   logHenry=log10(7.69e-8),
#'   logWSol=log10(1.58e-4),
#'   MP=	99.4,
#'   MW=268.404
#'   )
#'   
#' # Add the information to HTTK's database:
#' chem.physical_and_invitro.data <- add_chemtable(
#'  cheminfo,
#'  current.table=chem.physical_and_invitro.data,
#'  data.list=list(
#'  Compound="Compound",
#'  CAS="CASRN",
#'   DTXSID="DTXSID",
#'   MW="MW",
#'   logP="logP",
#'   logHenry="logHenry",
#'   logWSol="logWSol",
#'   MP="MP"),
#'   species="Human",
#'   reference="CompTox Dashboard 31921")
#' 
#' # Run the Armitage et al. (2014) model:
#' out <- armitage_eval(
#'   casrn.vector = "793-24-8", 
#'   this.FBSf = 0.1,
#'   this.well_number = 384, 
#'   nomconc = 10)
#'   
#' print(out)
#' 
#' @export armitage_eval
#' 

armitage_eval <- function(chem.cas=NULL,
                          chem.name=NULL,
                          dtxsid = NULL,
                          casrn.vector = NA_character_, # vector of CAS numbers
                          nomconc.vector = 1, # nominal concentration vector (e.g. apparent AC50 values) in uM = umol/L
                          this.well_number = 384,
                          this.FBSf = NA_real_, # Must be set if not in tcdata, this is the most sensitive parameter in the model.
                          tcdata = NA, # A data.table with casrn, ac50, and well_number or all of sarea, v_total, and v_working
                          user_assay_parameters = NA, # A data.table with user-entered assay parameters (optional)
                          this.sarea = NA_real_,
                          this.v_total = NA_real_,
                          this.v_working = NA_real_,
                          this.cell_yield = NA_real_,
                          this.Tsys = 37,
                          this.Tref = 298.15,
                          this.option.kbsa2 = FALSE,
                          this.option.swat2 = FALSE,
                          this.option.kpl2 = FALSE,
                          this.option.bottom = TRUE,
                          this.pseudooct = 0.01, # storage lipid content of cells
                          this.memblip = 0.04, # membrane lipid content of cells
                          this.nlom = 0.05, # structural protein content of cells
                          this.P_nlom = 0.035, # proportionality constant to octanol structural protein
                          this.P_dom = 0.05,# proportionality constant to octanol dom
                          this.P_cells = 1,# proportionality constant to octanol storage-liqid
                          this.cell_pH = 7.4, #cell pH
                          this.Anionic_VF = 0.175, #Anionic phospholipid fraction
                          this.A_Prop_acid = 0.05, #Sorption to anionic lipids - acidic chemicals
                          this.A_Prop_base = 20, #Sorption to anionic lipids - basic chemicals
                          this.Lyso_VF = 0.0068, #lysosome volume fraction
                          this.Lyso_Diam = 500, #diameter of lysosome (500 nm)
                          this.Lyso_pH = 5.1, #pH of lysosome (5.1)
                          this.csalt = 0.15, # ionic strength of buffer, M = mol/L
                          this.celldensity=1, # kg/L g/mL  mg/uL
                          this.cellmass = 3, #ng/cell
                          this.f_oc = 1, # everything assumed to be like proteins
                          this.conc_ser_alb = 24, # g/L mass concentration of albumin in serum
                          this.conc_ser_lip = 1.9, # g/L mass concentration of lipids in serum
                          this.Vdom = 0, # L the volume of dissolved organic matter (DOM)
                          this.pH = 7.0, # pH of cell culture
                          restrict.ion.partitioning = FALSE, # Should we restrict the partitioning concentration to neutral only?
                          surface.area.switch = TRUE #calculate surface area (assumes yes)
)
{
  #R CMD CHECK throws notes about "no visible binding for global variable", for
  #each time a data.table column name is used without quotes. To appease R CMD
  #CHECK, a variable has to be created for each of these column names and set to
  #NULL. Note that within the data.table, these variables will not be NULL! Yes,
  #this is pointless and annoying.
  sarea<-v_total<-v_working<-cell_yield<-NULL
  casrn <- well_number <- NULL
  Fneutral <- Fcharged <- Fpositive <- Fnegative <- IOC_Type <- NULL 
  csat <- activity <- cair <- calb <- cslip <- cdom <- ccells <- NULL
  cplastic <- mwat_s  <- mair <- mbsa  <- mslip <- mdom  <- mcells <- NULL
  mplastic  <- mprecip  <- xwat_s  <- xair <- xbsa  <- xslip <- xdom <- NULL
  xcells <- xplastic  <- xprecip  <- eta_free  <- cfree.invitro <- NULL
  swat_subcooled_liquid.GSE <- gswat_subcooled_liquid.GSE_25C <- NULL
  gswat_subcooled_liquid.GSE <- option.swat2 <- swat_L <- Vbm <- Vwell <- NULL
  Vcells <- cellmass <- celldensity <- Vair <- Valb <- FBSf <- NULL
  conc_ser_alb <- Vslip <- conc_ser_lip <- Vdom <- Vm <- mtot <- NULL
  nomconc <- cwat <- P_dom <- f_oc <- cwat_s <- NULL
  A_Prop_base <- cell_DR_kow <- csalt <- cell_DR_kmw <- IOC_mult <- NULL
  DR_kcw_preadj <- P_cells <- P_nlom <- pKa_Accept <- largest_pKa_Accept <- NULL
  Lyso_MemVF <- Lyso_Diam <- Lyso_Pump <- Lyso_pH <- DR_kcw <- Lyso_VF <- NULL
  DR_kow <- DR_kaw <- DR_kbsa <- DR_kpl <- DR_swat <- MP_K <- MP_C <- NULL
  F_ratio <- gswat_liquid.GSE <- swat_liquid.GSE <- gswat_solid.GSE <- NULL
  swat_solid.GSE <- ksalt <- Tsys <- Tcor <- Tref <- duow <- duaw <- NULL
  gkow_n_temp <- gkow_i_temp <- gkaw_n_temp <- gkaw_n <- gswat_n_temp <- NULL
  gswat_n <- Fneutral_cell <- Fpositive_cell <- Fcharged_cell <- NULL
  Fnegative_cell <- IOC_Type_cell <- kow_n <- kow_i <- swat_n <- kaw_n <- NULL
  kbsa_n <- kpl_n <- kmw_n <- cell_DR_kow_preadj <- cell_DR_kmw_preadj <- NULL
  Anionic_VF <- A_Prop_acid <- cellwat <- pseudooct <- memblip <- nlom <- NULL
  gkmw_n <- gkow_n <- option.kbsa2 <- gkbsa_n <- option.kpl2 <- gkpl_n <- NULL
  SFkow <- SFmw <- SFbsa_acidic <- SFbsa_basic <- SFplw <- gkow_i <- NULL
  kmw_i <- kbsa_i_acidic <- kbsa_i_basic <- kpl_i <- ksalt <- Tsys <- NULL
  Tcor <- Tref <- duow <- duaw <- NULL
  #End R CMD CHECK appeasement.
    
  #if no data table supplied
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
    
    tcdata <- data.table(DTXSID = dtxsid,
                         Compound = chem.name,
                         casrn = chem.cas,
                         nomconc = nomconc.vector,
                         well_number = this.well_number,
                         sarea = this.sarea,
                         v_total = this.v_total,
                         v_working = this.v_working,
                         cell_yield = this.cell_yield)
  }
  
  ### User Entered Parameter Checks ###
  
  # Check CAS and AC50 supplied
  #if(any(is.na(tcdata[,.(casrn,nomconc)]))){
  #  stop("casrn or nomconc undefined")
  #}  
  
  #add in the optional parameters:
  manual.input.list <- list(Tsys=this.Tsys, Tref=this.Tref,
                            option.kbsa2=this.option.kbsa2, 
                            option.swat2=this.option.swat2,
                            option.kpl2=this.option.kpl2,
                            option.bottom=this.option.bottom,
                            FBSf=this.FBSf, pseudooct=this.pseudooct, 
                            memblip=this.memblip,
                            nlom=this.nlom, P_nlom=this.P_nlom, 
                            P_dom=this.P_dom, P_cells=this.P_cells,
                            Anionic_VF=this.Anionic_VF, 
                            A_Prop_acid=this.A_Prop_acid,
                            A_Prop_base=this.A_Prop_base,
                            Lyso_VF=this.Lyso_VF, Lyso_Diam=this.Lyso_Diam,
                            Lyso_pH=this.Lyso_pH, 
                            csalt=this.csalt, celldensity=this.celldensity, 
                            cellmass=this.cellmass, f_oc=this.f_oc,
                            conc_ser_alb = this.conc_ser_alb, 
                            conc_ser_lip = this.conc_ser_lip, Vdom = this.Vdom)
  
  check.list <- c("duow","duaw", "SFkow", "SFmw", "SFbsa_acidic", "SFbsa_basic", "SFplw",
                  "gkmw_n","gkbsa_n","gkpl_n","ksalt")
  
  req.list <- c("Tsys","Tref","option.kbsa2","option.swat2", "option.kpl2",
                "option.bottom", "option.plastic",
                "FBSf","pseudooct","memblip","nlom","P_nlom","P_dom","P_cells",
                "Anionic_VF", "A_Prop_acid", "A_Prop_base", "Lyso_VF", 
                "Lyso_Diam", "Lyso_pH", "csalt","celldensity",
                "cellmass","f_oc","conc_ser_alb", "conc_ser_lip","Vdom")
  
  if(!all(check.list%in%names(tcdata))){
    tcdata[,check.list[!(check.list %in% names(tcdata))]] <- as.double(NA)}
  
  if(!all(req.list%in%names(tcdata))){
    tcdata[,req.list[!(req.list %in% names(tcdata))]] <- 
      manual.input.list[!(names(manual.input.list) %in% names(tcdata))]}
  
  
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

  
  #final check after surface area function
  if(any(is.na(this.FBSf)) & !"FBSf" %in% names(tcdata)){
    stop("this.FBSf must be defined or FBSf must be a column in tcdata")
  }
  
  
  #### Parameterize Armitage: ####
  tcdata <- parameterize_armitage(tcdata) #call parameterize_armitage(), overwrite tcdata with the updated variables
  
  #### Run Armitage Code: ####
  
  # Check if we allowed ionized molecules to partition into various in vitro
  # components:
  if (restrict.ion.partitioning == FALSE)
  {
    # if not, allow all of the chemical to partition:
    tcdata[, Fneutral := 1] %>% 
      .[, Fcharged := 0] %>% 
      .[, Fpositive := 0] %>% 
      .[, Fnegative := 0]
  }
  
  #characterize chemical by ionization state
  tcdata[Fneutral > 0.5, IOC_Type := "Neutral"] %>% 
    .[Fneutral < 0.5 & Fpositive > Fnegative, IOC_Type := "Base"] %>% 
    .[Fneutral < 0.5 & Fnegative > Fpositive, IOC_Type := "Acid"]
  
  R <- 8.3144621 # set gas law constant - units: J/(mol*K)
  
  #Calculate the fraction of the cell made up of water 
  #(ie not storage or membrane lipid or structural protein)  
  tcdata[,cellwat := 1-(pseudooct+memblip+nlom)]
  
  ### Single Parameter Linear Free Energy Relationships (spLFERs) ###
  # Use spLFERs to calculate partition coefficients not provided by user:
  # kmw (membrane-water PC) spLFER
  tcdata[is.na(gkmw_n), gkmw_n:=1.01*gkow_n + 0.12]
        #source: Capacities of Membrane Lipids to Accumulate Neutral Organic Chemicals (Endo, 2011)
  
  # kbsa (bovine serum albumin-water PC) spLFER
  tcdata[option.kbsa2==TRUE & is.na(gkbsa_n) & gkow_n<4.5, gkbsa_n:=(1.08*gkow_n-0.7)] %>% # option 1 for kbsa calc
    .[option.kbsa2==TRUE & is.na(gkbsa_n) & gkow_n>=4.5, gkbsa_n:=(0.37*gkow_n+2.56)] %>% 
        #source: Figure 2 from Serum Albumin Binding of Structurally Diverse Neutral Organic Compounds: Data and Models (Endo + Goss 2011)
    .[option.kbsa2==FALSE & is.na(gkbsa_n),gkbsa_n:=(0.71*gkow_n+0.42)] # option 2 for kbsa calc
        #source: Serum Albumin Binding of Structurally Diverse Neutral Organic Compounds: Data and Models (Endo + Goss 2011)
  
  # kpl (plastic-water PC) spLFER 
  tcdata[option.kpl2==FALSE & is.na(gkpl_n),gkpl_n:=0.97*gkow_n-6.94] %>% #from Kramer 2012 (Kramer, N.I. Measuring, Modeling, and Increasing the Free Concentration of Test Chemicals in Cell Assays)
    .[option.kpl2==TRUE & is.na(gkpl_n),gkpl_n:=0.56*gkow_n-4.635] #from Fischer 2018 (Application of Experimental Polystyrene Partition Constants andDiﬀusion Coeﬃcients to Predict the Sorption of Neutral OrganicChemicals to Multiwell Plates in in Vivo and in Vitro Bioassay)
  
  ### Calculating Ionized Partition Coefficients ###
  # set up scaling factors (used to calculate PCs for the charged portion of the chemical)
  tcdata[is.na(SFkow),SFkow:=3.5] %>% # scaling factor for octanol-water
    .[is.na(SFmw),SFmw:=1] %>% # scaling factor for membrane-water
    .[is.na(SFbsa_acidic),SFbsa_acidic:=0] %>% # scaling factor for bsa-water (for acidic chemicals)
    .[is.na(SFbsa_basic),SFbsa_basic:=1] %>% # scaling factor for bsa-water (for basic chemicals)
    .[is.na(SFplw),SFplw:=3.5] # scaling factor for plastic-water
  
  # calculate partitioning properties of the charged form of the chemical
  tcdata[,gkow_i:=(gkow_n-SFkow)] %>% # gkow_ionized (logged)
    .[,kmw_i:=10^(gkmw_n-SFmw)] %>% # kmw_ionized (unlogged)
    .[,kbsa_i_acidic:=10^(gkbsa_n-SFbsa_acidic)] %>% # kbsa_ionized_acidic (unlogged)
    .[,kbsa_i_basic:=10^(gkbsa_n-SFbsa_basic)] %>% # kbsa_ionized_basic (unlogged)
    .[,kpl_i:=10^(gkpl_n-SFplw)] # kpl_ionized (unlogged)
  
  ### Calculate Setschenow salting-out constant (Ks) if not provided ###
  tcdata[is.na(ksalt),ksalt:=0.04*gkow_n+0.114] #Ni, N.; Yalkowsky, S. H., Prediction of Setschenow constants 2003
  
  ### System Temperature Correction ###
  #gkmw_n, gkmw_i, gkbsa_n, gkbsa_i, reference temperature is already 37 C (spLFER equations derived using logKow @ 25C)
  #need to correct gkow_n, gkow_i, and gkaw_n (reference temperature is 25 C)
  
  #Adjust gKow_n, gkow_i, gKaw_n, and gswat_n to match system temperature
  tcdata[,Tsys:=Tsys+273.15] %>%  #convert from Celcius to Kelvin
    .[,Tcor:=((1/Tsys)-(1/Tref))/(2.303*R)] %>% # calculate temperature correction using van't Hoff approach (2.303 is from ln(10))
    .[is.na(duow),duow:=-20000] %>% # internal energy of phase change for octanol-water (J/mol)
    .[is.na(duaw),duaw:=60000] %>% # internal energy of phase change for air-water (J/mol)
    .[,gkow_n_temp := gkow_n-duow*Tcor] %>%  #correct gkow_n for temp 
    .[,gkow_i_temp := gkow_i-duow*Tcor] %>%  #correct gkow_i for temp 
    .[,gkaw_n_temp := gkaw_n-duaw*Tcor] %>% #correct gkaw for temp 
    .[,gswat_n_temp := gswat_n-(-1*duow)*Tcor] #correct gswat for temp 
  
  ### Calculate ~CELL SPECIFIC~ pH dependent distribution ratios (DR) ###
  #fraction neutral/charged at cell pH
  tcdata[, Fneutral_cell := apply(.SD,1,function(x) calc_ionization(
    pH = this.cell_pH,    
    pKa_Donor = x["pKa_Donor"], 
    pKa_Accept = x["pKa_Accept"])[["fraction_neutral"]])] %>% 
    .[, Fpositive_cell := apply(.SD,1,function(x) calc_ionization(
      pH = this.cell_pH,    
      pKa_Donor = x["pKa_Donor"], 
      pKa_Accept = x["pKa_Accept"])[["fraction_positive"]])] %>% 
    .[, Fcharged_cell := 1- Fneutral_cell] %>% 
    .[, Fnegative_cell := Fcharged_cell - Fpositive_cell]
  
  #characterize chemical by ionization state (for cell pH)
  tcdata[Fneutral_cell > 0.5, IOC_Type_cell := "Neutral"] %>% 
    .[Fneutral_cell < 0.5 & Fpositive_cell > Fnegative_cell, IOC_Type_cell := "Base"] %>% 
    .[Fneutral_cell < 0.5 & Fnegative_cell > Fpositive_cell, IOC_Type_cell := "Acid"]
  
  #convert logged PCs to unlogged versions
  tcdata[,kow_n := 10^(gkow_n_temp)] %>% 
    .[,kow_i := 10^(gkow_i_temp)] %>% 
    .[,swat_n := 10^(gswat_n_temp)] %>% 
    .[,kaw_n := 10^(gkaw_n_temp)] %>% 
    .[,kbsa_n := 10^(gkbsa_n)] %>% 
    .[,kpl_n := 10^(gkpl_n)] %>% 
    .[,kmw_n:=10^(gkmw_n)] #kmw_i already done above
  
  #calculate anionic specific membrane-water distribution ratio
  tcdata[,cell_DR_kow_preadj:= (Fneutral_cell*kow_n)+(Fcharged_cell*kow_i)] %>% # DR kow - cell
    .[,cell_DR_kmw_preadj:= (1-Anionic_VF)*(Fneutral_cell*kmw_n+Fcharged_cell*kmw_i)+(Anionic_VF)*(Fneutral_cell*kmw_n + Fnegative_cell*kmw_i*A_Prop_acid + Fpositive_cell*kmw_i*A_Prop_base)] # DR kmw - cell
  
  ### Adjust cell DRs to account for salting out ###
  tcdata[,cell_DR_kow := cell_DR_kow_preadj / 10^(-1*ksalt*csalt)] %>% 
    .[,cell_DR_kmw := cell_DR_kmw_preadj / 10^(-1*ksalt*csalt)]
  
  ### Calculate Cell Distribution Ratio (DR_kcw) ###
  # IOC_mult accounts for ion trapping
  tcdata[IOC_Type_cell=="Neutral", IOC_mult:= 1] %>% #Neutral =  1
    .[IOC_Type_cell=="Acid", IOC_mult:= 10^(this.cell_pH-this.pH)] %>% #Acid = 10^(cell_pH - system_pH)
    .[IOC_Type_cell=="Base", IOC_mult:= 10^(this.pH-this.cell_pH)] %>% #Base = 10^(system_pH - cell_pH)
    .[,DR_kcw_preadj:= (P_cells * pseudooct * cell_DR_kow + 
                          memblip  * cell_DR_kmw + 
                          P_nlom * nlom * cell_DR_kow + 
                          cellwat) * IOC_mult] %>% 
    .[,cell_DR_kmw:=cell_DR_kmw*IOC_mult] #correct cell_kmw to account for ion trapping
  
  ### Adjust DR_kcw to account for lysosomal trapping ###
  
  #split up pKa_Accept for the bases to get the highest value
  tcdata[IOC_Type_cell=="Base" & is.character(pKa_Accept) & regexpr(",",pKa_Accept)!=-1,
         largest_pKa_Accept := suppressWarnings(max(as.numeric(unlist(strsplit(pKa_Accept, ","))))), 
         by = seq_len(nrow(tcdata[IOC_Type_cell=="Base" & is.character(pKa_Accept) & regexpr(",",pKa_Accept)!=-1,]))] %>%  #pull out largest value
    .[IOC_Type_cell=="Base" & is.character(pKa_Accept) & (pKa_Accept != " ") & is.na(largest_pKa_Accept), 
      largest_pKa_Accept:=as.numeric(pKa_Accept),
      by = seq_len(nrow(tcdata[IOC_Type_cell=="Base" & is.character(pKa_Accept) & (pKa_Accept != " ") & is.na(largest_pKa_Accept),]))]  #if only one pka accept value, set that at the largest value
  #if there are no lines that fit the criteria, we wont use largest pka (line 702) so do not need to set to zero
  
  #Account for lysosomal trapping with Lyso_pump; sorption to anionics already accounted for in Dmw
  tcdata[, Lyso_MemVF:= (((4/3*pi*(Lyso_Diam/2)^3)-(4/3*pi*((Lyso_Diam/2)-5)^3))/(4/3*pi*(Lyso_Diam/2)^3))] %>% #lysosome membrane volume fraction
    .[IOC_Type_cell=="Neutral" | IOC_Type_cell=="Acid", Lyso_Pump:= 1] %>% #sequestration factor
    .[IOC_Type_cell=="Base", Lyso_Pump:= ((1 + 10^(largest_pKa_Accept - Lyso_pH)) / (1 + 10^(largest_pKa_Accept - this.cell_pH)))] %>% #sequestration factor
    .[, DR_kcw := (1 - Lyso_VF) * (DR_kcw_preadj) + Lyso_VF * (Lyso_MemVF * cell_DR_kmw * IOC_mult * Lyso_Pump)]
  #	Lyso_Pump: Neutral = 1, Acid = 1, Base = (1 + 10^(pKa - Lyso_pH)) / (1 + 10^(pKa - cell_pH))
  
  ### End of cell-specific distribution ratio calculation ###
  
  ### Calculate pH dependent distribution ratios (DR) ###
  tcdata[Fneutral == 0, Fneutral := 0.00001] %>% #if Fneutral=0, reassign bc we use it to divide
    .[,DR_kow:= (Fneutral*kow_n) +(Fcharged*kow_i)] %>% #DR kow (not cell-based)
    .[,DR_kaw:= (Fneutral*kaw_n)] %>% # no dependence on ionization: charged form assumed to have negligible vapor pressure
    .[,DR_kbsa:= (Fneutral*kbsa_n)+(Fpositive*kbsa_i_basic)+(Fnegative*kbsa_i_acidic)] %>% # DR kbsa (depends on acid/base)
    .[,DR_kpl:= (Fneutral*kpl_n)+(Fcharged*kpl_i)] %>% # DR kpl
    .[,DR_swat:= swat_n*(1+(Fcharged/Fneutral))] # no scaling factors, just this ratio
  
  ### Adjust DRs to account for salting out ###
  tcdata[,DR_kow:= DR_kow / 10^(-1*ksalt*csalt)] %>% 
    .[,DR_kaw:= DR_kaw / 10^(-1*ksalt*csalt)] %>% 
    .[,DR_kbsa:= DR_kbsa / 10^(-1*ksalt*csalt)] %>% 
    .[,DR_kpl:= DR_kpl / 10^(-1*ksalt*csalt)] %>% 
    .[,DR_swat:= DR_swat / 10^(-1*ksalt*csalt)]
  
  #Solubility: calculate the fugacity ratio (ratio of solid to liquid solubility at a given temperature)
  tcdata[,MP_K:= MP_C+273.15] %>% # Convert melting point to degrees K
    .[,F_ratio:=10^(0.01*(298.15-MP_K))] %>% #calculate fugacity ratio at 25C
    .[MP_K<=Tsys,F_ratio:=1] #if the system temperature is above the melting point, the chemical is liquid
  # F_ratio is only applicable for chemicals that are solid at system temperature
  
  ### General Solubility Equations (option.swat2==TRUE) ###
  #general solubility equation - estimate water solubility limits if none provided by user (option.swat2==TRUE)
  tcdata[,gswat_liquid.GSE:=0.5-gkow_n] %>% # general solubility equation for liquids or sub-cooled liquids
    .[,swat_liquid.GSE:=10^gswat_liquid.GSE] #unlog
  
  #general solubility equation for chemicals that are solid at the system temperature (option.swat2==TRUE)
  tcdata[MP_K>Tsys, gswat_solid.GSE:=0.5-0.01*(MP_C-25)-gkow_n] %>% # gse (at 25C) for solids (system temp lower than MP)
    .[MP_K>Tsys, swat_solid.GSE:=10^gswat_solid.GSE] %>% #unlog
    .[MP_K>Tsys, swat_subcooled_liquid.GSE := swat_solid.GSE/F_ratio] %>% #if the chem is solid at sys temp, use F_ratio to get sub-cooled liquid water solubility limit (not needed for liquids)
    .[MP_K>Tsys, gswat_subcooled_liquid.GSE_25C := log10(swat_subcooled_liquid.GSE)] %>% # log transform gswat (at 25C)
    .[MP_K>Tsys, gswat_subcooled_liquid.GSE := gswat_subcooled_liquid.GSE_25C-(-1*duow)*Tcor] %>% #temp correction bc calculated for 25C
    .[MP_K>Tsys, swat_subcooled_liquid.GSE := 10^gswat_subcooled_liquid.GSE] #un-log
  
  #assign gse value if the alt water solubility correction is true
  tcdata[option.swat2==TRUE & MP_K>Tsys, DR_swat:=swat_solid.GSE] %>% # system temp lower than melting point (SOLID) %>% 
    .[option.swat2==TRUE & MP_K>Tsys, swat_L:=swat_subcooled_liquid.GSE] %>%  # #subcooled liquid water solubility for Activity calculation 
    .[option.swat2==TRUE & MP_K<=Tsys, DR_swat:=swat_liquid.GSE] %>% # system temp higher than melting point (LIQUID)
    .[option.swat2==TRUE & MP_K<=Tsys, swat_L:=swat_liquid.GSE] #use regular liquid water solubility for Activity calculation 
  
  #otherwise assign swat value if the alt water solubility correction is false (default)
  tcdata[option.swat2==FALSE, swat_L:=DR_swat/F_ratio]
  
  ### Calculate the volume (in Liters) of each compartment ###
  tcdata[,Vbm:=v_working/1e6] %>% # uL to L; the volume of bulk medium
    .[,Vwell:=v_total/1e6] %>% # uL to L; the volume of well
    .[,Vcells:=cell_yield*(cellmass/1e6)/celldensity/1e6] %>% # cell*(ng/cell)*(1mg/1e6ng)/(mg/uL)*(1uL/L); the volume of cells
    .[,Vair:=Vwell-Vbm-Vcells] %>%  # the volume of head space
    .[,Valb:=Vbm*FBSf*0.733*conc_ser_alb/1000] %>% # the volume of serum albumin
    .[,Vslip:=Vbm*FBSf*conc_ser_lip/1000] %>% # the volume of serum lipids
    .[,Vdom:=Vdom/1e6] %>% # uL to L; the volume of Dissolved Organic Matter (DOM)
    .[,Vm:=Vbm-Valb-Vslip-Vdom] # the volume of medium
  #add in conv_units for the above chunk
  
  # umol/L for all concentrations
  tcdata[,mtot:= nomconc*Vbm] %>% # amount of umol chemical in the bulk medium in each well
    .[,cwat:=mtot/(DR_kaw*Vair + Vm + DR_kbsa*Valb +
                     P_cells*DR_kow*Vslip + DR_kow*P_dom*f_oc*Vdom + DR_kcw*Vcells +
                     1000*DR_kpl*sarea)] %>% #calculate freely dissolved aqueous concentration in the test system (umol)
    #DR_kpl (m3/m2) * sarea (m2) * 1000 (L/m3) =  (L)
    .[cwat>DR_swat,cwat_s:=DR_swat] %>% #if the water solubility is exceeded, we use the water solubility as the dissolved concentration in the water (uM) and the excess is assumed to precipitate, therefore not included in the mass balance 
    .[cwat>DR_swat,csat:=1] %>% # and note that the solution is saturated (1 = true)
    .[cwat<=DR_swat,cwat_s:=cwat] %>% #if the water solubility is not exceeded, we use the calculated aqueous concentration
    .[cwat<=DR_swat,csat:=0] %>% # and note that the solution is not saturated (0 = false)
    .[,activity:=cwat_s/swat_L] #chemical activity >= 0.01 indicates the potential for baseline toxicity to occur
  
  #set baseline value of zero
  tcdata[,c("cair","calb","cslip","cdom","ccells")] <- 0
  
  #all uM
  tcdata[Vair>0,cair:=DR_kaw*cwat_s] %>% #concentration in air 
    .[Valb>0,calb:=DR_kbsa*cwat_s] %>% #concentration bound to bsa
    .[Vslip>0,cslip:=DR_kow*cwat_s*P_cells] %>% #concentration bound to serum lipids
    .[Vdom>0,cdom:=DR_kow*cwat_s*P_dom*f_oc] %>% #concentration bound to dissolved organic matter
    .[Vcells>0,ccells:=DR_kcw*cwat_s] %>% #concentration in cells
    .[,cplastic:=DR_kpl*cwat_s*1000] %>% #DR_kpl (m3/m2) * cwat_s (umol/L) * 1000 (L/m3) = cplastic (umol/m2)
    .[,mwat_s:=cwat_s*Vm] %>% #umol in water (medium)
    .[,mair:=cair*Vair] %>% #umol in air (headspace)
    .[,mbsa:=calb*Valb] %>% #umol in bsa
    .[,mslip:=cslip*Vslip] %>% #umol in serum lipids
    .[,mdom:=cdom*Vdom] %>% #umol in dissolved organic matter
    .[,mcells:=ccells*Vcells] %>% #umol in cells
    .[,mplastic:=cplastic*sarea] %>% #cplastic (umol/m2) * sarea (m2) = mplastic (umol)
    .[,mprecip:=0] %>% #baseline - umol precipitated
    .[cwat>DR_swat,mprecip:=mtot-(mwat_s+mair+mbsa+mslip+mdom+mcells+mplastic)] %>% #if solubility is exceeded, umol precipitated
    .[,xwat_s:=mwat_s/mtot] %>% #Fraction dissolved in water 
    .[,xair:=mair/mtot] %>% #Fraction in the air
    .[,xbsa:=mbsa/mtot] %>% #Fraction bound to bovine serum albumin
    .[,xslip:=mslip/mtot] %>% #Fraction bound to serum lipids
    .[,xdom:=mdom/mtot] %>% #Fraction bound to dissolved organic matter 
    .[,xcells:=mcells/mtot] %>% #Fraction within cells
    .[,xplastic:=mplastic/mtot] %>% #Fraction bound to plastic 
    .[,xprecip:=mprecip/mtot] %>% # Fraction precipitated out of solution
    .[,eta_free := cwat_s/nomconc] %>%  # Effective availability ratio
    .[,cfree.invitro := cwat_s] # Free in vitro concentration in micromolar
  
  print("4/10/24 Working Version")
  
  return(tcdata)
  
  #output concentrations in umol/L
  #output mass (mwat_s etc.) in mols
  #output mol fraction xbsa etc.
  
}