#' Estimate well surface area
#' 
#' Estimate geometry surface area of plastic in well plate based on well plate
#' format suggested values from Corning.  option.plastic == T (default) give
#' nonzero surface area (sarea, m^2) option.bottom == T (default) includes
#' surface area of the bottom of the well in determining sarea.  Optionally
#' include user values for working volume (v_working, m^3) and surface area.
#' 
#' 
#' @param tcdata A data table with well_number corresponding to plate format,
#' optionally include v_working, sarea, option.bottom, and option.plastic
#' @param this.well_number For single value, plate format default is 384, used
#' if is.na(tcdata)==TRUE
#' @param this.cell_yield For single value, optionally supply cell_yield,
#' otherwise estimated based on well number
#' @param this.v_working For single value, optionally supply working volume,
#' otherwise estimated based on well number (m^3)
#'
#' @return tcdata, A data table with well_number, sarea (surface area, m^2),
#' cell_yield (# cells), v_working (m^3), v_total (m^3) per well
#'
#' @author Greg Honda
#'
#' @references
#' Armitage, J. M., Arnot, J. A., Wania, F., & Mackay, D. (2013). Development 
#' and evaluation of a mechanistic bioconcentration model for ionogenic organic 
#' chemicals in fish. Environmental toxicology and chemistry, 32(1), 115-128.
#'
#' @import magrittr
#'
#' @export armitage_estimate_sarea
armitage_estimate_sarea <- function(tcdata = NA, # optionally supply columns v_working,sarea, option.bottom, and option.plastic
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
  #End R CMD CHECK appeasement.
  
  if(all(is.na(tcdata))){
    tcdata <- data.table(well_number = this.well_number, cell_yield = this.cell_yield, v_working = this.v_working)
  }
  
  if(!(all(c("option.bottom","option.plastic") %in% names(tcdata)))){
      tcdata[,c("option.bottom","option.plastic")[!(c("option.bottom","option.plastic")%in%names(tcdata))]] <- as.logical(NA)
  }

  if(!(all(c("sarea","cell_yield","v_working")%in%names(tcdata)))){
      tcdata[,c("sarea","cell_yield","v_working")[!(c("sarea","cell_yield","v_working")%in%names(tcdata))]] <- as.double(NA)
  }

  well.desc.list <- c("flat_bottom","standard","clear_flat_bottom")
  well.number.list <- c(6,12,24,48)
  well.param <- copy(well_param)[well_number %in% well.number.list |
                             well_desc %in% well.desc.list]

  setnames(well.param,c("cell_yield","v_working"),c("cell_yield_est","v_working_est"))


  tcdata <- well.param[tcdata,on=.(well_number)]

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

  return(tcdata)
}





#' Evaluate the updated Armitage model
#' 
#' Evaluate the Armitage model for chemical distributon in vitro. Takes input
#' as data table or vectors of values. Outputs a data table. Updates over
#' the model published in Armitage et al. 2014 include binding to plastic walls
#' and lipid and protein compartments in cells.
#' 
#' 
#' @param casrn.vector For vector or single value, CAS number
#' @param nomconc.vector For vector or single value, micromolar nominal 
#' concentration (e.g. AC50 value)
#' @param this.well_number For single value, plate format default is 384, used
#' if is.na(tcdata)==TRUE
#' @param this.FBSf Fraction fetal bovine serum, must be entered by user.
#' @param tcdata A data.table with casrn, nomconc, MP, gkow, gkaw, gswat, sarea,
#' v_total, v_working. Otherwise supply single values to this.params.
#' @param this.sarea Surface area per well (m^2)
#' @param this.v_total Total volume per well (m^3)
#' @param this.v_working Working volume per well (m^3)
#' @param this.cell_yield Number of cells per well
#' @param this.Tsys System temperature (degrees C)
#' @param this.Tref Reference temperature (degrees K)
#' @param this.option.kbsa2 Use alternative bovine-serum-albumin partitioning
#' model
#' @param this.option.swat2 Use alternative water solubility correction
#' @param this.pseudooct Pseudo-octanol cell storage lipid content
#' @param this.memblip Membrane lipid content of cells
#' @param this.nlom Structural protein conent of cells
#' @param this.P_nlom Proportionality constant to octanol structural protein
#' @param this.P_dom Proportionality constant to dissolve organic material
#' @param this.P_cells Proportionality constant to octanol storage lipid
#' @param this.csalt Ionic strength of buffer, mol/L
#' @param this.celldensity Cell density kg/L, g/mL
#' @param this.cellmass Mass per cell, ng/cell
#' @param this.f_oc 1, everything assumed to be like proteins
#'
#' @return
#' \tabular{lll}{
#' \strong{Column} \tab \strong{Description} \tab \strong{units} \cr
#' casrn \tab Chemical Abstracts Service Registry Number \tab \cr
#' nomconc \tab Nominal Concentration \tab mol/L \cr       
#' well_number \tab Number of wells in plate \tab unitless \cr   
#' sarea \tab Surface area of well \tab m^2 \cr         
#' v_total \tab Total volume of well \tab m^3 \cr       
#' v_working \tab Filled volume of well \tab m^3 \cr     
#' cell_yield \tab Number of cells \tab cells \cr    
#' gkow \tab log10 octanol to water partition coefficient (PC)\tab log10 \cr          
#' logHenry \tab log10 Henry's law constant '\tab log10 atm-m3/mol \cr      
#' gswat \tab log10 Water solubility \tab log10 mol/L \cr         
#' MP \tab Melting Point \tab degrees Celsius \cr           
#' MW \tab Molecular Weight \tab g/mol \cr            
#' gkaw \tab air to water PC \tab (mol/m3)/(mol/m3) \cr
#' dsm \tab \tab \cr           
#' duow \tab \tab \cr          
#' duaw \tab \tab \cr          
#' dumw \tab \tab \cr          
#' gkmw \tab \tab \cr          
#' gkcw \tab \tab \cr          
#' gkbsa \tab \tab \cr         
#' gkpl \tab \tab \cr          
#' ksalt \tab \tab \cr        
#' Tsys \tab \tab \cr          
#' Tref \tab \tab \cr          
#' option.kbsa2 \tab \tab \cr  
#' option.swat2 \tab \tab \cr  
#' FBSf \tab \tab \cr          
#' pseudooct \tab \tab \cr     
#' memblip \tab \tab \cr       
#' nlom \tab \tab \cr          
#' P_nlom \tab \tab \cr   
#' P_dom \tab dissolved organic matter to water PC \tab Dimensionless \cr         
#' P_cells \tab \tab \cr      
#' csalt \tab \tab \cr         
#' celldensity \tab \tab \cr   
#' cellmass \tab \tab \cr      
#' f_oc \tab \tab \cr          
#' cellwat \tab \tab \cr       
#' Tcor \tab \tab \cr          
#' Vm \tab Volume of media \tab L \cr            
#' Vwell \tab volume of medium (aqueous phase only) \tab L \cr         
#' Vair \tab volume of head space \tab L \cr          
#' Vcells \tab volume of cells/tissue\tab \cr        
#' Valb \tab volume of serum albumin \tab \cr         
#' Vslip \tab volume of serum lipids \tab \cr         
#' Vdom \tab volume of dissolved organic matter\tab \cr          
#' F_ratio \tab \tab \cr       
#' gs1.GSE \tab \tab \cr       
#' s1.GSE \tab \tab \cr        
#' gss.GSE \tab \tab \cr       
#' ss.GSE \tab \tab \cr        
#' kmw \tab \tab \cr           
#' kow \tab octanol to water PC \tab \cr           
#' kaw \tab the air towater PC \tab dimensionless \cr           
#' swat \tab \tab \cr         
#' kpl \tab \tab \cr           
#' kcw \tab cell/tissue to water PC \tab dimensionless \cr           
#' kbsa \tab \tab \cr          
#' swat_L \tab \tab \cr        
#' oct_L \tab \tab \cr        
#' scell_L \tab \tab \cr       
#' cinit \tab Initial concentration \tab mol \cr         
#' mtot \tab Total moles \tab mol \cr          
#' cwat \tab Total concentration in water \tab mol/L \cr          
#' cwat_s \tab Dissolved concentration in water \tab mol/L \cr        
#' csat \tab Is the solution saturated (1/0) \tab Boolean \cr         
#' activity \tab \tab \cr      
#' cair \tab \tab mol/L \cr          
#' calb \tab \tab mol/L \cr          
#' cslip \tab \tab mol/L \cr         
#' cdom \tab concentration of/in dissolved organic matter\tab mol/L \cr          
#' ccells \tab \tab mol/L \cr        
#' cplastic \tab \tab mol/L \cr      
#' mwat_s \tab Mass dissolved in water \tab mols \cr        
#' mair \tab Mass in air \tab mols \cr          
#' mbsa \tab Mass bound to bovine serum albumin \tab mols \cr          
#' mslip \tab Mass bound to serum lipids \tab mols \cr        
#' mdom \tab Mass bound to dissolved organic matter \tab mols \cr          
#' mcells \tab Mass in cells \tab mols \cr        
#' mplastic \tab Mass bond to plastic \tab mols \cr      
#' mprecip \tab Mass precipitated out of solution \tab \cr       
#' xwat_s \tab Fraction dissolved in water \tab fraction \cr        
#' xair \tab Fraction in the air \tab fraction \cr          
#' xbsa \tab Fraction bound to bovine serum albumin \tab fraction \cr          
#' xslip \tab Fraction bound to serum lipids \tab fraction \cr         
#' xdom \tab Fraction bound to dissolved organic matter \tab fraction \cr          
#' xcells \tab Fraction within cells \tab fraction \cr        
#' xplastic \tab Fraction bound to plastic \tab fraction \cr     
#' xprecip \tab Fraction precipitated out of solution \tab fraction \cr       
#' eta_free \tab effective availability ratio \tab fraction \cr      
#' \strong{cfree.invitro} \tab \strong{Free concentration in the in vitro media} (use for Honda1 and Honda2) \tab micromolar \cr
#' }
#'
#' @author Greg Honda
#'
#' @references Armitage, J. M.; Wania, F.; Arnot, J. A. Environ. Sci. Technol. 
#' 2014, 48, 9770-9779. https://doi.org/10.1021/es501955g
#'
#' Honda et al. PloS one 14.5 (2019): e0217564. https://doi.org/10.1371/journal.pone.0217564
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
#' # Since we don't look up phys-chem from dashboard:
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
armitage_eval <- function(casrn.vector = NA_character_, # vector of CAS numbers
                          nomconc.vector = 1, # nominal concentration vector (e.g. apparent AC50 values)
                          this.well_number = 384,
                          this.FBSf = NA_real_, # Must be set if not in tcdata, this is the most senstive parameter in the model
                          tcdata = NA, # A data.table with casrn, ac50, and well_number or all of sarea, v_total, and v_working
                          this.sarea = NA_real_,
                          this.v_total = NA_real_,
                          this.v_working = NA_real_,
                          this.cell_yield = NA_real_,
                          this.Tsys = 37,
                          this.Tref = 298.15,
                          this.option.kbsa2 = F,
                          this.option.swat2 = F,
                          this.pseudooct = 0.01, # storage lipid content of cells
                          this.memblip = 0.04, # membrane lipid content of cells
                          this.nlom = 0.20, # structural protein content of cells
                          this.P_nlom = 0.035, # proportionality constant to octanol structural protein
                          this.P_dom = 0.05,# proportionality constant to octanol dom
                          this.P_cells = 1,# proportionality constant to octanol storage-liqid
                          this.csalt = 0.15, # ionic strength of buffer, mol/L
                          this.celldensity=1, # kg/L g/mL  mg/uL
                          this.cellmass = 3, #ng/cell
                          this.f_oc = 1 # everything assumed to be like proteins
){
  # this.Tsys <- 37
  # this.Tref <- 298.15
  # this.option.kbsa2 <- F
  # this.option.swat2 <- F
  # this.FBSf <- 0.1
  # this.pseudooct <- 0.01 # storage lipid content of cells
  # this.memblip <- 0.04 # membrane lipid content of cells
  # this.nlom <- 0.20 # structural protein content of cells
  # this.P_nlom <- 0.035 # proportionality constant to octanol structural protein
  # this.P_dom <- 0.05 # proportionality constant to octanol dom
  # this.P_cells <- 1 # proportionality constant to octanol storage-liqid
  # this.csalt <- 0.15 # ionic strength of buffer, mol/L
  # this.celldensity<-1 # kg/L g/mL  mg/uL
  # this.cellmass <- 3 #ng/cell
  # this.f_oc <- 1 # everything assumed to be like proteins


  #R CMD CHECK throws notes about "no visible binding for global variable", for
  #each time a data.table column name is used without quotes. To appease R CMD
  #CHECK, a variable has to be created for each of these column names and set to
  #NULL. Note that within the data.table, these variables will not be NULL! Yes,
  #this is pointless and annoying.
  casrn<-ac50<-MP<-gkow<-gkaw<-gswat<-sarea<-v_total<-v_working<-NULL
  cell_yield<-cellwat<-pseudooct<-memblip<-nlom<-Tsys<-Tcor<-Tref<-Vm<-NULL
  Vwell<-Vair<-Vcells<-cellmass<-celldensity<-Valb<-FBSf<-Vslip<-Vdom<-dsm<-NULL
  duow<-duaw<-dumw<-F_ratio<-gs1.GSE<-s1.GSE<-gss.GSE<-ss.GSE<-gkmw<-kmw<-NULL
  kow<-kaw<-swat<-gkpl<-kpl<-gkcw<-P_cells<-P_nlom<-kcw<-gkbsa<-kbsa<-NULL
  option.kbsa2<-ksalt<-csalt<-swat_L<-option.swat2<-soct_L<-scell_L<-cinit<-NULL
  mtot<-cwat<-P_dom<-f_oc<-cwat_s<-csat<-activity<-cair<-calb<-cslip<-cdom<-NULL
  ccell<-cplastic<-mwat_s<-mair<-mbsa<-mslip<-mdom<-mcells<-mplastic<-NULL
  mprecip<-xwat_s<-xair<-xbsa<-xslip<-xdom<-xcells<-xplastic<-xprecip<-NULL
  ccells<-eta_free <- cfree.invitro <- nomconc <- well_number <- NULL
  logHenry <- logWSol <- NULL
  #End R CMD CHECK appeasement.

  if(all(is.na(tcdata))){
    tcdata <- data.table(casrn = casrn.vector,
                         nomconc = nomconc.vector,
                         well_number = this.well_number,
                         sarea = this.sarea,
                         v_total = this.v_total,
                         v_working = this.v_working,
                         cell_yield = this.cell_yield)
  }
  
  # Check CAS and AC50 supplied
  if(any(is.na(tcdata[,.(casrn,nomconc)]))){
    stop("casrn or nomconc undefined")
  }  
  
  if(any(is.na(this.FBSf)) & !"FBSf" %in% names(tcdata)){
    stop("this.FBSf must be defined or FBSf must be a column in tcdata")
  }
  
  if(!all(names(tcdata) %in% c("sarea", "v_total", "v_working", "cell_yield")) |
     any(is.na(tcdata[,.(sarea, v_total, v_working, cell_yield)]))){
    
    if(all(names(tcdata) %in% c("sarea", "v_total", "v_working", "cell_yield")) &
       any(is.na(tcdata[,.(sarea, v_total, v_working, cell_yield)]))){
      missing.rows <- which(is.na(tcdata[,sarea]))
    }else{
      missing.rows <- 1:length(tcdata[,casrn])
    }
    
    if(any(is.na(tcdata[missing.rows, well_number]))){
      print(paste0("Either well_number or geometry must be defined for rows: ", 
                  paste(which(tcdata[, is.na(sarea) & is.na(well_number)]),collapse = ",")))
      stop()
    }else{
      temp <- armitage_estimate_sarea(tcdata[missing.rows,])
      tcdata[missing.rows,"sarea"] <- temp[,"sarea"]
      tcdata[missing.rows,"v_total"] <- temp[,"v_total"]
      tcdata[missing.rows,"v_working"] <- temp[,"v_working"]
      tcdata[missing.rows,"cell_yield"] <- temp[,"cell_yield"]
    }
    
    
    
  }
  
  if(!all(c("gkow","logHenry","gswat","MP","MW") %in% names(tcdata))){
    tcdata[, c("gkow","logHenry","gswat","MP","MW") := 
             get_physchem_param(param = c("logP","logHenry","logWSol","MP","MW"), 
                                chem.cas = casrn)]
  }
  tcdata[, "gkaw" := logHenry - log10(298.15*8.2057338e-5)] # log10 atm-m3/mol to (mol/m3)/(mol/m3)

  manual.input.list <- list(Tsys=this.Tsys, Tref=this.Tref,
                                option.kbsa2=this.option.kbsa2, option.swat2=this.option.swat2,
                                FBSf=this.FBSf, pseudooct=this.pseudooct, memblip=this.memblip,
                                nlom=this.nlom, P_nlom=this.P_nlom, P_dom=this.P_dom, P_cells=this.P_cells,
                                csalt=this.csalt, celldensity=this.celldensity, cellmass=this.cellmass, f_oc=this.f_oc)

  check.list <- c("dsm","duow","duaw","dumw",
                  "gkmw","gkcw","gkbsa","gkpl","ksalt")

  req.list <- c("Tsys","Tref","option.kbsa2","option.swat2",
                 "FBSf","pseudooct","memblip","nlom","P_nlom","P_dom","P_cells",
                 "csalt","celldensity","cellmass","f_oc")
  if(!all(check.list%in%names(tcdata))){
  tcdata[,check.list[!(check.list %in% names(tcdata))]] <- as.double(NA)}

  if(!all(req.list%in%names(tcdata))){
  tcdata[,req.list[!(req.list %in% names(tcdata))]] <- manual.input.list[!(names(manual.input.list) %in% names(tcdata))]}

  R <- 8.3144

  tcdata[,cellwat := 1-(pseudooct+memblip+nlom)] %>%
    .[,Tsys:=Tsys+273.15] %>%
    .[,Tcor:=((1/Tsys)-(1/Tref))/(2.303*R)]


  tcdata[,Vm:=v_working/1e6] %>% # uL to L
      .[,Vwell:=v_total/1e6] %>% # uL to L
      .[,Vair:=Vwell-Vm] %>%
      .[,Vcells:=cell_yield*(cellmass/1e6)/celldensity/1e6] %>% # cell*(ng/cell)*(1mg/1e6ng)/(mg/uL)*(1uL/L)
      .[,Valb:=Vm*FBSf*0.733*24/1000] %>%
      .[,Vslip:=Vm*FBSf*1.9/1000] %>%
      .[,Vdom:=0]

  tcdata[is.na(dsm),dsm:=56.5] %>%
    .[is.na(duow),duow:=-20000] %>%
    .[is.na(duaw),duaw:=60000] %>%
    .[is.na(dumw),dumw:=duow] %>%
    .[,MP:= MP+273.15] %>%
    .[,F_ratio:=exp(-(dsm/R)*(MP/Tsys))] %>% #Walden's rule
    .[MP<=Tsys,F_ratio:=1]

  tcdata[,gs1.GSE:=0.5-gkow] %>%
    .[,gs1.GSE:=gs1.GSE-(-1*duow)*Tcor] %>%
    .[,s1.GSE:=10^gs1.GSE] %>%
    .[MP>298.15,gss.GSE:=0.5-0.01*((MP-273.15)-25)-gkow] %>%
    .[MP>298.15,gss.GSE:=gss.GSE-(-1*duow)*Tcor] %>%
    .[MP>298.15,ss.GSE:=10^gss.GSE] %>%
    .[is.na(gkmw),gkmw:=1.01*gkow + 0.12] %>%
    .[,gkmw:=gkmw-dumw*Tcor] %>%
    .[,kmw:=10^gkmw] %>%
    .[,gkow:=gkow-duow*Tcor] %>% # check this
    .[,kow:=10^gkow] %>%
    .[,gkaw:=gkaw-duaw*Tcor] %>%
    .[,kaw := 10^gkaw] %>%
    .[,gswat:=gswat-(-1*duow)*Tcor] %>%
    .[,swat:=10^gswat]

  tcdata[is.na(gkpl),gkpl:=0.97*gkow-6.94] %>%
    .[,kpl:=10^gkpl] %>%
    .[!(is.na(gkcw)),gkcw:=gkcw-duow*Tcor] %>%
    .[is.na(gkcw),gkcw:=log10(P_cells*pseudooct*kow + memblip*kmw +
                                P_nlom*nlom*kow + cellwat)] %>%
    .[,kcw:=10^gkcw]

  tcdata[!(is.na(gkbsa)),gkbsa:=gkbsa-duow*Tcor] %>%
    .[!(is.na(gkbsa)),kbsa:=10^gkbsa]

  tcdata[option.kbsa2==TRUE & is.na(gkbsa) & gkaw<4.5, kbsa:=10^(1.08*gkow-0.7)] %>%
    .[option.kbsa2==TRUE & is.na(gkbsa) & gkaw>=4.5, kbsa:=10^(0.37*gkow+2.56)]

  tcdata[option.kbsa2==FALSE & is.na(gkbsa),kbsa:=10^(0.71*gkow+0.42)]

  tcdata[is.na(ksalt),ksalt:=0.04*gkow+0.114] %>%
    .[,swat:=swat*10^(-1*ksalt*csalt)] %>%
    .[,s1.GSE:=s1.GSE*10^(-1*ksalt*csalt)] %>%
    .[MP>298.15,ss.GSE:=ss.GSE*10^(-1*ksalt*csalt)] %>%
    .[,swat_L:=swat/F_ratio] %>%
    .[,kow:=kow/(10^(-1*ksalt*csalt))] %>%
    .[,kaw:=kaw/(10^(-1*ksalt*csalt))] %>%
    .[,kcw:=kcw/(10^(-1*ksalt*csalt))] %>%
    .[,kbsa:=kbsa/(10^(-1*ksalt*csalt))]

  tcdata[option.swat2==TRUE & MP>298.15,swat:=ss.GSE] %>%
    .[option.swat2==TRUE & MP>298.15,swat_L:=s1.GSE] %>%  # double check this
    .[option.swat2==TRUE & MP<=298.15,swat:=s1.GSE] %>%
    .[option.swat2==TRUE & MP<=298.15,swat_L:=s1.GSE]

  tcdata[,soct_L:=kow*swat_L] %>%
    .[,scell_L:=kcw*swat_L]

  tcdata[,nomconc := nomconc/1e6] %>% # umol/L to mol/L
    .[,cinit:= nomconc] %>%
    .[,mtot:= nomconc*Vm] %>% # total moles
    .[,cwat:=mtot/(kaw*Vair + Vm + kbsa*Valb +
                     P_cells*kow*Vslip + P_dom*f_oc*Vdom + kcw*Vcells +
                     1000*kpl*sarea)] %>%
    .[cwat>swat,cwat_s:=swat] %>%
    .[cwat>swat,csat:=1] %>%
    .[cwat<=swat,cwat_s:=cwat] %>%
    .[cwat<=swat,csat:=0] %>%
    .[,activity:=cwat_s/swat_L]

  tcdata[,c("cair","calb","cslip","cdom","ccells")] <- 0

  tcdata[Vair>0,cair:=kaw*cwat_s] %>%
    .[Valb>0,calb:=kbsa*cwat_s] %>%
    .[Vslip>0,cslip:=kow*cwat_s*P_cells] %>%
    .[Vdom>0,cdom:=kow*cwat_s*P_dom*f_oc] %>%
    .[Vcells>0,ccells:=kcw*cwat_s] %>%
    .[,cplastic:=kpl*cwat_s*1000] %>%
    .[,mwat_s:=cwat_s*Vm] %>%
    .[,mair:=cair*Vair] %>%
    .[,mbsa:=calb*Valb] %>%
    .[,mslip:=cslip*Vslip] %>%
    .[,mdom:=cdom*Vdom] %>%
    .[,mcells:=ccells*Vcells] %>%
    .[,mplastic:=cplastic*sarea] %>%
    .[,mprecip:=0] %>%
    .[cwat>swat,mprecip:=mtot-(mwat_s+mair+mbsa+mslip+mdom+mcells+mplastic)] %>%
    .[,xwat_s:=mwat_s/mtot] %>%
    .[,xair:=mair/mtot] %>%
    .[,xbsa:=mbsa/mtot] %>%
    .[,xslip:=mslip/mtot] %>%
    .[,xdom:=mdom/mtot] %>%
    .[,xcells:=mcells/mtot] %>%
    .[,xplastic:=mplastic/mtot] %>%
    .[,xprecip:=mprecip/mtot] %>% 
    .[, eta_free := cwat_s/nomconc] %>%  # effective availability ratio
    .[, cfree.invitro := cwat_s * 1e6] # free invitro concentration in micromolar
  
  return(tcdata)
  #output concentrations in mol/L
  #output mass (mwat_s etc.) in mols
  #output mol fraction xbsa etc.
}

