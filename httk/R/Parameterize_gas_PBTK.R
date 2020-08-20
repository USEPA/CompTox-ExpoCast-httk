#' Parameterize_gas_pbtk
#' 
#' This function initializes the parameters needed in the function solve_gas_pbtk
#' 
#' 
#' @param chem.name Either the chemical name or the CAS number must be
#' specified. 
#' @param chem.cas Either the chemical name or the CAS number must be
#' specified. 
#' @param dtxsid EPA's DSSTox Structure ID (\url{http://comptox.epa.gov/dashboard})   
#' the chemical must be identified by either CAS, name, or DTXSIDs
#' @param species Species desired (either "Rat", "Rabbit", "Dog", "Mouse", or
#' default "Human").
#' @param default.to.human Substitutes missing animal values with human values
#' if true (hepatic intrinsic clearance or fraction of unbound plasma).
#' @param tissuelist Specifies compartment names and tissues groupings.
#' Remaining tissues in tissue.data are lumped in the rest of the body.
#' However, solve_pbtk only works with the default parameters.
#' @param force.human.clint.fup Forces use of human values for hepatic
#' intrinsic clearance and fraction of unbound plasma if true.
#' @param clint.pvalue.threshold Hepatic clearance for chemicals where the in
#' vitro clearance assay result has a p-values greater than the threshold are
#' set to zero.
#' @param adjusted.Funbound.plasma Returns adjusted Funbound.plasma when set to
#' TRUE along with parition coefficients calculated with this value.
#' @param regression Whether or not to use the regressions in calculating
#' partition coefficients.
#' @param suppress.messages Whether or not the output message is suppressed.
#' @param minimum.Funbound.plasma Monte Carlo draws less than this value are set 
#' equal to this value (default is 0.0001 -- half the lowest measured Fup in our
#' dataset).
#' @param vmax Michaelis-Menten vmax value in reactions/min
#' @param km Michaelis-Menten concentration of half-maximal reaction velocity
#' in desired output concentration units. 
#' @param exercise Logical indicator of whether to simulate an exercise-induced
#' heightened respiration rate
#' @param fR Respiratory frequency (breaths/minute), used especially to adjust
#' breathing rate in the case of exercise. This parameter, along with VT and VD
#' (below) gives another option for calculating Qalv (Alveolar ventilation) 
#' in case pulmonary ventilation rate is not known 
#' @param VT Tidal volume (L), to be modulated especially as part of simulating
#' the state of exercise
#' @param VD Anatomical dead space (L), to be modulated especially as part of
#' simulating the state of exercise
#' @param ... Other parameters
#' 
#' @return \item{BW}{Body Weight, kg.} 
#' \item{Clint}{Hepatic intrinsic clearance, uL/min/10^6 cells}
#' \item{Clint.dist}{Distribution of hepatic intrinsic clearance values
#' (median, lower 95th, upper 95th, p value)} 
#' \item{Clmetabolismc}{Hepatic Clearance, L/h/kg BW.} 
#' \item{Fgutabs}{Fraction of the oral dose absorbed, i.e. the fraction of the
#' dose that enters the gut lumen.}
#' \item{Fhep.assay.correction}{The fraction of chemical unbound in hepatocyte
#' assay using the method of Kilford et al. (2008)} 
#' \item{Funbound.plasma}{Fraction of chemical unbound to plasma.} 
#' \item{Funbound.plasma.adjustment}{Fraction unbound to plasma adjusted as
#' described in Pearce et al. 2017}
#' \item{Funbound.plasma.dist}{Distribution of fraction unbound to plasma
#' (median, lower 95th, upper 95th)}
#' \item{hematocrit}{Percent volume of red blood cells in the blood.}
#' \item{Kblood2air}{Ratio of concentration of chemical in blood to air}
#' \item{Kgut2pu}{Ratio of concentration of chemical in gut tissue to unbound
#' concentration in plasma.} 
#' \item{kgutabs}{Rate that chemical enters the gut from gutlumen, 1/h.} 
#' \item{Kkidney2pu}{Ratio of concentration of chemical in kidney tissue to
#' unbound concentration in plasma.} 
#' \item{Kliver2pu}{Ratio of concentration of chemical in liver tissue to
#' unbound concentration in plasma.} 
#' \item{Klung2pu}{Ratio of concentration of chemical in lung tissue
#' to unbound concentration in plasma.} 
#' \item{km}{Michaelis-Menten concentration of half-maximal activity}
#' \item{Kmuc2air}{Mucus to air partition coefficient}
#' \item{Krbc2pu}{Ratio of concentration of chemical in red blood cells to
#' unbound concentration in plasma.}
#' \item{Krest2pu}{Ratio of concentration of chemical in rest of body tissue to
#' unbound concentration in plasma.} 
#' \item{kUrtc}{Unscaled upper respiratory tract uptake parameter (L/h/kg^0.75)}
#' \item{liver.density}{Density of liver in g/mL}
#' \item{MA}{phospholipid:water distribution coefficient, membrane affinity}
#' \item{million.cells.per.gliver}{Millions cells per gram of liver tissue.} 
#' \item{MW}{Molecular Weight, g/mol.}
#' \item{pKa_Accept}{compound H association equilibrium constant(s)}
#' \item{pKa_Donor}{compound H dissociation equilibirum constant(s)}
#' \item{Pow}{octanol:water partition coefficient (not log transformed)}
#' \item{Qalvc}{Unscaled alveolar ventilation rate (L/h/kg^0.75)}
#' \item{Qcardiacc}{Cardiac Output, L/h/kg BW^3/4.} 
#' \item{Qgfrc}{Glomerular Filtration Rate, L/h/kg BW^0.75, volume of fluid
#' filtered from kidney and excreted.} 
#' \item{Qgutf}{Fraction of cardiac output flowing to the gut.}
#' \item{Qkidneyf}{Fraction of cardiac output flowing to the kidneys.}
#' \item{Qliverf}{Fraction of cardiac output flowing to the liver.}
#' \item{Qlungf}{Fraction of cardiac output flowing to lung tissue.}
#' \item{Qrestf}{Fraction of blood flow to rest of body}
#' \item{Rblood2plasma}{The ratio of the concentration of the chemical in the
#' blood to the concentration in the plasma from available_rblood2plasma.}
#' \item{Vartc}{Volume of the arteries per kg body weight, L/kg BW.}
#' \item{Vgutc}{Volume of the gut per kg body weight, L/kg BW.}
#' \item{Vkidneyc}{Volume of the kidneys per kg body weight, L/kg BW.}
#' \item{Vliverc}{Volume of the liver per kg body weight, L/kg BW.}
#' \item{Vlungc}{Volume of the lungs per kg body weight, L/kg BW.}
#' \item{vmax}{Michaelis-Menten maximum reaction velocity (1/min)}
#' \item{Vmucc}{Unscaled mucosal volume (L/kg BW^0.75}
#' \item{Vrestc}{ Volume of the rest of the body per kg body weight, L/kg BW.}
#' \item{Vvenc}{Volume of the veins per kg body weight, L/kg BW.} 
#'
#' @author Matt Linakis, Robert Pearce, John Wambaugh
#'
#' @references 
#' Linakis, Matthew W., et al. "Development and Evaluation of a High Throughput 
#' Inhalation Model for Organic Chemicals", submitted
#'
#' Kilford, P. J., Gertz, M., Houston, J. B. and Galetin, A.
#' (2008). Hepatocellular binding of drugs: correction for unbound fraction in
#' hepatocyte incubations using microsomal binding or drug lipophilicity data.
#' Drug Metabolism and Disposition 36(7), 1194-7, 10.1124/dmd.108.020834.
#'
#' @keywords Parameter
#'
#' @examples
#' parameters <- parameterize_gas_pbtk(chem.cas='129-00-0')
#' 
#' parameters <- parameterize_gas_pbtk(chem.name='pyrene',species='Rat')
#' 
#' parameterize_gas_pbtk(chem.cas = '56-23-5')
#' 
#' parameters <- parameterize_gas_pbtk(chem.name='Carbon tetrachloride',species='Rat')
#' 
#' # Change the tissue lumping:
#' compartments <- list(liver=c("liver"),fast=c("heart","brain","muscle","kidney"),
#'                       lung=c("lung"),gut=c("gut"),slow=c("bone"))
#' parameterize_gas_pbtk(chem.name="Bisphenol a",species="Rat",default.to.human=TRUE,
#'                    tissuelist=compartments) 
#' 
#' @export parameterize_gas_pbtk
parameterize_gas_pbtk <- function(chem.cas=NULL,
                              chem.name=NULL,
                              dtxsid=NULL,
                              species="Human",
                              default.to.human=F,
                              tissuelist=list(
                                liver=c("liver"),
                                kidney=c("kidney"),
                                lung=c("lung"),
                                gut=c("gut")),
                              force.human.clint.fup = F,
                              clint.pvalue.threshold=0.05,
                              adjusted.Funbound.plasma=T,
                              regression=T,
                              vmax = 0,
                              km = 1,
                              exercise = F,
                              fR = 12,
                              VT = 0.75,
                              VD = 0.15,
                              suppress.messages=F,
                              minimum.Funbound.plasma=0.0001,
                              ...)
{
  physiology.data <- physiology.data

# We need to describe the chemical to be simulated one way or another:
  if (is.null(chem.cas) & 
    is.null(chem.name) & 
    is.null(dtxsid)) 
    stop('chem.name, chem.cas, or dtxsid must be specified.')

# Look up the chemical name/CAS, depending on what was provide:
  out <- get_chem_id(
    chem.cas=chem.cas,
    chem.name=chem.name,
    dtxsid=dtxsid)
  chem.cas <- out$chem.cas
  chem.name <- out$chem.name                                
  dtxsid <- out$dtxsid
   
  if (class(tissuelist)!='list') stop("tissuelist must be a list of vectors.") 
  # Clint has units of uL/min/10^6 cells
  Clint.db <- try(get_invitroPK_param("Clint",species,chem.cas=chem.cas),silent=T)
  # Check that the trend in the CLint assay was significant:
  Clint.pValue <- try(get_invitroPK_param("Clint.pValue",species,chem.cas=chem.cas),silent=T)
  if ((class(Clint.db) == "try-error" & default.to.human) || force.human.clint.fup) 
  {
    Clint.db <- try(get_invitroPK_param("Clint","Human",chem.cas=chem.cas),silent=T)
    Clint.pValue <- try(get_invitroPK_param("Clint.pValue","Human",chem.cas=chem.cas),silent=T)
    if (!suppress.messages)
      warning(paste(species,"coerced to Human for metabolic clearance data."))
  }
  if (class(Clint.db) == "try-error") 
    stop("Missing metabolic clearance data for given species. Set default.to.human to true to substitute human value.")
  # Check if clint is a point value or a distribution, if a distribution, use the median:
  if (nchar(Clint.db) - nchar(gsub(",","",Clint.db))==3) 
  {
    Clint.dist <- Clint.db
    Clint<- as.numeric(strsplit(Clint.db,",")[[1]][1])
    Clint.pValue <- as.numeric(strsplit(Clint.db,",")[[1]][4])
    if (!suppress.messages) warning("Clint is provided as a distribution.")
  } else {
    Clint <- Clint.db
    Clint.dist <- NA
  }
  if (!is.na(Clint.pValue) & Clint.pValue > clint.pvalue.threshold) Clint  <- 0

  
# Predict the PCs for all tissues in the tissue.data table:
  schmitt.params <- parameterize_schmitt(chem.cas=chem.cas,
                                         species=species,
                                         default.to.human=default.to.human,
                                         force.human.fup=force.human.clint.fup,
                                         suppress.messages=T,
                                         minimum.Funbound.plasma=minimum.Funbound.plasma)
  PCs <- predict_partitioning_schmitt(parameters=schmitt.params,
                                      species=species,
                                      adjusted.Funbound.plasma=adjusted.Funbound.plasma,
                                      regression=regression,
                                      suppress.messages=suppress.messages,
                                      minimum.Funbound.plasma=minimum.Funbound.plasma)
# Get_lumped_tissues returns a list with the lumped PCs, vols, and flows:
  lumped_params <- lump_tissues(PCs,tissuelist=tissuelist,species=species)
  
# Check to see if we should use the in vitro fup assay correction:  
  if (adjusted.Funbound.plasma)
  {
    fup <- schmitt.params$Funbound.plasma
    if (!suppress.messages) warning(
'Funbound.plasma adjusted for in vitro partitioning (Pearce, 2017). Set adjusted.Funbound.plasma to FALSE to use original value.')
  } else fup <- schmitt.params$unadjusted.Funbound.plasma

# Restrict the value of fup:
  if (fup < minimum.Funbound.plasma) fup <- minimum.Funbound.plasma

  Fgutabs <- try(get_invitroPK_param("Fgutabs",species,chem.cas=chem.cas),silent=T)
  if (class(Fgutabs) == "try-error") Fgutabs <- 1
  
 # Check the species argument for capitilization problems and whether or not it is in the table:  
  if (!(species %in% colnames(physiology.data)))
  {
    if (toupper(species) %in% toupper(colnames(physiology.data)))
    {
      phys.species <- colnames(physiology.data)[toupper(colnames(physiology.data))==toupper(species)]
    } else stop(paste("Physiological PK data for",species,"not found."))
  } else phys.species <- species

# Load the physiological parameters for this species
  this.phys.data <- physiology.data[,phys.species]
  names(this.phys.data) <- physiology.data[,1]
  
  MW <- get_physchem_param("MW",chem.cas=chem.cas) #g/mol
  pKa_Donor <- suppressWarnings(get_physchem_param("pKa_Donor",chem.cas=chem.cas)) # acid dissociation constants
  pKa_Accept <- suppressWarnings(get_physchem_param("pKa_Accept",chem.cas=chem.cas)) # basic association cosntants
  Pow <- 10^get_physchem_param("logP",chem.cas=chem.cas) # Octanol:water partition coeffiecient

  outlist <- list()
   # Begin flows:
  #mL/min/kgBW converted to L/h/kgBW:
  QGFRc <- this.phys.data["GFR"]/1000*60 
  Qcardiacc = this.phys.data["Cardiac Output"]/1000*60 
  flows <- unlist(lumped_params[substr(names(lumped_params),1,1) == 'Q'])

  outlist <- c(outlist,c(
    Qcardiacc = as.numeric(Qcardiacc),
    flows[!names(flows) %in% c('Qtotal.liverf')], #MWL removed 'Qlungf', 9/19/19
    Qliverf= flows[['Qtotal.liverf']] - flows[['Qgutf']],
    Qgfrc = as.numeric(QGFRc))) 
  # end flows  
  
  # Begin volumes
  # units should be L/kgBW  
  Vartc = this.phys.data["Plasma Volume"]/(1-this.phys.data["Hematocrit"])/2/1000 #L/kgBW
  Vvenc = this.phys.data["Plasma Volume"]/(1-this.phys.data["Hematocrit"])/2/1000 #L/kgBW

  outlist <- c(outlist,
    Vartc = as.numeric(Vartc),
    Vvenc = as.numeric(Vvenc),
    lumped_params[substr(names(lumped_params),1,1) == 'V'],
    lumped_params[substr(names(lumped_params),1,1) == 'K'])
  
  
# Create the list of parameters:
  BW <- this.phys.data["Average BW"]
  hematocrit = this.phys.data["Hematocrit"]
  outlist <- c(outlist,list(BW = as.numeric(BW),
    kgutabs = 2.18, # 1/h 
    Funbound.plasma = fup, # unitless fraction
    Funbound.plasma.dist = schmitt.params$Funbound.plasma.dist,
    hematocrit = as.numeric(hematocrit), # unitless ratio
    MW = MW, #g/mol
    Pow = Pow,
    pKa_Donor=pKa_Donor,
    pKa_Accept=pKa_Accept,
    MA=schmitt.params[["MA"]],
    kUrtc = 11.0, #Added MWL 9-20-19
    Vmucc = 0.0001)) #Added MWL 9-20-19
  
  # Correct for unbound fraction of chemical in the hepatocyte intrinsic clearance assay (Kilford et al., 2008)
  outlist <- c(outlist,list(
    Fhep.assay.correction=calc_hep_fu(
      parameters = list(Pow = schmitt.params$Pow,
      pKa_Donor=schmitt.params$pKa_Donor,
      pKa_Accept=schmitt.params$pKa_Accept,
      suppress.messages=suppress.messages))))  # fraction 

  if (vmax==0)
  {
    if (!suppress.messages) 
      warning("Cannot calculate saturable metabolism with Vmax = 0. Defaulting to first-order metabolic clearance.")
    outlist <- c(outlist, list(
      vmax=0,
      km=1, #km value of 1 is a dummy value here
      Clint=Clint, 
      Clint.dist = Clint.dist,
      Clmetabolismc = as.numeric(calc_hep_clearance(
        chem.name = chem.name,
        hepatic.model="unscaled",
        parameters=list(
          Clint=Clint, #uL/min/10^6 cells
          Funbound.plasma=fup, # unitless fraction
          hep.assay.correction=outlist$Fhep.assay.correction, 
          million.cells.per.gliver= 110, # 10^6 cells/g-liver
          liver.density= 1.05, # g/mL
          Dn=0.17,BW=BW,
          Vliverc=lumped_params$Vliverc, #L/kg
          Qtotal.liverc=(lumped_params$Qtotal.liverc)/1000*60),
        suppress.messages=T)), #L/h/kg BW
      million.cells.per.gliver=110, # 10^6 cells/g-liver
      liver.density=1.05, # g/mL
      Fgutabs=Fgutabs)) #L/h/kg BW
  } else {
    outlist <- c(outlist,list(
      vmax=vmax,km=km,
      Clint=Clint, 
      Clint.dist = Clint.dist, 
      Clmetabolismc=0,                       
      million.cells.per.gliver=110, # 10^6 cells/g-liver
      liver.density=1.05, # g/mL
      Fgutabs=Fgutabs))#ML added Km = km 9-19-19
  }
 
 
 if (adjusted.Funbound.plasma) 
  {
    outlist["Funbound.plasma.adjustment"] <- schmitt.params$Funbound.plasma.adjustment
  } else outlist["Funbound.plasma.adjustment"] <- NA
   
    outlist <- c(outlist,
      Rblood2plasma=available_rblood2plasma(chem.cas=chem.cas,
        species=species,
        adjusted.Funbound.plasma=adjusted.Funbound.plasma,
        suppress.messages=T))
    
    #alveolar ventilation: 15 L/h/kg^.75 from campbell 2007
    #henry's law in atm * m^3 / mol, converted atm to Pa
    #human body temperature of 310 Kelvin
    logHenry = get_physchem_param(param = 'logHenry', chem.cas=chem.cas) #for log base 10 compiled Henry's law values
    hl <- 10^logHenry #Henry's constant in atm*m^3 / mol 
    #Gas constant 8.314 in units of J/(mol*K), body temperature 
    body_temp = as.numeric(this.phys.data['Average Body Temperature']) + 273.15 #C -> K
    Kwater2air <- 8.314 * body_temp / (hl * 101325)   #101325 atm to Pa 
    Kblood2air <- Kwater2air * outlist$Rblood2plasma / outlist$Funbound.plasma
    lKair2muc <- log10(1/Kwater2air) - (log10(Pow) - 1) * 0.524 #If no value is added for logP, it's assumed Kmuc2air = Kwater2air
    Kair2muc <- 10^(lKair2muc)
    Kmuc2air <- 1/Kair2muc
    if(exercise){
      Qalvc = ((fR*60) * (VT - VD))/outlist$BW^0.75 #L/h/kg^0.75, 
      #Added 4-30-19 to allow user-input respiratory and/or work values,
      #assumes input units of L and min^-1
    } else {
      Vdot <- this.phys.data["Pulmonary Ventilation Rate"]
      Qalvc <- Vdot * (0.67) #L/h/kg^0.75
    }
    outlist <- c(outlist,Kblood2air =  Kblood2air,Kmuc2air = Kmuc2air,Qalvc=as.numeric(Qalvc))
    
        
  return(lapply(outlist[sort(names(outlist))],set_httk_precision))
}
