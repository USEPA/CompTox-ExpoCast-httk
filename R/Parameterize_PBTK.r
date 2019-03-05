# This function parameterizes a PBPK model. The argument tissuelist allows the specific tissues parameerized to be customized.
# All tissues not specified by tissuelist are lumped into a rest of body compartment ("Rest")



parameterize_pbtk <- function(chem.cas=NULL,
                              chem.name=NULL,
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
                              suppress.messages=F)
{
  physiology.data <- physiology.data
# Look up the chemical name/CAS, depending on what was provide:
  out <- get_chem_id(chem.cas=chem.cas,chem.name=chem.name)
  chem.cas <- out$chem.cas
  chem.name <- out$chem.name
   
  if(class(tissuelist)!='list') stop("tissuelist must be a list of vectors.") 
  # Clint has units of uL/min/10^6 cells
  Clint <- try(get_invitroPK_param("Clint",species,chem.CAS=chem.cas),silent=T)
  if ((class(Clint) == "try-error" & default.to.human) || force.human.clint.fup) 
  {
    Clint <- try(get_invitroPK_param("Clint","Human",chem.CAS=chem.cas),silent=T)
    warning(paste(species,"coerced to Human for metabolic clearance data."))
  }
  if (class(Clint) == "try-error") stop("Missing metabolic clearance data for given species. Set default.to.human to true to substitute human value.")
    # Check that the trend in the CLint assay was significant:
  Clint.pValue <- get_invitroPK_param("Clint.pValue",species,chem.CAS=chem.cas)
  if (!is.na(Clint.pValue) & Clint.pValue > clint.pvalue.threshold) Clint <- 0
  
  
# Predict the PCs for all tissues in the tissue.data table:
  schmitt.params <- parameterize_schmitt(chem.cas=chem.cas,
                                         species=species,
                                         default.to.human=default.to.human,
                                         force.human.fup=force.human.clint.fup,
                                         suppress.messages=T)
  PCs <- predict_partitioning_schmitt(parameters=schmitt.params,
                                      species=species,
                                      adjusted.Funbound.plasma=adjusted.Funbound.plasma,
                                      regression=regression)
# Get_lumped_tissues returns a list with the lumped PCs, vols, and flows:
  lumped_params <- lump_tissues(PCs,tissuelist=tissuelist,species=species)
  
# Check to see if we should use the in vitro fup assay correction:  
  if (adjusted.Funbound.plasma)
  {
    fup <- schmitt.params$Funbound.plasma
    warning('Funbound.plasma adjusted for in vitro partioning (Pearce, 2017). Set adjusted.Funbound.plasma to FALSE to use original value.')
  } else fup <- schmitt.params$unadjusted.Funbound.plasma

# Check to see if fup is a distribution:
  if (nchar(fup) - nchar(gsub(",","",fup))==2) 
  {
    fup <- schmitt.params$unadjusted.Funbound.plasma
  }

  Fgutabs <- try(get_invitroPK_param("Fgutabs",species,chem.CAS=chem.cas),silent=T)
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
  
  MW <- get_physchem_param("MW",chem.CAS=chem.cas) #g/mol
  pKa_Donor <- suppressWarnings(get_physchem_param("pKa_Donor",chem.CAS=chem.cas)) # acid dissociation constants
  pKa_Accept <- suppressWarnings(get_physchem_param("pKa_Accept",chem.CAS=chem.cas)) # basic association cosntants
  Pow <- 10^get_physchem_param("logP",chem.CAS=chem.cas) # Octanol:water partition coeffiecient

  outlist <- list()
   # Begin flows:
  #mL/min/kgBW converted to L/h/kgBW:
  QGFRc <- this.phys.data["GFR"]/1000*60 
  Qcardiacc = this.phys.data["Cardiac Output"]/1000*60 
  flows <- unlist(lumped_params[substr(names(lumped_params),1,1) == 'Q'])

  outlist <- c(outlist,c(
    Qcardiacc = as.numeric(Qcardiacc),
    flows[!names(flows) %in% c('Qlungf','Qtotal.liverf')],
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
    hematocrit = as.numeric(hematocrit), # unitless ratio
    MW = MW, #g/mol
    Pow = Pow,
    pKa_Donor=pKa_Donor,
    pKa_Accept=pKa_Accept,
    MA=schmitt.params[["MA"]]))
  
  # Correct for unbound fraction of chemical in the hepatocyte intrinsic clearance assay (Kilford et al., 2008)
 outlist <- c(outlist,list(
              Fhep.assay.correction=calc_fu_hep(schmitt.params$Pow,
                pKa_Donor=schmitt.params$pKa_Donor,
                pKa_Accept=schmitt.params$pKa_Accept)))  # fraction 

  outlist <- c(outlist,
    list(Clint=Clint,
         Clmetabolismc= as.numeric(calc_hepatic_clearance(hepatic.model="unscaled",
                          parameters=list(
                            Clint=Clint, #uL/min/10^6 cells
                            Funbound.plasma=fup, # unitless fraction
                            Fhep.assay.correction=outlist$Fhep.assay.correction, 
                            million.cells.per.gliver= 110, # 10^6 cells/g-liver
                            liver.density= 1.05, # g/mL
                            Dn=0.17,BW=BW,
                            Vliverc=lumped_params$Vliverc, #L/kg
                            Qtotal.liverc=(lumped_params$Qtotal.liverc)/1000*60),
                          suppress.messages=T)), #L/h/kg BW
         million.cells.per.gliver=110, # 10^6 cells/g-liver
         liver.density=1.05, # g/mL
         Fgutabs=Fgutabs)) 
  
  if (adjusted.Funbound.plasma) 
  {
    outlist["Funbound.plasma.adjustment"] <- schmitt.params$Funbound.plasma.adjustment
  } else outlist["Funbound.plasma.adjustment"] <- NA
   
    outlist <- c(outlist,
      Rblood2plasma=available_rblood2plasma(chem.cas=chem.cas,
        species=species,
        adjusted.Funbound.plasma=adjusted.Funbound.plasma))
        
  return(outlist[sort(names(outlist))])
}
