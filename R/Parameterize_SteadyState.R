# This function retrieves the paramters needed to run the constant infusion dose model for determining steady-state concentration.
parameterize_steadystate <- function(chem.cas=NULL,
                                     chem.name=NULL,
                                     species="Human",
                                     clint.pvalue.threshold=0.05,
                                     default.to.human=F,
                                     human.clint.fub=F,
                                     adjusted.Funbound.plasma=T,
                                     restrictive.clearance=T)
{
  Parameter <- Species <- variable <- Tissue <- NULL
  physiology.data <- physiology.data
  tissue.data <- tissue.data
# Look up the chemical name/CAS, depending on what was provide:
  out <- get_chem_id(chem.cas=chem.cas,chem.name=chem.name)
  chem.cas <- out$chem.cas
  chem.name <- out$chem.name

  #Capitilie the first letter of spcies only:
  species <- tolower(species)
  substring(species,1,1) <- toupper(substring(species,1,1))

  if (!(species %in% colnames(physiology.data)))
  {
    if (toupper(species) %in% toupper(colnames(physiology.data)))
    {
      phys.species <- colnames(physiology.data)[toupper(colnames(physiology.data))==toupper(species)]
    } else stop(paste("Physiological PK data for",species,"not found."))
  } else phys.species <- species
  this.phys.data <- physiology.data[,phys.species]
  names(this.phys.data) <- physiology.data[,1]
  
  QGFRc <- this.phys.data[["GFR"]] #mL/min/kgBW
  BW <- this.phys.data[["Average BW"]]
    

  Qtotal.liverc <- subset(tissue.data,
                          tolower(Species) == tolower(species) & 
                          variable == 'Flow (mL/min/kg^(3/4))' & 
                          Tissue == 'liver')[,'value']  #mL/min/kgBW^3/4
  Vliverc <- subset(tissue.data,tolower(Species) == tolower(species) & variable == 'Vol (L/kg)' & Tissue == 'liver')[,'value'] # L/kg BW
  Clint <- try(get_invitroPK_param("Clint",species,chem.CAS=chem.cas),silent=T)
  if (class(Clint) == "try-error" & default.to.human || human.clint.fub) 
  {
    Clint <- try(get_invitroPK_param("Clint","Human",chem.CAS=chem.cas),silent=T)
    warning(paste(species,"coerced to Human for metabolic clerance data."))
  }
  if (class(Clint) == "try-error") stop("Missing metabolic clearance data for given species. Set default.to.human to true to substitute human value.")
    # Check that the trend in the CLint assay was significant:
  Clint.pValue <- get_invitroPK_param("Clint.pValue",species,chem.CAS=chem.cas)
  if (!is.na(Clint.pValue) & Clint.pValue > clint.pvalue.threshold) Clint <- 0
  
  # unitless fraction of chemical unbound with plasma
  fub <- try(get_invitroPK_param("Funbound.plasma",species,chem.CAS=chem.cas),silent=T)
  if (class(fub) == "try-error" & default.to.human || human.clint.fub) 
  {
    fub <- try(get_invitroPK_param("Funbound.plasma","Human",chem.CAS=chem.cas),silent=T)
    warning(paste(species,"coerced to Human for protein binding data."))
  }
  if (class(fub) == "try-error") stop("Missing protein binding data for given species. Set default.to.human to true to substitute human value.")
  pKa_Donor <- suppressWarnings(get_physchem_param("pKa_Donor",chem.CAS=chem.cas)) # acid dissociation constants
  pKa_Accept <- suppressWarnings(get_physchem_param("pKa_Accept",chem.CAS=chem.cas)) # basic association cosntants
  Pow <- 10^get_physchem_param("logP",chem.CAS=chem.cas) # Octanol:water partition coeffiecient
  if (fub == 0)
  {
    fub <- 0.005
    warning("Fraction unbound = 0, changed to 0.005.")
  }
  if(adjusted.Funbound.plasma){
    if(human.clint.fub) Flipid <- subset(physiology.data,Parameter=='Plasma Effective Neutral Lipid Volume Fraction')[,which(colnames(physiology.data) == 'Human')]
    else Flipid <- subset(physiology.data,Parameter=='Plasma Effective Neutral Lipid Volume Fraction')[,which(tolower(colnames(physiology.data)) == tolower(species))]
    ion <- calc_ionization(pH=7.4,pKa_Donor=pKa_Donor,pKa_Accept=pKa_Accept)
    dow <- Pow * (ion$fraction_neutral + 0.001 * ion$fraction_charged + ion$fraction_zwitter)
    fub.adjust <- 1 / ((dow) * Flipid + 1 / fub)/fub
    warning('Funbound.plasma recalculated with adjustment.  Set adjusted.Funbound.plasma to FALSE to use original value.')
  } else fub.adjust <- NA
  
  Fgutabs <- try(get_invitroPK_param("Fgutabs",species,chem.CAS=chem.cas),silent=T)
  if(class(Fgutabs) == "try-error") Fgutabs <- 1
 

  Params <- list()
  Params[["Clint"]] <- Clint # uL/min/10^6
  if (!is.na(fub.adjust)) Params[["Funbound.plasma"]] <- fub*fub.adjust # unitless fraction
  else Params[["Funbound.plasma"]] <- fub # unitless fraction
  Params[["Funbound.plasma.adjustment"]] <- fub.adjust
  Params[["Qtotal.liverc"]] <- Qtotal.liverc/1000*60     #        L/h/kgBW
  Params[["Qgfrc"]] <- QGFRc/1000*60 #        L/h/kgBW     
  Params[["BW"]] <- BW # kg
  Params[["MW"]] <- get_physchem_param("MW",chem.CAS=chem.cas) # molecular weight g/mol
#  Params[["Pow"]] <- Pow
#  Params[["pKa_Donor"]] <- pKa_Donor
#  Params[["pKa_Accept"]] <- pKa_Accept

# Correct for unbound fraction of chemical in the hepatocyte intrinsic clearance assay (Kilford et al., 2008)
  Params[["Fhep.assay.correction"]] <- calc_fu_hep(Pow,pKa_Donor=pKa_Donor,pKa_Accept=pKa_Accept) # fraction 

  Params[["million.cells.per.gliver"]] <- 110 # 10^6 cells/g-liver
  Params[["Vliverc"]] <- Vliverc # L/kg BW
  Params[["liver.density"]] <- 1.05 # g/mL
  Params[['Fgutabs']] <- Fgutabs
  
  Rb2p <- available_rblood2plasma(chem.name=chem.name,chem.cas=chem.cas,species=species,adjusted.Funbound.plasma=adjusted.Funbound.plasma)
  Params[["Rblood2plasma"]] <- Rb2p

  # Need to have a parameter with this name to calculate clearance, but need 
  # clearance to calculate bioavailability:
  Params[["hepatic.bioavailability"]] <- NA
  cl <- calc_hepatic_clearance(parameters=Params,hepatic.model='unscaled',suppress.messages=T)#L/h/kg boduweight
  Qliver <- Params$Qtotal.liverc / Params$BW^.25 #L/h

  if(restrictive.clearance) Params[['hepatic.bioavailability']] <- Qliver / (Qliver + Params$Funbound.plasma * cl / Rb2p)
  else Params[['hepatic.bioavailability']] <- Qliver / (Qliver + cl*BW / Rb2p) 
    return(Params)
}
