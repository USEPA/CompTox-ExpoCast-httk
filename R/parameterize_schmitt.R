parameterize_schmitt <- function(chem.cas=NULL,
                                 chem.name=NULL,
                                 species="Human",
                                 default.to.human=F,
                                 force.human.fub=F)
{

physiology.data <- physiology.data
Species <- variable <- Tissue <- Parameter <- NULL
# Look up the chemical name/CAS, depending on what was provide:
  out <- get_chem_id(chem.cas=chem.cas,chem.name=chem.name)
  chem.cas <- out$chem.cas
  chem.name <- out$chem.name                                
                                 
  # unitless fraction of chemical unbound with plasma
  fub <- try(get_invitroPK_param("Funbound.plasma",species,chem.CAS=chem.cas),silent=T)
  if ((class(fub) == "try-error" & default.to.human) || force.human.fub) 
  {
    fub <- try(get_invitroPK_param("Funbound.plasma","Human",chem.CAS=chem.cas),silent=T)
    warning(paste(species,"coerced to Human for protein binding data."))
  }
  if (class(fub) == "try-error") stop("Missing protein binding data for given species. Set default.to.human to true to substitute human value.")
  if (fub == 0)
  {
    fub <- 0.005
    warning("Fraction unbound = 0, changed to 0.005.")
  }                                 
                                 
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
  
# Load the physico-chemical properties:  
  pKa_Donor <- suppressWarnings(get_physchem_param("pKa_Donor",chem.CAS=chem.cas))
  pKa_Accept <- suppressWarnings(get_physchem_param("pKa_Accept",chem.CAS=chem.cas))
  Pow <- 10^get_physchem_param("logP",chem.CAS=chem.cas)
  MA <- suppressWarnings(10^(get_physchem_param("logMA",chem.CAS=chem.cas))) 
  Fprotein <- physiology.data[which(physiology.data[,'Parameter'] =='Plasma Protein Volume Fraction'),which(tolower(colnames(physiology.data)) == tolower(species))]
  if(force.human.fub) Flipid <- subset(physiology.data,Parameter=='Plasma Effective Neutral Lipid Volume Fraction')[,which(colnames(physiology.data) == 'Human')]
  else Flipid <- subset(physiology.data,Parameter=='Plasma Effective Neutral Lipid Volume Fraction')[,which(tolower(colnames(physiology.data)) == tolower(species))]
  ion <- calc_ionization(pH=7.4,pKa_Donor=pKa_Donor,pKa_Accept=pKa_Accept)
  dow <- Pow * (ion$fraction_neutral + 0.001 * ion$fraction_charged + ion$fraction_zwitter)
  fub.corrected <- 1 / ((dow) * Flipid + 1 / fub)
  outlist <- list(Funbound.plasma=fub.corrected,unadjusted.Funbound.plasma=fub,Pow=Pow,pKa_Donor=pKa_Donor,pKa_Accept=pKa_Accept,MA=MA,Fprotein.plasma = Fprotein,plasma.pH=7.4,alpha=0.001)
  
  return(outlist)                                
                                 
}