parameterize_1comp <- function(chem.cas=NULL,
                               chem.name=NULL,
                               species='Human',
                               default.to.human=F,
                               adjusted.Funbound.plasma=T,
                               regression=T,
                               restrictive.clearance=T,
                               well.stirred.correction=T,
                               suppress.messages=F,
                               clint.pvalue.threshold=0.05)
{
  physiology.data <- physiology.data
  if(is.null(chem.cas) & is.null(chem.name)) stop('Must specify chem.name or chem.cas')
  params <- list()
  
  params[['Vdist']] <- calc_vdist(chem.cas=chem.cas,
                                  chem.name=chem.name,
                                  species=species,
                                  default.to.human=default.to.human,
                                  adjusted.Funbound.plasma=adjusted.Funbound.plasma,
                                  regression=regression,suppress.messages=T)
  
  ss.params <- suppressWarnings(parameterize_steadystate(chem.name=chem.name,
                                                         chem.cas=chem.cas,
                                                         species=species,
                                                         default.to.human=default.to.human,
                                                         adjusted.Funbound.plasma=adjusted.Funbound.plasma,
                                                         restrictive.clearance=restrictive.clearance,
                                                         clint.pvalue.threshold=clint.pvalue.threshold))
  ss.params <- c(ss.params, params['Vdist'])
  
  params[['kelim']] <- calc_elimination_rate(parameters=ss.params,
                                             chem.cas=chem.cas,
                                             chem.name=chem.name,
                                             species=species,
                                             suppress.messages=T,
                                             default.to.human=default.to.human,
                                             adjusted.Funbound.plasma=adjusted.Funbound.plasma,
                                             regression=regression,
                                             restrictive.clearance=restrictive.clearance,
                                             well.stirred.correction=well.stirred.correction,
                                             clint.pvalue.threshold=clint.pvalue.threshold)
  
  params[["Clint"]] <- ss.params[["Clint"]]
  params[["Clint.dist"]] <- ss.params[["Clint.dist"]]
  params[["Funbound.plasma"]] <- ss.params[["Funbound.plasma"]] 
  params[["Funbound.plasma.dist"]] <- ss.params[["Funbound.plasma.dist"]] 
  params[["Funbound.plasma.adjustment"]] <- ss.params[["Funbound.plasma.adjustment"]] 
  params[["Fhep.assay.correction"]] <- ss.params[["Fhep.assay.correction"]]
  phys.params <-  suppressWarnings(parameterize_schmitt(chem.name=chem.name,
                                                         chem.cas=chem.cas,
                                                         species=species,
                                                         default.to.human=default.to.human)) 
  params[["Pow"]] <- phys.params[["Pow"]]
  params[["pKa_Donor"]] <- phys.params[["pKa_Donor"]] 
  params[["pKa_Accept"]] <- phys.params[["pKa_Accept"]]
  params[["MA"]] <- phys.params[["MA"]]

  params[['kgutabs']] <- 2.18
  
  params[['Rblood2plasma']] <- available_rblood2plasma(chem.cas=chem.cas,chem.name=chem.name,species=species,adjusted.Funbound.plasma=adjusted.Funbound.plasma)
  
  params[['million.cells.per.gliver']] <- 110
  params[["liver.density"]] <- 1.05 # g/mL
   
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
    
    params[['hematocrit']] <- this.phys.data[["Hematocrit"]]
  
  if(is.null(chem.cas)) chem.cas <- get_chem_id(chem.name=chem.name)[['chem.cas']]
  params[['MW']] <- get_physchem_param("MW",chem.CAS=chem.cas)
  
    Fgutabs <- try(get_invitroPK_param("Fgutabs",species,chem.CAS=chem.cas),silent=T)
    if (class(Fgutabs) == "try-error") Fgutabs <- 1
    
    params[['Fgutabs']] <- Fgutabs
  
    params[['hepatic.bioavailability']] <- ss.params[['hepatic.bioavailability']]  
    
    params[['BW']] <- this.phys.data[["Average BW"]]
  
  return(params)
}
