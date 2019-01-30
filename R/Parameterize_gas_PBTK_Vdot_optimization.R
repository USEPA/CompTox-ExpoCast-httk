# This function parameterizes a PBPK model. The argument tissuelist allows the specific tissues parameerized to be customized.
# All tissues not specified by tissuelist are lumped into a rest of body compartment ("Rest")



parameterize_gas_pbtk_Vdot_optimization <- function(chem.cas=NULL,
                              chem.name=NULL,
                              species="Human",
                              default.to.human=F,
                              tissuelist=list(liver=c("liver"),kidney=c("kidney"),lung=c("lung"),gut=c("gut")),
                              force.human.clint.fub = F,
                              clint.pvalue.threshold=0.05,
                              adjusted.Funbound.plasma=T,
                              regression=T,
			      vmax.km=F,
			      vmax = 0,
			      km = 1,
			      vp.koa = F,
			      vp = 0,
			      koa = 0,
			      muc = F,
			      lP = 1,
			      Vdot = 24.45,
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
  if ((class(Clint) == "try-error" & default.to.human) || force.human.clint.fub) 
  {
    Clint <- try(get_invitroPK_param("Clint","Human",chem.CAS=chem.cas),silent=T)
    warning(paste(species,"coerced to Human for metabolic clearance data."))
  }
  if (class(Clint) == "try-error") stop("Missing metabolic clearance data for given species. Set default.to.human to true to substitute human value.")
    # Check that the trend in the CLint assay was significant:
  Clint.pValue <- get_invitroPK_param("Clint.pValue",species,chem.CAS=chem.cas)
  if (!is.na(Clint.pValue) & Clint.pValue > clint.pvalue.threshold) Clint <- 0
  
  
# Predict the PCs for all tissues in the tissue.data table:
  schmitt.params <- parameterize_schmitt(chem.cas=chem.cas,species=species,default.to.human=default.to.human,force.human.fub=force.human.clint.fub)
  PCs <- predict_partitioning_schmitt(parameters=schmitt.params,species=species,adjusted.Funbound.plasma=adjusted.Funbound.plasma,regression=regression)
# Get_lumped_tissues returns a list with the lumped PCs, vols, and flows:
  lumped_params <- lump_tissues(PCs,tissuelist=tissuelist,species=species)
  if(adjusted.Funbound.plasma){
    fub <- schmitt.params$Funbound.plasma
    warning('Funbound.plasma recalculated with adjustment.  Set adjusted.Funbound.plasma to FALSE to use original value.')
  }else fub <- schmitt.params$unadjusted.Funbound.plasma

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

  outlist <- list()
   # Begin flows:
  #mL/min/kgBW converted to L/h/kgBW:
  QGFRc <- this.phys.data["GFR"]/1000*60 
  Qcardiacc = this.phys.data["Cardiac Output"]/1000*60 
  flows <- unlist(lumped_params[substr(names(lumped_params),1,1) == 'Q'])
  omit <- 'Qtotal.liverf'
  outlist <- c(outlist,c(
    Qcardiacc = as.numeric(Qcardiacc),
    flows[!names(flows) %in% omit],
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
    kgutabs = 1, # 1/h
    Funbound.plasma = as.numeric(fub), # unitless fraction
    hematocrit = as.numeric(hematocrit), # unitless ratio
    MW = MW)) #g/mol
  
  # Correct for unbound fraction of chemical in the hepatocyte intrinsic clearance assay (Kilford et al., 2008)
 outlist <- c(outlist,list(Fhep.assay.correction=calc_fu_hep(schmitt.params$Pow,pKa_Donor=schmitt.params$pKa_Donor,pKa_Accept=schmitt.params$pKa_Accept)))  # fraction 

  if(vmax.km){
    outlist <- c(outlist,list(vmax=vmax,km=km))
  }else{
  outlist <- c(outlist,
    list(Clmetabolismc= as.numeric(calc_hepatic_clearance(hepatic.model="unscaled",parameters=list(
                                Clint=Clint, #uL/min/10^6 cells
                                Funbound.plasma=fub, # unitless fraction
                                Fhep.assay.correction=outlist$Fhep.assay.correction, 
                                million.cells.per.gliver= 110, # 10^6 cells/g-liver
                                liver.density= 1.05, # g/mL
                                Dn=0.17,BW=BW,
                                Vliverc=lumped_params$Vliverc, #L/kg
                                Qtotal.liverc=(lumped_params$Qtotal.liverc)/1000*60),suppress.messages=T)),million.cells.per.gliver=110,Fgutabs=Fgutabs)) #L/h/kg BW
  }

    outlist <- c(outlist,Rblood2plasma=available_rblood2plasma(chem.cas=chem.cas,species=species,adjusted.Funbound.plasma=adjusted.Funbound.plasma))
    #alveolar ventilation:15 L/h/kg^.75 from campbell 2007
    #henry's law in atm * m^3 / mol, converted atm to Pa
    #human body temperature of 310 Kelvin
    hl <- subset(chem.physical_and_invitro.data,CAS == chem.cas)[,'HL']
    vp <- vp
    koa <- koa
    lP <- lP
    if(vp.koa){
	Kwater2air <- (hl * MW) / (8.21E-5 * 310)  #310 K body temp, 8.21E-5 m^3atmK-1mol-1 
  	if(vp > 30 & Kwater2air > 0.1){
    		Kblood2air <- ((0.8417/Kwater2air) + (0.006232 * (10^(koa))))
  	} else{ 
	    	Kblood2air <- ((0.4445/Kwater2air) + (0.005189 * (10^(koa))))
  	}
    }else{
	Kwater2air <- 8.314 * 310 / (hl * 101325)   #310 K body temp, 101325 atm to Pa, 
    	Kblood2air <- Kwater2air * outlist$Rblood2plasma / outlist$Funbound.plasma#((1 - parameters$hematocrit) / parameters$Funbound.plasma + parameters$hematocrit * parameters$Krbc2pu)
    }
    #Vdot <- 300 #ml/s
    #Vdot <- Vdot / 1000 *3600 / outlist$BW^.75 #L/h/kg^.75   from ml/s
    #Vdot <- 24.75 #L/h
    #Vdot <- Vdot * outlist$BW^.75
    Fds <- 0.33
    if(muc){
	lKair2muc <- log10(1/Kwater2air) - (lP - 1) * 0.524 #If no value is added for logP, it's assumed Kmuc2air = Kwater2air
	Kair2muc <- 10^(lKair2muc)
	Kmuc2air <- 1/Kair2muc
	outlist <- c(outlist,Kblood2air =  Kblood2air,Kmuc2air = Kmuc2air,Qalv=Vdot*(1-Fds))
    }else{
    outlist <- c(outlist,Kblood2air =  Kblood2air,Qalv=Vdot*(1-Fds))
    }
        
  return(outlist[sort(names(outlist))])
}