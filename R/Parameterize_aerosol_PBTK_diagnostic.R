# This function parameterizes a PBPK model. The argument tissuelist allows the specific tissues parameerized to be customized.
# All tissues not specified by tissuelist are lumped into a rest of body compartment ("Rest")



parameterize_aerosol_pbtk_diagnostic <- function(chem.cas=NULL,
                              chem.name=NULL,
                              species="Human",
                              default.to.human=F,
                              tissuelist=list(liver=c("liver"),kidney=c("kidney"),lung=c("lung"),gut=c("gut")),
                              force.human.clint.fub = F,
                              clint.pvalue.threshold=0.05,
                              adjusted.Funbound.plasma=T,
                              regression=T,
                              dae = 1,
                              particle.density = 3,
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
  

    outlist <- c(outlist,Rblood2plasma=available_rblood2plasma(chem.cas=chem.cas,species=species,adjusted.Funbound.plasma=adjusted.Funbound.plasma))
    #alveolar ventilation:15 L/h/kg^.75 from campbell 2007
    #henry's law in atm * m^3 / mol, converted atm to Pa
    #human body temperature of 310 Kelvin
      Vdot <- 300 #ml/s This quantity is used to calculate volume for aerosol particle distribution, but is then changed to be equivalent to the gas model in L/h so it can be used in the model itself
      #Vdot <- 24.45 * 1000 * (1/60) * (1/60) #L/h to mL/s
      Fds <- 0.33
      dae <- dae #* 1e-4 #dae in cm from um? double-check because this should potentially be in um per apex5 volume2
        #Define particle size parameters
	#Finh <- 1 - 0.5 * (1 - (7.6e-4 * dae^2 + 1)^(-1)) #DIFFERENT
	Finh <- 1 - 0.5 * (1 - (1 / (7.6e-4 * dae^2.8 + 1)))
        lambda <- 7.12e-1  #mean free path um
        #calc_ccf <- function(x) return(1 + (2 * lambda / x) * (1.257 + 0.4 * exp((-1.1 * x) / (2 * lambda))))
	calc_ccf <- function(x) return(1 + (lambda / x)*(2.514 + 0.8 * exp(-0.55 * (x / lambda)))) #x should be thermodynamic diameter in cm
        #fun <- function(x) sqrt(1.5 * calc_ccf(dae)) * dae - x * sqrt(calc_ccf(x) * particle.density)
        #fun <- function(x) sqrt((particle.density/1.5) * (calc_ccf(dae)/calc_ccf(x))) - x #um DIFFERENT
	fun <- function(x) dae * sqrt((1.5/particle.density) * (calc_ccf(dae)/calc_ccf(x)))-x
	dth <- uniroot(fun,c(1e-100,1e100))$root #um
        ccf <- calc_ccf(dth)
        D <- (ccf * 1.38e-19 * 310.15) / (3 * pi * 1.9e-6 * (dth/1e4))  # cm^2 / s
	#Define volumes and scaling factors
        Vt <- 750 #ml
        FRC <- 3301 #ml
        VdET <- 50 #ml
        VdBB <- 49 #ml
        Vdbb <- 47 #ml
        Vdtotal <- 146
	SF1 <- 1
	SF2 <- 1
	SF3 <- 1
	sex = 1 #male
	height = 175.7 #cm
	if(sex == 0){
		FRC <- 0.222 * height^2 - 26.483 * height + 959.95
		Vdtotal <- 0.0079 * height^2 - 0.7403 * height + 30.381
		VdET <- 0.0029 * height^2 - 0.2861 * height + 9.5306
		VdBB <- 0.0022 * height^2 - 0.1523 * height + 5.9635
		Vdbb <- 0.0029 * height^2 - 0.3225 * height + 16.098
		SF1 <- 0.000128 * height^2 - 0.043542 * height + 4.7975
		SF2 <- 0.0000232 * height^2 + 0.011225 * height + 2.2597
		SF3 <- 0.0000805 * height^2 - 0.032543 * height + 4.2585
	} else {
		FRC <- 0.2278 * height^2 - 27.95 * height + 1035.3
		Vdtotal <- 0.0078 * height^2 - 0.7135 * height + 29.316
		VdET <- 0.0031 * height^2 - 0.3175 * height + 10.907
		VdBB <- 0.0026 * height^2 - 0.2392 * height + 9.7143
		Vdbb <- 0.0023 * height^2 - 0.2119 * height + 11.375
		SF1 <- 0.00011354 * height^2 - 0.0407 * height + 4.6711
		SF2 <- 0.00002302 * height^2 - 0.011168 * height + 2.2567
		SF3 <- 0.00007836 * height^2 - 0.0321 * height + 4.2381
	}

	#Define residence times
        tB <- VdBB / Vdot * (1 + 0.5 * Vt / FRC)
        tb <- Vdbb / Vdot * (1 + 0.5 * Vt / FRC)
        ta <- (Vt - VdET - (VdBB + Vdbb) * (1 + Vt / FRC)) / Vdot

#Nose filter
        #Nae <- .5 * (1 - 1/(3e-4 * (dae^2 * Vdot * SF1^3) + 1))
        #Nth <- .5 * (1 - exp(- 18 * sqrt(D * (Vdot * SF1)^-.25)))
	Nae <- 1 - exp(-3e-4 * (dae^2 * Vdot * SF1^3) ^ 1)
	Nth <- 1 - exp(-18 * (D * (Vdot * SF1)^-0.25) ^ 0.5)
        if(Nae < 0) Nae <- 0
        if(Nth < 0) Nth <- 0
        N <- sqrt(Nae^2 + Nth^2)
      #if(N > 1) N <- 1
#Oropharynx filter
        #Oae <- 1 - 1 / (5.5e-5 * (dae^2 * Vdot * SF1^3)^1.17 + 1)
        #Oth <- 1 - exp(- 15.1 * (D * (Vdot * SF1)^-.25)^.538)
	#if(mouthbreath = 1){
		#Oae <- 1 - exp(-1.1e-4 * (dae^2 * Vdot^0.6 * Vt^-0.2 * SF1^1.4)^1.4)
		#Oth <- 1 - exp(-9 * (dae^2 * Vdot^0.6 * Vt^-0.2 * SF1^1.4)^0.5)
	#} else{
		Oae <- 1 - exp(-5.5e-5 * (dae^2 * Vdot * SF1^3)^1.17)
		Oth <- 1 - exp(-15.1 * (D * (Vdot * SF1)^-0.25)^0.538)
	#}
        if(Oae < 0) Oae <- 0
        if(Oth < 0) Oth <- 0
        O <- sqrt(Oae^2 + Oth^2)
        #if(O > 1) O <- 1
#Tracheobronchial filter
        Binae <- 1 - exp(-4.08e-6 * (dae^2 * Vdot * SF1^2.3)^1.152)
        Bexae <- 1 - exp(-2.04e-6 * (dae^2 * Vdot * SF1^2.3)^1.152)
        Bth <- 1 - exp(-(22.02 * SF1^1.24 * (1 + 100 * exp(-1*(log10(100 + (10 / dth^.9)))^2))) * (D * tB)^0.6391)
        if(Binae < 0) Binae <- 0
        if(Bexae < 0) Bexae <- 0
        if(Bth < 0) Bth <- 0
        Bin <- sqrt(Binae^2 + Bth^2)
       if(Bin > 1) Bin <- 1
        Bex <- sqrt(Bexae^2 + Bth^2)
      #if(Bex > 1) Bex <- 1
#Bronchiole filter
        bae <- 1 - exp(-0.1147 * ((0.056 + tb^1.5) * dae^(tb^-0.25))^1.173)
        bth <- 1 - exp(-1 * (-76.8 + 167 * SF2^0.65) * (D * tb)^0.5676)
        if(bae < 0) bae <- 0
        if(bth < 0) bth <- 0
        b <- sqrt(bae^2 + bth^2)
      #if(b > 1) b <- 1
#Alveoli filter
        aae <- 1 - exp(-(0.146 * SF3^0.98) * (dae^2 * ta)^0.6495)
        ath <- 1 - exp(-(170 + 103*SF3^2.13) * (D * ta)^0.6101)
        if(aae < 0) aae <- 0
        if(ath < 0) ath <- 0
        a <- sqrt(aae^2 + ath^2)
        if(a > 1) a <- 1
#Define additional volumes
        Nv <- 1
        Ov <- 1
        Bv <- 1 - VdET / Vt
        bv <- 1 - (VdET + VdBB * (1 + Vt / FRC)) / Vt
        av <- 1 - (VdET + (VdBB + Vdbb) * (1 + Vt / FRC)) / Vt
# 9 filters plus inhalability
      #dej = dej-1 * Nj * ej * (1 / (nj-1) - 1) where ej = vj / vj-1
	N <- 0
	O <- 0	# NOTE THESE ARE FOR THE PURPOSES OF REPLICATING FIGURE 4 ONLY
	deNin <- N *  Finh
        deOin <- O * (1 -N) * Finh#deNin * O * (Ov / Nv) * (1/N - 1)
        deBin <- Bin * Bv * (1 - O) * (1-N) * Finh#deOin * Bin * (Bv / Ov) * (1/O - 1)
        debin <- b * bv * (1 - Bin) * (1 - O) * (1-N) * Finh#deBin * b * (bv / Bv) * (1/Bin -1)
        dea <- a * av * (1 - b) * (1 - Bin) * (1 - O) * (1-N) * Finh#debin * a * (av / bv) * (1/b - 1)
        debex <- b * bv * (1 - a) * (1 - b) * (1 - Bin) * (1 - O) * (1-N) * Finh#dea * b * (bv / av) * (1/a - 1)
        deBex <- Bex * Bv * (1 - a) * (1 - b)^2 * (1 - Bin) * (1 - O) * (1-N) * Finh#debex * Bex * (Bv / bv) * (1/b - 1)
        deOex <- O * (1 - Bex) * (1 - a) * (1 - b)^2 * (1 - Bin) * (1 - O) * (1-N) * Finh#deBex * O * (Ov / Bv) * (1/Bex - 1)
        deNex <- N * (1 - Bex) * (1 - a) * (1 - b)^2 * (1 - Bin) * (1 - O)^2 * (1-N) * Finh#deOex * N * (Nv / Ov) * (1/O - 1)
      
	#minh <- Finh 
	#deNin <- (minh) * N
	#deOin <- (minh - (deNin)) * O
	#deBin <- (minh - (deNin + deOin)) * Bin
	#debin <- (minh - (deNin + deOin + deBin)) * b
	#dea   <- (minh - (deNin + deOin + deBin + debin)) * a
	#debex <- (minh - (deNin + deOin + deBin + debin + dea)) * b
	#deBex <- (minh - (deNin + deOin + deBin + debin + dea + debex)) * Bex
	#deOex <- (minh - (deNin + deOin + deBin + debin + dea + debex + deBex)) * O
	#deNex <- (minh - (deNin + deOin + deBin + debin + dea + debex + deBex + deOex)) * N
	


        #Flung <- Finh * (1 - N) * (1 - O) * (1 - Bin) * (1 - b)
        Fdeposited <-  deNin + deOin + deBin + debin + debex + deBex + deOex + deNex
	FBB <- deBin + deBex
	Fbb <- debin + debex
        #Fdeposited.ex <- debex + deBex + deOex + deNex
        #Vdot <- Vdot / 1000 *3600 / outlist$BW^.75 #L/h/kg^.75   from ml/s #DOUBLE CHECK
	Vdot <- 24.75 #L/h for 1-kg
        outlist <- c(outlist,Fdeposited=Fdeposited,DEalv=dea,FBB=FBB,Fbb=Fbb,Finh=Finh,Fds=Fds,Vdot=Vdot)
           
  return(outlist[sort(names(outlist))])
}