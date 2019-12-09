#' Parameterize_gas_PBTK
#' 
#' This function initializes the parameters needed in the function
#' solve_gas_pbtk. 
#' 
#' This function parameterizes a PBPK model. The argument tissuelist allows the
#' specific tissues parameerized to be customized. All tissues not specified by
#' tissuelist are lumped into a rest of body compartment ("Rest")
#' 
#' @param chem.name Either the chemical name or the CAS number must be
#' specified. 
#' @param chem.cas Either the chemical name or the CAS number must be
#' specified. 
#' @param species Species desired (either "Rat", "Rabbit", "Dog", "Mouse", or
#' default "Human").
#' @param default.to.human Substitutes missing animal values with human values
#' if true (hepatic intrinsic clearance or fraction of unbound plasma).
#' @param tissuelist Specifies compartment names and tissues groupings.
#' Remaining tissues in tissue.data are lumped in the rest of the body.
#' However, solve_pbtk only works with the default parameters.
#' @param force.human.clint.fup Forces use of human values for hepatic
#' intrinsic clearance and fraction of unbound plasma if true.
#' @param clint.pvalue.threshold Hepatic clearances with clearance assays
#' having p-values greater than the threshold are set to zero.
#' @param adjusted.Funbound.plasma Returns adjusted Funbound.plasma when set to
#' TRUE along with parition coefficients calculated with this value.
#' @param regression Whether or not to use the regressions in calculating
#' partition coefficients.
#' @param suppress.messages Whether or not the output message is suppressed.
#' @return
#' 
#' \item{BW}{Body Weight, kg.} \item{Clmetabolismc}{Hepatic Clearance, L/h/kg
#' BW.} \item{Fgutabs}{Fraction of the oral dose absorbed, i.e. the fraction of
#' the dose that enters the gutlumen.} \item{Funbound.plasma}{Fraction of
#' plasma that is not bound.} \item{Fhep.assay.correction}{The fraction of
#' chemical unbound in hepatocyte assay using the method of Kilford et al.
#' (2008)} \item{hematocrit}{Percent volume of red blood cells in the blood.}
#' \item{Kgut2pu}{Ratio of concentration of chemical in gut tissue to unbound
#' concentration in plasma.} \item{kgutabs}{Rate that chemical enters the gut
#' from gutlumen, 1/h.} \item{Kkidney2pu}{Ratio of concentration of chemical in
#' kidney tissue to unbound concentration in plasma.} \item{Kliver2pu}{Ratio of
#' concentration of chemical in liver tissue to unbound concentration in
#' plasma.} \item{Klung2pu}{Ratio of concentration of chemical in lung tissue
#' to unbound concentration in plasma.} \item{Krbc2pu}{Ratio of concentration
#' of chemical in red blood cells to unbound concentration in plasma.}
#' \item{Krest2pu}{Ratio of concentration of chemical in rest of body tissue to
#' unbound concentration in plasma.} \item{million.cells.per.gliver}{Millions
#' cells per gram of liver tissue.} \item{MW}{Molecular Weight, g/mol.}
#' \item{Qcardiacc}{Cardiac Output, L/h/kg BW^3/4.} \item{Qgfrc}{Glomerular
#' Filtration Rate, L/h/kg BW^3/4, volume of fluid filtered from kidney and
#' excreted.} \item{Qgutf}{Fraction of cardiac output flowing to the gut.}
#' \item{Qkidneyf}{Fraction of cardiac output flowing to the kidneys.}
#' \item{Qliverf}{Fraction of cardiac output flowing to the liver.}
#' \item{Rblood2plasma}{The ratio of the concentration of the chemical in the
#' blood to the concentration in the plasma from available_rblood2plasma.}
#' \item{Vartc}{Volume of the arteries per kg body weight, L/kg BW.}
#' \item{Vgutc}{Volume of the gut per kg body weight, L/kg BW.}
#' \item{Vkidneyc}{Volume of the kidneys per kg body weight, L/kg BW.}
#' \item{Vliverc}{Volume of the liver per kg body weight, L/kg BW.}
#' \item{Vlungc}{Volume of the lungs per kg body weight, L/kg BW.}
#' \item{Vrestc}{ Volume of the rest of the body per kg body weight, L/kg BW.}
#' \item{Vvenc}{Volume of the veins per kg body weight, L/kg BW.}
#' \item{Kblood2air}{Blood: air partition coefficient.} \item{Qalv}{For gas,
#' air flow to alevoli (Vdot * (1-Fds)).}
#' 
#' @author John Wambaugh and Robert Pearce
#' @references Kilford, P. J., Gertz, M., Houston, J. B. and Galetin, A.
#' (2008). Hepatocellular binding of drugs: correction for unbound fraction in
#' hepatocyte incubations using microsomal binding or drug lipophilicity data.
#' Drug Metabolism and Disposition 36(7), 1194-7, 10.1124/dmd.108.020834.
#' @keywords Parameter
#' @export parameterize_gas_pbtk



parameterize_gas_pbtk <- function(chem.cas=NULL,
                              chem.name=NULL,
                              species="Human",
                              default.to.human=F,
                              tissuelist=list(liver=c("liver"),kidney=c("kidney"),lung=c("lung"),gut=c("gut")),
                              force.human.clint.fup = F,
                              clint.pvalue.threshold=0.05,
                              adjusted.Funbound.plasma=T,
                              regression=T,
			                        vmax.km = F,
			                        vmax = 0,
			                        km = 1,
                              suppress.messages=F)
{
  physiology.data <- physiology.data
# Look up the chemical name/CAS, depending on what was provide:
  out <- get_chem_id(chem.cas=chem.cas,chem.name=chem.name)
  chem.cas <- out$chem.cas
  chem.name <- out$chem.name
   
  if(class(tissuelist)!='list') stop("tissuelist must be a list of vectors.") 
  # Clint has units of uL/min/10^6 cells
  Clint <- try(get_invitroPK_param("Clint",species,chem.cas=chem.cas),silent=T)
  if ((class(Clint) == "try-error" & default.to.human) || force.human.clint.fup) 
  {
    Clint <- try(get_invitroPK_param("Clint","Human",chem.cas=chem.cas),silent=T)
    warning(paste(species,"coerced to Human for metabolic clearance data."))
  }
  if (class(Clint) == "try-error") stop("Missing metabolic clearance data for given species. Set default.to.human to true to substitute human value.")
    # Check that the trend in the CLint assay was significant:
  Clint.pValue <- get_invitroPK_param("Clint.pValue",species,chem.cas=chem.cas)
  if (!is.na(Clint.pValue) & Clint.pValue > clint.pvalue.threshold) Clint <- 0
  
  
# Predict the PCs for all tissues in the tissue.data table:
  schmitt.params <- parameterize_schmitt(chem.cas=chem.cas,species=species,default.to.human=default.to.human,force.human.fup=force.human.clint.fup)
  PCs <- predict_partitioning_schmitt(parameters=schmitt.params,species=species,adjusted.Funbound.plasma=adjusted.Funbound.plasma,regression=regression)
# Get_lumped_tissues returns a list with the lumped PCs, vols, and flows:
  lumped_params <- lump_tissues(PCs,tissuelist=tissuelist,species=species)
  if(adjusted.Funbound.plasma){
    fup <- schmitt.params$Funbound.plasma
    warning('Funbound.plasma recalculated with adjustment.  Set adjusted.Funbound.plasma to FALSE to use original value.')
  }else fup <- schmitt.params$unadjusted.Funbound.plasma

  Fgutabs <- try(get_invitroPK_param("Fgutabs",species,chem.cas=chem.cas),silent=T)
  if (class(Fgutabs) == "try-error") Fgutabs <- 1
    
  
 # Check the species argument for capitalization problems and whether or not it is in the table:  
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
  lP <- 1 #Set to 1 in case log P isn't available from table: Kmuc2air will then just be set to Kwater2air below
  lP <- get_physchem_param("logP",chem.cas = chem.cas)

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
    Funbound.plasma = as.numeric(fup), # unitless fraction
    hematocrit = as.numeric(hematocrit), # unitless ratio
    MW = MW)) #g/mol
  
  # Correct for unbound fraction of chemical in the hepatocyte intrinsic clearance assay (Kilford et al., 2008)
 outlist <- c(outlist,list(Fhep.assay.correction=calc_hep_fu(schmitt.params$Pow,pKa_Donor=schmitt.params$pKa_Donor,pKa_Accept=schmitt.params$pKa_Accept)))  # fraction 

  if(vmax.km){
    if(vmax==0){
	#stop("Cannot calculate saturable metabolism with Vmax = 0.") #Do we want to throw an error if MM kinetics are attempted without providing Vmax, or simply default back to first-order as below?
	warning("Cannot calculate saturable metabolism with Vmax = 0. Defaulting to first-order metabolic clearance.")
	outlist <- c(outlist,
    	list(vmax=0,km=1,Clmetabolismc= as.numeric(calc_hepatic_clearance(hepatic.model="unscaled",parameters=list(
                                Clint=Clint, #uL/min/10^6 cells
                                Funbound.plasma=fup, # unitless fraction
                                Fhep.assay.correction=outlist$Fhep.assay.correction, 
                                million.cells.per.gliver= 110, # 10^6 cells/g-liver
                                liver.density= 1.05, # g/mL
                                Dn=0.17,BW=BW,
                                Vliverc=lumped_params$Vliverc, #L/kg
                                Qtotal.liverc=(lumped_params$Qtotal.liverf*Qcardiacc/1000*60) ),suppress.messages=T),million.cells.per.gliver=110,Fgutabs=Fgutabs))) #L/h/kg BW
	}else{
	  outlist <- c(outlist,list(vmax=vmax,km=km,Clmetabolismc=0))}
  }else{
  outlist <- c(outlist,
    list(vmax=0,km=1,Clmetabolismc= as.numeric(calc_hepatic_clearance(hepatic.model="unscaled",parameters=list(
                                Clint=Clint, #uL/min/10^6 cells
                                Funbound.plasma=fup, # unitless fraction
                                Fhep.assay.correction=outlist$Fhep.assay.correction, 
                                million.cells.per.gliver= 110, # 10^6 cells/g-liver
                                liver.density= 1.05, # g/mL
                                Dn=0.17,BW=BW,
                                Vliverc=lumped_params$Vliverc, #L/kg
                                Qtotal.liverc =(lumped_params$Qtotal.liverf*Qcardiacc/1000*60) ),suppress.messages=T),million.cells.per.gliver=110,Fgutabs=Fgutabs))) #L/h/kg BW
  }

    outlist <- c(outlist,Rblood2plasma=available_rblood2plasma(chem.cas=chem.cas,species=species,adjusted.Funbound.plasma=adjusted.Funbound.plasma))
    #alveolar ventilation:15 L/h/kg^.75 from campbell 2007
    #henry's law in atm * m^3 / mol, converted atm to Pa
    #human body temperature of 310 Kelvin
    hl <- subset(chem.physical_and_invitro.data,CAS == chem.cas)[,'HL']
    Kwater2air <- 8.314 * 310 / (hl * 101325)   #310 K body temp, 101325 atm to Pa, 
    Kblood2air <- Kwater2air * outlist$Rblood2plasma / outlist$Funbound.plasma#((1 - parameters$hematocrit) / parameters$Funbound.plasma + parameters$hematocrit * parameters$Krbc2pu)
    #Vdot <- 300 #ml/s
    #Vdot <- Vdot / 1000 *3600 / outlist$BW^.75 #L/h/kg^.75   from ml/s
    Vdot <- 24.75 #L/h
    #Vdot <- Vdot * outlist$BW^.75 #This is scaled in the model code, making scaling here unnecessary
    Fds <- 0.33
    lKair2muc <- log10(1/Kwater2air) - (lP - 1) * 0.524 #If no value is added for logP, it's assumed Kmuc2air = Kwater2air
    Kair2muc <- 10^(lKair2muc)
    Kmuc2air <- 1/Kair2muc
    outlist <- c(outlist,Kblood2air =  Kblood2air,Kmuc2air = Kmuc2air,Qalv=Vdot*(1-Fds))

        
  return(outlist[sort(names(outlist))])
}
