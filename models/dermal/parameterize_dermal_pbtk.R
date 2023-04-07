#' Parameterize_dermal_PBTK
#' 
#' This function initializes the parameters needed in the functions solve_dermal_pbtk.
#' 
#' 
#' @param chem.name Either the chemical name or the CAS number must be
#' specified. 
#' @param chem.cas Either the chemical name or the CAS number must be
#' specified. 
#' @param dtxsid EPA's DSSTox Structure ID (\url{https://comptox.epa.gov/dashboard})   
#' the chemical must be identified by either CAS, name, or DTXSIDs.
#' @param model.type Choice of dermal model, either the default "dermal_1subcomp" for
#' the model with 1 compartment for the skin; or (not usable yet) "dermal" for the 
#' model with 2 sub compartments (a sc and ed layer) for skin which defaults 
#' to the sc layer being the stratum corneum and the ed layer being the combined
#' viable epidermis and dermis.
#' @param method.permeability For "dermal_1subcomp" model, method of calculating 
#' the permeability coefficient, P, either "Potts-Guy" or "UK-Surrey". 
#' Default is "UK-Surrey" (Sawyer et al., 2016 and Chen et al., 2015), which uses Fick's
#' law of diffusion to calculate P. For "dermal" model, this parameter is ignored.
#' @param species Species desired (either "Rat", "Rabbit", "Dog", "Mouse", or
#' default "Human").
#' @param default.to.human Substitutes missing animal values with human values
#' if true (hepatic intrinsic clearance or fraction of unbound plasma).
#' @param tissuelist Specifies compartment names and tissues groupings.
#' Remaining tissues in tissue.data are lumped in the rest of the body.
#' However, solve_dermal_pbtk only works with the default parameters.
#' @param force.human.clint.fup Forces use of human values for hepatic
#' intrinsic clearance and fraction of unbound plasma if true.
#' @param clint.pvalue.threshold Hepatic clearances with clearance assays
#' having p-values greater than the threshold are set to zero.
#' @param adjusted.Funbound.plasma Returns adjusted Funbound.plasma when set to
#' TRUE along with parition coefficients calculated with this value.
#' @param regression Whether or not to use the regressions in calculating
#' partition coefficients.
#' @param suppress.messages Whether or not the output message is suppressed.
#' @param minimum.Funbound.plasma Monte Carlo draws less than this value are set 
#' equal to this value (default is 0.0001 -- half the lowest measured Fup in our
#' dataset).
#' @param skin_depth skin_depth of skin, cm, used in calculating P.
#' @param skin.pH pH of dermis/skin, used in calculating P and Kskin2vehicle.
#' @param height Height in cm, used in calculating totalSA.
#' @param Kvehicle2water Partition coefficient for the vehicle (sometimes called the 
#' vehicle) carrying the chemical to water. Default is "water", which assumes the vehicle is water.
#' Other optional inputs are "octanol", "olive oil", or a numeric value.
#' @param InfiniteDose If TRUE, we assume infinite dosing (i.e., a constant unchanging concentration
#' of chemical in the vehicle is considered) and Cvehicle is a constant. If
#' FALSE (default), dosing is finite and Cvehicle changes over time.
#' @return
#' 
#' \item{BW}{Body Weight, kg.} 
#' \item{Clmetabolismc}{Hepatic Clearance, L/h/kg BW.} 
#' \item{Fgutabs}{Fraction of the oral dose absorbed, i.e. the fraction of
#' the dose that enters the gutlumen.} 
#' \item{Fhep.assay.correction}{The fraction of chemical unbound in hepatocyte 
#' assay using the method of Kilford et al.(2008)} 
#' \item{Fskin_depth_sc}{Fraction of skin depth in strateum corneum so that the depth
#' of the SC is Fskin_depth_sc*skin_depth. This parameter does not appear when 
#' model.type="dermal_1subcomp".} 
#' \item{Fskin_depth_ed}{Fraction of skin depth in combined viable epidermis and
#' dermis so that the depth of the ED is Fskin_depth_ed*skin_depth. This parameter does not appear when 
#' model.type="dermal_1subcomp".} 
#' \item{Fskin_exposed}{Fraction of skin exposed.} 
#' \item{Funbound.plasma}{Fraction of plasma that is not bound.} 
#' \item{hematocrit}{Percent volume of red blood cells in the blood.}
#' \item{InfiniteDose}{If InfiniteDose=1, infinite dosing is assumed and Cvehicle_infinite
#' is used in place of Cvehicle; if InfiniteDose=0, finite dosing is assumed and
#' Avehicle is used for dosing. When InfiniteDose=1, the state variable Avehicle 
#' does not have meaning.}
#' \item{Kblood2air}{Ratio of concentration of chemical in blood to air, calculated
#' using calc_kair.}
#' \item{Kgut2pu}{Ratio of concentration of chemical in gut tissue to unbound
#' concentration in plasma.} 
#' \item{kgutabs}{Rate that chemical enters the gut from gutlumen, 1/h.} 
#' \item{Kkidney2pu}{Ratio of concentration of chemical in
#' kidney tissue to unbound concentration in plasma.} 
#' \item{Kliver2pu}{Ratio of concentration of chemical in liver tissue to unbound 
#' concentration in plasma.} 
#' \item{Klung2pu}{Ratio of concentration of chemical in lung tissue
#' to unbound concentration in plasma.} 
#' \item{Krbc2pu}{Ratio of concentration of chemical in red blood cells to unbound 
#' concentration in plasma.}
#' \item{Krest2pu}{Ratio of concentration of chemical in rest of body tissue to
#' unbound concentration in plasma.} 
#' \item{Kskin2pu}{Ratio of concentration of chemical in skin tissue to
#' unbound concentration in plasma.}
#' \item{Kskin2vehicle}{Partition coefficient between exposed skin and vehicle. 
#' This parameter only appears when model.type="dermal_1subcomp"
#' and is replaced by Ksc2vehicle when model.type="dermal".} 
#' \item{Ksc2vehicle}{Partition coefficient between SC and vehicle. This parameter 
#' does not appear when model.type="dermal_1subcomp".} 
#' \item{Ksc2ed}{Partition coefficient between ED and
#' SC. This parameter does not appear when model.type="dermal_1subcomp".} 
#' \item{MA}{?}
#' \item{million.cells.per.gliver}{Millions cells per gram of liver tissue.} 
#' \item{MW}{Molecular Weight, g/mol.} 
#' \item{P}{Permeability of the skin, cm/h. When model.type="dermal_1subcomp", 
#' this parameter changes depending on method.permeability. When model.type="dermal",
#' this parameter is replaced by Pvehicle2sc and Psc2ed.} 
#' \item{Pvehicle2sc}{Permeability
#' of the stratum corneum (SC), cm/h. This parameter does not appear when 
#' model.type="dermal_1subcomp".} 
#' \item{Psc2ed}{Permeability of the combined viable epidermis and dermis layer of the skin ("ed"), cm/h.
#' This parameter does not appear when model.type="dermal_1subcomp".} 
#' \item{Qalvc}{Unscaled alveolar ventilation rate, L/h/kg BW^3/4.}
#' \item{Qcardiacc}{Cardiac Output, L/h/kg BW^3/4.} 
#' \item{Qgfrc}{Glomerular Filtration Rate, L/h/kg BW^3/4, volume of fluid filtered 
#' from kidney and excreted.} 
#' \item{Qgutf}{Fraction of cardiac output flowing to the gut.}
#' \item{Qkidneyf}{Fraction of cardiac output flowing to the kidneys.}
#' \item{Qliverf}{Fraction of cardiac output flowing to the liver.}
#' \item{Qlungf}{Fraction of cardiac output flowing to the lung.}
#' \item{Qskinf}{Fraction of cardiac output flowing to the skin, or to the ed
#' layer of the skin when model.type="dermal".}
#' \item{Rblood2plasma}{The ratio of the concentration of the chemical in the
#' blood to the concentration in the plasma from available_rblood2plasma.}
#' \item{skin_depth}{Skin depth, cm.}
#' \item{totalSA}{Total body surface area, cm^2.} 
#' \item{Vartc}{Volume of the arteries per kg body weight, L/kg BW.}
#' \item{Vgutc}{Volume of the gut per kg body weight, L/kg BW.}
#' \item{Vkidneyc}{Volume of the kidneys per kg body weight, L/kg BW.}
#' \item{Vliverc}{Volume of the liver per kg body weight, L/kg BW.}
#' \item{Vlungc}{Volume of the lungs per kg body weight, L/kg BW.}
#' \item{Vrestc}{ Volume of the rest of the body per kg body weight, L/kg BW.}
#' \item{Vvenc}{Volume of the veins per kg body weight, L/kg BW.}
#' \item{Vskinc}{Volume of the skin per kg body weight, L/kg BW.}
#' \item{Vskin_scc}{Volume of the sc or upper layer of the skin per 
#' kg body weight, L/kg BW. This parameter does not appear when 
#' model.type="dermal_1subcomp".}
#' \item{Vskin_edc}{Volume of the combined viable epidermis and dermis layer of the skin per 
#' kg body weight, L/kg BW. This parameter does not appear when model.type="dermal_1subcomp".}
#' @author Annabel Meade, John Wambaugh, and Robert Pearce
#' @references Chen, L., Han, L., Saib, O. and Lian, G. (2015). In Silico Prediction of Percutaneous
#' Absorption and Disposition Kinetics of Chemicals. Pharmaceutical Research 32, 1779-93, 10.1007/s11095-014-1575-0
#' 
#' Kilford, P. J., Gertz, M., Houston, J. B. and Galetin, A.
#' (2008). Hepatocellular binding of drugs: correction for unbound fraction in
#' hepatocyte incubations using microsomal binding or drug lipophilicity data.
#' Drug Metabolism and Disposition 36(7), 1194-7, 10.1124/dmd.108.020834.
#' 
#' Potts, R. O., Guy, R. H. (1992). Predicting skin permeability. Pharmaceutical 
#' research 9(5), 663-9, 10.1002/ajim.4700230505.
#' 
#' Sawyer, M. E., Evans, M. V., Wilson, C. A., Beesley, L. J., Leon, L. S., Eklund, 
#' C. R., Croom, E. L., Pegram, R. A. (2016). Development of a human physiologically 
#' based pharmacokinetic (PBPK) model for dermal permeability for lindane. Toxicology
#' Letters 245, 106-9, 10.1016/j.toxlet.2016.01.008
#' 
#' @keywords Parameter
#' @examples
#' 
#' params <- parameterize_dermal_pbtk(chem.cas="80-05-7")
#' 
#' params <- parameterize_dermal_pbtk(chem.cas="80-05-7", model.type="dermal_1subcomp", 
#' method.permeability="Potts-Guy")
#' 
#' params <- parameterize_dermal_pbtk(chem.cas="80-05-7", model.type="dermal", 
#' Kvehicle2water = "octanol")
#' 
#' 
#'  
#' 
#' @export parameterize_dermal_pbtk


parameterize_dermal_pbtk <- function(chem.cas=NULL,
                              chem.name=NULL,
                              dtxsid=NULL,
                              model.type="dermal_1subcomp", #can also be "dermal"
                              method.permeability = "UK-Surrey",
                              species="Human",
                              default.to.human=F,
                              tissuelist=list(liver=c("liver"),kidney=c("kidney"),lung=c("lung"),gut=c("gut"),skin="skin"),
                              force.human.clint.fup = F,
                              clint.pvalue.threshold=0.05,
                              adjusted.Funbound.plasma=T,
                              regression=T,
                              suppress.messages=F,
                              minimum.Funbound.plasma = 1e-04, #added copying parameterize_gas_pbtk, AEM 1/13/2022
                              skin_depth=0.12,
                              skin.pH=7,
                              height = 175,
                              totalSA = NULL,
                              Kvehicle2water = "water",
                              restrictive.clearance = TRUE,
                              InfiniteDose = 0,
                              ...) 
{
  physiology.data <- physiology.data
  
# We need to describe the chemical to be simulated one way or another:
  if (is.null(chem.cas) & 
      is.null(chem.name) & 
      is.null(dtxsid)) 
    stop('chem.name, chem.cas, or dtxsid must be specified.')
  
# Look up the chemical name/CAS, depending on what was provide:
  out <- get_chem_id(chem.cas=chem.cas,
                     chem.name=chem.name,
                     dtxsid=dtxsid)
  chem.cas <- out$chem.cas
  chem.name <- out$chem.name
  dtxsid <- out$dtxsid
   
  if(class(tissuelist)!='list') stop("tissuelist must be a list of vectors.") 
  # Clint has units of uL/min/10^6 cells
  Clint.db <- try(get_invitroPK_param("Clint",
                                      species,
                                      chem.cas=chem.cas),
                  silent=TRUE)
  # Check that the trend in the Clint assay was significant:
  Clint.pValue <- try(get_introPK_param("Clint.pValue",
                                        species,
                                        chem.cas=chem.cas),
                      silent=TRUE)
  if ((class(Clint.db) == "try-error" & default.to.human) || 
      force.human.clint.fup) 
  {
    Clint.db <- try(get_invitroPK_param("Clint",
                                     "Human",
                                     chem.cas=chem.cas),
                 silent=T)
    Clint.pValue <- try(get_invitroPK_param("Clint.pValue",
                                            "Human",
                                            chem.cas=chem.cas),
                        silent=TRUE)
    if (!suppress.messages){
      warning(paste(species,"coerced to Human for metabolic clearance data."))
    }
  }
  if (class(Clint.db) == "try-error") 
    stop("Missing metabolic clearance data for given species. \n\
         Set default.to.human to true to substitute human value.")
  # Check if Clint is a point value or a distribution, if a distribution, use the median:
  if (nchar(Clint.db) - nchar(gsub(",","",Clint.db))==3)
  {
    Clint.dist <- Clint.db
    Clint <- as.numeric(strsplit(Clint.db,",")[[1]][1])
    Clint.pValue <- as.numeric(strsplit(Clint.db,",")[[1]][4])
    if (!suppress.messages) warning("Clint is provided as a distribution.")
  } else {
    Clint <- Clint.db
    Clint.dist <- NA
  }
  if (!is.na(Clint.pValue) & Clint.pValue > clint.pvalue.threshold) Clint <- 0
  
# Predict the PCs for all tissues in the tissue.data table:
  schmitt.params <- parameterize_schmitt(chem.cas=chem.cas,
                                         species=species,
                                         default.to.human=default.to.human,
                                         force.human.fup=force.human.clint.fup,
                                         suppress.messages=suppress.messages)
  PCs <- predict_partitioning_schmitt(parameters=schmitt.params,
                                      species=species,
                                      adjusted.Funbound.plasma=adjusted.Funbound.plasma,
                                      regression=regression,
                                      suppress.messages=suppress.messages,
                                     minimum.Funbound.plasma=minimum.Funbound.plasma)
# Get_lumped_tissues returns a list with the lumped PCs, vols, and flows:
  lumped_params <- lump_tissues(PCs,
                                tissuelist=tissuelist,
                                species=species,
                                suppress.messages=suppress.messages)

# Check to see if we should use the in vitro fup assay correction:
  if(adjusted.Funbound.plasma){
    fup <- schmitt.params$Funbound.plasma
    if (!suppress.messages) warning('Funbound.plasma recalculated with adjustment.  Set adjusted.Funbound.plasma to FALSE to use original value.')
  } else fup <- schmitt.params$unadjusted.Funbound.plasma

# Restrict the value of fup:
  if (fup < minimum.Funbound.plasma) fup <- minimum.Funbound.plasma

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
  pKa_Donor <- suppressWarnings(get_physchem_param(
    "pKa_Donor",
    chem.cas=chem.cas)) # acid dissociation constants
  pKa_Accept <- suppressWarnings(get_physchem_param(
    "pKa_Accept",
    chem.cas=chem.cas)) # basic association cosntants
  Pow <- 10^get_physchem_param(
    "logP",
    chem.cas=chem.cas) # Octanol:water partition coeffiecient
  
  #INITIALIZE outlist
  outlist <- list()
  
   # Begin flows:
  #mL/min/kgBW converted to L/h/kgBW:
  QGFRc <- this.phys.data["GFR"]/1000*60 
  Qcardiacc = this.phys.data["Cardiac Output"]/1000*60 
  flows <- unlist(lumped_params[substr(names(lumped_params),1,1) == 'Q'])

  outlist <- c(outlist,c(
    Qcardiacc = as.numeric(Qcardiacc),
    flows[!names(flows) %in% c('Qtotal.liverf')], #MWL removed "Qlungf, 9/19/19??? (copying parameterize_gas_pbtk.R, AEM 1/13/2022)
    Qliverf= flows[['Qtotal.liverf']] - flows[['Qgutf']],
    Qgfrc = as.numeric(QGFRc))) 
  
  # end flows  
  
  # Begin volumes:
  # units should be L/kgBW  
  Vartc = this.phys.data["Plasma Volume"]/(1-this.phys.data["Hematocrit"])/2/1000 #L/kgBW
  Vvenc = this.phys.data["Plasma Volume"]/(1-this.phys.data["Hematocrit"])/2/1000 #L/kgBW

  outlist <- c(outlist,
    Vartc = as.numeric(Vartc),
    Vvenc = as.numeric(Vvenc),
    lumped_params[substr(names(lumped_params),1,1) == 'V'],
    lumped_params[substr(names(lumped_params),1,1) == 'K'])
  
  if (model.type=="dermal"){ #rename Kskin2pu for dermal model (2 subcompartments)
    names(outlist)[names(outlist)=="Kskin2pu"] <- "Ked2pu"
  }
  
# Create the list of parameters:
  BW <- this.phys.data["Average BW"]
  hematocrit = this.phys.data["Hematocrit"]
  outlist <- c(outlist,list(BW = as.numeric(BW),
    kgutabs = 2.18, # 1/h
    Funbound.plasma = as.numeric(fup), # unitless fraction
    Funbound.plasma.dist = schmitt.params$Funbound.plasma.dist,
    hematocrit = as.numeric(hematocrit), # unitless ratio
    MW = MW, #g/mol
    Pow = Pow,
    pKa_Donar = pKa_Donor,
    pKa_Accept=pKa_Accept,
    MA=schmitt.params[["MA"]]))
    
  # Correct for unbound fraction of chemical in the hepatocyte intrinsic clearance assay (Kilford et al., 2008)
 outlist <- c(outlist,list(Fhep.assay.correction=calc_hep_fu(
     parameters = list(Pow = schmitt.params$Pow,
                       pKa_Donor=schmitt.params$pKa_Donor,
                       pKa_Accept=schmitt.params$pKa_Accept,
                       suppress.messages=suppress.messages))))  # fraction 
    outlist <- c(outlist,
    list(Clmetabolismc= as.numeric(calc_hep_clearance(hepatic.model="unscaled",parameters=list(
                                Clint=Clint, #uL/min/10^6 cells
                                Funbound.plasma=fup, # unitless fraction
                                Fhep.assay.correction=outlist$Fhep.assay.correction, 
                                million.cells.per.gliver= 110, # 10^6 cells/g-liver
                                liver.density= 1.05, # g/mL
                                Dn=0.17,BW=BW,
                                Vliverc=lumped_params$Vliverc, #L/kg
                                Qtotal.liverc=(lumped_params$Qtotal.liverc)/1000*60),suppress.messages=T)),million.cells.per.gliver=110)) #L/h/kg BW

  outlist <- c(outlist,Rblood2plasma=available_rblood2plasma(chem.cas=chem.cas,
                                                             species=species,
                                                             adjusted.Funbound.plasma=adjusted.Funbound.plasma,
                                                             suppress.messages=suppress.messages),
               Fgutabs=Fgutabs)
  
  # Get the blood:air partition coefficient:
  Kx2air <- calc_kair(chem.name=chem.name,
                      chem.cas=chem.cas,
                      dtxsid=dtxsid,
                      species=species,
                      default.to.human=default.to.human,
                      adjusted.Funbound.plasma=adjusted.Funbound.plasma,
                      suppress.messages=suppress.messages)
  Kblood2air <- Kx2air$Kblood2air
  
  # Get unscaled alveolar ventilation rate (L/h/kg BW^(3/4))
  Vdot <- this.phys.data["Pulmonary Ventilation Rate"]
  Qalvc <- Vdot * (0.67) #L/h/kg^0.75
  
  outlist <- c(outlist,
               Kblood2air = Kblood2air,
               Qalvc = unname(Qalvc))
  
  #Skin parameters
  Fskin_depth_sc = 17/527;
  Fskin_depth_ed = 1 - Fskin_depth_sc;
  if (is.null(totalSA)){
    totalSA <- sqrt(height * unname(BW) / 3600) * 100^2; #TotalSA=4 * (outlist$BW + 7) / (outlist$BW + 90) * 100^2
  }
  # Calculation of dermal partition coefficient (Sawyer, 2016 and Chen, 2015):
    
    # Partition coefficients (sc = stratum corneum, w = water, m = media/vehicle, ed = viable epidermis and dermis layers)
    
    rho_lip = 0.9; rho_w = 1; rho_pro = 1.37 #bulk density of lipid, water, and protein, respectively (g/cm^3) (Nitsche et al. 2006)
    phi_lip = 0.125*0.45; phi_w = 0.55; phi_pro = 0.875*0.45; #volume fractions of lipid, water, and protein phases in SC, respectively
        # Table III, Chen 2010
    Ksc2w <- phi_lip * (rho_lip/rho_w) * Pow^0.69 + phi_pro * (rho_pro/rho_w) * 4.23 * Pow^0.31 #Equation 10, Wang, Chen, Lian, Han, 2010
    
    if (is.numeric(Kvehicle2water)){
      Km2w <- Kvehicle2water
    } else if (Kvehicle2water=="water"){
      Km2w <- 1 #vehicle=water
      if(!suppress.messages) warning("Since parameter Kvehicle2water is null, vehicle containing chemical is assumed to be water.")
    } else if (Kvehicle2water=="octanol"){
      Km2w <- Pow
    } else  if (Kvehicle2water=="olive oil"){
      Km2w <- 4.62 * Pow^0.55 #Figure 2, R^2=0.95, Chen, 2015
    } else { stop('Kvehicle2water must be numeric, "octanol", "olive oil", or "water" and default to vehicle being water.')}
    
    Km2sc = Km2w/Ksc2w; #Equation 1, Chen, 2015
      ionization <- calc_ionization(chem.cas=chem.cas,pH=skin.pH)
      fnon <- 1 - ionization$fraction_charged      
    Ked2w <- 0.7 * (0.68 + 0.32 / fup + 0.025 * fnon * Ksc2w) #Equation 11, Chen , 2015
    Ked2m <-  Ked2w / Km2w
    Ked2sc <- Ked2w / Ksc2w
    
    # Calculate diffusion in Stratum Corneum
      
      # Calculate Diffusion through lipid mortar of SC
      if (MW<=1) Dsc = 2e-9 * exp(-0.46*r_s^2) * 100^2 * 60^2 #m2/s converted to cm2/h
      if (MW>1) Dsc = 3e-13 * 100^2 * 60^2 #m2/s converted to cm2/h
      
      Pm2sc_lipid <- (1/Km2sc) * Dsc / (skin_depth*(1-Fskin_depth_ed))
    
      # Calculate DIffusion thourgh corn bricks of SC
      r_s = (0.9087*MW*(3/4/pi))^(1/3)*1e-10 # emperical equation for calculating solute radius in meters # Equation after Equation 4, Chen, 2009
      #Calculate diffusion in water
      Kval = 1.3806488e-23 #Boltzmann constant m2*kg/s2/K
      temp = 309 #temperature in Kelvin, 36 C - temperature at surface of skin
      eta = 7.1e-4 #viscosity of water at above temperature (Pa s)
      Dw = (Kval*temp)/(6*pi*eta*r_s) # m2/s
      
      thetab = 0.25 # fraction of water content in SC corneocytes ???
      #phib = 0.65 # fraction of water content in SC corneocytes at saturation
      beta = 9.32e-8; alpha = 9.47; gamma = -1.17; lambda=1.09 # model parameters, Chen, 2015
      r_f = 35e-10 # radius of keratin microfibril (35 Angstrom converted to 35e-10 m), Chen, 2015
      k=beta*r_f^2*(1-thetab)^lambda; # hydraulic permeability, Chen 2015
      S = (1-thetab)*((r_s + r_f)/r_f)^2
      Dsc = exp(-alpha*S^lambda) / (1 + r_s/sqrt(k) + r_s^2/(3*k)) * Dw * 100^2 * 60^2 # m2/s converted to cm2/h, Equation 6, Chen 2015
      
      Pm2sc_corn <- (1/Km2sc) * Dsc / (skin_depth*(1-Fskin_depth_ed))
      
      # Average permeabilitys of SC lipid and corneocytes together
      Pm2sc <- 1/(1/Pm2sc_lipid + 1/Pm2sc_corn)
    
      # Diffusion in viable epidermis and dermis: Equation 15, Chen, 2015
      Ded <- 10^(-8.5 - 0.655 * log10(MW)) / (0.68 + 0.32 / fup + 0.025 * fnon * Ksc2w) * 100^2 * 60^2  #cm^2/h
            
      # Permeability coefficient from sc to ed
      Psc2ed <- Ked2sc * Ded / (skin_depth*Fskin_depth_ed) #cm/h
      
    # Permeability coefficient from m to ed
    if (model.type=="dermal_1subcomp") {
    if (method.permeability=="UK-Surrey"){
      skin_depth = skin_depth - 0.002
      P <- Ked2m * Ded / skin_depth #10^(-6.3 - 0.0061 * MW + 0.71 * log10(schmitt.params$Pow)) # cm/h Potts-Guy Equation  
    } else if (method.permeability=="Potts-Guy"){
      P <- 10^(-2.7 -0.0061 * MW + 0.71 * log10(schmitt.params$Pow)) #cm/h
    } else if (model.type=="dermal") {
      if(!suppress.messages) warning("Input method.permeability ignored, since there is only one method do calculate Psc2ed and Pm2sc in this function.")
    } else stop(
      "method.permeatility must be set to either 'Potts-Guy' or 'UK-Surrey'")
    } 

    # Added by AEM, 1/27/2022
    if (model.type=="dermal"){
      outlist <- c(outlist, 
                   totalSA = totalSA,
                   skin_depth = skin_depth,
                   #Fdermabs = 1,
                   Fskin_exposed=0.1,
                   Fskin_depth_sc = Fskin_depth_sc, #"The stratum corneum compartment was assumed to be 11/560th of total skin volume." Poet et al. (2002)
                   Fskin_depth_ed = 1-Fskin_depth_sc, #AEM's best guess
                   Pvehicle2sc = Pm2sc, 
                   Psc2ed = Psc2ed,
                   Ksc2vehicle = 1/Km2sc, #function above
                   Ksc2ed = 1/Ked2sc,
                   #Ked2pu = outlist$Kskin2pu, #partition coefficient
                   Vskin_scc = outlist$Vskinc*Fskin_depth_sc,
                   Vskin_edc=outlist$Vskinc*(1-Fskin_depth_sc),
                   InfiniteDose=InfiniteDose)
    } else if (model.type=="dermal_1subcomp"){
      outlist <- c(outlist,
                   totalSA = totalSA,
                   skin_depth=skin_depth,
                   #Fdermabs=1,
                   Fskin_exposed=0.1,
                   P = P,
                   Kskin2vehicle = Ked2m,
                   InfiniteDose = InfiniteDose) 
    }
    

  return(outlist[sort(names(outlist))])
}