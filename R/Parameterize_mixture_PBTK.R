#' Parameterize_PBTK
#' 
#' This function initializes the parameters needed in the functions
#' solve_mixture_pbtk, simulating a gas/aerosol mixture.
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
#' @param dae Aerodynamic diameter of aerosol, units of micrometers.
#' @param particle.density Particle density of aerosol, g/cm^3.
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
#' \item{Fdeposited}{Fraction of aerosol deposited in airways, absorbed through
#' gut lumen.} \item{DEalv}{Depositional efficiency of alveoli, fraction of
#' aerosol deposited in alveoli, directly absorbed into blood.}
#' \item{Fds}{Fraction of dead space in lungs, used in calculating alveolar
#' ventilation, Vdot * (1-Fds)} \item{Vdot}{Respiratory minute volume, ml/s.}
#' \item{Kblood2air}{Blood: air partition coefficient.} \item{Fgas}{Fraction of
#' gas per unit volume in gas/aerosol mixture.}
#' 
#' @author John Wambaugh and Robert Pearce
#' @references Kilford, P. J., Gertz, M., Houston, J. B. and Galetin, A.
#' (2008). Hepatocellular binding of drugs: correction for unbound fraction in
#' hepatocyte incubations using microsomal binding or drug lipophilicity data.
#' Drug Metabolism and Disposition 36(7), 1194-7, 10.1124/dmd.108.020834.
#' @keywords Parameter
#' @export parameterize_mixture_pbtk



parameterize_mixture_pbtk <- function(chem.cas=NULL,
                              chem.name=NULL,
                              species="Human",
                              default.to.human=F,
                              tissuelist=list(liver=c("liver"),kidney=c("kidney"),lung=c("lung"),gut=c("gut")),
                              force.human.clint.fup = F,
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
 outlist <- c(outlist,list(Fhep.assay.correction=calc_fu_hep(schmitt.params$Pow,pKa_Donor=schmitt.params$pKa_Donor,pKa_Accept=schmitt.params$pKa_Accept)))  # fraction 

  outlist <- c(outlist,
    list(Clmetabolismc= as.numeric(calc_hepatic_clearance(hepatic.model="unscaled",parameters=list(
                                Clint=Clint, #uL/min/10^6 cells
                                Funbound.plasma=fup, # unitless fraction
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
      Vdot <- 300 #ml/s
      Fds <- 0.33
        Finh <- 1 - 0.5 * (1 - 1/(7.6e-4 * dae^2.8 + 1))
        lambda <- 0.0712  #mean free path um
        calc_ccf <- function(x) return(1 + 2 * lambda / x * (1.257 + 0.4 * exp(-1.1 * x / 2 / lambda)))
        fun <- function(x) sqrt(1.5 * calc_ccf(dae)) * dae - x * sqrt(calc_ccf(x) * particle.density)
        dth <- uniroot(fun,c(1e-100,1e100))$root
        ccf <- calc_ccf(dth)
        D <- ccf * 1.38e-16 * 310 / 3 / pi / 1.81e-4 / (dth/1e4)  # cm^2 / s
        Vt <- 750 #ml
        FRC <- 3301 #ml
        VdET <- 50 #ml
        VdBB <- 47 #ml
        Vdbb <- 47 #ml
        Vdtotal <- 146
        tB <- VdBB / Vdot * (1 + 0.5 * Vt / FRC)
        tb <- Vdbb / Vdot * (1 + 0.5 * Vt / FRC)
        ta <- (Vt - VdET - (VdBB + Vdbb) * (1 + Vt / FRC)) / Vdot
        SF <- 1
        Nae <- .5 * (1 - 1/(3e-4 * (dae^2 * Vdot * SF^3) + 1))
        Nth <- .5 * (1 - exp(- 18 * sqrt(D * (Vdot * SF)^-.25)))
        if(Nae < 0) Nae <- 0
        if(Nth < 0) Nth <- 0
        N <- sqrt(Nae^2 + Nth^2)
      #if(N > 1) N <- 1
        Oae <- 1 - 1 / (5.5e-5 * (dae^2 * Vdot * SF^3)^1.17 + 1)
        Oth <- 1 - exp(- 15.1 * (D * (Vdot * SF)^-.25)^.538)
        if(Oae < 0) Oae <- 0
        if(Oth < 0) Oth <- 0
        O <- sqrt(Oae^2 + Oth^2)
        #if(O > 1) O <- 1
        Binae <- 1 - exp(-4.08e-6 * (dae^2 * Vdot * SF^2.3)^1.152)
        Bexae <- 1 - exp(-2.04e-6 * (dae^2 * Vdot * SF^2.3)^1.152)
        Bth <- 1 - exp(-22.02 * SF^1.24 * (1 + 100 * exp(-log10(100 + 10 / dae^.9)^2)) * (D * tB)^0.6391)
        if(Binae < 0) Binae <- 0
        if(Bexae < 0) Bexae <- 0
        if(Bth < 0) Bth <- 0
        Bin <- sqrt(Binae^2 + Bth^2)
       #if(Bin > 1) Bin <- 1
        Bex <- sqrt(Bexae^2 + Bth^2)
      #if(Bex > 1) Bex <- 1
        bae <- 1 - exp(-0.1147 * ((0.056 + tb^1.5) * dae^(tb^-.25))^1.173)
        bth <- 1 - exp(- (-76.8 * 167 * SF^0.65) * (D * tb)^0.5676)
        if(bae < 0) bae <- 0
        if(bth < 0) bth <- 0
        b <- sqrt(bae^2 + bth^2)
      #if(b > 1) b <- 1
        aae <- 1 - exp(-0.146 * SF^0.98 * (dae^2 * ta)^0.6495)
        ath <- 1 - exp(- (170 + 103*SF^2.13) * (D * ta)^0.6101)
        if(aae < 0) aae <- 0
        if(ath < 0) ath <- 0
        a <- sqrt(aae^2 + ath^2)
        if(a > 1) a <- 1
        Nv <- 1
        Ov <- 1
        Bv <- 1 - VdET / Vt
        bv <- 1 - (VdET + VdBB * (1 + Vt / FRC)) / Vt
        av <- 1 - (VdET + (VdBB + Vdbb) * (1 + Vt / FRC)) / Vt
       # 9 filters plus inhalability
      #dej = dej-1 * Nj * ej * (1 / (nj-1) - 1) where ej = vj / vj-1
        deNin <- N *  Finh
        deOin <- O * (1 -N) * Finh#deNin * O * (Ov / Nv) * (1/N - 1)
        deBin <- Bin * Bv * (1 - O) * (1-N) * Finh#deOin * Bin * (Bv / Ov) * (1/O - 1)
        debin <- b * bv * (1 - Bin) * (1 - O) * (1-N) * Finh#deBin * b * (bv / Bv) * (1/Bin -1)
        dea <- a * av * (1 - b) * (1 - Bin) * (1 - O) * (1-N) * Finh#debin * a * (av / bv) * (1/b - 1)
        debex <- b * bv * (1 - a) * (1 - b) * (1 - Bin) * (1 - O) * (1-N) * Finh#dea * b * (bv / av) * (1/a - 1)
        deBex <- Bex * Bv * (1 - a) * (1 - b)^2 * (1 - Bin) * (1 - O) * (1-N) * Finh#debex * Bex * (Bv / bv) * (1/b - 1)
        deOex <- O * (1 - Bex) * (1 - a) * (1 - b)^2 * (1 - Bin) * (1 - O) * (1-N) * Finh#deBex * O * (Ov / Bv) * (1/Bex - 1)
        deNex <- N * (1 - Bex) * (1 - a) * (1 - b)^2 * (1 - Bin) * (1 - O)^2 * (1-N) * Finh#deOex * N * (Nv / Ov) * (1/O - 1)
      
        #Flung <- Finh * (1 - N) * (1 - O) * (1 - Bin) * (1 - b)
        Fdeposited <-  deNin + deOin + deBin + debin + debex + deBex + deOex + deNex
        #Fdeposited.ex <- debex + deBex + deOex + deNex
        Vdot <- Vdot / 1000 *3600 / outlist$BW^.75 #L/h/kg^.75   from ml/s
        outlist <- c(outlist,Fdeposited=Fdeposited,DEalv=dea,Fds=Fds,Vdot=Vdot)

        hl <- subset(chem.physical_and_invitro.data,CAS == chem.cas)[,'HL']
        Kwater2air <- 8.314 * 310 / (hl * 101325)   #310 K body temp, 101325 atm to Pa, 
        Kblood2air <- Kwater2air * outlist$Rblood2plasma / outlist$Funbound.plasma
        outlist <- c(outlist,Kblood2air = Kblood2air,Fgas = 0.5)
 
  return(outlist[sort(names(outlist))])
}
