#' Parameterize_SteadyState
#' 
#' This function initializes the parameters needed in the functions
#' calc_mc_css, calc_mc_oral_equiv, and calc_analytic_css for the three
#' compartment steady state model ('3compartmentss').  
#' 
#' 
#' @param chem.name Either the chemical name or the CAS number must be
#' specified.
#' @param chem.cas Either the chemical name or the CAS number must be
#' specified.
#' @param species Species desired (either "Rat", "Rabbit", "Dog", "Mouse", or
#' default "Human").
#' @param clint.pvalue.threshold Hepatic clearances with clearance assays
#' having p-values greater than the threshold are set to zero.
#' @param default.to.human Substitutes missing rat values with human values if
#' true.
#' @param human.clint.fup Uses human hepatic intrinsic clearance and fraction
#' of unbound plasma in calculation of partition coefficients for rats if true.
#' @param adjusted.Funbound.plasma Returns adjusted Funbound.plasma when set to
#' TRUE.
#' @param restrictive.clearance In calculating hepatic.bioavailability, protein
#' binding is not taken into account (set to 1) in liver clearance if FALSE.
#' @param fup.lod.default Default value used for fraction of unbound plasma for
#' chemicals where measured value was below the limit of detection. Default
#' value is 0.0005.
#' @param suppress.messages Whether or not the output message is suppressed.
#' @param minimum.Funbound.plasma Monte Carlo draws less than this value are set 
#' equal to this value (default is 0.0001 -- half the lowest measured Fup in our
#' dataset).
#' @param Caco2.options A list of options to use when working with Caco2 apical to
#' basolateral data \code{Caco2.Pab}, default is Caco2.options = list(Caco2.default = 2,
#' Caco2.Fabs = TRUE, Caco2.Fgut = TRUE, overwrite.invivo = FALSE, keepit100 = FALSE). Caco2.default sets the default value for 
#' Caco2.Pab if Caco2.Pab is unavailable. Caco2.Fabs = TRUE uses Caco2.Pab to calculate
#' fabs.oral, otherwise fabs.oral = \code{Fabs}. Caco2.Fgut = TRUE uses Caco2.Pab to calculate 
#' fgut.oral, otherwise fgut.oral = \code{Fgut}. overwrite.invivo = TRUE overwrites Fabs and Fgut in vivo values from literature with 
#' Caco2 derived values if available. keepit100 = TRUE overwrites Fabs and Fgut with 1 (i.e. 100 percent) regardless of other settings.
#' 
#' @return \item{Clint}{Hepatic Intrinsic Clearance, uL/min/10^6 cells.}
#' \item{Fabsgut}{Fraction of the oral dose absorbed and surviving gut metabolism, i.e. the 
#' fraction of the dose that enters the gutlumen.}  \item{Funbound.plasma}{Fraction of plasma
#' that is not bound.} \item{Qtotal.liverc}{Flow rate of blood exiting the
#' liver, L/h/kg BW^3/4.} \item{Qgfrc}{Glomerular Filtration Rate, L/h/kg
#' BW^3/4, volume of fluid filtered from kidney and excreted.} \item{BW}{Body
#' Weight, kg} \item{MW}{Molecular Weight, g/mol}
#' \item{million.cells.per.gliver}{Millions cells per gram of liver tissue.}
#' \item{Vliverc}{Volume of the liver per kg body weight, L/kg BW.}
#' \item{liver.density}{Liver tissue density, kg/L.}
#' \item{Fhep.assay.correction}{The fraction of chemical unbound in hepatocyte
#' assay using the method of Kilford et al. (2008)}
#' \item{hepatic.bioavailability}{Fraction of dose remaining after first pass
#' clearance, calculated from the corrected well-stirred model.}
#' @author John Wambaugh
#' @keywords Parameter
#' @examples
#' 
#'  parameters <- parameterize_steadystate(chem.name='Bisphenol-A',species='Rat')
#'  parameters <- parameterize_steadystate(chem.cas='80-05-7')
#' 
#' @export parameterize_steadystate
parameterize_steadystate <- function(chem.cas=NULL,
                                     chem.name=NULL,
                                     species="Human",
                                     clint.pvalue.threshold=0.05,
                                     default.to.human=F,
                                     human.clint.fup=F,
                                     adjusted.Funbound.plasma=T,
                                     restrictive.clearance=T,
                                     fup.lod.default=0.005,
                                     suppress.messages=F,
                                     minimum.Funbound.plasma=0.0001,
                                     Caco2.options = list(Caco2.Pab.default = "1.6",
                                                          Caco2.Fgut = TRUE,
                                                          Caco2.Fabs = TRUE,
                                                          overwrite.invivo = FALSE,
                                                          keepit100 = FALSE
                                                          ))
{
  Parameter <- Species <- variable <- Tissue <- NULL
  physiology.data <- physiology.data
  tissue.data <- tissue.data
# Look up the chemical name/CAS, depending on what was provide:
  out <- get_chem_id(chem.cas=chem.cas,chem.name=chem.name)
  chem.cas <- out$chem.cas
  chem.name <- out$chem.name
  
  # Ammend list options
  if(!all(c("Caco2.Pab.default", "Caco2.Fgut", "Caco2.Fabs", "overwrite.invivo", "keepit100")%in% names(unlist(Caco2.options)))){
    Caco2.options <- ammend.httk.option.list(httk.option.list = Caco2.options)
  }

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

  # Rate of disappearance of compound from a hepatocyte incubation
  # (hepatic intrinsic clearance -- uL/min/million hepatocytes):
  Clint.db <- try(get_invitroPK_param("Clint",species,chem.CAS=chem.cas),silent=T)
  # Check that the trend in the CLint assay was significant:
  Clint.pValue <- try(get_invitroPK_param("Clint.pValue",species,chem.CAS=chem.cas),silent=T)
  if (class(Clint.db) == "try-error" & default.to.human || human.clint.fup) 
  {
    Clint.db <- try(get_invitroPK_param("Clint","Human",chem.CAS=chem.cas),silent=T)
    Clint.pValue <- try(get_invitroPK_param("Clint.pValue","Human",chem.CAS=chem.cas),silent=T)
    warning(paste(species,"coerced to Human for metabolic clerance data."))
  }
  if (class(Clint.db) == "try-error") 
    stop("Missing metabolic clearance data for given species. Set default.to.human to true to substitute human value.")
  # Check if clintis a point value or a distribution, if a distribution, use the median:
  if (nchar(Clint.db) - nchar(gsub(",","",Clint.db))==3) 
  {
    Clint.dist <- Clint.db
    Clint.point <- as.numeric(strsplit(Clint.db,",")[[1]][1])
    Clint.pValue <- as.numeric(strsplit(Clint.db,",")[[1]][4])
    if (!suppress.messages) warning("Clint is provided as a distribution.")
  } else {
    Clint.point <- Clint.db
    Clint.dist <- NA
  }
  if (!is.na(Clint.pValue) & Clint.pValue > clint.pvalue.threshold) Clint.point  <- 0
  
  
  
  
  # unitless fraction of chemical unbound with plasma
  # fup.db contains whatever was in the chem.phys table
  fup.db <- try(get_invitroPK_param("Funbound.plasma",species,chem.CAS=chem.cas),silent=T)
  if (class(fup.db) == "try-error" & default.to.human || human.clint.fup) 
  {
    fup.db<- try(get_invitroPK_param("Funbound.plasma","Human",chem.CAS=chem.cas),silent=T)
    if (!suppress.messages) warning(paste(species,"coerced to Human for protein binding data."))
  }
  if (class(fup.db) == "try-error") stop("Missing protein binding data for given species. Set default.to.human to true to substitute human value.")
  # Check if fup is a point value or a distribution, if a distribution, use the median:
  if (nchar(fup.db) - nchar(gsub(",","",fup.db))==2) 
  {
    fup.point <- as.numeric(strsplit(fup.db,",")[[1]][1])
    fup.dist <- fup.db
    if (!suppress.messages) warning("Fraction unbound is provided as a distribution.")
  } else {
    fup.point <- fup.db
    fup.dist <- NA
  }
  if (fup.point == 0)
  {
    fup.point <- fup.lod.default
    if (!suppress.messages) warning("Fraction unbound = 0, changed to 0.005.")
  }

  pKa_Donor <- suppressWarnings(get_physchem_param("pKa_Donor",chem.CAS=chem.cas)) # acid dissociation constants
  pKa_Accept <- suppressWarnings(get_physchem_param("pKa_Accept",chem.CAS=chem.cas)) # basic association cosntants
  Pow <- 10^get_physchem_param("logP",chem.CAS=chem.cas) # Octanol:water partition coeffiecient
  ion <- calc_ionization(pH=7.4,pKa_Donor=pKa_Donor,pKa_Accept=pKa_Accept)
  dow <- Pow * (ion$fraction_neutral + 0.001 * ion$fraction_charged + ion$fraction_zwitter)

  if (adjusted.Funbound.plasma)
  {
    if (human.clint.fup) 
    {
      Flipid <- subset(physiology.data,
                  Parameter=='Plasma Effective Neutral Lipid Volume Fraction')[,
                    which(colnames(physiology.data) == 'Human')]
    } else {
      Flipid <- subset(physiology.data,
                  Parameter=='Plasma Effective Neutral Lipid Volume Fraction')[,
                    which(tolower(colnames(physiology.data)) == tolower(species))]
    }
    fup.adjustment <- max(1 / ((dow) * Flipid + 1 / fup.point)/fup.point,
                        minimum.Funbound.plasma) # Enforcing a sanity check on 
                                                 # plasma binding
    fup.adjusted <- fup.point*fup.adjustment # unitless fraction
    if (!suppress.messages) warning('Funbound.plasma adjusted for in vitro partitioning (Pearce, 2017).  Set adjusted.Funbound.plasma to FALSE to use original value.')
  } else {
    fup.adjusted <- fup.point
    fup.adjustment <- NA
  }
# Restrict values of fup:
  if (fup.adjusted < minimum.Funbound.plasma) fup.adjusted <- minimum.Funbound.plasma
  

  Params <- list()
  Params[["Clint"]] <- Clint.point # uL/min/10^6
  Params[["Clint.dist"]] <- Clint.dist
  Params[["Funbound.plasma.adjustment"]] <- fup.adjustment
  Params[["Funbound.plasma"]] <- fup.adjusted
  Params[["Funbound.plasma.dist"]] <- fup.dist
  Params[["Qtotal.liverc"]] <- Qtotal.liverc/1000*60     #        L/h/kgBW
  Params[["Qgfrc"]] <- QGFRc/1000*60 #        L/h/kgBW     
  Params[["Dow74"]] <- dow # unitless istribution coefficient at plasma pH 7.4
  Params[["BW"]] <- BW # kg
  Params[["MW"]] <- get_physchem_param("MW",chem.CAS=chem.cas) # molecular weight g/mol
# Correct for unbound fraction of chemical in the hepatocyte intrinsic clearance assay (Kilford et al., 2008)
  Params[["Fhep.assay.correction"]] <- calc_fu_hep(Pow,
                                         pKa_Donor=pKa_Donor,
                                         pKa_Accept=pKa_Accept) # fraction 
  Params[["million.cells.per.gliver"]] <- 110 # 10^6 cells/g-liver
  Params[["Vliverc"]] <- Vliverc # L/kg BW
  Params[["liver.density"]] <- 1.05 # g/mL
  
  Rb2p <- available_rblood2plasma(chem.name=chem.name,
            chem.cas=chem.cas,
            species=species,
            adjusted.Funbound.plasma=adjusted.Funbound.plasma,
            suppress.messages=T)
  Params[["Rblood2plasma"]] <- Rb2p
  
  # Need to have a parameter with this name to calculate clearance, but need 
  # clearance to calculate bioavailability:
  Params[["hepatic.bioavailability"]] <- NA
  cl_us <- calc_hepatic_clearance(parameters=Params,
          hepatic.model='unscaled',
          suppress.messages=T)#L/h/kg body weight
  Params[["cl_us"]] <- cl_us
  
  if(Caco2.options$keepit100 == TRUE){
    Params[["Fabs"]] <- 1
    Params[["Fgut"]] <- 1
    Params[["Fabsgut"]] <- 1
    Params[["Caco2.Pab"]] <- 10
    Params[["Caco2.Pab.dist"]] <- NA
  }else{
    # Caco-2 Pab:
    Caco2.Pab.db <- try(get_invitroPK_param("Caco2.Pab", species = "Human", chem.CAS = chem.cas), silent = T)
    if (class(Caco2.Pab.db) == "try-error"){  
      Caco2.Pab.db <- as.character(Caco2.options$Caco2.Pab.default)
      warning(paste0("Default value of ", Caco2.options$Caco2.Pab.default, " used for Caco2 permeability."))
    }
    # Check if Caco2 a point value or a distribution, if a distribution, use the median:
    if (nchar(Caco2.Pab.db) - nchar(gsub(",","",Caco2.Pab.db)) == 2) 
    {
      Caco2.Pab.dist <- Caco2.Pab.db
      Caco2.Pab.point <- as.numeric(strsplit(Caco2.Pab.db,",")[[1]][1])
      if (!suppress.messages) warning("Clint is provided as a distribution.")
    } else {
      Caco2.Pab.point <- as.numeric(Caco2.Pab.db)
      Caco2.Pab.dist <- NA
    }
    
    Params[["Caco2.Pab"]] <- Caco2.Pab.point
    Params[["Caco2.Pab.dist"]] <- Caco2.Pab.dist
    
    # Select Fabs, optionally overwrite based on Caco2.Pab
    Fabs <- try(get_invitroPK_param("Fabs",species,chem.CAS=chem.cas),silent=T)
    if (class(Fabs) == "try-error" | Caco2.options$overwrite.invivo == TRUE){
      if(Caco2.options$overwrite == TRUE | (Caco2.options$Caco2.Fabs == TRUE & class(Fabs) == "try-error")){
        Params[["Fabs"]] <- 1
        Fabs <- calc_fabs.oral(Params = Params, species = "Human") # only calculable for human, assume the same across species
      }else{
        Fabs <- 1
      }
    }
    
    Fgut <- try(get_invitroPK_param("Fgut",species,chem.CAS=chem.cas),silent=T)
    if (class(Fgut) == "try-error" | Caco2.options$overwrite.invivo == TRUE){
      if(Caco2.options$overwrite == TRUE | (Caco2.options$Caco2.Fgut == TRUE & class(Fgut) == "try-error")){
        Params[["Fgut"]] <- 1
        Fgut <- calc_fgut.oral(Params = Params, species = species) 
      }else{
        Fgut <- 1
      }
    }
    Params[['Fabsgut']] <- Fabs*Fgut
    Params[['Fabs']] <- Fabs
    Params[['Fgut']] <- Fgut
  }
  Qliver <- Params$Qtotal.liverc / Params$BW^.25 #L/h/kg body weight

  if (restrictive.clearance) 
  { 
    Params[['hepatic.bioavailability']] <- 
      Qliver / (Qliver + fup.adjusted * cl_us / Rb2p) # Units cancel (i.e., unitless)
  } else {
    Params[['hepatic.bioavailability']] <- 
      Qliver / (Qliver + cl_us / Rb2p) # Units cancel (i.e., unitless)
  }
  if (is.na(Params[['hepatic.bioavailability']])) browser() 
  return(Params)
}
