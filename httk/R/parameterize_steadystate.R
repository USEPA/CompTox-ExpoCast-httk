#' Parameterize_SteadyState
#' 
#' This function initializes the parameters needed in the functions
#' calc_mc_css, calc_mc_oral_equiv, and calc_analytic_css for the three
#' compartment steady state model ('3compartmentss').  
#' 
#' 
#' @param chem.cas Chemical Abstract Services Registry Number (CAS-RN) -- the 
#' chemical must be identified by either CAS, name, or DTXISD
#' @param chem.name Chemical name (spaces and capitalization ignored) --  the 
#' chemical must be identified by either CAS, name, or DTXISD
#' @param dtxsid EPA's DSSTox Structure ID (\url{https://comptox.epa.gov/dashboard})  
#' -- the chemical must be identified by either CAS, name, or DTXSIDs
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
#'
#' @return 
#' \item{Clint}{Hepatic Intrinsic Clearance, uL/min/10^6 cells.}
#' \item{Fgutabs}{Fraction of the oral dose absorbed, i.e. the fraction of the
#' dose that enters the gutlumen.} 
#' \item{Funbound.plasma}{Fraction of plasma that is not bound.} 
#' \item{Qtotal.liverc}{Flow rate of blood exiting the liver, L/h/kg BW^3/4.} 
#' \item{Qgfrc}{Glomerular Filtration Rate, L/h/kg
#' BW^3/4, volume of fluid filtered from kidney and excreted.} 
#' \item{BW}{Body Weight, kg} 
#' \item{MW}{Molecular Weight, g/mol}
#' \item{million.cells.per.gliver}{Millions cells per gram of liver tissue.}
#' \item{Vliverc}{Volume of the liver per kg body weight, L/kg BW.}
#' \item{liver.density}{Liver tissue density, kg/L.}
#' \item{Fhep.assay.correction}{The fraction of chemical unbound in hepatocyte
#' assay using the method of Kilford et al. (2008)}
#' \item{hepatic.bioavailability}{Fraction of dose remaining after first pass
#' clearance, calculated from the corrected well-stirred model.}
#'
#' @author John Wambaugh
#'
#' @references Pearce, Robert G., et al. "Httk: R package for high-throughput 
#' toxicokinetics." Journal of statistical software 79.4 (2017): 1.
#'
#' Kilford, P. J., Gertz, M., Houston, J. B. and Galetin, A.
#' (2008). Hepatocellular binding of drugs: correction for unbound fraction in
#' hepatocyte incubations using microsomal binding or drug lipophilicity data.
#' Drug Metabolism and Disposition 36(7), 1194-7, 10.1124/dmd.108.020834.
#'
#' @keywords Parameter 3compss
#'
#' @examples
#' 
#'  parameters <- parameterize_steadystate(chem.name='Bisphenol-A',species='Rat')
#'  parameters <- parameterize_steadystate(chem.cas='80-05-7')
#' 
#' @export parameterize_steadystate
parameterize_steadystate <- function(
                              chem.cas=NULL,
                              chem.name=NULL,
                              dtxsid=NULL,
                              species="Human",
                              clint.pvalue.threshold=0.05,
                              default.to.human=F,
                              human.clint.fup=F,
                              adjusted.Funbound.plasma=T,
                              restrictive.clearance=T,
                              fup.lod.default=0.005,
                              suppress.messages=F,
                              minimum.Funbound.plasma=0.0001)
{
#R CMD CHECK throws notes about "no visible binding for global variable", for
#each time a data.table column name is used without quotes. To appease R CMD
#CHECK, a variable has to be created for each of these column names and set to
#NULL. Note that within the data.table, these variables will not be NULL! Yes,
#this is pointless and annoying.
  Parameter <- Species <- variable <- Tissue <- NULL
  physiology.data <- physiology.data
  tissue.data <- tissue.data
#End R CMD CHECK appeasement.  

# We need to describe the chemical to be simulated one way or another:
  if (is.null(chem.cas) & 
      is.null(chem.name) & 
      is.null(dtxsid)) 
    stop('chem.name, chem.cas, or dtxsid must be specified.')

# Look up the chemical name/CAS, depending on what was provide:
  out <- get_chem_id(
          chem.cas=chem.cas,
          chem.name=chem.name,
          dtxsid=dtxsid)
  chem.cas <- out$chem.cas
  chem.name <- out$chem.name                                
  dtxsid <- out$dtxsid

  #Capitilize the first letter of species only:
  species <- tolower(species)
  substring(species,1,1) <- toupper(substring(species,1,1))

  if (!(species %in% colnames(physiology.data)))
  {
    if (toupper(species) %in% toupper(colnames(physiology.data)))
    {
      phys.species <- 
        colnames(physiology.data)[toupper(colnames(physiology.data))==
                                    toupper(species)]
    } else stop(paste("Physiological PK data for",species,"not found."))
  } else phys.species <- species
  this.phys.data <- physiology.data[,phys.species]
  names(this.phys.data) <- physiology.data[,1]
  
  QGFRc <- this.phys.data[["GFR"]] #mL/min/kgBW
  BW <- this.phys.data[["Average BW"]]
    

  Qtotal.liverc <- subset(tissue.data,
                          tolower(Species) == tolower(species) & 
                          variable == 'Flow (mL/min/kg^(3/4))' & 
                          Tissue == 'liver')[,'value']/1000*60 #L/h/(kg BW)^3/4
  Vliverc <- subset(tissue.data,
               tolower(Species) == tolower(species) & 
               variable == 'Vol (L/kg)' & 
               Tissue == 'liver')[,'value'] # L/kg BW

  # Rate of disappearance of compound from a hepatocyte incubation
  # (hepatic intrinsic clearance -- uL/min/million hepatocytes):
  Clint.db <- try(get_invitroPK_param(
                    "Clint",
                    species,
                    chem.cas=chem.cas,
                    chem.name=chem.name,
                    dtxsid=dtxsid),
                silent=T)
                
  # Check that the trend in the CLint assay was significant:
  Clint.pValue <- try(get_invitroPK_param(
                        "Clint.pValue",
                        species,
                        chem.cas=chem.cas,
                        chem.name=chem.name,
                        dtxsid=dtxsid),
                     silent=T)
                    
  if (class(Clint.db) == "try-error" & default.to.human || human.clint.fup) 
  {
    Clint.db <- try(get_invitroPK_param(
                      "Clint",
                      "Human",
                        chem.cas=chem.cas,
                        chem.name=chem.name,
                        dtxsid=dtxsid),
                  silent=T)
    Clint.pValue <- try(get_invitroPK_param(
                          "Clint.pValue",
                          "Human",
                        chem.cas=chem.cas,
                        chem.name=chem.name,
                        dtxsid=dtxsid),
                      silent=T)
    warning(paste(species,"coerced to Human for metabolic clerance data."))
  }
  if (class(Clint.db) == "try-error") 
    stop("Missing metabolic clearance data for given species.\n\
Set default.to.human to true to substitute human value.")
# Check if clintis a point value or a distribution, if a distribution, 
#use the median:
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
  if (!is.na(Clint.pValue) & 
      Clint.pValue > clint.pvalue.threshold) Clint.point  <- 0
  
  # unitless fraction of chemical unbound with plasma
  # fup.db contains whatever was in the chem.phys table
  fup.db <- try(get_invitroPK_param(
                  "Funbound.plasma",
                  species,
                        chem.cas=chem.cas,
                        chem.name=chem.name,
                        dtxsid=dtxsid),
              silent=T)
  if (class(fup.db) == "try-error" & default.to.human || human.clint.fup) 
  {
    fup.db<- try(get_invitroPK_param(
                   "Funbound.plasma",
                   "Human",
                        chem.cas=chem.cas,
                        chem.name=chem.name,
                        dtxsid=dtxsid),
               silent=T)
    if (!suppress.messages) 
      warning(paste(species,"coerced to Human for protein binding data."))
  }
  if (class(fup.db) == "try-error") 
    stop("Missing protein binding data for given species.\n\
Set default.to.human to true to substitute human value.")
# Check if fup is a point value or a distribution, if a distribution, 
# use the median:
  if (nchar(fup.db) - nchar(gsub(",","",fup.db))==2) 
  {
    fup.point <- as.numeric(strsplit(fup.db,",")[[1]][1])
    fup.dist <- fup.db
    if (!suppress.messages) 
      warning("Fraction unbound is provided as a distribution.")
  } else {
    fup.point <- fup.db
    fup.dist <- NA
  }
  if (fup.point == 0)
  {
    fup.point <- fup.lod.default
    if (!suppress.messages) warning("Fraction unbound = 0, changed to 0.005.")
  }

# acid dissociation constants
  pKa_Donor <- suppressWarnings(get_physchem_param(
                                  "pKa_Donor",
                        chem.cas=chem.cas,
                        chem.name=chem.name,
                        dtxsid=dtxsid))
# basic association cosntants
  pKa_Accept <- suppressWarnings(get_physchem_param(
                                   "pKa_Accept",
                        chem.cas=chem.cas,
                        chem.name=chem.name,
                        dtxsid=dtxsid))
# Octanol:water partition coeffiecient
  Pow <- 10^get_physchem_param("logP",chem.cas=chem.cas) 
  ion <- calc_ionization(pH=7.4, pKa_Donor=pKa_Donor, pKa_Accept=pKa_Accept)
  dow <- Pow * (ion$fraction_neutral + 
                0.001 * ion$fraction_charged + 
                ion$fraction_zwitter)

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
    if (!suppress.messages) 
      warning('Funbound.plasma adjusted for in vitro partitioning (Pearce, 2017).')
  } else {
    fup.adjusted <- fup.point
    fup.adjustment <- NA
  }
# Restrict values of fup:
  if (fup.adjusted < minimum.Funbound.plasma) 
    fup.adjusted <- minimum.Funbound.plasma
  
  Fgutabs <- try(get_invitroPK_param("Fgutabs",
                   species,
                        chem.cas=chem.cas,
                        chem.name=chem.name,
                        dtxsid=dtxsid),
               silent=T)
  if (class(Fgutabs) == "try-error") Fgutabs <- 1

  Params <- list()
  Params[["Clint"]] <- Clint.point # uL/min/10^6
  Params[["Clint.dist"]] <- Clint.dist
  Params[["Funbound.plasma.adjustment"]] <- fup.adjustment
  Params[["Funbound.plasma"]] <- fup.adjusted
  Params[["Funbound.plasma.dist"]] <- fup.dist
  Params[["Qtotal.liverc"]] <- Qtotal.liverc    #        L/h/kgBW
  Params[["Qgfrc"]] <- QGFRc/1000*60 #        L/h/kgBW     
  Params[["Dow74"]] <- dow # unitless istribution coefficient at plasma pH 7.4
  Params[["BW"]] <- BW # kg
  Params[["MW"]] <- 
    get_physchem_param("MW",chem.cas=chem.cas) # molecular weight g/mol
# Correct for unbound fraction of chemical in the hepatocyte intrinsic 
# clearance assay (Kilford et al., 2008)
  Params[["Fhep.assay.correction"]] <- calc_hep_fu(parameters=list(Pow=Pow,
                                         pKa_Donor=pKa_Donor,
                                         pKa_Accept=pKa_Accept,
                                         suppress.messages=suppress.messages)) # fraction 
  Params[["million.cells.per.gliver"]] <- 110 # 10^6 cells/g-liver
  Params[["Vliverc"]] <- Vliverc # L/kg BW
  Params[["liver.density"]] <- 1.05 # g/mL
  Params[['Fgutabs']] <- Fgutabs
  
  Rb2p <- available_rblood2plasma(chem.name=chem.name,
            chem.cas=chem.cas,
            species=species,
            adjusted.Funbound.plasma=fup.adjusted,
            suppress.messages=T)
  Params[["Rblood2plasma"]] <- Rb2p

# Need to have a parameter with this name to calculate clearance, but need 
# clearance to calculate bioavailability:
  Params[["hepatic.bioavailability"]] <- NA
  cl <- calc_hep_clearance(parameters=Params,
          hepatic.model='unscaled',
          suppress.messages=T)#L/h/kg body weight

  Params[['hepatic.bioavailability']] <- calc_hep_bioavailability(
    parameters=list(Qtotal.liverc=Qtotal.liverc, # L/h/kg^3/4
                    Funbound.plasma=fup.adjusted,
                    Clmetabolismc=cl, # L/h/kg
                    Rblood2plasma=Params[["Rblood2plasma"]],
                    BW=BW),
    restrictive.clearance=restrictive.clearance)

  if (is.na(Params[['hepatic.bioavailability']])) browser() 
  return(lapply(Params,set_httk_precision))
}
