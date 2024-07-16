#' Parameters for a three-compartment toxicokinetic model at steady-state
#' 
#' This function initializes the parameters needed in the functions
#' \code{\link{calc_mc_css}}, \code{\link{calc_mc_oral_equiv}}, and 
#' \code{\link{calc_analytic_css}} for the three
#' compartment steady state model ('3compartmentss') as used in 
#' Rotroff et al. (2010), Wetmore et al. (2012), Wetmore et al. (2015), and 
#' elsewhere. By assuming that enough time has passed to reach steady-state, we
#' eliminate the need for tissue-specific parititon coefficients because we 
#' assume all tissues have come to equilibrium with the unbound concentration
#' in plasma. However, we still use chemical properties to predict the 
#' blood:plasma ratio for estimating first-pass hepatic metabolism for oral
#' exposures.
#'
#' We model systemic oral bioavailability as 
#' \ifelse{html}{\out{F<sub>bio</sub>=F<sub>abs</sub>*F<sub>gut</sub>*F<sub>hep</sub>}}{\eqn{F_{bio}=F_{abs}*F_{gut}*F_{hep}}}.
#' \ifelse{html}{\out{F<sub>hep</sub>}}{\eqn{F_{hep}}}
#' is estimated from in vitro TK data using 
#' \code{\link{calc_hep_bioavailability}}.
#' If \ifelse{html}{\out{F<sub>bio</sub>}}{\eqn{F_{bio}}}
#' has been measured in vivo and is found in
#' table \code{\link{chem.physical_and_invitro.data}} then we set 
#' \ifelse{html}{\out{F<sub>abs</sub>*F<sub>gut</sub>}}{\eqn{F_{abs}*F_{git}}} 
#' to the measured value divided by 
#' \ifelse{html}{\out{F<sub>hep</sub>}}{\eqn{F_{hep}}} 
#' Otherwise, if Caco2 membrane permeability data or predictions
#' are available \ifelse{html}{\out{F<sub>abs</sub>}}{\eqn{F_{abs}}} is estimated
#' using \code{\link{calc_fabs.oral}}.
#' Intrinsic hepatic metabolism is used to very roughly estimate
#' \ifelse{html}{\out{F<sub>gut</sub>}}{\eqn{F_{gut}}}
#' using \code{\link{calc_fgut.oral}}.
#' 
#' @param chem.cas Chemical Abstract Services Registry Number (CAS-RN) -- the 
#' chemical must be identified by either CAS, name, or DTXISD
#' 
#' @param chem.name Chemical name (spaces and capitalization ignored) --  the 
#' chemical must be identified by either CAS, name, or DTXISD
#' 
#' @param dtxsid EPA's DSSTox Structure ID (\url{https://comptox.epa.gov/dashboard})  
#' -- the chemical must be identified by either CAS, name, or DTXSIDs
#' 
#' @param species Species desired (either "Rat", "Rabbit", "Dog", "Mouse", or
#' default "Human").
#' 
#' @param clint.pvalue.threshold Hepatic clearances with clearance assays
#' having p-values greater than the threshold are set to zero.
#' 
#' @param default.to.human Substitutes missing species-specific values with human values if
#' TRUE (default is FALSE).
#' 
#' @param force.human.clint.fup Uses human hepatic intrinsic clearance and fraction
#' of unbound plasma in calculation of partition coefficients for rats if true.
#' 
#' @param adjusted.Funbound.plasma Uses Pearce et al. (2017) lipid binding adjustment
#' for Funbound.plasma (which impacts partition coefficients) when set to TRUE (Default).
#' 
#' @param adjusted.Clint Uses Kilford et al. (2008) hepatocyte incubation
#' binding adjustment for Clint when set to TRUE (Default).
#' 
#' @param restrictive.clearance In calculating hepatic.bioavailability, protein
#' binding is not taken into account (set to 1) in liver clearance if FALSE.
#' 
#' @param fup.lod.default Default value used for fraction of unbound plasma for
#' chemicals where measured value was below the limit of detection. Default
#' value is 0.0005.
#' 
#' @param suppress.messages Whether or not the output message is suppressed.
#' 
#' @param minimum.Funbound.plasma Monte Carlo draws less than this value are set 
#' equal to this value (default is 0.0001 -- half the lowest measured Fup in our
#' dataset).
#' 
#' @param Caco2.options A list of options to use when working with Caco2 apical to
#' basolateral data \code{Caco2.Pab}, default is Caco2.options = list(Caco2.Pab.default = 1.6,
#' Caco2.Fabs = TRUE, Caco2.Fgut = TRUE, overwrite.invivo = FALSE, keepit100 = FALSE). Caco2.Pab.default sets the default value for 
#' Caco2.Pab if Caco2.Pab is unavailable. Caco2.Fabs = TRUE uses Caco2.Pab to calculate
#' fabs.oral, otherwise fabs.oral = \code{Fabs}. Caco2.Fgut = TRUE uses Caco2.Pab to calculate 
#' fgut.oral, otherwise fgut.oral = \code{Fgut}. overwrite.invivo = TRUE overwrites Fabs and Fgut in vivo values from literature with 
#' Caco2 derived values if available. keepit100 = TRUE overwrites Fabs and Fgut with 1 (i.e. 100 percent) regardless of other settings.
#' See \code{\link{get_fbio}} for further details.
#' 
#' @param ... Other parameters
#' 
#' @return \item{Clint}{Hepatic Intrinsic Clearance, uL/min/10^6 cells.}
#' \item{Fabsgut}{Fraction of the oral dose absorbed and surviving gut metabolism, 
#' that is, the fraction of the dose that enters the gutlumen.}  
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
#' @author John Wambaugh and Greg Honda
#'
#' @references 
#'
#' \insertRef{pearce2017httk}{httk}
#'
#' \insertRef{kilford2008hepatocellular}{httk}
#'
#' @seealso \code{\link{calc_analytic_css_3compss}}
#'
#' @seealso \code{\link{apply_clint_adjustment}}
#'
#' @seealso \code{\link{tissue.data}}
#'
#' @seealso \code{\link{physiology.data}}
#'
#' @examples
#' 
#'  parameters <- parameterize_steadystate(chem.name='Bisphenol-A',species='Rat')
#'  parameters <- parameterize_steadystate(chem.cas='80-05-7')
#' 
#' @keywords 3compss
#' 
#' @export parameterize_steadystate
parameterize_steadystate <- function(
                              chem.cas=NULL,
                              chem.name=NULL,
                              dtxsid=NULL,
                              species="Human",
                              clint.pvalue.threshold=0.05,
                              default.to.human=FALSE,
                              force.human.clint.fup=FALSE,
                              adjusted.Funbound.plasma=TRUE,
                              adjusted.Clint=TRUE,
                              restrictive.clearance=TRUE,
                              fup.lod.default=0.005,
                              suppress.messages=FALSE,
                              minimum.Funbound.plasma=0.0001,
                              Caco2.options=NULL,
                              ...)
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

# Make sure we have all the parameters we need:
  check_model(chem.cas=chem.cas, 
              chem.name=chem.name,
              dtxsid=dtxsid,
              model="3compartmentss",
              species=species,
              default.to.human=default.to.human)

  #Capitalize the first letter of species only:
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
# Get the intrinsic hepatic clearance:  
  Clint.list <- get_clint(
      dtxsid=dtxsid,
      chem.name=chem.name,
      chem.cas=chem.cas,
      species=species,
      default.to.human=default.to.human,
      force.human.clint=force.human.clint.fup,
      clint.pvalue.threshold=clint.pvalue.threshold,
      suppress.messages=suppress.messages) 
  Clint.point <- Clint.list$Clint.point
  Clint.dist <- Clint.list$Clint.dist

# Get phys-chemical properties:
  MW <- get_physchem_param("MW",chem.cas=chem.cas) #g/mol
  # acid dissociation constants
  pKa_Donor <- suppressWarnings(get_physchem_param(
    "pKa_Donor",
    chem.cas=chem.cas)) 
  # basic association cosntants
  pKa_Accept <- suppressWarnings(get_physchem_param(
    "pKa_Accept",
    chem.cas=chem.cas)) 
  # Octanol:water partition coefficient
  Pow <- 10^get_physchem_param(
    "logP",
    chem.cas=chem.cas) 
    
# Calculate unbound fraction of chemical in the hepatocyte intrinsic 
# clearance assay (Kilford et al., 2008)
  Fu_hep <- calc_hep_fu(parameters=list(
    Pow=Pow,
    pKa_Donor=pKa_Donor,
    pKa_Accept=pKa_Accept)) # fraction 

# Correct for unbound fraction of chemical in the hepatocyte intrinsic 
# clearance assay (Kilford et al., 2008)
  if (adjusted.Clint) Clint.point <- apply_clint_adjustment(
                               Clint.point,
                               Fu_hep=Fu_hep,
                               suppress.messages=suppress.messages)
  
  # Get the central tendency (point estimate) and potentially the distribution
  # quantiles for the fraction unbound in plasma (fup):
  fup.list <- get_fup(
      dtxsid=dtxsid,
      chem.name=chem.name,
      chem.cas=chem.cas,
      species=species,
      default.to.human=default.to.human,
      force.human.fup=force.human.clint.fup,
      suppress.messages=suppress.messages) 
  fup.point <- fup.list$Funbound.plasma.point
  fup.dist <- fup.list$Funbound.plasma.dist 

  if (fup.point == 0)
  {
    fup.point <- fup.lod.default
    if (!suppress.messages) warning(paste0(
        "Fraction unbound = 0, changed to ",
        fup.lod.default,
        "."))
  }

# Distribution coefficient:
  dow<- calc_dow(Pow = Pow, pH=7.4, pKa_Donor=pKa_Donor, pKa_Accept=pKa_Accept)

  # Get the Pearce et al. (2017) lipid binding correction:       
  fup.adjustment <- calc_fup_correction(fup.point,
                                        dtxsid=dtxsid,
                                        chem.name=chem.name,
                                        chem.cas=chem.cas,
                                        species=species,
                                        default.to.human=default.to.human,
                                        force.human.fup=force.human.clint.fup,
                                        suppress.messages=suppress.messages)

  # Apply the correction if requested:
  if (adjusted.Funbound.plasma)
  { 
    fup.corrected <- apply_fup_adjustment(
                       fup.point,
                       fup.correction=fup.adjustment,
                       suppress.messages=suppress.messages,
                       minimum.Funbound.plasma=minimum.Funbound.plasma
                       )
  } else {
    fup.corrected <- fup.point
    fup.adjustment <- NA
  }
  
  Params <- list()
  Params[["Clint"]] <- Clint.point # uL/min/10^6
  Params[["Clint.dist"]] <- Clint.dist
  Params[["Funbound.plasma.adjustment"]] <- fup.adjustment
  Params[["Funbound.plasma"]] <- fup.corrected
  Params[["Funbound.plasma.dist"]] <- fup.dist
  Params[["Qtotal.liverc"]] <- Qtotal.liverc    #        L/h/kgBW
  Params[["Qgfrc"]] <- QGFRc/1000*60 #        L/h/kgBW     
  Params[["Dow74"]] <- dow # unitless istribution coefficient at plasma pH 7.4
  Params[["BW"]] <- BW # kg
  Params[["MW"]] <- 
    get_physchem_param("MW",chem.cas=chem.cas) # molecular weight g/mol
  Params[["Fhep.assay.correction"]] <- Fu_hep
  Params[["million.cells.per.gliver"]] <- 110 # 10^6 cells/g-liver
  Params[["Vliverc"]] <- Vliverc # L/kg BW
  Params[["liver.density"]] <- 1.05 # g/mL
  
# Blood to plasma ratio:
  Rb2p <- available_rblood2plasma(
            chem.name=chem.name,
            chem.cas=chem.cas,
            dtxsid=dtxsid,
            species=species,
            adjusted.Funbound.plasma=fup.corrected,
            suppress.messages=TRUE)
  Params[["Rblood2plasma"]] <- Rb2p
  
# Oral bioavailability parameters:
# Need to have a parameter with this name to calculate clearance, but need 
# clearance to calculate bioavailability:
  Params[["hepatic.bioavailability"]] <- NA
  cl <- calc_hep_clearance(parameters=Params,
          hepatic.model="unscaled",
          restrictive.clearance = restrictive.clearance,
          suppress.messages=TRUE) #L/h/kg body weight
          
# "hepatic bioavailability" simulates first-pass hepatic metabolism since we 
# don't explicitly model blood from the gut:
  Params[['hepatic.bioavailability']] <- calc_hep_bioavailability(
    parameters=list(Qtotal.liverc=Qtotal.liverc, # L/h/kg^3/4
                    Funbound.plasma=fup.corrected,
                    Clmetabolismc=cl, # L/h/kg
                    Rblood2plasma=Params[["Rblood2plasma"]],
                    BW=BW),
    restrictive.clearance=restrictive.clearance)

  if (is.na(Params[['hepatic.bioavailability']])) browser() 

  Params <- c(
    Params, do.call(get_fbio, args=purrr::compact(c(
    list(
      parameters=Params,
      dtxsid=dtxsid,
      chem.cas=chem.cas,
      chem.name=chem.name,
      species=species,
      suppress.messages=suppress.messages
      ),
    Caco2.options))
    ))
  
  return(lapply(Params[model.list[["3compartmentss"]]$param.names],
                set_httk_precision))
}
