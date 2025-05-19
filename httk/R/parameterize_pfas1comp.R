#' Parameters for a one compartment (empirical) toxicokinetic model for PFAS
#' 
#' This function initializes the parameters needed in the function solve_1comp.
#' The toxiokinetic model is of the form of an
#' empirical, single compartment in which all tissues are well mixed. 
#' The route of exposure can be oral or intravenous. 
#' For oral exposures a hepatic extraction factor (first-pass 
#' metabolism) is estimated using
#' chemical-specific \emph{in vitro}-measured intrinsic hepatic 
#' clearance and fraction
#' unbound in plasma, if available. If these chemical-specific parameters are 
#' not available then all chemical is assumed to be absorbed. 
#' The rate of oral absorption used
#' is 2.2 L/h, the median rate observed across 44 chemicals by 
#' \href{https://doi.org/10.1093/toxsci/kfy020}{Wambaugh et al. (2018)}.
#' There is a single, unspecified route of elimination (clearance). 
#' Half-life is estimated using the 
#' \href{https://doi.org/10.3390/toxics11020098}{Dawson et al. (2023)} 
#' machine learning model for per- and poly-flurinated alkyl substances (PFAS).
#' In keeping with the findings of that paper, volume of distribtuion is held 
#' fixed at 0.205 L kg/BW. Clearance is calculated as the product of elimination
#' rate (determined from half-life) and the volume of distribution.
#' The ratio of chemical concentration in blood to plasma is determined
#' according to
#' \href{https://doi.org/10.1021/acs.est.7b03299}{Poothong et al. (2017)}
#' where compounds that are ionized at pH 7.4 (plasma) get a value of 0.5, while
#' chemicals that are neutral get a value of 20. 

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
#' @param suppress.messages Whether to suppress messages (Defaults to FALSE).
#'
#' @param restrict.doa Whether to restrict to chemicals within an estimated
#' domain of applicability based on the properties of the training set
#' ("ClassModDomain"), the domain of all models ("AMAD"), or none ("none")
#' (Defaults to "ClassModDomain").
#' @param physchem.exclude Exclude chemicals on the basis of physico-chemical
#' properties (currently only Henry's law constant) as specified by 
#' the relevant modelinfo_[MODEL] file (default TRUE).
#'
#' @param estimate.firstpass Whether to estimate first-pass hepatic metabolism,
#' which can only be done for a subset of PFAS with in vitro HTTK parameters
#' (Defaults to TRUE).
#' 
#' @param Caco2.options A list of options to use when working with Caco2 apical 
#' to basolateral data \code{Caco2.Pab}, default is Caco2.options = 
#' list(Caco2.Pab.default = 1.6, Caco2.Fabs = TRUE, Caco2.Fgut = TRUE, 
#' overwrite.invivo = FALSE, keepit100 = FALSE). Caco2.Pab.default sets the 
#' default value for Caco2.Pab if Caco2.Pab is unavailable. Caco2.Fabs = TRUE 
#' uses Caco2.Pab to calculate fabs.oral, otherwise fabs.oral = \code{Fabs}. 
#' Caco2.Fgut = TRUE uses Caco2.Pab to calculate 
#' fgut.oral, otherwise fgut.oral = \code{Fgut}. overwrite.invivo = TRUE 
#' overwrites Fabs and Fgut in vivo values from literature with 
#' Caco2 derived values if available. keepit100 = TRUE overwrites Fabs and Fgut 
#' with 1 (i.e. 100 percent) regardless of other settings.
#' See \code{\link{get_fbio}} for further details.
#' 
#' @param ... Additional arguments, not currently used.





#' 















#' @return 
#' \item{Vdist}{Volume of distribution, units of L/kg BW.}
#' \item{plasma.vol}{Volume of the plasma, L/kg BW.}
#' \item{Fabsgut}{Fraction of the oral dose absorbed, that is, the fraction of the
#' dose that enters the gutlumen.} 
#' \item{Fhep.assay.correction}{Not used for this model} 

#' \item{kelim}{Elimination rate, units of 1/h.} 
#' \item{hematocrit}{Percent volume of red blood cells in the blood.}
#' \item{kgutabs}{Rate chemical is absorbed, 1/h.}
#' \item{million.cells.per.gliver}{Not used for this model}
#' \item{MW}{Molecular Weight, g/mol.} 
#' \item{Rblood2plasma}{The ratio of the concentration of the chemical in the 
#' blood to the concentration in the plasma. Not used in calculations but 
#' included for the conversion of plasma outputs.} 
#' \item{hepatic.bioavailability}{Fraction of dose remaining after
#' first pass clearance, calculated from the corrected well-stirred model.}
#' \item{BW}{Body Weight, kg.}
#' \item{pKa_Donor}{Ionization equilibria (if any) for hydrogen donation (acids).} 
#' \item{pKa_Accept}{Ionization equilibria (if any) for hydrogen acceptance (bases).} 
#'
#' @author John Wambaugh 
#'
#' @references 
#' Dawson, Daniel E., et al. "A Machine Learning Model to Estimate 
#' Toxicokinetic Half-Lives of Per-and Polyfluoro-Alkyl Substances (PFAS) in 
#' Multiple Species." Toxics 11.2 (2023): 98.
#' 
#' \insertRef{pearce2017httk}{httk}
#'
#' \insertRef{schmitt2008general}{httk}
#'
#' \insertRef{pearce2017evaluation}{httk}
#'
#' Wambaugh, John F., et al. "Evaluating in vitro-in vivo extrapolation of 
#' toxicokinetics." Toxicological Sciences 163.1 (2018): 152-169.
#'
#' Poothong, Somrutai, et al. "Distribution of novel and well-known poly-and 
#' perfluoroalkyl substances (PFASs) in human serum, plasma, and whole blood." 
#' Environmental Science & Technology 51.22 (2017): 13388-13396.
#'
#' @keywords Parameter 1compartment PFAS
#'
#' @seealso \code{\link{solve_1comp}}
#'
#' @seealso \code{\link{calc_analytic_css_1comp}}
#'
#' @seealso \code{\link{calc_vdist}}
#'
#' @seealso \code{\link{parameterize_steadystate}}
#'
#' @seealso \code{\link{apply_clint_adjustment}}
#'
#' @seealso \code{\link{tissue.data}}
#'
#' @seealso \code{\link{physiology.data}}
#'
#' @examples
#' # Human elimination rate for PFOA:
#' parameterize_pfas1comp(dtxsid="DTXSID8031865")$kelim
#' # Female rat is much faster than human:
#' parameterize_pfas1comp(dtxsid="DTXSID8031865", species="rat")$kelim
#' # Male rat is slower than female but faster than humans:
#' parameterize_pfas1comp(dtxsid="DTXSID8031865", species="rat", sex="male")$kelim
#'
#' @export parameterize_pfas1comp
parameterize_pfas1comp <- function(
                        chem.cas=NULL,
                        chem.name=NULL,
                        dtxsid = NULL,
                        species='Human',
                        sex="Female",
                        dosingadj="Oral",
                        restrict.doa = "ClassModDomain",
                        estimate.firstpass = TRUE,
                        suppress.messages=FALSE,
                        Caco2.options = list(),
                        ...
                        )
{
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
    
  params <- list()
  params[['Vdist']] <- 0.205 # Dawson et al. (2023)

# Check for valid argument values:
  if (!(tolower(dosingadj) %in% c("iv","oral","other")))
    stop("Argument dosingadj values are limited to \"IV\", \"Oral\", and \"Other\".")
  if (!(tolower(sex) %in% c("female","male")))
    stop("Argument sex values are limited to \"female\" and \"male\".")
  avail.species <- unique(dawson2023$Species)
  if (!(tolower(species) %in% tolower(avail.species)))
    stop(paste("Available species are limited to ",
               paste(avail.species,collapse=", ")))

# Check to see if we have a prediction from Dawson et al. (2023):
  this.subset <- subset(dawson2023,
                        tolower(DTXSID)==tolower(dtxsid))
  if (dim(this.subset)[1]==0)
      stop(paste("No predictions for chemical",
                 dtxsid,
                 "available in table httk::dawson2023"))
  
  if (!(tolower(species) %in% tolower(this.subset$Species)))
        stop(paste("No chemical",
                 dtxsid,
                 "predictions for species",
                 species,
                 "available in table httk::dawson2023"))
  if (!(tolower(sex) %in% tolower(this.subset$Sex)))
      stop(paste("No chemical",
                 dtxsid,
                 "predictions for sex",
                 sex,
                 "available in table httk::dawson2023"))
  if (!(tolower(dosingadj) %in% tolower(this.subset$DosingAdj)))
      stop(paste("No chemical",
                 dtxsid,
                 "predictions for dose route",
                 dosingadj,
                 "available in table httk::dawson2023"))

  this.subset <- subset(this.subset, 
                        tolower(Species) == tolower(species) &
                        tolower(DosingAdj) == tolower(dosingadj) &
                        tolower(Sex) == tolower(sex))
           
  if (tolower(restrict.doa)==tolower("ClassModDomain") &
      this.subset$ClassModDomain==0) 
      stop("Chemical outside domain of applicability estimated from training set.")
      
  if (tolower(restrict.doa)==tolower("AMAD") &
      this.subset$AMAD==0) 
      stop("Chemical outside domain of applicability of one of the models used to estimate half-life.")
 
# Median of the Dawson et al. training set bins in h:             
  if (this.subset$ClassPredFull==1) thalf <- 4.4 
  else if (this.subset$ClassPredFull==2) thalf <- 2.2*24
  else if (this.subset$ClassPredFull==3) thalf <- 33*24
  else thalf <- 3.3*365*24
    
  params[['kelim']] <- log(2)/thalf
  
# We need params with these names to use the 1comp functions, but we don't use them
  params[["Clint"]] <- NA
  params[["Clint.dist"]] <- NA
  params[["Funbound.plasma"]] <- NA
  params[["Funbound.plasma.dist"]] <- NA
  params[["Funbound.plasma.adjustment"]] <- NA
  params[["Fhep.assay.correction"]] <- NA
  params[["Funbound.plasma.dist"]] <- NA
  params[["Caco2.Pab"]] <- NA
  params[["Caco2.Pab.dist"]] <- NA
  
# Phys-chem properties:
  phys.params <-  suppressWarnings(parameterize_schmitt(chem.name=chem.name,
                    chem.cas=chem.cas,
                    species=species,
                    default.to.human=TRUE,
                    class.exclude=FALSE,
                    minimum.Funbound.plasma=1e-4)) 
  params[["Pow"]] <- phys.params[["Pow"]]
  params[["pKa_Donor"]] <- phys.params[["pKa_Donor"]] 
  params[["pKa_Accept"]] <- phys.params[["pKa_Accept"]]
  params[["MA"]] <- phys.params[["MA"]]

# Average kgutabs value across 44 chemicals in Wambaugh et al. (2018):
  params[['kgutabs']] <- 2.18

  # Now let's use calc_ionization to estimate the chemical's charge profile:
  ion <- calc_ionization(
    pH=7.4,
    pKa_Donor=params[["pKa_Donor"]],
    pKa_Accept=params[["pKa_Accept"]])
  
  # Poothong (2017)
  if (ion$fraction_negative > 0.9) params[['Rblood2plasma']] <- 0.5
  else params[['Rblood2plasma']] <- 10

  params[['million.cells.per.gliver']] <- NA
  params[["liver.density"]] <- NA # g/mL
   
# Check the species argument for capitalization problems and whether or not 
# it is in the table:  
  if (!(species %in% colnames(physiology.data)))
  {
    if (toupper(species) %in% toupper(colnames(physiology.data)))
    {
      phys.species <- colnames(physiology.data)[
                        toupper(colnames(physiology.data))==toupper(species)]
    } else stop(paste("Physiological PK data for",species,"not found."))
  } else phys.species <- species

# Load the physiological parameters for this species
  this.phys.data <- physiology.data[,phys.species]
  names(this.phys.data) <- physiology.data[,1]
  params[['BW']] <- this.phys.data[["Average BW"]]
                                
  params[['hematocrit']] <- this.phys.data[["Hematocrit"]]
  params[['plasma.vol']] <- this.phys.data[["Plasma Volume"]]/1000 # L/kg BW

  params[['MW']] <- get_physchem_param("MW",chem.cas=chem.cas)

# Assume well absobred:
  params[["fbio.oral"]] <- 1
  params[["fabs.oral"]] <- 1
  params[["fgut.oral"]] <- 1
  params[["fhep.oral"]] <- 1
  params[["kgutabs"]] <- 10
  params[["Fabsgut"]] <- 1
  params[["hepatic.bioavailability"]] <- 1

  return(lapply(params[model.list[["pfas1compartment"]]$param.names],
                set_httk_precision))
}
