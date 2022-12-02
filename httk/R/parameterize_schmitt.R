#' Parameters for Schmitt's (2008) Tissue Partition Coefficient Method
#' 
#' This function provides the necessary parameters to run
#' \code{\link{predict_partitioning_schmitt}}, excluding the data in table
#' \code{\link{tissue.data}}. The model is based on the Schmitt (2008) method
#' for predicting tissue:plasma partition coefficients as modified by Pearce 
#' et al. (2017). The modifications include approaches adapted from Peyret 
#' et al. (2010).
#'
#' @param chem.cas Chemical Abstract Services Registry Number (CAS-RN) -- if
#'  parameters is not specified then the chemical must be identified by either
#'  CAS, name, or DTXISD
#' 
#' @param chem.name Chemical name (spaces and capitalization ignored) --  if
#'  parameters is not specified then the chemical must be identified by either
#'  CAS, name, or DTXISD
#' 
#' @param dtxsid EPA's DSSTox Structure ID (\url{https://comptox.epa.gov/dashboard})  
#'  -- if parameters is not specified then the chemical must be identified by 
#' either CAS, name, or DTXSIDs
#' 
#' @param parameters Chemcial and physiological description parameters needed
#' to run the Schmitt et al. (2008) model
#' 
#' @param species Species desired (either "Rat", "Rabbit", "Dog", "Mouse", or
#' default "Human").
#' 
#' @param default.to.human Substitutes missing fraction of unbound plasma with
#' human values if true.
#' 
#' @param force.human.fup Returns human fraction of unbound plasma in
#' calculation for rats if true.
#' When species is specified as rabbit, dog, or mouse, the human unbound
#' fraction is substituted.
#' 
#' @param adjusted.Funbound.plasma Uses Pearce et al. (2017) lipid binding adjustment
#' for Funbound.plasma (which impacts partition coefficients) when set to TRUE (Default).
#' 
#' @param suppress.messages Whether or not the output message is suppressed.
#' 
#' @param minimum.Funbound.plasma Monte Carlo draws less than this value are set 
#' equal to this value (default is 0.0001 -- half the lowest measured Fup in our
#' dataset).
#' 
#' @return
#' \item{Funbound.plasma}{Unbound fraction in plasma, adjusted for lipid binding according to Pearce et al. (2017)}
#' \item{unadjusted.Funbound.plasma}{measured unbound fraction in plasma (0.005
#' if below limit of detection)} \item{Pow}{octanol:water partition coefficient
#' (not log transformed)} \item{pKa_Donor}{compound H dissociation equilibrium
#' constant(s)} \item{pKa_Accept}{compound H association equilibrium
#' constant(s)} \item{MA}{phospholipid:water distribution coefficient, membrane
#' affinity} \item{Fprotein.plasma}{protein fraction in plasma}
#' \item{plasma.pH}{pH of the plasma}
#'
#' @author Robert Pearce and John Wambaugh
#'
#' @keywords Parameter schmitt
#'
#' @seealso \code{\link{predict_partitioning_schmitt}}
#'
#' @seealso \code{\link{tissue.data}}
#'
#' @references 
#' Pearce, Robert G., et al. "Httk: R package for high-throughput 
#' toxicokinetics." Journal of statistical software 79.4 (2017): 1.
#'
#' Schmitt, Walter. "General approach for the calculation of 
#' tissue to plasma partition coefficients." Toxicology in Vitro 22.2 (2008): 
#' 457-467.
#' 
#' Schmitt, Walter. "Corrigendum to: General approach for the calculation of 
#' tissue to plasma partition coefficients" Toxicology in Vitro 22.6 (2008): 1666.
#' 
#' Peyret, Thomas, Patrick Poulin, and Kannan Krishnan. "A unified algorithm 
#' for predicting partition coefficients for PBPK modeling of drugs and 
#' environmental chemicals." Toxicology and applied pharmacology 249.3 (2010): 
#' 197-207.
#'
#' Pearce, Robert G., et al. "Evaluation and calibration of high-throughput 
#' predictions of chemical distribution to tissues." Journal of pharmacokinetics 
#' and pharmacodynamics 44.6 (2017): 549-565.
#'
#' @examples
#' 
#' parameterize_schmitt(chem.name='bisphenola')
#' 
#' @export parameterize_schmitt
parameterize_schmitt <- function(chem.cas=NULL,
                          chem.name=NULL,
                          dtxsid = NULL,
                          parameters=NULL,
                          species="Human",
                          default.to.human=FALSE,
                          force.human.fup=FALSE,
                          adjusted.Funbound.plasma=TRUE,
                          suppress.messages=FALSE,
                          minimum.Funbound.plasma=0.0001)
{
#R CMD CHECK throws notes about "no visible binding for global variable", for
#each time a data.table column name is used without quotes. To appease R CMD
#CHECK, a variable has to be created for each of these column names and set to
#NULL. Note that within the data.table, these variables will not be NULL! Yes,
#this is pointless and annoying.
  physiology.data <- physiology.data
  Species <- variable <- Tissue <- Parameter <- NULL
#End R CMD CHECK appeasement.  
  
# We need to describe the chemical to be simulated one way or another:
  if (is.null(chem.cas) & 
      is.null(chem.name) & 
      is.null(dtxsid) &
      is.null(parameters)) 
    stop('Parameters, chem.name, chem.cas, or dtxsid must be specified.')

# Look up the chemical name/CAS, depending on what was provided:
  if (is.null(parameters))
    if (any(is.null(chem.cas),is.null(chem.name),is.null(dtxsid)))
  {
    out <- get_chem_id(
            chem.cas=chem.cas,
            chem.name=chem.name,
            dtxsid=dtxsid)
    chem.cas <- out$chem.cas
    chem.name <- out$chem.name                                
    dtxsid <- out$dtxsid
  }

# Check the species argument for capitalization problems and whether or not it 
# is in the table:  
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

#    required.params <- model.table[["Schmitt"]]$parameterize_params
#    if (!(all(required.parasms%in%names(parameters)))) 
#      stop("Missing parameters",
#        paste(required.params[!(required.params%in%names(parameters))],
#          collapse=", "),
#        "in parameterize_schmitt")

  # Check to see if these parameters have been provided:
  if (!is.null(parameters))
  {
    if ("Funbound.plasma.dist" %in% names(parameters))
    {
      fup.db <- parameters$Funbound.plasma.dist
    } else {
      fup.db <- NA
    }
    if ("Funbound.plasma" %in% names(parameters))
    {
      fup <- parameters$Funbound.plasma
    } else {
      fup <- NA
    }
    if ("Pow" %in% names(parameters))
    {
      Pow <- parameters$Pow
    } else {
      Pow <- NA
    }
    if ("pKa_Donor" %in% names(parameters))
    {
      pKa_Donor <- parameters$pKa_Donor
    } else {
      pKa_Donor <- -999
    }
    if ("pKa_Accept" %in% names(parameters))
    {
      pKa_Accept <- parameters$pKa_Accept
    } else {
      pKa_Accept <- -999
    }
    if ("MA" %in% names(parameters))
    {
      MA <- parameters$MA
    } else {
      MA <- NA
    }
    if ("Fprotein.plasma" %in% names(parameters))
    {
      Fprotein <- parameters$Fprotein.plasma
    } else {
      Fprotein <- NA
    }
    if ("plasma.pH" %in% names(parameters))
    {
      plasma.pH <- parameters$plasma.pH
    } else {
      plasma.pH <- NA
    }
    if ("alpha" %in% names(parameters))
    {
      alpha <- parameters$alpha
    } else {
      alpha <- NA
    }
# If not, mark them incomplete and try to retrieve them.
# (Note that "NA" is an acceptable value for pKa's, so use -999)
  } else {
    fup.db <- NA
    fup <- NA
    Pow <- NA
    pKa_Donor <- -999
    pKa_Accept <- -999
    MA <- NA
    Fprotein <- NA
    plasma.pH <- NA
    alpha <- NA
  }
    
  if (is.na(fup.db))
  {
    # unitless fraction of chemical unbound with plasma
    fup.db <- try(
      get_invitroPK_param(
        "Funbound.plasma",
        species,
        chem.cas=chem.cas,
        chem.name=chem.name,
        dtxsid=dtxsid),
      silent=TRUE)
    if ((is(fup.db,"try-error") & default.to.human) || force.human.fup) 
    {
      fup.db <- try(
        get_invitroPK_param(
          "Funbound.plasma",
          "Human",
          chem.cas=chem.cas,
          chem.name=chem.name,
          dtxsid=dtxsid),
        silent=TRUE)
      if (!suppress.messages) 
        warning(paste(species,"coerced to Human for protein binding data."))
    }
    # If we couldn't retrieve fup.db and fup wasn't provided as a parameter:
    if (is(fup.db,"try-error") & is.na(fup)) 
      stop("Missing protein binding data for given species. Set default.to.human to true to substitute human value.")
    if (is(fup.db,"try-error")) fup.db <- fup
  }
  
  if (is.na(Pow))
  {
    Pow <- 10^get_physchem_param("logP",chem.cas=chem.cas)
  }   

  if (pKa_Donor == -999)
  {
    pKa_Donor <- suppressWarnings(get_physchem_param(
                                    "pKa_Donor",
                                    chem.cas=chem.cas,
                                    chem.name=chem.name,
                                    dtxsid=dtxsid))
  }
  
  if (pKa_Accept == -999)
  {    
    pKa_Accept <- suppressWarnings(get_physchem_param(
                                     "pKa_Accept",
                                     chem.cas=chem.cas,
                                     chem.name=chem.name,
                                     dtxsid=dtxsid))
  }

  if (is.na(MA))
  {
    if (any(!is.null(chem.cas),!is.null(chem.name),!is.null(dtxsid)))
      MA <- suppressWarnings(10^(get_physchem_param("logMA",
            chem.cas=chem.cas,
            chem.name=chem.name,
            dtxsid=dtxsid))) 
  }
  
  # If we don't have a measured value for membrane affintity, 
  # use Yun & Edgington (2013):
  if (is.na(MA))
  {
    if (!suppress.messages) warning(
      "Membrane affintity (MA) predicted with method of Yun and Edginton (2013)")  
    MA <- 
      10^(1.294 + 0.304 * log10(Pow))
  }   
  
  if (is.na(Fprotein))
  {
    Fprotein <- physiology.data[
                which(physiology.data[,'Parameter'] ==
                  'Plasma Protein Volume Fraction'),
                which(tolower(colnames(physiology.data)) == tolower(species))]
  }
  
  if (is.na(plasma.pH)) plasma.pH <- 7.4

  if (is.na(alpha)) alpha <- 0.001

# Check if fup is a point value or a distribution, if a distribution, use the median:
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
  
# If species-specific fup is 0 try the human value:  
  if (fup.point == 0 & tolower(species)!="human" & default.to.human) 
  {
    if (!suppress.messages) 
      warning(paste("Fraction unbound of zero for ",species,"replaced with human value."))
     fup.db <- try(
                get_invitroPK_param(
                  "Funbound.plasma",
                  "Human",
                  chem.cas=chem.cas,
                  chem.name=chem.name,
                  dtxsid=dtxsid),
                silent=TRUE)
  # Check if fup is a point value or a distribution, if a distribution, use the median:
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
  }

# We need a non-zero fup to make predictions:
  if (fup.point == 0 & !suppress.messages)
  {
    if (tolower(species)!="human" & !default.to.human) 
    {
      warning("Fraction unbound = 0, cannot predict tissue partitioning (try default.to.human=TRUE?).")
    } else warning("Fraction unbound = 0, cannot predict tissue partitioning.")
  }
  
# Calculate Pearce (2017) in vitro plasma binding correction:
  if (force.human.fup) 
    Flipid <- subset(
                physiology.data,
                Parameter == 'Plasma Effective Neutral Lipid Volume Fraction')[,
                  which(colnames(physiology.data) == 'Human')]
  else Flipid <- subset(
                   physiology.data,
                   Parameter=='Plasma Effective Neutral Lipid Volume Fraction')[,
                     which(tolower(colnames(physiology.data)) == tolower(species))]
  if (!is.null(parameters))
    if ("Flipid" %in% names(parameters))
      Flipid <- parameters$Flipid

  ion <- calc_ionization(
           pH=plasma.pH,
           pKa_Donor=pKa_Donor,
           pKa_Accept=pKa_Accept)
  dow <- Pow * (ion$fraction_neutral + 
                alpha * ion$fraction_charged + 
                ion$fraction_zwitter)
  fup.corrected <- max(
                     1 / ((dow) * Flipid + 1 / fup.point),
                     minimum.Funbound.plasma) # Enforcing a sanity check on 
                                               # plasma binding
  
  outlist <- list(Funbound.plasma=fup.corrected,
                  unadjusted.Funbound.plasma=fup.point,
                  Funbound.plasma.dist=fup.dist,
                  Funbound.plasma.adjustment=fup.corrected/fup.point,
                  Pow=Pow,
                  pKa_Donor=pKa_Donor,
                  pKa_Accept=pKa_Accept,
                  MA=MA,
                  Fprotein.plasma = Fprotein,
                  plasma.pH=plasma.pH,
                  alpha=alpha)
  
  return(lapply(outlist,set_httk_precision))                                
}
