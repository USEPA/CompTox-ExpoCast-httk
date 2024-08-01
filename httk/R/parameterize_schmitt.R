#' Parameters for Schmitt's (2008) Tissue Partition Coefficient Method
#' 
#' @description
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
#' @seealso \code{\link{calc_ma}}
#'
#' @seealso \code{\link{apply_fup_adjustment}}
#'
#' @references 
#'
#' \insertRef{pearce2017httk}{httk}
#'
#' \insertRef{schmitt2008general}{httk}
#'
#' \insertRef{schmitt2008corrigendum}{httk}
#'
#' \insertRef{pearce2017evaluation}{httk}
#' 
#' \insertRef{peyret2010unified}{httk}
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
  {
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
    # Make sure we have all the parameters we need:
    check_model(chem.cas=chem.cas, 
                chem.name=chem.name,
                dtxsid=dtxsid,
                model="schmitt",
                species=species,
                default.to.human=default.to.human
                )
  } else {
    # Work with local copy of parameters in function(scoping):
    if (is.data.table(parameters)) parameters <- copy(parameters) 
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
      fup.dist <- parameters$Funbound.plasma.dist
    } else {
      fup.dist <- NA
    }
    if ("Funbound.plasma" %in% names(parameters))
    {
      fup.point <- parameters$Funbound.plasma
    } else {
      fup.point <- NA
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
    fup.point <- NA
    fup.dist <-NA
    Pow <- NA
    pKa_Donor <- -999
    pKa_Accept <- -999
    MA <- NA
    Fprotein <- NA
    plasma.pH <- NA
    alpha <- NA
  }
    
  if (is.na(Pow))
  {
    Pow <- 10^get_physchem_param("logP",chem.cas=chem.cas)
  }   

  if (!is.na(pKa_Donor)) if (pKa_Donor == -999)
  {
    pKa_Donor <- suppressWarnings(get_physchem_param(
                                    "pKa_Donor",
                                    chem.cas=chem.cas,
                                    chem.name=chem.name,
                                    dtxsid=dtxsid))
  }
  
  if (!is.na(pKa_Accept)) if (pKa_Accept == -999)
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
    # If we don't have a measured value for membrane affintity, 
    # use Yun & Edgington (2013):
    if (is.na(MA))
    {
      MA <- calc_ma( chem.cas=chem.cas,
            chem.name=chem.name,
            dtxsid=dtxsid,
            suppress.messages=suppress.messages)
    }
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

  # Get the central tendency (point estimate) and potentially the distribution
  # quantiles for the fraction unbound in plasma (fup):
  if (is.na(fup.point))
  {
    fup.list <- get_fup(
        dtxsid=dtxsid,
        chem.name=chem.name,
        chem.cas=chem.cas,
        species=species,
        default.to.human=default.to.human,
        force.human.fup=force.human.fup,
        minimum.Funbound.plasma=minimum.Funbound.plasma,
        suppress.messages=suppress.messages) 
    fup.point <- fup.list$Funbound.plasma.point
    fup.dist <- fup.list$Funbound.plasma.dist 
  }
 
  if (fup.point > 0)
  {     
    # Apply the correction if requested:
    if (adjusted.Funbound.plasma)
    { 
      # Get the Pearce et al. (2017) lipid binding correction:       
      fup.adjustment <- calc_fup_correction(fup.point,
                                            parameters=parameters,
                                            dtxsid=dtxsid,
                                            chem.name=chem.name,
                                            chem.cas=chem.cas,
                                            default.to.human=default.to.human,
                                            force.human.fup=force.human.fup,
                                            suppress.messages=suppress.messages)
      fup.corrected <- apply_fup_adjustment(
                         fup.point,
                         fup.correction=fup.adjustment,
                         suppress.messages=suppress.messages,
                         minimum.Funbound.plasma=minimum.Funbound.plasma
                         )
    } else {
      fup.adjustment <- 1 
      fup.corrected <- fup.point
    } 
  } else {
    fup.adjustment <- NA
    fup.corrected <- NA
  } 
  
  if (is.na(fup.corrected))
  {
    if (species=="Human"){
      stop("fup is NA, Schmitt's method for tissue partition cannot be used.")
    }else{
      stop("fup is NA, Schmitt's method for tissue partition cannot be used. Try setting default.to.human to TRUE.")
    }
  }

  outlist <- list(Funbound.plasma=fup.corrected,
                  unadjusted.Funbound.plasma=fup.point,
                  Funbound.plasma.dist=fup.dist,
                  Funbound.plasma.adjustment=fup.adjustment,
                  Pow=Pow,
                  pKa_Donor=pKa_Donor,
                  pKa_Accept=pKa_Accept,
                  MA=MA,
                  Fprotein.plasma = Fprotein,
                  plasma.pH=plasma.pH,
                  alpha=alpha)
                  
  # Only include parameters specified in modelinfo:
  outlist <- outlist[model.list[["schmitt"]]$param.names]

  # alphabetize:
  outlist <- outlist[order(tolower(names(outlist)))]
  
  return(lapply(outlist,set_httk_precision))                                
}
