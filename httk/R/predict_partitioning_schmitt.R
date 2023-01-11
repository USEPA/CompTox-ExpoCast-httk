#' Predict partition coefficients using the method from Schmitt (2008).
#'                                                                                                                        #' This function implements the method from Schmitt (2008) for predicting the 
#' tissue to unbound plasma partition coefficients for the tissues contained 
#' in the \code{\link{tissue.data}} table. The method has been modifed
#' by Pearce et al. (2017) based on an evalaution using in vivo measured 
#' partition coefficients.
#' 
#' To understand this method, it is 
#' important to recognize that in  a given media the fraction unbound in that 
#' media is inverse of the media:water partition coefficient. 
#' In Schmitt's model, each tissue is composed of cells and
#' interstitium, with each cell consisting of neutral lipid,
#' neutral phospholipid, water, protein, and acidic phospholipid.
#' Each tissue cell is defined as the sum of separate
#' compartments for each constituent, all of which partition
#' with a shared water compartment. The partitioning between
#' the cell components and cell water is compound specific
#' and determined by log Pow (in neutral lipid partitioning),
#' membrane affinity (phospholipid and protein partitioning),
#' and pKa (neutral lipid and acidic phospholipid partitioning).
#' For a given compound the partitioning into each
#' component is identical across tissues. Thus the differences
#' among tissues are driven by their composition, that is, the
#' varying volumes of components such as neutral lipid.
#' However, pH differences across tissues also determine
#' small differences in partitioning between cell and plasma
#' water. The fup is used as the plasma water to total plasma
#' partition coefficient and to approximate the partitioning
#' between interstitial protein and water.
#' 
#' A regression is used to predict membrane affinity when measured values are 
#' not available (\code{\link{calc_ma}}).  The
#' regressions for correcting each tissue are performed on tissue plasma
#' partition coefficients (Ktissue2pu * Funbound.plasma) calculated with the
#' corrected Funbound.plasma value and divided by this value to get Ktissue2pu.
#' Thus the regressions should be used with the corrected Funbound.plasma.
#' 
#' A separate regression is used when adjusted.Funbound.plasma is FALSE.
#' 
#' The red blood cell regression can be used but is not by default because of
#' the span of the data used for evaluation, reducing confidence in the 
#' regression for higher and lower predicted values.
#' 
#' Human tissue volumes are used for species other than Rat.
#' 
#' @param chem.name Either the chemical name or the CAS number must be
#' specified. 
#' @param chem.cas Either the chemical name or the CAS number must be
#' specified. 
#' @param dtxsid EPA's DSSTox Structure ID (\url{https://comptox.epa.gov/dashboard})  
#' the chemical must be identified by either CAS, name, or DTXSIDs
#' @param species Species desired (either "Rat", "Rabbit", "Dog", "Mouse", or
#' default "Human").
#' @param default.to.human Substitutes missing animal values with human values
#' if true (hepatic intrinsic clearance or fraction of unbound plasma).
#' @param parameters Chemical parameters from \code{\link{parameterize_schmitt}}
#' overrides chem.name, dtxsid, and chem.cas.
#' @param alpha Ratio of Distribution coefficient D of totally charged species
#' and that of the neutral form
#' @param adjusted.Funbound.plasma Whether or not to use Funbound.plasma
#' adjustment.
#' @param regression Whether or not to use the regressions.  Regressions are
#' used by default.
#' @param regression.list Tissues to use regressions on.
#' @param tissues Vector of desired partition coefficients.  Returns all by
#' default.
#' @param minimum.Funbound.plasma Monte Carlo draws less than this value are set 
#' equal to this value (default is 0.0001 -- half the lowest measured Fup in our
#' dataset).
#' @param suppress.messages Whether or not the output message is suppressed.
#' @param model Model for which partition coefficients are neeeded (for example,
#' "pbtk", "3compartment")
#'
#' @seealso \code{\link{parameterize_schmitt}}
#'
#' @seealso \code{\link{tissue.data}}
#' 
#' @return Returns tissue to unbound plasma partition coefficients for each
#' tissue.
#'
#' @author Robert Pearce
#'
#' @keywords Parameter
#'
#' @references
#' Schmitt, Walter. "General approach for the calculation of tissue to plasma 
#' partition coefficients." Toxicology in Vitro 22.2 (2008): 457-467.
#'
#' Birnbaum, L., et al. "Physiological parameter values for PBPK models." 
#' International Life Sciences Institute, Risk Science Institute, Washington, 
#' DC (1994).
#' 
#' Pearce, Robert G., et al. "Evaluation and calibration of high-throughput 
#' predictions of chemical distribution to tissues." Journal of pharmacokinetics 
#' and pharmacodynamics 44.6 (2017): 549-565.
#'
#' Yun, Y. E., and A. N. Edginton. "Correlation-based prediction of 
#' tissue-to-plasma partition coefficients using readily available input 
#' parameters." Xenobiotica 43.10 (2013): 839-852.
#'
#' @examples
#' 
#' predict_partitioning_schmitt(chem.name='ibuprofen',regression=FALSE)
#'
#' @import magrittr
#'
#' @export predict_partitioning_schmitt
predict_partitioning_schmitt <- function(
  chem.name=NULL,
  chem.cas=NULL,
  dtxsid=NULL,
  species='Human',
  model="pbtk",
  default.to.human=FALSE,
  parameters=NULL,
  alpha=0.001,
  adjusted.Funbound.plasma=TRUE,
  regression=TRUE,
  regression.list=c(
    'brain',
    'adipose',
    'gut',
    'heart',
    'kidney',
    'liver',
    'lung',
    'muscle',
    'skin',
    'spleen',
    'bone'),
  tissues=NULL,
  minimum.Funbound.plasma=0.0001,
  suppress.messages=FALSE) 
{
  #R CMD CHECK throws notes about "no visible binding for global variable", for
  #each time a data.table column name is used without quotes. To appease R CMD
  #CHECK, a variable has to be created for each of these column names and set to
  #NULL. Note that within the data.table, these variables will not be NULL! Yes,
  #this is pointless and annoying.
  Tissue <- Species <- variable <- Reference <- value <- 
    pearce2017regression <-physiology.data <- NULL
  #End R CMD CHECK appeasement.
  
  if (is.null(model)) stop("Model must be specified.")
  model <- tolower(model)
  if (!(model %in% names(model.list)))            
  {
    stop(paste("Model",model,"not available. Please select from:",
      paste(names(model.list),collapse=", ")))
  } else {
  # We need to know which tissues to include in the model 
  #(for example, placenta, gills)
    if(is.null(tissues)) tissues <- model.list[[model]]$alltissues
  }
  
  if (is.null(parameters))
  {
    parameters <- parameterize_schmitt(
      chem.name=chem.name,
      chem.cas=chem.cas,
      dtxsid=dtxsid,
      species=species,
      default.to.human=default.to.human,
      minimum.Funbound.plasma=minimum.Funbound.plasma,
      suppress.messages=suppress.messages)
    user.params <- F
  } else {
    user.params <- T
    if (!"plasma.pH"%in%names(parameters)) parameters$plasma.pH <- 7.4
    if (!"Fprotein.plasma"%in%names(parameters)) 
      parameters$Fprotein.plasma <-  physiology.data[
      which(physiology.data[,'Parameter'] == 
        'Plasma Protein Volume Fraction'),
      which(tolower(colnames(physiology.data)) == tolower(species))]
  }
  
  if (!adjusted.Funbound.plasma & user.params == FALSE) 
    parameters$Funbound.plasma <- parameters$unadjusted.Funbound.plasma

  if (any(parameters$Funbound.plasma == 0))
    if (tolower(species) == "human" | default.to.human) {
      stop("Fraction unbound below limit of detection, cannot predict partitioning.")
    } else {
      stop("\
Fraction unbound for species below limit of detection, cannot predict partitioning. Perhaps try default.to.human=TRUE.")
    }
    
        
# If we don't have a measured value, use Yun & Edgington (2013):
  if (any(is.na(parameters$MA)))
  {
     if (!suppress.messages) warning(
"Membrane affintity (MA) predicted with method of Yun and Edginton (2013)")  
    parameters$MA[is.na(parameters$MA)] <- 
      10^(1.294 + 0.304 * log10(parameters$Pow))
  }   
  
  if(! tolower(species) %in% c('rat','human')){
    species <- 'Human'
    if (!suppress.messages) warning(
'Human fractional tissue volumes used in calculating partition coefficients.')
  }
  if (!("alpha" %in% names(parameters))) parameters$alpha <- alpha

# Create tissue composition descriptors (following Schmitt 2008) for the 
# lumped compartment "rest" containing those tissues in "Physiological Parameter
# Values for PBPK Models" (1994) that are not described by tissue.data by using
# the mean value for the tissues that are described:
  for (this.comp in c(
    'Fcell',  # Cells Fraction of Total Volume
    'Fint',   # Interstitiom Fraction of Total Volume
    'FWc',    # Water Fraction of Cell Volume
    'FLc',    # Lipid Fraction of Cell Volume
    'FPc',    # Protein Fraction of Cell Volume
    'Fn_Lc',  # Neutral Lipid Fraction of Total Lipid
    'Fn_PLc', # Neutral Phospholipid Fraction of Total Lipid
    'Fa_PLc', # Acidic Phospholipid Fraction of Total Lipid
    'pH'))    # Tissue pH
  {
    this.row <- cbind('rest',species,NA,this.comp,
      mean(as.numeric(subset(tissue.data,
        Tissue != 'red blood cells' & 
        tolower(Species) == tolower(species) & 
        variable == this.comp)[,'value'])))
    colnames(this.row) <- colnames(tissue.data)
    tissue.data <- rbind(tissue.data,this.row) %>%
    as.data.frame()
  }

	Ktissue2pu <- list()
	
	# water fraction in plasma:
	FWpl <- 1 - parameters$Fprotein.plasma
	# protein fraction in interstitium:
  FPint <- 0.37 * parameters$Fprotein.plasma
	# water fraction in interstitium:
  FWint <- FWpl
  
# These are the calibrations from Pearce et al. (2017):
  if (regression)
  {
   #  regression coefficients (intercept and slope) add to table 
   # These parameters should be in a table in the /data director!
    # Fup Parameter estimates are now added to `pearce2017regression`.
    if (adjusted.Funbound.plasma)
    {
      reg <- httk::pearce2017regression[,
        grep(colnames(httk::pearce2017regression),
        pattern = "adj")]
    } else {
      reg <- httk::pearce2017regression[,
        -grep(colnames(httk::pearce2017regression),
        pattern = "adj")]
    }
    colnames(reg) <- c('intercept','slope')      
  }

	for (this.tissue in tissues)
	{
# Load Schmitt (2008) descriptors for this tissue:
		this.subset <- subset(tissue.data,
      Tissue == this.tissue & 
      tolower(Species) == tolower(species) &
      variable %in% c(
         "Fcell",
         "Fint",
         "FWc",
         "FPc",
         "FLc",
         "Fn_Lc",
         "Fn_PLc",
         "Fa_PLc",
         "pH"))
 # If there are no descriptors in the tissue.data table for this species, try
 # the human ones: 
    if (dim(this.subset)[1]==0) 
    {
      this.subset <- subset(tissue.data,
        Tissue == this.tissue & 
        tolower(Species) == "human" &
       variable %in% c(
         "Fcell",
         "Fint",
         "FWc",
         "FPc",
         "FLc",
         "Fn_Lc",
         "Fn_PLc",
         "Fa_PLc",
         "pH"))
      if (dim(this.subset)[1]>0)
      {
        if (!suppress.messages) warning(paste(
          "Human tissue composition values for",
          this.tissue,
          "used in calculating partition coefficients."))
      } else stop(paste("Missing data in tissue.data on",this.tissue))
    }

# Tissue-specific cellular/interstitial volume fractions:
    Ftotal <- as.numeric(subset(this.subset,variable=='Fcell')[,'value']) + 
      as.numeric(subset(this.subset,variable=='Fint')[,'value'])
    # Normalized Cellular fraction of total volume:
		Fcell <- as.numeric(subset(this.subset,variable=='Fcell')[,'value']) / 
      Ftotal
		# Normalized interstitial fraction of total volume:
		Fint <- as.numeric(subset(this.subset,variable=='Fint')[,'value']) / Ftotal
		if (is.na(Fint)) Fint <- 0
		
# Tissue-specific cellular sub-fractions:
		# water volume fraction:
		FW <- as.numeric(subset(this.subset,variable=='FWc')[,'value'])
		# protein volume fraction:
		FP <-  as.numeric(subset(this.subset,variable=='FPc')[,'value'])

# Tissue-specific cellular lipid sub-sub-fractions:        
		# neutral lipid volume fraction:
		Fn_L <-  as.numeric(subset(this.subset,variable=='FLc')[,'value']) * 
      as.numeric(subset(this.subset,variable=='Fn_Lc')[,'value'])
		if (is.na(Fn_L)) Fn_L <- 0
		# neutral phospholipid volume fraction:
		Fn_PL <- as.numeric(subset(this.subset,variable=='FLc')[,'value']) * 
      as.numeric(subset(this.subset,variable=='Fn_PLc')[,'value'])
		if (is.na(Fn_PL)) Fn_PL <- 0
		# acidic phospholipid volume fraction:
		Fa_PL <-  as.numeric(subset(this.subset,variable=='FLc')[,'value']) * 
      as.numeric(subset(this.subset,variable=='Fa_PLc')[,'value'])
		if (is.na(Fa_PL)) Fa_PL <- 0
		
		# tissue-specific pH
		pH <- as.numeric(subset(this.subset,variable=='pH')[,'value'])

		# neutral phospholipid:water partition coefficient:
	  Kn_PL <- parameters$MA

# Schmitt Schmitt (2008) section 2.5: Partition coefficients for tissue components:
# First, calc_ionization handles the Hendersen-Hasselbalch relation for arbitrary pKa''s.
# We need to calculate the distribution of charged and uncharged molecules at
# the pH of the tissue:
    ionization <- calc_ionization(pH=pH,parameters=parameters)
    fraction_neutral  <- ionization[["fraction_neutral"]]
    fraction_charged <- ionization[["fraction_charged"]]
    fraction_negative <- ionization[["fraction_negative"]]
    fraction_positive <- ionization[["fraction_positive"]]
    fraction_zwitter <- ionization[["fraction_zwitter"]]

# Schmitt (2008) section 2.5.1: neutral lipid:water partition coefficient
# This is a generalized version of Schmitt (2008) equations 13 and 14:
		Kn_L <- calc_dow(Pow = parameters$Pow,
                     fraction_charged = fraction_charged,
                     alpha = parameters$alpha)

# Schmitt (2008) section 2.5.3: protein:water partition coefficient 
# Schmitt (2008) equation 19:
		KP <- 0.163 + 0.0221*Kn_PL
		
# Schmitt (2008) section 2.5.2: acidic phospholipid:water partition coefficient:
# This is a generalized version of Schmitt (2008) equations 17 and 18:
		Ka_PL <- Kn_PL * (fraction_neutral + fraction_zwitter + 
      20*fraction_positive + 0.05*fraction_negative)

# Schmitt (2008) section 2.2: Unbound fraction in interstitial space
# It is important to recognize that in a given media the fraction unbound in
# that media is one over the media:water partition coefficient.
# Schmitt (2008) Equation 4: 
  	Kint <- (FWint + FPint/parameters$Fprotein.plasma*
      (1/parameters$Funbound.plasma - FWpl))
		
# Schmitt (2008) section 2.3: Unbound fraction in cellular space
# It is important to recognize that in a given media the fraction unbound in
# that media is one over the media:water partition coefficient.
# Schmitt (2008) Equation 7: 
		Kcell <- (FW + Kn_L * Fn_L + Kn_PL * Fn_PL + Ka_PL * Fa_PL + KP * FP) 

# The following is our attempt to calculate the parameter kappa in general. 
# We run calc_ionization a second time for the plasma pH.
    plasma <- calc_ionization(pH=parameters$plasma.pH,parameters=parameters)
    fraction_neutral_plasma <- plasma[['fraction_neutral']]
    fraction_zwitter_plasma <- plasma[['fraction_zwitter']]    
    fraction_charged_plasma <- plasma[['fraction_charged']] 
# We then calculate the pH gradient infuence (Schmitt (2008) section 2.4) 
# as a sort  of generalized version of Schmitt (2008) equations 10 and 11:	
    KAPPAcell2pu <- (fraction_neutral_plasma + 
                     fraction_zwitter_plasma + 
                     parameters$alpha * fraction_charged_plasma) /
                     (fraction_neutral + 
                     fraction_zwitter + 
                     parameters$alpha * fraction_charged)
    
    
    if (this.tissue == 'red blood cells') 
    {
      eval(parse(text=paste(
        "Ktissue2pu[[\"Krbc2pu\"]] <- Fint * Kint + KAPPAcell2pu*Fcell * Kcell",
        sep=''))) 
 	  } else {
 	    eval(parse(text=paste(
        "Ktissue2pu[[\"K",
        this.tissue,
        "2pu\"]] <- Fint * Kint + KAPPAcell2pu*Fcell * Kcell",
        sep='')))
 	  }
   
    if (regression & this.tissue %in% regression.list)
    {
      if (this.tissue == 'red blood cells') eval(parse(text=paste(
        "Ktissue2pu[[\"Krbc2pu\"]] <- 10^(reg[[this.tissue,\'intercept\']] + reg[[this.tissue,\'slope\']] * log10(Ktissue2pu[[\"Krbc2pu\"]] * parameters$Funbound.plasma)) / parameters$Funbound.plasma",
        sep='')))
      else if (this.tissue != 'rest') eval(parse(text=paste(
        "Ktissue2pu[[\"K",this.tissue,"2pu\"]] <- 10^(reg[this.tissue,\'intercept\'] + reg[this.tissue,\'slope\'] * log10(Ktissue2pu[[\"K",
        this.tissue,
        "2pu\"]] * parameters$Funbound.plasma)) / parameters$Funbound.plasma",
        sep='')))
    }
	}
  if (regression & all(unique(tissue.data[,'Tissue']) %in% tissues)) 
    Ktissue2pu[['Krest2pu']] <- mean(unlist(Ktissue2pu[!names(Ktissue2pu) %in% 
      c('Krbc2pu','Krest2pu')])) 

 	return(lapply(Ktissue2pu,set_httk_precision))
}
