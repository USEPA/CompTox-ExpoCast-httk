# This predicts the coefficient of tissue to FREE plasma fraction via Schmitt's method (2008):  
# fupl: unbound fraction in plasma
# Pow: octonol:water partition coefficient (not log transformed)
# pKa_Donor: compound H dissociation equilibirum constant(s)
# pKa_Accept: compound H association equilibrium constant(s)   
# MA: phospholipid:water distribution coefficient
# KAPPAcell2pu: Ratio of D inside the cell to D in the plasma, as derived from the different pHs and pKas
# Fprotein.plasma: protein fraction in plasma - from Gardner 1980


#' Predict partition coefficients using the method from Schmitt (2008).
#' 
#' This function implements the method from Schmitt (2008) in predicting the 
#' tissue to unbound plasma partition coefficients for the tissues contained 
#' in the tissue.data table.
#' 
#' A separate regression is used when adjusted.Funbound.plasma is FALSE.
#' 
#' A regression is used for membrane affinity when not provided.  The
#' regressions for correcting each tissue are performed on tissue plasma
#' partition coefficients (Ktissue2pu * Funbound.plasma) calculated with the
#' corrected Funbound.plasma value and divided by this value to get Ktissue2pu.
#' Thus the regressions should be used with the corrected Funbound.plasma.
#' 
#' The red blood cell regression can be used but is not by default because of
#' the span of the data used, reducing confidence in the regression for higher
#' and lower predicted values.
#' 
#' Human tissue volumes are used for species other than Rat.
#' 
#' @param chem.name Either the chemical name or the CAS number must be
#' specified. 
#' @param chem.cas Either the chemical name or the CAS number must be
#' specified. 
#' @param species Species desired (either "Rat", "Rabbit", "Dog", "Mouse", or
#' default "Human").
#' @param default.to.human Substitutes missing animal values with human values
#' if true (hepatic intrinsic clearance or fraction of unbound plasma).
#' @param parameters Chemical parameters from the parameterize_schmitt
#' function, overrides chem.name and chem.cas.
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
#' @return Returns tissue to unbound plasma partition coefficients for each
#' tissue.
#' @author Robert Pearce
#' @keywords Parameter
#' @examples
#' 
#' predict_partitioning_schmitt(chem.name='ibuprofen',regression=FALSE)
#' 
#' @import magrittr
#' @export predict_partitioning_schmitt
predict_partitioning_schmitt <- function(chem.name=NULL,
                                         chem.cas=NULL,
                                         species='Human',
                                         default.to.human=F,
                                         parameters=NULL,
                                         alpha=0.001,
                                         adjusted.Funbound.plasma=T,
                                         regression=T,
                                         regression.list=c('brain',
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
                                         minimum.Funbound.plasma=0.0001) 
{
  #R CMD CHECK throws notes about "no visible binding for global variable", for
  #each time a data.table column name is used without quotes. To appease R CMD
  #CHECK, a variable has to be created for each of these column names and set to
  #NULL. Note that within the data.table, these variables will not be NULL! Yes,
  #this is pointless and annoying.
  Tissue <- Species <- variable <- Reference <- value <- physiology.data <- NULL
  #End R CMD CHECK appeasement.
  
  
  if (is.null(parameters))
  {
    parameters <- parameterize_schmitt(chem.name=chem.name,
                    chem.cas=chem.cas,
                    species=species,
                    default.to.human=default.to.human,
                    minimum.Funbound.plasma=minimum.Funbound.plasma)
    user.params <- F
  } else {
    user.params <- T
    if (!"plasma.pH"%in%names(parameters)) parameters$plasma.pH <- 7.4
    if (!"Fprotein.plasma"%in%names(parameters)) 
      parameters$Fprotein.plasma <-  httk::physiology.data[
      which(httk::physiology.data[,'Parameter'] == 'Plasma Protein Volume Fraction'),
      which(tolower(colnames(httk::physiology.data)) == tolower(species))]
  }
  
  if(!adjusted.Funbound.plasma & user.params == FALSE) parameters$Funbound.plasma <- parameters$unadjusted.Funbound.plasma
   
  if(! tolower(species) %in% c('rat','human')){
    species <- 'Human'
    warning('Human fractional tissue volumes used in calculating partition coefficients.')
  }
  if (!("alpha" %in% names(parameters))) parameters$alpha <- alpha
# For the "rest" tissue containing those tissues in "Physiological Parameter
# Values for PBPK Models" (2004) that are not described by Schmitt (2008)

# we use the average values for the Schmitt (2008) tissues
  mycomps <- c('Fcell','Fint','FWc','FLc','FPc','Fn_Lc','Fn_PLc','Fa_PLc','pH')
  tissue.data <- as.data.table(tissue.data)[Tissue != 'red blood cells' & 
                                              variable %in% mycomps & 
                                              tolower(Species) == tolower(species),
                                            .(value = mean(value, na.rm = T),
                                              Tissue = 'rest',
                                              Reference = NA_character_), 
                                            .(variable, Species)] %>% 
    rbind(.,tissue.data) %>% 
    as.data.frame()


	Ktissue2pu <- list()
	
	# water fraction in plasma:
	FWpl <- 1 - parameters$Fprotein.plasma
	# protein fraction in interstitium:
  FPint <- 0.37 * parameters$Fprotein.plasma
	# water fraction in interstitium:
  FWint <- FWpl
  if (regression)
  {
   #  regression coefficients (intercept and slope) add to table 
    if (adjusted.Funbound.plasma)
    {
      reg <- matrix(c(-0.167,0.543, # brain
                      -0.325,0.574, # adipose
                      -0.006,0.267, # red blood cells
                      0.143, 0.764, # gut
                      0.116, 0.683, # heart
                      0.452, 0.673, # kidney
                      0.475, 0.621, # liver
                      0.087, 0.866, # lung
                      -0.022, 0.658, # muscle
                      -0.09, 0.566, # skin
                      0.034, 0.765, # spleen
                      0.036, 0.781), # bone
                      12,2,byrow=T)
    } else {
      reg <- matrix(c(-0.117,0.377, # brain
                      -0.324,0.544, # adipose
                      -0.022,0.196, # red blood cells
                      0.14, 0.735, # gut
                      0.12, 0.534, # heart 
                      0.443, 0.631, # kidney 
                      0.487, 0.513, # liver 
                      0.113, 0.75, # lung
                      -0.025, 0.537, # muscle 
                      -0.086, 0.498, # skin
                      0.011, 0.675, # spleen
                      0.025, 0.758), # bone
                      12,2,byrow=T)

    }
    rownames(reg) <- c('brain','adipose','red blood cells','gut','heart',
                       'kidney','liver','lung','muscle','skin','spleen','bone')
    colnames(reg) <- c('intercept','slope')      
  }
  if(is.null(tissues)) tissues <- unique(tissue.data[,'Tissue'])
	for (this.tissue in tissues)
	{
		this.subset <- subset(tissue.data,Tissue == this.tissue & tolower(Species) == tolower(species))
# Tissue-specific cellular/interstial volume fractions:
    Ftotal <- as.numeric(subset(this.subset,variable=='Fcell')[,'value']) + as.numeric(subset(this.subset,variable=='Fint')[,'value'])
    # Normalized Cellular fraction of total volume:
		Fcell <- as.numeric(subset(this.subset,variable=='Fcell')[,'value']) / Ftotal
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
		Fn_L <-  as.numeric(subset(this.subset,variable=='FLc')[,'value']) * as.numeric(subset(this.subset,variable=='Fn_Lc')[,'value'])
		if (is.na(Fn_L)) Fn_L <- 0
		# neutral phospholipid volume fraction:
		Fn_PL <- as.numeric(subset(this.subset,variable=='FLc')[,'value']) * as.numeric(subset(this.subset,variable=='Fn_PLc')[,'value'])
		if (is.na(Fn_PL)) Fn_PL <- 0
		# acidic phospholipid volume fraction:
		Fa_PL <-  as.numeric(subset(this.subset,variable=='FLc')[,'value']) * as.numeric(subset(this.subset,variable=='Fa_PLc')[,'value'])
		if (is.na(Fa_PL)) Fa_PL <- 0
		
		# tissue pH
		pH <- as.numeric(subset(this.subset,variable=='pH')[,'value'])

		# neutral phospholipid:water parition coefficient:
		if (is.null(parameters$MA))
		{     
			Kn_PL <- 10^(1.294 + 0.304 * log10(parameters$Pow))
		}else if(is.na(parameters$MA)){
 	    Kn_PL <- 10^(1.294 + 0.304 * log10(parameters$Pow))
    }else{
			Kn_PL <- parameters$MA
		}

    # Need to calculate the amount of un-ionized parent:
    ionization <- calc_ionization(pH=pH,parameters=parameters)
    fraction_neutral  <- ionization[["fraction_neutral"]]
    fraction_charged <- ionization[["fraction_charged"]]
    fraction_negative <- ionization[["fraction_negative"]]
    fraction_positive <- ionization[["fraction_positive"]]
    fraction_zwitter <- ionization[["fraction_zwitter"]]

		# neutral lipid:water partition coefficient
		Kn_L <- parameters$Pow * (fraction_neutral + fraction_zwitter + parameters$alpha * fraction_charged)

		# protein:water partition coefficient:
		KP <- 0.163 + 0.0221*Kn_PL
		
		# acidic phospholipid:water partition coefficient:
		Ka_PL <- Kn_PL * (fraction_neutral + fraction_zwitter + 20*fraction_positive + 0.05*fraction_negative)

  	Kint <- (FWint +   FPint/parameters$Fprotein.plasma*(1/parameters$Funbound.plasma - FWpl))
		
		Kcell <- (FW  + Kn_L * Fn_L + Kn_PL * Fn_PL + Ka_PL * Fa_PL + KP * FP) 
		
    plasma <- calc_ionization(pH=parameters$plasma.pH,parameters=parameters)
    fraction_neutral_plasma <- plasma[['fraction_neutral']]
    fraction_zwitter_plasma <- plasma[['fraction_zwitter']]    
    fraction_charged_plasma <- plasma[['fraction_charged']] 
    KAPPAcell2pu <- (fraction_neutral_plasma + fraction_zwitter_plasma + parameters$alpha * fraction_charged_plasma)/(fraction_neutral + fraction_zwitter + parameters$alpha * fraction_charged)
    
    if(this.tissue == 'red blood cells') eval(parse(text=paste("Ktissue2pu[[\"Krbc2pu\"]] <- Fint * Kint + KAPPAcell2pu*Fcell * Kcell" ,sep='')))
 	  else eval(parse(text=paste("Ktissue2pu[[\"K",this.tissue,"2pu\"]] <- Fint * Kint + KAPPAcell2pu*Fcell * Kcell",sep='')))
   
    if(regression & this.tissue %in% regression.list){
      #if(parameters$Pow > 4){
        if(this.tissue == 'red blood cells') eval(parse(text=paste("Ktissue2pu[[\"Krbc2pu\"]] <- 10^(reg[[this.tissue,\'intercept\']] + reg[[this.tissue,\'slope\']] * log10(Ktissue2pu[[\"Krbc2pu\"]] * parameters$Funbound.plasma)) / parameters$Funbound.plasma",sep='')))
        else if(this.tissue != 'rest') eval(parse(text=paste("Ktissue2pu[[\"K",this.tissue,"2pu\"]] <- 10^(reg[this.tissue,\'intercept\'] + reg[this.tissue,\'slope\'] * log10(Ktissue2pu[[\"K",this.tissue,"2pu\"]] * parameters$Funbound.plasma)) / parameters$Funbound.plasma",sep='')))
    }
	}
  if(regression & all(unique(tissue.data[,'Tissue']) %in% tissues)) Ktissue2pu[['Krest2pu']] <- mean(unlist(Ktissue2pu[!names(Ktissue2pu) %in% c('Krbc2pu','Krest2pu')])) 
 # if(user.params) warning(paste(species,' fractional tissue volumes used in calculation.  Parameters should match species argument used (',species,').',sep="")) 
 	return(Ktissue2pu)
}
