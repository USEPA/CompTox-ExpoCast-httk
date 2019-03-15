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
#' %% ~~ A concise (1-5 lines) description of what the function does. ~~ This
#' function implements the method from Schmitt (2008) in predicting the tissue
#' to unbound plasma partition coefficients from for the tissues contained in
#' the tissue.data table.
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
#' specified. %% ~~Describe \code{obs} here~~
#' @param chem.cas Either the chemical name or the CAS number must be
#' specified. %% ~~Describe \code{pred} here~~
#' @param species Species desired (either "Rat", "Rabbit", "Dog", "Mouse", or
#' default "Human").
#' @param default.to.human Substitutes missing animal values with human values
#' if true (hepatic intrinsic clearance or fraction of unbound plasma).
#' @param parameters Chemical parameters from the parameterize_schmitt
#' function, overrides chem.name and chem.cas.
#' @param adjusted.Funbound.plasma Whether or not to use Funbound.plasma
#' adjustment.
#' @param regression Whether or not to use the regressions.  Regressions are
#' used by default.
#' @param regression.list Tissues to use regressions on.
#' @param tissues Vector of desired partition coefficients.  Returns all by
#' default.
#' @return Returns tissue to unbound plasma partition coefficients for each
#' tissue.
#' @author Robert Pearce
#' @keywords Parameter
#' @examples
#' 
#' predict_partitioning_schmitt(chem.name='ibuprofen',regression=FALSE)
#' 
#' @export predict_partitioning_schmitt
predict_partitioning_schmitt <- function(chem.name=NULL,
                                         chem.cas=NULL,
                                         species='Human',
                                         default.to.human=F,
                                         parameters=NULL,
                                         adjusted.Funbound.plasma=T,
                                         regression=T,
                                         regression.list=c('brain','adipose','gut','heart','kidney','liver','lung','muscle','skin','spleen','bone'),
                                         tissues=NULL) #Schmitt 2008
{
  Tissue <- Species <- variable <- NULL
  
  if(is.null(parameters)){
    parameters <- parameterize_schmitt(chem.name=chem.name,chem.cas=chem.cas,species=species,default.to.human=default.to.human)
    user.params <- F
  } else {
    user.params <- T
    if (!"plasma.pH"%in%names(parameters)) parameters$plasma.pH <- parameterize_schmitt(chem.cas="80-05-7")$plasma.pH
  }
  if(!adjusted.Funbound.plasma) parameters$Funbound.plasma <- parameters$unadjusted.Funbound.plasma
   
  if(! tolower(species) %in% c('rat','human')){
    species <- 'Human'
    warning('Human fractional tissue volumes used in calculating partition coefficients.')
  }
  #parameters$alpha <- 0.001
# For the "rest" tissue containing those tissues in "Physiological Parameter
# Values for PBPK Models" (2004) that are not described by Schmitt (2008)

# we use the average values for the Schmitt (2008) tissues
for(this.comp in c('Fcell','Fint','FWc','FLc','FPc','Fn_Lc','Fn_PLc','Fa_PLc','pH')){
  this.row <- cbind('rest',species,NA,this.comp,mean(as.numeric(subset(tissue.data,Tissue != 'red blood cells' & tolower(Species) == tolower(species) & variable == this.comp)[,'value'])))
  colnames(this.row) <- colnames(tissue.data)
  tissue.data <- rbind(tissue.data,this.row)
}


	Ktissue2pu <- list()
	
	# water fraction in plasma:
	FWpl <- 1 - parameters$Fprotein.plasma
	# protein fraction in interstitium:
  FPint <- 0.37 * parameters$Fprotein.plasma
	# water fraction in interstitium:
  FWint <- FWpl
  if(regression){
   #  regression coefficients add to table 
    if(adjusted.Funbound.plasma){
      reg <- matrix(c(-0.167,0.543,-0.325,0.574,-0.006,0.267, 0.143, 0.764, 0.116, 0.683, 0.452, 0.673, 0.475, 0.621, 0.087, 0.866, -0.022, 0.658, -0.09, 0.566, 0.034, 0.765, 0.036, 0.781),12,2,byrow=T)
      rownames(reg) <- c('brain','adipose','red blood cells','gut','heart','kidney','liver','lung','muscle','skin','spleen','bone')
      colnames(reg) <- c('intercept','slope')
    }else{
      reg <- matrix(c(-0.117,0.377,-0.324,0.544,-0.022,0.196, 0.14, 0.735, 0.12, 0.534, 0.443, 0.631, 0.487, 0.513, 0.113, 0.75, -0.025, 0.537, -0.086, 0.498, 0.011, 0.675, 0.025, 0.758),12,2,byrow=T)
      rownames(reg) <- c('brain','adipose','red blood cells','gut','heart','kidney','liver','lung','muscle','skin','spleen','bone')
      colnames(reg) <- c('intercept','slope')  
    } 
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
