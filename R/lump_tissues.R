#' Lump tissue parameters
#' 
#' This function takes the parameters from predict_partitioning_schmitt and 
#' lumps the partition coefficients along with the volumes and flows based on 
#' the given tissue list. It is useful in Monte Carlo simulation of individual
#' partition coefficients when calculating the rest of body partition
#' coefficient.
#' 
#' This function returns the flows, volumes, and partition coefficients for the
#' lumped tissues specified in tissue list Ktissue2plasma -- tissue to free
#' plasma concentration partition coefficients for every tissue specified by 
#' Schmitt (2008) (the tissue.data table) tissuelist -- a list of character 
#' vectors, the name of each entry in the list is a lumped tissue, the words in
#' the vector are the Schmitt (2008) tissues that are to be lumped, for
#' example: tissuelist<-list(Rapid=c("Brain","Kidney")) species specifies the
#' flow.col and vol.col in the tissuedata.table
#' 
#' @param Ktissue2pu.in List of partition coefficients from
#' predict_partitioning_schmitt.
#' @param tissuelist Specifies compartment names and tissues groupings.
#' Remaining tissues in tissue.data are lumped in the rest of the body.
#' @param species Species desired (either "Rat", "Rabbit", "Dog", "Mouse", or
#' default "Human").
#' @return \item{Krbc2pu}{Ratio of concentration of chemical in red blood cells
#' to unbound concentration in plasma.} \item{Krest2pu}{Ratio of concentration
#' of chemical in rest of body tissue to unbound concentration in plasma.}
#' \item{Vrestc}{ Volume of the rest of the body per kg body weight, L/kg BW.}
#' \item{Vliverc}{ Volume of the liver per kg body weight, L/kg BW.}
#' \item{Qtotal.liverf}{Fraction of cardiac output flowing to the gut and
#' liver, i.e. out of the liver.} \item{Qgutf}{Fraction of cardiac output
#' flowing to the gut.} \item{Qkidneyf}{Fraction of cardiac output flowing to
#' the kidneys.}
#' @author John Wambaugh
#' @keywords Parameter
#' @examples
#' 
#' pcs <- predict_partitioning_schmitt(chem.name='bisphenola')
#' tissuelist <- list(liver=c("liver"),kidney=c("kidney"),lung=c("lung"),gut=c("gut")
#' ,muscle.bone=c('muscle','bone'))
#' lump_tissues(pcs,tissuelist=tissuelist)
#' 
#' @export lump_tissues
lump_tissues <- function(Ktissue2pu.in,
                            tissuelist=NULL,
                            species="Human")
{
  physiology.data <- physiology.data
  tissue.data <- tissue.data
  Tissue <- Species <- varable <- Parameter <- variable <- NULL

  if(length(Ktissue2pu.in) != length(unique(tissue.data[,'Tissue'])) | !all(unique(tissue.data[,'Tissue']) %in% c(substr(names(Ktissue2pu.in),2,nchar(names(Ktissue2pu.in))-3)
  [!substr(names(Ktissue2pu.in),2,nchar(names(Ktissue2pu.in))-3) %in% 'rbc'],'red blood cells'))) stop(paste('Ktissue2pu.in must contain the tissues from tissue.data:',paste(unique(tissue.data[,'Tissue']),collapse=', ')))
  if (!(species %in% colnames(physiology.data)))
  {
    if (toupper(species) %in% toupper(colnames(physiology.data)))
    {
      species <- colnames(physiology.data)[toupper(colnames(physiology.data))==toupper(species)]
    } else stop(paste("Tissue data for",species,"not found."))
  }

# Initialize the output lists:
	vol <- list()
	flow <- list()
	Ktissue2pu.out <- list()
 
# The vector all.tissues indicates whether each tissue in tissue.data has been lumped yet (TRUE/FALSE)
	all.tissues <- rep(FALSE,length(unique(tissue.data[,'Tissue'])))
	names(all.tissues) <- unique(tissue.data[,'Tissue'])
  #Renames pcs to match tissue names
  names(Ktissue2pu.in) <- substr(names(Ktissue2pu.in),2,nchar(names(Ktissue2pu.in))-3)
  names(Ktissue2pu.in)[names(Ktissue2pu.in) == 'rbc'] <- 'red blood cells'
# Blood cells only need a partioncoefficient:
  Ktissue2pu.out[["red blood cells"]] <- Ktissue2pu.in[["red blood cells"]]	
  all.tissues["red blood cells"] <- T
 
# This loop adds up the volumes and flows for the tissues within each lumped tissue
# as well as Red blood cells
	for (this.lumped.tissue in c(names(tissuelist),"cleanup"))
	{
# Anything that has not yet been lumped is added to the lumped tissue "Rest"
		if (this.lumped.tissue == "cleanup")
		{
			this.lumped.tissue <- "rest"
# First check to see if rest has been created and create it if it is missing:
			if (!("rest" %in% names(vol)))
			{
				vol[["rest"]] <- 0
				flow[["rest"]] <- 0
        Ktissue2pu.out[["rest"]] <- 0
			}
# Every tissue not already lumped gets added to "Rest"
			these.lumped.tissues <- unique(tissue.data[,'Tissue'])[!all.tissues]
		}	else{
			vol[[this.lumped.tissue]] <- 0
			flow[[this.lumped.tissue]] <- 0
			Ktissue2pu.out[[this.lumped.tissue]] <- 0
			these.lumped.tissues <- tissuelist[[this.lumped.tissue]]
		}
# Loop over every tissue that is lumped into the tissue:   
		for (this.tissue in these.lumped.tissues)
		{
			if (!(this.tissue %in% unique(tissue.data[,'Tissue'])))
				stop(paste(this.tissue,"not in list:",paste(unique(tissue.data[,'Tissue']),collapse=', ')))
			if (all.tissues[[this.tissue]] & this.tissue !="rest")
				stop(paste(this.tissue,"assigned to multiple lumped tissues"))

# Mark that this tissue has been lumped:
			all.tissues[[this.tissue]] <- TRUE
			this.subset <- subset(tissue.data,Tissue == this.tissue & tolower(Species) == tolower(species))
#Add the volume for this tissue to the lumped tissue:
			vol[[this.lumped.tissue]] <- vol[[this.lumped.tissue]] +  as.numeric(subset(this.subset,variable == 'Vol (L/kg)')[,'value']) 
#Add the flow for this tissue to the lumped tissue:                             
			flow[[this.lumped.tissue]] <- flow[[this.lumped.tissue]] + as.numeric(subset(this.subset,variable == 'Flow (mL/min/kg^(3/4))')[,'value']) 
			 
#Add a contribution to the partition coefficient weighted by the volume of this tissue:

			Ktissue2pu.out[[this.lumped.tissue]] <- Ktissue2pu.out[[this.lumped.tissue]] + as.numeric(subset(this.subset,variable == 'Vol (L/kg)')[,'value'])*Ktissue2pu.in[[this.tissue]]
		}
#Calculate the average parition coefficient by dividing by the total volume of
#the lumped tissue:
		Ktissue2pu.out[[this.lumped.tissue]] <- Ktissue2pu.out[[this.lumped.tissue]]/vol[[this.lumped.tissue]]
	}

  # Must have tissue-specific flows for these tissues (even if lumped) in order
  # to calculate other quantities (e.g. rate of metabolism, renal clearance):
  for (this.tissue in c("liver","gut","kidney"))
    if (is.null(flow[[this.tissue]]))  flow[[this.tissue]] <- as.numeric(subset(tissue.data,Tissue == this.tissue & tolower(Species) == tolower(species) &  variable == 'Flow (mL/min/kg^(3/4))')[,'value'])          
    
  # Must have tissue-specific volumes for these tissues (even if lumped) in order
  # to calculate other quantities (e.g. rate of metabolism):
    for (this.tissue in c("liver"))
     if (is.null(vol[[this.tissue]])) vol[[this.tissue]] <- as.numeric(subset(tissue.data,Tissue == this.tissue & tolower(Species) == tolower(species) &  variable == 'Vol (L/kg)')[,'value'])

    names(Ktissue2pu.out)[names(Ktissue2pu.out) == 'red blood cells'] <- 'rbc'
    names(Ktissue2pu.out) <- paste("K",names(Ktissue2pu.out),"2pu",sep='')
    names(vol) <- paste('V',names(vol),'c',sep='')
    names(flow)[names(flow) == 'liver'] <- 'total.liver'
    flow <- subset(unlist(flow), names(flow) != 'rest') / subset(physiology.data,Parameter=='Cardiac Output')[[species]]
    names(flow) <- paste('Q',names(flow),'f',sep='')
    
 	return(c(Ktissue2pu.out,vol,flow))
}
