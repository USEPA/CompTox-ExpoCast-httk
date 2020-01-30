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
#' @param parameters A list of physiological parameters including flows and
#' volumes for tissues in \code{tissuelist}
#' @param tissuelist Specifies compartment names and tissues groupings.
#' Remaining tissues in tissue.data are lumped in the rest of the body.
#' @param species Species desired (either "Rat", "Rabbit", "Dog", "Mouse", or
#' default "Human").
#' @param tissue.vols A list of volumes for tissues in \code{tissuelist}
#' @param tissue.flows A list of flows for tissues in \code{tissuelist}
#'
#' @return \item{Krbc2pu}{Ratio of concentration of chemical in red blood cells
#' to unbound concentration in plasma.} \item{Krest2pu}{Ratio of concentration
#' of chemical in rest of body tissue to unbound concentration in plasma.}
#' \item{Vrestc}{ Volume of the rest of the body per kg body weight, L/kg BW.}
#' \item{Vliverc}{ Volume of the liver per kg body weight, L/kg BW.}
#' \item{Qtotal.liverf}{Fraction of cardiac output flowing to the gut and
#' liver, i.e. out of the liver.} \item{Qgutf}{Fraction of cardiac output
#' flowing to the gut.} \item{Qkidneyf}{Fraction of cardiac output flowing to
#' the kidneys.}
#' 
#' @author John Wambaugh and Robert Pearce
#' 
#' @references Pearce, Robert G., et al. "Evaluation and calibration of 
#' high-throughput predictions of chemical distribution to tissues." Journal of
#' pharmacokinetics and pharmacodynamics 44.6 (2017): 549-565.
#'
#' @keywords Parameter
#' 
#' @examples
#' 
#' pcs <- predict_partitioning_schmitt(chem.name='bisphenola')
#' tissuelist <- list(liver=c("liver"),kidney=c("kidney"),lung=c("lung"),gut=c("gut")
#' ,muscle.bone=c('muscle','bone'))
#' lump_tissues(pcs,tissuelist=tissuelist)
#' 
#' @export lump_tissues
lump_tissues <- function(Ktissue2pu.in,
                         parameters=NULL,
                         tissuelist=NULL,
                         species="Human",
                         tissue.vols=NULL,
                         tissue.flows=NULL)
{
#R CMD CHECK throws notes about "no visible binding for global variable", for
#each time a data.table column name is used without quotes. To appease R CMD
#CHECK, a variable has to be created for each of these column names and set to
#NULL. Note that within the data.table, these variables will not be NULL! Yes,
#this is pointless and annoying.
  Tissue <- Species <- varable <- Parameter <- variable <- NULL
#End R CMD CHECK appeasement.

  if (length(Ktissue2pu.in) != length(unique(tissue.data[,'Tissue'])) | 
      !all(unique(tissue.data[,'Tissue']) %in% 
        c(substr(names(Ktissue2pu.in),2,nchar(names(Ktissue2pu.in))-3)
        [!substr(names(Ktissue2pu.in),2,nchar(names(Ktissue2pu.in))-3) %in% 
          'rbc'],'red blood cells'))) 
    stop(paste('Ktissue2pu.in must contain the tissues from tissue.data:',
      paste(unique(tissue.data[,'Tissue']),collapse=', ')))
      
  if (!(species %in% colnames(physiology.data)))
  {
    if (toupper(species) %in% toupper(colnames(physiology.data)))
    {
      species <- colnames(physiology.data)[
                   toupper(colnames(physiology.data))==toupper(species)]
    } else stop(paste("Tissue data for",species,"not found."))
  }

# Initialize the output lists:
	vol <- list()
	flow <- list()
	Ktissue2pu.out <- list()

# List all tissues for which HTTK has human tissue information. 
# This will be used in lumping.  
  tissuenames <- sort(unique(subset(
                               httk::tissue.data,
                               tolower(Species)==tolower(species))$Tissue))
 
# The vector all.tissues indicates whether each tissue in tissue.data has been 
# lumped yet (TRUE/FALSE)	
  all.tissues <- rep(FALSE,length(unique(tissuenames)))
	names(all.tissues) <- unique(tissuenames)
  #Renames pcs to match tissue names
  names(Ktissue2pu.in) <- substr(
                            names(Ktissue2pu.in),
                            2,
                            nchar(names(Ktissue2pu.in))-3)
  names(Ktissue2pu.in)[names(Ktissue2pu.in) == 'rbc'] <- 'red blood cells'
# Blood cells only need a partioncoefficient:
  Ktissue2pu.out[["red blood cells"]] <- Ktissue2pu.in[["red blood cells"]]	
  all.tissues["red blood cells"] <- T
 
# This loop adds up the volumes and flows for the tissues within each lumped 
# tissue as well as Red blood cells
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
			these.lumped.tissues <- unique(tissue.data[, "Tissue"])[!all.tissues
        [unique(tissue.data[, "Tissue"])]]
		}	else{
			vol[[this.lumped.tissue]] <- 0
			flow[[this.lumped.tissue]] <- 0
			Ktissue2pu.out[[this.lumped.tissue]] <- 0
			these.lumped.tissues <- tissuelist[[this.lumped.tissue]]
		}
# Loop over every tissue that is lumped into the tissue:   
		for (this.tissue in these.lumped.tissues)
		{
      this.vol.param <- paste("V",this.tissue,"c",sep="")
      this.flow.param <- paste("Q",this.tissue,"f",sep="")
      if (!is.null(parameters) & !(this.flow.param %in% names(parameters)))
        stop(paste(
               "Parameters != NULL but", this.flow.param, "not in parameters."))
      if (!is.null(parameters) & !(this.vol.param %in% names(parameters)))
        stop(paste(
               "Parameters != NULL but", this.flow.param, "not in parameters."))
      else if (!is.null(parameters))
      {
        this.vol <- parameters[[this.vol.param]]
        this.flow <- parameters[[this.flow.param]]
      }   
			else if (!(this.tissue %in% unique(tissue.data[,'Tissue'])))
				stop(paste(
               this.tissue,
               "not in list:",
               paste(unique(tissue.data[,'Tissue']),collapse=', ')))
			else if (all.tissues[[this.tissue]] & this.tissue !="rest")
				stop(paste(this.tissue,"assigned to multiple lumped tissues"))
      else {
        if (is.null(tissue.vols))
        {
      		this.subset <- subset(
            tissue.data,
            Tissue == this.tissue & tolower(Species) == tolower(species))
          this.vol <- as.numeric(subset(
                                   this.subset,
                                   variable == 'Vol (L/kg)')[,'value'])
          this.flow <- as.numeric(subset(
                         this.subset,
                         variable == 'Flow (mL/min/kg^(3/4))')[,'value']) / 
            as.numeric(subset(physiology.data,Parameter=='Cardiac Output')[[species]])
  			} else {
          this.vol <- tissue.vols[this.lumped.tissue]
          this.flow <- tissue.flows[this.lumped.tissue] / 
            as.numeric(subset(physiology.data,Parameter=='Cardiac Output')[[species]])
        }
      }
# Mark that this tissue has been lumped:
			all.tissues[[this.tissue]] <- TRUE
# Add the volume for this tissue to the lumped tissue:
  		vol[[this.lumped.tissue]] <- vol[[this.lumped.tissue]] + this.vol  
# Add a contribution to the partition coefficient weighted by the volume of 
# this tissue:
		Ktissue2pu.out[[this.lumped.tissue]] <- 
      Ktissue2pu.out[[this.lumped.tissue]] + 
      this.vol*Ktissue2pu.in[[this.tissue]]
# Add the flow for this tissue to the lumped tissue:                             
  		flow[[this.lumped.tissue]] <- flow[[this.lumped.tissue]] + this.flow 
		}
#Calculate the average parition coefficient by dividing by the total volume of
#the lumped tissue:
		Ktissue2pu.out[[this.lumped.tissue]] <- 
      Ktissue2pu.out[[this.lumped.tissue]] / vol[[this.lumped.tissue]]
	}

  # Must have tissue-specific flows for these tissues (even if lumped) in order
  # to calculate other quantities (e.g. rate of metabolism, renal clearance):
  for (this.tissue in c("liver","gut","kidney"))
  {
    if (is.null(flow[[this.tissue]]))
    {
      this.flow.param <- paste("Q",this.tissue,"f",sep="")
      if (!is.null(parameters) & !(this.flow.param %in% names(parameters)))
        stop(paste(
               "Parameters != NULL but", this.flow.param, "not in parameters."))
        else if (!is.null(parameters))
        {
          this.flow <- parameters[[this.flow.param]]
        } else if (is.null(tissue.flows))
        {
          this.flow <- 
            as.numeric(subset(
              tissue.data,
              Tissue == this.tissue & 
                tolower(Species) == tolower(species) &  
                variable == 'Flow (mL/min/kg^(3/4))')[,'value']) / 
            subset(physiology.data,Parameter=='Cardiac Output')[[species]]
        } else this.flow <- tissue.flows[this.tissue] / 
            subset(physiology.data,Parameter=='Cardiac Output')[[species]]         
        flow[[this.tissue]] <- this.flow
      }
    }
    
  # Must have tissue-specific volumes for these tissues (even if lumped) in order
  # to calculate other quantities (e.g. rate of metabolism):
  for (this.tissue in c("liver"))
  {
    if (is.null(vol[[this.tissue]])) 
    {
      this.vol.param <- paste("V",this.tissue,"c",sep="")
      if (!is.null(parameters) & !(this.vol.param %in% names(parameters)))
        stop(paste(
              "Parameters != NULL but", this.flow.param, "not in parameters."))
      else if (!is.null(parameters))
      {
        this.vol <- parameters[[this.vol.param]]
      } else if (is.null(tissue.vols))
      {
        this.vol <- 
          as.numeric(subset(
                       tissue.data,
                       Tissue == this.tissue & 
                         tolower(Species) == tolower(species) &  
                         variable == 'Vol (L/kg)')[,'value'])
      } else this.vol <- tissue.vols[this.tissue]
      vol[[this.tissue]] <- this.vol
    }
  }

  names(Ktissue2pu.out)[names(Ktissue2pu.out) == 'red blood cells'] <- 'rbc'
  names(Ktissue2pu.out) <- paste("K",names(Ktissue2pu.out),"2pu",sep='')
  names(vol) <- paste('V',names(vol),'c',sep='')
  names(flow)[names(flow) == 'liver'] <- 'total.liver'
  names(flow) <- paste('Q',names(flow),'f',sep='')
    
 	return(c(Ktissue2pu.out,vol,flow))
}
