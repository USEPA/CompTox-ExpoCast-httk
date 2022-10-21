#' Lump tissue parameters
#'                                               
#' This function takes the parameters from 
#' \code{\link{predict_partitioning_schmitt}} and 
#' lumps the partition coefficients along with the volumes and flows based on 
#' the given tissue list. It is useful in Monte Carlo simulation of individual
#' partition coefficients when calculating the rest of body partition
#' coefficient.
#' 
#' This function returns the flows, volumes, and partition coefficients for the
#' lumped tissues specified in tissue list. Tissues are aggregated from table
#' \code{\link{tissue.data}}.
#' 
#' The name of each entry in the list is its own compartment.
#' The tissues in the alltissues vector are the tissues that are
#' to be considered in the lumping process. The tissuelist can also be manually
#' specified for alternate lumping schemes: for example,
#' tissuelist<-list(Rapid=c("Brain","Kidney")) specifies the flow.col and
#' vol.col in the tissuedata.table. 
#' 
#' The tissues contained in tissue.data that are unused in each of these models 
#' are aggregated into a single compartment termed
#' "rest", whose partition coefficient is calculated by averaging the remaining 
#' partition coefficients,
#' weighted by their species-specific tissue volumes.
#' 
#' @param Ktissue2pu.in List of partition coefficients from
#' predict_partitioning_schmitt.
#' @param parameters A list of physiological parameters including flows and
#' volumes for tissues in \code{tissuelist}
#' @param tissuelist Manually specifies compartment names and tissues, which
#' override the standard compartment names and tissues that are usually
#' specified in a model's associated modelinfo file. Remaining tissues in the
#' model's associated \code{alltissues} listing are lumped in the rest of the body.
#' @param species Species desired (either "Rat", "Rabbit", "Dog", "Mouse", or
#' default "Human").
#' @param tissue.vols A list of volumes for tissues in \code{tissuelist}
#' @param tissue.flows A list of flows for tissues in \code{tissuelist}
#' @param model Specify which model (and therefore which tissues) are being 
#' considered
#' @param suppress.messages Whether or not the output message is suppressed.
#'
#' @seealso \code{\link{predict_partitioning_schmitt}}
#' @seealso \code{\link{tissue.data}}
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
                         tissue.flows=NULL,
                         model="pbtk",
                         suppress.messages=FALSE)
{
#R CMD CHECK throws notes about "no visible binding for global variable", for
#each time a data.table column name is used without quotes. To appease R CMD
#CHECK, a variable has to be created for each of these column names and set to
#NULL. Note that within the data.table, these variables will not be NULL! Yes,
#this is pointless and annoying.
  Tissue <- Species <- varable <- Parameter <- variable <- NULL
#End R CMD CHECK appeasement.

  #run some basic checks for naming consistency and completeness on input: 
  if ((is.null(model)) & is.null(parameters))
    stop('The "model" variable must be specified if a complete set of
          "parameters" is not otherwise provided.')
  
  if (is.null(model)) stop("Model must be specified.")
  model <- tolower(model)
  if (!(model %in% names(model.list)))            
  {
    stop(paste("Model",model,"not available. Please select from:",
      paste(names(model.list),collapse=", ")))
  } else {
    #Before using tissuelist, make sure it is initialized with the tissuelist
    #entry from the modelinfo file of interest. If tissuelist is already manually
    #specified, it takes priority.
    if (is.null(tissuelist)){
      tissuelist <- model.list[[model]]$tissuelist
    }
    
    # List all tissues/compartments for which a model needs partitioning
    # information, regardless of whether the tissue/compartment is to be lumped 
    # or not.  
    tissuenames <- sort(unique(model.list[[model]]$alltissues))
  }

  if (!all(unlist(tissuelist) %in% tissuenames))
  {
    stop("Not all of the tissues/compartments specified in \"tissuelist\" 
are present in the entries of the associated modelinfo file's \"alltissues.\"")
  }
  
  #Check to make sure the tissuelist is a list of character vectors.
  if (!(is.null(tissuelist)))
  {
    if (!is.list(tissuelist)) stop("tissuelist must be a list of vectors, or 
NULL if the model is a 1 compartment model where no lumping is necessary.") 
  }
    
    #Because red blood cells are not involved in this lumping scheme, and
    #because they also undergo a name change between the associated partition
    #coefficient and the "red blood cells" name from tissue.data, they are
    #kept separate (assuming they are indicated in the tissuenames list). 
    if ("red blood cells" %in% tissuenames)
    {
      names(Ktissue2pu.in)[names(Ktissue2pu.in) == 'Krbc2pu'] <- 
        'red blood cells'
      pcs_names_standard_treatment <- 
        names(Ktissue2pu.in)[names(Ktissue2pu.in) != "red blood cells"]
      tissue_name_verification_vec <- c("red blood cells",
        substr(pcs_names_standard_treatment,2, 
               nchar(pcs_names_standard_treatment)-3))
    } else {
      tissue_name_verification_vec <- 
        substr(names(Ktissue2pu.in),2,nchar(names(Ktissue2pu.in))-3)
    }

    #Exclude "rest" from list of tissues called for among alltissues entries
#    tissue_name_verification_vec <- 
#      tissue_name_verification_vec[tissue_name_verification_vec != "rest"] 
    
    #Now use this verification vector to check if the requested tissuenames 
    #are among those for which partitioning info has been passed. 
    if (!all(tissuenames %in% tissue_name_verification_vec)){
      stop(paste("These names listed in the associated modelinfo file\'s
\"alltissues\" list must have correspondingly named entries in Ktissue2pu.in:",
                 paste(tissuenames, collapse=', ')))
    }
  
      
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

 
# The vector all.tissues indicates whether each tissue in tissue.data has been 
# lumped yet (TRUE/FALSE)	
  all.tissues <- rep(FALSE,length(tissuenames))
	names(all.tissues) <- tissuenames
	
  
# Blood cells only need a partition coefficient, so the input value is ready
# for output. Still have some special naming treatment for rbc's to go here:
  if ("red blood cells" %in% tissuenames){
    Ktissue2pu.out[["red blood cells"]] <- Ktissue2pu.in[["red blood cells"]]	
    all.tissues["red blood cells"] <- T
    names(Ktissue2pu.in)[names(Ktissue2pu.in) != "red blood cells"] <- 
      substr(names(Ktissue2pu.in)[names(Ktissue2pu.in) != "red blood cells"],
             2, nchar(names(Ktissue2pu.in)[names(Ktissue2pu.in) !=
                                             "red blood cells"])-3)
  } else{
    #Renames pcs to match tissue names
    names(Ktissue2pu.in) <- substr(names(Ktissue2pu.in),2,
                                   nchar(names(Ktissue2pu.in))-3)
  }

  
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
			these.lumped.tissues <- these.lumped.tissues[!is.na(these.lumped.tissues)] 
			#need to trim away NA values that could result here from the all.tissues
			#logical vector operations^^^
		}	else{
			vol[[this.lumped.tissue]] <- 0
			flow[[this.lumped.tissue]] <- 0
			Ktissue2pu.out[[this.lumped.tissue]] <- 0
			these.lumped.tissues <- tissuelist[[this.lumped.tissue]]
		}
# Loop over every tissue that is lumped into the tissue, drawing tissue volume
	  #and flow information from wherever it is available:   
		for (this.tissue in these.lumped.tissues)
		{
      this.vol.param <- paste("V",this.tissue,"c",sep="")
      this.flow.param <- paste("Q",this.tissue,"f",sep="")
      
      if (all.tissues[[this.tissue]] & this.tissue !="rest")
        stop(paste(this.tissue,"assigned to multiple lumped tissues"))
      
      if (!is.null(parameters)) { #parameters should be complete
        if (!(this.flow.param %in% names(parameters)))
          stop(paste(
            "Parameters != NULL but", this.flow.param, "not in parameters."))
        #if this.flow.param is in parameters  vv
        else this.flow <- parameters[[this.flow.param]]
        
        if (!(this.vol.param %in% names(parameters)))
          stop(paste(
            "Parameters != NULL but", this.vol.param, "not in parameters."))
        #if this.vol.param is in parameters  vv
        else this.vol <- parameters[[this.vol.param]]
        
      } else if (!(this.tissue %in% unique(tissue.data[,'Tissue'])) & 
                 (is.null(tissue.vols) | is.null(tissue.flows)) )
				stop(paste(
               this.tissue,
               "Not provided in tissue.vols/tissue.flow, and is not in list:",
               paste(unique(tissue.data[,'Tissue']),collapse=', ')))
      else {
        #give tissue.vols and tissue.flows priority
        if ((is.null(tissue.vols)) | is.null(tissue.flows)) 
        {
          this.subset <- subset(
            tissue.data,
            Tissue == this.tissue & 
            tolower(Species) == tolower(species) &
            variable %in% c("Flow (mL/min/kg^(3/4))","Vol (L/kg)"))
          if (dim(this.subset)[1]==0) 
          {
            this.subset <- subset(tissue.data,
              Tissue == this.tissue & 
              tolower(Species) == "human" &
              variable %in% c("Flow (mL/min/kg^(3/4))","Vol (L/kg)"))
            if (dim(this.subset)[1]>0)
            {
              if (!suppress.messages) warning(paste(
                "Human tissue flow and volume values for",
                this.tissue,
                "used in tissue lumping."))
            }
          }
          
          if ((is.null(tissue.vols)) | 
            (!(this.lumped.tissue %in% names(tissue.vols)))) 
          {
            this.vol <- as.numeric(subset(
              this.subset,
              variable == 'Vol (L/kg)')[,'value'])
          }
          if ((is.null(tissue.flows)) | 
            (!(this.lumped.tissue %in% names(tissue.flows))))
          {
            this.flow <- as.numeric(subset(
              this.subset,
              variable == 'Flow (mL/min/kg^(3/4))')[,'value']) / 
              as.numeric(subset(physiology.data,
                Parameter=='Cardiac Output')[[species]])
          }
        }
        
        if ((!(is.null(tissue.vols))) & 
          (this.lumped.tissue %in% names(tissue.vols)))
        {
          this.vol <- tissue.vols[[this.lumped.tissue]]
        }
            
        if ((!(is.null(tissue.flows))) & 
          (this.lumped.tissue %in% names(tissue.flows)))
        {
          this.flow <- tissue.flows[[this.lumped.tissue]]
        }
      	
      		#if this.flow or this.vol still NULL after checking all sources
      	if (!suppress.messages & 
          ((length(this.flow) == 0) | (length(this.vol)==0)))
        {
      	  warning("A flow or volume associated with the ",this.tissue," and 
      	passed to lump_tissues is undefined. You may need to check to make
      	sure the input tissue information, if no tissue volume or flow is
      	intended to be left out.")
     		}
      }
# Mark that this tissue has been lumped:
			all.tissues[[this.tissue]] <- TRUE
# Add the volume for this tissue to the lumped tissue:
  		vol[[this.lumped.tissue]] <- vol[[this.lumped.tissue]] + this.vol  
# Add a contribution to the partition coefficient weighted by the volume of 
# this tissue, and check to see if the tissue volume is zero. If it is
# a zero volume send a warning to make sure it is intended to have either a 
# a zero value or no defined tissue volume. This could address the case
# of the placenta, where a partition coefficient can be calculated, but
# it doesn't make sense to have one value for placenta volume on file to
# to work with:
  		if (length(this.vol) > 0){
		Ktissue2pu.out[[this.lumped.tissue]] <- 
      Ktissue2pu.out[[this.lumped.tissue]] + 
      this.vol*Ktissue2pu.in[[this.tissue]]
  		} else { #in case that this.vol is undefined due, which could be
  		  #the case for a tissue that has Schmitt params but no fixed
  		  #volumes or flows in an associated model (like the placenta
  		  #in model fetal_pbtk). Otherwise, there may be an error
  		  #with the tissue volume inputs.
  		  Ktissue2pu.out[[this.lumped.tissue]] <- Ktissue2pu.in[[this.tissue]]
  		}
# Add the flow for this tissue to the lumped tissue:                             
  		flow[[this.lumped.tissue]] <- flow[[this.lumped.tissue]] + this.flow 
		}
#Calculate the average partition coefficient by dividing by the total volume of
#the lumped tissue
	  if (length(vol[[this.lumped.tissue]]) > 0){
	    Ktissue2pu.out[[this.lumped.tissue]] <- 
	      Ktissue2pu.out[[this.lumped.tissue]] / vol[[this.lumped.tissue]]
	  }
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
  
  #handle red blood cells separately due to its variable naming conventions
  if ("red blood cells" %in% tissuenames){
  names(Ktissue2pu.out)[names(Ktissue2pu.out) == 'red blood cells'] <- 'rbc'
  }
  
  #Now assign the general values to the output variables. 
  names(Ktissue2pu.out) <- paste("K",names(Ktissue2pu.out),"2pu",sep='')
  names(vol) <- paste('V',names(vol),'c',sep='')
  names(flow)[names(flow) == 'liver'] <- 'total.liver'
  names(flow) <- paste('Q',names(flow),'f',sep='')
    
 	return(c(Ktissue2pu.out,vol,flow))
}
