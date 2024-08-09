#' Retrieve chemical information available from HTTK package
#' 
#' This function lists information on all the chemicals within HTTK for which 
#' there are sufficient data for the specified model and species. 
#' By default the function returns only CAS (that is, info="CAS"). 
#' The type of information available includes chemical identifiers 
#' ("Compound", "CAS", "DTXSID"), in vitro
#' measurements ("Clint", "Clint.pvalue", "Funbound plasma", "Rblood2plasma"), 
#' and physico-chemical information ("Formula", "logMA", "logP", "MW",
#' "pKa_Accept", "pKa_Donor"). The argument "info" can be a single type of 
#' information, "all" information, or a vector of specific types of information.
#' The argument "model" defaults to 
#' "3compartmentss" and the argument "species" defaults to "human".  
#' Since different models have different 
#' requirements and not all chemicals have complete data, this function will 
#' return different numbers of chemicals depending on the model specified. If
#' a chemical is not listed by get_cheminfo then either the in vitro or
#' physico-chemical data needed are currently missing (but could potentially
#' be added using \code{\link{add_chemtable}}.
#' 
#' When default.to.human is set to TRUE, and the species-specific data,
#' Funbound.plasma and Clint, are missing from 
#' \code{\link{chem.physical_and_invitro.data}}, human values are given instead.
#'
#' In some cases the rapid equilibrium dialysis method (Waters et al., 2008)
#' fails to yield detectable concentrations for the free fraction of chemical. 
#' In those cases we assume the compound is highly bound (that is, Fup approaches
#' zero). For some calculations (for example, steady-state plasma concentration)
#' there is precedent (Rotroff et al., 2010) for using half the average limit 
#' of detection, that is, 0.005 (this value is configurable via the argument
#' fup.lod.default). We do not recommend using other models where 
#' quantities like partition coefficients must be predicted using Fup. We also
#' do not recommend including the value 0.005 in training sets for Fup predictive
#' models.
#'
#' \strong{Note} that in some cases the \strong{Funbound.plasma} (fup) and the 
#' \strong{intrinsic clearance} (clint) are
#' \emph{provided as a series of numbers separated by commas}. These values are the 
#' result of Bayesian analysis and characterize a distribution: the first value
#' is the median of the distribution, while the second and third values are the 
#' lower and upper 95th percentile (that is quantile 2.5 and 97.5) respectively.
#' For intrinsic clearance a fourth value indicating a p-value for a decrease is
#' provided. Typically 4000 samples were used for the Bayesian analysis, such
#' that a p-value of "0" is equivalent to "<0.00025". See Wambaugh et al. (2019)
#' for more details. If argument median.only == TRUE then only the median is
#' reported for parameters with Bayesian analysis distributions. If the 95% 
#' credible interval is larger than fup.ci.cutoff (defaults
#' to NULL) then the Fup is treated as too uncertain and the value NA is given.
#' 
#' @param info A single character vector (or collection of character vectors)
#' from "Compound", "CAS", "DTXSID, "logP", "pKa_Donor"," pKa_Accept", "MW", "Clint",
#' "Clint.pValue", "Funbound.plasma","Structure_Formula", or "Substance_Type". info="all"
#' gives all information for the model and species.
#' 
#' @param species Species desired (either "Rat", "Rabbit", "Dog", "Mouse", or
#' default "Human").
#' 
#' @param fup.lod.default Default value used for fraction of unbound plasma for
#' chemicals where measured value was below the limit of detection. Default
#' value is 0.0005.
#' 
#' @param model Model used in calculation, 'pbtk' for the multiple compartment
#' model, '1compartment' for the one compartment model, '3compartment' for
#' three compartment model, '3compartmentss' for the three compartment model
#' without partition coefficients, or 'schmitt' for chemicals with logP and
#' fraction unbound (used in predict_partitioning_schmitt).
#' 
#' @param default.to.human Substitutes missing values with human values if
#' true.
#' 
#' @param median.only Use median values only for fup and clint.  Default is FALSE.
#' 
#' @param fup.ci.cutoff Cutoff for the level of uncertainty in fup estimates.
#' This value should be between (0,1). Default is `NULL` specifying no filtering.
#' 
#' @param clint.pvalue.threshold Hepatic clearance for chemicals where the in
#' vitro clearance assay result has a p-values greater than the threshold are
#' set to zero.
#' 
#' @param class.exclude Exclude chemical classes identified as outside of 
#' domain of applicability by relevant modelinfo_[MODEL] file (default TRUE).
#' 
#' @param suppress.messages Whether or not the output messages are suppressed 
#' (default FALSE).
#' 
#' @return \item{vector/data.table}{Table (if info has multiple entries) or 
#' vector containing a column for each valid entry 
#' specified in the argument "info" and a row for each chemical with sufficient
#' data for the model specified by argument "model":
#' \tabular{lll}{
#' \strong{Column} \tab \strong{Description} \tab \strong{units} \cr
#' Compound \tab The preferred name of the chemical compound \tab none \cr 
#' CAS \tab The preferred Chemical Abstracts Service Registry Number \tab none \cr  
#' DTXSID \tab DSSTox Structure ID 
#' (\url{https://comptox.epa.gov/dashboard}) \tab none \cr 
#' logP \tab The log10 octanol:water partition coefficient\tab log10 unitless ratio \cr 
#' MW \tab The chemical compound molecular weight \tab g/mol \cr 
#' pKa_Accept \tab The hydrogen acceptor equilibria concentrations 
#' \tab logarithm \cr   
#' pKa_Donor \tab The hydrogen donor equilibria concentrations 
#'  \tab logarithm \cr   
#' [SPECIES].Clint \tab (Primary hepatocyte suspension) 
#' intrinsic hepatic clearance. \emph{Entries with comma separated values are Bayesian estimates of
#' the Clint distribution - displayed as the median, 95th credible interval
#' (that is quantile 2.5 and 97.5, respectively), and p-value.} \tab uL/min/10^6 hepatocytes \cr    
#' [SPECIES].Clint.pValue \tab Probability that there is no clearance observed.
#' Values close to 1 indicate clearance is not statistically significant. \tab none \cr  
#' [SPECIES].Funbound.plasma \tab Chemical fraction unbound in presence of 
#' plasma proteins (fup). \emph{Entries with comma separated values are Bayesian estimates of
#' the fup distribution - displayed as the median and 95th credible interval
#' (that is quantile 2.5 and 97.5, respectively).} \tab unitless fraction \cr 
#' [SPECIES].Rblood2plasma \tab Chemical concentration blood to plasma ratio \tab unitless ratio \cr  
#' }
#' }
#' 
#' @author John Wambaugh, Robert Pearce, and Sarah E. Davidson
#' 
#' @keywords Retrieval
#' 
#'@references 
#' Rotroff, Daniel M., et al. "Incorporating human dosimetry and exposure into 
#' high-throughput in vitro toxicity screening." Toxicological Sciences 117.2 
#' (2010): 348-358.
#' 
#' Waters, Nigel J., et al. "Validation of a rapid equilibrium dialysis approach 
#' for the measurement of plasma protein binding." Journal of pharmaceutical 
#' sciences 97.10 (2008): 4586-4595.
#'
#' Wambaugh, John F., et al. "Assessing toxicokinetic uncertainty and 
#' variability in risk prioritization." Toxicological Sciences 172.2 (2019): 
#' 235-251.
#'
#' @examples
#' 
#' \donttest{
#' # List all CAS numbers for which the 3compartmentss model can be run in humans: 
#' get_cheminfo()
#' 
#' get_cheminfo(info=c('compound','funbound.plasma','logP'),model='pbtk') 
#' # See all the data for humans:
#' get_cheminfo(info="all")
#' 
#' TPO.cas <- c("741-58-2", "333-41-5", "51707-55-2", "30560-19-1", "5598-13-0", 
#' "35575-96-3", "142459-58-3", "1634-78-2", "161326-34-7", "133-07-3", "533-74-4", 
#' "101-05-3", "330-54-1", "6153-64-6", "15299-99-7", "87-90-1", "42509-80-8", 
#' "10265-92-6", "122-14-5", "12427-38-2", "83-79-4", "55-38-9", "2310-17-0", 
#' "5234-68-4", "330-55-2", "3337-71-1", "6923-22-4", "23564-05-8", "101-02-0", 
#' "140-56-7", "120-71-8", "120-12-7", "123-31-9", "91-53-2", "131807-57-3", 
#' "68157-60-8", "5598-15-2", "115-32-2", "298-00-0", "60-51-5", "23031-36-9", 
#' "137-26-8", "96-45-7", "16672-87-0", "709-98-8", "149877-41-8", "145701-21-9", 
#' "7786-34-7", "54593-83-8", "23422-53-9", "56-38-2", "41198-08-7", "50-65-7", 
#' "28434-00-6", "56-72-4", "62-73-7", "6317-18-6", "96182-53-5", "87-86-5", 
#' "101-54-2", "121-69-7", "532-27-4", "91-59-8", "105-67-9", "90-04-0", 
#' "134-20-3", "599-64-4", "148-24-3", "2416-94-6", "121-79-9", "527-60-6", 
#' "99-97-8", "131-55-5", "105-87-3", "136-77-6", "1401-55-4", "1948-33-0", 
#' "121-00-6", "92-84-2", "140-66-9", "99-71-8", "150-13-0", "80-46-6", "120-95-6",
#' "128-39-2", "2687-25-4", "732-11-6", "5392-40-5", "80-05-7", "135158-54-2", 
#' "29232-93-7", "6734-80-1", "98-54-4", "97-53-0", "96-76-4", "118-71-8", 
#' "2451-62-9", "150-68-5", "732-26-3", "99-59-2", "59-30-3", "3811-73-2", 
#' "101-61-1", "4180-23-8", "101-80-4", "86-50-0", "2687-96-9", "108-46-3", 
#' "95-54-5", "101-77-9", "95-80-7", "420-04-2", "60-54-8", "375-95-1", "120-80-9",
#' "149-30-4", "135-19-3", "88-58-4", "84-16-2", "6381-77-7", "1478-61-1", 
#' "96-70-8", "128-04-1", "25956-17-6", "92-52-4", "1987-50-4", "563-12-2", 
#' "298-02-2", "79902-63-9", "27955-94-8")
#' httk.TPO.rat.table <- subset(get_cheminfo(info="all",species="rat"),
#'  CAS %in% TPO.cas)
#'  
#' httk.TPO.human.table <- subset(get_cheminfo(info="all",species="human"),
#'  CAS %in% TPO.cas)
#'  
#' # create a data.frame with all the Fup values, we ask for model="schmitt" since
#' # that model only needs fup, we ask for "median.only" because we don't care
#' # about uncertainty intervals here:
#' fup.tab <- get_cheminfo(info="all",median.only=TRUE,model="schmitt")
#' # calculate the median, making sure to convert to numeric values:
#' median(as.numeric(fup.tab$Human.Funbound.plasma),na.rm=TRUE)
#' # calculate the mean:
#' mean(as.numeric(fup.tab$Human.Funbound.plasma),na.rm=TRUE)
#' # count how many non-NA values we have (should be the same as the number of 
#' # rows in the table but just in case we ask for non NA values:
#' sum(!is.na(fup.tab$Human.Funbound.plasma))
#' }
#' 
#' @export get_cheminfo
get_cheminfo <- function(info="CAS",
                         species="Human",
                         fup.lod.default=0.005,
                         model='3compartmentss',
                         default.to.human=FALSE,
                         median.only=FALSE,
                         fup.ci.cutoff=TRUE,
                         clint.pvalue.threshold=0.05,
                         class.exclude=TRUE,
                         suppress.messages=FALSE)
{
                                        # Parameters in this list can be retrieve with the info argument:
  valid.info <- c("Compound",
                  "CAS",
                  "Clint",
                  "Clint.pValue",
                  "DTXSID",
                  "Formula",
                  "Funbound.plasma",
                  "logMA",
                  "logP",
                  "MW",
                  "Rblood2plasma",
                  "pKa_Accept",
                  "pKa_Donor"
                  )
  if (any(!(toupper(info) %in% toupper(valid.info))) & 
    any(tolower(info)!="all")) stop(paste("Data on",
    info[!(info %in% valid.info)],"not available. Valid options are:",
    paste(valid.info,collapse=" ")))
  if (any(toupper(info)=="ALL")) info <- valid.info
  # ignore captilization:
  info <- toupper(info)
  
  #Create a local copy so we can edit it:
  chem.physical_and_invitro.data <- chem.physical_and_invitro.data
  physiology.data <- physiology.data
  
  #End R CMD CHECK appeasement.

# Figure out which species we support
  valid.species <- 
    colnames(physiology.data)[!(colnames(physiology.data)
    %in% c("Parameter","Units"))]
# Standardize the species capitalization
  if (tolower(species) %in% tolower(valid.species)) species <-
    valid.species[tolower(valid.species)==tolower(species)]
  else stop("Requested species not found in physiology.table.")
  
# We need to know model-specific information (from modelinfo_[MODEL].R]): 
  model <- tolower(model)
  if (!(model %in% names(model.list)))            
  {
    stop(paste("Model",model,"not available. Please select from:",
      paste(names(model.list),collapse=", ")))
  } else {
    necessary.params <- model.list[[model]]$required.params
    exclude.fup.zero <- model.list[[model]]$exclude.fup.zero
    log.henry.threshold <- model.list[[model]]$log.henry.threshold
    chem.class.filt     <- model.list[[model]]$chem.class.filt
  }
  if (is.null(necessary.params)) stop(paste("Necessary parameters for model",
    model,"have not been defined."))
  
  # For now let's not require these because it's still hard to distringuish
  # between compounds that don't ionize and those for which we don't have
  # good predictions
  necessary.params <- necessary.params[!(tolower(necessary.params)%in%
    tolower(c("pKa_Donor","pKa_Accept","Dow74")))]
  
  # Change to the names in chem.physical_and_invitro.table:
  if ("pow" %in% tolower(necessary.params)) 
    necessary.params[tolower(necessary.params)=="pow"] <-
    "logP"
  
  # Flag in case we can't find a column for every parameter:
  incomplete.data <- FALSE

  # Identify the appropriate column for Clint (if needed):
  species.clint <- paste0(species,'.Clint')
  species.clint.pvalue <- paste0(species,'.Clint.pValue')
  # Make sure capitalization matches a table column:
  if (tolower(species.clint) %in% 
    tolower(colnames(chem.physical_and_invitro.data)))
  {
    species.clint <- colnames(chem.physical_and_invitro.data)[
      tolower(colnames(chem.physical_and_invitro.data)) ==
      tolower(species.clint)]
    species.clint.pvalue <- colnames(chem.physical_and_invitro.data)[
      tolower(colnames(chem.physical_and_invitro.data)) ==
      tolower(species.clint.pvalue)]
  }
  
  # Check to see if we need clint:
  if (tolower("Clint") %in% 
    unique(tolower(c(necessary.params,info))))   
  {
    # Check to see if we will use human data where species data is missing:
    if (default.to.human)
    {
      # Check to see if this is a column that already has data:
      if (species.clint %in% colnames(chem.physical_and_invitro.data))
      {
        # Replace chemicals with NA's only:
        replace.index <- is.na(chem.physical_and_invitro.data[,species.clint])
        if (any(replace.index))
        {
          chem.physical_and_invitro.data[replace.index,species.clint] <-
            chem.physical_and_invitro.data[replace.index,'Human.Clint']
          chem.physical_and_invitro.data[replace.index,species.clint.pvalue] <-
            chem.physical_and_invitro.data[replace.index,'Human.Clint.pValue']
          if (!suppress.messages) 
            warning('Human values substituted for Clint and Clint.pValue.')
        }
      } else {
        chem.physical_and_invitro.data[,species.clint] <-
          chem.physical_and_invitro.data[,'Human.Clint']
        chem.physical_and_invitro.data[,species.clint.pvalue] <-
          chem.physical_and_invitro.data[,'Human.Clint.pValue']
        if (!suppress.messages) 
          warning('Human values substituted for Clint and Clint.pValue.')
      }    
    }
    # Check to see if we have a column for this species in the table:
    if (!(species.clint %in% colnames(chem.physical_and_invitro.data)))  
    {
      incomplete.data <- TRUE
    } else {
      # Set observed clint values to 0 if clint.pvalue > threshold
      if (!is.null(clint.pvalue.threshold))
      {
        if (any(!is.na(chem.physical_and_invitro.data[, 
            species.clint])))
        {
          clint.values  <- strsplit(chem.physical_and_invitro.data[,species.clint],
            split = ",")
        } else {
          clint.values <- rep(NA,dim(chem.physical_and_invitro.data)[1])
        }
        clint.pvalues <- chem.physical_and_invitro.data[,species.clint.pvalue]
        # Replace the clint.value with 0 when clint.pvalue > threshold
        clint.values[lapply(clint.values,length)!=4] <- 
          ifelse(
            clint.pvalues[lapply(clint.values,length)!=4] > 
            clint.pvalue.threshold & 
            !is.na(clint.pvalues[lapply(clint.values,length)!=4]),
            yes = "0",
            no = clint.values[lapply(clint.values,length)!=4]
          )
        # Replace the (median,l95,u95) with 0 when clint.pvalue > threshold
        clint.values[lapply(clint.values,length)==4]<-
          ifelse(
            clint.pvalues[lapply(clint.values,length)==4] >
            clint.pvalue.threshold & 
            !is.na(clint.pvalues[lapply(clint.values,length)==4]),
            yes = lapply(clint.values[lapply(clint.values,length)==4],
            function(x){x<-c(rep("0",3),x[[4]])}),
            no = clint.values[lapply(clint.values,length)==4]
          )
        
        clint.values <- lapply(clint.values,function(x)paste(x,collapse = ","))
        chem.physical_and_invitro.data[,species.clint] <- unlist(clint.values)
        if (!suppress.messages & "CLINT" %in% info)
          warning(paste(
            'Clint values with a pvalue >',
            clint.pvalue.threshold,
            'were set to 0.'))
      }    
    }
    # Change the necessary parameters to the chem.physical_and_invitro.data col:
    if (!is.null(species.clint)) 
    {
      necessary.params[necessary.params=="Clint"]<-species.clint
    }
  }

# Check to see if we need fup (don't we always?)
  if (tolower("Funbound.plasma") %in% 
    unique(tolower(c(necessary.params,info))))
  {
    # Identify the appropriate column for Funbound (if needed):
    species.fup <- paste0(species,'.Funbound.plasma')
    if (default.to.human)
    {
      # Check to see if this is a column that already has data:
      if (species.fup %in% colnames(chem.physical_and_invitro.data))
      {
        # Identify values to replace with human:
        if (exclude.fup.zero) 
        {
          # Turn triples with confidence intervals into single values: 
          temp.fup <- strsplit(as.character(
            chem.physical_and_invitro.data[,species.fup]),",")
          if (any(unlist(lapply(temp.fup,length))>1)) 
          {
            temp.fup <-  suppressWarnings(as.numeric(unlist(lapply(
              temp.fup, 
              function(x) x[[1]]))))
          } else {
            temp.fup <-  suppressWarnings(as.numeric(unlist(temp.fup)))
          }    # Check to see if we will use human data where species data is missing:          # Replace all the zeros if that will impact the model:
          replace.index <- (temp.fup==0)
          # Comparisons with NA's will produce NA's
          replace.index[is.na(replace.index)] <- TRUE
        } else {
          # Otherwise just replace NA's
          replace.index <- is.na(chem.physical_and_invitro.data[,species.fup])
        }
        if (any(replace.index))
        {
          chem.physical_and_invitro.data[replace.index,species.fup] <-
            chem.physical_and_invitro.data[replace.index,
            'Human.Funbound.plasma']
          if (!suppress.messages) 
            warning('Human values substituted for Funbound.plasma.')
        }
      } else {
        chem.physical_and_invitro.data[,species.fup] <-
          chem.physical_and_invitro.data[,'Human.Funbound.plasma']
        if (!suppress.messages)
          warning('Human values substituted for Funbound.plasma.')
      }
    }
    # Check to see if we have a column for this species in the table:
    if (!(species.fup %in% 
      colnames(chem.physical_and_invitro.data))) 
    {
      incomplete.data <- TRUE
    }
    # Change the necessary parameters to the chem.physical_and_invitro.data col:
    if (!is.null(species.fup)) 
    {
      necessary.params[necessary.params=="Funbound.plasma"]<-species.fup
    }
  }

  # Check to see if we need Rblood2plasma:
  if (tolower("Rblood2plasma") %in% 
    unique(tolower(c(necessary.params,info))))   
  {
    # Identify the appropriate column for Rblood2plasma (if needed):
    species.rblood2plasma <- paste0(species,'.Rblood2plasma')
    if (default.to.human)
    {
      # Check to see if this is a column that already has data:
      if (species.rblood2plasma %in% colnames(chem.physical_and_invitro.data))
      {
        # Replace chemicals with NA's only:
        replace.index <- is.na(
          chem.physical_and_invitro.data[,species.rblood2plasma])
        if (any(replace.index))
        {
          chem.physical_and_invitro.data[replace.index,species.rblood2plasma] <-
            chem.physical_and_invitro.data[replace.index,'Human.Rblood2plasma']
          if (!suppress.messages)
            warning('Human values substituted for Rblood2plasma.')
        }
      } else {
        chem.physical_and_invitro.data[,species.rblood2plasma] <-
          chem.physical_and_invitro.data[,'Human.Rblood2plasma']
        if (!suppress.messages)
          warning('Human values substituted for Rblood2plasma.')
      }    
    }
    if (!(species.rblood2plasma %in% 
      colnames(chem.physical_and_invitro.data)))
    {
      incomplete.data <- TRUE
    }
    # Change the necessary parameters to the chem.physical_and_invitro.data col:
    if (!is.null(species.rblood2plasma)) 
    {
      necessary.params[necessary.params=="Rblood2plasma"]<-species.rblood2plasma
    }
  } 

  if (!incomplete.data)
  {
    # Only look for parameters that we have in the table:
    necessary.params <- necessary.params[tolower(necessary.params) %in%
    tolower(colnames(chem.physical_and_invitro.data))]
  
  # Pare the chemical data down to only those chemicals where all the necessary
  # parameters are not NA
    good.chemicals.index <- apply(
      chem.physical_and_invitro.data[,necessary.params],
      1,
      function(x) all(!is.na(x)))
  # print a warning of exclusion criteria for compounds
  #  - ONLY if it is applies (i.e. is an exclusion criterion for at least 1 compound)
  if (!suppress.messages & any(good.chemicals.index==FALSE))
  {
    warning(paste(
      "Excluding compounds that have one or more needed parameters missing in chem.physical_and_invitro.table.\n
For model ", model, " each chemical must have non-NA values for:",
      paste(necessary.params,collapse=", "), sep=""))
  }
  # If we need fup:
    if (tolower(paste(species,"Funbound.plasma",sep=".")) %in% 
      unique(tolower(c(necessary.params,info))))
    { 
     # Make sure that we have a usable fup:     
      fup.values <- strsplit(as.character(
        chem.physical_and_invitro.data[,species.fup]),",")
      if (any(unlist(lapply(fup.values,length))>1)) 
      {
      # Go with the upper 95th credible interval before throwing anything out:
        fup.values[lapply(fup.values,length)==3] <- 
          lapply(fup.values[lapply(fup.values,length)==3], function(x) x[[3]])
      } 
      fup.values <-  suppressWarnings(as.numeric(unlist(fup.values)))
      fup.values.numeric <- !is.na(fup.values)
  # If we are exclude the fups with a zero, then get rid of those:
      if (exclude.fup.zero) 
      {
        suppressWarnings(fup.values.numeric[fup.values==0] <- FALSE)
      }
      good.chemicals.index <- good.chemicals.index & fup.values.numeric
      # print a warning of exclusion criteria for compounds
      #  - ONLY if it is applies (i.e. is an exclusion criterion for at least 1 compound)
      if(!suppress.messages & any(fup.values.numeric==FALSE)){
        if(exclude.fup.zero){
          warning("Excluding compounds without a 'fup' value (i.e. fup value = NA) or a 'fup' value of 0.")
        }else{
          warning("Excluding compounds without a 'fup' value (i.e. fup value = NA).")
        }
      }
      # If we are excluding fups with uncertain ci intervals, then get rid of those:
      if(fup.ci.cutoff){
        # separate concatenated values
        fup.ci.diff <- strsplit(as.character(
          chem.physical_and_invitro.data[,species.fup]),",")
        # if only one element assume TRUE
        fup.ci.diff[lapply(fup.ci.diff,length)!=3] <- TRUE
        # if 3 elements, then calculate interval length and check if it passes the cutoff 
        fup.ci.diff[lapply(fup.ci.diff,length)==3] <- 
          lapply(fup.ci.diff[lapply(fup.ci.diff,length)==3],function(x){
            t.ci <- as.numeric(x)
            # Fup's where confidence interval spans nearly all possible values:
            out  <- ifelse((t.ci[3]>0.9 & t.ci[2]<0.1),yes = FALSE, no = TRUE)
            return(out)
          })
        fup.ci.cert <- unlist(fup.ci.diff)
        
        good.chemicals.index <- good.chemicals.index & fup.ci.cert
        # print a warning of exclusion criteria for compounds
        #  - ONLY if it is applies (i.e. is an exclusion criterion for at least 1 compound)
        if(!suppress.messages & any(fup.ci.cert==FALSE)){
          warning("Excluding compounds with uncertain 'fup' confidence/credible intervals.")
        }
      }
    }
    
# If we need Clint:
    if (tolower(paste(species,"Clint",sep=".")) %in% 
      unique(tolower(c(necessary.params,info))))
    {
      clint.values <- chem.physical_and_invitro.data[,species.clint]
      clint.values.numeric <- suppressWarnings(!is.na(as.numeric(clint.values)))
      clint.values.dist <- 
        suppressWarnings(nchar(clint.values) - 
        nchar(gsub(",","",clint.values))==3)
      clint.values.dist[is.na(clint.values.dist)] <- FALSE
      good.chemicals.index <- good.chemicals.index &
# Either a numeric value:
        (clint.values.numeric |
# or four values separated by three commas:
        clint.values.dist)
      # print a warning of exclusion criteria for compounds
      #  - ONLY if it is applies (i.e. is an exclusion criterion for at least 1 compound)
      if(!suppress.messages & any((clint.values.numeric|clint.values.dist)==FALSE)){
        warning("Excluding compounds that do not have a clint value or distribution of clint values.")
      }
    }
    
    # If we need to remove volatile compounds:
    if(!is.null(log.henry.threshold)){
      # keep compounds with logHenry constant less than threshold & 'NA'
      log.henry.pass <- 
        chem.physical_and_invitro.data[,"logHenry"] < 
        log.henry.threshold|is.na(chem.physical_and_invitro.data[,"logHenry"])
      
      # obtain the the chemical indexes to keep
      good.chemicals.index <- good.chemicals.index & log.henry.pass
      # print a warning of exclusion criteria for compounds
      #  - ONLY if it is applies (i.e. is an exclusion criterion for at least 1 compound)
      if(!suppress.messages & any(log.henry.pass==FALSE)){
        warning("Excluding volatile compounds defined as log.Henry >= ",log.henry.threshold,".")
      }
    }
    # If we need to remove compounds belonging to a given chemical class:
    if (class.exclude & !is.null(chem.class.filt))
    {
      # obtain the chemical classifications
      chem.class <- strsplit(
        chem.physical_and_invitro.data[,"Chemical.Class"],
        split = ",")
      # check if the chemical class is in the filter-out object
      no.chem.class.index <- lapply(
        chem.class,
        function(x)!(any(x%in%chem.class.filt)))
      # obtain the chemical indexes to keep
      good.chemicals.index <- good.chemicals.index & unlist(no.chem.class.index)
      # print a warning of exclusion criteria for compounds
      #  - ONLY if it is applies (i.e. is an exclusion criterion for at least 1 compound)
      if(!suppress.messages & any(unlist(no.chem.class.index)==FALSE)){
        warning("Excluding compounds that are categorized in one or more of ",
                "the following chemical classes: ",
                paste(chem.class.filt,collapse = ", "),".")
      }
    }
    
# Kep just the chemicals we want:    
    good.chemical.data <- chem.physical_and_invitro.data[good.chemicals.index,] 
    
# Get the calpitalizes tion correct on the information requested:
    if ('mw' %in% tolower(info)) info <- c('MW',info[tolower(info) != 'mw'])
    if ('pka_accept' %in% tolower(info)) info <- 
      c('pKa_Accept',info[tolower(info) != 'pka_accept'])
    if ('pka_donor' %in% tolower(info)) info <- 
      c('pKa_Donor',info[tolower(info) != 'pka_donor'])
    if ('logp' %in% tolower(info)) info <- 
      c('logP',info[tolower(info) != 'logp'])
    if ('compound' %in% tolower(info)) info <- 
      c('Compound',info[tolower(info) != 'compound'])
    if ('cas' %in% tolower(info)) info <- c('CAS',info[tolower(info) != 'cas'])
    if ('dsstox_substance_id' %in% tolower(info)) info <- 
      c('DSSTox_Substance_Id',info[tolower(info) != 'dsstox_substance_id'])
    if ('structure_formula' %in% tolower(info)) info <- 
      c('Structure_Formula',info[tolower(info) != 'structure_formula'])
    if ('substance_type' %in% tolower(info)) info <- 
      c('Substance_Type',info[tolower(info) != 'substance_type'])    
 
    if (toupper("Clint") %in% toupper(info)) 
      info[toupper(info)==toupper("Clint")] <- species.clint
    if (toupper("Clint.pValue") %in% toupper(info)) 
      info[toupper(info)==toupper("Clint.pValue")] <- species.clint.pvalue
    if (toupper("Funbound.plasma") %in% toupper(info)) 
      info[toupper(info)==toupper("Funbound.plasma")] <- species.fup
    if (toupper("Rblood2plasma") %in% toupper(info)) 
      info[toupper(info)==toupper("Rblood2plasma")] <- species.rblood2plasma
    
    columns <- colnames(chem.physical_and_invitro.data)
    this.subset <- good.chemical.data[,
      toupper(colnames(chem.physical_and_invitro.data))%in%toupper(columns)]
    
    if('CAS' %in% info) rownames(this.subset) <- NULL 
    
    if (!exclude.fup.zero) 
    {
      fup.zero.chems <- 
        suppressWarnings(as.numeric(this.subset[,species.fup]) == 0)
      fup.zero.chems[is.na(fup.zero.chems)] <- FALSE
      this.subset[fup.zero.chems, species.fup] <- fup.lod.default
    }
    # If we want the median values only for fup and clint
    if(median.only){
      fup.values      <- strsplit(as.character(this.subset[,species.fup]),",")
      fup.median.only <- lapply(fup.values, function(x) x[[1]])
      this.subset[,species.fup] <- as.numeric(unlist(fup.median.only))
      
      clint.values      <- strsplit(
        as.character(this.subset[,species.clint]),
        ",")
      clint.median.only <- lapply(clint.values,function(x)x[[1]])
      this.subset[,species.clint] <- as.numeric(unlist(clint.median.only))
    }
                                
    return.info <- this.subset[,colnames(this.subset)%in%info]
  } else return.info <- NULL 
    
  return(return.info)
}