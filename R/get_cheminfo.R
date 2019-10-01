#' Retrieve chemical information from HTTK package
#' 
#' This function provides the information specified in "info=" (can be single entry
#' or vector) for all chemicals for which a toxicokinetic model can be
#' paramterized for a given species.
#' 
#' When default.to.human is set to TRUE, and the species-specific data,
#' Funbound.plasma and Clint, are missing from chem.physical_and_invitro.data,
#' human values are given instead.
#' 
#' @param info A single character vector (or collection of character vectors)
#' from "Compound", "CAS", "logP", "pKa_Donor"," pKa_Accept", "MW", "Clint",
#' "Clint.pValue", "Funbound.plasma",
#' "DSSTox_Substance_Id","Structure_Formula", or "Substance_Type". info="all"
#' gives all information for the model and species.
#' @param species Species desired (either "Rat", "Rabbit", "Dog", "Mouse", or
#' default "Human").
#' @param exclude.fup.zero Whether or not to exclude chemicals with a fraction
#' of unbound plasma equal to zero or include them with a value of
#' fup.lod.default. Defaults to FALSE for '3compartmentss' and TRUE for pk
#' models and schmitt.
#' @param fup.lod.default Default value used for fraction of unbound plasma for
#' chemicals where measured value was below the limit of detection. Default
#' value is 0.0005.
#' @param model Model used in calculation, 'pbtk' for the multiple compartment
#' model, '1compartment' for the one compartment model, '3compartment' for
#' three compartment model, '3compartmentss' for the three compartment model
#' without partition coefficients, or 'schmitt' for chemicals with logP and
#' fraction unbound (used in predict_partitioning_schmitt).
#' @param default.to.human Substitutes missing values with human values if
#' true.
#' @return \item{info}{Table/vector containing values specified in "info" for
#' valid chemicals.}
#' @author John Wambaugh and Robert Pearce
#' @keywords Retrieval
#' @examples
#' 
#' \dontrun{
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
#' }
#' 
#' @export get_cheminfo
get_cheminfo <- function(info="CAS",
                         species="Human",
                         exclude.fup.zero=NA,
                         fup.lod.default=0.005,
                         model='3compartmentss',
                         default.to.human=F)
{
# Parameters in this list can be retreive with the info argument:
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
                  "pKa_Accept",
                  "pKa_Donor"
                  )
                  
  chem.physical_and_invitro.data <- chem.physical_and_invitro.data
  if (tolower(species) == 'human') species <- 'Human' 
  else if (tolower(species) == 'rat') species <- 'Rat'
  else if (tolower(species) == 'dog') species <- 'Dog'
  else if (tolower(species) == 'rabbit') species <- 'Rabbit'
  else if (tolower(species) == 'mouse') species <- 'Mouse'
  else stop("Only species of human, rat, mouse, rabbit, and dog accepted.")
  
  model <- tolower(model)
  species.fup <- NULL
  species.clint <- NULL
  species.clint.pvalue <- NULL
  if (model == "pbtk" | 
      model == "3compartment" | 
      model == "1compartment" | 
      model == "3compartmentss")
  {
    if (!all(c(paste0(species,'.Funbound.plasma'),paste0(species,'.Clint')) %in% 
        colnames(chem.physical_and_invitro.data)) & 
        !default.to.human) incomplete.data <- T
    else
    {    
      if (paste0(species,'.Funbound.plasma') %in% 
        colnames(chem.physical_and_invitro.data)) 
        species.fup <- paste0(species,'.Funbound.plasma')
      else
      {
        species.fup <- 'Human.Funbound.plasma'
        warning('Human values substituted for Funbound.plasma.')
      }
      if (paste0(species,'.Clint') %in% 
        colnames(chem.physical_and_invitro.data))
      {
        species.clint <- paste0(species,'.Clint')
        species.clint.pvalue <- paste0(species,'.Clint.pValue')
      } else {
        species.clint <- 'Human.Clint'
        species.clint.pvalue <- 'Human.Clint.pValue'
        warning('Human values substituted for Clint and Clint.pValue.')
      }

      if (model == '3compartmentss')
      {
        necessary.params <- c(species.clint,"MW","logP")
        if(is.na(exclude.fup.zero)) exclude.fup.zero <- F 
      } else {
        necessary.params <- c(species.clint,"MW","logP")
        if(is.na(exclude.fup.zero)) exclude.fup.zero <- T
      }
      incomplete.data <- F
    }
  } else if (model == 'schmitt'){
    if (!paste0(species,'.Funbound.plasma') %in% 
      colnames(chem.physical_and_invitro.data) & 
      !default.to.human) incomplete.data <- T
    else
    {
      if (paste0(species,'.Funbound.plasma') %in% 
        colnames(chem.physical_and_invitro.data)) 
        species.fup <- paste0(species,'.Funbound.plasma')
      else
      {
        species.fup <- 'Human.Funbound.plasma'
        warning('Human values substituted for Funbound.plasma.')
      }
      if ('clint.pvalue' %in% tolower(info) | any(tolower(info) == 'all'))
      {
        if (paste0(species,'.Clint') %in% 
        colnames(chem.physical_and_invitro.data))
        {
          species.clint <- paste0(species,'.Clint')
          species.clint.pvalue <- paste0(species,'.Clint.pValue')
        } else if (default.to.human) {
          species.clint <- 'Human.Clint'
          species.clint.pvalue <- 'Human.Clint.pValue'
          warning('Human values substituted for Clint and Clint.pValue.')
        } else stop('Set default.to.human to TRUE for Clint values with selected species') 
      }   
      necessary.params <- c(species.fup,"logP")
      exclude.fup.zero <- T  
      incomplete.data <- F
    } 
  } else stop("Valid models are currently only: pbtk, 1compartment, 3compartment, schmitt, and 3compartmentss.")
  if (!incomplete.data)
  {
  # Pare the chemical data down to only those chemicals where all the necessary
  # parameters are not NA
    good.chemicals.index <- apply(chem.physical_and_invitro.data[,necessary.params],
      1,function(x) all(!is.na(x)))
# Make sure that we have a usable fup:
    fup.values <- chem.physical_and_invitro.data[,species.fup]
    fup.values.numeric <- suppressWarnings(!is.na(as.numeric(fup.values)))
# If we are exclude the fups with a zero, then get rid of those:
    if (exclude.fup.zero) 
    {
      fup.values.numeric[as.numeric(fup.values)==0] <- F
      fup.values.numeric[is.na(fup.values.numeric)] <- F 
    }
    fup.values.dist <- suppressWarnings(nchar(fup.values) - nchar(gsub(",","",fup.values))==2) 
    fup.values.dist[is.na(fup.values.dist)] <- F
    good.chemicals.index <- good.chemicals.index & 
# Either a numeric value:
      (fup.values.numeric |
# or three values separated by two commas:
      fup.values.dist)
# Make sure that we have a usable clint:    
    if (!(model %in% c("schmitt")))
    {
      clint.values <- chem.physical_and_invitro.data[,species.clint]
      clint.values.numeric <- suppressWarnings(!is.na(as.numeric(clint.values)))
      clint.values.dist <- suppressWarnings(nchar(clint.values) - nchar(gsub(",","",clint.values))==3)
      clint.values.dist[is.na(clint.values.dist)] <- F
      good.chemicals.index <- good.chemicals.index &
# Either a numeric value:
        (clint.values.numeric |
# or four values separated by three commas:
        clint.values.dist)
    }
    good.chemical.data <- chem.physical_and_invitro.data[good.chemicals.index,] 
    
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
    
    if (any(toupper(info)=="ALL")) info <- valid.info
    
    if (any(!(toupper(info) %in% toupper(valid.info)))) stop(paste("Data on",
      info[!(info %in% valid.info)],"not available. Valid options are:",
      paste(valid.info,collapse=" ")))
  
    if (toupper("Clint") %in% toupper(info)) 
      info[toupper(info)==toupper("Clint")] <- species.clint
    if (toupper("Clint.pValue") %in% toupper(info)) 
      info[toupper(info)==toupper("Clint.pValue")] <- species.clint.pvalue
    if (toupper("Funbound.plasma") %in% toupper(info)) 
      info[toupper(info)==toupper("Funbound.plasma")] <- species.fup
    
    columns <- colnames(chem.physical_and_invitro.data)
    this.subset <- good.chemical.data[,
      toupper(colnames(chem.physical_and_invitro.data))%in%toupper(columns)]
    
    if('CAS' %in% info) rownames(this.subset) <- NULL 
    
    if (!exclude.fup.zero) 
    {
      fup.zero.chems <- suppressWarnings(as.numeric(this.subset[,species.fup]) == 0)
      fup.zero.chems[is.na(fup.zero.chems)] <- FALSE
      this.subset[fup.zero.chems, species.fup] <- fup.lod.default
    }
                                
    data.table <- this.subset
    return.info <- data.table[,info]
  } else return.info <- NULL 
    
  return(return.info)
}
