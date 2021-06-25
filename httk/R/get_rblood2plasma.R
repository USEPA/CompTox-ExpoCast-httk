#' Get ratio of the blood concentration to the plasma concentration.
#' 
#' This function attempts to retrieve a measured species- and chemical-specific 
#' blood:plasma concentration ratio.
#' 
#' A value of NA is returned when the requested value is unavailable.  Values
#' are retrieved from chem.physical_and_invitro.data. %% ~~ If necessary, more
#' details than the description above ~~
#' 
#' @param chem.name Either the chemical name or the CAS number must be
#' specified. 
#' @param chem.cas Either the CAS number or the chemical name must be
#' specified. 
#' @param dtxsid EPA's 'DSSTox Structure ID (https://comptox.epa.gov/dashboard)  
#' the chemical must be identified by either CAS, name, or DTXSIDs
#' @param species Species desired (either "Rat", "Rabbit", "Dog", "Mouse", or
#' default "Human"). 
#' @param default.to.human Substitutes missing animal values with human values
#' if true.
#'
#' @author Robert Pearce
#'
#' @keywords Parameter
#'
#' @return
#' A numeric value for the steady-state ratio of chemical concentration in blood
#' to plasma
#'
#' @examples
#' 
#' get_rblood2plasma(chem.name="Bisphenol A")
#' get_rblood2plasma(chem.name="Bisphenol A",species="Rat")
#' 
#' @export get_rblood2plasma


get_rblood2plasma <- function(
                       chem.name=NULL,
                       chem.cas=NULL,
                       dtxsid=NULL,
                       species='Human',
                       default.to.human=FALSE)
{
  #R CMD CHECK throws notes about "no visible binding for global variable", for
  #each time a data.table column name is used without quotes. To appease R CMD
  #CHECK, a variable has to be created for each of these column names and set to
  #NULL. Note that within the data.table, these variables will not be NULL! Yes,
  #this is pointless and annoying.
  CAS <- NULL
  #End R CMD CHECK appeasement.
  
  
  chem.physical_and_invitro.data <- chem.physical_and_invitro.data

# We need to describe the chemical to be simulated one way or another:
  if (is.null(chem.cas) & 
      is.null(chem.name) & 
      is.null(dtxsid))
    stop('chem.name, chem.cas, or dtxsid must be specified.')

# Look up the chemical name/CAS, depending on what was provide:
  out <- get_chem_id(
          chem.cas=chem.cas,
          chem.name=chem.name,
          dtxsid=dtxsid)
  chem.cas <- out$chem.cas
  chem.name <- out$chem.name                                
  dtxsid <- out$dtxsid  

  species.Rblood2plasma <- paste0(toupper(substr(species,1,1)),
                             substr(species,2,nchar(species)),'.Rblood2plasma')
  if (!species.Rblood2plasma %in% colnames(chem.physical_and_invitro.data)) 
    Rblood2plasma <- NA
  else Rblood2plasma <- subset(chem.physical_and_invitro.data,CAS == chem.cas)[,
    species.Rblood2plasma]
  if (default.to.human & is.na(Rblood2plasma) & tolower(species) != 'human')
  {
    Rblood2plasma <- subset(chem.physical_and_invitro.data,CAS == chem.cas)[,
                       'Human.Rblood2plasma']
    if (!is.na(Rblood2plasma)) 
      warning('Human in vivo Rblood2plasma substituted for missing value.')
  }
  return(Rblood2plasma)
}
