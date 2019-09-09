#' Retrieve chemical identity from HTTK package
#' 
#' Given one of chem.name, chem.cas (Chemical Abstract Service Registry Number),
#' or DTXSID (DSStox Substance Identifier \url{https://comptox.epa.gov} this
#' function checks if the chemical is available and, if so, returns all three
#' pieces of information.
#' @author John Wambaugh and Robert Pearce
#' @keywords cheminformatics
#' @param chem.cas CAS regstry number
#' @param chem.name Chemical name
#' @param dtxsid DSSTox Substance identifier
get_chem_id <- function(chem.cas=NULL,
                        chem.name=NULL,
                        dtxsid=NULL)
{
  if (is.null(chem.cas) & is.null(chem.name) & is.null(dtxsid))
  {
    stop("Must specifiy compound namem, CAS, or DTXSID.\n")
  } else if ((!is.null(chem.cas) & 
    !any(chem.physical_and_invitro.data$CAS==chem.cas)) & 
    (!is.null(chem.name) & 
    !any(chem.physical_and_invitro.data$Compound==chem.name)) &
    (!is.null(dtxsid) & 
    !any(chem.physical_and_invitro.data$DTXSID==dtxsid)))
  {
    stop("Compound not found.\n")
  }

  found.chem.cas <- NULL
  found.chem.name <- NULL
  found.dtxsid <- NULL

  if (!is.null(chem.cas))
  {
#If chemical is identified by CAS, we must make sure its a valid CAS:
    if (!(chem.cas %in% chem.physical_and_invitro.data$CAS)) 
      stop("CAS number not found, use get_cheminfo() for valid CAS numbers.\n")
#Set the chemical name:
    found.chem.name <- chem.physical_and_invitro.data[
      chem.physical_and_invitro.data[,"CAS"]==chem.cas,"Compound"]
    found.dtxsid <- chem.physical_and_invitro.data[
      chem.physical_and_invitro.data[,"CAS"]==chem.cas,"DTXSID"] 
  }

  if (!is.null(chem.name))
  {
#If called by name, need to do a search to find the CAS number:
    names.index <- gsub("\\s","",tolower(chem.physical_and_invitro.data$Compound))
    names.index <- gsub("\\-","",names.index)
    name.key <- gsub("\\s","",tolower(chem.name))
    name.key <- gsub("\\-","",name.key)
    if (!any(names.index==name.key)) 
      stop ("Chemical name not found, use get_cheminfo(info=\"compound\") for \
valid compound names.")
#Set the chemical CAS:
    found.chem.cas <- chem.physical_and_invitro.data[names.index==name.key,"CAS"]
    found.chem.cas <- found.chem.cas[!is.na(found.chem.cas)]
    found.dtxsid <- chem.physical_and_invitro.data[names.index==name.key,"DTXSID"]
    found.dtxsid <- found.dtxsid[!is.na(found.dtxsid)]
  }

  if (!is.null(dtxsid))
  {
    if (!(dtxsid %in% chem.physical_and_invitro.data$DTXSID)) 
      stop("DTXSID not found, use get_cheminfo(info=\"DTXISD\") for valid \
DTXSIDs.\n")
#Set the chemical name:
    found.chem.name <- chem.physical_and_invitro.data[
      tolower(chem.physical_and_invitro.data[,"DTXSID"])==tolower(dtxsid),
      "Compound"]
    found.chem.cas <- chem.physical_and_invitro.data[
      tolower(chem.physical_and_invitro.data[,"DTXSID"])==tolower(dtxsid),
      "CAS"] 
  }

  if (!is.null(found.chem.cas) & 
    !is.null(found.chem.name) & 
    !is.null(found.dtxsid))
  {
    if (chem.cas != found.chem.cas) stop(paste("Both CAS",
      chem.cas,"and either name or DTXSID were provided as arguments, but found 
      other CAS -- ",found.chem.cas))
    else if (dtxsid != found.dtxsid) stop(paste("Both DTXSID",
      dtxsid,"and either CAS or name were provided as arguments, but found 
      other DTXSID -- ",found.dtxsid))
    else if (tolower(chem.name) != tolower(found.chem.name)) 
      warning(paste("Both name",chem.name," and  either DTXSID or CASS were \
provided as arguments, but also found name",found.chem.name))
  }

  if (is.null(chem.cas)) chem.cas <- found.chem.cas
  if (is.null(chem.name)) chem.name <- found.chem.name
  if (is.null(dtxsid)) dtxsid <- found.dtxsid
  
  return(list(chem.cas=chem.cas,chem.name=chem.name,dtxsid=dtxsid))
}
