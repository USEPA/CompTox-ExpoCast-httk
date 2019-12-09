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
#' @export get_chem_id
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
# get rid of white spaces:
    cas.key <- gsub("\\s","",chem.cas)
#If chemical is identified by CAS, we must make sure its a valid CAS:
    if (!(cas.key %in% chem.physical_and_invitro.data$CAS)) 
      stop("CAS number not found, use get_cheminfo() for valid CAS numbers.\n")
#Set the chemical name:
    found.chem.name <- as.character(na.omit(chem.physical_and_invitro.data[
      chem.physical_and_invitro.data[,"CAS"]==cas.key,"Compound"]))
    found.dtxsid <- as.character(na.omit(chem.physical_and_invitro.data[
      chem.physical_and_invitro.data[,"CAS"]==cas.key,"DTXSID"])) 
  }

#If called by name, need to do a search to find the CAS number and DTXSID:
  if (!is.null(chem.name))
  {
# get rid of white spaces and capitalization:
    names.index <- gsub("\\s","",tolower(chem.physical_and_invitro.data$Compound))
# get rid of dashes:
    names.index <- gsub("\\-","",names.index)
# get rid of white spaces and capitalization:
    name.key <- gsub("\\s","",tolower(chem.name))
# get rid of dashes:
    name.key <- gsub("\\-","",name.key)
    if (!any(names.index==name.key)) 
      stop ("Chemical name not found, use get_cheminfo(info=\"compound\") for \
valid compound names.")
#Set the chemical CAS:
    found.chem.cas <- as.character(na.omit(chem.physical_and_invitro.data[
      names.index==name.key,"CAS"]))
    found.dtxsid <- as.character(na.omit(chem.physical_and_invitro.data[
      names.index==name.key,"DTXSID"]))
  }

  if (!is.null(dtxsid))
  {
# get rid of white spaces:
    dtxsid.key <- gsub("\\s","",tolower(dtxsid))
    if (!(dtxsid.key %in% tolower(chem.physical_and_invitro.data$DTXSID))) 
      stop("DTXSID not found, use get_cheminfo(info=\"DTXISD\") for valid \
DTXSIDs.\n")
#Set the chemical name:
    found.chem.name <- as.character(na.omit(chem.physical_and_invitro.data[
      tolower(chem.physical_and_invitro.data[,"DTXSID"])==tolower(dtxsid.key),
      "Compound"]))
    found.chem.cas <- as.character(na.omit(chem.physical_and_invitro.data[
      tolower(chem.physical_and_invitro.data[,"DTXSID"])==tolower(dtxsid.key),
      "CAS"])) 
  }

  if (!is.null(found.chem.cas) & 
    !is.null(found.chem.name) & 
    !is.null(found.dtxsid))
  {
    if (!is.null(chem.cas)) 
    {
      if (chem.cas != found.chem.cas) stop(paste("Both CAS",
        chem.cas,"and either name or DTXSID were provided as arguments, but found 
        other CAS -- ",found.chem.cas))
    } else if (!is.null(dtxsid)) {
      if (dtxsid != found.dtxsid) stop(paste("Both DTXSID",
        dtxsid,"and either CAS or name were provided as arguments, but found 
        other DTXSID -- ",found.dtxsid))
    } else if (!is.null(chem.name)) {
      if (tolower(chem.name) != tolower(found.chem.name)) stop(paste("Both name",
        chem.name,"and either CAS or DTXSID were provided as arguments, but found 
        other name -- ",found.chem.name))
    }
  }
  
  if (is.null(chem.cas)) chem.cas <- found.chem.cas
  if (is.null(chem.name)) chem.name <- found.chem.name
  if (is.null(dtxsid)) dtxsid <- found.dtxsid
  
  return(list(chem.cas=chem.cas,chem.name=chem.name,dtxsid=dtxsid))
}
