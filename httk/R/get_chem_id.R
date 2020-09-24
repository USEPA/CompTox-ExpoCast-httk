#' Retrieve chemical identity from HTTK package
#' 
#' Given one of chem.name, chem.cas (Chemical Abstract Service Registry Number),
#' or DTXSID (DSStox Substance Identifier \url{https://comptox.epa.gov/dashboard}) this
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
    stop("Must specify compound name, CAS, or DTXSID.\n")
  } 
  
  num.chems <- max(length(chem.cas),length(chem.name),length(dtxsid),na.rm=T)
  
  chem.cas.out <- NULL
  chem.name.out <- NULL
  dtxsid.out <- NULL
  
# any loop is really inefficient, need to give thought to how often this is 
# called in vetor mode and vectorize if possible:
  for (i in 1:num.chems)
  {
    this.chem.cas <- chem.cas[i] 
    this.chem.name <- chem.name[i] 
    this.dtxsid <- dtxsid[i] 
  
    if ((!is.null(this.chem.cas) & 
      !any(chem.physical_and_invitro.data$CAS==this.chem.cas)) & 
      (!is.null(this.chem.name) & 
      !any(chem.physical_and_invitro.data$Compound==this.chem.name)) &
      (!is.null(this.dtxsid) &            
      !any(chem.physical_and_invitro.data$DTXSID==this.dtxsid)))
    {
      stop("Compound not found.\n")
    }
  
    found.chem.cas <- NULL
    found.chem.name <- NULL
    found.dtxsid <- NULL
  
    if (!is.null(this.chem.cas))
    {
  # get rid of white spaces:
      cas.key <- gsub("\\s","",this.chem.cas)
  #If chemical is identified by CAS, we must make sure its a valid CAS:
      if (!(cas.key %in% chem.physical_and_invitro.data$CAS)) 
        stop("CAS number not found, use get_cheminfo() for valid CAS numbers.\n")
  #Set the chemical name:
      found.chem.name <- as.character(na.omit(chem.physical_and_invitro.data[
        chem.physical_and_invitro.data[,"CAS"]==cas.key,"Compound"]))
      found.dtxsid <- as.character(na.omit(chem.physical_and_invitro.data[
        chem.physical_and_invitro.data[,"CAS"]==cas.key,"DTXSID"])) 
    }
  
  #If called by name, need to do a search to find the CAS number and this.dtxsid:
    if (!is.null(this.chem.name))
    {
  # get rid of white spaces and capitalization:
      names.index <- gsub("\\s","",tolower(chem.physical_and_invitro.data$Compound))
  # get rid of dashes:
      names.index <- gsub("\\-","",names.index)
  # get rid of white spaces and capitalization:
      name.key <- gsub("\\s","",tolower(this.chem.name))
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
  
    if (!is.null(this.dtxsid))
    {
  # get rid of white spaces:
      this.dtxsid.key <- gsub("\\s","",tolower(this.dtxsid))
      if (!(this.dtxsid.key %in% tolower(chem.physical_and_invitro.data$DTXSID))) 
        stop("this.dtxsid not found, use get_cheminfo(info=\"DTXISD\") for valid \
  this.dtxsids.\n")
  #Set the chemical name:
      found.chem.name <- as.character(na.omit(chem.physical_and_invitro.data[
        tolower(chem.physical_and_invitro.data[,"DTXSID"])==tolower(this.dtxsid.key),
        "Compound"]))
      found.chem.cas <- as.character(na.omit(chem.physical_and_invitro.data[
        tolower(chem.physical_and_invitro.data[,"DTXSID"])==tolower(this.dtxsid.key),
        "CAS"])) 
    }
  
    if (!is.null(found.chem.cas) & 
      !is.null(found.chem.name) & 
      !is.null(found.dtxsid))
    {
      if (!is.null(this.chem.cas)) 
      {
        if (!is.na(found.chem.cas)) 
        {
          if (this.chem.cas != found.chem.cas) stop(paste("Both CAS",
            this.chem.cas,"and either name or this.dtxsid were provided as arguments, but found 
            other CAS -- ",found.chem.cas))
        }
      } else if (!is.null(this.dtxsid)) 
      {
        if (!is.na(found.dtxsid))
        {
          if (this.dtxsid != found.dtxsid) stop(paste("Both this.dtxsid",
            this.dtxsid,"and either CAS or name were provided as arguments, but found 
            other this.dtxsid -- ",found.dtxsid))
        }
      } else if (!is.null(this.chem.name)) 
      {
        if (!is.na(found.chem.name))
        {
          if (tolower(this.chem.name) != tolower(found.chem.name)) stop(paste("Both name",
            this.chem.name,"and either CAS or this.dtxsid were provided as arguments, but found 
            other name -- ",found.chem.name))
        }
      }
    }
    
    if (is.null(this.chem.cas)) this.chem.cas <- found.chem.cas
    if (is.null(this.chem.name)) this.chem.name <- found.chem.name
    if (is.null(this.dtxsid)) this.dtxsid <- found.dtxsid
    
    chem.cas.out <- c(chem.cas.out, this.chem.cas)
    chem.name.out <- c(chem.name.out, this.chem.name)
    dtxsid.out <- c(dtxsid.out, this.dtxsid)
  }
  
  return(list(chem.cas=chem.cas.out,chem.name=chem.name.out,dtxsid=dtxsid.out)
 )
}
