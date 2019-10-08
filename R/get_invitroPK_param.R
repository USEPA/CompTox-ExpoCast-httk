#' Retrieve data from chem.physical_and_invitro.data table
#'
#; This function retrieves in vitro PK data (e.g. intrinsic metabolic clearance 
#' or fraction unbound in plasma) from the main HTTK data. This function looks
#' for species-specific values.
#'
#' @return The value of the parameter, if found
#'
#' @author John Wambaugh and Robert Pearce
#'
#' @import utils

get_invitroPK_param <- function(
                    param,
                    species,
                    chem.name=NULL,
                    chem.cas=NULL,
                    dtxsid=NULL)
{
  chem.physical_and_invitro.data <- chem.physical_and_invitro.data
  
# We need to describe the chemical to be simulated one way or another:
  if (is.null(chem.cas) & 
      is.null(chem.name) & 
      is.null(dtxsid) ) 
    stop('Cchem.name, chem.cas, or dtxsid must be specified.')

# Look up the chemical name/CAS, depending on what was provide:
  if (any(!is.null(chem.cas,chem.name,dtxsid)))
  {
    out <- get_chem_id(
            chem.cas=chem.cas,
            chem.name=chem.name,
            dtxsid=dtxsid)
    chem.cas <- out$chem.cas
    chem.name <- out$chem.name                                
    dtxsid <- out$dtxsid
  }

  chem.physical_and_invitro.data.index <- which(chem.physical_and_invitro.data$DTXSID==dtxsid)
  this.col.name <- tolower(paste(species,param,sep="."))
  if (!(this.col.name %in% tolower(colnames(chem.physical_and_invitro.data))))
  {
    warning(paste("No in vitro ",param," data for ",chem.name," in ",species,".",sep=""))
    for (alternate.species in c("Human","Rat","Mouse","Dog","Monkey","Rabbit"))
    {
      this.col.name <- tolower(paste(alternate.species,param,sep="."))
      if (this.col.name %in% tolower(colnames(chem.physical_and_invitro.data)))
      {
        warning(paste("Substituting ",alternate.species," in vitro ",
          param," data for ",chem.name," ",species,".",sep=""))
        break()
      }
    }
  }
  if (this.col.name %in% tolower(colnames(chem.physical_and_invitro.data)))
  {
    this.col.index <- which(tolower(colnames(chem.physical_and_invitro.data))==this.col.name)
    param.val <- chem.physical_and_invitro.data[chem.physical_and_invitro.data.index,this.col.index]
    if (param=="Clint" & (nchar(param.val) -
          nchar(gsub(",","",param.val)))==3) return(param.val)
    else if (param=="Funbound.plasma" & (nchar(param.val) -
          nchar(gsub(",","",param.val)))==2) return(param.val)
    else if (param=="Clint.pValue") return(param.val)
     else if (!is.na(as.numeric(param.val))) return(as.numeric(param.val))
  }
  stop(paste("Incomplete in vitro PK data for ",chem.name,
    " in ",species," -- missing ",param,".",sep=""))
}