#' Retrieve species-specific in vitro data from chem.physical_and_invitro.data table
#'
#' This function retrieves in vitro PK data (for example, intrinsic metabolic clearance 
#' or fraction unbound in plasma) for the the chemical specified by argument "chem.name", "dtxsid", 
#' or chem.cas from the table \code{\link{chem.physical_and_invitro.data}}.
#' This function looks for species-specific values based on the argument "species".
#'
#' @details 
#' Note that this function works with a local version of the 
#' \code{\link{chem.physical_and_invitro.data}} table to allow users to 
#' add/modify chemical
#' data (for example, adding new data via \code{\link{add_chemtable}} or 
#' loading in silico predictions distributed with httk via
#' \code{\link{load_sipes2017}}, \code{\link{load_pradeep2020}},
#' \code{\link{load_dawson2021}}, or \code{\link{load_honda2023}}).
#' 
#' User can request via argument param (case-insensitive):
#' \tabular{lll}{
#' \strong{Parameter} \tab \strong{Description} \tab \strong{Units} \cr
#'  [SPECIES].Clint \tab  (Primary hepatocyte suspension) 
#' intrinsic hepatic clearance. \emph{Entries with comma separated values are Bayesian estimates of
#' the Clint distribution - displayed as the median, 95th credible interval
#' (that is quantile 2.5 and 97.5, respectively), and p-value.} \tab  uL/min/10^6 hepatocytes \cr                   
#'  [SPECIES].Clint.pValue \tab  Probability that there is no clearance observed.
#'  Values close to 1 indicate clearance is not statistically significant. \tab  none \cr       
#'  [SPECIES].Caco2.Pab \tab  Caco-2 Apical-to-Basal Membrane Permeability \tab  10^-6 cm/s \cr            
#'  [SPECIES].Fabs \tab  In vivo measured fraction of an oral dose of chemical 
#' absorbed from the gut lumen into the gut \tab  unitless fraction \cr            
#'  [SPECIES].Fgut \tab  In vivo measured fraction of an oral dose of chemical 
#' that passes gut metabolism and clearance \tab  unitless fraction \cr          
#'  [SPECIES].Foral \tab  In vivo measued fractional systemic bioavailability of 
#' an oral dose, modeled as he product of Fabs * Fgut * Fhep (where Fhep is 
#' first pass hepatic metabolism). \tab  unitless fraction \cr
#'  [SPECIES].Funbound.plasma \tab  Chemical fraction unbound in presence of 
#' plasma proteins (fup). \emph{Entries with comma separated values are Bayesian estimates of
#' the fup distribution - displayed as the median and 95th credible interval
#' (that is quantile 2.5 and 97.5, respectively).} \tab  unitless fraction \cr
#'  [SPECIES].Rblood2plasma \tab  Chemical concentration blood to plasma ratio \tab  unitless ratio \cr       
#' }
#' 
#' @param param The desired parameters, a vector or single value.
#' 
#' @param chem.name The chemical names that you want parameters for, a vector or single value
#' 
#' @param chem.cas The chemical CAS numbers that you want parameters for, a vector or single value
#' 
#' @param dtxsid EPA's 'DSSTox Structure ID (https://comptox.epa.gov/dashboard)  
#' 
#' @param species Species desired (either "Rat", "Rabbit", "Dog", "Mouse", or
#' default "Human"). 
#' 
#' @seealso \code{\link{chem.physical_and_invitro.data}} 
#' @seealso \code{\link{get_invitroPK_param}} 
#' @seealso \code{\link{add_chemtable}} 
#'
#' @return The parameters, either a single value, a named list for a single chemical, or a list of lists
#' 
#' @author John Wambaugh and Robert Pearce
#'
#' @import utils
#' @export get_invitroPK_param 
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
    stop('Chem.name, chem.cas, or dtxsid must be specified.')

# Look up the chemical name/CAS, depending on what was provide:
  if (any(is.null(chem.cas),is.null(chem.name),is.null(dtxsid)))
  {
    out <- get_chem_id(
            chem.cas=chem.cas,
            chem.name=chem.name,
            dtxsid=dtxsid)
    chem.cas <- out$chem.cas
    chem.name <- out$chem.name                                
    dtxsid <- out$dtxsid
  }

  if (length(dtxsid)!=0) chem.physical_and_invitro.data.index <- 
    which(chem.physical_and_invitro.data$DTXSID == dtxsid)
  else if (length(chem.cas)!=0) chem.physical_and_invitro.data.index <- 
    which(chem.physical_and_invitro.data$CAS == chem.cas)
  else chem.physical_and_invitro.data.index <- 
    which(chem.physical_and_invitro.data$Compound == chem.name)

  this.col.name <- tolower(paste(species,param,sep="."))
#  if (!(this.col.name %in% tolower(colnames(chem.physical_and_invitro.data))))
#  {
#    warning(paste("No in vitro ",param," data for ",chem.name," in ",species,".",sep=""))
#    for (alternate.species in c("Human","Rat","Mouse","Dog","Monkey","Rabbit"))
#    {
#      this.col.name <- tolower(paste(alternate.species,param,sep="."))
#      if (this.col.name %in% tolower(colnames(chem.physical_and_invitro.data)))
#      {
#        warning(paste("Substituting ",alternate.species," in vitro ",
#          param," data for ",chem.name," ",species,".",sep=""))
#        break()
#      }
#    }
 # }
  if (this.col.name %in% tolower(colnames(chem.physical_and_invitro.data)))
  {
    this.col.index <- which(tolower(colnames(chem.physical_and_invitro.data))==this.col.name)
    param.val <- chem.physical_and_invitro.data[chem.physical_and_invitro.data.index,this.col.index]

    if (is.na(param.val))
    {
      stop(param," does not currently exist for ",species," in the `chem.physical_and_invitro.data`.")
    } else if(param=="Clint" & (nchar(param.val) - nchar(gsub(",","",param.val)))==3){
      return(param.val)
    } else if (param %in% c("Caco2.Pab","Funbound.plasma") & (nchar(param.val) - nchar(gsub(",","",param.val)))==2){
      return(param.val)
    } else if (param=="Clint.pValue"){
      return(param.val)
    } else if (!is.na(as.numeric(param.val))){
      return(as.numeric(param.val))
    }
  }
  stop(paste("Incomplete in vitro PK data for ",chem.name,
    " in ",species," -- missing ",param,".",sep=""))
}

