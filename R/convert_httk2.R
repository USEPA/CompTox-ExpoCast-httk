#'Converts HTTK-Pop virtual population into parameters relevant to an HTTK
#'model.
#'
#'@param indiv.model.bio A data.table containing the physiological
#'  parameters as expected by HTTK (from \code{\link{httkpop_bio}}) and
#'  \code{Funbound.plasma} and \code{Clint} values (from
#'  \code{\link{draw_fup_clint}}).
#'@param model Which HTTK model to use. One of '1compartment', '3compartmentss',
#'  '3compartment', or 'pbtk'.
#'@param this.chem CAS number for the chemical in the HTTK data set (see 
#'  \code{\link[httk]{get_cheminfo}}) for which
#'  parameters are to be generated.
#'@param parameters A list of chemical-specific model parameters containing at least
#' Funbound.plasma, Clint, and Fhep.assay.correction. 
#' @param adjusted.Funbound.plasma Uses adjusted Funbound.plasma when set to TRUE.
#' @param regression Whether or not to use the regressions in calculating partition 
#' coefficients.
#' @param well.stirred.correction Uses correction in calculation of hepatic clearance 
#' for well-stirred model if TRUE for hepatic.model well-stirred. This assumes 
#' clearance relative to amount unbound in whole blood instead of plasma, but 
#' converted to use with plasma concentration.
#' @param restrictive.clearance Protein binding not taken into account (set to 1) in 
#' liver clearance if FALSE.
#' @param clint.pvalue.threshold Hepatic clearance for chemicals where the in vitro 
#' clearance assay result has a p-values greater than the threshold are set to zero.
##'
#'@return A data.table whose columns are the parameters of the HTTK model
#'  specified in \code{model}.
#'
#' @author Caroline Ring, John Wambaugh, and Greg Honda
convert_httk2 <- function(indiv.model.bio,
                         model,
                         this.chem=NULL,
                         parameters=NULL,
                         adjusted.Funbound.plasma=T,
                         regression=T,
                         well.stirred.correction=T,
                         restrictive.clearance=T,
                         clint.pvalue.threshold=0.05,
                         ...)
{
  #R CMD CHECK throws notes about "no visible binding for global variable", for
  #each time a data.table column name is used without quotes. To appease R CMD
  #CHECK, a variable has to be created for each of these column names and set to
  #NULL. Note that within the data.table, these variables will not be NULL! Yes,
  #this is pointless and annoying.
  Funbound.plasma <- Vrestc <- Qrestf <- Clint <- NULL
  Fhep.assay.correction <- million.cells.per.gliver <- NULL
  BW <- Vliverc <- Qtotal.liverc <- Clmetabolismc <- RBC.vol <- NULL
  plasma.vol <- hematocrit <- Vdist <- Qgfrc <- liver.density <- NULL
  kelim <- Rblood2plasma <- Krbc2pu <- NULL
  Qliver<-Qcardiacc<-Qgutf<-Qliverf<-hepatic.bioavailability<-NULL
  #End R CMD CHECK appeasement.


# Makes sure function call is properly formated:
  if (is.null(indiv.model.bio))
  {
    stop("indiv.model.bio (physiological parameters from httkpop_bio) must be specified.")
  }
  
# Makes sure function call is properly formated:
  if (model %in% names(model.list))            
  {
    indiv.model <- do.call(model.list[[model]]$convert.httkpop.func,c(list(
      indiv.model.bio = indiv.model.bio,
      this.chem=this.chem,
      parameters=parameters),
      list(...)))
  } else {
    stop(paste("Model",model,"not available. Please select from:",
      paste(names(model.list),collapse=", ")))
  }

  
  return(indiv.model)
}
