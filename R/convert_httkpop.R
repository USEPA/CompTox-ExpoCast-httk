#'Converts HTTK-Pop physiologyn into parameters relevant to an HTTK
#'model.
#'
#' @param chem.cas Chemical Abstract Services Registry Number (CAS-RN) -- if
#'  parameters is not specified then the chemical must be identified by either
#'  CAS, name, or DTXISD
#' @param chem.name Chemical name (spaces and capitalization ignored) --  if
#'  parameters is not specified then the chemical must be identified by either
#'  CAS, name, or DTXISD
#' @param dtxsid EPA's 'DSSTox Structure ID (http://comptox.epa.gov/dashboard)  
#'  -- if parameters is not specified then the chemical must be identified by 
#' either CAS, name, or DTXSIDs
#' @param parameters Parameters from the appropriate parameterization function
#' for the model indicated by argument model
#'@param httk.pop.biomets A data.table containing the physiological
#'  parameterers predicted from biometric data by HTTK-pop 
#' '(from \code{\link{httkpop_bio}}) and
#'  \code{Funbound.plasma} and \code{Clint} values (from
#'  \code{\link{draw_fup_clint}}).
#'@param model Which HTTK model to use. One of '1compartment', '3compartmentss',
#'  '3compartment', or 'pbtk'.
#' @param adjusted.Funbound.plasma Uses adjusted Funbound.plasma when set to TRUE.
#' @param regression Whether or not to use the regressions in calculating partition 
#' coefficients.
#' @param well.stirred.correction Uses correction in calculation of hepatic clearance 
#' for well-stirred model if TRUE for hepatic.model well-stirred. This assumes 
#' clearance re/lative to amount unbound in whole blood instead of plasma, but 
#' converted to use with plasma concentration.
#' @param restrictive.clearance Protein binding not taken into account (set to 1) in 
#' liver clearance if FALSE.
#' @param concentration Blood, plasma, or tissue concentration. 
#' @param clint.pvalue.threshold Hepatic clearance for chemicals where the in vitro 
#' clearance assay result has a p-values greater than the threshold are set to zero.
#'
#'@return A data.table whose columns are the parameters of the HTTK model
#'  specified in \code{model}.
#'
#' @author Caroline Ring, John Wambaugh, and Greg Honda
#'
#'@references Ring, Caroline L., et al. "Identifying populations sensitive to 
#'environmental chemicals by simulating toxicokinetic variability." Environment 
#'International 106 (2017): 105-118
#'
#' @keyword httk-pop
#' @import utils
#' @export convert_httkpop
convert_httkpop <- function(
                     httk.pop.biomets,
                     model,
                     chem.cas=NULL,
                     chem.name=NULL,
                     dtxsid = NULL,
                     parameters=NULL,
                     ...
                     )
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
               
  # Start with the biometrics from httk-pop:             
  parameters.dt <- data.table::copy(httk.pop.biomets)

  #First convert to physiological parameters used by HTTK
  parameters.dt <- httkpop_biotophys_default(indiv_dt = parameters.dt)
  
# We need to describe the chemical to be simulated one way or another:
  if (is.null(chem.cas) & 
      is.null(chem.name) & 
      is.null(dtxsid) &
      is.null(parameters)) 
    stop('Parameters, chem.name, chem.cas, or dtxsid must be specified.')
  
# We need to know model-specific information (from modelinfo_[MODEL].R]) 
# to set up the solver:
  if (is.null(model)) stop("Model must be specified.")
  model <- tolower(model)
  if (!(model %in% names(model.list)))            
  {
    stop(paste("Model",model,"not available. Please select from:",
      paste(names(model.list),collapse=", ")))
  } else {
  #Depending on model, choose which parameters are not to be Monte Carlo sampled
    noMC.names <- model.list[[model]]$noMC.params
  }

  if (is.null(parameters))
  {
  #Depending on model, choose the function in HTTK that will return the default
  #HTTK parameters for this chemical
    paramfun <- model.list[[model]]$parameterize.func
    parameters <- do.call(getFromNamespace(paramfun, "httk"),
                    args=c(list(chem.cas=chem.cas,
                        chem.name=chem.name,
                        dtxsid=dtxsid),
                      ...))
  }

  #Assign the default values to the non-Monte Carlo parameters for all
  #individuals in the virtual population
  parameters.dt[, (noMC.names):=parameters[noMC.names]]



  # Force pKa to NA_real_ so data.table doesn't replace everything with text
  if(any(c("pKa_Donor","pKa_Accept") %in% names(parameters.dt))){
    suppressWarnings(parameters.dt[, c("pKa_Donor","pKa_Accept") := NULL]) %>% 
      .[, c("pKa_Donor","pKa_Accept") := NA_real_]
  }

  #Return only the HTTK parameters for the specified model. That is, only the
  #columns whose names are in the names of the default parameter set.
  parameters.dt<- parameters.dt[,
                            names(parameters.dt)[names(parameters.dt) %in% c('Rblood2plasma',names(parameters))],
                            with=FALSE]

  return(parameters.dt)
}
