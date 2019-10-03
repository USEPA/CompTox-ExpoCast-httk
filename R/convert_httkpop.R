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
  
# List all tissues for which HTTK has human tissue information. 
# This will be used in lumping.  
  tissuenames <- sort(unique(subset(httk::tissue.data,Species=="Human")$Tissue))
# We don't use these tissues for lumping:
  tissuenames <- tissuenames[!(tissuenames %in% c("red blood cells"))]

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


# For models with tissue-to-plasma partition coefficients we neeed to calculate
# them for each individual because each individual has a different 
# Funbound.plasma value:
  if (model.table[model]$ComputePCforMC)
  {

    #First, get the default parameters used for the Schmitt method of estimating
    #partition coefficients.
    if (!is.null(this.chem))
    {
      pschmitt <- httk::parameterize_schmitt(chem.cas=this.chem,
                                           species='Human')
    } else {
      pschmitt <- parameters[param.names.schmitt[param.names.schmitt%in%names(parameters)]]
      pschmitt.chemindependent <- httk::parameterize_schmitt(chem.cas="80-05-7",species="Human")
      pschmitt <- c(pschmitt,pschmitt.chemindependent[c("Fprotein.plasma","plasma.pH","alpha")])
#      pschmitt[["MA"]] <- NA
    }
    #next, replace the single default value for Funbound.plasma with the vector
    #of Funbound.plasma values from the virtual population data.table.
    pschmitt$Funbound.plasma<-parameters.dt[, Funbound.plasma]

    #Now, predict the partitioning coefficients using Schmitt's method. The
    #result will be a list of numerical vectors, one vector for each
    #tissue-to-plasma partitioning coefficient, and one element of each vector
    #for each individual. The list element names specify which partition
    #coefficient it is, e.g. Kliver2plasma, Kgut2plasma, etc.
    PCs <- httk::predict_partitioning_schmitt(parameters=pschmitt,
                                              chem.cas=this.chem,
                                              species='Human',
                                              adjusted.Funbound.plasma=adjusted.Funbound.plasma,
                                              regression=regression)

    #Depending on model, get the list of compartments.
    #All other tissues will be lumped into a "rest" compartment.
    tissue.list <- switch(model,
                          #1 compartment model lumps everything, so list of
                          #compartments is empty.
                          '1compartment'=vector(mode='character',
                                                length=0),
                          #3 compartment model has only liver and gut
                          #compartments; everything else is lumped.
                          '3compartment'=c('liver',
                                           'gut'),
                          #PBTK model has liver, kidney, gut, and lung
                          #compartments; everything else is lumped. To do: let
                          #user specify PBTK compartment list (but only useful
                          #after HTTK can do something with that)
                          'pbtk'=c('liver',
                                   'kidney',
                                   'lung',
                                   'gut'))

    #Now get the list of tissues to be lumped: that is, everything in
    #tissuenames that wasn't in the list of compartments for this model.
    rest.tissues <- tissuenames[!(tissuenames %in%
                                    c(tissue.list,
                                      'red blood cells'))]
    #Lump the volumes by simply summing them.
    vol.restc <- parameters.dt[,
                             Reduce('+', .SD),
                             .SDcols=paste0('V',
                                            rest.tissues,
                                            'c')]
    #Lump the flows by summing them.
    flow.restf <- parameters.dt[,
                              Reduce('+', .SD),
                              .SDcols=paste0('Q',
                                             rest.tissues,
                                             'f')]
    #Lumped partition coefficient: sum partition coefficients of rest.tissues,
    #weighted by their volumes; then divide by total lumped volume.
    Krest2pu <- Reduce('+',
                           lapply(as.list(rest.tissues),
                                  function(x) PCs[[paste0('K',
                                                          x,
                                                          '2pu')]]*
                                    unlist(parameters.dt[,
                                                       paste0('V',
                                                              x,
                                                              'c'),
                                                       with=FALSE])
                           )
    )/vol.restc

    #Add lumped volumes, flows, and partition coefficients to population
    #data.table
    parameters.dt[, Vrestc:=vol.restc]
    parameters.dt[, Qrestf:=flow.restf]
    parameters.dt[, Krest2pu:=Krest2pu]
    parameters.dt[, Krbc2pu:=PCs[['Krbc2pu']]]

    if (!(length(tissue.list)==0)){
      #For enumerated tissue compartments (if any), add their partitition
      #coefficients to the population data.table as well.

      #First get the vector of partition coefficient names
      #(names of elements of PCs)
      knames <- paste0('K',
                       tissue.list,
                       '2pu')
      #Then add them to the population data.table. data.table syntax: wrap
      #vector of column names in parentheses to assign to multiple columns at
      #once
      parameters.dt[, (knames):=PCs[knames]]
    }

    if (!"Rblood2plasma" %in% colnames(parameters.dt))
    {
      #For 1 compartment, 3 compartment, or PBTK models: Calculate Rblood2plasma
      #based on hematocrit and Krbc2plasma. This is the ratio of chemical in blood
      #vs. in plasma.
      if (is.null(parameters))
      {
        Rblood2plasma <- get_rblood2plasma(chem.cas=this.chem,species='Human')
      } else {
        Rblood2plasma <- calc_rblood2plasma(params=pschmitt,species="Human")
      }
      parameters.dt[,Rblood2plasma:=Rblood2plasma]
    }
    parameters.dt[is.na(Rblood2plasma),
                Rblood2plasma:=(1-
                                  hematocrit +
                                  hematocrit*
                                  Krbc2pu*
                                  Funbound.plasma)]
  }

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
