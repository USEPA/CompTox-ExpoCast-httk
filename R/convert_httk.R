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
#' @param concentration Blood, plasma, or tissue concentration. 
#' @param clint.pvalue.threshold Hepatic clearance for chemicals where the in vitro 
#' clearance assay result has a p-values greater than the threshold are set to zero.
#'
#'@return A data.table whose columns are the parameters of the HTTK model
#'  specified in \code{model}.
#'
#' @author Caroline Ring, John Wambaugh, and Greg Honda
#' @import utils
#' @export convert_httk
convert_httk <- function(indiv.model.bio,
                         model,
                         this.chem=NULL,
                         parameters=NULL,
                         adjusted.Funbound.plasma=T,
                         regression=T,
                         well.stirred.correction=T,
                         restrictive.clearance=T,
                         concentration = "plasma",
                         clint.pvalue.threshold=0.05){
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

  indiv.model <- data.table::copy(indiv.model.bio)

  #List all tissues that HTTK has tissue information for. This will be used in
  #lumping.
  tissuenames <- c('adipose',
                   'bone',
                   'brain',
                   'gut',
                   'heart',
                   'kidney',
                   'liver',
                   'lung',
                   'muscle',
                   'skin',
                   'spleen',
                   'rest')

  if (is.null(parameters))
  {
  #Depending on model, choose the function in HTTK that will return the default
  #HTTK parameters for this chemical
  paramfun <- switch(model,
                            '1compartment'='parameterize_1comp',
                            '3compartment'='parameterize_3comp',
                            'pbtk'='parameterize_pbtk',
                            '3compartmentss'='parameterize_steadystate')
  #And get the default HTTK parameters. These values will be used for all
  #parameters not being Monte Carlo sampled
  if(paramfun == 'parameterize_steadystate'){
    p <- parameterize_steadystate(chem.cas=this.chem,species='Human',adjusted.Funbound.plasma=adjusted.Funbound.plasma,restrictive.clearance=restrictive.clearance,clint.pvalue.threshold=clint.pvalue.threshold)
    if(concentration == "tissue"){
      p <- add_schmitt.param_to_3compss(parameters = p, chem.cas = this.chem)
    }
  } else if(paramfun == 'parameterize_1comp') p <- parameterize_1comp(chem.cas=this.chem,species='Human',adjusted.Funbound.plasma=adjusted.Funbound.plasma,regression=regression,restrictive.clearance=restrictive.clearance,clint.pvalue.threshold=clint.pvalue.threshold)
  else{
  p <- do.call(getFromNamespace(paramfun, "httk"),
               args=list(chem.cas=this.chem,
                         species='Human',adjusted.Funbound.plasma=adjusted.Funbound.plasma,regression=regression))
  }
  } else p <- parameters
  #Depending on model, choose which parameters are not to be Monte Carlo sampled
  noMC.names <- switch(model,
                       '1compartment'=c('kgutabs',
                                        'MW',
                                        'Pow',
                                        "MA",
                                        'pKa_Donor',
                                        'pKa_Accept',
                                        'Fgutabs',
                                        "Fhep.assay.correction",
                                        "Funbound.plasma.adjustment"),
                       '3compartment'=c('MW',
                                        'Pow',
                                        "MA",
                                        'pKa_Donor',
                                        'pKa_Accept',
                                        "Fhep.assay.correction",
                                        "Funbound.plasma.adjustment",
                                        'Fgutabs',
                                        'kgutabs'),
                       'pbtk'=c('kgutabs',
                                'MW',
                                'Pow',
                                "MA",
                                'pKa_Donor',
                                'pKa_Accept',
                                "Fhep.assay.correction",
                                "Funbound.plasma.adjustment",
                                'Fgutabs'),
                       '3compartmentss'=c("Dow74",
                                          'MW',
                                          'Fgutabs',
                                          "Fhep.assay.correction",
                                          "Funbound.plasma.adjustment"))
  
  if(model == '3compartmentss' & concentration == "tissue"){
    noMC.names <- c(noMC.names, "MA","Pow","pKa_Donor", "pKa_Accept")
  }
  #Assign the default values to the non-Monte Carlo parameters for all
  #individuals in the virtual population
  indiv.model[, (noMC.names):=p[noMC.names]]

# Use something like this for 1.10:
#  # Use optim to estimate alpha and beta such that the median and 95% credible interval approximate the estimate from MCMC:
#  ppb.fit <- optim(c(2,2), function(x) (0.95-pbeta(ppb.high95,x[1],x[2])+pbeta(ppb.low95,x[1],x[2]))^2+(ppb.median-qbeta(0.5,x[1],x[2]))^2)
## We are drawing new values for the unadjusted Fup:
#  pop_u_httk[, unadjusted.Funbound.plasma:=rbeta(1000,ppb.fit$par[1],ppb.fit$par[2])]
## then we need to adjust:
#  pop_u_httk[,Flipid:=subset(physiology.data,Parameter=='Plasma Effective Neutral Lipid Volume Fraction')[,which(colnames(physiology.data) == 'Human')]]
#  pop_u_httk[,Funbound.plasma.adjustment:=1 / (Dow74 * Flipid + 1 / unadjusted.Funbound.plasma)/unadjusted.Funbound.plasma]


  if (model != '3compartmentss')
  {
    #For 1 compartment, 3 compartment, or PBTK models, need to compute the
    #tissue-to-plasma partition coefficients for each individual, because each
    #individual has a different Funbound.plasma value.

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
    pschmitt$Funbound.plasma<-indiv.model[, Funbound.plasma]

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
    vol.restc <- indiv.model[,
                             Reduce('+', .SD),
                             .SDcols=paste0('V',
                                            rest.tissues,
                                            'c')]
    #Lump the flows by summing them.
    flow.restf <- indiv.model[,
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
                                    unlist(indiv.model[,
                                                       paste0('V',
                                                              x,
                                                              'c'),
                                                       with=FALSE])
                           )
    )/vol.restc

    #Add lumped volumes, flows, and partition coefficients to population
    #data.table
    indiv.model[, Vrestc:=vol.restc]
    indiv.model[, Qrestf:=flow.restf]
    indiv.model[, Krest2pu:=Krest2pu]
    indiv.model[, Krbc2pu:=PCs[['Krbc2pu']]]

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
      indiv.model[, (knames):=PCs[knames]]
    }

    if (!"Rblood2plasma" %in% colnames(indiv.model))
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
      indiv.model[,Rblood2plasma:=Rblood2plasma]
    }
    indiv.model[is.na(Rblood2plasma),
                Rblood2plasma:=(1-
                                  hematocrit +
                                  hematocrit*
                                  Krbc2pu*
                                  Funbound.plasma)]
    
#    if (model!='1compartment'){
      #Need to compute hepatic clearance,
      #Clmetabolismc. Computed from Clint and hepatocellularity. Convert Clint
      #to Clmetabolismc using unscaled model (unscaled means that it's unscaled
      #by weight, i.e. L/h/kg bodyweight).

      #First, construct a list of the parameters needed by HTTK to compute total
      #hepatic clearance. each list element will be a vector of individual
      #values, except liver.density and Dn, which are fixed default values.
      calc_hep_params <- c(as.list(indiv.model[, list(Clint,
                                                   Funbound.plasma,
                                                   Fhep.assay.correction,
                                                   million.cells.per.gliver,
                                                   BW,
                                                   Vliverc,
                                                   Qtotal.liverc)]),
                           liver.density=1.05,
                           Dn=0.17)

      #Call HTTK function to compute total hepatic clearance, using unscaled
      #hepatic model.
      indiv.model[,
                  Clmetabolismc:=httk::calc_hepatic_clearance(hepatic.model="unscaled",
                                                              parameters=calc_hep_params,
                                                              suppress.messages=TRUE,
                                                              clint.pvalue.threshold=clint.pvalue.threshold)]


#    }else
    if (model=='1compartment'){
      #for 1-compartment model, don't need to compute total hepatic clearance,
      #but do need to compute volume of distribution and elimination rate.

      #HTTK contains a function to compute volume of distribution, but it pulls
      #Funbound.plasma from its table of default values, meaning we can't give
      #that function our vector of individual Funbound.plasma values. So
      #instead, I've re-implemented the Vdist equation here.

      #To compute volume of distribution, need to get volume of red blood cells.
      #Can compute that from plasma volume and hematocrit.

      indiv.model[, RBC.vol:=plasma.vol/
                    (1 - hematocrit)*
                    hematocrit]
      #Compute Vdist, volume of distribution
      indiv.model[,Vdist:=plasma.vol +
                    RBC.vol*
                    PCs[["Krbc2pu"]]*
                    Funbound.plasma+
                    Krest2pu*
                    vol.restc*
                    Funbound.plasma]
      #Compute kelim: Elimination rate, units of 1/h. First make a list of the
      #parameters that HTTK uses to calculate kelim. Each list element will be a
      #vector of the values for each individual.
      calc_elim_params <- c(as.list(indiv.model[,
                                                list(Vdist,
                                                  Clint,
                                                  Funbound.plasma,
                                                  Qtotal.liverc,
                                                  Qgfrc,
                                                  BW,
                                                  million.cells.per.gliver,
                                                  Rblood2plasma,
                                                  Vliverc,
                                                  Fhep.assay.correction,
                                                  liver.density)]))
      #Call HTTK function to calculate total elimination rate. This one is OK
      #because it uses the vector of Funbound.plasma that we give it.
      ke <- httk::calc_elimination_rate(parameters=calc_elim_params,
                                        chem.cas=this.chem,
                                        suppress.messages=TRUE,
                                        adjusted.Funbound.plasma=adjusted.Funbound.plasma,regression=regression,
                                        well.stirred.correction=well.stirred.correction,
                                        restrictive.clearance=restrictive.clearance,
                                        clint.pvalue.threshold=clint.pvalue.threshold)
      #Add kelim to the population data.table.
      indiv.model[, kelim:=ke]
    }
  } else {
    calc_hep_params <- c(as.list(indiv.model[, list(Clint,
                                                    Funbound.plasma,
                                                    Fhep.assay.correction,
                                                    million.cells.per.gliver,
                                                    BW,
                                                    Vliverc,
                                                    Qtotal.liverc)]),
                         liver.density=1.05,
                         Dn=0.17)
    indiv.model[,Rblood2plasma:=available_rblood2plasma(chem.cas=this.chem,
      species='Human',
      adjusted.Funbound.plasma=adjusted.Funbound.plasma)]
      
    indiv.model[,
                Clmetabolismc:=httk::calc_hepatic_clearance(hepatic.model="unscaled",
                                                            parameters=calc_hep_params,
                                                            suppress.messages=TRUE,
                                                            clint.pvalue.threshold=clint.pvalue.threshold)]
    

  }
  # For models that don't described first pass blood flow from the gut, need to
  # cacluate a hepatic bioavailability (Rowland, 2009):
  if (model %in% c('1compartment', '3compartmentss'))
  {
    indiv.model[, Qliver:=Qcardiacc*(Qgutf+Qliverf)*BW^0.75] # L/h
    indiv.model[, hepatic.bioavailability:= Qliver / (Qliver + Funbound.plasma * Clmetabolismc*BW / Rblood2plasma)]
  }

  # Force pKa to NA_real_ so data.table doesn't replace everything with text
  if(any(c("pKa_Donor","pKa_Accept") %in% names(indiv.model))){
    suppressWarnings(indiv.model[, c("pKa_Donor","pKa_Accept") := NULL]) %>% 
      .[, c("pKa_Donor","pKa_Accept") := NA_real_]
  }

  #Return only the HTTK parameters for the specified model. That is, only the
  #columns whose names are in the names of the default parameter set.
  indiv.model<- indiv.model[,
                            names(indiv.model)[names(indiv.model) %in% c('Rblood2plasma',names(p))],
                            with=FALSE]

  return(indiv.model)
}
