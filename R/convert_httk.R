#'Converts HTTK-Pop virtual population into parameters relevant to an HTTK 
#'model.
#'
#'@export
#'
#'@param indiv.model.bio A data.table containing the physiological
#'  parameters as expected by HTTK (from \code{\link{httkpop_bio}}) and
#'  \code{Funbound.plasma} and \code{Clint} values (from
#'  \code{\link{draw_fup_clint}}).
#'@param model Which HTTK model to use. One of '1compartment', '3compartmentss',
#'  '3compartment', or 'pbtk'.
#'@param this.chem CAS number for the chemical in the HTTK data set for which 
#'  parameters are to be generated.
#'  
#'@return A data.table whose columns are the parameters of the HTTK model 
#'  specified in \code{model}.

convert_httk <- function(indiv.model.bio, 
                         model,
                         this.chem,
                         adjusted.Funbound.plasma=T,
                         regression=T,
                         well.stirred.correction=T,
                         restrictive.clearance=T){
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
  
  #Depending on model, choose the function in HTTK that will return the default
  #HTTK parameters for this chemical
  paramfun <- switch(model,
                            '1compartment'='parameterize_1comp',
                            '3compartment'='parameterize_3comp',
                            'pbtk'='parameterize_pbtk',
                            '3compartmentss'='parameterize_steadystate')
  #And get the default HTTK parameters. These values will be used for all
  #parameters not being Monte Carlo sampled
  if(paramfun == 'parameterize_steadystate') p <- parameterize_steadystate(chem.cas=this.chem,species='Human',adjusted.Funbound.plasma=adjusted.Funbound.plasma,restrictive.clearance=restrictive.clearance)
  else if(paramfun == 'parameterize_1comp') p <- parameterize_1comp(chem.cas=this.chem,species='Human',adjusted.Funbound.plasma=adjusted.Funbound.plasma,regression=regression,restrictive.clearance=restrictive.clearance)
  else{
  p <- do.call(getFromNamespace(paramfun, "httk"),
               args=list(chem.cas=this.chem, 
                         species='Human',adjusted.Funbound.plasma=adjusted.Funbound.plasma,regression=regression))
  }
  #Depending on model, choose which parameters are not to be Monte Carlo sampled
  noMC.names <- switch(model,
                       '1compartment'=c('kgutabs',
                                        'MW',
                                        'Fgutabs',
                                        'hepatic.bioavailability'),
                       '3compartment'=c('MW',
                                        'Fgutabs',
                                        'kgutabs'),
                       'pbtk'=c('kgutabs',
                                'MW',
                                'Fgutabs'),
                       '3compartmentss'=c('MW',
                                          'Fgutabs',
                                          'hepatic.bioavailability'))
  #Assign the default values to the non-Monte Carlo parameters for all
  #individuals in the virtual population
  indiv.model[, (noMC.names):=p[noMC.names]]
  
  if (model != '3compartmentss'){
    #For 1 compartment, 3 compartment, or PBTK models, need to compute the
    #tissue-to-plasma partition coefficients for each individual, because each
    #individual has a different Funbound.plasma value.
    
    #First, get the default parameters used for the Schmitt method of estimating
    #partition coefficients.
    pschmitt <- httk::parameterize_schmitt(chem.cas=this.chem,
                                           species='Human')
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
    
    if (model!='1compartment'){
      #For 3 compartment or PBTK models, need to compute hepatic clearance,
      #CLmetabolismc. Computed from Clint and hepatocellularity. Convert Clint
      #to CLmetabolismc using unscaled model.
      
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
                                                              suppress.messages=TRUE)]
      
      
    }else if (model=='1compartment'){ 
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
                                        restrictive.clearance=restrictive.clearance)
      #Add kelim to the population data.table.
      indiv.model[, kelim:=ke]
    }
    #For 1 compartment, 3 compartment, or PBTK models: Calculate Rblood2plasma
    #based on hematocrit and Krbc2plasma. This is the ratio of chemical in blood
    #vs. in plasma.
    Rblood2plasma <- get_rblood2plasma(chem.cas=this.chem,species='Human')
    if(!is.na(Rblood2plasma)){
      indiv.model[,Rblood2plasma:=Rblood2plasma]    
      warning('Human in vivo Rblood2plasma substituted.')
    }else{ indiv.model[, 
                Rblood2plasma:=(1-
                                  hematocrit + 
                                  hematocrit*
                                  Krbc2pu*
                                  Funbound.plasma)]
    }
  }else indiv.model[,Rblood2plasma:=available_rblood2plasma(chem.cas=this.chem,species='Human',adjusted.Funbound.plasma=adjusted.Funbound.plasma)]
    

  #Return only the HTTK parameters for the specified model. That is, only the
  #columns whose names are in the names of the default parameter set.
  indiv.model<- indiv.model[, 
                            names(indiv.model)[names(indiv.model) %in% c('Rblood2plasma',names(p))], 
                            with=FALSE]
  
  return(indiv.model)
}