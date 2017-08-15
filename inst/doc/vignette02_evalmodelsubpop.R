## ---- include=FALSE------------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = '#>')

## ----load_packages-------------------------------------------------------
library(httk)
library(data.table)
library(EnvStats)

## ----get_chemcas, eval=FALSE---------------------------------------------
#  chemlist <- httk::get_cheminfo(info='CAS', exclude.fub.zero=FALSE)

## ----doforeachchem, eval=FALSE-------------------------------------------
#  doforeachchem <- function(this.chemcas,
#                            model,
#                            species,
#                            sigma.factor,
#                            css.method,
#                            indiv.model.bio,
#                            poormetab,
#                            fup.censor,
#                            ExpoCast.group,
#                            nsamp,
#                            Clint.vary){
#  
#    indiv.model.bio <- data.table::copy(indiv.model.bio)
#    #Convert to HTTK model params
#    if (ExpoCast.group=="indepMC"){
#      indiv.model.tmp <- cbind(indiv.model.bio,
#                           httk::draw_fup_clint(this.chem=this.chemcas,
#                                                   nsamp=nrow(indiv.model.bio),
#                                                   poormetab=poormetab,
#                                                   fup.censor=fup.censor))
#      indiv.model <- httk::convert_httk(indiv.model.bio=indiv.model.tmp,
#                                           model=model,
#                                           this.chem=this.chemcas)
#    }else{
#    indiv.model <- httk::get_httk_params(indiv_dt=indiv.model.bio,
#                                         model=model,
#                                         chemcas=this.chemcas,
#                                         poormetab=poormetab,
#                                         fup.censor=fup.censor,
#                                         Clint.vary=Clint.vary)
#    }
#  
#    #If model is 3compartmentss, convert Funbound.plasma to Funbound.blood
#    if (model=="3compartmentss"){
#    #First, get the default parameters used for the Schmitt method of estimating
#      #partition coefficients.
#      pschmitt <- httk::parameterize_schmitt(chem.cas=this.chemcas,
#                                             species='Human')
#      #next, replace the single default value for Funbound.plasma with the vector
#      #of Funbound.plasma values from the virtual population data.table.
#      pschmitt$Funbound.plasma<-indiv.model[, Funbound.plasma]
#  
#      #Now, predict the partitioning coefficients using Schmitt's method. The
#      #result will be a list of numerical vectors, one vector for each
#      #tissue-to-plasma partitioning coefficient, and one element of each vector
#      #for each individual. The list element names specify which partition
#      #coefficient it is, e.g. Kliver2plasma, Kgut2plasma, etc.
#      PCs <- httk::predict_partitioning_schmitt(parameters=pschmitt,
#                                                chem.cas=this.chemcas,
#                                                species='Human')
#      Rb2p <- 1 - indiv.model.bio$hematocrit + indiv.model.bio$hematocrit *
#        PCs[["Krbc2pu"]] *
#        indiv.model$Funbound.plasma
#  
#    indiv.model[, Funbound.plasma:=Funbound.plasma/Rb2p]
#    }
#  
#    #Evaluate model
#    if (tolower(css.method)=='analytic') {
#      #Css
#      css <- httk::calc_analytic_css(chem.cas=this.chemcas,
#                                     parameters=indiv.model,
#                                     daily.dose=1,
#                                     output.units="uM",
#                                     model=model,
#                                     species=species,
#                                     suppress.messages=TRUE,
#                                     recalc.blood2plasma=TRUE)
#  
#      if (model=="3compartmentss"){ #convert from Css.blood back to Css.plasma
#        css <- css/Rb2p
#      }
#      }
#    else if (tolower(css.method)=='full'){
#      #Css
#      css <- apply(X=indiv.model,
#                   MARGIN=1,
#                   FUN=function(x) httk::calc_css(chem.cas=this.chemcas,
#                                                  parameters=as.list(x),
#                                                  daily.dose=1,
#                                                  output.units="uM",
#                                                  model=model,
#                                                  species=species,
#                                                  suppress.messages=TRUE)[['avg']])
#      }
#    #Compute percentiles
#    prob.vect <- c(0.01, 0.05,0.1,0.25,0.5,0.75,0.9,0.95, 0.99)
#    css.q <- quantile(css, probs=prob.vect)
#    #Function to compute lower and upper CI bounds
#    tmpfun <- function(x,z){
#      tmp <- tryCatch(EnvStats::eqnpar(x,
#                                       p=z,
#                                       ci=TRUE,
#                                       lb=0)$interval[['limits']],
#                      error=function(err){
#                        return(c(LCL='NA', UCL='NA'))
#                        })
#      return(tmp)
#      }
#    #Compute Css CI bounds
#    css.cl <- sapply(prob.vect, function(z) tmpfun(z, x=css)
#                     )
#    #Construct list to return
#    dat.chem.out <- c(as.list(css.q),
#                         as.list(css.cl['LCL',]),
#                         as.list(css.cl['UCL',]),
#                         as.list(var(css)),
#                         as.list(this.chemcas),
#                         as.list(ExpoCast.group))
#    names(dat.chem.out) <- c(paste0('css',100*prob.vect),
#                             paste0('LCL', 'css', 100*prob.vect),
#                             paste0('UCL', 'css', 100*prob.vect),
#                             'var.css',
#                             'chemcas',
#                             'ExpoCast.group')
#    return(dat.chem.out)
#    }

## ----eval_parallel, eval=FALSE-------------------------------------------
#  numcluster <- 40 #The number of processors to use in parallel
#  #Note: This will depend on how many your machine has available!
#  cluster <- parallel::makeCluster(numcluster, outfile='subpoprun_parallel_out.txt')
#  parallel::clusterEvalQ(cl=cluster,
#                         {library(httk)})
#  #Set seeds on all workers for reproducibility
#  parallel::clusterSetRNGStream(cluster,
#                                TeachingDemos::char2seed("Caroline Ring"))
#  #List subpopulations
#  ExpoCast.groups<-list("Total",
#                        "Age.6.11",
#                        "Age.12.19",
#                        "Age.20.65",
#                        "Age.GT65",
#                        "BMIgt30",
#                        "BMIle30",
#                        "Females",
#                        "Males",
#                        "ReproAgeFemale",
#                        "Age.20.50.nonobese")
#  #Evaluate model
#  model <- '3compartmentss'
#  popmethod <- "dr"
#    for (grp in ExpoCast.groups){
#      for (poormetab in c(TRUE, FALSE)){
#        for (fup.censor in c(TRUE, FALSE)) {
#          #First read in population data.table
#          grp.dt <- readRDS(file=paste0('data/',paste('httkpop',
#                                                      popmethod,
#                                                      grp, sep='_'),
#                                        '.Rdata'))
#          nsamp <- nrow(grp.dt)
#          #Next, loop over chemicals and rbind the result.
#          allchems.dt <- data.table::rbindlist(parallel::parLapply(cl = cluster,
#                                                                   X = chemlist,
#                                                                   fun = doforeachchem,
#                                                                   model = model,
#                                                                   species = 'Human',
#                                                                   sigma.factor = 0.3,
#                                                                   css.method = 'analytic',
#                                                                   indiv.model.bio = grp.dt,
#                                                                   ExpoCast.group = grp,
#                                                                   poormetab = poormetab,
#                                                                   fup.censor = fup.censor,
#                                                                   nsamp = nsamp,
#                                                                   Clint.vary = TRUE))
#          #Now, save the result. Put some metadata in the filename,
#          #like the group, the method used to generate this population,
#          #and the values of poormetab and fup.censor.
#          #Also put which HTTK model was used.
#          saveRDS(object = allchems.dt,
#                  file = paste0('data/',
#                                paste('allchems', grp, popmethod,
#                                      'poormetab', poormetab,
#                                      'fup.censor', fup.censor,
#                                      model,
#                                      "FuptoFub", sep='_'),
#                                '.Rdata'))
#          }
#        }
#      }
#  parallel::stopCluster(cluster)

## ---- eval=FALSE---------------------------------------------------------
#  indep_gen <- function(nsamp=1000, sigma.factor=0.3){
#  
#    COmean <- physiology.data[physiology.data$Parameter=='Cardiac Output',
#                                 'Human']
#    indep.bio <- data.table(Qcardiacc=truncnorm::rtruncnorm(n=nsamp,
#                                                            mean=COmean,
#                                                            sd=sigma.factor*COmean,
#                                                            a=0)/1000*60)
#    indep.bio[, BW:=truncnorm::rtruncnorm(n=nsamp,
#                                          mean=physiology.data[physiology.data$Parameter=='Average BW',
#                                                                  'Human'],
#                                          sd=sigma.factor*physiology.data[physiology.data$Parameter=='Average BW',
#                                                                             'Human'],
#                                          a=0)]
#    indep.bio[, plasma.vol:=truncnorm::rtruncnorm(n=nsamp,
#                                                  mean=physiology.data[physiology.data$Parameter=='Plasma Volume',
#                                                                          'Human'],
#                                                  sd=sigma.factor*physiology.data[physiology.data$Parameter=='Plasma Volume',
#                                                                                     'Human'],
#                                                  a=0)/1000] #convert mL/kg to L/kg
#    indep.bio[, hematocrit:=truncnorm::rtruncnorm(n=nsamp,
#                                                  mean=physiology.data[physiology.data$Parameter=='Hematocrit',
#                                                                          'Human'],
#                                                  sd=sigma.factor*physiology.data[physiology.data$Parameter=='Hematocrit',
#                                                                                     'Human'],
#                                                  a=0,
#                                                  b=1)]
#    indep.bio[, million.cells.per.gliver:=truncnorm::rtruncnorm(n=nsamp,
#                                                                mean=110,
#                                                                sd=sigma.factor*110,
#                                                                a=0)]
#  
#    all.tissues <- tissue.data$Tissue[tissue.data$Tissue!='red blood cells']
#    for (tissue in all.tissues){
#      vol.mean <- tissue.data[tissue.data$Tissue==tissue,
#                              'Human Vol (L/kg)']
#      flow.mean <- tissue.data[tissue.data$Tissue==tissue,
#                               'Human Flow (mL/min/kg^(3/4))']/
#        1000*60
#      if (tissue=='liver'){ #subtract gut flow from portal vein flow
#        #to get arterial flow
#        flow.mean <- (tissue.data[tissue.data$Tissue=='liver',
#                                  'Human Flow (mL/min/kg^(3/4))'] -
#                        tissue.data[tissue.data$Tissue=='gut',
#                                    'Human Flow (mL/min/kg^(3/4))'])/
#          1000*60
#        }
#  
#      indep.bio[, paste0('V',
#                         tissue,
#                         'c'):=truncnorm::rtruncnorm(n=nsamp,
#                                                     mean=vol.mean,
#                                                     sd=sigma.factor*vol.mean,
#                                                     a=0)]
#      indep.bio[, paste0('Q',
#                         tissue,
#                         'f'):=truncnorm::rtruncnorm(n=nsamp,
#                                                     mean=flow.mean,
#                                                     sd=sigma.factor*flow.mean,
#                                                     a=0)/Qcardiacc]
#      }
#  
#  
#    indep.bio[, Qtotal.liverc:=(Qliverf+Qgutf)*Qcardiacc]
#    indep.bio[, liver.density:=1.05]
#    gfr.mean<-physiology.data[physiology.data$Parameter=='GFR',
#                                 'Human']*60/1000 #convert from ml/min/kg^(3/4) to L/hr/kg(3/4)
#    indep.bio[, Qgfrc:=truncnorm::rtruncnorm(n=nsamp,
#                                             mean=gfr.mean,
#                                             sd=sigma.factor*gfr.mean,
#                                             a=0)]
#    indep.bio[,
#              Vartc:= plasma.vol/
#                (1-hematocrit)/2] #L/kgBW
#    indep.bio[,
#              Vvenc:= plasma.vol/
#                (1-hematocrit)/2] #L/kgBW
#    return(indep.bio)
#    }

## ---- eval=FALSE---------------------------------------------------------
#  model <- '3compartmentss'
#  popmethod <- 'indepMC'
#  TeachingDemos::char2seed("Caroline Ring")
#  indep.bio <- indep_gen()
#  numcluster <- 40 #The number of processors to use in parallel
#  #Note: This will depend on how many your machine has available!
#  cluster <- parallel::makeCluster(numcluster, outfile='indepMC_evalmodels_parallel_out.txt')
#  parallel::clusterEvalQ(cl=cluster,
#                         {library(httk)})
#  #Set seeds on all workers for reproducibility
#  parallel::clusterSetRNGStream(cluster,
#                                TeachingDemos::char2seed("Caroline Ring"))
#  for (poormetab in c(TRUE, FALSE)){
#    for (fup.censor in c(TRUE, FALSE)){
#  allchems.dt <- data.table::rbindlist(parallel::parLapply(cl = cluster,
#                                                           X = chemlist,
#                                                           fun = doforeachchem,
#                                                           model = model,
#                                                           species = 'Human',
#                                                           sigma.factor = 0.3,
#                                                           css.method = 'analytic',
#                                                           indiv.model.bio = indep.bio,
#                                                           ExpoCast.group = 'indepMC',
#                                                           poormetab = poormetab,
#                                                           fup.censor = fup.censor,
#                                                           nsamp = 1000,
#                                                           Clint.vary = TRUE))
#  #Now, save the result. Put some metadata in the filename,
#  #like the group, the method used to generate this population,
#  #and the values of poormetab and fup.censor.
#  #Also put which HTTK model was used.
#  saveRDS(object = allchems.dt,
#          file = paste0('data/',
#                        paste('allchems', popmethod,
#                              'poormetab', poormetab,
#                              'fup.censor', fup.censor,
#                              model,
#                              "FuptoFub",
#                              sep='_'),
#                        '.Rdata'))
#  }
#  }
#  
#  parallel::stopCluster(cluster)

