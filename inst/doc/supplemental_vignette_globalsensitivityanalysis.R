## ---- include=FALSE------------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = '#>')

## ------------------------------------------------------------------------
library(httk)
library(data.table)
library(parallel)

## ---- eval=FALSE---------------------------------------------------------
#  seed.int <- TeachingDemos::char2seed('Caroline Ring', set=FALSE)+1

## ---- eval=FALSE---------------------------------------------------------
#  nsamp<-1000
#  ExpoCast.group<-list("Total",
#                       "Age.6.11",
#                       "Age.12.19",
#                       "Age.20.65",
#                       "Age.GT65",
#                       "BMIgt30",
#                       "BMIle30",
#                       "Females",
#                       "Males",
#                       "ReproAgeFemale",
#                       "Age.20.50.nonobese")
#  
#  gendernum <- c(rep(list(NULL),7),
#                 list(list(Male=0, Female=1000)),
#                 list(list(Male=1000, Female=0)),
#                 list(list(Male=0, Female=1000)),
#                 list(NULL))
#  
#  agelim<-c(list(c(0,79),
#                 c(6,11),
#                 c(12,19),
#                 c(20,65),
#                 c(66,79)),
#            rep(list(c(0,79)),4),
#            list(c(16,49)),
#            list(c(20,50)))
#  
#  bmi_category <- c(rep(list(c('Underweight',
#                               'Normal',
#                               'Overweight',
#                               'Obese')),
#                        5),
#                    list('Obese',
#                         c('Underweight','Normal', 'Overweight')),
#                    rep(list(c('Underweight',
#                               'Normal',
#                               'Overweight',
#                               'Obese')),
#                        3),
#                    list(c('Underweight', 'Normal', 'Overweight')))

## ---- eval=FALSE---------------------------------------------------------
#  tmpfun <- function(gendernum, agelim, bmi_category, ExpoCast_grp,
#                     nsamp, method){
#    result <- tryCatch({
#      pops <- httk::httkpop_generate(
#        method=method,
#        nsamp=nsamp,
#        gendernum = gendernum,
#        agelim_years = agelim,
#        weight_category = bmi_category)
#  
#      filepart <- switch(method,
#                         'virtual individuals' = 'vi',
#                         'direct resampling' = 'dr')
#      saveRDS(object=pops,
#              file=paste0('data/httkpop_',
#                          filepart,
#                          '_',
#                          ExpoCast_grp,
#                          '2.Rdata')) #Note we've added a 2 to the file name!
#      return(0)
#      }, error = function(err){
#        print(paste('Error occurred:', err))
#        return(1)
#        })
#    }
#  
#  
#  cluster <- parallel::makeCluster(40,
#                                   outfile='subpopulations_parallel_out2.txt')
#  
#  evalout <- parallel::clusterEvalQ(cl=cluster,
#                                    {library(data.table)
#                                     library(httk)})
#  parallel::clusterExport(cl = cluster,
#                          varlist = 'tmpfun')
#  #Set seeds on all workers for reproducibility
#  parallel::clusterSetRNGStream(cluster,
#                                seed.int)
#  out_dr <- parallel::clusterMap(cl=cluster,
#                                 fun = tmpfun,
#                                 gendernum=gendernum,
#                                 agelim=agelim,
#                                 bmi_category=bmi_category,
#                                 ExpoCast_grp = ExpoCast.group,
#                                 MoreArgs = list(nsamp = nsamp,
#                                                 method = 'direct resampling')
#                                 )
#  parallel::stopCluster(cluster)

## ---- eval=FALSE---------------------------------------------------------
#  doforeachparam <- function(i,
#                             indiv.A,
#                             indiv.B,
#                             this.chemcas,
#                             model,
#                             species='Human',
#                             css.method,
#                             f.A, #already evaluated model for A
#                             f.B, #already evaluated model for B
#                             g0, #normalized f.A
#                             g0prime, #normalized f.B
#                             p0){ #spurious correlation betweenf.A and f.B
#  
#    #make copies so as not to change these data.tables outside the function
#    indiv.A <- copy(indiv.A)
#    indiv.B <- copy(indiv.B)
#  
#    #First, let's swap column i.
#    indiv.ABi <- copy(indiv.A)
#    indiv.ABi[, (i):= indiv.B[, i, with=FALSE]]
#    indiv.BAi <- copy(indiv.B)
#    indiv.BAi[, (i):=indiv.A[, i, with=FALSE]]
#  
#    #Convert to HTTK parameters for matrix ABi and matrix BAi
#    indiv.ABi.httk <- httk::convert_httk(indiv.model.bio = indiv.ABi,
#                                            model = model,
#                                            this.chem = this.chemcas)
#    indiv.BAi.httk <- httk::convert_httk(indiv.model.bio = indiv.BAi,
#                                            model = model,
#                                            this.chem = this.chemcas)
#  
#    #If model is 3compartmentss, convert Funbound.plasma to Funbound.blood
#    if (model=="3compartmentss"){
#      #First, get the default parameters used for the Schmitt method of estimating
#      #partition coefficients.
#      pschmitt <- httk::parameterize_schmitt(chem.cas=this.chemcas,
#                                             species='Human')
#      convert_Fup <- function(DT.bio, DT.httk, pschmitt, this.chemcas){
#        DT.bio <- copy(DT.bio)
#        DT.httk <- copy(DT.httk)
#      #next, replace the single default value for Funbound.plasma with the vector
#      #of Funbound.plasma values from the virtual population data.table.
#      pschmitt$Funbound.plasma<-DT.httk[, Funbound.plasma]
#      #Now, predict the partitioning coefficients using Schmitt's method. The
#      #result will be a list of numerical vectors, one vector for each
#      #tissue-to-plasma partitioning coefficient, and one element of each vector
#      #for each individual. The list element names specify which partition
#      #coefficient it is, e.g. Kliver2plasma, Kgut2plasma, etc.
#      PCs <- httk::predict_partitioning_schmitt(parameters=pschmitt,
#                                                chem.cas=this.chemcas,
#                                                species='Human')
#      #Compute predicted Rblood2plasma (amount in blood/amount in plasma)
#      Rb2p <- 1 - DT.bio$hematocrit + DT.bio$hematocrit *
#        PCs[["Krbc2pu"]] *
#        DT.httk$Funbound.plasma
#  
#    Funbound.blood <- DT.httk$Funbound.plasma/Rb2p
#    return(list(Rb2p=Rb2p,
#                Funbound.blood=Funbound.blood))
#    }
#  
#    Fconvert.ABi <- convert_Fup(DT.bio=indiv.ABi,
#                                                  DT.httk=indiv.ABi.httk,
#                                                  pschmitt=pschmitt,
#                                                  this.chemcas=this.chemcas)
#  
#    Fconvert.BAi <- convert_Fup(DT.bio=indiv.BAi,
#                                                  DT.httk=indiv.BAi.httk,
#                                                  pschmitt=pschmitt,
#                                                  this.chemcas=this.chemcas)
#  
#    indiv.ABi.httk[, Funbound.plasma:=Fconvert.ABi$Funbound.blood]
#    indiv.BAi.httk[, Funbound.plasma:=Fconvert.BAi$Funbound.blood]
#    }
#    #Next, let's evaluate the model.
#    if (tolower(css.method)=='analytic') {
#      f.ABi <- httk::calc_analytic_css(chem.cas=this.chemcas,
#                                       parameters=indiv.ABi.httk,
#                                       daily.dose=1,
#                                       output.units="uM",
#                                       model=model,
#                                       species=species,
#                                       suppress.messages=TRUE,
#                                       recalc.blood2plasma=TRUE)
#      f.BAi <- httk::calc_analytic_css(chem.cas=this.chemcas,
#                                       parameters=indiv.BAi.httk,
#                                       daily.dose=1,
#                                       output.units="uM",
#                                       model=model,
#                                       species=species,
#                                       suppress.messages=TRUE,
#                                       recalc.blood2plasma=TRUE)
#  
#      if (model=="3compartmentss"){
#        f.ABi <- f.ABi/Fconvert.ABi$Rb2p
#        f.BAi <- f.BAi/Fconvert.BAi$Rb2p
#      }
#      }
#    else if (tolower(css.method)=='full'){
#      f.ABi <- apply(X=indiv.ABi.httk,
#                     MARGIN=1,
#                     FUN=function(x) httk::calc_css(chem.cas=this.chemcas,
#                                                    parameters=as.list(x),
#                                                    daily.dose=1,
#                                                    output.units="uM",
#                                                    model=model,
#                                                    species=species,
#                                                    suppress.messages=TRUE,
#                                                    f.change=1e-5)[['avg']])
#      f.BAi <- apply(X=indiv.BAi.httk,
#                     MARGIN=1,
#                     FUN=function(x) httk::calc_css(chem.cas=this.chemcas,
#                                                    parameters=as.list(x),
#                                                    daily.dose=1,
#                                                    output.units="uM",
#                                                    model=model,
#                                                    species=species,
#                                                    suppress.messages=TRUE,
#                                                    f.change=1e-5)[['avg']])
#      }
#  
#    #Finally, let's estimate the correlations and the sensitivity indexes. This code uses the same notation as Glen and Isaacs, so you can follow along from their paper.
#    gj <- (f.ABi-mean(f.ABi))/sqrt(var(f.ABi) * (length(f.ABi)-1) / length(f.ABi))
#    gjprime<-(f.BAi-mean(f.BAi))/sqrt(var(f.BAi) * (length(f.BAi)-1) / length(f.BAi))
#  
#    csj <- mean(g0prime*gj)
#    csminusj <- mean(g0*gj)
#    cdj <- sum(g0prime*gj + g0*gjprime)/(2*length(g0))
#    cdminusj <- sum(g0*gj + g0prime*gjprime)/(2*length(g0))
#    pj <- sum(g0*g0prime + gj*gjprime)/(2*length(g0))
#    caj <- (cdj-pj*cdminusj)/(1-pj^2)
#    caminusj <- (cdminusj - pj*cdj)/(1-pj^2)
#    Si <- cdj - pj*(caminusj)/(1-caj*caminusj)
#    Ti <- 1 - cdminusj + pj*caj/(1-caj*caminusj)
#    return(list(Ti=Ti,
#                Si=Si,
#                p0=p0,
#                pj=pj,
#                csj=csj,
#                csminusj=csminusj,
#                cdj=cdj,
#                cdminusj=cdminusj,
#                caj=caj,
#                caminusj=caminusj))
#    }

## ---- eval=FALSE---------------------------------------------------------
#  doforeachchem <- function(this.chemcas, #CAS for one chemical
#                            model, #HTTK model to use
#                            species='Human', #Species for HTTK
#                            nsamp, #Number of people in virtual population
#                            css.method, #'analytic' or 'full'
#                            sigma.factor=0.3, #coefficient of variation
#                            indiv.model.bio1, #output of httkbio()
#                            indiv.model.bio2, #output of httkbio()
#                            ExpoCast.group, #subpopulation of interest
#                            poormetab, #TRUE or FALSE
#                            fup.censor){ #TRUE or FALSE
#  
#    #To avoid making changes outside the function
#    indiv.model.bio1 <- copy(indiv.model.bio1)
#    indiv.model.bio2 <- copy(indiv.model.bio2)
#  
#    #First draw Funbound.plasma and Clint
#    indiv.A <- cbind(indiv.model.bio1,
#                     httk::draw_fup_clint(this.chem=this.chemcas,
#                                       nsamp=nrow(indiv.model.bio1),
#                                       sigma.factor=sigma.factor,
#                                       poormetab=poormetab,
#                                       fup.censor=fup.censor))
#    indiv.B <- cbind(indiv.model.bio2,
#                     httk::draw_fup_clint(this.chem=this.chemcas,
#                                       nsamp=nrow(indiv.model.bio2),
#                                       sigma.factor=sigma.factor,
#                                       poormetab=poormetab,
#                                       fup.censor=fup.censor))
#    #Convert to HTTK parameters for matrix A and matrix B
#    indiv.A.httk <- httk::convert_httk(indiv.model.bio = indiv.A,
#                                          model = model,
#                                          this.chem = this.chemcas)
#    indiv.B.httk <- httk::convert_httk(indiv.model.bio = indiv.B,
#                                          model = model,
#                                          this.chem = this.chemcas)
#  
#    #If model is 3compartmentss, convert Funbound.plasma to Funbound.blood
#    if (model=="3compartmentss"){
#      #First, get the default parameters used for the Schmitt method of estimating
#      #partition coefficients.
#      pschmitt <- httk::parameterize_schmitt(chem.cas=this.chemcas,
#                                             species='Human')
#      convert_Fup <- function(DT.bio, DT.httk, pschmitt, this.chemcas){
#        DT.bio <- copy(DT.bio)
#        DT.httk <- copy(DT.httk)
#      #next, replace the single default value for Funbound.plasma with the vector
#      #of Funbound.plasma values from the virtual population data.table.
#      pschmitt$Funbound.plasma<-DT.httk[, Funbound.plasma]
#      #Now, predict the partitioning coefficients using Schmitt's method. The
#      #result will be a list of numerical vectors, one vector for each
#      #tissue-to-plasma partitioning coefficient, and one element of each vector
#      #for each individual. The list element names specify which partition
#      #coefficient it is, e.g. Kliver2plasma, Kgut2plasma, etc.
#      PCs <- httk::predict_partitioning_schmitt(parameters=pschmitt,
#                                                chem.cas=this.chemcas,
#                                                species='Human')
#      Rb2p <- 1 - DT.bio$hematocrit + DT.bio$hematocrit *
#        PCs[["Krbc2pu"]] *
#        DT.httk$Funbound.plasma
#  
#    Funbound.blood <- DT.httk$Funbound.plasma/Rb2p
#    return(list(Rb2p=Rb2p,
#                Funbound.blood=Funbound.blood))
#    }
#  
#    Fconvert.A <- convert_Fup(DT.bio=indiv.A,
#                                                  DT.httk=indiv.A.httk,
#                                                  pschmitt=pschmitt,
#                                                  this.chemcas=this.chemcas)
#  
#    Fconvert.B <- convert_Fup(DT.bio=indiv.B,
#                                                  DT.httk=indiv.B.httk,
#                                                  pschmitt=pschmitt,
#                                                  this.chemcas=this.chemcas)
#  
#    indiv.A.httk[, Funbound.plasma:=Fconvert.A$Funbound.blood]
#    indiv.B.httk[, Funbound.plasma:=Fconvert.B$Funbound.blood]
#    }
#    #Evaluate model
#    if (tolower(css.method)=='analytic') {
#      f.A <- httk::calc_analytic_css(chem.cas=this.chemcas,
#                                     parameters=indiv.A.httk,
#                                     daily.dose=1,
#                                     output.units="uM",
#                                     model=model,
#                                     species=species,
#                                     suppress.messages=TRUE,
#                                     recalc.blood2plasma=TRUE)
#      f.B <- httk::calc_analytic_css(chem.cas=this.chemcas,
#                                     parameters=indiv.B.httk,
#                                     daily.dose=1,
#                                     output.units="uM",
#                                     model=model,
#                                     species=species,
#                                     suppress.messages=TRUE,
#                                     recalc.blood2plasma=TRUE)
#  
#      if (model=="3compartmentss"){
#        f.A <- f.A/Fconvert.A$Rb2p
#        f.B <- f.B/Fconvert.B$Rb2p
#      }
#      }
#    else if (tolower(css.method)=='full'){
#      f.A <- apply(X=indiv.A.httk,
#                   MARGIN=1,
#                   FUN=function(x) httk::calc_css(chem.cas=this.chemcas,
#                                                  parameters=as.list(x),
#                                                  daily.dose=1,
#                                                  output.units="uM",
#                                                  model=model,
#                                                  species=species,
#                                                  suppress.messages=TRUE,
#                                                  f.change=1e-5)[['avg']])
#      f.B <- apply(X=indiv.B.httk,
#                   MARGIN=1,
#                   FUN=function(x) httk::calc_css(chem.cas=this.chemcas,
#                                                  parameters=as.list(x),
#                                                  daily.dose=1,
#                                                  output.units="uM",
#                                                  model=model,
#                                                  species=species,
#                                                  suppress.messages=TRUE,
#                                                  f.change=1e-5)[['avg']])
#      }
#    #Compute normalized data (number of standard deviations from the mean)
#    g0<-(f.A-mean(f.A))/sqrt(var(f.A) * (length(f.A)-1) / length(f.A))
#    g0prime <- (f.B-mean(f.B))/sqrt(var(f.B) * (length(f.B)-1) / length(f.B))
#  
#    #Compute spurious correlation term between f.A and f.B
#    p0 <- mean(g0*g0prime)
#  
#    #Next, define the list of HTTK parameters that constitute the physiological parameters for this model.
#    #Strictly speaking this should be all of the columns in indiv.model.bio1, except for liver.density, which is a fixed value.
#    phys.par <- names(indiv.model.bio1)[names(indiv.model.bio1)!=
#                                          'liver.density']
#    #Add Funbound.plasma
#    parlist <- list('Funbound.plasma',
#                    'Clint',
#                    phys.par)
#    #Note: this is a list with three members!
#    #Sensitivity is calculated to the *group* of physiological parameters *as a whole*, not to each one individually.
#  
#    #If Clint is fixed at zero,
#    #don't test sensitivity to it
#    if (all(indiv.A[, Clint]==0)){
#      parlist <- parlist[parlist!='Clint']
#      }
#  
#    #Now loop over parameters (and groups thereof)
#    eff.list <- lapply(parlist,
#                       doforeachparam,
#                       indiv.A=indiv.A,
#                       indiv.B=indiv.B,
#                       this.chemcas=this.chemcas,
#                       model=model,
#                       species=species,
#                       css.method=css.method,
#                       f.A=f.A,
#                       f.B=f.B,
#                       g0=g0,
#                       g0prime=g0prime,
#                       p0=p0)
#  
#    parnames <- parlist
#    #Name the sensitivity indexes by their parameters
#    #For the group of physiological parameters, just name it "phys.par"
#    parnames[sapply(parnames, function(x) length(x)>1)] <- 'phys.par'
#    names(eff.list) <- parnames
#  
#    #If Clint was zero, then sensitivity to it wasn't calculated.
#    #Set all the sensitivity quantities to NA to indicate this.
#    if (all(indiv.A[, Clint]==0)){
#      eff.list <- c(eff.list,
#                    list(Clint=list(Ti=NA,
#                                    Si=NA,
#                                    p0=NA,
#                                    pj=NA,
#                                    csj=NA,
#                                    csminusj=NA,
#                                    cdj=NA,
#                                    cdminusj=NA,
#                                    caj=NA,
#                                    caminusj=NA)))
#      }
#    #Construct a data.table to return
#    eff.dt <- as.data.table(do.call(rbind.data.frame,
#                                    eff.list),
#                            keep.rownames=TRUE)
#    eff.dt[, chemcas:=this.chemcas]
#    return(eff.dt)
#    }

## ---- eval=FALSE---------------------------------------------------------
#  doforeachsubpop <- function(ecg, #Short for "ExpoCast group"
#                              nsamp,
#                              chemlist, #list of chemicals to loop over
#                              model, #HTTK model to use
#                              species = 'Human', #HTTK species to use
#                              sigma.factor = 0.3, #Coefficient of variation
#                              css.method = 'analytic', #or 'full'
#                              poormetab, #TRUE or FALSE
#                              fup.censor, #TRUE or FALSE
#                              cluster){ #The parallel cluster to use (we'll parallelize the loop over chemicals)
#    #First read in the virtual population data.
#    indiv1.dt<-readRDS(paste0('data/httkpop_',
#                              'dr',
#                              '_',
#                              ecg,
#                              '.Rdata'))
#    indiv2.dt<-readRDS(paste0('data/httkpop_',
#                              'dr',
#                              '_',
#                              ecg,
#                              '2.Rdata'))
#  
#    #Convert the biological parameters first
#    indiv.model.bio1 <- httk::httkpop_bio(indiv_dt=indiv1.dt)
#    indiv.model.bio2 <- httk::httkpop_bio(indiv_dt=indiv2.dt)
#  
#    #Now loop over the chemicals
#    eff.allchems <- rbindlist(
#      parLapplyLB(cl=cluster,
#                  chemlist,
#                  doforeachchem,
#                  model=model,
#                  species=species,
#                  sigma.factor=sigma.factor,
#                  css.method=css.method,
#                  indiv.model.bio1=indiv.model.bio1,
#                  indiv.model.bio2=indiv.model.bio2,
#                  ExpoCast.group=ecg,
#                  poormetab=poormetab,
#                  fup.censor=fup.censor,
#                  nsamp=nsamp))
#    #Add a column to the data.table denoting the subpopulation
#    eff.allchems[,ExpoCast.group:=ecg]
#    #And return the output
#    return(eff.allchems)
#    }

## ---- eval=FALSE---------------------------------------------------------
#  model <- "3compartmentss"
#  species <- "Human"
#  sigma.factor <- 0.3
#  css.method <- "analytic"
#  chemlist <- httk::get_cheminfo(info="CAS",
#                                 exclude.fub.zero=FALSE)
#  
#  cluster <- parallel::makeCluster(40,
#                                   outfile='globalsens_parallel_out.txt')
#  
#  evalout <- parallel::clusterEvalQ(cl=cluster,
#                                    {library(data.table)
#                                     library(httk)})
#  parallel::clusterExport(cl = cluster,
#                          varlist = c('doforeachparam',
#                                      'doforeachchem',
#                                      'doforeachsubpop'))
#  #Set seeds on all workers for reproducibility
#  parallel::clusterSetRNGStream(cluster,
#                                TeachingDemos::char2seed("Caroline Ring"))
#  
#  system.time({for (poormetab in c(TRUE, FALSE)){
#    for (fup.censor in c(TRUE, FALSE)) {
#      eff.all<-rbindlist(lapply(ExpoCast.group,
#                                doforeachsubpop,
#                                nsamp=nsamp,
#                                chemlist=chemlist,
#                                model=model,
#                                species=species,
#                                sigma.factor=sigma.factor,
#                                css.method=css.method,
#                                poormetab=poormetab,
#                                fup.censor=fup.censor,
#                                cluster=cluster))
#      saveRDS(eff.all,
#              paste0('data/',
#                     'sens_glenisaacs_nhanes_',
#                     'allchems_',
#                     'allgroups_',
#                     model,
#                     '_',
#                     css.method,
#                     '_fup_censor_',
#                     fup.censor,
#                     '_poormetab_',
#                     poormetab,
#                     "_FuptoFub",
#                     '.Rdata'))
#      }
#    }
#    })
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
#  TeachingDemos::char2seed("Caroline Ring")
#  indep.bio1 <- indep_gen()
#  indep.bio2 <- indep_gen()

## ---- eval=FALSE---------------------------------------------------------
#  doforeachchem_indep <- function(this.chemcas, #CAS for one chemical
#                                  model, #HTTK model to use
#                                  species='Human', #Species for HTTK
#                                  nsamp, #Number of people in virtual population
#                                  css.method, #'analytic' or 'full'
#                                  sigma.factor=0.3, #coefficient of variation
#                                  indep.model.bio1, #output of indep_gen()
#                                  indep.model.bio2, #output of indep_gen()
#                                  poormetab, #TRUE or FALSE
#                                  fup.censor){  #TRUE or FALSE
#    #To avoid making changes outside the function
#    indiv.model.bio1 <- copy(indep.model.bio1)
#    indiv.model.bio2 <- copy(indep.model.bio2)
#  
#    #First draw Funbound.plasma and Clint
#    indiv.A <- httk::draw_fup_clint(this.chem=this.chemcas,
#                                       indiv.bio.tmp=indiv.model.bio1,
#                                       sigma.factor=sigma.factor,
#                                       poormetab=poormetab,
#                                       fup.censor=fup.censor)
#    indiv.B <- httk::draw_fup_clint(this.chem=this.chemcas,
#                                       indiv.bio.tmp=indiv.model.bio2,
#                                       sigma.factor=sigma.factor,
#                                       poormetab=poormetab,
#                                       fup.censor=fup.censor)
#    #Convert to HTTK parameters for matrix A and matrix B
#    indiv.A.httk <- httk::convert_httk(indiv.model.bio = indiv.A,
#                                          model = model,
#                                          this.chem = this.chemcas)
#    indiv.B.httk <- httk::convert_httk(indiv.model.bio = indiv.B,
#                                          model = model,
#                                          this.chem = this.chemcas)
#  
#    #If model is 3compartmentss, convert Funbound.plasma to Funbound.blood
#    if (model=="3compartmentss"){
#      #First, get the default parameters used for the Schmitt method of estimating
#      #partition coefficients.
#      pschmitt <- httk::parameterize_schmitt(chem.cas=this.chemcas,
#                                             species='Human')
#      convert_Fup <- function(DT.bio, DT.httk, pschmitt, this.chemcas){
#        DT.bio <- copy(DT.bio)
#        DT.httk <- copy(DT.httk)
#      #next, replace the single default value for Funbound.plasma with the vector
#      #of Funbound.plasma values from the virtual population data.table.
#      pschmitt$Funbound.plasma<-DT.httk[, Funbound.plasma]
#      #Now, predict the partitioning coefficients using Schmitt's method. The
#      #result will be a list of numerical vectors, one vector for each
#      #tissue-to-plasma partitioning coefficient, and one element of each vector
#      #for each individual. The list element names specify which partition
#      #coefficient it is, e.g. Kliver2plasma, Kgut2plasma, etc.
#      PCs <- httk::predict_partitioning_schmitt(parameters=pschmitt,
#                                                chem.cas=this.chemcas,
#                                                species='Human')
#      Rb2p <- 1 - DT.bio$hematocrit + DT.bio$hematocrit *
#        PCs[["Krbc2pu"]] *
#        DT.httk$Funbound.plasma
#  
#    Funbound.blood <- DT.httk$Funbound.plasma/Rb2p
#    return(list(Rb2p=Rb2p,
#                Funbound.blood=Funbound.blood))
#    }
#  
#    Fconvert.A <- convert_Fup(DT.bio=indiv.A,
#                                                  DT.httk=indiv.A.httk,
#                                                  pschmitt=pschmitt,
#                                                  this.chemcas=this.chemcas)
#  
#    Fconvert.B <- convert_Fup(DT.bio=indiv.B,
#                                                  DT.httk=indiv.B.httk,
#                                                  pschmitt=pschmitt,
#                                                  this.chemcas=this.chemcas)
#  
#    indiv.A.httk[, Funbound.plasma:=Fconvert.A$Funbound.blood]
#    indiv.B.httk[, Funbound.plasma:=Fconvert.B$Funbound.blood]
#    }
#    #Evaluate model
#    if (tolower(css.method)=='analytic') {
#      f.A <- httk::calc_analytic_css(chem.cas=this.chemcas,
#                                     parameters=indiv.A.httk,
#                                     daily.dose=1,
#                                     output.units="uM",
#                                     model=model,
#                                     species=species,
#                                     suppress.messages=TRUE,
#                                     recalc.blood2plasma=TRUE)
#      f.B <- httk::calc_analytic_css(chem.cas=this.chemcas,
#                                     parameters=indiv.B.httk,
#                                     daily.dose=1,
#                                     output.units="uM",
#                                     model=model,
#                                     species=species,
#                                     suppress.messages=TRUE,
#                                     recalc.blood2plasma=TRUE)
#      if (model=="3compartmentss"){
#        f.A <- f.A/Fconvert.A$Rb2p
#        f.B <- f.B/Fconvert.B$Rb2p
#      }
#      }
#    else if (tolower(css.method)=='full'){
#      f.A <- apply(X=indiv.A.httk,
#                   MARGIN=1,
#                   FUN=function(x) httk::calc_css(chem.cas=this.chemcas,
#                                                  parameters=as.list(x),
#                                                  daily.dose=1,
#                                                  output.units="uM",
#                                                  model=model,
#                                                  species=species,
#                                                  suppress.messages=TRUE,
#                                                  f.change=1e-5)[['avg']])
#      f.B <- apply(X=indiv.B.httk,
#                   MARGIN=1,
#                   FUN=function(x) httk::calc_css(chem.cas=this.chemcas,
#                                                  parameters=as.list(x),
#                                                  daily.dose=1,
#                                                  output.units="uM",
#                                                  model=model,
#                                                  species=species,
#                                                  suppress.messages=TRUE,
#                                                  f.change=1e-5)[['avg']])
#      }
#    #Compute normalized data (number of standard deviations from the mean)
#    g0<-(f.A-mean(f.A))/sqrt(var(f.A) * (length(f.A)-1) / length(f.A))
#    g0prime <- (f.B-mean(f.B))/sqrt(var(f.B) * (length(f.B)-1) / length(f.B))
#  
#    #Compute spurious correlation term between f.A and f.B
#    p0 <- mean(g0*g0prime)
#  
#    #Next, define the list of HTTK parameters that constitute the physiological parameters for this model.
#    #Strictly speaking this should be all of the columns in indiv.model.bio1, except for liver.density, which is a fixed value.
#  #   #From previous runs, these are the only parameters that the models are sensitive to
#    phys.par <- switch(model,
#                       'pbtk'=c('BW',
#                                'hematocrit',
#                                'million.cells.per.gliver',
#                                'Qcardiacc',
#                                'Qgfrc',
#                                'Qgutf',
#                                'Qkidneyf',
#                                'Qliverf',
#                                'Vliverc'),
#                       '3compartment'=c('BW',
#                                        'hematocrit',
#                                        'million.cells.per.gliver',
#                                        'Qgfrc',
#                                        'Qgutf',
#                                        'Qliverf',
#                                        'Vliverc'),
#                       '3compartmentss'=c('BW',
#                                          'million.cells.per.gliver',
#                                          'Qgfrc',
#                                          'Qtotal.liverc',
#                                          'Vliverc'),
#                       '1compartment'=c('BW',
#                                        'million.cells.per.gliver',
#                                        'Qgfrc',
#                                        'Qtotal.liverc',
#                                        'Vliverc'))
#  phys.par.all <- names(indiv.model.bio1)[names(indiv.model.bio1)!=
#                                              'liver.density']
#    parlist <- list('Clint',
#                    'Funbound.plasma',
#                    phys.par.all)
#  
#    parlist <- c(parlist, as.list(phys.par))
#    #Note: Now sensitivity is being calculated to each of the physiological parameter independently, as well as grouped.
#  
#    #If Clint is fixed at zero,
#    #don't test sensitivity to it
#    if (all(indiv.A[, Clint]==0)){
#      parlist <- parlist[parlist!='Clint']
#      }
#  
#    #Now loop over parameters (and groups thereof)
#    eff.list <- lapply(parlist,
#                       doforeachparam,
#                       indiv.A=indiv.A,
#                       indiv.B=indiv.B,
#                       this.chemcas=this.chemcas,
#                       model=model,
#                       species=species,
#                       css.method=css.method,
#                       f.A=f.A,
#                       f.B=f.B,
#                       g0=g0,
#                       g0prime=g0prime,
#                       p0=p0)
#  
#    parnames <- parlist
#    #Name the sensitivity indexes by their parameters
#    #For the group of physiological parameters, just name it "phys.par"
#    parnames[sapply(parnames, function(x) length(x)>1)] <- 'phys.par'
#    names(eff.list) <- parnames
#  
#    #If Clint was zero, then sensitivity to it wasn't calculated.
#    #Set all the sensitivity quantities to NA to indicate this.
#    if (all(indiv.A[, Clint]==0)){
#      eff.list <- c(eff.list,
#                    list(Clint=list(Ti=NA,
#                                    Si=NA,
#                                    p0=NA,
#                                    pj=NA,
#                                    csj=NA,
#                                    csminusj=NA,
#                                    cdj=NA,
#                                    cdminusj=NA,
#                                    caj=NA,
#                                    caminusj=NA)))
#      }
#    #Construct a data.table to return
#    eff.dt <- as.data.table(do.call(rbind.data.frame,
#                                    eff.list),
#                            keep.rownames=TRUE)
#    eff.dt[, chemcas:=this.chemcas]
#    return(eff.dt)
#    }

## ---- eval=FALSE---------------------------------------------------------
#  model <- "3compartmentss"
#  species <- "Human"
#  sigma.factor <- 0.3
#  css.method <- "analytic"
#  chemlist <- httk::get_cheminfo(info="CAS",
#                                 exclude.fub.zero=FALSE)
#  
#  cluster <- parallel::makeCluster(40,
#                                   outfile='globalsens_indep_parallel_out.txt')
#  
#  evalout <- parallel::clusterEvalQ(cl=cluster,
#                                    {library(data.table)
#                                     library(httk)})
#  parallel::clusterExport(cl = cluster,
#                          varlist = c('doforeachparam'))
#  #Set seeds on all workers for reproducibility
#  parallel::clusterSetRNGStream(cluster,
#                                TeachingDemos::char2seed("Caroline Ring"))
#  for (poormetab in c(TRUE, FALSE)){
#    for (fup.censor in c(TRUE, FALSE)) {
#      #Now loop over the chemicals
#      eff.allchems <- rbindlist(
#        parLapplyLB(cl=cluster,
#                    chemlist,
#                    doforeachchem_indep,
#                    model=model,
#                    species=species,
#                    sigma.factor=sigma.factor,
#                    css.method=css.method,
#                    indep.model.bio1=indep.bio1,
#                    indep.model.bio2=indep.bio2,
#                    poormetab=poormetab,
#                    fup.censor=fup.censor,
#                    nsamp=1000))
#      saveRDS(eff.allchems,
#              paste0('data/',
#                     'sens_glenisaacs_nhanes_',
#                     'allchems_',
#                     'indepMC_',
#                     model,
#                     '_',
#                     css.method,
#                     '_fup_censor_',
#                     fup.censor,
#                     '_poormetab_',
#                     poormetab,
#                     "_FuptoFub",
#                     '.Rdata'))
#      }
#    }
#  parallel::stopCluster(cluster)

