## ----load_packages, eval = FALSE-----------------------------------------
#  library(httk)
#  library(data.table)
#  library(EnvStats)

## ----read_study_dat, eval=FALSE------------------------------------------
#  johnson[, Compound:=tolower(Compound)]
#  #Compute clearance median and CI assuming lognormal dist
#  johnson[, cv:=Clearance.sd/Clearance.mean]
#  johnson[, sigma:=sqrt(log(1+cv^2))]
#  johnson[, mu:=log(Clearance.mean)-(0.5*sigma^2)]
#  johnson[, Clearance.median:=exp(mu)]
#  johnson[, Clearance.95CI.min:=exp(mu-1.96*sigma)]
#  johnson[, Clearance.95CI.max:=exp(mu+1.96*sigma)]
#  johnson[, src:="Johnson et al. 2006"]
#  
#  howgate[, Compound:=tolower(Compound)]
#  howgate[, Clearance.units:='1/h']
#  setnames(howgate,
#           c("Median.Clearance..1.h.",
#             "X90..CI.min",
#             "X90..CI.max"),
#           c("Clearance.median",
#             "Clearance.90CI.min",
#             "Clearance.90CI.max"))
#  
#  #Convert 90% CI to 95% CI, still assuming a log-normal distribution
#  howgate[, sigma:=log(Clearance.90CI.max/Clearance.median)/1.65]
#  howgate[, Clearance.95CI.min:=Clearance.90CI.min*exp(-0.31*sigma)]
#  howgate[, Clearance.95CI.max:=Clearance.90CI.max*exp(0.31*sigma)]
#  howgate[, src:="Howgate et al. 2006"]
#  
#  jh <- rbind(johnson, howgate, fill=TRUE)
#  
#  #Get CAS numbers by matching to HTTK data
#  chem.dt <- as.data.table(httk::get_cheminfo(model='3compartmentss',
#                                              species='Human',
#                                              info=c('CAS', 'Compound'),
#                                              exclude.fup.zero=FALSE))
#  chem.dt[, Compound:=tolower(Compound)]
#  
#  #Harmonize compound names
#  chem.dt[CAS=="57-41-0", Compound:="phenytoin"]
#  chem.dt[CAS=="58-08-2", Compound:="caffeine"]
#  chem.dt[CAS=="59865-13-3", Compound:="cyclosporine"]
#  
#  jhchem <- merge(jh,chem.dt,by='Compound')
#  
#  #Exclude infants
#  jhchem <- jhchem[!(Age.min==0 & Age.max==0), ]
#  
#  #Convert in vivo clearance to in vivo Css
#  jhchem[Clearance.units=='L/h/kg',
#         css.invivo:=1/(Clearance.median)]
#  jhchem[, Study.id:=1:nrow(jhchem)]
#  saveRDS(object=jhchem,
#          file="data/jhchem.Rdata")

## ----extract_study_pop_specs, eval=FALSE---------------------------------
#  jhtmp <- jhchem[, .(Study.id, CAS, Total.subjects,
#                      Age.min, Age.max, Female.subjects)]

## ----eval_studypop, eval=FALSE-------------------------------------------
#  eval_studypop <- function(args,
#                            fup.censor,
#                            poormetab,
#                            model){
#    #if number of female subjects is specified,
#    #set up gendernum appropriately; otherwise leave it NULL
#    if(is.na(args$Female.subjects)){
#      gendernum<-NULL
#      } else{
#        gendernum<-list(Female=args$Female.subjects,
#                        Male=args$Total.subjects-
#                          args$Female.subjects)
#        }
#    #Generate a virtual study population.
#    indiv.pop <- httk::httkpop_generate(method='virtual individuals',
#                                          nsamp=args$Total.subjects,
#                                          gendernum=gendernum,
#                                          agelim_years=c(args$Age.min,
#                                                         args$Age.max))
#    #Convert to HTTK model params
#    indiv.httk <- httk::get_httk_params(indiv_dt=indiv.pop,
#                                         model=model,
#                                         chemcas=args$CAS,
#                                         poormetab=poormetab,
#                                         fup.censor=fup.censor)
#  
#    #If model is 3compartmentss, convert Funbound.plasma to Funbound.blood
#    if (model=="3compartmentss"){
#    #First, get the default parameters used for the Schmitt method of estimating
#      #partition coefficients.
#      pschmitt <- httk::parameterize_schmitt(chem.cas=args$CAS,
#                                             species='Human')
#      #next, replace the single default value for Funbound.plasma with the vector
#      #of Funbound.plasma values from the virtual population data.table.
#      pschmitt$Funbound.plasma<-indiv.httk[, Funbound.plasma]
#  
#      #Now, predict the partitioning coefficients using Schmitt's method. The
#      #result will be a list of numerical vectors, one vector for each
#      #tissue-to-plasma partitioning coefficient, and one element of each vector
#      #for each individual. The list element names specify which partition
#      #coefficient it is, e.g. Kliver2plasma, Kgut2plasma, etc.
#      PCs <- httk::predict_partitioning_schmitt(parameters=pschmitt,
#                                                chem.cas=args$CAS,
#                                                species='Human')
#      Rb2p <- 1 - indiv.pop$hematocrit + indiv.pop$hematocrit *
#        PCs[["Krbc2pu"]] *
#        indiv.httk$Funbound.plasma
#  
#    indiv.httk[, Funbound.plasma:=Funbound.plasma/Rb2p]
#    }
#    #Evaluate Css using HTTK model
#    css <- httk::calc_analytic_css(chem.cas=args$CAS,
#                                   parameters=indiv.httk,
#                                   daily.dose=1,
#                                   output.units="uM",
#                                   model=model,
#                                   species='Human',
#                                   suppress.messages=TRUE,
#                                   recalc.blood2plasma=TRUE)
#  
#    if(model=="3compartmentss"){ #Convert from css.blood back to css.plasma
#      css <- css/Rb2p
#    }
#    #Compute Css statistics
#    css.median <- median(css)
#    css.95CI.min<-quantile(css, probs=0.025)
#    css.95CI.max<-quantile(css, probs=0.975)
#    css.median.95CI <- EnvStats::eqnpar(css, p=0.5, ci=TRUE, lb=0)$interval[['limits']]
#    css.bw <- css/indiv.pop[, weight_adj] #Bodyweight-adjusted
#    css.bw.median <- median(css.bw)
#    css.bw.95CI.min<-quantile(css.bw, probs=0.025)
#    css.bw.95CI.max<-quantile(css.bw, probs=0.975)
#    css.bw.median.95CI <- EnvStats::eqnpar(css.bw, p=0.5, ci=TRUE, lb=0)$interval[['limits']]
#  
#    #For comparison, get bodyweight stats too
#    bw.median <- median(indiv.pop[, weight_adj])
#    bw.mean <- mean(indiv.pop[, weight_adj])
#    bw.sd <- sd(indiv.pop[, weight_adj])
#    log.bw.sd <- sd(indiv.pop[, log(weight_adj)])
#  
#    return(data.table(Study.id=args$Study.id,
#                      bw.median=bw.median,
#                      bw.mean=bw.mean,
#                      bw.sd=bw.sd,
#                      log.bw.sd=log.bw.sd,
#                      css.median=css.median,
#                      css.95CI.min=css.95CI.min,
#                      css.95CI.max=css.95CI.max,
#                      css.median.95CI.min=css.median.95CI['LCL'],
#                      css.median.95CI.max=css.median.95CI['UCL'],
#                      css.bw.median=css.bw.median,
#                      css.bw.95CI.min=css.bw.95CI.min,
#                      css.bw.95CI.max=css.bw.95CI.max,
#                      css.bw.median.95CI.min=css.bw.median.95CI['LCL'],
#                      css.bw.median.95CI.max=css.bw.median.95CI['UCL']))
#    }

## ----repfun, eval=FALSE--------------------------------------------------
#  repfun <- function(args, fup.censor, poormetab, model){
#    #Replicate each study population 20 times and evaluate model;
#    #rbind the results into one big data.table
#  
#    #Initialize output list
#    allreps.ls <- vector(mode='list', length=20)
#  
#    for (i in seq_along(allreps.ls)){
#      allreps.ls[[i]] <- eval_studypop(args = args,
#                                       fup.censor = fup.censor,
#                                       poormetab = poormetab,
#                                       model=model)
#      }
#    allreps <- rbindlist(allreps.ls) #the 20 replicates of a single study
#    return(allreps)
#    }

## ----eval_all_parallel, eval=FALSE---------------------------------------
#  #Convert jhtmp to a list of lists by row, because eval_studypop() expects a list of arguments
#  jh.ls<-lapply(as.list(1:nrow(jhtmp)),
#                function(x) as.list(jhtmp[x,]))
#  
#  #Choose HTTK model
#  model <- '3compartmentss'
#  
#  cluster <- parallel::makeCluster(40, outfile='virtpops_parallel_out.txt')
#  evalout <- parallel::clusterEvalQ(cl=cluster,
#                                    {library('httk')
#                                     library('data.table')
#                                     library('EnvStats')})
#  parallel::clusterExport(cl=cluster,
#                          c('eval_studypop', 'repfun'))
#  #Set seeds on all workers for reproducibility
#  parallel::clusterSetRNGStream(cluster,
#                                TeachingDemos::char2seed("Caroline Ring"))
#  #Loop over values of poormetab and fup.censor
#  system.time({for (poormetab in c(TRUE, FALSE)){
#    for (fup.censor in c(TRUE, FALSE)){
#      rep_stat <- rbindlist(parallel::parLapply(cl=cluster, jh.ls, repfun,
#                                      poormetab = poormetab,
#                                      fup.censor = fup.censor,
#                                      model = model))
#  
#      #Save the stats for each replicate of each study population
#      saveRDS(object=rep_stat,
#              file=paste0('data/',
#                          paste(paste('virtstudypop_allreps_',
#                                      model,
#                                      'poormetab', poormetab,
#                                      'fup.censor', fup.censor,
#                                      "FuptoFub",
#                                      sep='_'),
#                                'Rdata',
#                                sep='.')))
#  
#      #Now compute overall stats, over all replicates of each study
#      overall.stats <- rep_stat[,
#                                list(median(css.median),
#                                     mean(css.95CI.max/css.median),
#                                     mean(css.95CI.min/css.median),
#                                     median(css.bw.median),
#                                     mean(css.bw.95CI.max/css.bw.median),
#                                     mean(css.bw.95CI.min/css.bw.median)),
#                                by=Study.id]
#      setnames(overall.stats,
#               paste0('V', 1:6),
#               c('median.css.median',
#                 'mean.css.95CI.max.fold',
#                 'mean.css.95CI.min.fold',
#                 'median.css.bw.median',
#                 'mean.css.bw.95CI.max.fold',
#                 'mean.css.bw.95CI.min.fold'))
#      saveRDS(object=overall.stats,
#              file=paste0('data/',
#                          paste(paste('virtstudypop_overallstats',
#                                      model,
#                                      'poormetab', poormetab,
#                                      'fup.censor', fup.censor,
#                                      "FuptoFub",
#                                      sep='_'),
#                                'Rdata',
#                                sep='.')))
#      }
#    }
#  })
#  parallel::stopCluster(cluster)

