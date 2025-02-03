## ----knitrPrep, include=FALSE, eval = TRUE------------------------------------
knitr::opts_chunk$set(echo = TRUE, fig.width=5, fig.height=4)

## ----clear_memory, eval = TRUE------------------------------------------------
rm(list=ls()) 

## ----runchunks, eval = TRUE---------------------------------------------------
# Set whether or not the following chunks will be executed (run):
execute.vignette <- FALSE

## ----load_libraries, eval=execute.vignette------------------------------------
#  library(httk)
#  library(gdata)
#  library(ggplot2)
#  library(viridis)
#  library(censReg)
#  library(gmodels)
#  library(gplots)
#  library(scales)
#  library(colorspace)
#  library(gridExtra)

## ----scientific.notation, eval = execute.vignette-----------------------------
#  scientific_10 <- function(x) {
#    out <- gsub("1e", "10^", scientific_format()(x))
#    out <- gsub("\\+","",out)
#    out <- gsub("10\\^01","10",out)
#    out <- parse(text=gsub("10\\^00","1",out))
#  }

## ----setuppctable, eval=execute.vignette--------------------------------------
#  pc.table <- NULL
#  pc.data <- subset(pc.data,fu != 0 & Exp_PC != 0 & Tissue %in% c("Adipose","Bone","Brain","Gut",
#      "Heart","Kidney","Liver","Lung","Muscle","Skin","Spleen","Blood Cells") &
#      tolower(Species) == 'rat' & !CAS %in% c('10457-90-6','5786-21-0','17617-23-1','69-23-8','2898-12-6',
#      '57562-99-9','59-99-4','2955-38-6','155-97-5','41903-57-5','58-55-9','77-32-7','59-05-2','60-54-8'))
#  cas.list <- get_cheminfo(model='schmitt',species='rat',suppress.messages=TRUE)
#  cas.list <-  cas.list[cas.list %in% pc.data[,'CAS']]
#  ma.data.list <- subset(chem.physical_and_invitro.data,!is.na(logMA))[,'CAS']
#  for(this.cas in cas.list){
#    parameters <- parameterize_schmitt(
#      chem.cas=this.cas,
#      species='rat',
#      suppress.messages=TRUE)
#    init.parameters <- parameters
#    charge <- calc_ionization(
#      chem.cas=this.cas,
#      pH=7.4)$fraction_charged
#    if(!this.cas %in% ma.data.list){
#      init.parameters$MA <- 10^(0.999831 - 0.016578*38.7 + 0.881721 * log10(parameters$Pow))
#    }
#    pcs <- predict_partitioning_schmitt(
#      parameters=parameters,
#      species='rat',
#      regression=FALSE,
#      suppress.messages=TRUE
#      )
#    init.pcs <- predict_partitioning_schmitt(
#      parameters=init.parameters,
#      species='rat',
#      regression=FALSE,
#      suppress.messages=TRUE)
#    for(this.tissue in subset(pc.data,CAS==this.cas)[,'Tissue']){
#      if(this.tissue == 'Blood Cells') this.pc <- 'rbc'
#      else this.pc <- this.tissue
#          pc.table <- rbind(pc.table,
#                        cbind(
#                          as.data.frame(this.cas),
#                          as.data.frame(this.tissue),
#                          as.data.frame(log10(
#                            init.pcs[[which(substr(names(init.pcs),
#                              2,
#                              nchar(names(init.pcs))-3) ==
#                              tolower(this.pc))]] *
#                            init.parameters$Funbound.plasma)),
#                          as.data.frame(log10(
#                            pcs[[which(substr(names(pcs),
#                              2,
#                              nchar(names(pcs))-3) ==
#                              tolower(this.pc))]] *
#                            parameters$unadjusted.Funbound.plasma)),
#                          as.data.frame(log10(
#                            init.pcs[[which(substr(names(init.pcs),
#                              2,
#                              nchar(names(init.pcs))-3) ==
#                              tolower(this.pc))]] *
#                            init.parameters$unadjusted.Funbound.plasma)),
#                          as.data.frame(log10(
#                            pcs[[which(substr(names(pcs),
#                              2,
#                              nchar(names(pcs))-3) == tolower(this.pc))]] *
#                              parameters$Funbound.plasma)),
#                          as.data.frame(log10(
#                            subset(pc.data,
#                              CAS==this.cas & Tissue==this.tissue)[,'Exp_PC'])),
#                          as.data.frame(subset(pc.data,
#                            CAS==this.cas & Tissue==this.tissue)[,'LogP']),
#                          as.data.frame(charge),
#                          as.data.frame(as.character(subset(pc.data,
#                            CAS == this.cas)[1,'A.B.N'])),
#                          as.data.frame(subset(pc.data,
#                            CAS == this.cas)[1,'fu'])))
#    }
#  }
#  colnames(pc.table) <- c('CAS','Tissue',
#                          'fup.correction',
#                          'ma.correction',
#                          'init.Predicted',
#                          'Predicted',
#                          'Experimental',
#                          'logP',
#                          'charge',
#                          'type',
#                          'fup')
#  init.error <- pc.table[,'Experimental'] - pc.table[,'init.Predicted']
#  fup.error <- pc.table[,'Experimental'] - pc.table[,'fup.correction']
#  ma.error <- pc.table[,'Experimental'] - pc.table[,'ma.correction']
#  final.error <- pc.table[,'Experimental'] - pc.table[,'Predicted']
#  fup.improvement <- abs(init.error) - abs(fup.error)
#  ma.improvement <- abs(init.error) - abs(ma.error)
#  final.improvement <- abs(init.error) - abs(final.error)
#  pc.table <- cbind(pc.table,fup.improvement,ma.improvement, final.improvement,
#                    final.error,init.error,ma.error,fup.error)

## ----KpFigures, eval=execute.vignette-----------------------------------------
#  init.plot <- ggplot() +
#    geom_point(data=pc.table,aes(10^(init.Predicted),10^(Experimental))) +
#    geom_abline() +
#    labs(y=expression(paste("Measured ",K[p])),
#         x=expression(paste("Predicted ",K[p]))) +
#    theme(axis.text=element_text(size=16),axis.title=element_text(size=16),
#      plot.title=element_text(size=18,hjust = 0.5)) +
#    scale_x_log10(label=scientific_10,limits=c(0.01,10^4.5)) +
#    scale_y_log10(label=scientific_10,limits=c(0.01,10^4.5)) +
#    ggtitle('(A)')
#  print(init.plot)
#  init.stats <- summary(lm(Experimental ~ init.Predicted,
#                           data=pc.table))
#  
#  final.plot <- ggplot() +
#    geom_point(data=pc.table,aes(10^(Predicted),10^(Experimental))) +
#    geom_abline() +
#    labs(y=expression(paste("Measured ",K[p])),
#         x=expression(paste("Predicted ",K[p]))) +
#    theme(axis.text=element_text(size=16),axis.title=element_text(size=16),
#      plot.title=element_text(size=18,hjust=0.5)) +
#    scale_x_log10(label=scientific_10,limits=c(0.01,10^4.5)) +
#    scale_y_log10(label=scientific_10,limits=c(0.01,10^4.5)) +
#    ggtitle('(B)')
#  print(final.plot)
#  final.stats <- summary(lm(Experimental ~ Predicted,
#                            data=pc.table))
#  
#  fup.change.plot <-  ggplot() +
#    geom_point(data=pc.table[order(pc.table[,'fup.improvement'],decreasing=F),],
#      aes(10^(fup.correction),10^(Experimental),color=fup.improvement)) +
#    geom_abline() +
#    labs(y=expression(paste("Measured ",K[p])),
#         x=expression(paste("Predicted ",K[p])),color='Improvement') +
#    theme(axis.text=element_text(size=16),axis.title=element_text(size=16)) +
#    scale_x_log10(label=scientific_10,limits=c(0.01,10^4.5)) +
#    scale_y_log10(label=scientific_10,limits=c(0.01,10^4.5)) +
#    scale_color_viridis(direction=-1,option='inferno')
#  print(fup.change.plot)
#  fup.stats <- summary(lm(Experimental ~ fup.correction,
#                       data=pc.table))
#  
#  ma.subset <- subset(pc.table,!CAS %in% ma.data.list)
#  ma.change.plot <- ggplot() +
#      geom_point(data=ma.subset[order(ma.subset[,'ma.improvement']
#        ,decreasing=F),],
#        aes(10^(ma.correction),10^(Experimental),color=ma.improvement)) +
#      geom_abline() +
#      labs(y=expression(paste("Measured ",K[p])),
#        x=expression(paste("Predicted ",K[p])),color='Improvement') +
#      theme(axis.text=element_text(size=16),axis.title=element_text(size=16)) +
#      scale_x_log10(label=scientific_10,limits=c(0.01,10^4.5)) +
#      scale_y_log10(label=scientific_10,limits=c(0.01,10^4.5)) +
#      scale_color_viridis(direction=-1,option='inferno')
#  print(ma.change.plot)
#  ma.stats <- summary(lm(Experimental ~ ma.correction,
#                      data=ma.subset))
#  
#  fup.table <- data.frame(Test=c("Initial Tissue PC Accuracy","Fup Lipid Correction","Membrane Affinity","Final"),
#                             RSquared = signif(c(init.stats$adj.r.squared,
#                                           fup.stats$adj.r.squared,
#                                           ma.stats$adj.r.squared,
#                                           final.stats$adj.r.squared),3),
#                             RMSLE = signif(c(mean(init.stats$residuals^2,na.rm=TRUE)^(1/2),
#                                       mean(fup.stats$residuals^2,na.rm=TRUE)^(1/2),
#                                       mean(ma.stats$residuals^2,na.rm=TRUE)^(1/2),
#                                       mean(final.stats$residuals^2,na.rm=TRUE)^(1/2)),3)
#                            )
#  knitr::kable(fup.table)

## ----performPCregressions, eval=execute.vignette------------------------------
#  regressions <- NULL
#  
#  for(tissue in as.character(unique(pc.table[,'Tissue']))){
#    fit <- lm(Experimental ~ Predicted ,data=subset(pc.table,Tissue==tissue))
#    smry <- summary(fit)
#    est <- estimable(fit, cm=diag(2), beta0=c(0,1), joint.test=TRUE)
#    regressions <- rbind(regressions,cbind(tissue,as.data.frame(fit$coefficients[['(Intercept)']]),
#        as.data.frame(fit$coefficients[['Predicted']]),
#        as.data.frame(smry$coefficients[['Predicted','Pr(>|t|)']]),
#        as.data.frame(smry$sigma),as.data.frame(smry$r.squared),
#        as.data.frame(smry[[11]][1,1]),as.data.frame(smry[[11]][2,2]),
#        as.data.frame(smry[[11]][1,2]),as.data.frame(smry$df[2]),as.data.frame(est[[3]])))
#  }
#  colnames(regressions) <- c('Tissue','Intercept','Slope','P-value','SE','R-squared',
#                             'Int Var','Slp Var','Cov','df','estimable')
#  
#  for (this.col in 2:10) regressions[,this.col] <- signif(regressions[,this.col], 3)
#  regressions <- regressions[order(regressions[,1]),]
#  
#  knitr::kable(regressions, caption = "Table 1: The regressions for each tissue, after fup and membrane affinity adjustments, of the log10-transformed measured Kp regressed on
#  predicted Kp")
#  
#  write.table(regressions,
#              file=paste0("Pearce2017PCCalibration=",Sys.Date(),".txt"),
#              sep="/t",
#              row.names=FALSE)

## ----PCRegressionFigure, eval=execute.vignette--------------------------------
#  x.cf <- seq(-2,3.5,.01)
#  for(tissue in  as.character(unique(pc.table[,'Tissue'])))
#  {
#    conf <- qt(0.975,df=subset(regressions,Tissue==tissue)[['df']]+1) *
#        subset(regressions,Tissue==tissue)[['SE']] *
#        sqrt(subset(regressions,Tissue==tissue)[['Int Var']] +
#        x.cf^2 * subset(regressions,Tissue==tissue)[['Slp Var']] +
#        2 * x.cf * subset(regressions,Tissue==tissue)[['Cov']] + 1)
#    line <- subset(regressions,Tissue==tissue)[['Intercept']] +
#        x.cf * subset(regressions,Tissue==tissue)[['Slope']]
#    y.cf <- line + conf
#    y.ncf <- line - conf
#  
#    cf <- cbind(as.data.frame(x.cf),as.data.frame(y.cf),as.data.frame(y.ncf))
#    if(tissue == 'Blood Cells'){
#      eval(parse(text= paste('Blood <- ggplot() +
#                                 geom_abline(linetype = "dashed") +
#                                 geom_point(data=subset(pc.table,Tissue == \'',tissue,'\'),aes(10^(Predicted),
#                                 10^(Experimental)))  +  theme(axis.text=element_text(size=14),
#                                 axis.title=element_text(size=14),plot.title=element_text(size=14)) +
#                                 scale_x_log10(label=scientific_10,limits=c(0.01,1000)) +
#                                 scale_y_log10(label=scientific_10,limits=c(0.01,1000)) +
#                                 ylab(ifelse(tissue=="Brain",expression(paste("Inferred ",K[p])),"")) +
#                                 xlab(ifelse(tissue=="Skin", expression(paste("Predicted ",K[p])),"")) +
#                                 geom_line(data=cf,aes(10^(x.cf),10^(y.cf))) +
#                                 geom_line(data=cf,aes(10^(x.cf),10^(y.ncf))) +
#                                 geom_abline(intercept=subset(regressions,Tissue==tissue)[[\'Intercept\']],
#                                 slope=subset(regressions,Tissue==tissue)[[\'Slope\']]) +
#                                 ggtitle(\'Red Blood Cells\')',sep='')))
#    }else{
#      eval(parse(text= paste(tissue,' <- ggplot() + labs(y=expression(paste("Measured ",K[p]))
#                                 ,x=expression(paste("Predicted ",K[p]))) + geom_abline(linetype = "dashed") +
#                                 geom_point(data=subset(pc.table,Tissue == \'',tissue,'\'),
#                                 aes(10^(Predicted),10^(Experimental))) + theme(axis.text=element_text(size=14),
#                                 axis.title=element_text(size=14),plot.title=element_text(size=14)) +
#                                 scale_x_log10(label=scientific_10,limits=c(0.01,1000)) +
#                                 scale_y_log10(label=scientific_10,limits=c(0.01,1000)) +
#                                 ylab(ifelse(tissue=="Brain",expression(paste("Inferred ",K[p])),"")) +
#                                 xlab(ifelse(tissue=="Skin", expression(paste("Predicted ",K[p])),"")) +
#                                 geom_line(data=cf,aes(10^(x.cf),10^(y.cf))) +
#                                 geom_line(data=cf,aes(10^(x.cf),10^(y.ncf))) +
#                                 geom_abline(intercept=subset(regressions,Tissue==tissue)[[\'Intercept\']],
#                                 slope=subset(regressions,Tissue==tissue)[[\'Slope\']]) +
#                                 ggtitle(\'',tissue,'\')',sep='')))
#    }
#  }
#  
#  grid.arrange(Adipose, Blood, Bone, Brain, Gut, Heart, Kidney, Liver, Lung, Muscle, Skin, Spleen, nrow=4)

## ----Vdevalaution, eval=execute.vignette--------------------------------------
#  obach <- subset(Obach2008,CAS %in% get_cheminfo(model='schmitt'))
#  vd.table <- NULL
#  
#  for(this.cas in obach[,'CAS']){
#    parameters <- parameterize_schmitt(
#      chem.cas=this.cas,
#      suppress.messages=TRUE)
#    init.parameters <- parameters
#    if(!this.cas %in% ma.data.list){
#      init.parameters$MA <- 10^(0.999831 - 0.016578*37 + 0.881721 * log10(parameters$Pow))
#    }
#    pcs <- predict_partitioning_schmitt(
#      parameters=parameters,
#      regression=FALSE,
#      suppress.messages=TRUE)
#    init.pcs <- predict_partitioning_schmitt(
#      parameters=init.parameters,
#      regression=FALSE,
#      suppress.messages=TRUE)
#    reg.pcs <- predict_partitioning_schmitt(
#      parameters=parameters,
#      regression=TRUE,
#      suppress.messages=TRUE)
#    vdist <- calc_vdist(
#      parameters=c(pcs,Funbound.plasma=parameters$Funbound.plasma),
#      suppress.messages=TRUE)
#    init.vdist <- calc_vdist(
#      parameters=c(
#        init.pcs,
#        Funbound.plasma=parameters$unadjusted.Funbound.plasma),
#      suppress.messages = TRUE)
#    reg.vdist <- calc_vdist(
#      parameters=c(reg.pcs,Funbound.plasma=parameters$Funbound.plasma),
#      suppress.messages = TRUE)
#    vd.table <- rbind(
#      vd.table,
#      cbind(as.data.frame(this.cas),as.data.frame(log10(init.vdist)),
#      as.data.frame(log10(vdist)),as.data.frame(log10(reg.vdist)),
#      as.data.frame(log10(subset(obach,CAS==this.cas)[['VDss (L/kg)']]))))
#  }
#  colnames(vd.table) <- c(
#    'CAS',
#    'init.vdist',
#    'corrected.vdist',
#    'calibrated.vdist',
#    'Experimental')
#  init.error <- vd.table[,'Experimental'] - vd.table[,'init.vdist']
#  correction.error <- vd.table[,'Experimental'] - vd.table[,'corrected.vdist']
#  calibration.error <- vd.table[,'Experimental'] - vd.table[,'calibrated.vdist']
#  correction.improvement <- abs(init.error) - abs(correction.error)
#  calibration.improvement <- abs(correction.error) - abs(calibration.error)
#  vd.table <- cbind(vd.table,correction.improvement,calibration.improvement,
#                    init.error,correction.error,calibration.error)

## ----VdistRegressions, eval=execute.vignette----------------------------------
#  fit <- lm(Experimental ~ calibrated.vdist,data=vd.table)
#  smry <- summary(fit)
#  calibrated.reg <- cbind(as.data.frame(fit$coefficients['(Intercept)']),
#                          as.data.frame(fit$coefficients['calibrated.vdist']),
#                          as.data.frame(smry$coefficients['calibrated.vdist','Pr(>|t|)']),
#                          as.data.frame(smry$sigma),as.data.frame(smry$r.squared))
#  fit <- lm(Experimental ~ init.vdist,data=vd.table)
#  smry <- summary(fit)
#  init.reg <- cbind(as.data.frame(fit$coefficients['(Intercept)']),
#                    as.data.frame(fit$coefficients['init.vdist']),
#                    as.data.frame(smry$coefficients['init.vdist','Pr(>|t|)']),
#                    as.data.frame(smry$sigma),as.data.frame(smry$r.squared))
#  fit <- lm(Experimental ~ corrected.vdist,data=vd.table)
#  smry <- summary(fit)
#  corrected.reg <- cbind(as.data.frame(fit$coefficients['(Intercept)']),
#                         as.data.frame(fit$coefficients['corrected.vdist']),
#                         as.data.frame(smry$coefficients['corrected.vdist','Pr(>|t|)']),
#                         as.data.frame(smry$sigma),as.data.frame(smry$r.squared))
#  colnames(init.reg) <- colnames(corrected.reg) <-
#  colnames(calibrated.reg) <- c('Intercept','Slope','P-value','Std Err','R-squared')

## ----VdistPlots, eval=execute.vignette----------------------------------------
#  init.vd.plot <- ggplot(vd.table,aes(10^(init.vdist),10^(Experimental))) + geom_point() +
#      geom_abline(intercept = init.reg[['Intercept']], slope = init.reg[["Slope"]]) +
#      geom_abline(linetype = "dashed") + xlab("Predicted Volume of Distribution") +
#      ylab("Measured Volume of Distribution") + theme(axis.text=element_text(size=16),
#      axis.title=element_text(size=16),plot.title=element_text(size=18,hjust = 0.5)) +
#      scale_x_log10(label=scientific_10,limits=c(10^(-1.5),10^(8.5))) +
#      scale_y_log10(label=scientific_10,limits=c(10^(-1.5),10^(8.5))) +
#      ggtitle('(A)')
#  print(init.vd.plot)
#  
#  correction.plot <- ggplot() +
#      geom_point(data=vd.table[order(vd.table[,'correction.improvement'],decreasing=F),],
#      aes(10^(corrected.vdist),10^(Experimental),color=correction.improvement)) +
#      geom_abline(intercept = corrected.reg[['Intercept']],slope = corrected.reg[["Slope"]]) +
#      geom_abline(linetype = "dashed") + xlab("Predicted Volume of Distribution") +
#      ylab("Measured Volume of Distribution") + theme(legend.position = c(.95, .95),
#      legend.justification = c("right", "top"),legend.box.just = "right",
#      legend.margin = margin(6, 6, 6, 6),axis.text=element_text(size=16),
#      axis.title=element_text(size=16),plot.title=element_text(size=18,hjust = 0.5)) +
#      scale_x_log10(limits=c(10^(-1.5),10^(3))) + scale_y_log10(limits=c(10^(-1.5),10^(3))) +
#      ggtitle('(B)') + scale_color_viridis(direction=-1,option='inferno')
#  print(correction.plot)
#  
#  calibration.plot <- ggplot() +
#      geom_point(data=vd.table[order(vd.table[,'calibration.improvement'],decreasing=F),],
#      aes(10^(calibrated.vdist),10^(Experimental),color=calibration.improvement)) +
#      geom_abline(intercept = calibrated.reg[['Intercept']],slope = calibrated.reg[["Slope"]]) +
#      geom_abline(linetype = "dashed") + xlab("Predicted Volume of Distribution") +
#      ylab("Measured Volume of Distribution") + theme(legend.position = c(.95, .95),
#      legend.justification = c("right", "top"),legend.box.just = "right",
#      legend.margin = margin(6, 6, 6, 6),axis.text=element_text(size=16),
#      axis.title=element_text(size=16),plot.title=element_text(size=18,hjust = 0.5)) +
#      scale_x_log10(limits=c(10^(-1.5),10^(3))) + scale_y_log10(limits=c(10^(-1.5),10^(3))) +
#      ggtitle('(C)') + scale_color_viridis(direction=-1,option='inferno')
#  print(calibration.plot)

## ----bloodtoplasmaevaluation, eval=execute.vignette---------------------------
#  rb2p.data <- subset(chem.physical_and_invitro.data,!is.na(Human.Rblood2plasma))
#  measured.rb2p <- NULL
#  measured.krbc <- NULL
#  predicted.rb2p <- NULL
#  predicted.krbc <- NULL
#  cas <- NULL
#  charge <- NULL
#  fup <- NULL
#  logP <- NULL
#  pka_donor <- NULL
#  pka_accept <- NULL
#  for(this.cas in rb2p.data[rb2p.data[,'CAS'] %in%
#    get_cheminfo(model='schmitt', suppress.messages = TRUE),'CAS'])
#  {
#    rb2p <- get_rblood2plasma(chem.cas=this.cas)
#    krbc <- (rb2p + .44 - 1) / 0.44
#    measured.rb2p <- c(measured.rb2p,rb2p)
#    measured.krbc <- c(measured.krbc,krbc)
#    parameters <- parameterize_schmitt(
#      chem.cas=this.cas,
#      suppress.messages = TRUE)
#    pcs <- predict_partitioning_schmitt(
#      parameters=parameters,
#      suppress.messages=TRUE)
#    predicted.krbc <- c(predicted.krbc,pcs[['Krbc2pu']] * parameters$Funbound.plasma)
#    cas <- c(cas,this.cas)
#    charge <- c(charge,calc_ionization(chem.cas=this.cas,pH=7.4)$fraction_charged)
#    fup <- c(fup,parameters$unadjusted.Funbound.plasma)
#    logP <-  c(logP,log10(parameters$Pow))
#    pka_donor <- c(pka_donor,paste(parameters$pKa_Donor,collapse=','))
#    pka_accept <- c(pka_accept,paste(parameters$pKa_Accept,collapse=','))
#  }
#  predicted.rb2p <-  1 - 0.44 + 0.44 * predicted.krbc
#  rb2p.table <- cbind(as.data.frame(cas),as.data.frame(predicted.rb2p),as.data.frame(measured.rb2p))
#  colnames(rb2p.table) <- c('cas','predicted.rb2p','measured.rb2p')
#  error <- log10(rb2p.table[,'measured.rb2p']) - log10(rb2p.table[,'predicted.rb2p'])
#  rb2p.table <- cbind(rb2p.table,error,charge,fup,logP)
#  
#  error <- log10(measured.krbc) -  log10(predicted.krbc)
#  krbc.table <- cbind(as.data.frame(cas),as.data.frame(predicted.krbc),as.data.frame(measured.krbc),
#                      as.data.frame(error),charge,fup,logP,pka_donor,pka_accept)

## ----RBCStats, eval=execute.vignette------------------------------------------
#  pdta <- data.frame(x = predicted.krbc,
#                     y = measured.krbc)
#  pdta$y[pdta$y <= 0.1] <- 0.1
#  pdta$Censoring <- factor(c("Not Censored","Censored")[as.numeric(pdta$y <= 0.1) + 1])
#  y <- measured.krbc
#  x <- cbind(rep(1, length(y)),-1 * log10(predicted.krbc))
#  colnames(x) <- c("Intercept","Predicted")
#  cc <- as.numeric(y <= 0.1)
#  y[y < 0.1] <- 0.1
#  y <- -log10(y)
#  
#  out <- censReg(y~x, data = pdta, left=0.1)
#  out$betas <- out$estimate

## ----RBCFigure, eval=execute.vignette-----------------------------------------
#  censored.regression <- ggplot() +
#      geom_point(data=pdta,aes(x=x,y=y, color=Censoring)) +
#      scale_x_log10(limits=c(.0009,40)) + scale_y_log10(limits=c(.1,4),breaks=c(.1,.5,2.5)) +
#      labs(y=expression(paste("Inferred ",K[p])),x=expression(paste("Predicted ",K[p]))) +
#      geom_abline(intercept=0, slope=1, linetype='dashed') +
#      theme(axis.text=element_text(size=16),axis.title=element_text(size=16),
#      plot.title=element_text(size=18,hjust=0.5),legend.position = c(0.11, .8)) +
#      geom_abline(slope=out$betas[2],intercept=-out$betas[1]) + ggtitle('(B)')
#  print(censored.regression)
#  
#  rb2p.plot <- ggplot(rb2p.table,aes(predicted.rb2p,measured.rb2p)) +
#      geom_point()  + scale_x_log10(lim=c(.52,18)) +
#      scale_y_log10(lim=c(.52,2.5),breaks=c(0.5,1,2)) + geom_abline(linetype='dashed') +
#      labs(y=expression(paste("Measured Whole Blood ",K[p])),
#      x=expression(paste("Predicted Whole Blood ",K[p]))) +
#      theme(axis.text=element_text(size=16),axis.title=element_text(size=16),
#      plot.title=element_text(size=18,hjust=0.5)) + ggtitle('(A)')
#  print(rb2p.plot)

## ----summarytable, eval=execute.vignette--------------------------------------
#  heatmap.table <- NULL
#  for(this.cas in get_cheminfo(model='schmitt')){
#      parms <- parameterize_schmitt(
#        chem.cas=this.cas,
#        suppress.messages = TRUE)
#      pcs <- predict_partitioning_schmitt(
#        parameters=parms,
#        suppress.messages = TRUE)
#      heatmap.table <- cbind(heatmap.table,log10(unlist(pcs)[1:11]*parms$Funbound.plasma))
#  }
#  rownames(heatmap.table) <-  c('Adipose','Bone','Brain','Gut','Heart',
#                                'Kidney','Liver','Lung','Muscle','Skin','Spleen')
#  colnames(heatmap.table) <- rep("",dim(heatmap.table)[2])

## ----summaryheatmap, eval=execute.vignette------------------------------------
#  pal <- function (n, h = c(260, -328), c = 80, l = c(30, 100), power = 1.5,
#      fixup = TRUE, gamma = NULL, alpha = 1, ...)
#  {
#      if (!is.null(gamma))
#          warning("'gamma' is deprecated and has no effect")
#      if (n < 1L)
#          return(character(0L))
#     h <- rep(h, length.out = 2L)
#      c <- c[1L]
#      l <- rep(l, length.out = 2L)
#      power <- rep(power, length.out = 2L)
#      rval <- seq(1, -1, length = n)
#      rval <- hex(polarLUV(L = l[2L] - diff(l) * abs(rval)^power[2L],
#          C = c * abs(rval)^power[1L], H = ifelse(rval > 0, h[1L],
#              h[2L])), fixup = fixup, ...)
#      if (!missing(alpha)) {
#          alpha <- pmax(pmin(alpha, 1), 0)
#          alpha <- format(as.hexmode(round(alpha * 255 + 1e-04)),
#              width = 2L, upper.case = TRUE)
#          rval <- paste(rval, alpha, sep = "")
#      }
#      return(rval)
#  }
#  
#  hclust.ave <- function(x) hclust(x, method="ward.D2")
#  heatmap.2(heatmap.table,col=pal,trace="none", hclustfun=hclust.ave,
#            key.xlab=expression(paste("log10 ",K[p]," Value")),
#            key.ylab=expression(paste("Number of ",K[p])),
#            key.title="Partition Coefficient",xlab="Chemicals",cex.lab=2,margins=c(2,5))

