## ---- include=FALSE------------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = '#>')

## ---- eval = FALSE-------------------------------------------------------
#  library('data.table')
#  library('ggplot2')
#  library('httk')
#  library('reshape2')

## ---- eval=FALSE---------------------------------------------------------
#  data_read <- function(model, poormetab, fup.censor, chemlist){
#  httk.dat <- readRDS(paste0('data/',
#                        paste('allchems', 'indepMC',
#                              'poormetab', poormetab,
#                              'fup.censor', fup.censor,
#                              model, "FuptoFub", sep='_'),
#                        '.Rdata'))
#  
#  httk.LCL <- melt(httk.dat[,
#                                         c('chemcas',
#                                           grep(x=names(httk.dat),
#                                                pattern='LCLcss',
#                                                value=TRUE)),
#                                         with=FALSE],
#                           measure.vars=grep(x=names(httk.dat),
#                                             pattern='LCLcss',
#                                             value=TRUE),
#                           variable.name='pctile',
#                           value.name='LCL')
#  httk.LCL[, pctile:=as.numeric(gsub(x=pctile,
#                                             pattern='LCLcss',
#                                             replacement=''))]
#  httk.UCL <- melt(httk.dat[,
#                            c('chemcas',
#                              grep(x=names(httk.dat),
#                                   pattern='UCLcss',
#                                   value=TRUE)),
#                            with=FALSE],
#                   measure.vars=grep(x=names(httk.dat),
#                                     pattern='UCLcss',
#                                     value=TRUE),
#                   variable.name='pctile',
#                   value.name='UCL')
#  httk.UCL[, pctile:=as.numeric(gsub(x=pctile,
#                                     pattern='UCLcss',
#                                     replacement=''))]
#  
#  httk.cssval <- melt(httk.dat[,
#                                         c('chemcas',
#                                           grep(x=names(httk.dat),
#                                                pattern='(?<!CL)css\\d{1}',
#                                                perl=TRUE,
#                                                value=TRUE)),
#                                         with=FALSE],
#                           measure.vars=grep(x=names(httk.dat),
#                                             pattern='(?<!CL)css\\d{1}',
#                                             perl=TRUE,
#                                             value=TRUE),
#                           variable.name='pctile',
#                           value.name='css')
#  httk.cssval[, pctile:=as.numeric(gsub(x=pctile,
#                                             pattern='css',
#                                             replacement=''))]
#  httk.tmp <- merge(httk.cssval,
#                         httk.LCL,
#                         by=c('chemcas',
#                              'pctile'))
#  httk.tmp <- merge(httk.tmp,
#                    httk.UCL,
#                    by=c('chemcas',
#                         'pctile'))
#  httk.tmp[, method:='independentMC']
#  httk.tmp[, ExpoCast.group:='none']
#  
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
#  
#  tmpfun <- function(grp, popmethod, poormetab, fup.censor){
#    tmp.dt <- readRDS(paste0('data/',
#                                paste('allchems', grp, popmethod,
#                                      'poormetab', poormetab,
#                                      'fup.censor', fup.censor,
#                                      model, "FuptoFub", sep='_'),
#                                '.Rdata'))
#    tmp.dt[, ExpoCast.group:=grp]
#    return(tmp.dt)
#  
#  }
#  httkpop.dat <- rbindlist(lapply(ExpoCast.groups, tmpfun, popmethod='dr',
#                                  poormetab=poormetab,
#                                  fup.censor=fup.censor))
#  
#  httkpop.LCL <- melt(httkpop.dat[,
#                                      c('chemcas',
#                                        grep(x=names(httkpop.dat),
#                                             pattern='LCLcss',
#                                             value=TRUE),
#                                        'ExpoCast.group'),
#                                      with=FALSE],
#                        measure.vars=grep(x=names(httkpop.dat),
#                                          pattern='LCLcss',
#                                          value=TRUE),
#                        variable.name='pctile',
#                        value.name='LCL')
#  httkpop.LCL[, pctile:=as.numeric(gsub(x=pctile,
#                                          pattern='LCLcss',
#                                          replacement=''))]
#  
#  httkpop.UCL <- melt(httkpop.dat[,
#                                 c('chemcas',
#                                   grep(x=names(httkpop.dat),
#                                        pattern='UCLcss',
#                                        value=TRUE),
#                                   'ExpoCast.group'),
#                                 with=FALSE],
#                        measure.vars=grep(x=names(httkpop.dat),
#                                          pattern='UCLcss',
#                                          value=TRUE),
#                        variable.name='pctile',
#                        value.name='UCL')
#  httkpop.UCL[, pctile:=as.numeric(gsub(x=pctile,
#                                          pattern='UCLcss',
#                                          replacement=''))]
#  
#  httkpop.cssval <- melt(httkpop.dat[,
#                                         c('chemcas',
#                                           grep(x=names(httkpop.dat),
#                                                pattern='(?<!CL)css\\d{1}',
#                                                perl=TRUE,
#                                                value=TRUE),
#                                           'ExpoCast.group'),
#                                         with=FALSE],
#                           measure.vars=grep(x=names(httkpop.dat),
#                                             pattern='(?<!CL)css\\d{1}',
#                                             perl=TRUE,
#                                             value=TRUE),
#                           variable.name='pctile',
#                           value.name='css')
#  httkpop.cssval[, pctile:=as.numeric(gsub(x=pctile,
#                                             pattern='css',
#                                             replacement=''))]
#  httkpop.tmp <- merge(httkpop.cssval,
#                         httkpop.LCL,
#                         by=c('chemcas',
#                              'ExpoCast.group',
#                              'pctile'))
#  httkpop.tmp <- merge(httkpop.tmp,
#                         httkpop.UCL,
#                         by=c('chemcas',
#                              'ExpoCast.group',
#                              'pctile'))
#  httkpop.tmp[, method:='dr']
#  
#  m.dat <- rbindlist(list(httkpop.tmp,
#                 httk.tmp),
#                 use.names=TRUE)
#  m.dat <- merge(m.dat, chemlist, by='chemcas') #Add compound name and MW columns
#  m.dat[, poormetab:=poormetab]
#  m.dat[, fup.censor:=fup.censor]
#  
#  paramfun <- switch(model,
#                     '3compartmentss'='parameterize_steadystate',
#                     '3compartment'='parameterize_3comp',
#                     'pbtk'='parameterize_pbtk',
#                     '1compartment'='parameterize_1comp')
#  paramfunargs <- '(chem.cas=chemcas, species="Human", default.to.human=TRUE)'
#  if (model!='1compartment'){
#  p.dt <- chemlist[, eval(parse(text=paste(paramfun,
#                                             paramfunargs))),
#                     by=chemcas]
#  }
#  
#  
#  if (model=='1compartment'){ #get metabolism by doing steady-state parameterization
#    p.dt <- chemlist[, eval(parse(text=paste('parameterize_steadystate',
#                                              paramfunargs))),
#                      by=chemcas]
#  }
#  
#  metabname <- switch(model,
#                      '3compartmentss'='Clint',
#                      '3compartment'='Clmetabolismc',
#                      'pbtk'='Clmetabolismc',
#                      '1compartment'='Clint')
#  
#  m.dat<-merge(m.dat,
#        p.dt[, c('chemcas',
#                 metabname,
#                 'Funbound.plasma'),
#             with=FALSE],
#        by='chemcas')
#  
#  setnames(m.dat,
#           c(metabname,
#                    'Funbound.plasma'),
#           c('metab',
#             'fup'))
#  
#  
#  m.dat[, fup.lod:=factor(ifelse(fup<=0.01, TRUE, FALSE),
#                          levels=c(TRUE, FALSE),
#                          labels=c('below LOD',
#                                   'above LOD'))]
#  m.dat[, metab.zero:=factor(ifelse(metab==0, TRUE, FALSE),
#                             levels=c(TRUE, FALSE),
#                             labels=c('zero',
#                                      'nonzero'))]
#  
#  return(m.dat)
#  }

## ---- eval=FALSE---------------------------------------------------------
#  model <- '3compartmentss'
#  css.method <- 'analytic'
#  use.seed <- TRUE
#  pmlist <- c(TRUE, FALSE)
#  fclist <- c(TRUE, FALSE)
#  pmfc <- expand.grid(poormetab=pmlist,fup.censor=fclist)
#  chemlist <- as.data.table(get_cheminfo(info=c('CAS', 'Compound', 'MW'),
#                                           species='Human',
#                                           model=model,
#                                           exclude.fub.zero=FALSE))
#    setnames(chemlist, 'CAS', 'chemcas')
#  
#    data.read.list <- mapply(data_read,
#                             fup.censor=pmfc$fup.censor,
#                             poormetab=pmfc$poormetab,
#                             MoreArgs=list(model='3compartmentss',
#                                           chemlist=chemlist),
#                             SIMPLIFY=FALSE)
#    m.dat <- rbindlist(data.read.list)

## ---- eval=FALSE---------------------------------------------------------
#  methodvect <- c('dr', 'independentMC')
#  pctilevect <- c(5, 50, 95)
#  #First: cast the data table into appropriate form
#  tmp.css<-dcast.data.table(m.dat[method %in% methodvect &
#                                    pctile %in% pctilevect],
#                            Compound+chemcas+ExpoCast.group+pctile+fup.lod+
#                              metab.zero+fup.censor+poormetab+metab+fup~method,
#                            value.var=c('css'))
#  setnames(tmp.css, methodvect,
#          paste(methodvect,
#                'css',
#                sep='.'))
#  tmp.LCL <- dcast.data.table(m.dat[method %in% methodvect &
#                                      pctile %in% pctilevect],
#                              Compound+chemcas+ExpoCast.group+pctile+
#                                fup.lod+metab.zero+fup.censor+poormetab+metab+fup~method,
#                              value.var=c('LCL'))
#  setnames(tmp.LCL, methodvect,
#           paste(methodvect,
#                 'LCL',
#                 sep='.'))
#  tmp.UCL <- dcast.data.table(m.dat[method %in% methodvect &
#                                      pctile %in% pctilevect],
#                              Compound+chemcas+ExpoCast.group+pctile+
#                                fup.lod+metab.zero+fup.censor+poormetab+metab+fup~method,
#                              value.var=c('UCL'))
#  setnames(tmp.UCL, methodvect,
#           paste(methodvect,
#                 'UCL',
#                 sep='.'))
#  tmp <- merge(tmp.css,
#               tmp.LCL,
#               by=c('Compound',
#                    'chemcas',
#                    'ExpoCast.group',
#                    'pctile',
#                    'fup.lod',
#                    'metab.zero',
#                    'poormetab',
#                    'fup.censor',
#                    'metab',
#                    'fup'))
#  tmp <- merge(tmp,
#               tmp.UCL,
#               by=c('Compound',
#                    'chemcas',
#                    'ExpoCast.group',
#                    'pctile',
#                    'fup.lod',
#                    'metab.zero',
#                    'poormetab',
#                    'fup.censor',
#                    'metab',
#                    'fup'))

## ---- eval=FALSE---------------------------------------------------------
#   xonlynames<-c('dr.css',
#                                                             'dr.UCL',
#                                                             'dr.LCL')
#                                                yonlynames<-c('independentMC.css',
#                                                             'independentMC.LCL',
#                                                             'independentMC.UCL')
#  #Handle the independent MC numbers properly
#  tmp.httk <- tmp[ExpoCast.group=='none',]
#  tmp[, (yonlynames):=NULL, with=FALSE]
#  tmp.httkpop <- tmp[ExpoCast.group!='none',]
#  tmp.httk[, (xonlynames):=NULL, with=FALSE]
#  tmp.httk[, ExpoCast.group:=NULL]
#  tmp.merge <- merge(tmp.httkpop,
#                     tmp.httk,
#                     by=c('Compound',
#                          'chemcas',
#                          'pctile',
#                          'fup.lod',
#                          'metab.zero',
#                          'poormetab',
#                          'fup.censor',
#                          'metab',
#                          'fup'))
#  tmp <- copy(tmp.merge)

## ---- eval=FALSE---------------------------------------------------------
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
#   tmp[, poormetab:=factor(poormetab,
#                            levels=c(TRUE, FALSE),
#                            labels=c('included', 'excluded'))]
#    setnames(tmp, 'fup.censor', 'Fub') #rename this column for better labeling
#    tmp[, Fub:=factor(Fub,
#                      levels=c(TRUE, FALSE),
#                      labels=c('<lod censored', '<lod=lod/2'))]
#    tmp[, metab.zero:=factor(metab.zero, levels=c('nonzero', 'zero'))]
#    tmp[, fup.lod:=factor(fup.lod)]
#    tmp[, metab.zero.fup.lod:=factor(paste('CLint',
#                                           paste0(metab.zero, ','),
#                                           'Fub',
#                                           fup.lod))]
#    tmp[, ExpoCast.group:=factor(ExpoCast.group,
#                             levels=ExpoCast.groups)]

## ---- eval=FALSE---------------------------------------------------------
#   subset.list <- list(age=c('Total',
#                              'Age.6.11',
#                              'Age.12.19',
#                              'Age.20.65',
#                              'Age.GT65'),
#                        bmi=c('Total',
#                              'BMIle30',
#                              'BMIgt30'),
#                        sex=c('Total',
#                              'Males',
#                              'Females',
#                              'ReproAgeFemale'))
#  
#  axis.text.size <- 1.2
#  axis.title.size <- 1.2
#  legend.text.size <- 1.2
#  point.size <- 1.5
#  legend.title.size <- 1.2
#  strip.text.size <- 1.2
#  for (pct in c(5,50,95)){
#      for (i in seq_along(subset.list)){
#  
#         p <- ggplot(data=tmp[pctile==pct &
#                                        ExpoCast.group %in% subset.list[[i]],]) +
#    geom_point(aes(x=independentMC.css,
#                   y=dr.css,
#                   color=ExpoCast.group),
#               alpha=0.3,
#               size=point.size) +
#    scale_x_log10() +
#    scale_y_log10() +
#    geom_abline(slope=1,intercept=0)+
#    facet_grid(poormetab~Fub, labeller=labeller(poormetab=function(x) paste("CLint: PMs", x), Fub=label_both)) +
#    scale_color_brewer(type='qual', palette=2) +
#    labs(x=bquote(paste(C[ss]^.(pct), '(uM) using independent MC')),
#         y=bquote(paste(C[ss]^.(pct), '(uM) using HTTK-Pop')),
#         color='Demographic') +
#    guides(color=guide_legend(override.aes=list(size=4,alpha=1)))+
#    theme_bw() +
#    theme(strip.background=element_blank(),
#          legend.key=element_blank(),
#          legend.title=element_text(size=rel(legend.title.size)),
#          axis.text.x=element_text(size=rel(axis.text.size)),
#          axis.text.y=element_text(size=rel(axis.text.size)),
#          axis.title.x=element_text(size=rel(axis.title.size)),
#          axis.title.y=element_text(size=rel(axis.title.size)),
#          legend.text=element_text(size=rel(legend.text.size)),
#          strip.text=element_text(size=rel(strip.text.size)))
#  
#    ggsave(plot=p,
#           filename=paste0('pdf_figures/',
#                                          'cssplot_',
#                                          names(subset.list)[i],
#                                          '_drvsindep_',
#                                          model,
#                                          '_',
#                                          pct,
#                           "FuptoFub",
#                                          '.pdf'),
#           width=11,
#           height=8.5)
#    print(p)
#      }
#    }

