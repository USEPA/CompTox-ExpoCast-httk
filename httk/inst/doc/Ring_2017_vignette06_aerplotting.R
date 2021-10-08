## ---- include=FALSE-----------------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = '#>')

## ----load_libraries, eval = FALSE---------------------------------------------
#  library('data.table')
#  library('gplots')
#  library('ggplot2')
#  library('httk')

## ----read_css_data, eval=FALSE------------------------------------------------
#  #Set some basic parameters for which data set to use
#  poormetab <- TRUE
#  fup.censored.dist <- TRUE
#  model <- '3compartmentss'
#  #List all the subpopulations
#  ExpoCast.groups <- c('Total',
#                       'Age.6.11',
#                       'Age.12.19',
#                       'Age.20.65',
#                       'Age.GT65',
#                       'BMIgt30',
#                       'BMIle30',
#                       'Males',
#                       'Females',
#                       'ReproAgeFemale')
#  #Read in data from each subpop and bind it all together
#  #Use the direct-resampling data
#  dat <- rbindlist(lapply(ExpoCast.groups,
#                          function(x) {
#                            tmp <- readRDS(paste0('data/',
#                                                  paste('allchems',
#                                                        x,
#                                                        'dr',
#                                                        'poormetab',
#                                                        poormetab,
#                                                        'fup.censored.dist',
#                                                        fup.censored.dist,
#                                                        model,
#                                                        "FuptoFub",
#                                                        sep='_'),
#                                                  '.Rdata'))
#                            tmp[, ExpoCast.group:=x]
#                            return(tmp)
#                            }))

## ----add_chemnames, eval=FALSE------------------------------------------------
#  chem.dt <- as.data.table(httk::get_cheminfo(info=c('CAS', 'Compound'),
#                                              exclude.fup.zero=FALSE))
#  setnames(chem.dt, 'CAS', 'chemcas')
#  dat <- merge(dat, chem.dt, by='chemcas')

## ----read_toxcast, eval=FALSE-------------------------------------------------
#  #Column names are Assay Endpoint, CASRN, Activity Call, Q, AC50, Emax, Log AC50,
#  #B, T, W, Data Type, Chemical Name.
#  #Replace names containing spaces with names without spaces.
#  #setnames(tc.dt,
#  #         c('Assay Endpoint', 'AC 50', 'Chemical Name','Activity Call'),
#  #         c('Assay.Endpoint', 'AC50', 'Chemical.Name', 'Activity.Call'))

## ----delete_inactives, eval=FALSE---------------------------------------------
#  #Keep only the rows with "Active" calls.
#  #tc.dt.sub <- tc.dt[Activity.Call=="Active",
#  #                   .(Chemical.Name, CASRN, Assay.Endpoint, Activity.Call, AC50)]

## ----compute_ac50_pctiles, eval=FALSE-----------------------------------------
#  ac50pct <- tc.dt.sub[,
#                       as.list(quantile(AC50,
#                                        probs=c(0,0.05,0.1,
#                                                0.25,0.5,0.75,
#                                                0.9,0.95,1),
#                                        na.rm=TRUE)),
#                       keyby=CASRN]

## ----fix_ac50_names, eval=FALSE-----------------------------------------------
#  pctnames <- grep(names(ac50pct), pattern='%',value=TRUE)
#  setnames(ac50pct,
#           pctnames,
#           gsub(x=pctnames,
#                pattern='(\\d{1,3})\\%',
#                replacement='AC50p\\1',
#                perl=TRUE)
#           )
#  #While we're at it, let's change the name of the CAS column to comport with its
#  #name in the Css data
#  setnames(ac50pct, 'CASRN', 'chemcas')

## ----compute_oed, eval=FALSE--------------------------------------------------
#  #merge in the AC50 percentile data
#  m.tmp <- merge(dat,ac50pct,by='chemcas')
#  #find column names beginning with "AC50"
#  #to use for naming the OED columns
#  ac50names <- grep(x=names(m.tmp),
#                    pattern='^AC50',
#                    value=TRUE,
#                    perl=TRUE)
#  #Compute OEDs
#  m.tmp[, (paste('oed', ac50names, sep='.')):=lapply(.SD,
#                                                     function(x) x/m.tmp[, css95]),
#        .SDcols=ac50names]

## ----compute_oed_css, eval=FALSE----------------------------------------------
#  #merge in the AC50 percentile data
#  m.css <- merge(dat,ac50pct,by='chemcas')
#  #find column names beginning with "css"
#  #to use for naming the OED columns
#  cssnames <- grep(x=names(m.css),
#                    pattern='^css',
#                    value=TRUE,
#                    perl=TRUE)
#  #Compute OEDs
#  m.css[, (paste('oed', cssnames, sep='.')):=lapply(.SD,
#                                                     function(x) m.css[, AC50p10]/x),
#        .SDcols=cssnames]

## ----compute_aer, eval=FALSE--------------------------------------------------
#  #Merge in the exposure data
#  m.tmp <- merge(m.tmp,onlyp,
#                 by=c("chemcas", "ExpoCast.group"))
#  m.css <- merge(m.css,onlyp,
#                 by=c("chemcas", "ExpoCast.group"))
#  #Compute AER
#  m.tmp[, aer:=oed.AC50p10/exposure.median.95CI.upper]
#  m.css[, aer:=oed.css95/exposure.median.95CI.upper]

## ----shorten_chemnames, eval=FALSE--------------------------------------------
#  #Shorten a couple of compound names for display
#  m.tmp[Compound=="O-ethyl o-(p-nitrophenyl) phenylphosphonothioate",
#        Compound:="Phosphonothioic acid"]
#  m.tmp[Compound=="4-(1,1,3,3-tetramethylbutyl)phenol",
#        Compound:="p-tert-Octylphenol"]
#  m.css[Compound=="O-ethyl o-(p-nitrophenyl) phenylphosphonothioate",
#        Compound:="Phosphonothioic acid"]
#  m.css[Compound=="4-(1,1,3,3-tetramethylbutyl)phenol",
#        Compound:="p-tert-Octylphenol"]

## ----keep_only_total, eval=FALSE----------------------------------------------
#  #Plot only for the total population
#  m.Total <- m.tmp[ExpoCast.group=='Total',]
#  m.css.Total <- m.css[ExpoCast.group=='Total',]

## ----order_chemnames_by_aer, eval=FALSE---------------------------------------
#  #Order the chemicals by AER
#  setorder(m.Total, aer)
#  #Get a list of ordered chemical names so that ggplot2 will plot them in the
#  #right order (as opposed to its default alphabetical order)
#  cpdlevels <- m.Total[, Compound]
#  m.Total[, Compound.factor:=factor(Compound, levels=cpdlevels)]
#  m.tmp[, Compound.factor:=factor(Compound, levels=cpdlevels)]
#  
#  #Order the chemicals by AER
#  setorder(m.css.Total, aer)
#  #Get a list of ordered chemical names so that ggplot2 will plot them in the
#  #right order (as opposed to its default alphabetical order)
#  cpdlevels <- m.css.Total[, Compound]
#  m.css.Total[, Compound.factor:=factor(Compound, levels=cpdlevels)]
#  m.css[, Compound.factor:=factor(Compound, levels=cpdlevels)]

## ----oed_boxplot_boxes, eval=FALSE--------------------------------------------
#  #Start the plot: first, make the OED boxes, ranging between 25th and 75th
#  #percentile AC50, with a crossbar at median AC50.
#  p <- ggplot(data=m.Total) +
#    geom_crossbar(aes(x=Compound.factor,y=oed.AC50p50,
#                      ymin=oed.AC50p25,ymax=oed.AC50p75))

## ----oed_boxplot_whiskers, eval=FALSE-----------------------------------------
#  p <- p +
#    geom_linerange(aes(x=Compound.factor,
#                       ymin=oed.AC50p75,
#                       ymax=oed.AC50p90))+
#    geom_linerange(aes(x=Compound.factor,
#                       ymin=oed.AC50p10,
#                       ymax=oed.AC50p25))

## ----oed_boxplot_points, eval=FALSE-------------------------------------------
#  p <- p +
#    geom_point(aes(x=Compound.factor, y=oed.AC50p95)) +
#    geom_point(aes(x=Compound.factor, y=oed.AC50p5))

## ----oed_boxplot_exposure_boxes, eval=FALSE-----------------------------------
#  p <- p +
#    geom_crossbar(aes(x=Compound.factor, y=exposure.median,
#                      ymin=exposure.median.95CI.lower,
#                      ymax=exposure.median.95CI.upper),
#                  color='#FC8D62')

## ----oed_boxplot_tweak, eval=FALSE--------------------------------------------
#  p <- p +
#    scale_y_log10()+
#    theme_bw()+
#    theme(axis.text.x = element_text(size = 12, angle = 60,
#                                     hjust = 1, colour = "grey50"),
#          axis.ticks.x = element_line(size=0.01, color = 'grey50'),
#          legend.title = element_text(size=rel(1.2)),
#          legend.text = element_text(size=rel(1.2))) +
#    labs(x='Compound',
#         y='OED or Inferred Exposure, \n mg/kg/day')

## ----oed_boxplot_save, fig.width=14, fig.height=11, eval=FALSE----------------
#  ggsave(plot=p, filename=paste0('pdf_figures/',
#                                 paste('oed_exposure_plot',model,
#                                       'poormetab', poormetab,
#                                       'fupcensor', fup.censored.dist,
#                                       "FuptoFub",
#                                       'Total', 'OEDdistoverAC50',
#                                       sep='_'),'.pdf'),
#         width=14,height=8.5)
#  print(p)

## ----oed_css_boxplot, fig.width=14, fig.height=11, eval=FALSE-----------------
#  p <- ggplot(data=m.css.Total) +
#    geom_crossbar(aes(x=Compound.factor,y=oed.css50,
#                      ymin=oed.css75,
#                      ymax=oed.css25))+
#    geom_linerange(aes(x=Compound.factor,
#                       ymin=oed.css25,
#                       ymax=oed.css10))+
#    geom_linerange(aes(x=Compound.factor,
#                       ymin=oed.css90,
#                       ymax=oed.css75)) +
#    geom_point(aes(x=Compound.factor, y=oed.css5)) +
#    geom_point(aes(x=Compound.factor, y=oed.css95))+
#    geom_crossbar(aes(x=Compound.factor, y=exposure.median,
#                      ymin=exposure.median.95CI.lower,
#                      ymax=exposure.median.95CI.upper),
#                  color='#FC8D62')+
#    scale_y_log10()+
#    theme_bw()+
#    theme(axis.text.x = element_text(size = 12, angle = 60,
#                                     hjust = 1, colour = "grey50"),
#          axis.ticks.x = element_line(size=0.01, color = 'grey50'),
#          legend.title = element_text(size=rel(1.2)),
#          legend.text = element_text(size=rel(1.2))) +
#    labs(x='Compound',
#         y='OED or Inferred Exposure, \n mg/kg/day')
#  ggsave(plot=p, filename=paste0('pdf_figures/',
#                                 paste('oed_exposure_plot',model,
#                                       'poormetab', poormetab,
#                                       'fupcensor', fup.censored.dist,
#                                       "FuptoFub",
#                                       'Total', 'OEDdistoverCss',
#                                       sep='_'),'.pdf'),
#         width=14,height=8.5)
#  print(p)

## ----compute_aer_log10diff, eval=FALSE----------------------------------------
#  #Compute difference in log AERs:
#  #log AER group - log AER Total
#  m.tmp[, logAER.diff:=log10(aer)-
#          m.tmp[ExpoCast.group=='Total', log10(aer)],
#        by=ExpoCast.group]
#  #if logAER.diff is positive, then log AER group > log AER Total
#  #if logAER.diff is negative, then log AER group < log AER Total

## ----cast_matrix_aer, eval=FALSE----------------------------------------------
#  #Cast into matrix
#  m.mat <- reshape2::acast(m.tmp[,
#                                 .(Compound.factor, ExpoCast.group, logAER.diff)],
#                           Compound.factor~ExpoCast.group,
#                           value.var='logAER.diff')

## ----diverge_color, eval=FALSE------------------------------------------------
#  #Use a diverging color palette
#  #Based on http://stackoverflow.com/a/10986203 by Josh O'Brien
#  diverge.color <- function(data,pal_choice="RdBu",Thresh=0){
#    #use 100 colors total, and divide the data into 100 bins
#    #meaning that the "halfway" color is bin 50
#    nHalf<-50
#    Min <- min(data,na.rm=TRUE)
#    Max <- max(data,na.rm=TRUE)
#    pal<-RColorBrewer::brewer.pal(n=11,pal_choice)
#    #interpolate gradient between the first two colors
#    rc1<-colorRampPalette(colors=c(pal[1],pal[2]), space="Lab")(10)
#    #interpolate gradient between each succeeding pair of colors
#    for(i in 2:10){
#      tmp<-colorRampPalette(colors=c(pal[i],pal[i+1]), space="Lab")(10)
#      rc1<-c(rc1,tmp)
#      }
#    #calculate the data breaks
#    rb1 <- seq(Min, Thresh, length.out=nHalf+1)
#    rb2 <- seq(Thresh, Max, length.out=nHalf+1)[-1]
#    rampbreaks <- c(rb1, rb2)
#    cuts <- classInt::classIntervals(data, style="fixed", fixedBreaks=rampbreaks)
#    return(list(cuts=cuts,rc1=rc1))
#    }

## ----aer_colors, eval=FALSE---------------------------------------------------
#  brks <- diverge.color(data=c(as.vector(m.mat), -0.8,0.8),pal_choice='RdBu')
#  breaksv <- brks$cuts[['brks']]

## ---- eval=FALSE--------------------------------------------------------------
#  notemat <- matrix(rep("", length(m.mat)), nrow=nrow(m.mat), ncol=ncol(m.mat))
#  notemat[which(is.na(m.mat), arr.ind=TRUE)] <- 'NA'

## ---- eval=FALSE--------------------------------------------------------------
#  hm_args <- list(Rowv=FALSE, #don't reorder rows
#                   dendrogram = 'column',
#                   col=brks$rc1,
#                   breaks=breaksv,
#                   na.col = '#FFFF33',
#                   trace='none',
#                   margin=c(7, 8),
#                   key.xlab=expression(paste(Delta, 'log(AER), Group - Total')),
#                   key.ylab="Count",
#                   key.title=NA,
#                   srtCol=40,
#                   adjCol=c(1,1),
#                   denscol='black',
#                   cellnote = notemat,
#                   notecex=0.7,
#                   notecol='black',
#                   cexRow=0.8)

## ----plot_heatmap_aer, fig.width=12, fig.height=12, eval=FALSE----------------
#  pdf(paste0('pdf_figures/',
#             paste('deltaAERheatmap',
#                   'model',model,
#                   'poormetab',poormetab,
#                   'fupcensor',fup.censored.dist,
#                   "FuptoFub",
#                   'test',
#                   sep='_'), '.pdf'), pointsize=10)
#  hm1 <- do.call(what=heatmap.2,
#                 args=c(list(x=m.mat),
#                        hm_args))
#  dev.off()
#  hm1 <- do.call(what=heatmap.2,
#                 args=c(list(x=m.mat),
#                        hm_args))

## ----plot_heatmap_aer_annotate, fig.width=12, fig.height=12, eval=FALSE-------
#  m.Total[, aer.ordermag:=findInterval(log10(aer), 0:8)]
#  cols <- RColorBrewer::brewer.pal(name='Set3', n=9)
#  colvect <- cols[m.Total[,aer.ordermag+1]]
#  pdf(paste0('pdf_figures/',
#             paste('deltaAERheatmap',
#                   'model',model,
#                   'poormetab',poormetab,
#                   'fupcensor',fup.censored.dist,
#                   "FuptoFub",
#                   'bigger', 'annotated',
#                   sep='_'), '.pdf'),
#      pointsize = 10)
#  hm1.ann <- do.call(what=heatmap.2,
#                     args=c(list(x=m.mat,
#                                 RowSideColors=colvect),
#                            hm_args))
#  dev.off()
#  hm1.ann <- do.call(what=heatmap.2,
#                     args=c(list(x=m.mat,
#                                 RowSideColors=colvect),
#                            hm_args))

## ----compute_log10diff_oed, eval=FALSE----------------------------------------
#  #Likewise, compute differences in log OEDs
#  m.tmp[, logOED.diff:=log10(oed.AC50p10)-
#          m.tmp[ExpoCast.group=='Total', log10(oed.AC50p10)],
#        by=ExpoCast.group]

## ----cast_matrix_oed, eval=FALSE----------------------------------------------
#  m.mat2 <- reshape2::acast(m.tmp[,
#                                  .(Compound.factor, ExpoCast.group, logOED.diff)],
#                            Compound.factor~ExpoCast.group,
#                            value.var='logOED.diff')

## ----oed_color, eval=FALSE----------------------------------------------------
#  brks <- diverge.color(data=c(as.vector(m.mat2), -0.8,0.8), pal_choice='RdBu')
#  breaksv <- brks$cuts[['brks']]

## ----oed_order_subpops, eval=FALSE--------------------------------------------
#  m.mat2 <- m.mat2[, hm1$colInd]

## ----plot_heatmap_oed, fig.width=12, fig.height=12, eval=FALSE----------------
#  hm_args_oed <- hm_args
#  hm_args_oed$col <- brks$rc1
#  hm_args_oed$breaks <- breaksv
#  hm_args_oed$key.xlab<-expression(paste(Delta, 'log(OED), Group - Total'))
#  hm_args_oed$Colv <- FALSE
#  hm_args_oed$cellnote <- NULL
#  pdf(paste0('pdf_figures/',
#             paste('deltaOEDheatmap',
#                   'model',model,
#                   'poormetab',poormetab,
#                   'fupcensor',fup.censored.dist,
#                   "FuptoFub",
#                   sep='_'),
#             '.pdf'), pointsize=10)
#  do.call(what=heatmap.2,
#          args=c(list(x=m.mat2),
#                 hm_args_oed))
#  dev.off()
#  do.call(what=heatmap.2,
#          args=c(list(x=m.mat2),
#                 hm_args_oed))

## ----plot_heatmap_exposure, fig.width=12, fig.height=12, eval=FALSE-----------
#  #And likewise, compute differences in log exposures
#  m.tmp[, logexposure.diff:=log10(exposure.median.95CI.upper)-
#          m.tmp[ExpoCast.group=='Total', log10(exposure.median.95CI.upper)],
#        by=ExpoCast.group]
#  #Cast into matrix
#  m.mat3 <- reshape2::acast(m.tmp[,
#                        .(Compound.factor, ExpoCast.group, logexposure.diff)],
#                  Compound.factor~ExpoCast.group,
#                  value.var='logexposure.diff')
#  #Put columns in same order as for AER heatmap
#  m.mat3 <- m.mat3[,hm1$colInd]
#  #Make cell labeling matrix
#  notemat <- matrix(rep("", length(m.mat3)), nrow=nrow(m.mat3), ncol=ncol(m.mat3))
#  notemat[which(is.na(m.mat3), arr.ind=TRUE)] <- 'NA'
#  #Set up diverging colormap
#  brks <- diverge.color(data=c(as.vector(m.mat3),-0.8,0.8), pal_choice='RdBu')
#  breaksv <- brks$cuts[['brks']]
#  
#  hm_args_exp <- hm_args
#  hm_args_exp$col <- rev(brks$rc1)
#  hm_args_exp$breaks <- breaksv
#  hm_args_exp$key.xlab<-expression(paste(Delta, 'log(exposure), Group - Total'))
#  hm_args_exp$Colv <- FALSE
#  hm_args_exp$cellnote <- notemat
#  #And make the plot
#  pdf(paste0('pdf_figures/',
#             paste('deltaexposureheatmap',
#                   'model', model,
#                   'poormetab', poormetab,
#                   'fupcensor', fup.censored.dist,
#                   "FuptoFub",
#                   sep='_'),
#             '.pdf'), pointsize=10)
#  do.call(what=heatmap.2,
#          args=c(list(x=m.mat3),
#                 hm_args_exp))
#  dev.off()
#  do.call(what=heatmap.2,
#          args=c(list(x=m.mat3),
#                 hm_args_exp))

## ---- eval=FALSE--------------------------------------------------------------
#  setorder(m.tmp, ExpoCast.group, aer)
#  write.table(m.tmp[, .(Compound, ExpoCast.group, css95, oed.AC50p10, exposure.median.95CI.upper, aer)], file="data/aer_data_FuptoFub.txt", row.names=FALSE)

