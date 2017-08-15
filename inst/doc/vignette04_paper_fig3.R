## ---- include=FALSE------------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, 
                      comment = '#>', 
                      fig.width=7, 
                      fig.height=7)

## ------------------------------------------------------------------------
library('data.table')
library('ggplot2')
library('httk')

## ---- eval=FALSE---------------------------------------------------------
#  poormetab <- TRUE
#  fup.censor <- TRUE
#  grp <- 'Age.20.50.nonobese'
#  model <- '3compartmentss'
#  popmethod <- 'dr'
#  
#  m.dat <- readRDS(paste0('data/',
#                          paste('allchems',
#                                grp,
#                                popmethod,
#                                'poormetab',
#                                poormetab,
#                                'fup.censor',
#                                fup.censor,
#                                model,
#                                "FuptoFub",
#                                sep='_'),
#                          '.Rdata'))

## ---- eval=FALSE---------------------------------------------------------
#  #Convert clearance in mL/min/kg to Css in mg/L for a dose of 1 mg/kg/day
#  Obach2008[,"Css (mg/L)"] <- 1/
#    (24*60*
#       as.numeric(Obach2008[,
#                            "CL (mL/min/kg)"])/
#       1000) # 1 mg/kg/day / L/day/kg -> mg/L
#  literature.Css <- Obach2008[,c("Name","CAS","Css (mg/L)")]
#  Obach2008.dt <- as.data.table(literature.Css)

## ---- eval=FALSE---------------------------------------------------------
#  Wetmore2012 <- Wetmore2012[c(1:2,13,3:12),]
#  Wetmore2012.Css <- Wetmore2012[,c("Chemical","CAS","Literature.Css")]
#  Wetmore2012.Css[, 'Literature.Css'] <- gsub(x=Wetmore2012.Css[,
#                                                                'Literature.Css'],
#                                              pattern=',',
#                                              replacement='')
#  #strip commas from Css values for proper conversion
#  Wetmore2012.Css[, "Literature.Css"] <- as.numeric(Wetmore2012.Css[,
#                                                                    "Literature.Css"])
#  colnames(Wetmore2012.Css) <- c("Name","CAS","Css (mg/L)")
#  Wetmore2012.Css <- Wetmore2012.Css[!is.na(Wetmore2012.Css[,"Css (mg/L)"]),]
#  Wetmore2012.dt <- as.data.table(Wetmore2012.Css)

## ---- eval=FALSE---------------------------------------------------------
#  litCss.dt <- rbind(Obach2008.dt, Wetmore2012.dt)
#  setnames(litCss.dt, c('Name', 'Css (mg/L)'), c('Compound', 'litcss'))

## ---- eval=FALSE---------------------------------------------------------
#  #MW = weight in grams per mole of substance
#  #MW*1000 = weight in mg per mole of substance
#  #MW*1000/10^6 = MW/1000 = weight in mg per umol of substance
#  #Css in mg/L divided by MW in mg/umol = umol/L = uM
#  
#  #get MW from HTTK cheminfo
#  chemlist <- as.data.table(httk::get_cheminfo(info=c('CAS', 'Compound', 'MW'),
#                                               species='Human',
#                                               model=model,
#                                               exclude.fub.zero=FALSE))
#  setnames(chemlist, 'CAS', 'chemcas')
#  litCss.dt <- merge(litCss.dt,
#                     chemlist[, .(Compound, MW)],
#                     by='Compound')
#  #Now merge with litcss
#  setnames(litCss.dt,
#           'CAS',
#           'chemcas')
#  litCss.dt[, litcss:=litcss/(MW/1000)]

## ---- eval=FALSE---------------------------------------------------------
#  tmp.med <- merge(m.dat[, .(chemcas, css50)],
#                   litCss.dt,
#                   by=c('chemcas')
#                   )

## ---- eval=FALSE---------------------------------------------------------
#  mymodel <- lm(log10(css50)~log10(litcss), data=tmp.med)
#  summary(mymodel)

## ---- eval=FALSE---------------------------------------------------------
#  p <- ggplot(data=tmp.med)+
#    geom_point(aes(y=css50, #Plot data points
#                   x=litcss),
#               shape=21, #As open circles
#               size=4) +
#    stat_smooth(data=tmp.med, #Plot best-fit line
#                mapping=aes(x=litcss, y=css50), #data will be log10-transformed before this is done, so this is the same as the linear regression above
#                method="lm",
#                linetype=2, #dashed line
#                size=1,
#                color="black",
#                alpha=0.25, #shading for confidence interval
#                level=0.99) + #99% CI
#    geom_abline(slope=1,intercept=0, #identity line
#                linetype=1, size=1, color="black")+
#    scale_x_log10() + #scale x data as log10
#    scale_y_log10() + #scale y data as log10
#    labs(x='Literature median Css (uM)',
#         y='HTTK-Pop median Css (uM)') +
#    theme_bw()+
#    theme(axis.text.x=element_text(size=rel(2), angle=45, hjust=1),
#          axis.text.y=element_text(size=rel(2)),
#          axis.title.x=element_text(size=rel(2)),
#          axis.title.y=element_text(size=rel(2)))
#  print(p)

## ---- eval=FALSE---------------------------------------------------------
#  #First find the chemicals with the two highest literature Css values
#  setorder(tmp.med, litcss) #sort ascending by literature Css
#  knitr::kable(tmp.med[(nrow(tmp.med)-1):nrow(tmp.med),
#                       .(Compound, chemcas, css50, litcss, MW)],
#               format="markdown",
#               col.names=c("Compound", "CASRN", "Predicted median Css", "Literature median Css", "MW (g/mol)"))

## ---- eval=FALSE---------------------------------------------------------
#  tmp.med2 <- tmp.med[1:(nrow(tmp.med)-2), ] #remove the highest two lit Css chemicals
#  mymodel2 <- lm(log10(css50)~log10(litcss), data=tmp.med2) #re-do regression
#  summary(mymodel2)

## ---- eval=FALSE---------------------------------------------------------
#  p2 <- p + stat_smooth(data=tmp.med2,#Add new best-fit line
#                        mapping=aes(x=litcss, y=css50),
#                        method="lm",
#                        color="black",
#                        linetype=3, #dotted
#                        size=1.2, #slightly increase thickness
#                        level=0.99, #plot 99% CI shading
#                        alpha=0.25)
#  print(p2)
#  ggsave(filename="pdf_figures/Figure3_FuptoFub.pdf",
#         plot=p2,
#         height=11, width=14)

## ---- eval=FALSE---------------------------------------------------------
#  offset.model <- lm(log10(css50)~offset(log10(litcss)), data=tmp.med) #forces slope of 1
#  summary(offset.model)
#  anova(mymodel, offset.model) #Compare original regression to offset model
#  
#  offset.model2 <- lm(log10(css50)~offset(log10(litcss)), data=tmp.med2) #forces slope of 1
#  summary(offset.model2)
#  anova(mymodel2, offset.model2) #Compare second regression to offset model

## ---- eval=FALSE---------------------------------------------------------
#  tmp.med[, predicted.over.lit:=css50/litcss]
#  setorder(tmp.med, predicted.over.lit)

## ---- echo=FALSE, eval=FALSE---------------------------------------------
#  knitr::kable(tmp.med[, .(Compound, chemcas, css50, litcss, predicted.over.lit)],
#               format='markdown',
#               col.names=c("Compound", "CASRN", "Median predicted Css", "Median literature Css", "Ratio of median predicted to median literature Css"),
#               align=rep("l", 5))

## ---- eval=FALSE---------------------------------------------------------
#  tmp.med[, sum(predicted.over.lit<=10 & predicted.over.lit>=0.1)]

## ---- eval=FALSE---------------------------------------------------------
#  tmp.med[, sum(predicted.over.lit<=5 & predicted.over.lit>=0.2)]

## ---- eval=FALSE---------------------------------------------------------
#  tmp.med[, sum(predicted.over.lit<=2 & predicted.over.lit>=0.5)]

## ---- eval=FALSE---------------------------------------------------------
#  tmp.med[, sum(predicted.over.lit>1)]

## ---- eval=FALSE---------------------------------------------------------
#  nrow(tmp.med)

