## ---- include=FALSE------------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, 
                      comment = '#>', 
                      fig.width=12, 
                      fig.height=12)

## ------------------------------------------------------------------------
library('data.table')
library('GGally')
library('ggplot2')

## ---- eval=FALSE---------------------------------------------------------
#  subpop <- 'Age.20.65'
#  pop.vi <- readRDS(paste0('data/', paste('httkpop',
#                                 'vi',
#                                subpop,
#                                 sep='_'), '.Rdata'))
#  pop.dr <- readRDS(paste0('data/', paste('httkpop',
#                                 'dr',
#                                subpop,
#                                 sep='_'), '.Rdata'))

## ---- eval=FALSE---------------------------------------------------------
#  pop.vi[, which_method:='virtual individuals']
#  pop.dr[, which_method:='direct resampling']

## ---- eval=FALSE---------------------------------------------------------
#  pop.vi[, portalvein_flow:=Liver_flow+
#            Stomach_flow+
#            Small_intestine_flow+
#            Large_intestine_flow]
#  
#  pop.dr[, portalvein_flow:=Liver_flow+
#            Stomach_flow+
#            Small_intestine_flow+
#            Large_intestine_flow]

## ---- eval=FALSE---------------------------------------------------------
#  #List the variables of interest
#  inboth <- c('age_years',
#              'height',
#              'weight_adj',
#              'serum_creat',
#              'hematocrit',
#  'Liver_mass',
#  'portalvein_flow',
#  'million.cells.per.gliver',
#  'gfr_est')
#  bothpop <- rbind(pop.vi[, c(inboth, 'which_method'), with=FALSE],
#                      pop.dr[, c(inboth, 'which_method'), with=FALSE])

## ---- eval=FALSE---------------------------------------------------------
#  setnames(bothpop, inboth,
#           c('Age',
#             'Height',
#             'Weight',
#             'Serum.creat.',
#             'Hematocrit',
#             'Liver.mass',
#             'Portal.vein.flow',
#             'GFR',
#             'Hepatocell.'))
#  inboth <- c('Age',
#             'Height',
#             'Weight',
#             'Serum.creat.',
#             'Hematocrit',
#             'Liver.mass',
#             'Portal.vein.flow',
#             'GFR',
#             'Hepatocell.')

## ---- eval=FALSE---------------------------------------------------------
#    theme_set(theme_bw())
#    p <- ggpairs(data=bothpop,
#                 columns=1:9,
#                 linetype='which_method',
#                 color='which_method',
#                 upper=list(continuous='blank'),
#                 lower=list(continuous='density'),
#                 diag=list(continuous='density'),
#                 axisLabels='show',
#                 legends=TRUE)
#    print(p)
#  pdf("pdf_figures/Figure2.pdf", height=12, width=12)
#  print(p)
#  dev.off()

