## ---- include=FALSE------------------------------------------------------
rm(list=ls())
knitr::opts_chunk$set(collapse = TRUE, 
                      comment = '#>', 
                      fig.width=11.25, 
                      fig.height=19)

## ---- eval = FALSE-------------------------------------------------------
#  library(httk)
#  library(survey)
#  library(data.table)
#  library(ggplot2)

## ---- eval=FALSE---------------------------------------------------------
#  all.reth <- levels(nhanes_mec_svy$variables[, ridreth1])
#  all.gendr <- levels(nhanes_mec_svy$variables[, riagendr])

## ---- eval=FALSE---------------------------------------------------------
#  grct <- 1
#  smth.year <- vector(mode="list", length=10)
#  for (gendr in all.gendr){
#    for (r in all.reth){
#      grsub <- subset(nhanes_mec_svy, riagendr==gendr & ridreth1==r & !is.na(ridexagm))
#      smth.list <- vector(mode="list", length=3)
#      ct <- 1
#      for (y in grsub$variables[, levels(sddsrvyr)]){ #by cycle
#        ysub <- subset(grsub, sddsrvyr==y)
#        tmp <- svysmooth(~ridexagm,
#                         ysub,
#                         ngrid=960)$ridexagm
#        smth.list[[ct]] <- c(tmp, list(year=rep(y, length(tmp$x))))
#        ct <- ct + 1
#      }
#      tmp.year <- rbindlist(lapply(smth.list, as.data.table))
#      tmp.year[, gender:=gendr]
#      tmp.year[, reth:=r]
#      smth.year[[grct]] <- tmp.year
#      grct <- grct+1
#    }
#  }
#  #bind with smoothed data for all cycles combined
#  smth.all <- rbind(rbindlist(smth.year),
#                    age_dist_smooth[, smth[[1]], by=.(gender, reth)])
#  smth.all[, gender:=factor(gender, levels=all.gendr)]
#  smth.all[, reth:=factor(reth, levels=all.reth)]

## ---- eval=FALSE---------------------------------------------------------
#  #Plot
#  colvect <- c(RColorBrewer::brewer.pal(3, "Set2"), "black")
#  names(colvect) <- c('NHANES 2007-2008',
#                      'NHANES 2009-2010',
#                      'NHANES 2011-2012',
#                      'Overall')
#  p <- ggplot(data=smth.all) +
#    geom_line(aes(x=x, y=y, color=year)) +
#    scale_color_manual(values=colvect) +
#    labs(x='Age, months',
#         y='Smoothed density estimate') +
#    facet_grid(reth~gender)+
#    theme_bw() +
#    theme(strip.background=element_blank())
#  print(p)
#  ggsave(filename="pdf_figures/age_smooth.pdf",
#         plot=p,
#         width=8.5,
#         height=11)

