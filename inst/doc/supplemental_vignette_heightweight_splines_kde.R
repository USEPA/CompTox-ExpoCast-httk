## ---- include=FALSE------------------------------------------------------
rm(list=ls())
knitr::opts_chunk$set(collapse = TRUE, 
                      comment = '#>', 
                      fig.width=11.25, 
                      fig.height=19)

## ------------------------------------------------------------------------
library(httk)
library(survey)
library(data.table)
library(ks)
library(ggplot2)
all.reth <- levels(nhanes_mec_svy$variables[, ridreth1])
all.gendr <- levels(nhanes_mec_svy$variables[, riagendr])

## ----height_splines, eval=FALSE------------------------------------------
#  par(mfcol=c(5,2),
#      mar=c(2,2,2.5,2)+0.1,
#      oma=c(4,4,4,0),
#      mgp=c(1,1,0))
#  
#  for (gendr in all.gendr){
#    for (reth in all.reth){
#      grsub <- subset(nhanes_mec_svy, riagendr==gendr & ridreth1==reth)
#      svyplot(logbmxhtlenavg~ridexagm, grsub, style="transparent",
#              basecol="gray50", xlab=NA, ylab=NA, cex.axis=1.5)
#      lines(sort(unique(grsub$variables[,ridexagm])), predict(spline_heightweight[g==gendr & r==reth,
#                                                                 height_spline[[1]]],
#                                    sort(unique(grsub$variables[,ridexagm])))$y)
#      title(paste(gendr, reth), line=0.5, cex.main=1.5)
#    }
#  }
#  title(xlab="Age, months",
#        ylab="Log height, log cm",
#        outer=TRUE,
#        line=2, cex.lab=2)
#  title(main="Height vs. age with spline fits",
#        outer=TRUE, line=1, cex.main=2)

## ----height_splines_pdf, echo=FALSE, eval=FALSE--------------------------
#  pdf("pdf_figures/height_splines.pdf", width=8.5, height=11)
#  par(mfcol=c(5,2),
#      mar=c(2,2,2.5,2)+0.1,
#      oma=c(4,4,4,0),
#      mgp=c(1,1,0))
#  
#  for (gendr in all.gendr){
#    for (reth in all.reth){
#      grsub <- subset(nhanes_mec_svy, riagendr==gendr & ridreth1==reth & !is.na(ridexagm))
#      svyplot(logbmxhtlenavg~ridexagm, grsub, style="transparent",
#              basecol="gray50", xlab=NA, ylab=NA, cex.axis=1.5)
#      lines(sort(unique(grsub$variables[,ridexagm])), predict(spline_heightweight[g==gendr & r==reth,
#                                                                 height_spline[[1]]],
#                                    sort(unique(grsub$variables[,ridexagm])))$y)
#      title(paste(gendr, reth), line=0.5, cex.main=1.5)
#    }
#  }
#  title(xlab="Age, months",
#        ylab="Log height, log cm",
#        outer=TRUE,
#        line=2, cex.lab=2)
#  title(main="Height vs. age with spline fits",
#        outer=TRUE, line=1, cex.main=2)
#  dev.off()

## ----height_resid, eval=FALSE--------------------------------------------
#  par(mfcol=c(5,2),
#      mar=c(2,2,2.5,2)+0.1,
#      oma=c(4,4,4,0),
#      mgp=c(1,1,0))
#  
#  for (gendr in all.gendr){
#    for (reth in all.reth){
#      grsub <- subset(nhanes_mec_svy, riagendr==gendr & ridreth1==reth & !is.na(ridexagm))
#      grsub <- update(grsub, logbmxhtlenavg_resid=logbmxhtlenavg-
#                        predict(spline_heightweight[g==gendr &
#                                                      r==reth,
#                                                    height_spline[[1]]],
#                                grsub$variables[,ridexagm])$y)
#  
#      svyplot(logbmxhtlenavg_resid~ridexagm, grsub, style="transparent",
#              xlab=NA, ylab=NA, cex.axis=1.5)
#      title(paste(gendr, reth), line=0.5, cex.main=1.5)
#      }
#    }
#  title(xlab="Age, months",
#        ylab="Log height residual, log cm",
#        outer=TRUE,
#        line=2, cex.lab=2)
#  title(main = "Height residuals vs. age",
#        outer=TRUE,
#        line=1,
#        cex.main=2)

## ----weight_splines, eval=FALSE------------------------------------------
#  par(mfcol=c(5,2),
#      mar=c(2,2,2.5,2)+0.1,
#      oma=c(4,4,4,0),
#      mgp=c(1,1,0))
#  
#  for (gendr in all.gendr){
#    for (reth in all.reth){
#      grsub <- subset(nhanes_mec_svy, riagendr==gendr & ridreth1==reth & !is.na(ridexagm))
#      svyplot(logbmxwt~ridexagm, grsub, style="transparent",
#              basecol="gray50", xlab=NA, ylab=NA, cex.axis=1.5)
#      lines(sort(unique(grsub$variables[,ridexagm])), predict(spline_heightweight[g==gendr & r==reth,
#                                                                 weight_spline[[1]]],
#                                    sort(unique(grsub$variables[,ridexagm])))$y)
#      title(paste(gendr, reth), line=0.5, cex.main=1.5)
#    }
#  }
#  title(xlab="Age, months",
#        ylab="Log weight, log kg",
#        outer=TRUE,
#        line=2, cex.lab=2)
#  title(main = "Weight vs. age with spline fits",
#         outer=TRUE,
#         line=1,
#         cex.main=2)

## ----weight_splines_pdf, echo=FALSE, eval=FALSE--------------------------
#  pdf("pdf_figures/weight_splines.pdf", width=8.5, height=11)
#  par(mfcol=c(5,2),
#      mar=c(2,2,2.5,2)+0.1,
#      oma=c(4,4,4,0),
#      mgp=c(1,1,0))
#  
#  for (gendr in all.gendr){
#    for (reth in all.reth){
#      grsub <- subset(nhanes_mec_svy, riagendr==gendr & ridreth1==reth & !is.na(ridexagm))
#      svyplot(logbmxwt~ridexagm, grsub, style="transparent",
#              basecol="gray50", xlab=NA, ylab=NA, cex.axis=1.5)
#      lines(sort(unique(grsub$variables[,ridexagm])), predict(spline_heightweight[g==gendr & r==reth,
#                                                                 weight_spline[[1]]],
#                                    sort(unique(grsub$variables[,ridexagm])))$y)
#      title(paste(gendr, reth), line=0.5, cex.main=1.5)
#    }
#  }
#  title(xlab="Age, months",
#        ylab="Log weight, log kg",
#        outer=TRUE,
#        line=2, cex.lab=2)
#  title(main = "Weight vs. age with spline fits",
#         outer=TRUE,
#         line=1,
#         cex.main=2)
#  dev.off()

## ----weight_resid, eval=FALSE--------------------------------------------
#  par(mfcol=c(5,2),
#      mar=c(2,2,2.5,2)+0.1,
#      oma=c(4,4,4,0),
#      mgp=c(1,1,0))
#  
#  for (gendr in all.gendr){
#    for (reth in all.reth){
#      grsub <- subset(nhanes_mec_svy, riagendr==gendr & ridreth1==reth & !is.na(ridexagm))
#      grsub <- update(grsub, logbmxwt_resid=logbmxwt-
#                        predict(spline_heightweight[g==gendr &
#                                                      r==reth,
#                                                    weight_spline[[1]]],
#                                grsub$variables[,ridexagm])$y)
#      svyplot(logbmxwt_resid~ridexagm, grsub, style="transparent",
#              xlab=NA, ylab=NA, cex.axis=1.5)
#      title(paste(gendr, reth), line=0.5, cex.main=1.5)
#      }
#    }
#  title(xlab="Age, months",
#        ylab="Log weight residual, log kg",
#        outer=TRUE,
#        line=2, cex.lab=2)
#  title(main = "Weight residuals vs. age",
#        outer=TRUE, line=1, cex.main=2)

## ----hw_resid, eval=FALSE------------------------------------------------
#  par(mfcol=c(5,2),
#      mar=c(2,2,2.5,2)+0.1,
#      oma=c(4,4,4,0),
#      mgp=c(1,1,0))
#  for (gendr in all.gendr){
#    for (reth in all.reth){
#      grsub <- subset(nhanes_mec_svy, riagendr==gendr & ridreth1==reth & !is.na(ridexagm))
#      grsub <- update(grsub, logbmxwt_resid=logbmxwt-
#                        predict(spline_heightweight[g==gendr &
#                                                      r==reth,
#                                                    weight_spline[[1]]],
#                                grsub$variables[,ridexagm])$y)
#      grsub <- update(grsub, logbmxhtlenavg_resid=logbmxhtlenavg-
#                        predict(spline_heightweight[g==gendr &
#                                                      r==reth,
#                                                    height_spline[[1]]],
#                                grsub$variables[,ridexagm])$y)
#      hw_kde <- kde(x=as.matrix(grsub$variables[, .(logbmxwt_resid, logbmxhtlenavg_resid)]),
#                              w=grsub$variables[, wtmec6yr])
#       svyplot(logbmxhtlenavg_resid~logbmxwt_resid, grsub, style="transparent",
#               basecol="gray50",
#              xlab=NA, ylab=NA, cex.axis=1.5)
#      plot(hw_kde, cont=c(10,25,50,75,90,95), display="slice",
#           col="black", labcex=1.1, vfont=c("sans serif", "bold"), add=TRUE)
#      title(paste(gendr, reth), line=0.5, cex.main=1.5)
#      }
#    }
#     title(xlab="Log weight residual, log kg",
#        ylab="Log height residual, log cm",
#        outer=TRUE,
#        line=2, cex.lab=2)
#  title(main = "Height resid vs. weight resid with KDE",
#        outer=TRUE, line=1, cex.main=2)

## ----hw_resid_pdf, echo=FALSE, eval=FALSE--------------------------------
#  pdf("pdf_figures/heightweight_resid.pdf", width=8.5, height=11)
#  par(mfcol=c(5,2),
#      mar=c(2,2,2.5,2)+0.1,
#      oma=c(4,4,4,0),
#      mgp=c(1,1,0))
#  for (gendr in all.gendr){
#    for (reth in all.reth){
#      grsub <- subset(nhanes_mec_svy, riagendr==gendr & ridreth1==reth & !is.na(ridexagm))
#      grsub <- update(grsub, logbmxwt_resid=logbmxwt-
#                        predict(spline_heightweight[g==gendr &
#                                                      r==reth,
#                                                    weight_spline[[1]]],
#                                grsub$variables[,ridexagm])$y)
#      grsub <- update(grsub, logbmxhtlenavg_resid=logbmxhtlenavg-
#                        predict(spline_heightweight[g==gendr &
#                                                      r==reth,
#                                                    height_spline[[1]]],
#                                grsub$variables[,ridexagm])$y)
#      hw_kde <- kde(x=as.matrix(grsub$variables[, .(logbmxwt_resid, logbmxhtlenavg_resid)]),
#                              w=grsub$variables[, wtmec6yr])
#       svyplot(logbmxhtlenavg_resid~logbmxwt_resid, grsub, style="transparent",
#               basecol="gray50",
#              xlab=NA, ylab=NA, cex.axis=1.5)
#      plot(hw_kde, cont=c(10,25,50,75,90,95), display="slice",
#           col="black", labcex=1.1, vfont=c("sans serif", "bold"), add=TRUE)
#      title(paste(gendr, reth), line=0.5, cex.main=1.5)
#      }
#    }
#     title(xlab="Log weight residual, log kg",
#        ylab="Log height residual, log cm",
#        outer=TRUE,
#        line=2, cex.lab=2)
#  title(main = "Height resid vs. weight resid with KDE",
#        outer=TRUE, line=1, cex.main=2)
#  dev.off()

