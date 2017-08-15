## ---- include=FALSE------------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, 
                      comment = '#>', 
                      fig.width=11.25, 
                      fig.height=19)

## ------------------------------------------------------------------------
library(httk)
library(survey)
library(data.table)
library(ggplot2)
all.reth <- levels(nhanes_mec_svy$variables[, ridreth1])
all.gendr <- levels(nhanes_mec_svy$variables[, riagendr])

## ----hematocrit_spline, eval=FALSE---------------------------------------
#  par(mfcol=c(5,2),
#      mar=c(2,2,2.5,2)+0.1,
#      oma=c(4,4,4,0),
#      mgp=c(1,1,0))
#  
#  for (gendr in all.gendr){
#    for (r in all.reth){
#      grsub <- subset(nhanes_mec_svy, riagendr==gendr & ridreth1==r & is.finite(loglbxhct))
#      svyplot(loglbxhct~ridexagm, grsub, style="transparent",
#              basecol="gray50", xlab=NA, ylab=NA, cex.axis=1.5)
#      lines(sort(unique(grsub$variables[,ridexagm])),
#            predict(spline_hematocrit[gender==gendr & reth==r,
#                                      hct_spline[[1]]],
#                    sort(unique(grsub$variables[,ridexagm])))$y)
#      title(paste(gendr, r), line=0.5, cex.main=1.5)
#      }
#    }
#  title(xlab="Age, months",
#        ylab="Log hematocrit, log %",
#        outer=TRUE,
#        line=2, cex.lab=2)
#  title(main="Hematocrit vs. age, with spline fits",
#        outer=TRUE,
#        line=1, cex.main=2)

## ----hematocrit_spline_pdf, echo=FALSE, eval=FALSE-----------------------
#  pdf("pdf_figures/hematocrit_splines.pdf", width=8.5, height=11)
#  par(mfcol=c(5,2),
#      mar=c(2,2,2.5,2)+0.1,
#      oma=c(4,4,4,0),
#      mgp=c(1,1,0))
#  
#  for (gendr in all.gendr){
#    for (r in all.reth){
#      grsub <- subset(nhanes_mec_svy, riagendr==gendr & ridreth1==r & !is.na(loglbxhct))
#      svyplot(loglbxhct~ridexagm, grsub, style="transparent",
#              basecol="gray50", xlab=NA, ylab=NA, cex.axis=1.5)
#      lines(sort(unique(grsub$variables[,ridexagm])),
#            predict(spline_hematocrit[gender==gendr & reth==r,
#                                      hct_spline[[1]]],
#                    sort(unique(grsub$variables[,ridexagm])))$y)
#      title(paste(gendr, r), line=0.5, cex.main=1.5)
#      }
#    }
#  title(xlab="Age, months",
#        ylab="Log hematocrit, log %",
#        outer=TRUE,
#        line=2, cex.lab=2)
#  title(main="Hematocrit vs. age, with spline fits",
#        outer=TRUE,
#        line=1, cex.main=2)
#  dev.off()

## ----hematocrit_resid, eval=FALSE----------------------------------------
#  par(mfcol=c(5,2),
#      mar=c(2,2,2.5,2)+0.1,
#      oma=c(4,4,4,0),
#      mgp=c(1,1,0))
#  
#  for (gendr in all.gendr){
#    for (r in all.reth){
#      grsub <- subset(nhanes_mec_svy, riagendr==gendr & ridreth1==r & !is.na(loglbxhct))
#      grsub <- update(grsub, loglbxhct_resid=loglbxhct-
#                        predict(spline_hematocrit[gender==gendr &
#                                                      reth==r,
#                                                    hct_spline[[1]]],
#                                grsub$variables[,ridexagm])$y)
#      svyplot(loglbxhct_resid~ridexagm, grsub, style="transparent",
#              xlab=NA, ylab=NA, cex.axis=1.5)
#      title(paste(gendr, r), line=0.5, cex.main=1.5)
#      }
#    }
#  title(xlab="Age, months",
#        ylab="Log hematocrit residual, log %",
#        outer=TRUE,
#        line=2, cex.lab=2)
#  title(main = "Hematocrit residuals vs. age",
#        outer=TRUE, line=1, cex.main=2)

## ---- eval=FALSE---------------------------------------------------------
#  par(mfcol=c(5,2),
#      mar=c(2,2,2.5,2)+0.1,
#      oma=c(4,4,4,0),
#      mgp=c(1,1,0))
#  
#  for (gendr in all.gendr){
#    for (r in all.reth){
#      grsub <- subset(nhanes_mec_svy, riagendr==gendr & ridreth1==r & !is.na(loglbxhct))
#      grsub <- update(grsub, loghctresid=loglbxhct-
#                        predict(spline_hematocrit[gender==gendr &
#                                                      reth==r,
#                                                    hct_spline[[1]]],
#                                grsub$variables[,ridexagm])$y)
#  qprobs <- ppoints(n=length(grsub$prob))
#  norm.q <- qnorm(p=qprobs)
#  loghctresid.q <- svyquantile(~loghctresid,
#                              design=grsub,
#                              quantiles=qprobs)
#  plot(y=loghctresid.q,
#       x=norm.q, #Plot weighted points
#       type='p',
#       col="gray70",
#       xlab=NA, ylab=NA)
#  #Add qqline: passing through first and third quartiles
#  x <- qnorm(p=c(0.25, 0.75))
#  y <- as.vector(svyquantile(~loghctresid,
#                             design=grsub,
#                             quantiles=c(0.25, 0.75)))
#  m <- diff(y)/diff(x)
#  intercept <- -m*x[1]+y[1]
#  abline(a=intercept, b=m)
#  title(paste(gendr, r), line=0.5, cex.main=1.5)
#  }
#  }
#  title(xlab='Normal quantiles',
#       ylab='log Hct resid quantiles',
#       outer=TRUE,
#       line=2,
#       cex.lab=2)

## ---- eval=FALSE---------------------------------------------------------
#  par(mfcol=c(5,2),
#      mar=c(2,2,2.5,2)+0.1,
#      oma=c(4,4,4,0),
#      mgp=c(1,1,0))
#  
#  for (gendr in all.gendr){
#    for (r in all.reth){
#      grsub <- subset(nhanes_mec_svy, riagendr==gendr & ridreth1==r & !is.na(loglbxhct))
#      grsub <- update(grsub, loghctresid=loglbxhct-
#                        predict(spline_hematocrit[gender==gendr &
#                                                      reth==r,
#                                                    hct_spline[[1]]],
#                                grsub$variables[,ridexagm])$y)
#      tmp.kde <- ks::kde(x=grsub$variables[, loghctresid],
#                     w=grsub$variables[, wtmec6yr])
#      plot(tmp.kde, xlab=NA, ylab=NA, xlim=c(-0.65,0.4), drawpoints=TRUE, col.pt="black")
#      title(paste(gendr, r), line=0.5, cex.main=1.5)
#      }
#    }
#  title(xlab='Log hematocrit residual',
#       ylab='KDE density',
#       outer=TRUE,
#       line=2,
#       cex.lab=2)

