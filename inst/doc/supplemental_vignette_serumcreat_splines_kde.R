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

## ----serumcreat_spline, eval=FALSE---------------------------------------
#  par(mfcol=c(5,2),
#      mar=c(2,2,2.5,2)+0.1,
#      oma=c(4,4,4,0),
#      mgp=c(1,1,0))
#  
#  for (gendr in all.gendr){
#    for (r in all.reth){
#      grsub <- subset(nhanes_mec_svy, riagendr==gendr & ridreth1==r & !is.na(ridexagm) & !is.na(loglbxscr))
#      svyplot(loglbxscr~ridexagm, grsub, style="transparent",
#              basecol="gray50", xlab=NA, ylab=NA, cex.axis=1.5)
#      lines(sort(unique(grsub$variables[,ridexagm])), predict(spline_serumcreat[gender==gendr & reth==r,
#                                                                 sc_spline[[1]]],
#                                    sort(unique(grsub$variables[,ridexagm])))$y)
#      title(paste(gendr, r), line=0.5, cex.main=1.5)
#    }
#  }
#  title(xlab="Age, months",
#        ylab="Log serum creatinine, log g/dL",
#        outer=TRUE,
#        line=2, cex.lab=2)
#  title(main="Serum creatinine vs. age, with spline fits",
#        outer=TRUE,
#        line=1, cex.main=2)

## ----serumcreat_spline_pdf, echo=FALSE, eval=FALSE-----------------------
#  pdf("pdf_figures/serumcreat_spline.pdf", width=8.5, height=11)
#  par(mfcol=c(5,2),
#      mar=c(2,2,2.5,2)+0.1,
#      oma=c(4,4,4,0),
#      mgp=c(1,1,0))
#  
#  for (gendr in all.gendr){
#    for (r in all.reth){
#      grsub <- subset(nhanes_mec_svy, riagendr==gendr & ridreth1==r & !is.na(ridexagm) & !is.na(loglbxscr))
#      svyplot(loglbxscr~ridexagm, grsub, style="transparent",
#              basecol="gray50", xlab=NA, ylab=NA, cex.axis=1.5)
#      lines(sort(unique(grsub$variables[,ridexagm])), predict(spline_serumcreat[gender==gendr & reth==r,
#                                                                 sc_spline[[1]]],
#                                    sort(unique(grsub$variables[,ridexagm])))$y)
#      title(paste(gendr, r), line=0.5, cex.main=1.5)
#    }
#  }
#  title(xlab="Age, months",
#        ylab="Log serum creatinine, log g/dL",
#        outer=TRUE,
#        line=2, cex.lab=2)
#  title(main="Serum creatinine vs. age, with spline fits",
#        outer=TRUE,
#        line=1, cex.main=2)
#  dev.off()

## ----serumcreat_resid, eval=FALSE----------------------------------------
#  par(mfcol=c(5,2),
#      mar=c(2,2,2.5,2)+0.1,
#      oma=c(4,4,4,0),
#      mgp=c(1,1,0))
#  
#  for (gendr in all.gendr){
#    for (r in all.reth){
#      grsub <- subset(nhanes_mec_svy, riagendr==gendr & ridreth1==r & !is.na(ridexagm) & !is.na(loglbxscr))
#      grsub <- update(grsub, loglbxscr_resid=loglbxscr-
#                        predict(spline_serumcreat[gender==gendr &
#                                                      reth==r,
#                                                    sc_spline[[1]]],
#                                grsub$variables[,ridexagm])$y)
#      svyplot(loglbxscr_resid~ridexagm, grsub, style="transparent",
#              xlab=NA, ylab=NA, cex.axis=1.5)
#      title(paste(gendr, r), line=0.5, cex.main=1.5)
#      }
#    }
#  title(xlab="Age, months",
#        ylab="Log serum creatinine residual, log g/dL",
#        outer=TRUE,
#        line=2, cex.lab=2)
#  title(main = "Serum creatinine residuals vs. age",
#        outer=TRUE, line=1, cex.main=2)

## ---- eval=FALSE---------------------------------------------------------
#  par(mfcol=c(5,2),
#      mar=c(2,2,2.5,2)+0.1,
#      oma=c(4,4,4,0),
#      mgp=c(1,1,0))
#  
#  for (gendr in all.gendr){
#    for (r in all.reth){
#      grsub <- subset(nhanes_mec_svy, riagendr==gendr & ridreth1==r & !is.na(ridexagm) & !is.na(loglbxscr))
#      grsub <- update(grsub, logscresid=loglbxscr-
#                        predict(spline_serumcreat[gender==gendr &
#                                                      reth==r,
#                                                    sc_spline[[1]]],
#                                grsub$variables[,ridexagm])$y)
#  qprobs <- ppoints(n=length(grsub$prob))
#  norm.q <- qnorm(p=qprobs)
#  logscresid.q <- svyquantile(~logscresid,
#                              design=grsub,
#                              quantiles=qprobs)
#  plot(y=logscresid.q,
#       x=norm.q, #Plot weighted points
#       type='p',
#       col="gray70",
#       xlab=NA, ylab=NA)
#  #Add qqline: passing through first and third quartiles
#  x <- qnorm(p=c(0.25, 0.75))
#  y <- as.vector(svyquantile(~logscresid,
#                             design=grsub,
#                             quantiles=c(0.25, 0.75)))
#  m <- diff(y)/diff(x)
#  intercept <- -m*x[1]+y[1]
#  abline(a=intercept, b=m)
#  title(paste(gendr, r), line=0.5, cex.main=1.5)
#  }
#  }
#  title(xlab='Normal quantiles',
#       ylab='log SC resid quantiles',
#       outer=TRUE,
#       line=2,
#       cex.lab=2)

## ---- eval=FALSE---------------------------------------------------------
#  
#  par(mfcol=c(5,2),
#      mar=c(2,2,2.5,2)+0.1,
#      oma=c(4,4,4,0),
#      mgp=c(1,1,0))
#  
#  for (gendr in all.gendr){
#    for (r in all.reth){
#       grsub <- subset(nhanes_mec_svy, riagendr==gendr & ridreth1==r & !is.na(ridexagm) & !is.na(loglbxscr))
#      grsub <- update(grsub, logscresid=loglbxscr-
#                        predict(spline_serumcreat[gender==gendr &
#                                                      reth==r,
#                                                    sc_spline[[1]]],
#                                grsub$variables[,ridexagm])$y)
#       tmp.kde <- ks::kde(x=grsub$variables[, logscresid],
#                     w=grsub$variables[, wtmec6yr])
#      plot(tmp.kde, xlab=NA, ylab=NA, xlim=c(-1.72,2.7), drawpoints=TRUE, col.pt="black")
#      title(paste(gendr, r), line=0.5, cex.main=1.5)
#      }
#    }
#  title(xlab='Log serum creatinine residual',
#       ylab='KDE density',
#       outer=TRUE,
#       line=2,
#       cex.lab=2)

