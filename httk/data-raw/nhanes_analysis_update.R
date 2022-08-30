### Miyuki Breen
### update Caroline Ring's nhanes_analysis.r to get NHANES 2017-2018, 2015-2016, 2013-2014
### devtools::use_data has been deprecated and transferred to the usethis package 
### using usethis::use_data instead of devtools:use_data 
### June 30, 2020

#' NHANES analysis
#' ========================================================
#'
 
#' This script shows how the pre-processed NHANES survey data is analyzed to
#' produce the smoothed distributions, smoothing splines, and residual KDEs used
#' in the virtual-individuals method of HTTK-Pop.

#'  
#' First, load a couple of packages.
## ----load_libraries------------------------------------------------------
library(data.table)
library(survey)
library(ks)
library(TeachingDemos)

#Load McNally data
mcnally_dt <- fread("McNally2014_data.csv")

#' Pre-process the NHANES data.
source("make_bmiage_dt.R") #loads data.table called bmiage
#usethis::use_data(bmiage, overwrite=TRUE)
source("make_wfl.R") #loads data_table called wfl (weight for length)
#usethis::use_data(wfl, overwrite=TRUE)
source("get_weight_class_init.R")
source("nhanes_init_update.R")
#For reproducibility, first set a seed!
TeachingDemos::char2seed("Caroline Ring")
#Now read in and process the NHANES data.
nhanes_mec_svy <- nhanes_init_update(bmiage, wfl)
#Add the data to the package.
#' 
#' Since everything is done by gender and race/ethnicity, we'll write a function
#' for each item that takes gender and race/ethnicity as input arguments. Then
#' we'll loop over the genders and races/ethnicities only once, calling each
#' function.
#' 

#' 
#' # Height and weight splines and two-dimensional residual KDEs
#' The tasks of this function are as follows:
#' 
#' 1. Subset the data by gender and race/ethnicity, and keep only data where 
#' height and weight data are not NA or NaN. 
#' (Smoothing splines can't be fit with NA data.)
#' 2. Fit a smoothing spline to log height vs. age in months, and compute 
#' the residuals between NHANES and spline-predicted values.
#' 3. Fit a smoothing spline to log weight vs. age in months, and compute 
#' the residuals between NHANES and spline-predicted values.
#' 4. Fit a two-dimensional KDE to the (log weight residual, log height residual)
#'  data.
#' 
## ----heightweight--------------------------------------------------------
heightweight <- function(g, r, nhanes_mec_svy){
  #Take relevant subset of data
  d.sub<-subset(nhanes_mec_svy, 
                riagendr==g & #match gender
                  ridreth1==r &  #match race/ethnicity
                  is.finite(bmxhtlenavg) & #Height is not NA (spline fit will fail)
                  is.finite(bmxwt) #Weight is not NA (spline fit will fail)
                )
  
  #Fit smoothing spline to height vs. age in months
  height_spline <- smooth.spline(x=d.sub$variables[, ridexagm],
                                 y=d.sub$variables[, logbmxhtlenavg],
                                 w=d.sub$variables[, wtmec6yr],
                                 keep.data = FALSE)$fit #keep only the fit; see ?smooth.spline
  #Add predicted log heights and residuals to survey design object
  d.sub <- update(d.sub, 
                  loghpred=predict(height_spline,
                                   x=ridexagm)$y) #age in months at exam
  d.sub <- update(d.sub, 
                  loghresid=logbmxhtlenavg-loghpred)
  #Fit smoothing spline to weight vs. age in months
  weight_spline <- smooth.spline(x=d.sub$variables[, ridexagm],
                                 y=d.sub$variables[, logbmxwt],
                                 w=d.sub$variables[, wtmec6yr],
                                 keep.data = FALSE)$fit #keep only the fit; see ?smooth.spline
  #Add predicted log weights and residuals to survey design object
  #using "update" function of survey package
  d.sub <- update(d.sub, 
                  logwpred=predict(weight_spline,
                                   x=ridexagm)$y) #age in months at exam
  d.sub <- update(d.sub, 
                  logwresid=logbmxwt-logwpred)
  
  #Fit 2-D KDE to log-weight and log-height residuals
  hw_kde <- ks::kde(x=as.matrix(d.sub$variables[, .(logwresid, 
                                                    loghresid)]),
                    w=d.sub$variables[, wtmec6yr])
  #return only the 2-D KDE bandwidth as a list item
  
  return(hw_kde$H)
  }

#' 
#' # Hematocrit spline and KDE fit of residuals
#' The tasks of this function are as follows:
#' 
#' 1. Subset the data by gender and race/ethnicity, and keep only data where
#'  hematocrit is not NA or NaN. 
#'  (Smoothing splines can't be fit with NA or NaN data points.)
#' 2. Fit a smoothing spline to log hematocrit vs. age in months. 
#' Because NHANES only measured hematocrit for respondents ages 1 year or older, 
#' the spline fit is only valid for ages 12 months and above.
#' 3. Get the residuals between NHANES and spline-predicted log hematocrit data.
#' 4. Fit a one-dimensional KDE to the residuals.
#' 
## ----hematocrit----------------------------------------------------------
hematocrit <- function(g, r, nhanes_mec_svy){
  #Take relevant subset of data
  d.sub <- subset(nhanes_mec_svy,
                  riagendr==g & #match gender
                    ridreth1==r & #match race/ethnicity
                    is.finite(loglbxhct)) #ensure log hematocrit is not missing
  #Fit smoothing spline
  hematocrit_spline <- smooth.spline(x=d.sub$variables[, ridexagm],
                                     y=d.sub$variables[, loglbxhct],
                                     w=d.sub$variables[, wtmec6yr],
                                     keep.data = FALSE)$fit
  #Add predicted values and residuals to survey design object
  d.sub <- update(d.sub, 
                  loghctpred=predict(hematocrit_spline,
                                     x=ridexagm)$y)
  d.sub <- update(d.sub,
                  loghctresid=loglbxhct-loghctpred)
  #Fit 1-D KDE to residuals
  hct_kde <- ks::kde(x=d.sub$variables[, loghctresid],
                     w=d.sub$variables[['wtmec6yr']])
  
  #return only the KDE bandwidth

  return(hct_kde$h)
  }

#' 
#' # Serum creatinine
#' The tasks of this function are as follows:
#' 
#' 1. Subset the data by gender and race/ethnicity, and keep only data where 
#' serum creatinine is not NA or NaN. 
#' (Smoothing splines can't be fit with NA or NaN data points.) 
#' 2. Fit a smoothing spline to log serum creatinine vs. age in months. 
#' Because NHANES only gathered serum creatinine data from respondents ages 
#' 12 years and older, the spline fit is only valid for ages 144 months or above.
#' 3. Get the residuals between NHANES and spline-predicted log serum creatinine 
#' data.
#' 4. Fit a one-dimensional KDE to the residuals.
#' 
## ----serumcreat----------------------------------------------------------
serumcreat <- function(g, r, nhanes_mec_svy){
  #Take relevant subset of data
  d.sub <- subset(nhanes_mec_svy,
                  riagendr==g & #match gender
                    ridreth1 == r & #match race/ethnicity
                    is.finite(loglbxscr)) #ensure serum creatinine data isn't missing
  #Fit smoothing spline
  serumcreat_spline <- smooth.spline(x=d.sub$variables[, ridexagm],
                                     y=d.sub$variables[, loglbxscr],
                                     w=d.sub$variables[, wtmec6yr],
                                     keep.data = FALSE)$fit
  #Update survey design object with predicted values and residuals
  d.sub <- update(d.sub, 
                  logscpred=predict(serumcreat_spline,
                                    x=ridexagm)$y)
  
  d.sub <- update(d.sub,
                  logscresid=loglbxscr-logscpred)
  #Fit 1-dimensional KDE to residuals
  sc_kde <- ks::kde(x=d.sub$variables[, logscresid],
                    w=d.sub$variables[['wtmec6yr']])
  
  #return only the KDE bandwidth
 
  return(sc_kde$h)
  }

#' 
#' # Main loop over genders and races/ethnicities
#' Now, let's actually call these functions for all of the combinations of 
#' gender and race/ethnicity.
#' 
#' Get all combinations of genders and races/ethnicities.
## ----gender_race_combinations--------------------------------------------
genders <- nhanes_mec_svy$variables[,levels(riagendr)] 
reths <- nhanes_mec_svy$variables[, levels(ridreth1)]
gr_all <- as.data.table(expand.grid(g=genders,
                                    r=reths,
                                    stringsAsFactors = FALSE))

#' ## Height and weight
#' 2-d KDE bandwidth
#' #get list by gender/reth combinations
DT_hw_kde_H <- mapply(heightweight,
                      g = gr_all$g,
                      r = gr_all$r,
                    MoreArgs = list(nhanes_mec_svy = nhanes_mec_svy),
                  SIMPLIFY =  FALSE)
names(hw_H) <- paste(gr_all$g, gr_all$r)

# Hematocrit
hct_h <- mapply(hematocrit,
                g = gr_all$g,
                r = gr_all$r,
                MoreArgs = list(nhanes_mec_svy = nhanes_mec_svy),
                SIMPLIFY = FALSE)
names(hct_h) <- paste(gr_all$g, gr_all$r)

#Serum creatinine
scr_h <- mapply(serumcreat,
                g = gr_all$g,
                r = gr_all$r,
                MoreArgs = list(nhanes_mec_svy = nhanes_mec_svy),
                SIMPLIFY = FALSE)
names(scr_h) <- paste(gr_all$g, gr_all$r)


#remove extraneous variables from nhanes_mec_svy
nhanes_mec_svy$variables[, bmxbmi:=NULL]
nhanes_mec_svy$variables[, lbxsal:=NULL]
nhanes_mec_svy$variables[, ridstatr:=NULL]
nhanes_mec_svy$variables[, ridageyr:=NULL]
nhanes_mec_svy$variables[, ridagemn:=NULL]
nhanes_mec_svy$variables[, wtmec2yr:=NULL]
nhanes_mec_svy$variables[, bmdstats:=NULL]
nhanes_mec_svy$variables[, bmxrecum:=NULL]
nhanes_mec_svy$variables[, bmxht:=NULL]
nhanes_mec_svy$variables[, logbmxwt:=NULL]
nhanes_mec_svy$variables[, logbmxhtlenavg:=NULL]
nhanes_mec_svy$variables[, loglbxscr:=NULL]
nhanes_mec_svy$variables[, loglbxhct:=NULL]

#save NHANES data as data.table object
mecdt <- nhanes_mec_svy$variables

#save updated httkpop data
save(list=c("bmiage",
            "mcnally_dt",
            "mecdt",
            "wfl",
            "hw_H",
            "hct_h",
            "scr_h"
            ),
     compress = "bzip2",
     version = 3,
     file="../data/httkpop.RData")


