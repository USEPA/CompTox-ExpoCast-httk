### Miyuki Breen
### update Caroline Ring's nhanes_analysis.r to get NHANES 2017-2018, 2015-2016, 2013-2014
### devtools::use_data has been deprecated and transferred to the usethis package 
### using usethis::use_data instead of devtools:use_data 
### June 30, 2020

#' NHANES analysis
#' ========================================================
#'
 
#' This vignette shows how the pre-processed NHANES survey data is analyzed to
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
source("get_weight_class.R")
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

#' # Smoothed age distributions by gender and race/ethnicity
#' The tasks of this function are as follows:
#' 
#' 1. Subset the data by gender and race/ethnicity.
#' 2. Use the `survey::svysmooth` function to get a smoothed one-dimensional 
#' distribution of ages.
#' 3. Pack the result into a list to be turned into a data.table later.
#' 
## ----age_smooth----------------------------------------------------------
age_smooth <- function(g, r, nhanes_mec_svy){
  d.sub <- subset(nhanes_mec_svy, #take gender and race/ethnicity subset of data
                  riagendr==g & 
                    ridreth1==r)
  
  overall_smooth <- survey::svysmooth(~ridexagm, #smooth distribution of age in months
                                      d.sub, 
                                      ngrid=960)$ridexagm #evaluate at 0:959 months
 #return the smoothed age density
  return(data.table(x = overall_smooth$x,
                   y = overall_smooth$y))
  }

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
  #Put together output list
  hw_fit <- data.table(seqn = d.sub$variables$seqn,
                         logwresid = d.sub$variables$logwresid, #kernel centers
                        loghresid = d.sub$variables$loghresid,
                        hw_kde_H = list(hw_kde$H),
                        weight_spline = list(weight_spline),
                        height_spline = list(height_spline))
  
  return(hw_fit)
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
  outDT <- data.table(hct_spline=list(hematocrit_spline),
                      seqn = d.sub$variables$seqn,
                      loghctresid=d.sub$variables[, loghctresid], #kernel centers
                  h=hct_kde$h) 
  
  return(outDT)
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
  outDT<- list(sc_spline=list(serumcreat_spline),
               seqn = d.sub$variables$seqn,
               logscresid=d.sub$variables[, logscresid], #kernel centers
                  h=sc_kde$h) #optimal kernel bandwidth
  
  return(outDT)
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


# use data.table syntax to loop over variables.

DT_age <- gr_all[, age_smooth(g = g,
                             r = r,
                             nhanes_mec_svy = nhanes_mec_svy),
                by = .(g, r)]

#' ## Height and weight
DT_hw <- gr_all[, heightweight(g=g,
                               r=r,
                               nhanes_mec_svy = nhanes_mec_svy),
                by = .(g,r)]

#Pull out spline fits separately to reduce space
DT_hw_spline <- DT_hw[, .(height_spline = height_spline[1],
                          weight_spline = weight_spline[1]),
                      by = .(g, r)]
height_spline <- DT_hw_spline[, height_spline]
weight_spline <- DT_hw_spline[, weight_spline]
names(height_spline) <- names(weight_spline) <- DT_hw_spline[, paste(g, r)]
#Pull out KDE centers separately
DT_hw_kde_centers <- DT_hw[, .(g, r, seqn, logwresid, loghresid)]
#Pull out KDE 2-D bandwidth matrix separately
DT_hw_kde_H <- DT_hw[, as.data.table(
  t(
    sapply(
      hw_kde_H,
      as.vector
      )
    )
), by = .(g,r)]

DT_hw_kde_H <- DT_hw[, .(H = hw_kde_H[1]), by = .(g,r)]
hw_H <- DT_hw_kde_H[, H]
names(hw_H) <- DT_hw_kde_H[, paste(g,r)]



# Hematocrit
DT_hct <- gr_all[, hematocrit(g=g,
                               r=r,
                               nhanes_mec_svy = nhanes_mec_svy),
                 by = .(g,r)]
#Pull out splines separately to save space
DT_hct_spline <- DT_hct[, .(hct_spline = hct_spline[1]),
                      by = .(g, r)]
hct_spline <- DT_hct_spline[, hct_spline]
names(hct_spline) <- DT_hct_spline[, paste(g, r)]

#Pull out kde centers separately to save space
DT_hct_kde_centers <- DT_hct[, .(g, r, seqn, loghctresid)]
#Pull out kde bandwidth separately to save space
DT_hct_kde_h <- unique(DT_hct[, .(g, r, h)])
hct_h <- as.list(DT_hct_kde_h[, h])
names(hct_h) <- DT_hct_kde_h[, paste(g,r)]

#Serum creatinine
DT_scr <- gr_all[, serumcreat(g=g,
                              r=r,
                              nhanes_mec_svy = nhanes_mec_svy),
                 by = .(g,r)]
#Pull out splines separately to save space
DT_scr_spline <- DT_scr[, .(sc_spline = sc_spline[1]),
                        by = .(g, r)]
scr_spline <- DT_scr_spline[, sc_spline]
names(scr_spline) <- DT_scr_spline[, paste(g, r)]
#Pull out kde centers separately to save space
DT_scr_kde_centers <- DT_scr[, .(g, r, seqn, logscresid)]
#Pull out kde bandwidth separately to save space
DT_scr_kde_h <- unique(DT_scr[, .(g, r, h)])
scr_h <- as.list(DT_scr_kde_h[, h])
names(scr_h) <- DT_scr_kde_h[, paste(g,r)]

#similarly, merge KDE centers -- and age smooth
#note that we'll need to remove NAs before sampling/predicting,
#since not all seqns had values for all data
kde_centers <- DT_hw_kde_centers[
  DT_hct_kde_centers,
  on = c("g", "r", "seqn")
][
  DT_scr_kde_centers,
  on = c("g", "r", "seqn")]

#remove extraneous variables from nhanes_mec_svy
#Remove extraneous variables
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
nhanes_mec_svy$variables[, sdmvpsu:=NULL]
nhanes_mec_svy$variables[, sdmvstra:=NULL]


#save updated httkpop data
save(list=c("bmiage",
            "mcnally_dt",
            "nhanes_mec_svy",
            "wfl",
            "DT_age",
            #"kde_centers",
            # "hw_H",
            # "hct_h",
            # "scr_h",
            "height_spline",
            "weight_spline",
            "hct_spline",
            "scr_spline"),
     compress = "bzip2",
     version = 3,
     file="../data/httkpop.RData")


#' 
#' Now we have all the spline fits and residual KDEs used to generate 
#' virtual populations using the virtual-individuals method.

