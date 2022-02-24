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

#' 
#' Pre-process the NHANES data.
source("make_bmiage_dt.R") #loads data.table called bmiage
devtools::use_data(bmiage, overwrite=TRUE)
source("make_wfl.R") #loads data_table called wfl (weight for length)
devtools::use_data(wfl, overwrite=TRUE)
source("get_weight_class.R")
source("nhanes_init.R")
#For reproducibility, first set a seed!
TeachingDemos::char2seed("Caroline Ring")
#Now read in and process the NHANES data.
nhanes_mec_svy <- nhanes_init(bmiage, wfl)
#Add the data to the package.
devtools::use_data(nhanes_mec_svy, overwrite=TRUE)

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
 
   overall_smooth<- c(overall_smooth, 
                     list(year=rep('Overall', 
                                   length(overall_smooth$x))))
   #Pack into a list and return
  return(list(gender=g, reth=r, smth=overall_smooth))
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
  out_kde <- list(x=as.matrix(d.sub$variables[, .(logwresid,  #kernel centers
                                                  loghresid)]), 
                  w=d.sub$variables[, wtmec6yr], #kernel weights
                  H=hw_kde$H) #optimal kernel bandwidth matrix
  #return gender and race along with spline fits and optimal KDE bandwidth matrix
  #to later be turned into a data.table
  return(list(g=g, 
              r=r, 
              height_spline=height_spline, 
              weight_spline=weight_spline,
              hw_kde=out_kde))
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
                  loghtcpred=predict(hematocrit_spline,
                                     x=ridexagm)$y)
  d.sub <- update(d.sub,
                  loghtcresid=loglbxhct-loghtcpred)
  #Fit 1-D KDE to residuals
  hct_kde <- ks::kde(x=d.sub$variables[, loghtcresid],
                     w=d.sub$variables[['wtmec6yr']])
  tmp_kde <- list(x=d.sub$variables[, loghtcresid], #kernel centers
                  w=d.sub$variables[['wtmec6yr']], #kernel weights
                  h=hct_kde$h) #optimal kernel bandwidth
  
  return(list(gender=g,
              reth=r, 
              hct_spline=hematocrit_spline,
              hct_kde=tmp_kde))
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
  tmp_kde <- list(x=d.sub$variables[, logscresid], #kernel centers
                  w=d.sub$variables[['wtmec6yr']], #kernel weights
                  h=sc_kde$h) #optimal kernel bandwidth
  
  return(list(gender=g,
              reth=r, 
              sc_spline=serumcreat_spline,
              sc_kde=tmp_kde))
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
gr_all <- expand.grid(g=genders, r=reths)

#' 
#' Each function returns a list. Use `mapply` with `SIMPLIFY=FALSE` to get 
#' a list of lists; then transform that nested list into a data.table. 
#' 
#' ## Smoothed age distribution
## ----mapply_age_smooth---------------------------------------------------
age_smooth_list <- mapply(FUN=age_smooth,
                          g=as.character(gr_all$g),
                          r=as.character(gr_all$r),
                          MoreArgs=list(nhanes_mec_svy=nhanes_mec_svy),
                          SIMPLIFY=FALSE)
#Transform list of lists into a data.table
age_dist_smooth <- as.data.table(t(sapply(age_smooth_list,c)))
age_dist_smooth[, gender:=unlist(gender)]
age_dist_smooth[, reth:=unlist(reth)]
#add the data to the package
devtools::use_data(age_dist_smooth, overwrite=TRUE)

#' 
#' ## Height and weight
## ----mapply_heightweight, warning=FALSE----------------------------------
hw_list <- mapply(FUN=heightweight,
                  g=as.character(gr_all$g),
                  r=as.character(gr_all$r),
                  MoreArgs=list(nhanes_mec_svy=nhanes_mec_svy),
                  SIMPLIFY=FALSE)
#Transform list of lists into a data.table
spline_heightweight <- as.data.table(t(sapply(hw_list,c)))
spline_heightweight[, g:=unlist(g)]
spline_heightweight[, r:=unlist(r)]
spline_heightweight[, nkde:=length(hw_kde[[1]]$w),
      by=1:nrow(spline_heightweight)]
#add the data to the package
devtools::use_data(spline_heightweight, overwrite=TRUE)

#' 
#' ## Hematocrit
## ----mapply_hematocrit, warning=FALSE------------------------------------
hct_list <- mapply(FUN=hematocrit,
                   g=as.character(gr_all$g),
                  r=as.character(gr_all$r),
                  MoreArgs=list(nhanes_mec_svy=nhanes_mec_svy),
                  SIMPLIFY=FALSE)
#Transform list of lists into a data.table
spline_hematocrit <- as.data.table(t(sapply(hct_list,c)))
spline_hematocrit[, gender:=unlist(gender)]
spline_hematocrit[, reth:=unlist(reth)]
#add the data to the package
devtools::use_data(spline_hematocrit, overwrite=TRUE)
#' 
#' ## Serum creatinine
## ----mapply_serumcreat, warning=FALSE------------------------------------
sc_list <- mapply(FUN=serumcreat,
                   g=as.character(gr_all$g),
                  r=as.character(gr_all$r),
                  MoreArgs=list(nhanes_mec_svy=nhanes_mec_svy),
                  SIMPLIFY=FALSE)
#Transform list of lists into a data.table
spline_serumcreat <- as.data.table(t(sapply(sc_list,c)))
spline_serumcreat[, gender:=unlist(gender)]
spline_serumcreat[, reth:=unlist(reth)]
#add the data to the package
devtools::use_data(spline_serumcreat, overwrite=TRUE)
#' 
#' Now we have all the spline fits and residual KDEs used to generate 
#' virtual populations using the virtual-individuals method.
