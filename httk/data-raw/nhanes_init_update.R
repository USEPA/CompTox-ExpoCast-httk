### Miyuki Breen
### update Caroline Ring's nhanes_init.r to get NHANES 2017-2018, 2015-2016, 2013-2014
### In 2017-2018, 2015-2016 and 2013-2014, `ridexagy` is missing
### Computed 'ridexagy' from `ridexagm` if missing
### Octorber 27, 2021

# rm(list=ls())
# setwd("C:/Users/MBreen03/httk_miyuki_file/httk-datatables/HTTKpop_NHANES_2012_18_Breen2021")
# 
# library(data.table)
# source("get_weight_class.R") ### functions needed for analysis
# load(file = "bmiage.Rdata") ### data needed for analysis in "get_weight_class.R"
# load(file = "wfl.Rdata")  ### data needed for analysis in "get_weight_class.R"

nhanes_init_update <- function(bmiage, wfl){
  
#' This vignette shows how the publicly available NHANES data is processed for use
#' in HTTK-Pop.
#' 

#' ' We'll read in data from three different NHANES cycles. Because the data
#files are publicly available on the Web as SAS Transport files, we'll just pull
#them directly from the web, using the function `Hmisc::sasxport.get()`. ' ' #
#2017-2018 cycle ' ## Demographic data ' First, we read in the demographic data
#file. #
#------------------------------------------------------------------------
demodt_j <- as.data.table(Hmisc::sasxport.get("http://wwwn.cdc.gov/Nchs/Nhanes/2017-2018/DEMO_J.XPT")) 

#' `Hmisc::sasxport.get()` labels the variables by default, but the labels
#' break some other functions we'll need later, so let's go ahead and remove them,
#' using "unclass".
#' 
# ------------------------------------------------------------------------
demodt_j<- demodt_j[, lapply(.SD, unclass)]

#' ' The demographics data set contains a lot of variables that HTTK-Pop won't
#be using. To save space, let's just keep the ones we need. We do keep the
#respondent sequence number (unique ID), so if you later wish to look at some of
#the dropped variables, you can easily pull them by sequence number. ' 
#
#------------------------------------------------------------------------ 
#drop columns not used in analysis
demodt_j <- demodt_j[, .(seqn, #respondent sequence number
                         sddsrvyr, #data release cycle
                         ridstatr, #interview/examination status
                         riagendr, #gender
                         ridageyr, #age in years at screening
                         ridagemn, #age in months at screening (0-24 months)
                         ridreth1, #race/Hispanic origin
                         # ridexagy, #age in years at exam (2 to 19 years) - not available for 2017-2018
                         ridexagm, #age in months at exam (0 to 19 years)
                         wtmec2yr, #full sample 2 year exam weight
                         sdmvpsu, #masked variance pseudo-PSU
                         sdmvstra)] #masked variance pseudo-stratum

#' ' And let's keep only the respondents who underwent examination. This
#information is coded in `ridstatr`, which takes value 1 for interview only, and
#value 2 for both interview and MEC examination. ' #
#------------------------------------------------------------------------
demodt_j <- subset(demodt_j, ridstatr==2)

#' ' For this NHANES cycle, age in months at examination (`ridexagm`) was only
#recorded for respondents 0-19 years old. Therefore, we need to assign
#`ridexagm` for respondents where it is missing. Let's see how many missing
#values we have. ' #
#------------------------------------------------------------------------
demodt_j[, sum(!is.finite(ridexagm))]

#' ' To assign values, we go through a logical process. ' '
#' ' Because examination
#' #always took place after interview, if age in months at screening was recorded,
#' #then age in months at examination would have to be the same or greater.
#' #However, there are no respondents where `ridexagm` is missing but `ridagemn`
#' #was recorded. #
#' #------------------------------------------------------------------------
demodt_j[, sum(!is.finite(ridexagm) & is.finite(ridagemn))]

#' ' At this point the only information we have is the age in years at
#screening. So we draw a number between 0 and 11 months and assign it to
#12 times the age in years at screening. ' #
#------------------------------------------------------------------------
demodt_j[!is.finite(ridexagm) & is.finite(ridageyr),
         ridexagm:=as.integer((12*ridageyr)+sample(seq(from=0,to=11),
                                                   size=sum(!is.finite(ridexagm) &
                                                              is.finite(ridageyr)),
                                                   replace=TRUE))]

#' ' That takes care of all of the missing `ridexagm` values. #
#------------------------------------------------------------------------
demodt_j[, sum(!is.finite(ridexagm))]

#' In 2017-2018, `ridexagy` is missing, so we computed from `ridexagm`.
## ------------------------------------------------------------------------
#set it to floor(ridexagm/12)
demodt_j$ridexagy <- as.integer(floor(demodt_j$ridexagm/12))

#' 
#' ## Body measures data Next, we read in the body measures data file, including
#' information on height and weight.

## ------------------------------------------------------------------------
#read in 2017-2018 NHANES examination body measures data
bmdt_j <- as.data.table(Hmisc::sasxport.get("http://wwwn.cdc.gov/Nchs/Nhanes/2017-2018/BMX_J.XPT")) 
bmdt_j<- bmdt_j[, lapply(.SD, unclass)]
#drop columns not used in analysis
bmdt_j <- bmdt_j[,
                 .(seqn, #respondent sequence number
                   bmdstats, #body measures component status code
                   bmxwt, #weight (kg)
                   bmxrecum, #recumbent length (cm), 0-47 months
                   bmxht, #standing height, 2-150 years
                   bmxbmi)] #body mass index (kg/m^2)

#' 
#' Standard biochemistry profile
#' 
#' Next, we read in the standard biochemistry profile data file, containing data
#' on serum creatinine and serum albumin.

## ------------------------------------------------------------------------
biopro_j <- Hmisc::sasxport.get("http://wwwn.cdc.gov/Nchs/Nhanes/2017-2018/BIOPRO_J.XPT")
biopro_j <- data.table::as.data.table(biopro_j)
biopro_j<- biopro_j[, lapply(.SD, unclass)]
biopro_j <- biopro_j[, .(seqn, #respondent sequence number
                         lbxscr, #serum creatinine, mg/dL
                         lbxsal)] #serum albumin, g/dL

#' 
#' ## Complete blood count

#' Finally, we read in the complete blood count data file, containing
#' information on hematocrit.

## ------------------------------------------------------------------------
cbc_j <- Hmisc::sasxport.get("http://wwwn.cdc.gov/Nchs/Nhanes/2017-2018/CBC_J.XPT")
cbc_j <- data.table::as.data.table(cbc_j)
cbc_j<- cbc_j[, lapply(.SD, unclass)]
cbc_j <- cbc_j[, .(seqn, #respondent sequence number
                   lbxhct)] #hematocrit (%)

#' 
#' ## Combine all 2017-2018 data

#' Simply merge together these data sets by `seqn`, the unique respondent
#' sequence number. Use the option `all=TRUE` to retain respondents who may not
#' be included in all four data sets.

## ------------------------------------------------------------------------
nhanes_j <- Reduce(function(x, y) merge(x, y, by="seqn", all=TRUE),
                   list(demodt_j, bmdt_j, biopro_j, cbc_j))


#' ' We'll read in data from three different NHANES cycles. Because the data
#files are publicly available on the Web as SAS Transport files, we'll just pull
#them directly from the web, using the function `Hmisc::sasxport.get()`. ' ' #
#2015-2016 cycle ' ## Demographic data ' First, we read in the demographic data
#file. #
#------------------------------------------------------------------------
  demodt_i <- as.data.table(Hmisc::sasxport.get("http://wwwn.cdc.gov/Nchs/Nhanes/2015-2016/DEMO_I.XPT")) 

#' `Hmisc::sasxport.get()` labels the variables by default, but the labels
#' break some other functions we'll need later, so let's go ahead and remove them,
#' using "unclass".
#' 
# ------------------------------------------------------------------------
demodt_i<- demodt_i[, lapply(.SD, unclass)]

#' ' The demographics data set contains a lot of variables that HTTK-Pop won't
#be using. To save space, let's just keep the ones we need. We do keep the
#respondent sequence number (unique ID), so if you later wish to look at some of
#the dropped variables, you can easily pull them by sequence number. ' 
#
#------------------------------------------------------------------------ 
#drop columns not used in analysis
  demodt_i <- demodt_i[, .(seqn, #respondent sequence number
                           sddsrvyr, #data release cycle
                           ridstatr, #interview/examination status
                           riagendr, #gender
                           ridageyr, #age in years at screening
                           ridagemn, #age in months at screening (0-24 months)
                           ridreth1, #race/Hispanic origin
                          # ridexagy, #age in years at exam (2 to 19 years) - not available for 2015-2016
                           ridexagm, #age in months at exam (0 to 19 years)
                           wtmec2yr, #full sample 2 year exam weight
                           sdmvpsu, #masked variance pseudo-PSU
                           sdmvstra)] #masked variance pseudo-stratum

#' ' And let's keep only the respondents who underwent examination. This
#information is coded in `ridstatr`, which takes value 1 for interview only, and
#value 2 for both interview and MEC examination. ' #
#------------------------------------------------------------------------
demodt_i <- subset(demodt_i, ridstatr==2)

#' ' For this NHANES cycle, age in months at examination (`ridexagm`) was only
#recorded for respondents 0-19 years old. Therefore, we need to assign
#`ridexagm` for respondents where it is missing. Let's see how many missing
#values we have. ' #
#------------------------------------------------------------------------
demodt_i[, sum(!is.finite(ridexagm))]

#' ' To assign values, we go through a logical process. ' '
#' ' Because examination
#' #always took place after interview, if age in months at screening was recorded,
#' #then age in months at examination would have to be the same or greater.
#' #However, there are no respondents where `ridexagm` is missing but `ridagemn`
#' #was recorded. #
#' #------------------------------------------------------------------------
demodt_i[, sum(!is.finite(ridexagm) & is.finite(ridagemn))]

#' ' At this point the only information we have is the age in years at
#screening. So we draw a number between 0 and 11 months and assign it to
#12 times the age in years at screening. ' #
#------------------------------------------------------------------------
demodt_i[!is.finite(ridexagm) & is.finite(ridageyr),
         ridexagm:=as.integer((12*ridageyr)+sample(seq(from=0,to=11),
                                      size=sum(!is.finite(ridexagm) &
                                                is.finite(ridageyr)),
                                      replace=TRUE))]

#' ' That takes care of all of the missing `ridexagm` values. #
#------------------------------------------------------------------------
demodt_i[, sum(!is.finite(ridexagm))]

#' In 2015-2016, `ridexagy` is missing, so we computed from `ridexagm`.
## ------------------------------------------------------------------------
#set it to floor(ridexagm/12)
demodt_i$ridexagy <- as.integer(floor(demodt_i$ridexagm/12))

#' 
#' ## Body measures data Next, we read in the body measures data file, including
#' information on height and weight.

## ------------------------------------------------------------------------
#read in 2015-2016 NHANES examination body measures data
bmdt_i <- as.data.table(Hmisc::sasxport.get("http://wwwn.cdc.gov/Nchs/Nhanes/2015-2016/BMX_I.XPT")) 
bmdt_i<- bmdt_i[, lapply(.SD, unclass)]
#drop columns not used in analysis
bmdt_i <- bmdt_i[,
                 .(seqn, #respondent sequence number
                   bmdstats, #body measures component status code
                   bmxwt, #weight (kg)
                   bmxrecum, #recumbent length (cm), 0-47 months
                   bmxht, #standing height, 2-150 years
                   bmxbmi)] #body mass index (kg/m^2)

#' 
#' Standard biochemistry profile
#' 
#' Next, we read in the standard biochemistry profile data file, containing data
#' on serum creatinine and serum albumin.

## ------------------------------------------------------------------------
biopro_i <- Hmisc::sasxport.get("http://wwwn.cdc.gov/Nchs/Nhanes/2015-2016/BIOPRO_I.XPT")
biopro_i <- data.table::as.data.table(biopro_i)
biopro_i<- biopro_i[, lapply(.SD, unclass)]
biopro_i <- biopro_i[, .(seqn, #respondent sequence number
                         lbxscr, #serum creatinine, mg/dL
                         lbxsal)] #serum albumin, g/dL

#' 
#' ## Complete blood count

#' Finally, we read in the complete blood count data file, containing
#' information on hematocrit.

## ------------------------------------------------------------------------
cbc_i <- Hmisc::sasxport.get("http://wwwn.cdc.gov/Nchs/Nhanes/2015-2016/CBC_I.XPT")
cbc_i <- data.table::as.data.table(cbc_i)
cbc_i<- cbc_i[, lapply(.SD, unclass)]
cbc_i <- cbc_i[, .(seqn, #respondent sequence number
                   lbxhct)] #hematocrit (%)

#' 
#' ## Combine all 2015-2016 data

#' Simply merge together these data sets by `seqn`, the unique respondent
#' sequence number. Use the option `all=TRUE` to retain respondents who may not
#' be included in all four data sets.

## ------------------------------------------------------------------------
nhanes_i <- Reduce(function(x, y) merge(x, y, by="seqn", all=TRUE),
                   list(demodt_i, bmdt_i, biopro_i, cbc_i))

#2013-2014 cycle ' ## Demographic data ' First, we read in the demographic data
#file. #
#------------------------------------------------------------------------
demodt_h <- as.data.table(Hmisc::sasxport.get("http://wwwn.cdc.gov/Nchs/Nhanes/2013-2014/DEMO_H.XPT")) 

#' `Hmisc::sasxport.get()` labels the variables by default, but the labels
#' break some other functions we'll need later, so let's go ahead and remove them,
#' using "unclass".
#' 
# ------------------------------------------------------------------------
demodt_h<- demodt_h[, lapply(.SD, unclass)]

#' ' The demographics data set contains a lot of variables that HTTK-Pop won't
#be using. To save space, let's just keep the ones we need. We do keep the
#respondent sequence number (unique ID), so if you later wish to look at some of
#the dropped variables, you can easily pull them by sequence number. ' 
#
#------------------------------------------------------------------------ 
#drop columns not used in analysis
demodt_h <- demodt_h[, .(seqn, #respondent sequence number
                         sddsrvyr, #data release cycle
                         ridstatr, #interview/examination status
                         riagendr, #gender
                         ridageyr, #age in years at screening
                         ridagemn, #age in months at screening (0-24 months)
                         ridreth1, #race/Hispanic origin
                         # ridexagy, #age in years at exam (2 to 19 years) - not available for 2013-2014
                         ridexagm, #age in months at exam (0 to 19 years)
                         wtmec2yr, #full sample 2 year exam weight
                         sdmvpsu, #masked variance pseudo-PSU
                         sdmvstra)] #masked variance pseudo-stratum

#' ' And let's keep only the respondents who underwent examination. This
#information is coded in `ridstatr`, which takes value 1 for interview only, and
#value 2 for both interview and MEC examination. ' #
#------------------------------------------------------------------------
demodt_h <- subset(demodt_h, ridstatr==2)

#' ' For this NHANES cycle, age in months at examination (`ridexagm`) was only
#recorded for respondents 0-19 years old. Therefore, we need to assign
#`ridexagm` for respondents where it is missing. Let's see how many missing
#values we have. ' #
#------------------------------------------------------------------------
demodt_h[, sum(!is.finite(ridexagm))]

#' ' To assign values, we go through a logical process. ' '
#' ' Because examination
#' #always took place after interview, if age in months at screening was recorded,
#' #then age in months at examination would have to be the same or greater.
#' #However, there are no respondents where `ridexagm` is missing but `ridagemn`
#' #was recorded. #
#' #------------------------------------------------------------------------
demodt_h[, sum(!is.finite(ridexagm) & is.finite(ridagemn))]

#' ' At this point the only information we have is the age in years at
#screening. So we draw a number between 0 and 11 months and assign it to
#12 times the age in years at screening. ' #
#------------------------------------------------------------------------
demodt_h[!is.finite(ridexagm) & is.finite(ridageyr),
         ridexagm:=as.integer((12*ridageyr)+sample(seq(from=0,to=11),
                                                   size=sum(!is.finite(ridexagm) &
                                                              is.finite(ridageyr)),
                                                   replace=TRUE))]

#' ' That takes care of all of the missing `ridexagm` values. #
#------------------------------------------------------------------------
demodt_h[, sum(!is.finite(ridexagm))]

#' In 2013-2014, `ridexagy` is missing, so we computed from `ridexagm`.
## ------------------------------------------------------------------------
#set it to floor(ridexagm/12)
demodt_h$ridexagy <- as.integer(floor(demodt_h$ridexagm/12))

#' 
#' ## Body measures data Next, we read in the body measures data file, including
#' information on height and weight.

## ------------------------------------------------------------------------
#read in 2013-2014 NHANES examination body measures data
bmdt_h <- as.data.table(Hmisc::sasxport.get("http://wwwn.cdc.gov/Nchs/Nhanes/2013-2014/BMX_H.XPT")) 
bmdt_h<- bmdt_h[, lapply(.SD, unclass)]
#drop columns not used in analysis
bmdt_h <- bmdt_h[,
                 .(seqn, #respondent sequence number
                   bmdstats, #body measures component status code
                   bmxwt, #weight (kg)
                   bmxrecum, #recumbent length (cm), 0-47 months
                   bmxht, #standing height, 2-150 years
                   bmxbmi)] #body mass index (kg/m^2)

#' 
#' Standard biochemistry profile
#' 
#' Next, we read in the standard biochemistry profile data file, containing data
#' on serum creatinine and serum albumin.

## ------------------------------------------------------------------------
biopro_h <- Hmisc::sasxport.get("http://wwwn.cdc.gov/Nchs/Nhanes/2013-2014/BIOPRO_H.XPT")
biopro_h <- data.table::as.data.table(biopro_h)
biopro_h<- biopro_h[, lapply(.SD, unclass)]
biopro_h <- biopro_h[, .(seqn, #respondent sequence number
                         lbxscr, #serum creatinine, mg/dL
                         lbxsal)] #serum albumin, g/dL

#' 
#' ## Complete blood count

#' Finally, we read in the complete blood count data file, containing
#' information on hematocrit.

## ------------------------------------------------------------------------
cbc_h <- Hmisc::sasxport.get("http://wwwn.cdc.gov/Nchs/Nhanes/2013-2014/CBC_H.XPT")
cbc_h <- data.table::as.data.table(cbc_h)
cbc_h<- cbc_h[, lapply(.SD, unclass)]
cbc_h <- cbc_h[, .(seqn, #respondent sequence number
                   lbxhct)] #hematocrit (%)

#' 
#' ## Combine all 2013-2014 data

#' Simply merge together these data sets by `seqn`, the unique respondent
#' sequence number. Use the option `all=TRUE` to retain respondents who may not
#' be included in all four data sets.

## ------------------------------------------------------------------------
nhanes_h <- Reduce(function(x, y) merge(x, y, by="seqn", all=TRUE),
                   list(demodt_h, bmdt_h, biopro_h, cbc_h))

#' 
#' # Combine three NHANES cycles

#' To pool data from the three NHANES cycle, just bind together the three
#' data.tables.

## ------------------------------------------------------------------------
mecdt <- rbindlist(list(nhanes_h, nhanes_i, nhanes_j),
                   use.names=TRUE,
                   fill=TRUE)

#' 
#' ## Calculate combined sample weights

#' The sample weights for each cycle (`wtmec2yr`) need to be adjusted when
#' multiple cycles are pooled. To get the combined sample weights, simply divide
#' the 2-year sample weights by 3.

## ------------------------------------------------------------------------
mecdt[, wtmec6yr:=wtmec2yr/3]

#'   
#' # Relabel some factor variables

#' Variables representing factors like ethnicity, gender, and data release are
#' originally coded as integers. Let's give them labels to make them more
#' informative.

## ------------------------------------------------------------------------
#Relabel ethnicity factors to be informative names rather than just numbers
mecdt[, 
      ridreth1:=factor(ridreth1, #original ethnicity variable
                       levels=c(1,2,3,4,5), 
                       labels=c('Mexican American', 
                                'Other Hispanic',
                                'Non-Hispanic White',
                                'Non-Hispanic Black',
                                'Other'))]

#Relabel gender factors to be informative
mecdt[, riagendr:=factor(riagendr, 
                         levels=c(1,2), 
                         labels=c('Male', 'Female'))]

#Relabel data release numbers to be informative
mecdt[, sddsrvyr:=factor(sddsrvyr, 
                         levels=c(8,9,10), 
                         labels=c('NHANES 2013-2014',
                                  'NHANES 2015-2016',
                                  'NHANES 2017-2018'))]

#' 
#' Code a new variable that takes the average of recumbent length and standing
#' height. If one of them is NA, then it just takes the other. If both are NA,
#' then the result is NaN.

## ------------------------------------------------------------------------
mecdt[, 
      bmxhtlenavg:=rowMeans(cbind(bmxrecum, 
                                  bmxht),
                            na.rm=TRUE)]

#' 
#' Take log transformations of some variables, for use later in constructing
#' approximate population distributions.

## ------------------------------------------------------------------------
#Code a new variable that takes the log of bmxwt (bodyweight)
mecdt[, logbmxwt:=log(bmxwt)]
#Do the same for height
mecdt[, logbmxhtlenavg:=log(bmxhtlenavg)]
#Do the same for serum creatinine
mecdt[, loglbxscr:=log(lbxscr)]
#Do the same for hematocrit
mecdt[, loglbxhct:=log(lbxhct)]

#' 
#' ## Exclusions

#' Only keep people up to age 79 -- 80 and above were topcoded. Exclude any
#non-finite height or weight data. Exclude anyone over age 1 with missing
#hematocrit data. Exclude anyone over age 12 with missing serum creatinine data.
#' ' Before exclusions, there are `{r} nrow(dt)` unique respondents. #
#------------------------------------------------------------------------
nrow(mecdt)

#' 
#' They break down by race/ethnicity and gender as follows.
## ------------------------------------------------------------------------
knitr::kable(addmargins(xtabs(~ridreth1+riagendr, mecdt), margin=c(1,2)))

#' 
## ------------------------------------------------------------------------
mecdt <- mecdt[ridageyr<80 &
                 is.finite(bmxhtlenavg) &
                 is.finite(bmxwt) &
                 (ridexagy<1 |
                    (ridexagy>=1 & 
                       is.finite(lbxhct))
                 ) &
                 (ridexagy<12 |
                    (ridexagy>=12 & 
                       is.finite(lbxscr))
                 ),]

#' 
#' After exclusions, there are `{r} nrow(mecdt)` respondents. Their breakdown by
#' gender and race/ethnicity is as follows.

## ------------------------------------------------------------------------
knitr::kable(addmargins(xtabs(~ridreth1+riagendr, mecdt), margin=c(1,2)))

#' 
#' # Weight class

#' Code a categorical weight class variable based on age, weight, and height,
#' using BMI, BMI-for-age, and weight-for-length charts.

## ------------------------------------------------------------------------
#If BMI is missing but height and weight data are there, 
#then simply calculate BMI as weight/height^2
mecdt[!is.finite(bmxbmi), bmxbmi:=bmxwt/((bmxhtlenavg/100)^2)]  
mecdt[, weight_class:=get_weight_class(age_years=ridexagy,
                                       age_months=ridexagm,
                                       bmi=bmxbmi,
                                       recumlen=bmxhtlenavg,
                                       weight=bmxwt,
                                       gender=riagendr,
                                       bmiage=bmiage,
                                       wfl=wfl)]



#' 
#' Finally, construct a survey design object with the `survey` package. This
#' makes later analysis much easier, because the `survey` package handles the
#' sample weights transparently.
#' 

## ------------------------------------------------------------------------
nhanes_mec_svy <- survey::svydesign(strata=~sdmvstra, #masked stratification variable name
                                    id=~sdmvpsu, #masked PSU (cluster) variable name
                                    weights=~wtmec6yr, #use examination sample weights
                                    data = mecdt, #referring to mecdt data set
                                    nest=TRUE) #nest means masked PSUs are reused within each stratum.

return(nhanes_mec_svy)
}

