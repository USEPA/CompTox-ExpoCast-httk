nhanes_init <- function(bmiage, wfl){

#' This vignette shows how the publicly available NHANES data is processed for use
#' in HTTK-Pop.
#' 

#' ' We'll read in data from three different NHANES cycles. Because the data
#files are publicly available on the Web as SAS Transport files, we'll just pull
#them directly from the web, using the function `Hmisc::sasxport.get()`. ' ' #
#2011-2012 cycle ' ## Demographic data ' First, we read in the demographic data
#file. #
#------------------------------------------------------------------------
  demodt_g <- as.data.table(Hmisc::sasxport.get("http://wwwn.cdc.gov/Nchs/Nhanes/2011-2012/DEMO_G.XPT")) 

#' `Hmisc::sasxport.get()` labels the variables by default, but the labels
#' break some other functions we'll need later, so let's go ahead and remove them,
#' using "unclass".
#' 
# ------------------------------------------------------------------------
demodt_g<- demodt_g[, lapply(.SD, unclass)]

#' ' The demographics data set contains a lot of variables that HTTK-Pop won't
#be using. To save space, let's just keep the ones we need. We do keep the
#respondent sequence number (unique ID), so if you later wish to look at some of
#the dropped variables, you can easily pull them by sequence number. ' 
#
#------------------------------------------------------------------------ 
#drop columns not used in analysis
  demodt_g <- demodt_g[, .(seqn, #respondent sequence number
                           sddsrvyr, #data release cycle
                           ridstatr, #interview/examination status
                           riagendr, #gender
                           ridageyr, #age in years at screening
                           ridagemn, #age in months at screening (0-24 months)
                           ridreth1, #race/Hispanic origin
                           ridexagy, #age in years at exam (2 to 19 years)
                           ridexagm, #age in months at exam (0 to 19 years)
                           wtmec2yr, #full sample 2 year exam weight
                           sdmvpsu, #masked variance pseudo-PSU
                           sdmvstra)] #masked variance pseudo-stratum

#' ' And let's keep only the respondents who underwent examination. This
#information is coded in `ridstatr`, which takes value 1 for interview only, and
#value 2 for both interview and MEC examination. ' #
#------------------------------------------------------------------------
demodt_g <- subset(demodt_g, ridstatr==2)

#' ' For this NHANES cycle, age in months at examination (`ridexagm`) was only
#recorded for respondents 0-19 years old. Therefore, we need to assign
#`ridexagm` for respondents where it is missing. Let's see how many missing
#values we have. ' #
#------------------------------------------------------------------------
demodt_g[, sum(!is.finite(ridexagm))]

#' ' To assign values, we go through a logical process. ' ' First, if an age in
#years at examination (`ridexagy`) is recorded, then the individual must be
#somewhere between that birthday and the next one. For example, if someone is 1
#year old, they are at least 12 months old, but they cannot be more than 23
#months old -- because if they were 24 months old, they would be 2 years old. An
#age in months can be assigned by drawing a random number between 0 and 11 and
#adding it to 12 times the age in years at examination. ' ' Because examination
#always took place after interview, if age in months at screening was recorded,
#then age in months at examination would have to be the same or greater.
#However, there are no respondents where `ridexagm` is missing but `ridagemn`
#was recorded. #
#------------------------------------------------------------------------
demodt_g[, sum(!is.finite(ridexagm) & is.finite(ridagemn))]

## ------------------------------------------------------------------------
demodt_g[!is.finite(ridexagm) & is.finite(ridexagy),
         ridexagm:=as.integer((12*ridexagy)+sample(seq(from=0,to=11),
                                      size=sum(!is.finite(ridexagm) & 
                                                 is.finite(ridexagy)),
                                      replace=TRUE))]

#' ' However, this doesn't take care of all of the missing `ridexagm` values,
#because `ridexagy` was only recorded for respondents 2-19 years old. Let's see
#how many missing values we still have. ' #
#------------------------------------------------------------------------
demodt_g[, sum(!is.finite(ridexagm))]

#' ' At this point the only information we have is the age in years at
#screening. So again, we draw a number between 0 and 11 months and assign it to
#12 times the age in years at screening. ' #
#------------------------------------------------------------------------
demodt_g[!is.finite(ridexagm) & is.finite(ridageyr),
         ridexagm:=as.integer((12*ridageyr)+sample(seq(from=0,to=11),
                                      size=sum(!is.finite(ridexagm) & 
                                                 is.finite(ridageyr)),
                                      replace=TRUE))]

#' ' That takes care of all of the missing `ridexagm` values. #
#------------------------------------------------------------------------
demodt_g[, sum(!is.finite(ridexagm))]

#' 
#' Now, if `ridexagy` is missing, it can just be computed from `ridexagm`.
## ------------------------------------------------------------------------
#if ridexagy missing, set it to floor(ridexagm/12)
  demodt_g[!is.finite(ridexagy),
           ridexagy:=as.integer(floor(ridexagm/12))]

#' 
#' ## Body measures data Next, we read in the body measures data file, including
#' information on height and weight.

## ------------------------------------------------------------------------
#read in 2011-2012 NHANES examination body measures data
bmdt_g <- as.data.table(Hmisc::sasxport.get("http://wwwn.cdc.gov/Nchs/Nhanes/2011-2012/BMX_G.XPT")) 
  bmdt_g<- bmdt_g[, lapply(.SD, unclass)]
  #drop columns not used in analysis
  bmdt_g <- bmdt_g[,
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
biopro_g <- Hmisc::sasxport.get("http://wwwn.cdc.gov/Nchs/Nhanes/2011-2012/BIOPRO_G.XPT")
  biopro_g <- data.table::as.data.table(biopro_g)
  biopro_g<- biopro_g[, lapply(.SD, unclass)]
  biopro_g <- biopro_g[, .(seqn, #respondent sequence number
                           lbxscr, #serum creatinine, mg/dL
                           lbxsal)] #serum albumin, g/dL

#' 
#' ## Complete blood count

#' Finally, we read in the complete blood count data file, containing
#' information on hematocrit.

  ## ------------------------------------------------------------------------
cbc_g <- Hmisc::sasxport.get("http://wwwn.cdc.gov/Nchs/Nhanes/2011-2012/CBC_G.XPT")
  cbc_g <- data.table::as.data.table(cbc_g)
  cbc_g<- cbc_g[, lapply(.SD, unclass)]
  cbc_g <- cbc_g[, .(seqn, #respondent sequence number
                     lbxhct)] #hematocrit (%)

#' 
#' ## Combine all 2011-2012 data

#' Simply merge together these data sets by `seqn`, the unique respondent
#' sequence number. Use the option `all=TRUE` to retain respondents who may not
#' be included in all four data sets.

  ## ------------------------------------------------------------------------
nhanes_g <- Reduce(function(x, y) merge(x, y, by="seqn", all=TRUE),
       list(demodt_g, bmdt_g, biopro_g, cbc_g))

#' 
#' # 2009-2010 data
#' ## Demographics data

#' Again, we start by reading in the demographics data file.
## ------------------------------------------------------------------------
demodt_f <- as.data.table(Hmisc::sasxport.get("http://wwwn.cdc.gov/Nchs/Nhanes/2009-2010/DEMO_F.XPT")) #read in 2009-2010 NHANES demographic data
  demodt_f<- demodt_f[, lapply(.SD, unclass)]
  #drop columns not used in analysis
  demodt_f <- demodt_f[ridstatr==2, #keep only MEC examination data
                       .(seqn, #respondent sequence number
                           sddsrvyr, #data release cycle
                           ridstatr, #interview/examination status
                           riagendr, #gender
                           ridreth1, #race/ethnicity
                           ridageyr, #age in years at screening
                           ridagemn, #age in months at screening
                           ridageex, #age in months at exam (0-959 months)
                           wtmec2yr, #full sample 2 year exam weight
                           sdmvpsu, #masked variance pseudo-PSU
                           sdmvstra)] #masked variance pseudo-stratum

#' ' For this cycle, age in months at exam (`ridageex`) was recorded for
#individuals under 80 years of age. It is still missing for 398 respondents. #
#------------------------------------------------------------------------
demodt_f[, sum(!is.finite(ridageex))]

#' 
#' We'll set it to age in months at screening (`ridagemn`) if possible, and
#' otherwise, assign it again based on age in years at screening. Then we'll
#' find age in years at examination (not originally recorded) based on age in
#' months at examination.

 
## ------------------------------------------------------------------------
demodt_f[!is.finite(ridageex) & is.finite(ridagemn),
         ridageex:=ridagemn]
demodt_f[!is.finite(ridageex) & is.finite(ridageyr),
         ridageex:=as.integer((12*ridageyr)+sample(seq(from=0,to=11),
                                      size=sum(!is.finite(ridageex) & 
                                                 is.finite(ridageyr)),
                                      replace=TRUE))]
#convert age in months at exam to age in years at exam
demodt_f[, 
           ridexagy:=as.integer(floor(ridageex/12))] 

#' 
#' Then let's change the name `ridageex` to `ridexagm`, to correspond with the
#' name of the variable from the 2011-2012 data.

## ------------------------------------------------------------------------
setnames(demodt_f, "ridageex", "ridexagm")

#' 
#' ## Body measures data
## ------------------------------------------------------------------------
bmdt_f <- as.data.table(Hmisc::sasxport.get("http://wwwn.cdc.gov/Nchs/Nhanes/2009-2010/BMX_F.XPT")) #read in 2009-2010 NHANES examination body measures data
  bmdt_f<- bmdt_f[, lapply(.SD, unclass)]
  #drop columns not used in analysis
  bmdt_f <- bmdt_f[,
                   .(seqn, #respondent sequence number
                     bmdstats, #body measures component status code
                     bmxwt, #weight (kg)
                     bmxrecum, #recumbent length (cm), 0-47 months
                     bmxht, #standing height, measured for ages 2-150 years
                     bmxbmi)] #body mass index (kg/m^2)

#' 
#' ## Biochemistry profile data
## ------------------------------------------------------------------------
biopro_f <- as.data.table(Hmisc::sasxport.get("http://wwwn.cdc.gov/Nchs/Nhanes/2009-2010/BIOPRO_F.XPT"))
  biopro_f<- biopro_f[, lapply(.SD, unclass)]
biopro_f <- biopro_f[, .(seqn, #respondent sequence number
                         lbxscr, #serum creatinine, mg/dL
                         lbxsal)] #serum albumin, g/dL

#' 
#' ## Complete blood count data
## ------------------------------------------------------------------------
  cbc_f <- Hmisc::sasxport.get("http://wwwn.cdc.gov/Nchs/Nhanes/2009-2010/CBC_F.XPT")
  cbc_f <- data.table::as.data.table(cbc_f)
  cbc_f<- cbc_f[, lapply(.SD, unclass)]
  cbc_f <- cbc_f[, .(seqn, #respondent sequence number
                     lbxhct)] #hematocrit (%)

#' 
#' ## Combine all 2009-2010 data

#' Simply merge together these data sets by `seqn`, the unique respondent
#' sequence number.

  ## ------------------------------------------------------------------------
nhanes_f <- Reduce(function(x, y) merge(x, y, by="seqn", all=TRUE),
       list(demodt_f, bmdt_f, biopro_f, cbc_f))

#' 
#' # 2007-2008 data
#' ## Demographics data
## ------------------------------------------------------------------------
 demodt_e <- as.data.table(Hmisc::sasxport.get("http://wwwn.cdc.gov/Nchs/Nhanes/2007-2008/DEMO_E.XPT")) #read in 2007-2008 NHANES demographic data
  demodt_e<- demodt_e[, lapply(.SD, unclass)]
  #drop columns not used in analysis
  demodt_e <- demodt_e[ridstatr==2, #keep only examination respondents
                       .(seqn, #respondent sequence number
                           sddsrvyr, #data release cycle
                           ridstatr, #interview/examination status
                           riagendr, #gender
                           ridreth1, #race/ethnicity
                           ridageyr, #age in years at screening
                           ridagemn, #age in months at screening
                           ridageex, #age in months at exam (0-959 months)
                           wtmec2yr, #full sample 2 year exam weight
                           sdmvpsu, #masked variance pseudo-PSU
                           sdmvstra)] #masked variance pseudo-stratum

#' 
#' The age in months situation is similar to the 2009-2010 data.
## ------------------------------------------------------------------------
demodt_e[!is.finite(ridageex) & is.finite(ridagemn),
         ridageex:=ridagemn]
demodt_e[!is.finite(ridageex) & is.finite(ridageyr),
         ridageex:=as.integer((12*ridageyr)+sample(seq(from=0,to=11),
                                      size=sum(!is.finite(ridageex) & 
                                                 is.finite(ridageyr)),
                                      replace=TRUE))]
#convert age in months at exam to age in years at exam
demodt_e[, 
           ridexagy:=as.integer(floor(ridageex/12))] 
#Change name to comport with other data sets
setnames(demodt_e, "ridageex", "ridexagm")

#' 
#' ## Body measures data
## ------------------------------------------------------------------------
bmdt_e <- as.data.table(Hmisc::sasxport.get("http://wwwn.cdc.gov/Nchs/Nhanes/2007-2008/BMX_E.XPT")) #read in 2007-2008 NHANES examination body measures data
  bmdt_e<- bmdt_e[, lapply(.SD, unclass)]
  #drop columns not used in analysis
  bmdt_e <- bmdt_e[,
                   .(seqn, #respondent sequence number
                     bmdstats, #body measures component status code
                     bmxwt, #weight (kg)
                     bmxrecum, #recumbent length (cm), 0-47 months
                     bmxht, #standing height, 2-150 years
                     bmxbmi)] #body mass index (kg/m^2)

#' 
#' ## Biochemistry profile data
## ------------------------------------------------------------------------
biopro_e <- Hmisc::sasxport.get("http://wwwn.cdc.gov/Nchs/Nhanes/2007-2008/BIOPRO_E.XPT")
  biopro_e <- data.table::as.data.table(biopro_e)
  biopro_e<- biopro_e[, lapply(.SD, unclass)]
  biopro_e <- biopro_e[, .(seqn, #respondent sequence number
                           lbxscr, #serum creatinine, mg/dL
                           lbxsal)] #serum albumin, g/dL

#' 
#' ## Complete blood count data
## ------------------------------------------------------------------------
cbc_e <- Hmisc::sasxport.get("http://wwwn.cdc.gov/Nchs/Nhanes/2007-2008/CBC_E.XPT")
  cbc_e <- data.table::as.data.table(cbc_e)
  cbc_e<- cbc_e[, lapply(.SD, unclass)]
  cbc_e <- cbc_e[, .(seqn, #respondent sequence number
                     lbxhct)] #hematocrit (%)

#' 
#' ## Combine all 2007-2008 data

#' Simply merge together these data sets by `seqn`, the unique respondent
#' sequence number.

  ## ------------------------------------------------------------------------
nhanes_e <- Reduce(function(x, y) merge(x, y, by="seqn", all=TRUE),
       list(demodt_e, bmdt_e, biopro_e, cbc_e))

#' 
#' # Combine three NHANES cycles

#' To pool data from the three NHANES cycle, just bind together the three
#' data.tables.

  ## ------------------------------------------------------------------------
mecdt <- rbindlist(list(nhanes_g, nhanes_f, nhanes_e),
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
                           levels=c(5,6,7), 
                           labels=c('NHANES 2007-2008',
                                    'NHANES 2009-2010',
                                    'NHANES 2011-2012'))]

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