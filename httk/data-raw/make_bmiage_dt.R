#' Make bmiage data table.
#' 
#'For children ages 2 to 18, 
#'weight class depends on the BMI-for-age percentile.
#'<5th percentile = underweight
#'5-85th percentiles = normal weight
#'85-95th percentile = overweight
#'>=95th percentile = obese
#'
library(data.table)

#read in CDC BMI for age charts from the web
bmiagerev <- data.table::fread("http://www.cdc.gov/growthcharts/data/zscore/bmiagerev.csv")


#Remove the extra header row in the middle of the file
bmiagerev <- bmiagerev[Agemos!='Agemos',]
#convert column classes to what they should be:
#everything other than "Sex" is numeric....
bmiagerev[, names(bmiagerev)[names(bmiagerev)!="Sex"]:=lapply(.SD, 
                                                              as.numeric), 
          .SDcols=names(bmiagerev)[names(bmiagerev)!="Sex"]]

#...and "Sex" is a two-level factor
bmiagerev[, Sex:=factor(Sex, 
                     levels=c(1,2), 
                     labels=c('Male','Female'))]

#Ages in the original chart are generally as 24.5, 25.5, etc.
#whereas httkpop assigns ages in whole months
#so, interpolate the CDC data at whole months between 24 and 240
#to get the 5th, 85th, and 95th BMI percentiles for each age in months

#Do this separately for males and females
#Males:
bmiage_males <- data.table(Sex='Male', Agemos=24:240)
bmiage_males[, c('P5',
                 'P85',
                 'P95'):=list(spline(x=bmiagerev[Sex=='Male',
                                              Agemos],
                                     y=bmiagerev[Sex=='Male',
                                              P5],
                                     xout=Agemos)$y,
                              spline(x=bmiagerev[Sex=='Male',
                                              Agemos],
                                     y=bmiagerev[Sex=='Male',
                                              P85],
                                     xout=Agemos)$y,
                              spline(x=bmiagerev[Sex=='Male',
                                              Agemos],
                                     y=bmiagerev[Sex=='Male',
                                              P95],
                                     xout=Agemos)$y)]
#Females
bmiage_females <- data.table(Sex='Female', Agemos=24:240)
bmiage_females[, c('P5',
                 'P85',
                 'P95'):=list(spline(x=bmiagerev[Sex=='Female',
                                              Agemos],
                                     y=bmiagerev[Sex=='Female',
                                              P5],
                                     xout=Agemos)$y,
                              spline(x=bmiagerev[Sex=='Female',
                                              Agemos],
                                     y=bmiagerev[Sex=='Female',
                                              P85],
                                     xout=Agemos)$y,
                              spline(x=bmiagerev[Sex=='Female',
                                              Agemos],
                                     y=bmiagerev[Sex=='Female',
                                              P95],
                                     xout=Agemos)$y)]

#Bind the male and female interpolations back together into one table
bmiage <- rbind(bmiage_males,
                bmiage_females)
#and sort by age in months
setkey(bmiage, Sex, Agemos)
