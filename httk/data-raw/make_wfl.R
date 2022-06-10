#'Make weight-for-length data table

library(data.table)
#Read in CDC weight-for-length chart data for girls and boys
wfl_female <- fread('cdc_weightforlength_girls.txt')
wfl_male <- fread('cdc_weightforlength_boys.txt')

#Denote sexes
wfl_female[, Sex:='Female']
wfl_male[, Sex:='Male']

#Combine into one data table
wfl <- rbindlist(list(wfl_female,wfl_male))

#Change column names for easier reference
#(so they don't start with numbers or have spaces,
#which confuses data.table)
setnames(wfl, 
         c("2nd (2.3rd)", 
           "98th (97.7th)"), 
         c('P2.3', 
           'P97.7'))
#All the other percentiles end in "th" so use regular expressions
#to change column names
setnames(wfl,names(wfl),
         gsub(x=names(wfl), 
              pattern='(\\d{1,2})th', 
              replacement='P\\1', 
              perl=TRUE))

wfl <- wfl[, .(Sex, Length, P2.3, P97.7)]

