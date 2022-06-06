#summary of nhanes respondent numbers by age band
DT <- copy(nhanes_mec_svy$variables)
DT[ridexagm<=36, age_band:="0-3"]
DT[ridexagm>36 & ridexagm<=(12*6), age_band:="3-6"]
DT[ridexagm>(12*6) & ridexagm<=(12*11), age_band:="6-11"]
DT[ridexagm>(12*11) & ridexagm<=(12*18), age_band:="11-18"]
DT[ridexagm>(12*18) & ridexagm<=(12*65), age_band:="18-65"]
DT[ridexagm>(12*65), age_band:="65+"]
DT[, .N, by = age_band]
DT[, age_band:=factor(age_band, levels = c("0-3", "3-6", "6-11", "11-18", "18-65", "65+"))]
DT[, .N, by = age_band]
foo <- DT[, .N, by = age_band]
setorder(foo, age_band)
foo

