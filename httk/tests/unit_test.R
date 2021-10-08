#R CMD BATCH --no-timing --no-restore --no-save unit_test.R unit_test.Rout
library(httk)
#
# MW BPA is 228.29 g/mol
# 1 mg/L -> 1/228.29*1000 = 4.38 uM
convert_units("mg/L","uM",chem.cas="80-05-7")
#
# MW Diclofenac is 296.148 g/mol
# 1 uM -> 296.148/1000 =  0.296
convert_units("uM","mg/L",chem.name="diclofenac")
#
convert_units("uM","ppmv",chem.name="styrene")
#
# Compare with https://www3.epa.gov/ceampubl/learn2model/part-two/onsite/ia_unit_conversion.html
# 1 ug/L Toluene -> 0.263 ppmv
convert_units("ug/L","ppmv",chem.name="toluene")
#
# 1 pppmv Toluene, 0.0038 mg/L
convert_units("ppmv","mg/L",chem.name="toluene")