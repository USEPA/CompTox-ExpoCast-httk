# R CMD BATCH --no-timing --no-restore --no-save unit_test.R unit_test.Rout

# Get rid of anything in the workspace:
rm(list=ls()) 

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
# Compare with 
#   https://www.eurofinsus.com/environment-testing/services/air-and-vapor/unit-conversion-calculator/
# STP assumes 24.45 = (25?C and 1 atm)
# 1 ug/L Toluene -> 0.26539 ppmv
convert_units("ug/L","ppmv",
                chem.name="toluene",
                state="gas")
#
# 1 ppmv Toluene -> 0.0038 mg/L
convert_units("ppmv","mg/L",
              chem.name="toluene",
              state="gas")
# 1 ug/L Styrene ->  0.23478 ppmv
convert_units("ug/L","ppmv",
                chem.name="styrene",
                state="gas")
                
# Test that convert_solve_x doesn't throw any errors:
head(solve_gas_pbtk(chem.name="bisphenol a",
                    times=c(0,0.1,0.05),
                    output.units=setNames("mg/m3","Cendexhppmv"),
                    method = "lsode",
                    mf = 10, 
                    rtol=1e-7,
                    atol=1e-7))

# Quit without saving or displaying messages:
quit("no")