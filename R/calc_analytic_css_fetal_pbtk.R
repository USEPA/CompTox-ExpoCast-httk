#'Calculate the analytic steady state concentration for modelfetalPBTK.
#'
#'This function calculates the maternal and fetal analytic steady state plasma
#'or venous blood 
#'concentrations as a result of infusion dosing for both mother and developing
#'fetus as a function of time.
#'



# Calculate steady-state plasma Css:

#Css <- hourly.dose / ( (1 + Clmetabolism * fup / ((Qliver + Qgut) * Rblood2plasma)) *
  #(Qcardiac - (Qrest + Qthyroid + Qadipose + Qplacenta) - Qkidney / (1 + Qgfr * fup / (Qkidney * Rblood2plasma) ) )
   #- (Qliver + Qgut) ) 

