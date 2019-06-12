#Note some variables, like hematocrit, employed with distributions elsewhere 
#in httk, are not similarly treated or included in parameter lists here in the
#pregnancy case for the sake of simplicity due to the the absence of data to 
#better support anything other than simulation of the average case. 

#Compile list of useful parameter names for direct access, though these not
#all directly passed to ODE solver. This list also contains the names of 
#equivalent variables to those used in ode solving with modelfetalPBTK.c, as 
#they are found named differently elsewhere in httk prior to being bridged
#over in name for incorporation into the ode model solving process. 
param.names.fetal.pbtk = c("pre_pregnant_BW", 
                           "Clmetabolismc", 
                           "Kgutabs",
                           "Kkidney2pu",
                           "Kliver2pu",
                           "Krbc2pu", 
                           "Kadipose2pu", 
                           "Krest2pu", 
                           "Klung2pu", 
                           "Kgut2pu",
                           "Kthyroid2pu", 
                           "Kplacenta2pu", 
                           "Kfplacenta2pu",
                           "Kfkidney2pu", 
                           "Kfrest2pu",
                           "Kfthyroid2pu",
                           "Kfliver2pu", 
                           "Kflung2pu", 
                           "Kfgut2pu", 
                           "Kfbrain2pu", 
                           "Vartc",
                           "Vvenc",
                           "Vgutc",
                           "Vkidneyc",
                           "Vliverc",
                           "Vlungc",
                           "Vthyroidc",
                           "Clint",
                           "Funbound.plasma",
                           "Funbound.plasma.dist",
                           "Funbound.plasma.adjustment",
                           "Fgutabs",
                           "Fhep.assay.correction",
                           "liver.density",
                           "million.cells.per.gliver",
                           "MW",
                           "Pow",
                           "pKa_Donor",
                           "pKa_Accept",
                           "MA",
                           "Rblood2pu")

#Parameter names as used explicitly by the .c solver. 
param.names.fetal.pbtk.solver = c("pre_pregnant_BW", 
                                  "Clmetabolismc", 
                                  "Kgutabs",
                                  "Kkidney2pu",
                                  "Kliver2pu",
                                  "Krbc2pu", 
                                  "Kadipose2pu", 
                                  "Krest2pu", 
                                  "Klung2pu", 
                                  "Kgut2pu",
                                  "Kthyroid2pu", 
                                  "Kplacenta2pu", 
                                  "Kfplacenta2pu",
                                  "Kfkidney2pu", 
                                  "Kfrest2pu",
                                  "Kfthyroid2pu",
                                  "Kfliver2pu", 
                                  "Kflung2pu", 
                                  "Kfgut2pu", 
                                  "Kfbrain2pu", 
                                  "Vartc",
                                  "Vvenc",
                                  "Vgutc",
                                  "Vkidneyc",
                                  "Vliverc",
                                  "Vlungc",
                                  "Vthyroidc",
                                  "Fraction_unbound_plasma",
                                  "Ratioblood2plasma")



initParmsfetalpbtk <- function(newParms = NULL) {
  parms <- c(
    pre_pregnant_BW = 61.103,
    Clmetabolismc = 0.0,
    Kgutabs = 1,
    Kkidney2pu = 0.0,
    Kliver2pu = 0.0,
    Krbc2pu = 0.0,
    Kadipose2pu = 0.0,
    Krest2pu = 0.0,
    Klung2pu = 0.0,
    Kgut2pu = 0.0,
    Kthyroid2pu = 0.0,
    Kplacenta2pu = 0.0,
    Kfplacenta2pu = 0.0,
    Kfkidney2pu = 0.0,
    Kfrest2pu = 0.0,
    Kfthyroid2pu = 0.0,
    Kfliver2pu = 0.0,
    Kflung2pu = 0.0,
    Kfgut2pu = 0.0,
    Kfbrain2pu = 0.0,
    Vartc = 0.0,
    Vvenc = 0.0,
    Vgutc = 0.0,
    Vkidneyc = 0.0,
    Vliverc = 0.0,
    Vlungc = 0.0,
    Vart = 0.0,
    Vven = 0.0,
    Vgut = 0.0,
    Vkidney = 0.0,
    Vliver = 0.0,
    Vlung = 0.0,
    Vthyroidc = 0.0,
    Vthyroid = 0.0,
    Fraction_unbound_plasma = 0.0,
    Ratioblood2plasma = 0.0,
    Clmetabolism = 0.0
  )
  if (!is.null(newParms)) {
    if (!all(names(newParms) %in% c(names(parms)))) {
      stop(paste("illegal parameter name(s):",names(newParms)[!(names(newParms) %in% c(names(parms)))],sep=" "))
    }
  }
  if (!is.null(newParms))
    parms[names(newParms)] <- newParms
  out <- .C("getParmsfetalpbtk",  as.double(parms),
            out=double(length(parms)),
            as.integer(length(parms)))$out
  names(out) <- names(parms)
  out
}

Outputsfetalpbtk <- c(
    "Cgut",
    "Cliver",
    "Cven",
    "Clung",
    "Cart",
    "Cadipose",
    "Crest",
    "Ckidney",
    "Cserum",
    "Aserum",
    "Cthyroid",
    "Cplacenta",
    "Cfliver",
    "Cfven",
    "Cfart",
    "Cfgut",
    "Cflung",
    "Cfrest",
    "Cfthyroid",
    "Cfkidney",
    "Cfbrain",
    "Afserum",
    "Cfserum"
)

initStatesfetalpbtk <- function(parms, newStates = NULL) {
  Y <- c(
    Agutlumen = 0.0,
    Agut = 0.0,
    Aliver = 0.0,
    Aven = 0.0,
    Alung = 0.0,
    Aart = 0.0,
    Aadipose = 0.0,
    Arest = 0.0,
    Akidney = 0.0,
    Atubules = 0.0,
    Ametabolized = 0.0,
    AUC = 0.0,
    fAUC = 0.0,
    Athyroid = 0.0,
    Aplacenta = 0.0,
    Afgut = 0.0,
    Aflung = 0.0,
    Afliver = 0.0,
    Afven = 0.0,
    Afart = 0.0,
    Afrest = 0.0,
    Afthyroid = 0.0,
    Afkidney = 0.0,
    Afbrain = 0.0
  )
  if (!is.null(newStates)) {
    if (!all(names(newStates) %in% c(names(Y)))) {
      stop("illegal state variable name in newStates")
    }
    Y[names(newStates)] <- newStates
  }
  Y
}
