initParms <- function(newParms = NULL) {
  parms <- c(
    pre_pregnant_BW = 0.0,
    CLmetabolismc = 0.0,
    Kgutabs = 0.0,
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
    CLmetabolism = 0.0
  )
  parms <- within(as.list(parms), {
    pre_pregnant_BW = 61.103;
    CLmetabolismc = 0.0;
    Kgutabs = 1;
    Kkidney2pu = 0;
    Kliver2pu = 0;
    Krbc2pu = 0;
    Kadipose2pu = 0;
    Krest2pu = 0;
    Klung2pu = 0;
    Kgut2pu = 0;
    Kthyroid2pu = 0;
    Kplacenta2pu = 0;
    Kfplacenta2pu = 0;
    Kfkidney2pu = 0;
    Kfrest2pu = 0;
    Kfthyroid2pu = 0;
    Kfliver2pu = 0;
    Kflung2pu = 0;
    Kfgut2pu = 0;
    Kfbrain2pu = 0;
    Vartc = 0;
    Vvenc = 0;
    Vgutc = 0;
    Vkidneyc = 0;
    Vliverc = 0;
    Vlungc = 0;
    Vart = 0;
    Vven = 0;
    Vgut = 0;
    Vkidney = 0;
    Vliver = 0;
    Vlung = 0;
    Vthyroidc = 0;
    Vthyroid = 0;
    Fraction_unbound_plasma = 0;
    Ratioblood2plasma = 0.0;
    CLmetabolism = 0.0;
  })
  if (!is.null(newParms)) {
    if (!all(names(newParms) %in% c(names(parms)))) {
      stop("illegal parameter name")
    }
  }
  if (!is.null(newParms))
    parms[names(newParms)] <- newParms
  out <- .C("getParms",  as.double(parms),
            out=double(length(parms)),
            as.integer(length(parms)))$out
  names(out) <- names(parms)
  out
}

Outputs <- c(
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

initStates <- function(parms, newStates = NULL) {
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
