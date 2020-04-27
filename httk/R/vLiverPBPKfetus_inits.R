initparmsfetus <- function(newParms = NULL){
  parms <- c(
    pre_pregnant_BW = 0,
    Clmetabolismc = 0.0,
    hematocrit = 0,
    kgutabs = 1,
    Kkidney2plasma = 0,
    Kliver2plasma = 0,
    Krbc2plasma = 0,
    Krest2plasma = 0,
    Klung2plasma = 0,
    Kgut2plasma = 0,
    Kthyroid2plasma = 0,
    Kplacenta2plasma = 0,
    Kfplacenta2plasma = 0,
    Kfkidney2plasma = 0,
    Kfrest2plasma = 0,
    Kfthyroid2plasma = 0,
    Kfliver2plasma = 0,
    Kflung2plasma = 0,
    Kfgut2plasma = 0,
    Kfbrain2plasma = 0,
    Vartc = 0,
    Vvenc = 0,
    Vgutc = 0,
    Vkidneyc = 0,
    Vliverc = 0,
    Vlungc = 0,
    Vart = 0,
    Vven = 0,
    Vgut = 0,
    Vkidney = 0,
    Vliver = 0,
    Vlung = 0,
    Vthyroidc = 0,
    Vthyroid = 0,
    Vfgutc = 0,
    Fraction_unbound_plasma = 0,
    Ratioblood2plasma = 0.0,
    Clmetabolism = 0.0,
    Qgfrc = 0,
    Qgfr = 0.0
  )
  if (!is.null(newParms)) {
    if (!all(names(newParms) %in% c(names(parms)))) {
      stop("illegal parameter name")
    }
  }
  if (!is.null(newParms)) parms[names(newParms)] <- newParms
  out <- .C("getParms_fetus",
   as.double(parms),
  out=double(length(parms)),
  as.integer(length(parms)))$out
  names(out) <- names(parms)
  out
}

Outputsfetus <- c(
    "Cgut",
    "Cliver",
    "Cven",
    "Clung",
    "Cart",
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


initStatefetus <- function(parms, newState = NULL) {
  Y <- c(
    Agutlumen = 0.0,
    Agut = 0.0,
    Aliver = 0.0,
    Aven = 0.0,
    Alung = 0.0,
    Aart = 0.0,
    Arest = 0.0,
    Akidney = 0.0,
    Atubules = 0.0,
    Ametabolized = 0.0,
    AUC = 0.0,
    AUC_fetus = 0.0,
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
  Y <- with(as.list(parms), {  Y
  })

  if (!is.null(newState)) {
    if (!all(names(newState) %in% c(names(Y)))) {
      stop("illegal state variable name in newState")
    }
    Y[names(newState)] <- newState
  }
  Y
}
