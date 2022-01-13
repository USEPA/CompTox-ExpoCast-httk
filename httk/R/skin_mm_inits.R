initparms_skin_mm <- function(newParms = NULL){
  parms <- c(
    depth = 0,
    V0 = 0,
    Fskinexposed = 0,
    totalSA = 0,
    SA = 0,
    Kp = 0,
    Kskin2media = 0,
    BW = 70,
    Vmax = 0,
    Km = 1,
    hematocrit = 0.44,
    kgutabs = 1,
    Kkidney2pu = 0,
    Kliver2pu = 0,
    Krest2pu = 0,
    Kgut2pu = 0,
    Klung2pu = 0,
    Kskin2pu = 0,
    Qcardiacc = 4.8,
    Qgfrc = 0.108,
    Qskinf = 0,
    Qskin = 0,
    Qskinexposed = 0,
    Qgutf = 0,
    Qkidneyf = 0,
    Qliverf = 0,
    Vartc = 0,
    Vgutc = 0,
    Vkidneyc = 0,
    Vliverc = 0,
    Vlungc = 0,
    Vrestc = 0,
    Vvenc = 0,
    Vskinc = 0,
    Vskin = 0,
    Vskinexposed = 0,
    Fraction_unbound_plasma = 0.0,
    Rblood2plasma = 0.0,
    Qcardiac = 0.0,
    Qgfr = 0.0,
    Qgut = 0.0,
    Qkidney = 0.0,
    Qliver = 0.0,
    Qrest = 0.0,
    Vart = 0.0,
    Vgut = 0.0,
    Vkidney = 0.0,
    Vliver = 0.0,
    Vlung = 0.0,
    Vrest = 0.0,
    Vven = 0.0
  )
  if (!is.null(newParms)) {
    if (!all(names(newParms) %in% c(names(parms)))) {
      stop("illegal parameter name")
    }
  }
  if (!is.null(newParms)) parms[names(newParms)] <- newParms
  out <- .C("getParms_skin_mm",
   as.double(parms),
  out=double(length(parms)),
  as.integer(length(parms)))$out
  names(out) <- names(parms)
  out
}

Outputs_skin_mm <- c(
    "Cgut",
    "Cliver",
    "Cven",
    "Clung",
    "Cart",
    "Crest",
    "Ckidney",
    "Cplasma",
    "Aplasma",
    "Cskinexposed",
    "Cskin",
    "Cmedia"
)


initState_skin_mm <- function(parms, newState = NULL) {
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
    Askinexposed = 0.0,
    Askin = 0.0,
    Amedia = 0.0
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
