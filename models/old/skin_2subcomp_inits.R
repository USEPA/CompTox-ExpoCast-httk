initParms <- function(newParms = NULL) {
  parms <- c(
    skin_depth = 0.0,
    Fskin_depth_sc = 0.0,
    Fskin_depth_cd = 0.0,
    Pmedia2sc = 0.0,
    Psc2cd = 0.0,
    V0 = 0.0,
    Fskin_exposed = 0.0,
    totalSA = 0.0,
    SA_exposed = 0.0,
    BW = 0.0,
    Clmetabolismc = 0.0,
    hematocrit = 0.0,
    kgutabs = 0.0,
    Ksc2media = 0.0,
    Ksc2cd = 0.0,
    Kcd2pu = 0.0,
    Kkidney2pu = 0.0,
    Kliver2pu = 0.0,
    Krest2pu = 0.0,
    Kgut2pu = 0.0,
    Klung2pu = 0.0,
    Qcardiacc = 0.0,
    Qgfrc = 0.0,
    Qcomposite_dermalc = 0.0,
    Qgutc = 0.0,
    Qkidneyc = 0.0,
    Qliverc = 0.0,
    Vartc = 0.0,
    Vgutc = 0.0,
    Vkidneyc = 0.0,
    Vliverc = 0.0,
    Vlungc = 0.0,
    Vrestc = 0.0,
    Vvenc = 0.0,
    Vskinc = 0.0,
    Vstratum_corneumc = 0.0,
    Vcomposite_dermalc = 0.0,
    Fraction_unbound_plasma = 0.0,
    Rblood2plasma = 0.0,
    Clmetabolism = 0.0,
    Qcardiac = 0.0,
    Qcomposite_dermal = 0.0,
    Qcomposite_dermal_exposed = 0.0,
    Qcomposite_dermal_unexposed = 0.0,
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
    Vven = 0.0,
    Vskin = 0.0,
    Vstratum_corneum = 0.0,
    Vstratum_corneum_exposed = 0.0,
    Vstratum_corneum_unexposed = 0.0,
    Vcomposite_dermal = 0.0,
    Vcomposite_dermal_exposed = 0.0,
    Vcomposite_dermal_unexposed = 0.0
  )
  parms <- within(as.list(parms), {
    skin_depth = 0;
    Fskin_depth_sc = 0;
    Fskin_depth_cd = 0;
    Pmedia2sc = 0;
    Psc2cd = 0;
    V0 = 0;
    Fskin_exposed = 0;
    totalSA = 0;
    SA_exposed = 0;
    BW = 70;
    Clmetabolismc = 0.203;
    hematocrit = 0.44;
    kgutabs = 1;
    Ksc2media = 0;
    Ksc2cd = 0;
    Kcd2pu = 0;
    Kkidney2pu = 0;
    Kliver2pu = 0;
    Krest2pu = 0;
    Kgut2pu = 0;
    Klung2pu = 0;
    Qcardiacc = 4.8;
    Qgfrc = 0.108;
    Qcomposite_dermalc = 0;
    Qgutc = 0;
    Qkidneyc = 0;
    Qliverc = 0;
    Vartc = 0;
    Vgutc = 0;
    Vkidneyc = 0;
    Vliverc = 0;
    Vlungc = 0;
    Vrestc = 0;
    Vvenc = 0;
    Vskinc = 0;
    Vstratum_corneumc = 0;
    Vcomposite_dermalc = 0;
    Fraction_unbound_plasma = 0.0;
    Rblood2plasma = 0.0;
    Clmetabolism = 0.0;
    Qcardiac = 0.0;
    Qcomposite_dermal = 0;
    Qcomposite_dermal_exposed = 0;
    Qcomposite_dermal_unexposed = 0;
    Qgfr = 0.0;
    Qgut = 0.0;
    Qkidney = 0.0;
    Qliver = 0.0;
    Qrest = 0.0;
    Vart = 0.0;
    Vgut = 0.0;
    Vkidney = 0.0;
    Vliver = 0.0;
    Vlung = 0.0;
    Vrest = 0.0;
    Vven = 0.0;
    Vskin = 0;
    Vstratum_corneum = 0;
    Vstratum_corneum_exposed = 0;
    Vstratum_corneum_unexposed = 0;
    Vcomposite_dermal = 0;
    Vcomposite_dermal_exposed = 0;
    Vcomposite_dermal_unexposed = 0;
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
    "Crest",
    "Ckidney",
    "Cplasma",
    "Aplasma",
    "Cstratum_corneum_exposed",
    "Cstratum_corneum_unexposed",
    "Ccomposite_dermal_exposed",
    "Ccomposite_dermal_unexposed",
    "Cmedia"
)

initStates <- function(parms, newStates = NULL) {
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
    Astratum_corneum_exposed = 0.0,
    Astratum_corneum_unexposed = 0.0,
    Acomposite_dermal_exposed = 0.0,
    Acomposite_dermal_unexposed = 0.0,
    Amedia = 0.0
  )
  if (!is.null(newStates)) {
    if (!all(names(newStates) %in% c(names(Y)))) {
      stop("illegal state variable name in newStates")
    }
    Y[names(newStates)] <- newStates
  }
  Y
}
