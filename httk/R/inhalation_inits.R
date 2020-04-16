initParms_inh <- function(newParms = NULL) {
  parms <- c(
    BW = 0.0,
    Clmetabolismc = 0.0,
    vmax = 0.0,
    km = 0.0,
    hematocrit = 0.0,
    kgutabs = 0.0,
    Kkidney2pu = 0.0,
    Kliver2pu = 0.0,
    Krest2pu = 0.0,
    Kgut2pu = 0.0,
    Klung2pu = 0.0,
    Qcardiacc = 0.0,
    Qgfrc = 0.0,
    Qgutf = 0.0,
    Qkidneyf = 0.0,
    Qliverf = 0.0,
    Qlungf = 0.0,
    Vartc = 0.0,
    Vgutc = 0.0,
    Vkidneyc = 0.0,
    Vliverc = 0.0,
    Vlungc = 0.0,
    Vrestc = 0.0,
    Vvenc = 0.0,
    Fraction_unbound_plasma = 0.0,
    Rblood2plasma = 0.0,
    Clmetabolism = 0.0,
    Qcardiac = 0.0,
    Qgfr = 0.0,
    Qgut = 0.0,
    Qkidney = 0.0,
    Qliver = 0.0,
    Qlung = 0.0,
    Qrest = 0.0,
    Vart = 0.0,
    Vgut = 0.0,
    Vkidney = 0.0,
    Vliver = 0.0,
    Vlung = 0.0,
    Vrest = 0.0,
    Vven = 0.0,
    Qalv = 0.0,
    Kblood2air = 0.0,
    InhMag = 0.0,
    Period = 0.0,
    Exposure = 0.0,
    kUrtc = 0.0,
    kUrt = 0.0,
    Kmuc2air = 0.0,
    Vmucc = 0.0,
    Vmuc = 0.0,
    Vmax = 0.0,
    Km = 0.0
  )
  parms <- within(as.list(parms), {
    BW = 70;
    Clmetabolismc = 0.203;
    vmax = 0;
    km = 1;
    hematocrit = 0.44;
    kgutabs = 1;
    Kkidney2pu = 0;
    Kliver2pu = 0;
    Krest2pu = 0;
    Kgut2pu = 0;
    Klung2pu = 0;
    Qcardiacc = 4.8;
    Qgfrc = 0.108;
    Qgutf = 0.205;
    Qkidneyf = 0.221;
    Qliverf = 0.0536;
    Qlungf = 0;
    Vartc = 0.0487;
    Vgutc = 0.0158;
    Vkidneyc = 0.00119;
    Vliverc = 0.02448;
    Vlungc = 0.00723;
    Vrestc = 0.77654;
    Vvenc = 0.0487;
    Fraction_unbound_plasma = 0.0682;
    Rblood2plasma = 0.0;
    Clmetabolism = 0.0;
    Qcardiac = 0.0;
    Qgfr = 0.0;
    Qgut = 0.0;
    Qkidney = 0.0;
    Qliver = 0.0;
    Qlung = 0.0;
    Qrest = 0.0;
    Vart = 0.0;
    Vgut = 0.0;
    Vkidney = 0.0;
    Vliver = 0.0;
    Vlung = 0.0;
    Vrest = 0.0;
    Vven = 0.0;
    Qalv = 0;
    Kblood2air = 0;
    InhMag = 0;
    Period = 0;
    Exposure = 0;
    kUrtc = 11.0;
    kUrt = 0;
    Kmuc2air = 0;
    Vmucc = 0.0001;
    Vmuc = 0.0;
    Vmax = 0;
    Km = 1;
  })
  if (!is.null(newParms)) {
    if (!all(names(newParms) %in% c(names(parms)))) {
      stop("illegal parameter name")
    }
  }
  if (!is.null(newParms))
    parms[names(newParms)] <- newParms
  out <- .C("getParms_inh",  as.double(parms),
            out=double(length(parms)),
            as.integer(length(parms)))$out
  names(out) <- names(parms)
  out
}

Outputs_inh <- c(
    "Cgut",
    "Cliver",
    "Cven",
    "Clung",
    "Cart",
    "Crest",
    "Ckidney",
    "Cplasma",
    "Aplasma",
    "Calv",
    "Cendexh",
    "Cmixexh",
    "Cmuc"
)

initStates_inh <- function(parms, newStates = NULL) {
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
    Ainh = 0.0,
    Aexh = 0.0,
    Amuc = 0.0
  )
  if (!is.null(newStates)) {
    if (!all(names(newStates) %in% c(names(Y)))) {
      stop("illegal state variable name in newStates")
    }
    Y[names(newStates)] <- newStates
  }
  Y
}
