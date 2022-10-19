initParms <- function(newParms = NULL) {
  parms <- c(
    BW = 0,
    CLmetabolismc = 0,
    kgutabs = 0,
    Qcardiacc = 0,
    Qgfrc = 0,
    Qgutf = 0,
    Qliverf = 0,
    Vportvenc = 0,
    Vliverc = 0,
    Vsyscompc = 0,
    Fraction_unbound_plasma = 0,
    CLmetabolism = 0.0,
    Qcardiac = 0,
    Qgfr = 0.0,
    Qgut = 0.0,
    Qliver = 0.0,
    Kliver2plasma = 0,
    Krest2plasma = 0,
    Ratioblood2plasma = 0
  )

  if (!is.null(newParms)) {
    if (!all(names(newParms) %in% c(names(parms)))) {
      stop("illegal parameter name")
    }
    parms[names(newParms)] <- newParms
  }

  parms <- within(as.list(parms), {
  })
  out <- .C("getParms",  as.double(parms),
            out=double(length(parms)),
            as.integer(length(parms)))$out
  names(out) <- names(parms)
  out
}

Outputs <- c(
    "Cportven",
    "Cliver",
    "Csyscomp"
)

initStates <- function(parms, newStates = NULL) {
  Y <- c(
    Aintestine = 0.0,
    Aportven = 0.0,
    Aliver = 0.0,
    Asyscomp = 0.0,
    Ametabolized = 0.0,
    Atubules = 0.0
  )

  if (!is.null(newStates)) {
    if (!all(names(newStates) %in% c(names(Y)))) {
      stop("illegal state variable name in newStates")
    }
    Y[names(newStates)] <- newStates
  }

.C("initState", as.double(Y));
Y
}
