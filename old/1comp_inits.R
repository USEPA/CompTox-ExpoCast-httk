initParms <- function(newParms = NULL) {
  parms <- c(
    vdist = 0,
    ke = 0,
    kgutabs = 1,
    BW = 70
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
    "Ccompartment"
)

initStates <- function(parms, newStates = NULL) {
  Y <- c(
    Agutlumen = 0.0,
    Acompartment = 0.0,
    Ametabolized = 0.0,
    AUC = 0.0
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
