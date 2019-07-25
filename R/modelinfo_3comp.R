# Add this model to the list of models:
model.list[["3compartment"]]$analytic.css.func <- "calc_analytic_css_3comp"

#Define the parameter names for each model in one place so that all functions can use them:
param.names.3comp <- c("BW",
                     "Clint",
                     "Clint.dist",
                     "Clmetabolismc",
                     "Funbound.plasma",
                     "Funbound.plasma.dist",
                     "Funbound.plasma.adjustment",
                     "Fabsgut",
                     "Fabs",
                     "Fgut",
                     "Fhep.assay.correction",
                     "hematocrit",
                     "Kgut2pu",
                     "Krbc2pu",
                     "kgutabs",
                     "Kliver2pu",
                     "Krest2pu",
                     "liver.density",
                     "million.cells.per.gliver",
                     "MW",
                     "Pow",
                     "pKa_Donor",
                     "pKa_Accept",
                     "MA",
                     "Qcardiacc",
                     "Qgfrc",
                     "Qgutf",
                     "Qliverf",
                     "Rblood2plasma",
                     "Vgutc",
                     "Vliverc",
                     "Vrestc")

param.names.3comp.solver <- c("BW",
                     "Clmetabolismc",
                     'Fraction_unbound_plasma',
                     "Kgut2plasma",
                     "kgutabs",
                     "Kliver2plasma",
                     "Krest2plasma",
                     "Qcardiacc",
                     "Qgfrc",
                     "Qgutf",
                     "Qliverf",
                     "Ratioblood2plasma",
                     "Vgut",
                     "Vliver",
                     "Vrest")

initparms3comp <- function(newParms = NULL){
  parms <- c(
    BW = 70,
    Clmetabolismc = 0.203,
    kgutabs = 1,
    Qcardiacc = 0,
    Qgfrc = 0.108,
    Qgutf = 0.205,
    Qliverf = 0.0536,
    Vgut = 0,
    Vliver = 0,
    Vrest = 0,
    Fraction_unbound_plasma = 0.0682,
    Clmetabolism = 0.0,
    Qcardiac = 0,
    Qgfr = 0.0,
    Qgut = 0.0,
    Qliver = 0.0,
    Kliver2plasma = 0,
    Krest2plasma = 0,
    Kgut2plasma = 0,
    Ratioblood2plasma = 0
  )
  if (!is.null(newParms)) {
    if (!all(names(newParms) %in% c(names(parms)))) {
      stop("illegal parameter name")
    }
  }
  if (!is.null(newParms)) parms[names(newParms)] <- newParms
  out <- .C("getParms3comp",
   as.double(parms),
  out=double(length(parms)),
  as.integer(length(parms)))$out
  names(out) <- names(parms)
  out
}

Outputs3comp <- c(
    "Cgut",
    "Cliver",
    "Crest",
    "Cserum"
)


initState3comp <- function(parms, newState = NULL) {
  Y <- c(
    Agutlumen = 0.0,
    Agut = 0.0,
    Aliver = 0.0,
    Arest = 0.0,
    Ametabolized = 0.0,
    Atubules = 0.0,
    AUC = 0.0
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
