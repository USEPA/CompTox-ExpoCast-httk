initParms <- function(newParms = NULL) {
  parms <- c(
    pre_pregnant_BW = 0,
    Clmetabolismc = 0,
    Clmetabolism = 0,
    kgutabs = 0,
    Kkidney2pu = 0,
    Kliver2pu = 0,
    Kadipose2pu = 0,
    Krest2pu = 0,
    Klung2pu = 0,
    Kgut2pu = 0,
    Krbc2pu = 0,
    Kthyroid2pu = 0,
    Kconceptus2pu_initial = 0,
    Kconceptus2pu_final = 0,
    Vgutc = 0,
    Vgut = 0,
    Vkidneyc = 0,
    Vkidney = 0,
    Vliverc = 0,
    Vliver = 0,
    Vlungc = 0,
    Vlung = 0,
    Vthyroidc = 0,
    Vthyroid = 0,
    Fraction_unbound_plasma = 0,
    gut_density = 0,
    kidney_density = 0,
    liver_density = 0,
    lung_density = 0,
    thyroid_density = 0,
    adipose_density = 0,
    ffmx_density = 0,
    placenta_density = 0,
    amnf_density = 0,
    brain_density = 0,
    BW_cubic_theta1 = 0,
    BW_cubic_theta2 = 0,
    BW_cubic_theta3 = 0,
    Wadipose_linear_theta0 = 0,
    Wadipose_linear_theta1 = 0,
    hematocrit_quadratic_theta0 = 0,
    hematocrit_quadratic_theta1 = 0,
    hematocrit_quadratic_theta2 = 0,
    fBW_gompertz_theta0 = 0,
    fBW_gompertz_theta1 = 0,
    fBW_gompertz_theta2 = 0,
    Vplacenta_cubic_theta1 = 0,
    Vplacenta_cubic_theta2 = 0,
    Vplacenta_cubic_theta3 = 0,
    Vamnf_logistic_theta0 = 0,
    Vamnf_logistic_theta1 = 0,
    Vamnf_logistic_theta2 = 0,
    Vplasma_mod_logistic_theta0 = 0,
    Vplasma_mod_logistic_theta1 = 0,
    Vplasma_mod_logistic_theta2 = 0,
    Vplasma_mod_logistic_theta3 = 0,
    venous_blood_fraction = 0,
    arterial_blood_fraction = 0,
    Qcardiac_cubic_theta0 = 0,
    Qcardiac_cubic_theta1 = 0,
    Qcardiac_cubic_theta2 = 0,
    Qcardiac_cubic_theta3 = 0,
    term = 0,
    Qgut_percent_initial = 0,
    Qgut_percent_terminal = 0,
    Qkidney_cubic_theta0 = 0,
    Qkidney_cubic_theta1 = 0,
    Qkidney_cubic_theta2 = 0,
    Qkidney_cubic_theta3 = 0,
    Qliver_percent_initial = 0,
    Qliver_percent_terminal = 0,
    Qthyroid_percent_initial = 0,
    Qthyroid_percent_terminal = 0,
    Qplacenta_linear_theta1 = 0,
    Qadipose_percent_initial = 0,
    Qadipose_percent_terminal = 0,
    Qgfr_quadratic_theta0 = 0,
    Qgfr_quadratic_theta1 = 0,
    Qgfr_quadratic_theta2 = 0,
    fBW_13wks = 0,
    Vplacenta_13wks = 0,
    Vamnf_13wks = 0,
    Vconceptus_final = 0,
    Vconceptus_initial = 0,
    Qconceptus_final = 0,
    Qconceptus_initial = 0
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
    "Cgut",
    "Cliver",
    "Ckidney",
    "Clung",
    "Cven",
    "Cart",
    "Cadipose",
    "Cthyroid",
    "Crest",
    "Cconceptus",
    "Cplasma",
    "Aplasma",
    "Rblood2plasma",
    "Vven",
    "Vart",
    "Vadipose",
    "Vrest",
    "hematocrit",
    "Vconceptus",
    "Qconceptus",
    "Vffmx",
    "Vallx"
)

initStates <- function(parms, newStates = NULL) {
  Y <- c(
    Agutlumen = 0.0,
    Agut = 0.0,
    Aliver = 0.0,
    Akidney = 0.0,
    Alung = 0.0,
    Aven = 0.0,
    Aart = 0.0,
    Aadipose = 0.0,
    Athyroid = 0.0,
    Arest = 0.0,
    Aconceptus = 0.0,
    Atubules = 0.0,
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
