initParms <- function(newParms = NULL) {
  parms <- c(
    pre_pregnant_BW = 0.0,
    Clmetabolismc = 0.0,
    Clmetabolism = 0.0,
    kgutabs = 0.0,
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
    Vgutc = 0.0,
    Vgut = 0.0,
    Vkidneyc = 0.0,
    Vkidney = 0.0,
    Vliverc = 0.0,
    Vliver = 0.0,
    Vlungc = 0.0,
    Vlung = 0.0,
    Vthyroidc = 0.0,
    Vthyroid = 0.0,
    Fraction_unbound_plasma = 0.0,
    Rblood2plasma = 0.0,
    gut_density = 0.0,
    kidney_density = 0.0,
    liver_density = 0.0,
    lung_density = 0.0,
    thyroid_density = 0.0,
    adipose_density = 0.0,
    ffmx_density = 0.0,
    placenta_density = 0.0,
    amnf_density = 0.0,
    brain_density = 0.0,
    BW_cubic_theta1 = 0.0,
    BW_cubic_theta2 = 0.0,
    BW_cubic_theta3 = 0.0,
    Wadipose_linear_theta0 = 0.0,
    Wadipose_linear_theta1 = 0.0,
    Wfkidney_gompertz_theta0 = 0.0,
    Wfkidney_gompertz_theta1 = 0.0,
    Wfkidney_gompertz_theta2 = 0.0,
    Wfthyroid_gompertz_theta0 = 0.0,
    Wfthyroid_gompertz_theta1 = 0.0,
    Wfthyroid_gompertz_theta2 = 0.0,
    Wfliver_gompertz_theta0 = 0.0,
    Wfliver_gompertz_theta1 = 0.0,
    Wfliver_gompertz_theta2 = 0.0,
    Wfbrain_gompertz_theta0 = 0.0,
    Wfbrain_gompertz_theta1 = 0.0,
    Wfbrain_gompertz_theta2 = 0.0,
    Wfgut_gompertz_theta0 = 0.0,
    Wfgut_gompertz_theta1 = 0.0,
    Wfgut_gompertz_theta2 = 0.0,
    Wflung_gompertz_theta0 = 0.0,
    Wflung_gompertz_theta1 = 0.0,
    Wflung_gompertz_theta2 = 0.0,
    hematocrit_quadratic_theta0 = 0.0,
    hematocrit_quadratic_theta1 = 0.0,
    hematocrit_quadratic_theta2 = 0.0,
    fhematocrit_cubic_theta1 = 0.0,
    fhematocrit_cubic_theta2 = 0.0,
    fhematocrit_cubic_theta3 = 0.0,
    fBW_gompertz_theta0 = 0.0,
    fBW_gompertz_theta1 = 0.0,
    fBW_gompertz_theta2 = 0.0,
    Vplacenta_cubic_theta1 = 0.0,
    Vplacenta_cubic_theta2 = 0.0,
    Vplacenta_cubic_theta3 = 0.0,
    Vamnf_logistic_theta0 = 0.0,
    Vamnf_logistic_theta1 = 0.0,
    Vamnf_logistic_theta2 = 0.0,
    Vplasma_mod_logistic_theta0 = 0.0,
    Vplasma_mod_logistic_theta1 = 0.0,
    Vplasma_mod_logistic_theta2 = 0.0,
    Vplasma_mod_logistic_theta3 = 0.0,
    venous_blood_fraction = 0.0,
    arterial_blood_fraction = 0.0,
    fblood_weight_ratio = 0.0,
    Qcardiac_cubic_theta0 = 0.0,
    Qcardiac_cubic_theta1 = 0.0,
    Qcardiac_cubic_theta2 = 0.0,
    Qcardiac_cubic_theta3 = 0.0,
    term = 0.0,
    Qgut_percent_initial = 0.0,
    Qgut_percent_terminal = 0.0,
    Qkidney_cubic_theta0 = 0.0,
    Qkidney_cubic_theta1 = 0.0,
    Qkidney_cubic_theta2 = 0.0,
    Qkidney_cubic_theta3 = 0.0,
    Qliver_percent_initial = 0.0,
    Qliver_percent_terminal = 0.0,
    Qthyroid_percent_initial = 0.0,
    Qthyroid_percent_terminal = 0.0,
    Qplacenta_linear_theta1 = 0.0,
    Qadipose_percent_initial = 0.0,
    Qadipose_percent_terminal = 0.0,
    Qgfr_quadratic_theta0 = 0.0,
    Qgfr_quadratic_theta1 = 0.0,
    Qgfr_quadratic_theta2 = 0.0,
    Qfrvtl_logistic_theta0 = 0.0,
    Qfrvtl_logistic_theta1 = 0.0,
    Qfrvtl_logistic_theta2 = 0.0,
    Qflvtl_logistic_theta0 = 0.0,
    Qflvtl_logistic_theta1 = 0.0,
    Qflvtl_logistic_theta2 = 0.0,
    Qfda_logistic_theta0 = 0.0,
    Qfda_logistic_theta1 = 0.0,
    Qfda_logistic_theta2 = 0.0,
    Qfplacenta_logistic_theta0 = 0.0,
    Qfplacenta_logistic_theta1 = 0.0,
    Qfplacenta_logistic_theta2 = 0.0,
    Qfdv_gompertz_theta0 = 0.0,
    Qfdv_gompertz_theta1 = 0.0,
    Qfdv_gompertz_theta2 = 0.0,
    Qfnonplacental_percent = 0.0,
    Qfgut_percent = 0.0,
    Qfkidney_percent = 0.0,
    Qfbrain_percent = 0.0,
    Qbrain_percent = 0.0,
    Qkidney_percent = 0.0,
    Qgut_percent = 0.0,
    Qfliver_percent = 0.0,
    Qfthyroid_percent = 0.0
  )
  parms <- within(as.list(parms), {
    pre_pregnant_BW = 0;
    Clmetabolismc = 0;
    Clmetabolism = 0;
    kgutabs = 0;
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
    Vgutc = 0;
    Vgut = 0;
    Vkidneyc = 0;
    Vkidney = 0;
    Vliverc = 0;
    Vliver = 0;
    Vlungc = 0;
    Vlung = 0;
    Vthyroidc = 0;
    Vthyroid = 0;
    Fraction_unbound_plasma = 0;
    Rblood2plasma = 0;
    gut_density = 0;
    kidney_density = 0;
    liver_density = 0;
    lung_density = 0;
    thyroid_density = 0;
    adipose_density = 0;
    ffmx_density = 0;
    placenta_density = 0;
    amnf_density = 0;
    brain_density = 0;
    BW_cubic_theta1 = 0;
    BW_cubic_theta2 = 0;
    BW_cubic_theta3 = 0;
    Wadipose_linear_theta0 = 0;
    Wadipose_linear_theta1 = 0;
    Wfkidney_gompertz_theta0 = 0;
    Wfkidney_gompertz_theta1 = 0;
    Wfkidney_gompertz_theta2 = 0;
    Wfthyroid_gompertz_theta0 = 0;
    Wfthyroid_gompertz_theta1 = 0;
    Wfthyroid_gompertz_theta2 = 0;
    Wfliver_gompertz_theta0 = 0;
    Wfliver_gompertz_theta1 = 0;
    Wfliver_gompertz_theta2 = 0;
    Wfbrain_gompertz_theta0 = 0;
    Wfbrain_gompertz_theta1 = 0;
    Wfbrain_gompertz_theta2 = 0;
    Wfgut_gompertz_theta0 = 0;
    Wfgut_gompertz_theta1 = 0;
    Wfgut_gompertz_theta2 = 0;
    Wflung_gompertz_theta0 = 0;
    Wflung_gompertz_theta1 = 0;
    Wflung_gompertz_theta2 = 0;
    hematocrit_quadratic_theta0 = 0;
    hematocrit_quadratic_theta1 = 0;
    hematocrit_quadratic_theta2 = 0;
    fhematocrit_cubic_theta1 = 0;
    fhematocrit_cubic_theta2 = 0;
    fhematocrit_cubic_theta3 = 0;
    fBW_gompertz_theta0 = 0;
    fBW_gompertz_theta1 = 0;
    fBW_gompertz_theta2 = 0;
    Vplacenta_cubic_theta1 = 0;
    Vplacenta_cubic_theta2 = 0;
    Vplacenta_cubic_theta3 = 0;
    Vamnf_logistic_theta0 = 0;
    Vamnf_logistic_theta1 = 0;
    Vamnf_logistic_theta2 = 0;
    Vplasma_mod_logistic_theta0 = 0;
    Vplasma_mod_logistic_theta1 = 0;
    Vplasma_mod_logistic_theta2 = 0;
    Vplasma_mod_logistic_theta3 = 0;
    venous_blood_fraction = 0;
    arterial_blood_fraction = 0;
    fblood_weight_ratio = 0;
    Qcardiac_cubic_theta0 = 0;
    Qcardiac_cubic_theta1 = 0;
    Qcardiac_cubic_theta2 = 0;
    Qcardiac_cubic_theta3 = 0;
    term = 0;
    Qgut_percent_initial = 0;
    Qgut_percent_terminal = 0;
    Qkidney_cubic_theta0 = 0;
    Qkidney_cubic_theta1 = 0;
    Qkidney_cubic_theta2 = 0;
    Qkidney_cubic_theta3 = 0;
    Qliver_percent_initial = 0;
    Qliver_percent_terminal = 0;
    Qthyroid_percent_initial = 0;
    Qthyroid_percent_terminal = 0;
    Qplacenta_linear_theta1 = 0;
    Qadipose_percent_initial = 0;
    Qadipose_percent_terminal = 0;
    Qgfr_quadratic_theta0 = 0;
    Qgfr_quadratic_theta1 = 0;
    Qgfr_quadratic_theta2 = 0;
    Qfrvtl_logistic_theta0 = 0;
    Qfrvtl_logistic_theta1 = 0;
    Qfrvtl_logistic_theta2 = 0;
    Qflvtl_logistic_theta0 = 0;
    Qflvtl_logistic_theta1 = 0;
    Qflvtl_logistic_theta2 = 0;
    Qfda_logistic_theta0 = 0;
    Qfda_logistic_theta1 = 0;
    Qfda_logistic_theta2 = 0;
    Qfplacenta_logistic_theta0 = 0;
    Qfplacenta_logistic_theta1 = 0;
    Qfplacenta_logistic_theta2 = 0;
    Qfdv_gompertz_theta0 = 0;
    Qfdv_gompertz_theta1 = 0;
    Qfdv_gompertz_theta2 = 0;
    Qfnonplacental_percent = 0;
    Qfgut_percent = 0;
    Qfkidney_percent = 0;
    Qfbrain_percent = 0;
    Qbrain_percent = 0;
    Qkidney_percent = 0;
    Qgut_percent = 0;
    Qfliver_percent = 0;
    Qfthyroid_percent = 0;
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
    "tw",
    "Cgut",
    "Cliver",
    "Cven",
    "Clung",
    "Cart",
    "Cadipose",
    "Crest",
    "Ckidney",
    "Cplasma",
    "Aplasma",
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
    "Afplasma",
    "Cfplasma"
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
