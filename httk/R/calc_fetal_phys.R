#' Calculate maternal-fetal physiological parameters
#' 
#' This function uses the equations from Kapraun (2019) to calculate chemical-
#' independent physiological paramreters as a function of gestational age in 
#' weeks.
#' 
#' \deqn{BW = pre_pregnant_BW + 
#'    BW_cubic_theta1 * tw + 
#'    BW_cubic_theta2 * tw^2 + 
#'    BW_cubic_theta3 * tw^3
#'  }
#'
#' \deqn{
#'  Wadipose =  Wadipose_linear_theta0 + Wadipose_linear_theta1 * tw ;
#'  }
#'
#' \deqn{
#'  Wfkidney = 0.001 * Wfkidney_gompertz_theta0 * exp ( Wfkidney_gompertz_theta1 / Wfkidney_gompertz_theta2 * ( 1 - exp ( - Wfkidney_gompertz_theta2 * tw ) ) ) ;
#'  }
#'
#' \deqn{
#'  Wfthyroid = 0.001 * Wfthyroid_gompertz_theta0 * exp ( Wfthyroid_gompertz_theta1 / Wfthyroid_gompertz_theta2 * ( 1 - exp ( - Wfthyroid_gompertz_theta2 * tw ) ) ) ;
#'  }
#'
#' \deqn{
#'  Wfliver = 0.001 * Wfliver_gompertz_theta0 * exp ( Wfliver_gompertz_theta1 / Wfliver_gompertz_theta2 * ( 1 - exp ( - Wfliver_gompertz_theta2 * tw ) ) ) ;
#'  }
#'
#' \deqn{
#'  Wfbrain = 0.001 * Wfbrain_gompertz_theta0 * exp ( Wfbrain_gompertz_theta1 / Wfbrain_gompertz_theta2 * ( 1 - exp ( - Wfbrain_gompertz_theta2 * tw ) ) ) ;
#'  }
#'
#' \deqn{
#'  Wfgut = 0.001 * Wfgut_gompertz_theta0 * exp ( Wfgut_gompertz_theta1 / Wfgut_gompertz_theta2 * ( 1 - exp ( - Wfgut_gompertz_theta2 * tw ) ) ) ;
#'  }
#'
#' \deqn{
#'  Wflung = 0.001 * Wflung_gompertz_theta0 * exp ( Wflung_gompertz_theta1 / Wflung_gompertz_theta2 * ( 1 - exp ( - Wflung_gompertz_theta2 * tw ) ) ) ;
#'  }
#'
#' \deqn{
#'  hematocrit = ( hematocrit_quadratic_theta0 + hematocrit_quadratic_theta1 * tw + hematocrit_quadratic_theta2 * pow ( tw , 2 ) ) / 100 ;
#'  }
#'
#' \deqn{
#'  Rblood2plasma = 1 - hematocrit + hematocrit * Krbc2pu * Fraction_unbound_plasma ;
#'  }
#'
#' \deqn{
#'  fhematocrit = ( fhematocrit_cubic_theta1 * tw + fhematocrit_cubic_theta2 * pow ( tw , 2 ) + fhematocrit_cubic_theta3 * pow ( tw , 3 ) ) / 100 ;
#'  }
#'
#' \deqn{
#'  Rfblood2plasma = 1 - fhematocrit + fhematocrit * Kfrbc2pu * Fraction_unbound_plasma_fetus ;
#'  }
#'
#' \deqn{
#'  fBW = 0.001 * fBW_gompertz_theta0 * exp ( fBW_gompertz_theta1 / fBW_gompertz_theta2 * ( 1 - exp ( - fBW_gompertz_theta2 * tw ) ) ) ;
#'  }
#'
#' \deqn{
#'  Vplacenta = 0.001 * ( Vplacenta_cubic_theta1 * tw + Vplacenta_cubic_theta2 * pow ( tw , 2 ) + Vplacenta_cubic_theta3 * pow ( tw , 3 ) ) ;
#'  }
#'
#' \deqn{
#'  Vamnf = 0.001 * Vamnf_logistic_theta0 / ( 1 + exp ( - Vamnf_logistic_theta1 * ( tw - Vamnf_logistic_theta2 ) ) ) ;
#'  }
#'
#' \deqn{
#'  Vplasma = Vplasma_mod_logistic_theta0 / ( 1 + exp ( - Vplasma_mod_logistic_theta1 * ( tw - Vplasma_mod_logistic_theta2 ) ) ) + Vplasma_mod_logistic_theta3 ;
#'  }
#'
#' \deqn{
#'  Vrbcs = hematocrit / ( 1 - hematocrit ) * Vplasma ;
#'  }
#'
#' \deqn{
#'  Vven = venous_blood_fraction * ( Vrbcs + Vplasma ) ;
#'  }
#'
#' \deqn{
#'  Vart = arterial_blood_fraction * ( Vrbcs + Vplasma ) ;
#'  }
#'
#' \deqn{
#'  Vadipose = 1 / adipose_density * Wadipose ;
#'  }
#'
#' \deqn{
#'  Vffmx = 1 / ffmx_density * ( BW - Wadipose - ( fBW + placenta_density * Vplacenta + amnf_density * Vamnf ) ) ;
#'  }
#'
#' \deqn{
#'  Vallx = Vart + Vven + Vthyroid + Vkidney + Vgut + Vliver + Vlung ;
#'  }
#'
#' \deqn{
#'  Vrest = Vffmx - Vallx ;
#'  }
#'
#' \deqn{
#'  Vfart = 0.001 * arterial_blood_fraction * fblood_weight_ratio * fBW ;
#'  }
#'
#' \deqn{
#'  Vfven = 0.001 * venous_blood_fraction * fblood_weight_ratio * fBW ;
#'  }
#'
#' \deqn{
#'  Vfkidney = 1 / kidney_density * Wfkidney ;
#'  }
#'
#' \deqn{
#'  Vfthyroid = 1 / thyroid_density * Wfthyroid ;
#'  }
#'
#' \deqn{
#'  Vfliver = 1 / liver_density * Wfliver ;
#'  }
#'
#' \deqn{
#'  Vfbrain = 1 / brain_density * Wfbrain ;
#'  }
#'
#' \deqn{
#'  Vfgut = 1 / gut_density * Wfgut ;
#'  }
#'
#' \deqn{
#'  Vflung = 1 / lung_density * Wflung ;
#'  }
#'
#' \deqn{
#'  Vfrest = fBW - ( Vfart + Vfven + Vfbrain + Vfkidney + Vfthyroid + Vfliver + Vfgut + Vflung ) ;
#'  }
#'
#' \deqn{
#'  Qcardiac = 24 * ( Qcardiac_cubic_theta0 + Qcardiac_cubic_theta1 * tw + Qcardiac_cubic_theta2 * pow ( tw , 2 ) + Qcardiac_cubic_theta3 * pow ( tw , 3 ) ) ;
#'  }
#'
#' \deqn{
#'  Qgut = 0.01 * ( Qgut_percent_initial + ( Qgut_percent_terminal - Qgut_percent_initial ) / term * tw ) * Qcardiac ;
#'  }
#'
#' \deqn{
#'  Qkidney = 24 * ( Qkidney_cubic_theta0 + Qkidney_cubic_theta1 * tw + Qkidney_cubic_theta2 * pow ( tw , 2 ) + Qkidney_cubic_theta3 * pow ( tw , 3 ) ) ;
#'  }
#'
#' \deqn{
#'  Qliver = 0.01 * ( Qliver_percent_initial + ( Qliver_percent_terminal - Qliver_percent_initial ) / term * tw ) * Qcardiac ;
#'  }
#'
#' \deqn{
#'  Qthyroid = 0.01 * ( Qthyroid_percent_initial + ( Qthyroid_percent_terminal - Qthyroid_percent_terminal ) / term * tw ) * Qcardiac ;
#'  }
#'
#' \deqn{
#'  Qplacenta = 24 * Qplacenta_linear_theta1 * 1000 * Vplacenta ;
#'  }
#'
#' \deqn{
#'  Qadipose = 0.01 * ( Qadipose_percent_initial + ( Qadipose_percent_terminal - Qadipose_percent_initial ) / term * tw ) * Qcardiac ;
#'  }
#'
#' \deqn{
#'  Qrest = Qcardiac - ( Qgut + Qkidney + Qliver + Qthyroid + Qplacenta + Qadipose ) ;
#'  }
#'
#' \deqn{
#'  Qgfr = 60 * 24 * 0.001 * ( Qgfr_quadratic_theta0 + Qgfr_quadratic_theta1 * tw + Qgfr_quadratic_theta2 * pow ( tw , 2 ) ) ;
#'  }
#'
#' \deqn{
#'  Qfrvtl = 60 * 24 * 0.001 * Qfrvtl_logistic_theta0 / ( 1 + exp ( - Qfrvtl_logistic_theta1 * ( tw - Qfrvtl_logistic_theta2 ) ) ) ;
#'  }
#'
#' \deqn{
#'  Qflvtl = 60 * 24 * 0.001 * Qflvtl_logistic_theta0 / ( 1 + exp ( - Qflvtl_logistic_theta1 * ( tw - Qflvtl_logistic_theta2 ) ) ) ;
#'  }
#'
#' \deqn{
#'  Qfda = 60 * 24 * 0.001 * Qfda_logistic_theta0 / ( 1 + exp ( - Qfda_logistic_theta1 * ( tw - Qfda_logistic_theta2 ) ) ) ;
#'  }
#'
#' \deqn{
#'  Qfartb = Qflvtl + Qfda ;
#'  }
#'
#' \deqn{
#'  Qfcardiac = Qfartb ;
#'  }
#'
#' \deqn{
#'  Qflung = Qfrvtl - Qfda ;
#'  }
#'
#' \deqn{
#'  Qfplacenta = 60 * 24 * 0.001 * Qfplacenta_logistic_theta0 / ( 1 + exp ( - Qfplacenta_logistic_theta1 * ( tw - Qfplacenta_logistic_theta2 ) ) ) ;
#'  }
#'
#' \deqn{
#'  Qfdv = 60 * 24 * 0.001 * Qfdv_gompertz_theta0 * exp ( Qfdv_gompertz_theta1 / Qfdv_gompertz_theta2 * ( 1 - exp ( - Qfdv_gompertz_theta2 * tw ) ) ) ;
#'  }
#'
#' \deqn{
#'  Qfgut = Qfgut_percent / Qfnonplacental_percent * ( 1 - Qfplacenta / Qfartb ) * Qfartb ;
#'  }
#'
#' \deqn{
#'  Qfkidney = Qfkidney_percent / Qfnonplacental_percent * ( 1 - Qfplacenta / Qfartb ) * Qfartb ;
#'  }
#'
#' \deqn{
#'  Qfbrain = Qfbrain_percent / Qfnonplacental_percent * ( 1 - Qfplacenta / Qfartb ) * Qfartb ;
#'  }
#'
#' \deqn{
#'  Qfliver = Qfliver_percent / ( 100 - ( Qbrain_percent + Qkidney_percent + Qgut_percent ) ) * ( 1 - ( Qfbrain_percent +
#' Qfkidney_percent + Qfgut_percent ) / Qfnonplacental_percent ) * ( 1 - Qfplacenta / Qfartb ) * Qfartb ;
#'  }
#'
#' \deqn{
#'  Qfthyroid = Qfthyroid_percent / ( 100 - ( Qbrain_percent + Qkidney_percent + Qgut_percent ) ) * ( 1 - ( Qfbrain_percent +
#' Qfkidney_percent + Qfgut_percent ) / Qfnonplacental_percent ) * ( 1 - Qfplacenta / Qfartb ) * Qfartb ;
#'  }
#'
#' \deqn{
#'  Qfrest = Qfcardiac - ( Qfplacenta + Qfgut + Qfliver + Qfthyroid + Qfkidney + Qfbrain ) ;
#'  }
#'
#' \deqn{
#'  Qfbypass = Qfcardiac - Qflung ;
#'  }
#'
#' @param week Gestational week
#' @param ... Additional arguments to parameterize_fetal_pbtk
#'
#' @return list containing:
#' \item{BW}{Maternal body weight, kg}
#' \item{Wadipose}{Maternal adipose fraction of total weight}
#' \item{Wfkidney}{Fetal kidney fraction of total weight}
#' \item{Wfthyroid}{Fetal thyroid fraction of total weight}
#' \item{Wfliver }{Fetal liver fraction of total weight}
#' \item{Wfbrain}{Fetal brain fraction of total weight}
#' \item{Wfgut}{Fetal gut fraction of total weight}
#' \item{Wflung}{Fetal lung fraction of total weight}
#' \item{hematocrit}{Maternal hematocrit fraction of blood}
#' \item{Rblood2plasma}{Maternal Rblood2plasma}
#' \item{fhematocrit}{Fetal hematocrit fraction of blood}
#' \item{Rfblood2plasma}{Fetal Rfblood2plasma}
#' \item{fBW}{Fetal body weight, kg}
#' \item{Vplacenta}{Volume of Vplacenta, L}
#' \item{Vamnf}{Volume of amniotic fluid, L}
#' \item{Vplasma}{Maternal volume of plasma, L}
#' \item{Vrbcs}{Maternal volume of red blood cells, L}
#' \item{Vven}{Maternal volume of venous blood, L}
#' \item{Vart}{Maternal volume of arterial blood, L}
#' \item{Vadipose}{Maternal volume of adipose, L}
#' \item{Vffmx}{Fetal volume ofVffmx, L}
#' \item{Vallx}{Vallx, L}
#' \item{Vrest}{Maternal volume of rest of body, L}
#' \item{Vfart}{Fetal volume of arterial blood, L}
#' \item{Vfven}{Fetal volume of venous blood, L}
#' \item{Vfkidney}{Fetal volume of kidney, L}
#' \item{Vfthyroid}{Fetal volume of thyroid, L}
#' \item{Vfliver}{Fetal volume of liver, L}
#' \item{Vfbrain}{Fetal volume of brain, L}
#' \item{Vfgut}{Fetal volume of gut, L}
#' \item{Vflung}{Fetal volume of lung, L}
#' \item{Vfrest}{Fetal volume of rest of body, L}
#' \item{Qcardiac}{Maternal cardiac output blood flow, L/day}
#' \item{Qgut}{Maternal blood flow to gut, L/day}
#' \item{Qkidney}{Maternal blood flow to kidney, L/day}
#' \item{Qliver}{Maternal blood flow to liver, L/day}
#' \item{Qthyroid}{Maternal blood flow to thyroid, L/day}
#' \item{Qplacenta}{Maternal blood flow to placenta, L/day}
#' \item{Qadipose}{Maternal blood flow to adipose, L/day}
#' \item{Qrest}{Maternal blood flow to rest, L/day}
#' \item{Qgfr}{Maternal glomerular filtration rate in kidney, L/day}
#' \item{Qfrvtl}{Fetal blood flow to right ventricle, L/day}
#' \item{Qflvtl}{Fetal blood flow to left ventircle, L/day}
#' \item{Qfda}{Fetal blood flow to Qfda, L/day}
#' \item{Qfartb}{Fetal blood flow to Qfartb, L/day}
#' \item{Qfcardiac}{Fetal cardiac output blood flow, L/day}
#' \item{Qflung}{Fetal blood flow to lung, L/day}
#' \item{Qfplacenta}{Fetal blood flow to placenta, L/day}
#' \item{Qfdv}{Fetal blood flow to Qfdv, L/day}
#' \item{Qfgut}{Fetal blood flow to gut, L/day}
#' \item{Qfkidney}{Fetal blood flow to kidney, L/day}
#' \item{Qfbrain}{Fetal blood flow to brain, L/day}
#' \item{Qfliver}{Fetal blood flow to liver, L/day}
#' \item{Qfthyroid}{Fetal blood flow to thyroid, L/day}
#' \item{Qfrest}{Fetal blood flow to rest, L/day}
#' \item{Qfbypass}{Fetal blood flow to Qfbypass, L/day}
#'
#' @references 
#' \insertRef{kapraun2019empirical}{httk} 
#'
#' @keywords Parameter
#' 
#' @author John Wambaugh
#' 
#' @export calc_fetal_phys
calc_fetal_phys <- function(
  week = 12,
  ...)
{
  if (week < 0 | week > 41) stop("Functions only valid for gestational ages between 0 and 41 weeks")

  params <- do.call("parameterize_fetal_pbtk", c(list(
    chem.cas="80-05-7"),
    ...))

# Function that transforms the parameters to those needed by the solver:
    compiled_parameters_init <- model.list[["fetal_pbtk"]]$compiled.parameters.init
# name(s) of the compiled model parameters that control the solver:
    compiled_param_names <- model.list[["fetal_pbtk"]]$compiled.param.names
# name(s)s of the R parameters needed to initialize the compiled model params:
    Rtosolvermap <- model.list[["fetal_pbtk"]]$Rtosolvermap


  # Map the R parameters onto the names for the C code:
  for (this.param in names(Rtosolvermap)[!(names(Rtosolvermap) 
    %in% names(params))])
  {
    if (Rtosolvermap[[this.param]] %in% names(params))
      params[[this.param]] <- params[[Rtosolvermap[[this.param]]]]
    else stop(paste("Failed to find R parameter",Rtosolvermap[[this.param]],
      "to initialize parameter",this.param,"in the C code."))
  }
  
  # These parameters are presumably initialized by the compiled code, such as
  # parameters scaled from other parameters:
  for (this.param in compiled_param_names)
    if (!(this.param %in% names(params)))
      params[[this.param]] <- 0
            
# Here we remove model parameters that are not needed by the C solver (via
# only passing those parameters in compiled_param_names) and add in any
# additional parameters calculated by the C code (such as body weight scaling):
  params <- .C(
    getDLLRegisteredRoutines("httk")[[".C"]][[compiled_parameters_init]]$address,
    as.double(params[compiled_param_names]),
    out=double(length(params[compiled_param_names])),
    as.integer(length(params[compiled_param_names])))$out
  names(params) <- compiled_param_names
  params <- as.list(params)
  tw <- week
  
  pow <- function(x,y) return(x^y)
  
  BW <- with(params, pre_pregnant_BW + 
    BW_cubic_theta1 * tw + 
    BW_cubic_theta2 * tw^2 + 
    BW_cubic_theta3 * tw^3)

  Wadipose <- with(params, Wadipose_linear_theta0 + 
    Wadipose_linear_theta1 * tw )

  Wfkidney <- with(params, 0.001 * Wfkidney_gompertz_theta0 * 
    exp ( Wfkidney_gompertz_theta1 / Wfkidney_gompertz_theta2 * 
    ( 1 - exp ( - Wfkidney_gompertz_theta2 * tw ) ) ) )

  Wfthyroid <- with(params, 0.001 * Wfthyroid_gompertz_theta0 * 
    exp ( Wfthyroid_gompertz_theta1 / Wfthyroid_gompertz_theta2 * 
    ( 1 - exp ( - Wfthyroid_gompertz_theta2 * tw ) ) ) )

  Wfliver <- with(params, 0.001 * Wfliver_gompertz_theta0 * 
    exp ( Wfliver_gompertz_theta1 / Wfliver_gompertz_theta2 * 
    ( 1 - exp ( - Wfliver_gompertz_theta2 * tw ) ) ) )

  Wfbrain <- with(params, 0.001 * Wfbrain_gompertz_theta0 * 
    exp ( Wfbrain_gompertz_theta1 / Wfbrain_gompertz_theta2 * 
    ( 1 - exp ( - Wfbrain_gompertz_theta2 * tw ) ) ) )

  Wfgut <- with(params, 0.001 * Wfgut_gompertz_theta0 * 
    exp ( Wfgut_gompertz_theta1 / Wfgut_gompertz_theta2 * 
    ( 1 - exp ( - Wfgut_gompertz_theta2 * tw ) ) ) )

  Wflung <- with(params, 0.001 * Wflung_gompertz_theta0 * 
    exp ( Wflung_gompertz_theta1 / Wflung_gompertz_theta2 * 
    ( 1 - exp ( - Wflung_gompertz_theta2 * tw ) ) ) )

  hematocrit <- with(params, ( hematocrit_quadratic_theta0 + 
    hematocrit_quadratic_theta1 * tw + 
    hematocrit_quadratic_theta2 * pow ( tw , 2 ) ) / 100 )

  Rblood2plasma <- with(params, 1 - hematocrit + 
    hematocrit * Krbc2pu * Fraction_unbound_plasma )

  fhematocrit <- with(params, ( fhematocrit_cubic_theta1 * tw + 
    fhematocrit_cubic_theta2 * pow ( tw , 2 ) + 
    fhematocrit_cubic_theta3 * pow ( tw , 3 ) ) / 100 )

  Rfblood2plasma <- with(params, 1 - fhematocrit + 
    fhematocrit * Kfrbc2pu * Fraction_unbound_plasma_fetus )

  fBW <- with(params, 0.001 * fBW_gompertz_theta0 * 
    exp ( fBW_gompertz_theta1 / fBW_gompertz_theta2 * 
    ( 1 - exp ( - fBW_gompertz_theta2 * tw ) ) ) )

  Vplacenta <- with(params, 0.001 * ( Vplacenta_cubic_theta1 * tw + 
    Vplacenta_cubic_theta2 * pow ( tw , 2 ) + 
    Vplacenta_cubic_theta3 * pow ( tw , 3 ) ) )

  Vamnf <- with(params, 0.001 * Vamnf_logistic_theta0 / 
    ( 1 + exp ( - Vamnf_logistic_theta1 * ( tw - Vamnf_logistic_theta2 ) ) ) )

  Vplasma <- with(params, Vplasma_mod_logistic_theta0 / 
    ( 1 + exp ( - Vplasma_mod_logistic_theta1 * 
    ( tw - Vplasma_mod_logistic_theta2 ) ) ) + Vplasma_mod_logistic_theta3 )

  Vrbcs <- with(params, hematocrit / ( 1 - hematocrit ) * Vplasma )

  Vven <- with(params, venous_blood_fraction * ( Vrbcs + Vplasma ) )

  Vart <- with(params, arterial_blood_fraction * ( Vrbcs + Vplasma ) )

  Vadipose <- with(params, 1 / adipose_density * Wadipose )

  Vffmx <- with(params, 1 / ffmx_density * ( BW - Wadipose - 
    ( fBW + placenta_density * Vplacenta + amnf_density * Vamnf ) ) )

  Vallx <- with(params, Vart + Vven + Vthyroid + Vkidney + Vgut + Vliver + 
    Vlung )

  Vrest <- with(params, Vffmx - Vallx )

  Vfart <- with(params, 0.001 * arterial_blood_fraction * 
    fblood_weight_ratio * fBW )

  Vfven <- with(params, 0.001 * venous_blood_fraction * 
    fblood_weight_ratio * fBW )

  Vfkidney <- with(params, 1 / kidney_density * Wfkidney )

  Vfthyroid <- with(params, 1 / thyroid_density * Wfthyroid )

  Vfliver <- with(params, 1 / liver_density * Wfliver )

  Vfbrain <- with(params, 1 / brain_density * Wfbrain )

  Vfgut <- with(params, 1 / gut_density * Wfgut )

  Vflung <- with(params, 1 / lung_density * Wflung )

  Vfrest <- with(params, fBW - ( Vfart + Vfven + Vfbrain + 
    Vfkidney + Vfthyroid + Vfliver + Vfgut + Vflung ) )

  Qcardiac <- with(params, 24 * ( Qcardiac_cubic_theta0 + 
    Qcardiac_cubic_theta1 * tw + Qcardiac_cubic_theta2 * pow ( tw , 2 ) +
    Qcardiac_cubic_theta3 * pow ( tw , 3 ) ) )

  Qgut <- with(params, 0.01 * ( Qgut_percent_initial + 
    ( Qgut_percent_terminal - Qgut_percent_initial ) / term * tw ) * Qcardiac )

  Qkidney <- with(params, 24 * ( Qkidney_cubic_theta0 + 
    Qkidney_cubic_theta1 * tw + Qkidney_cubic_theta2 * pow ( tw , 2 ) + 
    Qkidney_cubic_theta3 * pow ( tw , 3 ) ) )

  Qliver <- with(params, 0.01 * ( Qliver_percent_initial + 
    ( Qliver_percent_terminal - Qliver_percent_initial ) / 
    term * tw ) * Qcardiac )

  Qthyroid <- with(params, 0.01 * ( Qthyroid_percent_initial + 
    ( Qthyroid_percent_terminal - Qthyroid_percent_terminal ) / term * tw ) * 
    Qcardiac )

  Qplacenta <- with(params, 24 * Qplacenta_linear_theta1 * 1000 * Vplacenta )

  Qadipose <- with(params, 0.01 * ( Qadipose_percent_initial + 
    ( Qadipose_percent_terminal - 
    Qadipose_percent_initial ) / term * tw ) * Qcardiac )

  Qrest <- with(params, Qcardiac - ( Qgut + Qkidney + Qliver + Qthyroid + 
    Qplacenta + Qadipose ) )

  Qgfr <- with(params, 60 * 24 * 0.001 * ( Qgfr_quadratic_theta0 + 
    Qgfr_quadratic_theta1 * tw + Qgfr_quadratic_theta2 * pow ( tw , 2 ) ) )

  Qfrvtl <- with(params, 60 * 24 * 0.001 * Qfrvtl_logistic_theta0 / 
    ( 1 + exp ( - Qfrvtl_logistic_theta1 * ( tw - Qfrvtl_logistic_theta2 ) ) ) )

  Qflvtl <- with(params, 60 * 24 * 0.001 * Qflvtl_logistic_theta0 / 
    ( 1 + exp ( - Qflvtl_logistic_theta1 * ( tw - Qflvtl_logistic_theta2 ) ) ) )

  Qfda <- with(params, 60 * 24 * 0.001 * Qfda_logistic_theta0 / 
    ( 1 + exp ( - Qfda_logistic_theta1 * ( tw - Qfda_logistic_theta2 ) ) ) )

  Qfartb <- with(params, Qflvtl + Qfda )

  Qfcardiac <- with(params, Qfartb )

  Qflung <- with(params, Qfrvtl - Qfda )

  Qfplacenta <- with(params, 60 * 24 * 0.001 * Qfplacenta_logistic_theta0 / 
    ( 1 + exp ( - Qfplacenta_logistic_theta1 * 
    ( tw - Qfplacenta_logistic_theta2 ) ) ) )

  Qfdv <- with(params, 60 * 24 * 0.001 * Qfdv_gompertz_theta0 * 
    exp ( Qfdv_gompertz_theta1 / Qfdv_gompertz_theta2 * 
    ( 1 - exp ( - Qfdv_gompertz_theta2 * tw ) ) ) )

  Qfgut <- with(params, Qfgut_percent / Qfnonplacental_percent * 
    ( 1 - Qfplacenta / Qfartb ) * Qfartb )

  Qfkidney <- with(params, Qfkidney_percent / Qfnonplacental_percent * 
    ( 1 - Qfplacenta / Qfartb ) * Qfartb )

  Qfbrain <- with(params, Qfbrain_percent / Qfnonplacental_percent * 
    ( 1 - Qfplacenta / Qfartb ) * Qfartb )

  Qfliver <- with(params, Qfliver_percent / 
    ( 100 - ( Qbrain_percent + Qkidney_percent + Qgut_percent ) ) * 
    ( 1 - ( Qfbrain_percent + Qfkidney_percent + Qfgut_percent ) / 
    Qfnonplacental_percent ) * ( 1 - Qfplacenta / Qfartb ) * Qfartb )

  Qfthyroid <- with(params, Qfthyroid_percent / 
    ( 100 - ( Qbrain_percent + Qkidney_percent + Qgut_percent ) ) * 
    ( 1 - ( Qfbrain_percent + Qfkidney_percent + Qfgut_percent ) / 
    Qfnonplacental_percent ) * ( 1 - Qfplacenta / Qfartb ) * Qfartb )

  Qfrest <- with(params, Qfcardiac - 
    ( Qfplacenta + Qfgut + Qfliver + Qfthyroid + Qfkidney + Qfbrain ) )

  Qfbypass <- with(params, Qfcardiac - Qflung )

  out <- list(
    BW = BW,
    Wadipose = Wadipose,
    Wfkidney = Wfkidney,
    Wfthyroid = Wfthyroid,
    Wfliver = Wfliver,
    Wfbrain = Wfbrain,
    Wfgut = Wfgut,
    Wflung = Wflung,
    hematocrit = hematocrit,
    Rblood2plasma = Rblood2plasma,
    fhematocrit = fhematocrit,
    Rfblood2plasma = Rfblood2plasma,
    fBW = fBW,
    Vplacenta = Vplacenta,
    Vamnf = Vamnf,
    Vplasma = Vplasma,
    Vrbcs = Vrbcs,
    Vven = Vven,
    Vart = Vart,
    Vadipose = Vadipose,
    Vffmx = Vffmx,
    Vallx = Vallx,
    Vrest = Vrest,
    Vfart = Vfart,
    Vfven = Vfven,
    Vfkidney = Vfkidney,
    Vfthyroid = Vfthyroid,
    Vfliver = Vfliver,
    Vfbrain = Vfbrain,
    Vfgut = Vfgut,
    Vflung = Vflung,
    Vfrest = Vfrest,
    Qcardiac = Qcardiac,
    Qgut = Qgut,
    Qkidney = Qkidney,
    Qliver = Qliver,
    Qthyroid = Qthyroid,
    Qplacenta = Qplacenta,
    Qadipose = Qadipose,
    Qrest = Qrest,
    Qgfr = Qgfr,
    Qfrvtl = Qfrvtl,
    Qflvtl = Qflvtl,
    Qfda = Qfda,
    Qfartb = Qfartb,
    Qfcardiac = Qfcardiac,
    Qflung = Qflung,
    Qfplacenta = Qfplacenta,
    Qfdv = Qfdv,
    Qfgut = Qfgut,
    Qfkidney = Qfkidney,
    Qfbrain = Qfbrain,
    Qfliver = Qfliver,
    Qfthyroid = Qfthyroid,
    Qfrest = Qfrest,
    Qfbypass = Qfbypass
  )

  return(lapply(out,set_httk_precision))
}