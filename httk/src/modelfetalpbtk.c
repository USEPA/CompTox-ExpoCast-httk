/* model_fetal_pbtk-raw.c for R deSolve package
   ___________________________________________________

   Model File:  model_fetal_pbtk.model

   Date:  Wed Mar 23 13:29:41 2022

   Created by:  "mod v6.1.0"
    -- a model preprocessor by Don Maszle
   ___________________________________________________

   Copyright (c) 1993-2019 Free Software Foundation, Inc.

   Model calculations for compartmental model:

   24 States:
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
     Afbrain = 0.0,

   25 Outputs:
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
    "Rblood2plasma",
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
    "Cfplasma",
    "Rfblood2plasma",

   0 Inputs:

   134 Parameters:
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
     Kplacenta2pu = 0,
     Kfplacenta2pu = 0,
     Kfkidney2pu = 0,
     Kfrest2pu = 0,
     Kfthyroid2pu = 0,
     Kfliver2pu = 0,
     Kflung2pu = 0,
     Kfgut2pu = 0,
     Kfrbc2pu = 0,
     Kfbrain2pu = 0,
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
     Fraction_unbound_plasma_fetus = 0,
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
     Wfkidney_gompertz_theta0 = 0,
     Wfkidney_gompertz_theta1 = 0,
     Wfkidney_gompertz_theta2 = 0,
     Wfthyroid_gompertz_theta0 = 0,
     Wfthyroid_gompertz_theta1 = 0,
     Wfthyroid_gompertz_theta2 = 0,
     Wfliver_gompertz_theta0 = 0,
     Wfliver_gompertz_theta1 = 0,
     Wfliver_gompertz_theta2 = 0,
     Wfbrain_gompertz_theta0 = 0,
     Wfbrain_gompertz_theta1 = 0,
     Wfbrain_gompertz_theta2 = 0,
     Wfgut_gompertz_theta0 = 0,
     Wfgut_gompertz_theta1 = 0,
     Wfgut_gompertz_theta2 = 0,
     Wflung_gompertz_theta0 = 0,
     Wflung_gompertz_theta1 = 0,
     Wflung_gompertz_theta2 = 0,
     hematocrit_quadratic_theta0 = 0,
     hematocrit_quadratic_theta1 = 0,
     hematocrit_quadratic_theta2 = 0,
     fhematocrit_cubic_theta1 = 0,
     fhematocrit_cubic_theta2 = 0,
     fhematocrit_cubic_theta3 = 0,
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
     fblood_weight_ratio = 0,
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
     Qfrvtl_logistic_theta0 = 0,
     Qfrvtl_logistic_theta1 = 0,
     Qfrvtl_logistic_theta2 = 0,
     Qflvtl_logistic_theta0 = 0,
     Qflvtl_logistic_theta1 = 0,
     Qflvtl_logistic_theta2 = 0,
     Qfda_logistic_theta0 = 0,
     Qfda_logistic_theta1 = 0,
     Qfda_logistic_theta2 = 0,
     Qfplacenta_logistic_theta0 = 0,
     Qfplacenta_logistic_theta1 = 0,
     Qfplacenta_logistic_theta2 = 0,
     Qfdv_gompertz_theta0 = 0,
     Qfdv_gompertz_theta1 = 0,
     Qfdv_gompertz_theta2 = 0,
     Qfnonplacental_percent = 0,
     Qfgut_percent = 0,
     Qfkidney_percent = 0,
     Qfbrain_percent = 0,
     Qbrain_percent = 0,
     Qkidney_percent = 0,
     Qgut_percent = 0,
     Qfliver_percent = 0,
     Qfthyroid_percent = 0,
*/

#include <R.h>
#include <Rinternals.h>
#include <Rdefines.h>
#include <R_ext/Rdynload.h>

/* Model variables: States */
#define ID_Agutlumen 0x00000
#define ID_Agut 0x00001
#define ID_Aliver 0x00002
#define ID_Aven 0x00003
#define ID_Alung 0x00004
#define ID_Aart 0x00005
#define ID_Aadipose 0x00006
#define ID_Arest 0x00007
#define ID_Akidney 0x00008
#define ID_Atubules 0x00009
#define ID_Ametabolized 0x0000a
#define ID_AUC 0x0000b
#define ID_fAUC 0x0000c
#define ID_Athyroid 0x0000d
#define ID_Aplacenta 0x0000e
#define ID_Afgut 0x0000f
#define ID_Aflung 0x00010
#define ID_Afliver 0x00011
#define ID_Afven 0x00012
#define ID_Afart 0x00013
#define ID_Afrest 0x00014
#define ID_Afthyroid 0x00015
#define ID_Afkidney 0x00016
#define ID_Afbrain 0x00017

/* Model variables: Outputs */
#define ID_Cgut 0x00000
#define ID_Cliver 0x00001
#define ID_Cven 0x00002
#define ID_Clung 0x00003
#define ID_Cart 0x00004
#define ID_Cadipose 0x00005
#define ID_Crest 0x00006
#define ID_Ckidney 0x00007
#define ID_Cplasma 0x00008
#define ID_Aplasma 0x00009
#define ID_Cthyroid 0x0000a
#define ID_Rblood2plasma 0x0000b
#define ID_Cplacenta 0x0000c
#define ID_Cfliver 0x0000d
#define ID_Cfven 0x0000e
#define ID_Cfart 0x0000f
#define ID_Cfgut 0x00010
#define ID_Cflung 0x00011
#define ID_Cfrest 0x00012
#define ID_Cfthyroid 0x00013
#define ID_Cfkidney 0x00014
#define ID_Cfbrain 0x00015
#define ID_Afplasma 0x00016
#define ID_Cfplasma 0x00017
#define ID_Rfblood2plasma 0x00018

/* Parameters */
static double parms[134];

#define pre_pregnant_BW parms[0]
#define Clmetabolismc parms[1]
#define Clmetabolism parms[2]
#define kgutabs parms[3]
#define Kkidney2pu parms[4]
#define Kliver2pu parms[5]
#define Kadipose2pu parms[6]
#define Krest2pu parms[7]
#define Klung2pu parms[8]
#define Kgut2pu parms[9]
#define Krbc2pu parms[10]
#define Kthyroid2pu parms[11]
#define Kplacenta2pu parms[12]
#define Kfplacenta2pu parms[13]
#define Kfkidney2pu parms[14]
#define Kfrest2pu parms[15]
#define Kfthyroid2pu parms[16]
#define Kfliver2pu parms[17]
#define Kflung2pu parms[18]
#define Kfgut2pu parms[19]
#define Kfrbc2pu parms[20]
#define Kfbrain2pu parms[21]
#define Vgutc parms[22]
#define Vgut parms[23]
#define Vkidneyc parms[24]
#define Vkidney parms[25]
#define Vliverc parms[26]
#define Vliver parms[27]
#define Vlungc parms[28]
#define Vlung parms[29]
#define Vthyroidc parms[30]
#define Vthyroid parms[31]
#define Fraction_unbound_plasma parms[32]
#define Fraction_unbound_plasma_fetus parms[33]
#define gut_density parms[34]
#define kidney_density parms[35]
#define liver_density parms[36]
#define lung_density parms[37]
#define thyroid_density parms[38]
#define adipose_density parms[39]
#define ffmx_density parms[40]
#define placenta_density parms[41]
#define amnf_density parms[42]
#define brain_density parms[43]
#define BW_cubic_theta1 parms[44]
#define BW_cubic_theta2 parms[45]
#define BW_cubic_theta3 parms[46]
#define Wadipose_linear_theta0 parms[47]
#define Wadipose_linear_theta1 parms[48]
#define Wfkidney_gompertz_theta0 parms[49]
#define Wfkidney_gompertz_theta1 parms[50]
#define Wfkidney_gompertz_theta2 parms[51]
#define Wfthyroid_gompertz_theta0 parms[52]
#define Wfthyroid_gompertz_theta1 parms[53]
#define Wfthyroid_gompertz_theta2 parms[54]
#define Wfliver_gompertz_theta0 parms[55]
#define Wfliver_gompertz_theta1 parms[56]
#define Wfliver_gompertz_theta2 parms[57]
#define Wfbrain_gompertz_theta0 parms[58]
#define Wfbrain_gompertz_theta1 parms[59]
#define Wfbrain_gompertz_theta2 parms[60]
#define Wfgut_gompertz_theta0 parms[61]
#define Wfgut_gompertz_theta1 parms[62]
#define Wfgut_gompertz_theta2 parms[63]
#define Wflung_gompertz_theta0 parms[64]
#define Wflung_gompertz_theta1 parms[65]
#define Wflung_gompertz_theta2 parms[66]
#define hematocrit_quadratic_theta0 parms[67]
#define hematocrit_quadratic_theta1 parms[68]
#define hematocrit_quadratic_theta2 parms[69]
#define fhematocrit_cubic_theta1 parms[70]
#define fhematocrit_cubic_theta2 parms[71]
#define fhematocrit_cubic_theta3 parms[72]
#define fBW_gompertz_theta0 parms[73]
#define fBW_gompertz_theta1 parms[74]
#define fBW_gompertz_theta2 parms[75]
#define Vplacenta_cubic_theta1 parms[76]
#define Vplacenta_cubic_theta2 parms[77]
#define Vplacenta_cubic_theta3 parms[78]
#define Vamnf_logistic_theta0 parms[79]
#define Vamnf_logistic_theta1 parms[80]
#define Vamnf_logistic_theta2 parms[81]
#define Vplasma_mod_logistic_theta0 parms[82]
#define Vplasma_mod_logistic_theta1 parms[83]
#define Vplasma_mod_logistic_theta2 parms[84]
#define Vplasma_mod_logistic_theta3 parms[85]
#define venous_blood_fraction parms[86]
#define arterial_blood_fraction parms[87]
#define fblood_weight_ratio parms[88]
#define Qcardiac_cubic_theta0 parms[89]
#define Qcardiac_cubic_theta1 parms[90]
#define Qcardiac_cubic_theta2 parms[91]
#define Qcardiac_cubic_theta3 parms[92]
#define term parms[93]
#define Qgut_percent_initial parms[94]
#define Qgut_percent_terminal parms[95]
#define Qkidney_cubic_theta0 parms[96]
#define Qkidney_cubic_theta1 parms[97]
#define Qkidney_cubic_theta2 parms[98]
#define Qkidney_cubic_theta3 parms[99]
#define Qliver_percent_initial parms[100]
#define Qliver_percent_terminal parms[101]
#define Qthyroid_percent_initial parms[102]
#define Qthyroid_percent_terminal parms[103]
#define Qplacenta_linear_theta1 parms[104]
#define Qadipose_percent_initial parms[105]
#define Qadipose_percent_terminal parms[106]
#define Qgfr_quadratic_theta0 parms[107]
#define Qgfr_quadratic_theta1 parms[108]
#define Qgfr_quadratic_theta2 parms[109]
#define Qfrvtl_logistic_theta0 parms[110]
#define Qfrvtl_logistic_theta1 parms[111]
#define Qfrvtl_logistic_theta2 parms[112]
#define Qflvtl_logistic_theta0 parms[113]
#define Qflvtl_logistic_theta1 parms[114]
#define Qflvtl_logistic_theta2 parms[115]
#define Qfda_logistic_theta0 parms[116]
#define Qfda_logistic_theta1 parms[117]
#define Qfda_logistic_theta2 parms[118]
#define Qfplacenta_logistic_theta0 parms[119]
#define Qfplacenta_logistic_theta1 parms[120]
#define Qfplacenta_logistic_theta2 parms[121]
#define Qfdv_gompertz_theta0 parms[122]
#define Qfdv_gompertz_theta1 parms[123]
#define Qfdv_gompertz_theta2 parms[124]
#define Qfnonplacental_percent parms[125]
#define Qfgut_percent parms[126]
#define Qfkidney_percent parms[127]
#define Qfbrain_percent parms[128]
#define Qbrain_percent parms[129]
#define Qkidney_percent parms[130]
#define Qgut_percent parms[131]
#define Qfliver_percent parms[132]
#define Qfthyroid_percent parms[133]

/* Forcing (Input) functions */
//static double forc[0];


/* Function definitions for delay differential equations */
//
//int Nout=1;
//int nr[1]={0};
//double ytau[1] = {0.0};
//
//static double yini[24] = {0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0}; /*Array of initial state variables*/
//
//void lagvalue(double T, int *nr, int N, double *ytau) {
//  static void(*fun)(double, int*, int, double*) = NULL;
//  if (fun == NULL)
//    fun = (void(*)(double, int*, int, double*))R_GetCCallable("deSolve", "lagvalue");
//  return fun(T, nr, N, ytau);
//}
//
//double CalcDelay(int hvar, double dTime, double delay) {
//  double T = dTime-delay;
//  if (dTime > delay){
//    nr[0] = hvar;
//    lagvalue( T, nr, Nout, ytau );
//}
//  else{
//    ytau[0] = yini[hvar];
//}
//  return(ytau[0]);
//}

/*----- Initializers */
void initmodfetal_pbtk (void (* odeparms)(int *, double *))
{
  int N=134;
  odeparms(&N, parms);
}

/*void initforcfetal_pbtk (void (* odeforcs)(int *, double *))
{
  int N=0;
  odeforcs(&N, forc);
}
*/

/* Calling R code will ensure that input y has same
   dimension as yini */
//void initState (double *y)
//{
//  int i;
//
//  for (i = 0; i < sizeof(yini) / sizeof(yini[0]); i++)
//  {
//    yini[i] = y[i];
//  }
//}

void getParmsfetal_pbtk (double *inParms, double *out, int *nout) {
/*----- Model scaling */

  int i;

  for (i = 0; i < *nout; i++) {
    parms[i] = inParms[i];
  }


  kgutabs = kgutabs * 24 ;
  Clmetabolism = Clmetabolismc * 24 * pre_pregnant_BW ;
  Vgut = Vgutc * pre_pregnant_BW / gut_density ;
  Vkidney = Vkidneyc * pre_pregnant_BW / kidney_density ;
  Vliver = Vliverc * pre_pregnant_BW / liver_density ;
  Vlung = Vlungc * pre_pregnant_BW / lung_density ;
  Vthyroid = Vthyroidc * pre_pregnant_BW / thyroid_density ;

  for (i = 0; i < *nout; i++) {
    out[i] = parms[i];
  }
  }
/*----- Dynamics section */

void derivsfetal_pbtk (int *neq, double *pdTime, double *y, double *ydot, double *yout, int *ip)
{
  /* local */ double tw;
  /* local */ double BW;
  /* local */ double Wadipose;
  /* local */ double Wfkidney;
  /* local */ double Wfthyroid;
  /* local */ double Wfliver;
  /* local */ double Wfbrain;
  /* local */ double Wfgut;
  /* local */ double Wflung;
  /* local */ double hematocrit;
  /* local */ double fhematocrit;
  /* local */ double fBW;
  /* local */ double Vplacenta;
  /* local */ double Vamnf;
  /* local */ double Vplasma;
  /* local */ double Vrbcs;
  /* local */ double Vven;
  /* local */ double Vart;
  /* local */ double Vadipose;
  /* local */ double Vffmx;
  /* local */ double Vallx;
  /* local */ double Vrest;
  /* local */ double Vfart;
  /* local */ double Vfven;
  /* local */ double Vfkidney;
  /* local */ double Vfthyroid;
  /* local */ double Vfliver;
  /* local */ double Vfbrain;
  /* local */ double Vfgut;
  /* local */ double Vflung;
  /* local */ double Vfrest;
  /* local */ double Qcardiac;
  /* local */ double Qgut;
  /* local */ double Qkidney;
  /* local */ double Qliver;
  /* local */ double Qthyroid;
  /* local */ double Qplacenta;
  /* local */ double Qadipose;
  /* local */ double Qrest;
  /* local */ double Qgfr;
  /* local */ double Qfrvtl;
  /* local */ double Qflvtl;
  /* local */ double Qfda;
  /* local */ double Qfartb;
  /* local */ double Qfcardiac;
  /* local */ double Qflung;
  /* local */ double Qfplacenta;
  /* local */ double Qfdv;
  /* local */ double Qfgut;
  /* local */ double Qfkidney;
  /* local */ double Qfbrain;
  /* local */ double Qfliver;
  /* local */ double Qfthyroid;
  /* local */ double Qfrest;
  /* local */ double Qfbypass;

  tw = (*pdTime) / 7.0 ;

  BW = pre_pregnant_BW + BW_cubic_theta1 * tw + BW_cubic_theta2 * pow ( tw , 2 ) + BW_cubic_theta3 * pow ( tw , 3 ) ;

  Wadipose = Wadipose_linear_theta0 + Wadipose_linear_theta1 * tw ;

  Wfkidney = 0.001 * Wfkidney_gompertz_theta0 * exp ( Wfkidney_gompertz_theta1 / Wfkidney_gompertz_theta2 * ( 1 - exp ( - Wfkidney_gompertz_theta2 * tw ) ) ) ;

  Wfthyroid = 0.001 * Wfthyroid_gompertz_theta0 * exp ( Wfthyroid_gompertz_theta1 / Wfthyroid_gompertz_theta2 * ( 1 - exp ( - Wfthyroid_gompertz_theta2 * tw ) ) ) ;

  Wfliver = 0.001 * Wfliver_gompertz_theta0 * exp ( Wfliver_gompertz_theta1 / Wfliver_gompertz_theta2 * ( 1 - exp ( - Wfliver_gompertz_theta2 * tw ) ) ) ;

  Wfbrain = 0.001 * Wfbrain_gompertz_theta0 * exp ( Wfbrain_gompertz_theta1 / Wfbrain_gompertz_theta2 * ( 1 - exp ( - Wfbrain_gompertz_theta2 * tw ) ) ) ;

  Wfgut = 0.001 * Wfgut_gompertz_theta0 * exp ( Wfgut_gompertz_theta1 / Wfgut_gompertz_theta2 * ( 1 - exp ( - Wfgut_gompertz_theta2 * tw ) ) ) ;

  Wflung = 0.001 * Wflung_gompertz_theta0 * exp ( Wflung_gompertz_theta1 / Wflung_gompertz_theta2 * ( 1 - exp ( - Wflung_gompertz_theta2 * tw ) ) ) ;

  hematocrit = ( hematocrit_quadratic_theta0 + hematocrit_quadratic_theta1 * tw + hematocrit_quadratic_theta2 * pow ( tw , 2 ) ) / 100 ;

  yout[ID_Rblood2plasma] = 1 - hematocrit + hematocrit * Krbc2pu * Fraction_unbound_plasma ;

  fhematocrit = ( fhematocrit_cubic_theta1 * tw + fhematocrit_cubic_theta2 * pow ( tw , 2 ) + fhematocrit_cubic_theta3 * pow ( tw , 3 ) ) / 100 ;

  yout[ID_Rfblood2plasma] = 1 - fhematocrit + fhematocrit * Kfrbc2pu * Fraction_unbound_plasma_fetus ;

  fBW = 0.001 * fBW_gompertz_theta0 * exp ( fBW_gompertz_theta1 / fBW_gompertz_theta2 * ( 1 - exp ( - fBW_gompertz_theta2 * tw ) ) ) ;

  Vplacenta = 0.001 * ( Vplacenta_cubic_theta1 * tw + Vplacenta_cubic_theta2 * pow ( tw , 2 ) + Vplacenta_cubic_theta3 * pow ( tw , 3 ) ) ;

  Vamnf = 0.001 * Vamnf_logistic_theta0 / ( 1 + exp ( - Vamnf_logistic_theta1 * ( tw - Vamnf_logistic_theta2 ) ) ) ;

  Vplasma = Vplasma_mod_logistic_theta0 / ( 1 + exp ( - Vplasma_mod_logistic_theta1 * ( tw - Vplasma_mod_logistic_theta2 ) ) ) + Vplasma_mod_logistic_theta3 ;

  Vrbcs = hematocrit / ( 1 - hematocrit ) * Vplasma ;

  Vven = venous_blood_fraction * ( Vrbcs + Vplasma ) ;

  Vart = arterial_blood_fraction * ( Vrbcs + Vplasma ) ;

  Vadipose = 1 / adipose_density * Wadipose ;

  Vffmx = 1 / ffmx_density * ( BW - Wadipose - ( fBW + placenta_density * Vplacenta + amnf_density * Vamnf ) ) ;

  Vallx = Vart + Vven + Vthyroid + Vkidney + Vgut + Vliver + Vlung ;

  Vrest = Vffmx - Vallx ;

  Vfart = 0.001 * arterial_blood_fraction * fblood_weight_ratio * fBW ;

  Vfven = 0.001 * venous_blood_fraction * fblood_weight_ratio * fBW ;

  Vfkidney = 1 / kidney_density * Wfkidney ;

  Vfthyroid = 1 / thyroid_density * Wfthyroid ;

  Vfliver = 1 / liver_density * Wfliver ;

  Vfbrain = 1 / brain_density * Wfbrain ;

  Vfgut = 1 / gut_density * Wfgut ;

  Vflung = 1 / lung_density * Wflung ;

  Vfrest = fBW - ( Vfart + Vfven + Vfbrain + Vfkidney + Vfthyroid + Vfliver + Vfgut + Vflung ) ;

  Qcardiac = 24 * ( Qcardiac_cubic_theta0 + Qcardiac_cubic_theta1 * tw + Qcardiac_cubic_theta2 * pow ( tw , 2 ) + Qcardiac_cubic_theta3 * pow ( tw , 3 ) ) ;

  Qgut = 0.01 * ( Qgut_percent_initial + ( Qgut_percent_terminal - Qgut_percent_initial ) / term * tw ) * Qcardiac ;

  Qkidney = 24 * ( Qkidney_cubic_theta0 + Qkidney_cubic_theta1 * tw + Qkidney_cubic_theta2 * pow ( tw , 2 ) + Qkidney_cubic_theta3 * pow ( tw , 3 ) ) ;

  Qliver = 0.01 * ( Qliver_percent_initial + ( Qliver_percent_terminal - Qliver_percent_initial ) / term * tw ) * Qcardiac ;

  Qthyroid = 0.01 * ( Qthyroid_percent_initial + ( Qthyroid_percent_terminal - Qthyroid_percent_terminal ) / term * tw ) * Qcardiac ;

  Qplacenta = 24 * Qplacenta_linear_theta1 * 1000 * Vplacenta ;

  Qadipose = 0.01 * ( Qadipose_percent_initial + ( Qadipose_percent_terminal - Qadipose_percent_initial ) / term * tw ) * Qcardiac ;

  Qrest = Qcardiac - ( Qgut + Qkidney + Qliver + Qthyroid + Qplacenta + Qadipose ) ;

  Qgfr = 60 * 24 * 0.001 * ( Qgfr_quadratic_theta0 + Qgfr_quadratic_theta1 * tw + Qgfr_quadratic_theta2 * pow ( tw , 2 ) ) ;

  Qfrvtl = 60 * 24 * 0.001 * Qfrvtl_logistic_theta0 / ( 1 + exp ( - Qfrvtl_logistic_theta1 * ( tw - Qfrvtl_logistic_theta2 ) ) ) ;

  Qflvtl = 60 * 24 * 0.001 * Qflvtl_logistic_theta0 / ( 1 + exp ( - Qflvtl_logistic_theta1 * ( tw - Qflvtl_logistic_theta2 ) ) ) ;

  Qfda = 60 * 24 * 0.001 * Qfda_logistic_theta0 / ( 1 + exp ( - Qfda_logistic_theta1 * ( tw - Qfda_logistic_theta2 ) ) ) ;

  Qfartb = Qflvtl + Qfda ;

  Qfcardiac = Qfartb ;

  Qflung = Qfrvtl - Qfda ;

  Qfplacenta = 60 * 24 * 0.001 * Qfplacenta_logistic_theta0 / ( 1 + exp ( - Qfplacenta_logistic_theta1 * ( tw - Qfplacenta_logistic_theta2 ) ) ) ;

  Qfdv = 60 * 24 * 0.001 * Qfdv_gompertz_theta0 * exp ( Qfdv_gompertz_theta1 / Qfdv_gompertz_theta2 * ( 1 - exp ( - Qfdv_gompertz_theta2 * tw ) ) ) ;

  Qfgut = Qfgut_percent / Qfnonplacental_percent * ( 1 - Qfplacenta / Qfartb ) * Qfartb ;

  Qfkidney = Qfkidney_percent / Qfnonplacental_percent * ( 1 - Qfplacenta / Qfartb ) * Qfartb ;

  Qfbrain = Qfbrain_percent / Qfnonplacental_percent * ( 1 - Qfplacenta / Qfartb ) * Qfartb ;

  Qfliver = Qfliver_percent / ( 100 - ( Qbrain_percent + Qkidney_percent + Qgut_percent ) ) * ( 1 - ( Qfbrain_percent + Qfkidney_percent + Qfgut_percent ) / Qfnonplacental_percent ) * ( 1 - Qfplacenta / Qfartb ) * Qfartb ;

  Qfthyroid = Qfthyroid_percent / ( 100 - ( Qbrain_percent + Qkidney_percent + Qgut_percent ) ) * ( 1 - ( Qfbrain_percent + Qfkidney_percent + Qfgut_percent ) / Qfnonplacental_percent ) * ( 1 - Qfplacenta / Qfartb ) * Qfartb ;

  Qfrest = Qfcardiac - ( Qfplacenta + Qfgut + Qfliver + Qfthyroid + Qfkidney + Qfbrain ) ;

  Qfbypass = Qfcardiac - Qflung ;

  yout[ID_Cgut] = y[ID_Agut] / Vgut ;

  yout[ID_Cliver] = y[ID_Aliver] / Vliver ;

  yout[ID_Cven] = y[ID_Aven] / Vven ;

  yout[ID_Clung] = y[ID_Alung] / Vlung ;

  yout[ID_Cart] = y[ID_Aart] / Vart ;

  yout[ID_Cadipose] = y[ID_Aadipose] / Vadipose ;

  yout[ID_Crest] = y[ID_Arest] / Vrest ;

  yout[ID_Ckidney] = y[ID_Akidney] / Vkidney ;

  yout[ID_Cplasma] = y[ID_Aven] / Vven / yout[ID_Rblood2plasma] ;

  yout[ID_Aplasma] = y[ID_Aven] / yout[ID_Rblood2plasma] * ( 1 - hematocrit ) ;

  yout[ID_Cthyroid] = y[ID_Athyroid] / Vthyroid ;

  yout[ID_Cplacenta] = y[ID_Aplacenta] / Vplacenta ;

  yout[ID_Cfart] = y[ID_Afart] / Vfart ;

  yout[ID_Cfven] = y[ID_Afven] / Vfven ;

  yout[ID_Cfkidney] = y[ID_Afkidney] / Vfkidney ;

  yout[ID_Cfrest] = y[ID_Afrest] / Vfrest ;

  yout[ID_Cfthyroid] = y[ID_Afthyroid] / Vfthyroid ;

  yout[ID_Cfliver] = y[ID_Afliver] / Vfliver ;

  yout[ID_Cfbrain] = y[ID_Afbrain] / Vfbrain ;

  yout[ID_Cflung] = y[ID_Aflung] / Vflung ;

  yout[ID_Cfgut] = y[ID_Afgut] / Vfgut ;

  yout[ID_Cfplasma] = y[ID_Afven] / Vfven / yout[ID_Rblood2plasma] ;

  yout[ID_Afplasma] = y[ID_Afven] / yout[ID_Rblood2plasma] * ( 1 - fhematocrit ) ;

  ydot[ID_Agutlumen] = - kgutabs * y[ID_Agutlumen] ;

  ydot[ID_Agut] = kgutabs * y[ID_Agutlumen] + Qgut * ( y[ID_Aart] / Vart - y[ID_Agut] / Vgut * yout[ID_Rblood2plasma] / Kgut2pu / Fraction_unbound_plasma ) ;

  ydot[ID_Aliver] = Qliver * y[ID_Aart] / Vart + Qgut * y[ID_Agut] / Vgut * yout[ID_Rblood2plasma] / Kgut2pu / Fraction_unbound_plasma - ( Qliver + Qgut ) * y[ID_Aliver] / Vliver / Kliver2pu / Fraction_unbound_plasma * yout[ID_Rblood2plasma] - Clmetabolism * y[ID_Aliver] / Vliver / Kliver2pu ;

  ydot[ID_Aven] = ( ( Qliver + Qgut ) * y[ID_Aliver] / Vliver / Kliver2pu + Qkidney * y[ID_Akidney] / Vkidney / Kkidney2pu + Qadipose * y[ID_Aadipose] / Vadipose / Kadipose2pu + Qrest * y[ID_Arest] / Vrest / Krest2pu + Qthyroid * y[ID_Athyroid] / Vthyroid / Kthyroid2pu + Qplacenta * y[ID_Aplacenta] / Vplacenta / Kplacenta2pu ) * yout[ID_Rblood2plasma] / Fraction_unbound_plasma - Qcardiac * y[ID_Aven] / Vven ;

  ydot[ID_Alung] = Qcardiac * ( y[ID_Aven] / Vven - y[ID_Alung] / Vlung * yout[ID_Rblood2plasma] / Klung2pu / Fraction_unbound_plasma ) ;

  ydot[ID_Aart] = Qcardiac * ( y[ID_Alung] / Vlung * yout[ID_Rblood2plasma] / Klung2pu / Fraction_unbound_plasma - y[ID_Aart] / Vart ) ;

  ydot[ID_Aadipose] = Qadipose * ( y[ID_Aart] / Vart - y[ID_Aadipose] / Vadipose * yout[ID_Rblood2plasma] / Kadipose2pu / Fraction_unbound_plasma ) ;

  ydot[ID_Arest] = Qrest * ( y[ID_Aart] / Vart - y[ID_Arest] / Vrest * yout[ID_Rblood2plasma] / Krest2pu / Fraction_unbound_plasma ) ;

  ydot[ID_Akidney] = Qkidney * y[ID_Aart] / Vart - Qkidney * y[ID_Akidney] / Vkidney / Kkidney2pu * yout[ID_Rblood2plasma] / Fraction_unbound_plasma - Qgfr * y[ID_Aart] / Vart / yout[ID_Rblood2plasma] * Fraction_unbound_plasma ;

  ydot[ID_Atubules] = Qgfr * y[ID_Aart] / Vart / yout[ID_Rblood2plasma] * Fraction_unbound_plasma ;

  ydot[ID_Ametabolized] = Clmetabolism * y[ID_Aliver] / Vliver / Kliver2pu ;

  ydot[ID_AUC] = y[ID_Aven] / Vven / yout[ID_Rblood2plasma] ;

  ydot[ID_Athyroid] = Qthyroid * ( y[ID_Aart] / Vart - y[ID_Athyroid] / Vthyroid / Kthyroid2pu / Fraction_unbound_plasma * yout[ID_Rblood2plasma] ) ;

  ydot[ID_Aplacenta] = Qplacenta * y[ID_Aart] / Vart - Qplacenta * ( y[ID_Aplacenta] / Vplacenta / Kplacenta2pu / Fraction_unbound_plasma * yout[ID_Rblood2plasma] ) - Qfplacenta * ( y[ID_Aplacenta] / Vplacenta / Kfplacenta2pu / Fraction_unbound_plasma_fetus * yout[ID_Rfblood2plasma] ) + Qfplacenta * y[ID_Afart] / Vfart ;

  ydot[ID_Afart] = Qflung * y[ID_Aflung] / Vflung * yout[ID_Rfblood2plasma] / Kflung2pu / Fraction_unbound_plasma_fetus + Qfbypass * y[ID_Afven] / Vfven - Qfcardiac * y[ID_Afart] / Vfart ;

  ydot[ID_Afven] = ( ( Qfliver + Qfgut + Qfplacenta - Qfdv ) * y[ID_Afliver] / Vfliver / Kfliver2pu + Qfdv * y[ID_Aplacenta] / Vplacenta / Kfplacenta2pu + Qfthyroid * y[ID_Afthyroid] / Vfthyroid / Kfthyroid2pu + Qfrest * y[ID_Afrest] / Vfrest / Kfrest2pu + Qfkidney * y[ID_Afkidney] / Vfkidney / Kfkidney2pu + Qfbrain * y[ID_Afbrain] / Vfbrain / Kfbrain2pu ) * yout[ID_Rfblood2plasma] / Fraction_unbound_plasma_fetus - Qfcardiac * y[ID_Afven] / Vfven ;

  ydot[ID_Afkidney] = Qfkidney * ( y[ID_Afart] / Vfart - y[ID_Afkidney] / Vfkidney / Kfkidney2pu * yout[ID_Rfblood2plasma] / Fraction_unbound_plasma_fetus ) ;

  ydot[ID_Afrest] = Qfrest * ( y[ID_Afart] / Vfart - y[ID_Afrest] / Vfrest / Kfrest2pu * yout[ID_Rfblood2plasma] / Fraction_unbound_plasma_fetus ) ;

  ydot[ID_Afthyroid] = Qfthyroid * ( y[ID_Afart] / Vfart - y[ID_Afthyroid] / Vfthyroid / Kfthyroid2pu * yout[ID_Rfblood2plasma] / Fraction_unbound_plasma_fetus ) ;

  ydot[ID_Afliver] = Qfliver * y[ID_Afart] / Vfart + ( Qfgut * y[ID_Afgut] / Vfgut * yout[ID_Rfblood2plasma] / Kfgut2pu + ( Qfplacenta - Qfdv ) * y[ID_Aplacenta] / Vplacenta / Kfplacenta2pu * yout[ID_Rfblood2plasma] - ( Qfliver + Qfgut + Qfplacenta - Qfdv ) * y[ID_Afliver] / Vfliver / Kfliver2pu * yout[ID_Rfblood2plasma] ) / Fraction_unbound_plasma_fetus ;

  ydot[ID_Aflung] = Qflung * ( y[ID_Afven] / Vfven - y[ID_Aflung] / Vflung * yout[ID_Rfblood2plasma] / Kflung2pu / Fraction_unbound_plasma_fetus ) ;

  ydot[ID_Afgut] = Qfgut * ( y[ID_Afart] / Vfart - y[ID_Afgut] / Vfgut * yout[ID_Rfblood2plasma] / Kfgut2pu / Fraction_unbound_plasma_fetus ) ;

  ydot[ID_Afbrain] = Qfbrain * ( y[ID_Afart] / Vfart - y[ID_Afbrain] / Vfbrain * yout[ID_Rfblood2plasma] / Kfbrain2pu / Fraction_unbound_plasma_fetus ) ;

  ydot[ID_fAUC] = y[ID_Afven] / Vfven / yout[ID_Rfblood2plasma] ;

} /* derivs */


/*----- Jacobian calculations: */
void jacfetal_pbtk (int *neq, double *t, double *y, int *ml, int *mu, double *pd, int *nrowpd, double *yout, int *ip)
{

} /* jac */


/*----- Events calculations: */
void eventfetal_pbtk (int *n, double *t, double *y)
{

} /* event */

/*----- Roots calculations: */
void rootfetal_pbtk (int *neq, double *t, double *y, int *ng, double *gout, double *out, int *ip)
{

} /* root */

