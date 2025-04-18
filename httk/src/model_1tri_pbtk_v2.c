/* firsttrimester/model_1tri_pbtk_v2-raw.c for R deSolve package
   ___________________________________________________

   Model File:  firsttrimester/model_1tri_pbtk_v2.model

   Date:  Thu Feb 29 09:16:55 2024

   Created by:  "C:/Users/ktruong/OneDrive - Environmental Protection Agency (EPA)/Profile/Documents/httk-dev/models/mod.exe v6.1.0"
    -- a model preprocessor by Don Maszle
   ___________________________________________________

   Copyright (c) 1993-2019 Free Software Foundation, Inc.

   Model calculations for compartmental model:

   14 States:
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
     AUC = 0.0,

   22 Outputs:
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
    "Vallx",

   0 Inputs:

   86 Parameters:
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
     Qconceptus_initial = 0,
*/

#include <R.h>
#include <Rinternals.h>
#include <Rdefines.h>
#include <R_ext/Rdynload.h>

/* Model variables: States */
#define ID_Agutlumen 0x00000
#define ID_Agut 0x00001
#define ID_Aliver 0x00002
#define ID_Akidney 0x00003
#define ID_Alung 0x00004
#define ID_Aven 0x00005
#define ID_Aart 0x00006
#define ID_Aadipose 0x00007
#define ID_Athyroid 0x00008
#define ID_Arest 0x00009
#define ID_Aconceptus 0x0000a
#define ID_Atubules 0x0000b
#define ID_Ametabolized 0x0000c
#define ID_AUC 0x0000d

/* Model variables: Outputs */
#define ID_Cgut 0x00000
#define ID_Cliver 0x00001
#define ID_Ckidney 0x00002
#define ID_Clung 0x00003
#define ID_Cven 0x00004
#define ID_Cart 0x00005
#define ID_Cadipose 0x00006
#define ID_Cthyroid 0x00007
#define ID_Crest 0x00008
#define ID_Cconceptus 0x00009
#define ID_Cplasma 0x0000a
#define ID_Aplasma 0x0000b
#define ID_Rblood2plasma 0x0000c
#define ID_Vven 0x0000d
#define ID_Vart 0x0000e
#define ID_Vadipose 0x0000f
#define ID_Vrest 0x00010
#define ID_hematocrit 0x00011
#define ID_Vconceptus 0x00012
#define ID_Qconceptus 0x00013
#define ID_Vffmx 0x00014
#define ID_Vallx 0x00015

/* Parameters */
static double parms[86];

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
#define Kconceptus2pu_initial parms[12]
#define Kconceptus2pu_final parms[13]
#define Vgutc parms[14]
#define Vgut parms[15]
#define Vkidneyc parms[16]
#define Vkidney parms[17]
#define Vliverc parms[18]
#define Vliver parms[19]
#define Vlungc parms[20]
#define Vlung parms[21]
#define Vthyroidc parms[22]
#define Vthyroid parms[23]
#define Fraction_unbound_plasma parms[24]
#define gut_density parms[25]
#define kidney_density parms[26]
#define liver_density parms[27]
#define lung_density parms[28]
#define thyroid_density parms[29]
#define adipose_density parms[30]
#define ffmx_density parms[31]
#define placenta_density parms[32]
#define amnf_density parms[33]
#define brain_density parms[34]
#define BW_cubic_theta1 parms[35]
#define BW_cubic_theta2 parms[36]
#define BW_cubic_theta3 parms[37]
#define Wadipose_linear_theta0 parms[38]
#define Wadipose_linear_theta1 parms[39]
#define hematocrit_quadratic_theta0 parms[40]
#define hematocrit_quadratic_theta1 parms[41]
#define hematocrit_quadratic_theta2 parms[42]
#define fBW_gompertz_theta0 parms[43]
#define fBW_gompertz_theta1 parms[44]
#define fBW_gompertz_theta2 parms[45]
#define Vplacenta_cubic_theta1 parms[46]
#define Vplacenta_cubic_theta2 parms[47]
#define Vplacenta_cubic_theta3 parms[48]
#define Vamnf_logistic_theta0 parms[49]
#define Vamnf_logistic_theta1 parms[50]
#define Vamnf_logistic_theta2 parms[51]
#define Vplasma_mod_logistic_theta0 parms[52]
#define Vplasma_mod_logistic_theta1 parms[53]
#define Vplasma_mod_logistic_theta2 parms[54]
#define Vplasma_mod_logistic_theta3 parms[55]
#define venous_blood_fraction parms[56]
#define arterial_blood_fraction parms[57]
#define Qcardiac_cubic_theta0 parms[58]
#define Qcardiac_cubic_theta1 parms[59]
#define Qcardiac_cubic_theta2 parms[60]
#define Qcardiac_cubic_theta3 parms[61]
#define term parms[62]
#define Qgut_percent_initial parms[63]
#define Qgut_percent_terminal parms[64]
#define Qkidney_cubic_theta0 parms[65]
#define Qkidney_cubic_theta1 parms[66]
#define Qkidney_cubic_theta2 parms[67]
#define Qkidney_cubic_theta3 parms[68]
#define Qliver_percent_initial parms[69]
#define Qliver_percent_terminal parms[70]
#define Qthyroid_percent_initial parms[71]
#define Qthyroid_percent_terminal parms[72]
#define Qplacenta_linear_theta1 parms[73]
#define Qadipose_percent_initial parms[74]
#define Qadipose_percent_terminal parms[75]
#define Qgfr_quadratic_theta0 parms[76]
#define Qgfr_quadratic_theta1 parms[77]
#define Qgfr_quadratic_theta2 parms[78]
#define fBW_13wks parms[79]
#define Vplacenta_13wks parms[80]
#define Vamnf_13wks parms[81]
#define Vconceptus_final parms[82]
#define Vconceptus_initial parms[83]
#define Qconceptus_final parms[84]
#define Qconceptus_initial parms[85]

// /* Forcing (Input) functions */
// static double forc[0];
// 
// 
// /* Function definitions for delay differential equations */
// 
// int Nout=1;
// int nr[1]={0};
// double ytau[1] = {0.0};
// 
// static double yini[14] = {0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0}; /*Array of initial state variables*/
// 
// void lagvalue(double T, int *nr, int N, double *ytau) {
//   static void(*fun)(double, int*, int, double*) = NULL;
//   if (fun == NULL)
//     fun = (void(*)(double, int*, int, double*))R_GetCCallable("deSolve", "lagvalue");
//   return fun(T, nr, N, ytau);
// }
// 
// double CalcDelay(int hvar, double dTime, double delay) {
//   double T = dTime-delay;
//   if (dTime > delay){
//     nr[0] = hvar;
//     lagvalue( T, nr, Nout, ytau );
// }
//   else{
//     ytau[0] = yini[hvar];
// }
//   return(ytau[0]);
// }

/*----- Initializers */
void initmod_firsttrimester (void (* odeparms)(int *, double *))
{
  int N=86;
  odeparms(&N, parms);
}

// void initforc (void (* odeforcs)(int *, double *))
// {
//   int N=0;
//   odeforcs(&N, forc);
// }


// /* Calling R code will ensure that input y has same
//    dimension as yini */
// void initState (double *y)
// {
//   int i;
// 
//   for (i = 0; i < sizeof(yini) / sizeof(yini[0]); i++)
//   {
//     yini[i] = y[i];
//   }
// }

void getParms_firsttrimester (double *inParms, double *out, int *nout) {
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

  fBW_13wks = 0.001 * fBW_gompertz_theta0 * exp ( fBW_gompertz_theta1 / fBW_gompertz_theta2 * ( 1 - exp ( - fBW_gompertz_theta2 * 13 ) ) ) ;
  Vplacenta_13wks = 0.001 * ( Vplacenta_cubic_theta1 * 13 + Vplacenta_cubic_theta2 * pow ( 13 , 2 ) + Vplacenta_cubic_theta3 * pow ( 13 , 3 ) ) ;
  Vamnf_13wks = 0.001 * Vamnf_logistic_theta0 / ( 1 + exp ( - Vamnf_logistic_theta1 * ( 13 - Vamnf_logistic_theta2 ) ) ) ;

  Vconceptus_final = fBW_13wks + Vplacenta_13wks + Vamnf_13wks ;
  Vconceptus_initial = 0 ;

  Qconceptus_final = 24 * Qplacenta_linear_theta1 * 1000 * Vplacenta_13wks ;
  Qconceptus_initial = 0 ;

  for (i = 0; i < *nout; i++) {
    out[i] = parms[i];
  }
  }
/*----- Dynamics section */

void derivs_firsttrimester (int *neq, double *pdTime, double *y, double *ydot, double *yout, int *ip)
{
  /* local */ double tw;
  /* local */ double BW;
  /* local */ double Wadipose;
  /* local */ double Vplasma;
  /* local */ double Vrbcs;
  /* local */ double Qcardiac;
  /* local */ double Qgut;
  /* local */ double Qkidney;
  /* local */ double Qliver;
  /* local */ double Qthyroid;
  /* local */ double Qadipose;
  /* local */ double Qrest;
  /* local */ double Qgfr;
  /* local */ double Kconceptus2pu;

  tw = (*pdTime) / 7.0 ;

  BW = pre_pregnant_BW + BW_cubic_theta1 * tw + BW_cubic_theta2 * pow ( tw , 2 ) + BW_cubic_theta3 * pow ( tw , 3 ) ;

  Wadipose = Wadipose_linear_theta0 + Wadipose_linear_theta1 * tw ;

  yout[ID_hematocrit] = ( hematocrit_quadratic_theta0 + hematocrit_quadratic_theta1 * tw + hematocrit_quadratic_theta2 * pow ( tw , 2 ) ) / 100 ;

  yout[ID_Rblood2plasma] = 1 - yout[ID_hematocrit] + yout[ID_hematocrit] * Krbc2pu * Fraction_unbound_plasma ;

  Vplasma = Vplasma_mod_logistic_theta0 / ( 1 + exp ( - Vplasma_mod_logistic_theta1 * ( tw - Vplasma_mod_logistic_theta2 ) ) ) + Vplasma_mod_logistic_theta3 ;

  Vrbcs = yout[ID_hematocrit] / ( 1 - yout[ID_hematocrit] ) * Vplasma ;

  yout[ID_Vven] = venous_blood_fraction * ( Vrbcs + Vplasma ) ;

  yout[ID_Vart] = arterial_blood_fraction * ( Vrbcs + Vplasma ) ;

  yout[ID_Vadipose] = 1 / adipose_density * Wadipose ;

  yout[ID_Vconceptus] = Vconceptus_initial + ( ( Vconceptus_final - Vconceptus_initial ) / 13 ) * tw ;

  yout[ID_Vffmx] = 1 / ffmx_density * ( BW - Wadipose - yout[ID_Vconceptus] ) ;

  yout[ID_Vallx] = yout[ID_Vart] + yout[ID_Vven] + Vthyroid + Vkidney + Vgut + Vliver + Vlung ;

  yout[ID_Vrest] = yout[ID_Vffmx] - yout[ID_Vallx] ;

  Qcardiac = 24 * ( Qcardiac_cubic_theta0 + Qcardiac_cubic_theta1 * tw + Qcardiac_cubic_theta2 * pow ( tw , 2 ) + Qcardiac_cubic_theta3 * pow ( tw , 3 ) ) ;

  Qgut = 0.01 * ( Qgut_percent_initial + ( Qgut_percent_terminal - Qgut_percent_initial ) / term * tw ) * Qcardiac ;

  Qkidney = 24 * ( Qkidney_cubic_theta0 + Qkidney_cubic_theta1 * tw + Qkidney_cubic_theta2 * pow ( tw , 2 ) + Qkidney_cubic_theta3 * pow ( tw , 3 ) ) ;

  Qliver = 0.01 * ( Qliver_percent_initial + ( Qliver_percent_terminal - Qliver_percent_initial ) / term * tw ) * Qcardiac ;

  Qthyroid = 0.01 * ( Qthyroid_percent_initial + ( Qthyroid_percent_terminal - Qthyroid_percent_initial ) / term * tw ) * Qcardiac ;

  yout[ID_Qconceptus] = Qconceptus_initial + ( ( Qconceptus_final - Qconceptus_initial ) / 13 ) * tw ;

  Qadipose = 0.01 * ( Qadipose_percent_initial + ( Qadipose_percent_terminal - Qadipose_percent_initial ) / term * tw ) * Qcardiac ;

  Qrest = Qcardiac - ( Qgut + Qkidney + Qliver + Qthyroid + yout[ID_Qconceptus] + Qadipose ) ;

  Qgfr = 60 * 24 * 0.001 * ( Qgfr_quadratic_theta0 + Qgfr_quadratic_theta1 * tw + Qgfr_quadratic_theta2 * pow ( tw , 2 ) ) ;

  yout[ID_Cgut] = y[ID_Agut] / Vgut ;

  yout[ID_Cliver] = y[ID_Aliver] / Vliver ;

  yout[ID_Cven] = y[ID_Aven] / yout[ID_Vven] ;

  yout[ID_Clung] = y[ID_Alung] / Vlung ;

  yout[ID_Cart] = y[ID_Aart] / yout[ID_Vart] ;

  yout[ID_Cadipose] = y[ID_Aadipose] / yout[ID_Vadipose] ;

  yout[ID_Crest] = y[ID_Arest] / yout[ID_Vrest] ;

  yout[ID_Ckidney] = y[ID_Akidney] / Vkidney ;

  yout[ID_Cplasma] = y[ID_Aven] / yout[ID_Vven] / yout[ID_Rblood2plasma] ;

  yout[ID_Aplasma] = y[ID_Aven] / yout[ID_Rblood2plasma] * ( 1 - yout[ID_hematocrit] ) ;

  yout[ID_Cthyroid] = y[ID_Athyroid] / Vthyroid ;

  yout[ID_Cconceptus] = ( yout[ID_Vconceptus] > 0 ? y[ID_Aconceptus] / yout[ID_Vconceptus] : 0 ) ;

  Kconceptus2pu = Kconceptus2pu_initial + ( ( Kconceptus2pu_final - Kconceptus2pu_initial ) / 13 ) * tw ;

  ydot[ID_Agutlumen] = - kgutabs * y[ID_Agutlumen] ;

  ydot[ID_Agut] = kgutabs * y[ID_Agutlumen] + Qgut * ( y[ID_Aart] / yout[ID_Vart] - y[ID_Agut] / Vgut * yout[ID_Rblood2plasma] / Kgut2pu / Fraction_unbound_plasma ) ;

  ydot[ID_Aliver] = Qliver * y[ID_Aart] / yout[ID_Vart] + Qgut * y[ID_Agut] / Vgut * yout[ID_Rblood2plasma] / Kgut2pu / Fraction_unbound_plasma - ( Qliver + Qgut ) * y[ID_Aliver] / Vliver / Kliver2pu / Fraction_unbound_plasma * yout[ID_Rblood2plasma] - Clmetabolism * y[ID_Aliver] / Vliver / Kliver2pu ;

  ydot[ID_Aven] = ( ( Qliver + Qgut ) * y[ID_Aliver] / Vliver / Kliver2pu + Qkidney * y[ID_Akidney] / Vkidney / Kkidney2pu + Qadipose * y[ID_Aadipose] / yout[ID_Vadipose] / Kadipose2pu + Qrest * y[ID_Arest] / yout[ID_Vrest] / Krest2pu + Qthyroid * y[ID_Athyroid] / Vthyroid / Kthyroid2pu + yout[ID_Qconceptus] * yout[ID_Cconceptus] / Kconceptus2pu ) * yout[ID_Rblood2plasma] / Fraction_unbound_plasma - Qcardiac * y[ID_Aven] / yout[ID_Vven] ;

  ydot[ID_Alung] = Qcardiac * ( y[ID_Aven] / yout[ID_Vven] - y[ID_Alung] / Vlung * yout[ID_Rblood2plasma] / Klung2pu / Fraction_unbound_plasma ) ;

  ydot[ID_Aart] = Qcardiac * ( y[ID_Alung] / Vlung * yout[ID_Rblood2plasma] / Klung2pu / Fraction_unbound_plasma - y[ID_Aart] / yout[ID_Vart] ) ;

  ydot[ID_Aadipose] = Qadipose * ( y[ID_Aart] / yout[ID_Vart] - y[ID_Aadipose] / yout[ID_Vadipose] * yout[ID_Rblood2plasma] / Kadipose2pu / Fraction_unbound_plasma ) ;

  ydot[ID_Arest] = Qrest * ( y[ID_Aart] / yout[ID_Vart] - y[ID_Arest] / yout[ID_Vrest] * yout[ID_Rblood2plasma] / Krest2pu / Fraction_unbound_plasma ) ;

  ydot[ID_Akidney] = Qkidney * y[ID_Aart] / yout[ID_Vart] - Qkidney * y[ID_Akidney] / Vkidney / Kkidney2pu * yout[ID_Rblood2plasma] / Fraction_unbound_plasma - Qgfr * y[ID_Aart] / yout[ID_Vart] / yout[ID_Rblood2plasma] * Fraction_unbound_plasma ;

  ydot[ID_Atubules] = Qgfr * y[ID_Aart] / yout[ID_Vart] / yout[ID_Rblood2plasma] * Fraction_unbound_plasma ;

  ydot[ID_Ametabolized] = Clmetabolism * y[ID_Aliver] / Vliver / Kliver2pu ;

  ydot[ID_AUC] = y[ID_Aven] / yout[ID_Vven] / yout[ID_Rblood2plasma] ;

  ydot[ID_Athyroid] = Qthyroid * ( y[ID_Aart] / yout[ID_Vart] - y[ID_Athyroid] / Vthyroid / Kthyroid2pu / Fraction_unbound_plasma * yout[ID_Rblood2plasma] ) ;

  ydot[ID_Aconceptus] = yout[ID_Qconceptus] * ( y[ID_Aart] / yout[ID_Vart] - yout[ID_Cconceptus] / Kconceptus2pu / Fraction_unbound_plasma * yout[ID_Rblood2plasma] ) ;

} /* derivs */


/*----- Jacobian calculations: */
void jac_firsttrimester (int *neq, double *t, double *y, int *ml, int *mu, double *pd, int *nrowpd, double *yout, int *ip)
{

} /* jac */


/*----- Events calculations: */
void event_firsttrimester (int *n, double *t, double *y)
{

} /* event */

/*----- Roots calculations: */
void root_firsttrimester (int *neq, double *t, double *y, int *ng, double *gout, double *out, int *ip)
{

} /* root */

