/* vLiverPBPKfetus.c for R deSolve package
   ___________________________________________________

   Model File:  vLiverPBPKfetus.model

   Date:  Fri Jun 19 16:21:07 2015

   Created by:  "mod v5.5.0"
    -- a model preprocessor by Don Maszle
   ___________________________________________________

   Copyright (c) 1993-2013 Free Software Foundation, Inc.

   Model calculations for compartmental model:

   23 States:
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

   22 Outputs:
    "Cgut",
    "Cliver",
    "Cven",
    "Clung",
    "Cart",
    "Crest",
    "Ckidney",
    "Cserum",
    "Aserum",
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
    "Afserum",
    "Cfserum",

   0 Inputs:

   40 Parameters:
     pre_pregnant_BW = 0,
     CLmetabolismc = 0.0,
     hematocrit = 0,
     kgutabs = 1,
     Kkidney2plasma = 0,
     Kliver2plasma = 0,
     Krbc2plasma = 0,
     Krest2plasma = 0,
     Klung2plasma = 0,
     Kgut2plasma = 0,
     Kthyroid2plasma = 0,
     Kplacenta2plasma = 0,
     Kfplacenta2plasma = 0,
     Kfkidney2plasma = 0,
     Kfrest2plasma = 0,
     Kfthyroid2plasma = 0,
     Kfliver2plasma = 0,
     Kflung2plasma = 0,
     Kfgut2plasma = 0,
     Kfbrain2plasma = 0,
     Vartc = 0,
     Vvenc = 0,
     Vgutc = 0,
     Vkidneyc = 0,
     Vliverc = 0,
     Vlungc = 0,
     Vart = 0,
     Vven = 0,
     Vgut = 0,
     Vkidney = 0,
     Vliver = 0,
     Vlung = 0,
     Vthyroidc = 0,
     Vthyroid = 0,
     Vfgutc = 0,
     Fraction_unbound_plasma = 0,
     Ratioblood2plasma = 0.0,
     CLmetabolism = 0.0,
     Qgfrc = 0,
     Qgfr = 0.0,
*/

#include <R.h>

/* Model variables: States */
#define ID_Agutlumen 0x0000
#define ID_Agut 0x0001
#define ID_Aliver 0x0002
#define ID_Aven 0x0003
#define ID_Alung 0x0004
#define ID_Aart 0x0005
#define ID_Arest 0x0006
#define ID_Akidney 0x0007
#define ID_Atubules 0x0008
#define ID_Ametabolized 0x0009
#define ID_AUC 0x000a
#define ID_fAUC 0x000b
#define ID_Athyroid 0x000c
#define ID_Aplacenta 0x000d
#define ID_Afgut 0x000e
#define ID_Aflung 0x000f
#define ID_Afliver 0x0010
#define ID_Afven 0x0011
#define ID_Afart 0x0012
#define ID_Afrest 0x0013
#define ID_Afthyroid 0x0014
#define ID_Afkidney 0x0015
#define ID_Afbrain 0x0016

/* Model variables: Outputs */
#define ID_Cgut 0x0000
#define ID_Cliver 0x0001
#define ID_Cven 0x0002
#define ID_Clung 0x0003
#define ID_Cart 0x0004
#define ID_Crest 0x0005
#define ID_Ckidney 0x0006
#define ID_Cserum 0x0007
#define ID_Aserum 0x0008
#define ID_Cthyroid 0x0009
#define ID_Cplacenta 0x000a
#define ID_Cfliver 0x000b
#define ID_Cfven 0x000c
#define ID_Cfart 0x000d
#define ID_Cfgut 0x000e
#define ID_Cflung 0x000f
#define ID_Cfrest 0x0010
#define ID_Cfthyroid 0x0011
#define ID_Cfkidney 0x0012
#define ID_Cfbrain 0x0013
#define ID_Afserum 0x0014
#define ID_Cfserum 0x0015

/* Parameters */
static double parms[40];

#define pre_pregnant_BW parms[0]
#define CLmetabolismc parms[1]
#define hematocrit parms[2]
#define kgutabs parms[3]
#define Kkidney2plasma parms[4]
#define Kliver2plasma parms[5]
#define Krbc2plasma parms[6]
#define Krest2plasma parms[7]
#define Klung2plasma parms[8]
#define Kgut2plasma parms[9]
#define Kthyroid2plasma parms[10]
#define Kplacenta2plasma parms[11]
#define Kfplacenta2plasma parms[12]
#define Kfkidney2plasma parms[13]
#define Kfrest2plasma parms[14]
#define Kfthyroid2plasma parms[15]
#define Kfliver2plasma parms[16]
#define Kflung2plasma parms[17]
#define Kfgut2plasma parms[18]
#define Kfbrain2plasma parms[19]
#define Vartc parms[20]
#define Vvenc parms[21]
#define Vgutc parms[22]
#define Vkidneyc parms[23]
#define Vliverc parms[24]
#define Vlungc parms[25]
#define Vart parms[26]
#define Vven parms[27]
#define Vgut parms[28]
#define Vkidney parms[29]
#define Vliver parms[30]
#define Vlung parms[31]
#define Vthyroidc parms[32]
#define Vthyroid parms[33]
#define Vfgutc parms[34]
#define Fraction_unbound_plasma parms[35]
#define Ratioblood2plasma parms[36]
#define CLmetabolism parms[37]
#define Qgfrc parms[38]
#define Qgfr parms[39]

/* Forcing (Input) functions */
static double forc[0];


/*----- Initializers */
void initmodfetus (void (* odeparms)(int *, double *))
{
  int N=40;
  odeparms(&N, parms);
}

void initforcfetus (void (* odeforcs)(int *, double *))
{
  int N=0;
  odeforcs(&N, forc);
}


void getParms_fetus (double *inParms, double *out, int *nout) {
/*----- Model scaling */

  int i;

  for (i = 0; i < *nout; i++) {
    parms[i] = inParms[i];
  }


  kgutabs = kgutabs * 24 ;
  CLmetabolism = CLmetabolismc * 24 * pre_pregnant_BW ;
  Vart = Vartc * pre_pregnant_BW ;
  Vgut = Vgutc * pre_pregnant_BW ;
  Vkidney = Vkidneyc * pre_pregnant_BW ;
  Vliver = Vliverc * pre_pregnant_BW ;
  Vlung = Vlungc * pre_pregnant_BW ;
  Vven = Vvenc * pre_pregnant_BW ;
  Qgfr = Qgfrc * pow ( pre_pregnant_BW , 0.75 ) * 24 ;
  Vthyroid = Vthyroidc * pre_pregnant_BW ;

  for (i = 0; i < *nout; i++) {
    out[i] = parms[i];
  }
  }
/*----- Dynamics section */

void derivsfetus (int *neq, double *pdTime, double *y, double *ydot, double *yout, int *ip)
{
  /* local */ double BW;
  /* local */ double Qcardiac;
  /* local */ double Qgut;
  /* local */ double Qkidney;
  /* local */ double Qliver;
  /* local */ double Qthyroid;
  /* local */ double Qplacenta;
  /* local */ double Qrest;
  /* local */ double Qfplacenta;
  /* local */ double Qfcardiac;
  /* local */ double Qflung;
  /* local */ double Qfbypass;
  /* local */ double Qfgut;
  /* local */ double Qfkidney;
  /* local */ double Qfbrain;
  /* local */ double Qfliver;
  /* local */ double Qfthyroid;
  /* local */ double Qfrest;
  /* local */ double Qfdv;
  /* local */ double fBW;
  /* local */ double Vplacenta;
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

  BW = pre_pregnant_BW + 0.006514 * exp ( 0.6797 / 0.08698 * ( 1 - exp ( -0.08698 * (*pdTime) / 7 ) ) ) ;

  Qcardiac = 24 * 60 * 7.514 / ( 1 + exp ( -.103 * ( (*pdTime) / 7 +7.458 ) ) ) ;

  Qgut = ( 17 - .1125 * (*pdTime) / 7 ) / 100 * Qcardiac ;

  Qkidney = ( 17 - .01 * (*pdTime) / 7 ) / 100 * Qcardiac ;

  Qliver = ( 6.5 - .0425 * (*pdTime) / 7 ) / 100 * Qcardiac ;

  Qthyroid = ( 1.5 - .01 * (*pdTime) / 7 ) / 100 * Qcardiac ;

  Qplacenta = 0.8 * ( .4 + .29 * (*pdTime) / 7 ) / 100 * Qcardiac ;

  Qrest = Qcardiac - ( Qgut + Qkidney + Qliver + Qplacenta + Qthyroid ) ;

  Qfplacenta = 24 * 60 * exp ( -10.85 + 4.97 * log ( (*pdTime) / 7 ) - 0.00125 * pow ( (*pdTime) / 7 , 2 ) ) / 1000 ;

  Qfcardiac = 24 * 60 / 1000 * ( exp ( -4.3056 + .65052 * (*pdTime) / 7 - 0.01387 * pow ( (*pdTime) / 7 , 2 ) + 0.00010637 * pow ( (*pdTime) / 7 , 3 ) ) + exp ( -2.2256 + .45676 * (*pdTime) / 7 - 0.0082968 * pow ( (*pdTime) / 7 , 2 ) + 0.000059783 * pow ( (*pdTime) / 7 , 3 ) ) ) ;

  Qflung = 24 * 60 / 1000 * ( exp ( -3.2374 + 0.59467 * (*pdTime) / 7 - 0.012764 * pow ( (*pdTime) / 7 , 2 ) + 0.00010515 * pow ( (*pdTime) / 7 , 3 ) ) - exp ( -2.2256 + .45676 * (*pdTime) / 7 - 0.0082968 * pow ( (*pdTime) / 7 , 2 ) + 0.000059783 * pow ( (*pdTime) / 7 , 3 ) ) ) ;

  Qfbypass = Qfcardiac - Qflung ;

  Qfgut = 6.8 / 75 * ( Qfcardiac - Qfplacenta ) ;

  Qfkidney = 5.4 / 75 * ( Qfcardiac - Qfplacenta ) ;

  Qfbrain = 14.3 / 75 * ( Qfcardiac - Qfplacenta ) ;

  Qfliver = ( 6.5 / 54 ) * ( 1 -26.5 / 75 ) * ( Qfcardiac - Qfplacenta ) ;

  Qfthyroid = ( 1.5 / 54 ) * ( 1 -26.5 / 75 ) * ( Qfcardiac - Qfplacenta ) ;

  Qfrest = Qfcardiac - ( Qfgut + Qfkidney + Qfliver + Qfplacenta + Qfthyroid + Qfbrain ) ;

  Qfdv = 24 * 60 * exp ( -1.8 + 0.91 * sqrt ( (*pdTime) / 7 ) ) / 1000 ;

  fBW = 0.00003107 * exp ( 0.8137 / 0.06458 * ( 1 - exp ( -0.06458 * (*pdTime) / 7 ) ) ) ;

  Vplacenta = 0.317 * pow ( fBW , 0.582 ) ;

  Vrest = BW - ( fBW + Vplacenta ) - ( Vthyroid + Vkidney + Vgut + Vliver + Vlung + Vart + Vven ) ;

  Vfart = 0.16 * 80 / 1000 * fBW ;

  Vfven = 0.595 * 80 / 1000 * fBW ;

  Vfkidney = 0.01082 * exp ( 0.4673 / 0.052 * ( 1 - exp ( -0.052 * (*pdTime) / 7 ) ) ) / 1000 ;

  Vfthyroid = 0.002091 * exp ( 0.3137 / 0.0367 * ( 1 - exp ( -0.0367 * (*pdTime) / 7 ) ) ) / 1000 ;

  Vfliver = 0.1528 * exp ( 0.3478 / 0.04058 * ( 1 - exp ( -0.04058 * (*pdTime) / 7 ) ) ) / 1000 ;

  Vfbrain = 0.2912 * exp ( 0.4244 / 0.05078 * ( 1 - exp ( -0.05078 * (*pdTime) / 7 ) ) ) / 1000 ;

  Vfgut = Vfgutc * fBW ;

  Vflung = 0.02611 * exp ( 0.5898 / 0.07125 * ( 1 - exp ( -0.07125 * (*pdTime) / 7 ) ) ) / 1000 ;

  Vfrest = fBW - ( Vfart + Vfven + Vfkidney + Vfthyroid + Vfliver + Vfbrain + Vfgut + Vflung ) ;

  yout[ID_Cgut] = y[ID_Agut] / Vgut ;

  yout[ID_Cliver] = y[ID_Aliver] / Vliver ;

  yout[ID_Cven] = y[ID_Aven] / Vven ;

  yout[ID_Clung] = y[ID_Alung] / Vlung ;

  yout[ID_Cart] = y[ID_Aart] / Vart ;

  yout[ID_Crest] = y[ID_Arest] / Vrest ;

  yout[ID_Ckidney] = y[ID_Akidney] / Vkidney ;

  yout[ID_Cserum] = y[ID_Aven] / Vven / Ratioblood2plasma ;

  yout[ID_Aserum] = y[ID_Aven] / Ratioblood2plasma * ( 1 - hematocrit ) ;

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

  yout[ID_Cfserum] = y[ID_Afven] / Vfven / Ratioblood2plasma ;

  yout[ID_Afserum] = y[ID_Afven] / Ratioblood2plasma * ( 1 - hematocrit ) ;

  ydot[ID_Agutlumen] = - kgutabs * y[ID_Agutlumen] ;

  ydot[ID_Agut] = kgutabs * y[ID_Agutlumen] + Qgut * ( y[ID_Aart] / Vart - y[ID_Agut] / Vgut * Ratioblood2plasma / Kgut2plasma / Fraction_unbound_plasma ) ;

  ydot[ID_Aliver] = Qliver * y[ID_Aart] / Vart + Qgut * y[ID_Agut] / Vgut * Ratioblood2plasma / Kgut2plasma / Fraction_unbound_plasma - ( Qliver + Qgut ) * y[ID_Aliver] / Vliver / Kliver2plasma / Fraction_unbound_plasma * Ratioblood2plasma - CLmetabolism * y[ID_Aliver] / Vliver / Kliver2plasma ;

  ydot[ID_Aven] = ( ( Qliver + Qgut ) * y[ID_Aliver] / Vliver / Kliver2plasma + Qkidney * y[ID_Akidney] / Vkidney / Kkidney2plasma + Qrest * y[ID_Arest] / Vrest / Krest2plasma + Qthyroid * y[ID_Athyroid] / Vthyroid / Kthyroid2plasma + Qplacenta * y[ID_Aplacenta] / Vplacenta / Kplacenta2plasma ) * Ratioblood2plasma / Fraction_unbound_plasma - Qcardiac * y[ID_Aven] / Vven ;

  ydot[ID_Alung] = Qcardiac * ( y[ID_Aven] / Vven - y[ID_Alung] / Vlung * Ratioblood2plasma / Klung2plasma / Fraction_unbound_plasma ) ;

  ydot[ID_Aart] = Qcardiac * ( y[ID_Alung] / Vlung * Ratioblood2plasma / Klung2plasma / Fraction_unbound_plasma - y[ID_Aart] / Vart ) ;

  ydot[ID_Arest] = Qrest * ( y[ID_Aart] / Vart - y[ID_Arest] / Vrest * Ratioblood2plasma / Krest2plasma / Fraction_unbound_plasma ) ;

  ydot[ID_Akidney] = Qkidney * y[ID_Aart] / Vart - Qkidney * y[ID_Akidney] / Vkidney / Kkidney2plasma * Ratioblood2plasma / Fraction_unbound_plasma - Qgfr * y[ID_Akidney] / Vkidney / Kkidney2plasma ;

  ydot[ID_Atubules] = Qgfr * y[ID_Akidney] / Vkidney / Kkidney2plasma ;

  ydot[ID_Ametabolized] = CLmetabolism * y[ID_Aliver] / Vliver / Kliver2plasma ;

  ydot[ID_AUC] = y[ID_Aven] / Vven / Ratioblood2plasma ;

  ydot[ID_Athyroid] = Qthyroid * ( y[ID_Aart] / Vart - y[ID_Athyroid] / Vthyroid / Kthyroid2plasma / Fraction_unbound_plasma * Ratioblood2plasma ) ;

  ydot[ID_Aplacenta] = Qplacenta * y[ID_Aart] / Vart - Qplacenta * ( y[ID_Aplacenta] / Vplacenta / Kplacenta2plasma / Fraction_unbound_plasma * Ratioblood2plasma ) - Qfplacenta * ( y[ID_Aplacenta] / Vplacenta / Kfplacenta2plasma / Fraction_unbound_plasma * Ratioblood2plasma ) + Qfplacenta * y[ID_Afart] / Vfart ;

  ydot[ID_Afart] = Qflung * y[ID_Aflung] / Vflung * Ratioblood2plasma / Kflung2plasma / Fraction_unbound_plasma + Qfbypass * y[ID_Afven] / Vfven - Qfcardiac * y[ID_Afart] / Vfart ;

  ydot[ID_Afven] = ( ( Qfliver + Qfgut + Qfplacenta - Qfdv ) * y[ID_Afliver] / Vfliver / Kfliver2plasma + Qfdv * y[ID_Aplacenta] / Vplacenta / Kfplacenta2plasma + Qfthyroid * y[ID_Afthyroid] / Vfthyroid / Kfthyroid2plasma + Qfrest * y[ID_Afrest] / Vfrest / Kfrest2plasma + Qfkidney * y[ID_Afkidney] / Vfkidney / Kfkidney2plasma + Qfbrain * y[ID_Afbrain] / Vfbrain / Kfbrain2plasma ) * Ratioblood2plasma / Fraction_unbound_plasma - Qfcardiac * y[ID_Afven] / Vfven ;

  ydot[ID_Afkidney] = Qfkidney * ( y[ID_Afart] / Vfart - y[ID_Afkidney] / Vfkidney / Kfkidney2plasma * Ratioblood2plasma / Fraction_unbound_plasma ) ;

  ydot[ID_Afrest] = Qfrest * ( y[ID_Afart] / Vfart - y[ID_Afrest] / Vfrest / Kfrest2plasma * Ratioblood2plasma / Fraction_unbound_plasma ) ;

  ydot[ID_Afthyroid] = Qfthyroid * ( y[ID_Afart] / Vfart - y[ID_Afthyroid] / Vfthyroid / Kfthyroid2plasma * Ratioblood2plasma / Fraction_unbound_plasma ) ;

  ydot[ID_Afliver] = Qfliver * y[ID_Afart] / Vfart + Qfgut * y[ID_Afgut] / Vfgut * Ratioblood2plasma / Kfgut2plasma / Fraction_unbound_plasma + ( Qfplacenta - Qfdv ) * y[ID_Aplacenta] / Vplacenta / Kfplacenta2plasma / Fraction_unbound_plasma * Ratioblood2plasma - ( Qfliver + Qfgut + Qfplacenta - Qfdv ) * y[ID_Afliver] / Vfliver / Kfliver2plasma * Ratioblood2plasma / Fraction_unbound_plasma ;

  ydot[ID_Aflung] = Qflung * ( y[ID_Afven] / Vfven - y[ID_Aflung] / Vflung * Ratioblood2plasma / Kflung2plasma / Fraction_unbound_plasma ) ;

  ydot[ID_Afgut] = Qfgut * ( y[ID_Afart] / Vfart - y[ID_Afgut] / Vfgut * Ratioblood2plasma / Kfgut2plasma / Fraction_unbound_plasma ) ;

  ydot[ID_Afbrain] = Qfbrain * ( y[ID_Afart] / Vfart - y[ID_Afbrain] / Vfbrain * Ratioblood2plasma / Kfbrain2plasma / Fraction_unbound_plasma ) ;

  ydot[ID_fAUC] = y[ID_Afven] / Vfven / Ratioblood2plasma ;

} /* derivs */


/*----- Jacobian calculations: */
void jacfetus (int *neq, double *t, double *y, int *ml, int *mu, double *pd, int *nrowpd, double *yout, int *ip)
{

} /* jac */


/*----- Events calculations: */
void eventfetus (int *n, double *t, double *y)
{

} /* event */

/*----- Roots calculations: */
void rootfetus (int *neq, double *t, double *y, int *ng, double *gout, double *out, int *ip)
{

} /* root */

