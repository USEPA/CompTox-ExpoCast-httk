/* fetalPBTK2.c for R deSolve package
   ___________________________________________________

   Model File:  fetalPBTK.model

   Date:  Fri Jun 07 15:10:38 2019

   Created by:  "mod v5.6.5"
    -- a model preprocessor by Don Maszle
   ___________________________________________________

   Copyright (c) 1993-2015 Free Software Foundation, Inc.

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

   23 Outputs:
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
    "Cfplasma",

   0 Inputs:

   37 Parameters:
     pre_pregnant_BW = 0,
     Clmetabolismc = 0.0,
     kgutabs = 0,
     Kkidney2pu = 0,
     Kliver2pu = 0,
     Krbc2pu = 0,
     Kadipose2pu = 0,
     Krest2pu = 0,
     Klung2pu = 0,
     Kgut2pu = 0,
     Kthyroid2pu = 0,
     Kplacenta2pu = 0,
     Kfplacenta2pu = 0,
     Kfkidney2pu = 0,
     Kfrest2pu = 0,
     Kfthyroid2pu = 0,
     Kfliver2pu = 0,
     Kflung2pu = 0,
     Kfgut2pu = 0,
     Kfbrain2pu = 0,
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
     Fraction_unbound_plasma = 0,
     Rblood2plasma = 0.0,
     Clmetabolism = 0.0,
*/

#include <R.h>

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
#define ID_Cplacenta 0x0000b
#define ID_Cfliver 0x0000c
#define ID_Cfven 0x0000d
#define ID_Cfart 0x0000e
#define ID_Cfgut 0x0000f
#define ID_Cflung 0x00010
#define ID_Cfrest 0x00011
#define ID_Cfthyroid 0x00012
#define ID_Cfkidney 0x00013
#define ID_Cfbrain 0x00014
#define ID_Afplasma 0x00015
#define ID_Cfplasma 0x00016

/* Parameters */
static double parms[37];

#define pre_pregnant_BW parms[0]
#define Clmetabolismc parms[1]
#define kgutabs parms[2]
#define Kkidney2pu parms[3]
#define Kliver2pu parms[4]
#define Krbc2pu parms[5]
#define Kadipose2pu parms[6]
#define Krest2pu parms[7]
#define Klung2pu parms[8]
#define Kgut2pu parms[9]
#define Kthyroid2pu parms[10]
#define Kplacenta2pu parms[11]
#define Kfplacenta2pu parms[12]
#define Kfkidney2pu parms[13]
#define Kfrest2pu parms[14]
#define Kfthyroid2pu parms[15]
#define Kfliver2pu parms[16]
#define Kflung2pu parms[17]
#define Kfgut2pu parms[18]
#define Kfbrain2pu parms[19]
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
#define Fraction_unbound_plasma parms[34]
#define Rblood2plasma parms[35]
#define Clmetabolism parms[36]

/* Forcing (Input) functions */
static double forc[0];


/*----- Initializers */
void initmodfetalpbtk (void (* odeparms)(int *, double *))
{
  int N=37;
  odeparms(&N, parms);
}

void initforcfetalpbtk (void (* odeforcs)(int *, double *))
{
  int N=0;
  odeforcs(&N, forc);
}


void getParmsfetalpbtk (double *inParms, double *out, int *nout) {
/*----- Model scaling */

  int i;

  for (i = 0; i < *nout; i++) {
    parms[i] = inParms[i];
  }


  kgutabs = kgutabs * 24 ;
  Clmetabolism = Clmetabolismc * 24 * pre_pregnant_BW ;
  Vart = Vartc * pre_pregnant_BW ;
  Vgut = Vgutc * pre_pregnant_BW ;
  Vkidney = Vkidneyc * pre_pregnant_BW ;
  Vliver = Vliverc * pre_pregnant_BW ;
  Vlung = Vlungc * pre_pregnant_BW ;
  Vven = Vvenc * pre_pregnant_BW ;
  Vthyroid = Vthyroidc * pre_pregnant_BW ;

  for (i = 0; i < *nout; i++) {
    out[i] = parms[i];
  }
  }
/*----- Dynamics section */

void derivsfetalpbtk (int *neq, double *pdTime, double *y, double *ydot, double *yout, int *ip)
{
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

  BW = 61.103 - 0.010614 * ( (*pdTime) / 7.0 ) + 0.029161 * pow ( ( (*pdTime) / 7.0 ) , 2 ) - 5.0203 * pow ( 10 , -4 ) * pow ( ( (*pdTime) / 7.0 ) , 3 ) ;

  Wadipose = 17.067 + 0.14937 * ( (*pdTime) / 7.0 ) ;

  Wfkidney = 0.001 * 6.3327 * pow ( 10 , -5 ) * exp ( 1.0409 / 0.076435 * ( 1 - exp ( -0.051995 * ( (*pdTime) / 7.0 ) ) ) ) ;

  Wfthyroid = 0.001 * 0.0038483 * exp ( 0.30799 / 0.039800 * ( 1 - exp ( -0.039800 * ( (*pdTime) / 7.0 ) ) ) ) ;

  Wfliver = 0.001 * 0.0074774 * exp ( 0.65856 / 0.061662 * ( 1 - exp ( -0.061662 * ( (*pdTime) / 7.0 ) ) ) ) ;

  Wfbrain = 0.001 * 0.01574 * exp ( 0.70707 / 0.064827 * ( 1 - exp ( -0.064827 * ( (*pdTime) / 7.0 ) ) ) ) ;

  Wfgut = 0.001 * 8.1828 * pow ( 10 , -4 ) * exp ( 0.65028 / 0.047724 * ( 1 - exp ( -0.047724 * ( (*pdTime) / 7.0 ) ) ) ) ;

  Wflung = 0.001 * 3.0454 * pow ( 10 , -4 ) * exp ( 1.0667 / 0.084604 * ( 1 - exp ( -0.084604 * ( (*pdTime) / 7.0 ) ) ) ) ;

  hematocrit = 39.192 - 0.10562 * ( (*pdTime) / 7.0 ) - 7.1045 * pow ( 10 , -4 ) * pow ( ( (*pdTime) / 7.0 ) , 2 ) ;

  fhematocrit = 4.5061 * ( (*pdTime) / 7.0 ) - 0.18487 * pow ( ( (*pdTime) / 7.0 ) , 2 ) + 0.0026766 * pow ( ( (*pdTime) / 7.0 ) , 3 ) ;

  fBW = 0.001 * 0.0018282 * exp ( 1.1735 / 0.077577 * ( 1 - exp ( -0.077577 * ( (*pdTime) / 7.0 ) ) ) ) ;

  Vplacenta = 0.001 * ( -1.7646 * ( (*pdTime) / 7.0 ) + 0.91775 * pow ( ( (*pdTime) / 7.0 ) , 2 ) - 0.011543 * pow ( ( (*pdTime) / 7.0 ) , 3 ) ) ;

  Vamnf = 0.001 * 822.34 / ( 1 + exp ( -0.26988 * ( ( (*pdTime) / 7.0 ) - 20.150 ) ) ) ;

  Vplasma = 1.2406 / ( 1 + exp ( -0.31338 * ( ( (*pdTime) / 7.0 ) - 17.813 ) ) ) + 2.4958 ;

  Vrbcs = hematocrit / ( 1 - hematocrit ) * Vplasma ;

  Vadipose = 1 / 0.950 * Wadipose ;

  Vffmx = 1 / 1.1 * ( BW - Wadipose - ( fBW + 1.02 * Vplacenta + 1.01 * Vamnf ) ) ;

  Vallx = Vplasma + Vrbcs + Vthyroid + Vkidney + Vgut + Vliver + Vlung ;

  Vrest = Vffmx - Vallx ;

  Vfart = 0.001 * 0.16 * 80 * fBW ;

  Vfven = 0.001 * 0.595 * 80 * fBW ;

  Vfkidney = 1 / 1.05 * Wfkidney ;

  Vfthyroid = 1 / 1.05 * Wfthyroid ;

  Vfliver = 1 / 1.05 * Wfliver ;

  Vfbrain = 1 / 1.04 * Wfbrain ;

  Vfgut = 1 / 1.045 * Wfgut ;

  Vflung = 1 / 1.05 * Wflung ;

  Vfrest = fBW - ( Vfart + Vfven + Vfbrain + Vfkidney + Vfthyroid + Vfliver + Vfgut + Vflung ) ;

  Qcardiac = 24 * ( 301.78 + 3.2512 * ( (*pdTime) / 7.0 ) + 0.15947 * pow ( ( (*pdTime) / 7.0 ) , 2 ) - 0.0047059 * pow ( ( (*pdTime) / 7.0 ) , 3 ) );

  Qgut = 0.01 * ( 17.0 + ( 12.5 - 17.0 ) / 40.0 * ( (*pdTime) / 7.0 ) ) * Qcardiac ;

  Qkidney = 24 * ( 53.248 + 3.6447 * ( (*pdTime) / 7.0 ) - 0.15357 * pow ( ( (*pdTime) / 7.0 ) , 2 ) + 0.0016968 * pow ( ( (*pdTime) / 7.0 ) , 3 ) ) ;

  Qliver = 0.01 * ( 27.0 + ( 20.0 - 27.0 ) / 40.0 * ( (*pdTime) / 7.0 ) ) * Qcardiac ;

  Qthyroid = 0.01 * ( 1.5 + ( 1.1 - 1.5 ) / 40.0 * ( (*pdTime) / 7.0 ) ) * Qcardiac ;

  Qplacenta = 24 * 0.059176 * 1000 * Vplacenta ; 

  Qadipose = 0.01 * ( 8.5 + ( 7.8 - 8.5 ) / 40.0 * ( (*pdTime) / 7.0 ) ) * Qcardiac ;

  Qrest = Qcardiac - ( Qgut + Qkidney + Qliver + Qthyroid + Qplacenta + Qadipose ) ;

  Qgfr = 60 * 24 * 0.001 * ( 113.73 + 3.5784 * ( (*pdTime) / 7.0 ) - 0.067272 * pow ( ( (*pdTime) / 7.0 ) , 2 ) ) ;

  Qfrvtl = 60 * 24 * 0.001 * 2466.5 / ( 1 + exp ( -0.14837 * ( ( (*pdTime) / 7.0 ) - 43.108 ) ) ) ;

  Qflvtl = 60 * 24 * 0.001 * 506.30 / ( 1 + exp ( -0.21916 * ( ( (*pdTime) / 7.0 ) - 30.231 ) ) ) ;

  Qfda = 60 * 24 * 0.001 * 1125.3 / ( 1 + exp ( -0.18031 * ( ( (*pdTime) / 7.0 ) - 35.939 ) ) ) ;

  Qfartb = Qflvtl + Qfda ;

  Qfcardiac = Qfartb ;

  Qflung = Qfrvtl - Qfda ;

  Qfplacenta = 60 * 24 * 0.001 * 262.20 / ( 1 + exp ( -0.22183 * ( ( (*pdTime) / 7.0 ) - 28.784 ) ) ) ;

  Qfdv = 60 * 24 * 0.001 * 1.892 * exp ( 0.098249 / 0.0064374 * ( 1 - exp ( -0.0064374 * ( (*pdTime) / 7.0 ) ) ) ) ;

  Qfgut = 6.8 / 75.0 * ( 1 - Qfplacenta / Qfartb ) * Qfartb ;

  Qfkidney = 5.4 / 75.0 * ( 1 - Qfplacenta / Qfartb ) * Qfartb ;

  Qfbrain = 14.3 / 75.0 * ( 1 - Qfplacenta / Qfartb ) * Qfartb ;

  Qfliver = 6.5 / 54.0 * ( 1 - 26.5 / 75.0 ) * ( 1 - Qfplacenta / Qfartb ) * Qfartb ;

  Qfthyroid = 1.5 / 54.0 * ( 1 - 26.5 / 75.0 ) * ( 1 - Qfplacenta / Qfartb ) * Qfartb ;

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

  yout[ID_Cplasma] = y[ID_Aven] / Vven / Rblood2plasma ;

  yout[ID_Aplasma] = y[ID_Aven] / Rblood2plasma * ( 1 - hematocrit ) ;

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

  yout[ID_Cfplasma] = y[ID_Afven] / Vfven / Rblood2plasma ;

  yout[ID_Afplasma] = y[ID_Afven] / Rblood2plasma * ( 1 - fhematocrit ) ;

  ydot[ID_Agutlumen] = - kgutabs * y[ID_Agutlumen] ;

  ydot[ID_Agut] = kgutabs * y[ID_Agutlumen] + Qgut * ( y[ID_Aart] / Vart - y[ID_Agut] / Vgut * Rblood2plasma / Kgut2pu / Fraction_unbound_plasma ) ;

  ydot[ID_Aliver] = Qliver * y[ID_Aart] / Vart + Qgut * y[ID_Agut] / Vgut * Rblood2plasma / Kgut2pu / Fraction_unbound_plasma - ( Qliver + Qgut ) * y[ID_Aliver] / Vliver / Kliver2pu / Fraction_unbound_plasma * Rblood2plasma - Clmetabolism * y[ID_Aliver] / Vliver / Kliver2pu ;

  ydot[ID_Aven] = ( ( Qliver + Qgut ) * y[ID_Aliver] / Vliver / Kliver2pu + Qkidney * y[ID_Akidney] / Vkidney / Kkidney2pu + Qadipose * y[ID_Aadipose] / Vadipose / Kadipose2pu + Qrest * y[ID_Arest] / Vrest / Krest2pu + Qthyroid * y[ID_Athyroid] / Vthyroid / Kthyroid2pu + Qplacenta * y[ID_Aplacenta] / Vplacenta / Kplacenta2pu ) * Rblood2plasma / Fraction_unbound_plasma - Qcardiac * y[ID_Aven] / Vven ;

  ydot[ID_Alung] = Qcardiac * ( y[ID_Aven] / Vven - y[ID_Alung] / Vlung * Rblood2plasma / Klung2pu / Fraction_unbound_plasma ) ;

  ydot[ID_Aart] = Qcardiac * ( y[ID_Alung] / Vlung * Rblood2plasma / Klung2pu / Fraction_unbound_plasma - y[ID_Aart] / Vart ) ;

  ydot[ID_Aadipose] = Qadipose * ( y[ID_Aart] / Vart - y[ID_Aadipose] / Vadipose * Rblood2plasma / Kadipose2pu / Fraction_unbound_plasma ) ;

  ydot[ID_Arest] = Qrest * ( y[ID_Aart] / Vart - y[ID_Arest] / Vrest * Rblood2plasma / Krest2pu / Fraction_unbound_plasma ) ;

  ydot[ID_Akidney] = Qkidney * y[ID_Aart] / Vart - Qkidney * y[ID_Akidney] / Vkidney / Kkidney2pu * Rblood2plasma / Fraction_unbound_plasma - Qgfr * y[ID_Akidney] / Vkidney / Kkidney2pu ;

  ydot[ID_Atubules] = Qgfr * y[ID_Akidney] / Vkidney / Kkidney2pu ;

  ydot[ID_Ametabolized] = Clmetabolism * y[ID_Aliver] / Vliver / Kliver2pu ;

  ydot[ID_AUC] = y[ID_Aven] / Vven / Rblood2plasma ;

  ydot[ID_Athyroid] = Qthyroid * ( y[ID_Aart] / Vart - y[ID_Athyroid] / Vthyroid / Kthyroid2pu / Fraction_unbound_plasma * Rblood2plasma ) ;

  ydot[ID_Aplacenta] = Qplacenta * y[ID_Aart] / Vart - Qplacenta * ( y[ID_Aplacenta] / Vplacenta / Kplacenta2pu / Fraction_unbound_plasma * Rblood2plasma ) - Qfplacenta * ( y[ID_Aplacenta] / Vplacenta / Kfplacenta2pu / Fraction_unbound_plasma * Rblood2plasma ) + Qfplacenta * y[ID_Afart] / Vfart ;

  ydot[ID_Afart] = Qflung * y[ID_Aflung] / Vflung * Rblood2plasma / Kflung2pu / Fraction_unbound_plasma + Qfbypass * y[ID_Afven] / Vfven - Qfcardiac * y[ID_Afart] / Vfart ;

  ydot[ID_Afven] = ( ( Qfliver + Qfgut + Qfplacenta - Qfdv ) * y[ID_Afliver] / Vfliver / Kfliver2pu + Qfdv * y[ID_Aplacenta] / Vplacenta / Kfplacenta2pu + Qfthyroid * y[ID_Afthyroid] / Vfthyroid / Kfthyroid2pu + Qfrest * y[ID_Afrest] / Vfrest / Kfrest2pu + Qfkidney * y[ID_Afkidney] / Vfkidney / Kfkidney2pu + Qfbrain * y[ID_Afbrain] / Vfbrain / Kfbrain2pu ) * Rblood2plasma / Fraction_unbound_plasma - Qfcardiac * y[ID_Afven] / Vfven ;

  ydot[ID_Afkidney] = Qfkidney * ( y[ID_Afart] / Vfart - y[ID_Afkidney] / Vfkidney / Kfkidney2pu * Rblood2plasma / Fraction_unbound_plasma ) ;

  ydot[ID_Afrest] = Qfrest * ( y[ID_Afart] / Vfart - y[ID_Afrest] / Vfrest / Kfrest2pu * Rblood2plasma / Fraction_unbound_plasma ) ;

  ydot[ID_Afthyroid] = Qfthyroid * ( y[ID_Afart] / Vfart - y[ID_Afthyroid] / Vfthyroid / Kfthyroid2pu * Rblood2plasma / Fraction_unbound_plasma ) ;

  ydot[ID_Afliver] = Qfliver * y[ID_Afart] / Vfart + Qfgut * y[ID_Afgut] / Vfgut * Rblood2plasma / Kfgut2pu / Fraction_unbound_plasma + ( Qfplacenta - Qfdv ) * y[ID_Aplacenta] / Vplacenta / Kfplacenta2pu / Fraction_unbound_plasma * Rblood2plasma - ( Qfliver + Qfgut + Qfplacenta - Qfdv ) * y[ID_Afliver] / Vfliver / Kfliver2pu * Rblood2plasma / Fraction_unbound_plasma ;

  ydot[ID_Aflung] = Qflung * ( y[ID_Afven] / Vfven - y[ID_Aflung] / Vflung * Rblood2plasma / Kflung2pu / Fraction_unbound_plasma ) ;

  ydot[ID_Afgut] = Qfgut * ( y[ID_Afart] / Vfart - y[ID_Afgut] / Vfgut * Rblood2plasma / Kfgut2pu / Fraction_unbound_plasma ) ;

  ydot[ID_Afbrain] = Qfbrain * ( y[ID_Afart] / Vfart - y[ID_Afbrain] / Vfbrain * Rblood2plasma / Kfbrain2pu / Fraction_unbound_plasma ) ;

  ydot[ID_fAUC] = y[ID_Afven] / Vfven / Rblood2plasma ;

} /* derivs */


/*----- Jacobian calculations: */
void jacfetalpbtk (int *neq, double *t, double *y, int *ml, int *mu, double *pd, int *nrowpd, double *yout, int *ip)
{

} /* jac */


/*----- Events calculations: */
void eventfetalpbtk (int *n, double *t, double *y)
{

} /* event */

/*----- Roots calculations: */
void rootfetalpbtk (int *neq, double *t, double *y, int *ng, double *gout, double *out, int *ip)
{

} /* root */

