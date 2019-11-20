/* model_gas_pbtk.c for R deSolve package
   ___________________________________________________

   Model File:  inhalation.model

   Date:  Mon Oct 22 11:15:43 2018

   Created by:  "C:/Users/mlinakis/Desktop/RLibrary/MCSIMU~1/mod/mod.exe v5.6.5"
    -- a model preprocessor by Don Maszle
   ___________________________________________________

   Copyright (c) 1993-2015 Free Software Foundation, Inc.

   Model calculations for compartmental model:

   12 States:
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
     Amuc = 0.0,

   13 Outputs:
    "Cgut",
    "Cliver",
    "Cven",
    "Clung",
    "Cart",
    "Crest",
    "Ckidney",
    "Cplasma",
    "Aplasma",
    "Calv",
    "Cendexh",
    "Cmixexh",
    "Cmuc",

   1 Input:
     Cinh (forcing function)

   51 Parameters:
     BW = 0.0,
     Clmetabolismc = 0.0,
     vmax = 0.0,
     km = 0.0,
     hematocrit = 0.0,
     kgutabs = 0.0,
     Kkidney2pu = 0.0,
     Kliver2pu = 0.0,
     Krest2pu = 0.0,
     Kgut2pu = 0.0,
     Klung2pu = 0.0,
     Qcardiacc = 0.0,
     Qgfrc = 0.0,
     Qgutf = 0.0,
     Qkidneyf = 0.0,
     Qliverf = 0.0,
     Qlungf = 0.0,
     Vartc = 0.0,
     Vgutc = 0.0,
     Vkidneyc = 0.0,
     Vliverc = 0.0,
     Vlungc = 0.0,
     Vrestc = 0.0,
     Vvenc = 0.0,
     Fraction_unbound_plasma = 0.0,
     Rblood2plasma = 0.0,
     Clmetabolism = 0.0,
     Qcardiac = 0.0,
     Qgfr = 0.0,
     Qgut = 0.0,
     Qkidney = 0.0,
     Qliver = 0.0,
     Qlung = 0.0,
     Qrest = 0.0,
     Vart = 0.0,
     Vgut = 0.0,
     Vkidney = 0.0,
     Vliver = 0.0,
     Vlung = 0.0,
     Vrest = 0.0,
     Vven = 0.0,
     Qalvc = 0.0,
     Qalv = 0.0,
     Kblood2air = 0.0,
     kUrtc = 0.0,
     kUrt = 0.0,
     Kmuc2air = 0.0,
     Vmucc = 0.0,
     Vmuc = 0.0,
     Vmax = 0.0,
     Km = 0.0,
*/

#include <R.h>

/* Model variables: States */
#define ID_Agutlumen 0x00000
#define ID_Agut 0x00001
#define ID_Aliver 0x00002
#define ID_Aven 0x00003
#define ID_Alung 0x00004
#define ID_Aart 0x00005
#define ID_Arest 0x00006
#define ID_Akidney 0x00007
#define ID_Atubules 0x00008
#define ID_Ametabolized 0x00009
#define ID_AUC 0x0000a
#define ID_Amuc 0x0000b

/* Model variables: Outputs */
#define ID_Cgut 0x00000
#define ID_Cliver 0x00001
#define ID_Cven 0x00002
#define ID_Clung 0x00003
#define ID_Cart 0x00004
#define ID_Crest 0x00005
#define ID_Ckidney 0x00006
#define ID_Cplasma 0x00007
#define ID_Aplasma 0x00008
#define ID_Calv 0x00009
#define ID_Cendexh 0x0000a
#define ID_Cmixexh 0x0000b
#define ID_Cmuc 0x0000c

/* Parameters */
static double parms[51];

#define BW parms[0]
#define Clmetabolismc parms[1]
#define vmax parms[2]
#define km parms[3]
#define hematocrit parms[4]
#define kgutabs parms[5]
#define Kkidney2pu parms[6]
#define Kliver2pu parms[7]
#define Krest2pu parms[8]
#define Kgut2pu parms[9]
#define Klung2pu parms[10]
#define Qcardiacc parms[11]
#define Qgfrc parms[12]
#define Qgutf parms[13]
#define Qkidneyf parms[14]
#define Qliverf parms[15]
#define Qlungf parms[16]
#define Vartc parms[17]
#define Vgutc parms[18]
#define Vkidneyc parms[19]
#define Vliverc parms[20]
#define Vlungc parms[21]
#define Vrestc parms[22]
#define Vvenc parms[23]
#define Fraction_unbound_plasma parms[24]
#define Rblood2plasma parms[25]
#define Clmetabolism parms[26]
#define Qcardiac parms[27]
#define Qgfr parms[28]
#define Qgut parms[29]
#define Qkidney parms[30]
#define Qliver parms[31]
#define Qlung parms[32]
#define Qrest parms[33]
#define Vart parms[34]
#define Vgut parms[35]
#define Vkidney parms[36]
#define Vliver parms[37]
#define Vlung parms[38]
#define Vrest parms[39]
#define Vven parms[40]
#define Qalvc parms[41]
#define Qalv parms[42]
#define Kblood2air parms[43]
#define kUrtc parms[44]
#define kUrt parms[45]
#define Kmuc2air parms[46]
#define Vmucc parms[47]
#define Vmuc parms[48]
#define Vmax parms[49]
#define Km parms[50]

/* Forcing (Input) functions */
static double forc[1];

#define Cinh forc[0]

/*----- Initializers */
void initmod_gas_pbtk (void (* odeparms)(int *, double *))
{
  int N=51;
  odeparms(&N, parms);
}

void initforc_gas_pbtk (void (* odeforcs)(int *, double *))
{
  int N=1;
  odeforcs(&N, forc);
}


void getParms_gas_pbtk (double *inParms, double *out, int *nout) {
/*----- Model scaling */

  int i;

  for (i = 0; i < *nout; i++) {
    parms[i] = inParms[i];
  }


  kgutabs = kgutabs * 24 ;
  Clmetabolism = Clmetabolismc * 24 * BW ;
  Qcardiac = Qcardiacc * 24 * pow ( BW , 0.75 ) ;
  Qgfr = Qgfrc * pow ( BW , 0.75 ) * 24 ;
  Qgut = Qcardiac * Qgutf ;
  Qkidney = Qcardiac * Qkidneyf ;
  Qliver = Qcardiac * Qliverf ;
  Qlung = Qcardiac * Qlungf ;
  Qrest = Qcardiac - ( Qgut + Qkidney + Qliver + Qlung ) ;
  Vart = Vartc * BW ;
  Vgut = Vgutc * BW ;
  Vkidney = Vkidneyc * BW ;
  Vliver = Vliverc * BW ;
  Vlung = Vlungc * BW ;
  Vrest = Vrestc * BW ;
  Vven = Vvenc * BW ;
  Qalv = Qalvc * 24 * pow ( BW , 0.75 ) ;
  kUrt = kUrtc * pow ( BW , 0.75 ) * 24 ;
  Vmuc = Vmucc * BW ;
  Vmax = vmax * 60 * 24 ;
  Km = km ;

  for (i = 0; i < *nout; i++) {
    out[i] = parms[i];
  }
  }
/*----- Dynamics section */

void derivs_gas_pbtk (int *neq, double *pdTime, double *y, double *ydot, double *yout, int *ip)
{

  yout[ID_Cgut] = y[ID_Agut] / Vgut ;

  yout[ID_Cliver] = y[ID_Aliver] / Vliver ;

  yout[ID_Cven] = y[ID_Aven] / Vven ;

  yout[ID_Clung] = y[ID_Alung] / Vlung ;

  yout[ID_Cart] = y[ID_Aart] / Vart ;

  yout[ID_Crest] = y[ID_Arest] / Vrest ;

  yout[ID_Ckidney] = y[ID_Akidney] / Vkidney ;

  yout[ID_Cplasma] = y[ID_Aven] / Vven / Rblood2plasma ;

  yout[ID_Aplasma] = y[ID_Aven] / Rblood2plasma * ( 1 - hematocrit ) ;

  yout[ID_Calv] = yout[ID_Cart] / Kblood2air ;

  yout[ID_Cendexh] = ( ( Qalv * yout[ID_Calv] ) + kUrt * ( ( yout[ID_Cmuc] / Kmuc2air ) - yout[ID_Calv] ) ) / Qalv ;

  yout[ID_Cmixexh] = 0.7 * yout[ID_Cendexh] + 0.3 * Cinh ;

  yout[ID_Cmuc] = y[ID_Amuc] / Vmuc ;

  ydot[ID_Agutlumen] = - kgutabs * y[ID_Agutlumen] ;

  ydot[ID_Agut] = kgutabs * y[ID_Agutlumen] + Qgut * ( yout[ID_Cart] - yout[ID_Cgut] * Rblood2plasma / Kgut2pu / Fraction_unbound_plasma ) ;

  ydot[ID_Aliver] = Qliver * yout[ID_Cart] + Qgut * yout[ID_Cgut] * Rblood2plasma / Kgut2pu / Fraction_unbound_plasma - ( Qliver + Qgut ) * yout[ID_Cliver] / Kliver2pu / Fraction_unbound_plasma * Rblood2plasma - Clmetabolism * yout[ID_Cliver] / Kliver2pu / Fraction_unbound_plasma * Rblood2plasma - Vmax * yout[ID_Cliver] / Kliver2pu / ( Km + yout[ID_Cliver] / Kliver2pu ) ;

  ydot[ID_Aven] = ( ( Qliver + Qgut ) * yout[ID_Cliver] / Kliver2pu + Qkidney * yout[ID_Ckidney] / Kkidney2pu + Qrest * yout[ID_Crest] / Krest2pu + Qlung * yout[ID_Clung] / Klung2pu ) * Rblood2plasma / Fraction_unbound_plasma - Qcardiac * yout[ID_Cven] ;

  ydot[ID_Alung] = Qlung * ( yout[ID_Cart] - yout[ID_Clung] * Rblood2plasma / Klung2pu / Fraction_unbound_plasma ) ;

  ydot[ID_Aart] = ( Qcardiac * ( yout[ID_Cven] - yout[ID_Cart] ) ) + ( Qalv * ( Cinh - yout[ID_Calv] ) ) - ( kUrt * ( Cinh - ( yout[ID_Cmuc] / Kmuc2air ) ) ) ;

  ydot[ID_Arest] = Qrest * ( yout[ID_Cart] - yout[ID_Crest] * Rblood2plasma / Krest2pu / Fraction_unbound_plasma ) ;

  ydot[ID_Akidney] = Qkidney * yout[ID_Cart] - Qkidney * yout[ID_Ckidney] / Kkidney2pu * Rblood2plasma / Fraction_unbound_plasma - Qgfr * yout[ID_Ckidney] / Kkidney2pu ;

  ydot[ID_Atubules] = Qgfr * yout[ID_Ckidney] / Kkidney2pu ;

  ydot[ID_Ametabolized] = Clmetabolism * yout[ID_Cliver] / Kliver2pu / Fraction_unbound_plasma * Rblood2plasma + Vmax * yout[ID_Cliver] / Kliver2pu / ( Km + yout[ID_Cliver] / Kliver2pu ) ;

  ydot[ID_AUC] = yout[ID_Cven] / Rblood2plasma ;

  ydot[ID_Amuc] = ( kUrt * ( Cinh - ( yout[ID_Cmuc] / Kmuc2air ) ) ) - ( kUrt * ( ( yout[ID_Cmuc] / Kmuc2air ) - yout[ID_Calv] ) ) ;

} /* derivs */


/*----- Jacobian calculations: */
void jac_gas_pbtk (int *neq, double *t, double *y, int *ml, int *mu, double *pd, int *nrowpd, double *yout, int *ip)
{

} /* jac */


/*----- Events calculations: */
void event_gas_pbtk (int *n, double *t, double *y)
{

} /* event */

/*----- Roots calculations: */
void root_gas_pbtk (int *neq, double *t, double *y, int *ng, double *gout, double *out, int *ip)
{

} /* root */

