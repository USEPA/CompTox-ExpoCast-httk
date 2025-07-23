/* skin_mm.c for R deSolve package
   ___________________________________________________

   Model File:  skin_mm.model

   Date:  Thu May 24 18:00:27 2018

   Created by:  "mod v5.5.0"
    -- a model preprocessor by Don Maszle
   ___________________________________________________

   Copyright (c) 1993-2013 Free Software Foundation, Inc.

   Model calculations for compartmental model:

   14 States:
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
     Askinexposed = 0.0,
     Askin = 0.0,
     Amedia = 0.0,

   12 Outputs:
    "Cgut",
    "Cliver",
    "Cven",
    "Clung",
    "Cart",
    "Crest",
    "Ckidney",
    "Cplasma",
    "Aplasma",
    "Cskinexposed",
    "Cskin",
    "Cmedia",

   2 Inputs:
     forcing (forcing function)
     forcing_switch (forcing function)

   51 Parameters:
     depth = 0,
     V0 = 0,
     Fskinexposed = 0,
     totalSA = 0,
     SA = 0,
     Kp = 0,
     Kskin2media = 0,
     BW = 70,
     Vmax = 0,
     Km = 1,
     hematocrit = 0.44,
     kgutabs = 1,
     Kkidney2pu = 0,
     Kliver2pu = 0,
     Krest2pu = 0,
     Kgut2pu = 0,
     Klung2pu = 0,
     Kskin2pu = 0,
     Qcardiacc = 4.8,
     Qgfrc = 0.108,
     Qskinf = 0,
     Qskin = 0,
     Qskinexposed = 0,
     Qgutf = 0,
     Qkidneyf = 0,
     Qliverf = 0,
     Vartc = 0,
     Vgutc = 0,
     Vkidneyc = 0,
     Vliverc = 0,
     Vlungc = 0,
     Vrestc = 0,
     Vvenc = 0,
     Vskinc = 0,
     Vskin = 0,
     Vskinexposed = 0,
     Fraction_unbound_plasma = 0.0,
     Rblood2plasma = 0.0,
     Qcardiac = 0.0,
     Qgfr = 0.0,
     Qgut = 0.0,
     Qkidney = 0.0,
     Qliver = 0.0,
     Qrest = 0.0,
     Vart = 0.0,
     Vgut = 0.0,
     Vkidney = 0.0,
     Vliver = 0.0,
     Vlung = 0.0,
     Vrest = 0.0,
     Vven = 0.0,
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
#define ID_Askinexposed 0x000b
#define ID_Askin 0x000c
#define ID_Amedia 0x000d

/* Model variables: Outputs */
#define ID_Cgut 0x0000
#define ID_Cliver 0x0001
#define ID_Cven 0x0002
#define ID_Clung 0x0003
#define ID_Cart 0x0004
#define ID_Crest 0x0005
#define ID_Ckidney 0x0006
#define ID_Cplasma 0x0007
#define ID_Aplasma 0x0008
#define ID_Cskinexposed 0x0009
#define ID_Cskin 0x000a
#define ID_Cmedia 0x000b

/* Parameters */
static double parms[51];

#define depth parms[0]
#define V0 parms[1]
#define Fskinexposed parms[2]
#define totalSA parms[3]
#define SA parms[4]
#define Kp parms[5]
#define Kskin2media parms[6]
#define BW parms[7]
#define Vmax parms[8]
#define Km parms[9]
#define hematocrit parms[10]
#define kgutabs parms[11]
#define Kkidney2pu parms[12]
#define Kliver2pu parms[13]
#define Krest2pu parms[14]
#define Kgut2pu parms[15]
#define Klung2pu parms[16]
#define Kskin2pu parms[17]
#define Qcardiacc parms[18]
#define Qgfrc parms[19]
#define Qskinf parms[20]
#define Qskin parms[21]
#define Qskinexposed parms[22]
#define Qgutf parms[23]
#define Qkidneyf parms[24]
#define Qliverf parms[25]
#define Vartc parms[26]
#define Vgutc parms[27]
#define Vkidneyc parms[28]
#define Vliverc parms[29]
#define Vlungc parms[30]
#define Vrestc parms[31]
#define Vvenc parms[32]
#define Vskinc parms[33]
#define Vskin parms[34]
#define Vskinexposed parms[35]
#define Fraction_unbound_plasma parms[36]
#define Rblood2plasma parms[37]
#define Qcardiac parms[38]
#define Qgfr parms[39]
#define Qgut parms[40]
#define Qkidney parms[41]
#define Qliver parms[42]
#define Qrest parms[43]
#define Vart parms[44]
#define Vgut parms[45]
#define Vkidney parms[46]
#define Vliver parms[47]
#define Vlung parms[48]
#define Vrest parms[49]
#define Vven parms[50]

/* Forcing (Input) functions */
static double forc[2];

#define forcing forc[0]
#define forcing_switch forc[1]

/*----- Initializers */
void initmod_skin_mm (void (* odeparms)(int *, double *))
{
  int N=51;
  odeparms(&N, parms);
}

void initforc_skin_mm (void (* odeforcs)(int *, double *))
{
  int N=2;
  odeforcs(&N, forc);
}


void getParms_skin_mm (double *inParms, double *out, int *nout) {
/*----- Model scaling */

  int i;

  for (i = 0; i < *nout; i++) {
    parms[i] = inParms[i];
  }


  kgutabs = kgutabs * 24 ;
  Qcardiac = Qcardiacc * 24 * pow ( BW , 0.75 ) ;
  Qgfr = Qgfrc * pow ( BW , 0.75 ) * 24 ;
  Qgut = Qcardiac * Qgutf ;
  Qkidney = Qcardiac * Qkidneyf ;
  Qliver = Qcardiac * Qliverf ;
  SA = Fskinexposed * totalSA ;
  Vskinexposed = SA * depth * 0.001 ;
  Vskin = Vskinc * BW - Vskinexposed ;
  Qskin = Qcardiac * Qskinf * ( 1 - Vskinexposed / ( Vskinc * BW ) ) ;
  Qskinexposed = Qcardiac * Qskinf * Vskinexposed / ( Vskinc * BW ) ;
  Qrest = Qcardiac - ( Qgut + Qkidney + Qliver + Qskin + Qskinexposed ) ;
  Vart = Vartc * BW ;
  Vgut = Vgutc * BW ;
  Vkidney = Vkidneyc * BW ;
  Vliver = Vliverc * BW ;
  Vlung = Vlungc * BW ;
  Vrest = Vrestc * BW ;
  Vven = Vvenc * BW ;

  for (i = 0; i < *nout; i++) {
    out[i] = parms[i];
  }
  }
/*----- Dynamics section */

void derivs_skin_mm (int *neq, double *pdTime, double *y, double *ydot, double *yout, int *ip)
{
  /* local */ double Vmedia;

  yout[ID_Cgut] = y[ID_Agut] / Vgut ;

  yout[ID_Cliver] = y[ID_Aliver] / Vliver ;

  yout[ID_Cven] = y[ID_Aven] / Vven ;

  yout[ID_Clung] = y[ID_Alung] / Vlung ;

  yout[ID_Cart] = y[ID_Aart] / Vart ;

  yout[ID_Crest] = y[ID_Arest] / Vrest ;

  yout[ID_Ckidney] = y[ID_Akidney] / Vkidney ;

  yout[ID_Cplasma] = y[ID_Aven] / Vven / Rblood2plasma ;

  yout[ID_Aplasma] = y[ID_Aven] / Rblood2plasma * ( 1 - hematocrit ) ;

  yout[ID_Cskin] = y[ID_Askin] / Vskin ;

  yout[ID_Cskinexposed] = y[ID_Askinexposed] / Vskinexposed ;

  Vmedia = V0 * forcing ;

  yout[ID_Cmedia] = y[ID_Amedia] / Vmedia ;

  ydot[ID_Agutlumen] = - kgutabs * y[ID_Agutlumen] ;

  ydot[ID_Amedia] = - Kp * SA * 24 * 0.001 * ( yout[ID_Cmedia] - yout[ID_Cskinexposed] / Kskin2media ) * forcing_switch ;

  ydot[ID_Agut] = kgutabs * y[ID_Agutlumen] + Qgut * ( y[ID_Aart] / Vart - y[ID_Agut] / Vgut * Rblood2plasma / Kgut2pu / Fraction_unbound_plasma ) ;

  ydot[ID_Aliver] = Qliver * y[ID_Aart] / Vart + Qgut * y[ID_Agut] / Vgut * Rblood2plasma / Kgut2pu / Fraction_unbound_plasma - ( Qliver + Qgut ) * y[ID_Aliver] / Vliver / Kliver2pu / Fraction_unbound_plasma * Rblood2plasma - Vmax * 24 * yout[ID_Cliver] / Kliver2pu / ( Km + yout[ID_Cliver] / Kliver2pu ) ;

  ydot[ID_Aven] = ( ( Qliver + Qgut ) * y[ID_Aliver] / Vliver / Kliver2pu + Qkidney * y[ID_Akidney] / Vkidney / Kkidney2pu + Qrest * y[ID_Arest] / Vrest / Krest2pu + Qskin * yout[ID_Cskin] / Kskin2pu + Qskinexposed * yout[ID_Cskinexposed] / Kskin2pu ) * Rblood2plasma / Fraction_unbound_plasma - Qcardiac * y[ID_Aven] / Vven ;

  ydot[ID_Alung] = Qcardiac * ( y[ID_Aven] / Vven - y[ID_Alung] / Vlung * Rblood2plasma / Klung2pu / Fraction_unbound_plasma ) ;

  ydot[ID_Aart] = Qcardiac * ( y[ID_Alung] / Vlung * Rblood2plasma / Klung2pu / Fraction_unbound_plasma - y[ID_Aart] / Vart ) ;

  ydot[ID_Arest] = Qrest * ( y[ID_Aart] / Vart - y[ID_Arest] / Vrest * Rblood2plasma / Krest2pu / Fraction_unbound_plasma ) ;

  ydot[ID_Akidney] = Qkidney * y[ID_Aart] / Vart - Qkidney * y[ID_Akidney] / Vkidney / Kkidney2pu * Rblood2plasma / Fraction_unbound_plasma - Qgfr * y[ID_Akidney] / Vkidney / Kkidney2pu ;

  ydot[ID_Atubules] = Qgfr * y[ID_Akidney] / Vkidney / Kkidney2pu ;

  ydot[ID_Ametabolized] = Vmax * yout[ID_Cliver] / Kliver2pu / ( Km + yout[ID_Cliver] / Kliver2pu ) ;

  ydot[ID_AUC] = y[ID_Aven] / Vven / Rblood2plasma ;

  ydot[ID_Askinexposed] = Qskinexposed * ( yout[ID_Cart] - yout[ID_Cskinexposed] * Rblood2plasma / Kskin2pu / Fraction_unbound_plasma ) + Kp * SA * 24 * 0.001 * ( yout[ID_Cmedia] - yout[ID_Cskinexposed] / Kskin2media ) * forcing_switch ;

  ydot[ID_Askin] = Qskin * ( yout[ID_Cart] - yout[ID_Cskin] * Rblood2plasma / Kskin2pu / Fraction_unbound_plasma ) ;

} /* derivs */


/*----- Jacobian calculations: */
void jac_skin_mm (int *neq, double *t, double *y, int *ml, int *mu, double *pd, int *nrowpd, double *yout, int *ip)
{

} /* jac */


/*----- Events calculations: */
void event_skin_mm (int *n, double *t, double *y)
{

} /* event */

/*----- Roots calculations: */
void root_skin_mm (int *neq, double *t, double *y, int *ng, double *gout, double *out, int *ip)
{

} /* root */

