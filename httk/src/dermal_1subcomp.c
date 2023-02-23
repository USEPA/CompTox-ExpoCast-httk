/* dermal_1subcomp_model.c for R deSolve package
   ___________________________________________________

   Model File:  dermal_1subcomp.model

   Date:  Wed Mar 09 10:12:41 2022

   Created by:  "C:/Users/AMEADE/modbin/mod.exe v6.1.0"
    -- a model preprocessor by Don Maszle
   ___________________________________________________

   Copyright (c) 1993-2019 Free Software Foundation, Inc.

   Model calculations for compartmental model:

   15 States:
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
     Askin_exposed = 0.0,
     Askin_unexposed = 0.0,
     Amedia = 0.0,
     Ain = 0.0,

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
    "Cskin_exposed",
    "Cskin_unexposed",
    "Cmedia",

   1 Input:
     Vmedia (forcing function)

   52 Parameters:
     skin_depth = 0,
     Fskin_exposed = 0,
     totalSA = 0,
     SA_exposed = 0,
     Kp = 0,
     Kskin2media = 0,
     BW = 70,
     Clmetabolismc = 0.0,
     hematocrit = 0.0,
     kgutabs = 0.0,
     Kkidney2pu = 0,
     Kliver2pu = 0,
     Krest2pu = 0,
     Kgut2pu = 0,
     Klung2pu = 0,
     Kskin2pu = 0,
     Qcardiacc = 0,
     Qgfrc = 0,
     Qskinf = 0,
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
     Fraction_unbound_plasma = 0.0,
     Rblood2plasma = 0.0,
     Clmetabolism = 0.0,
     Qcardiac = 0.0,
     Qgfr = 0.0,
     Qskin = 0.0,
     Qskin_exposed = 0.0,
     Qskin_unexposed = 0.0,
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
     Vskin = 0.0,
     Vskin_exposed = 0.0,
     Vskin_unexposed = 0.0,
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
#define ID_Arest 0x00006
#define ID_Akidney 0x00007
#define ID_Atubules 0x00008
#define ID_Ametabolized 0x00009
#define ID_AUC 0x0000a
#define ID_Askin_exposed 0x0000b
#define ID_Askin_unexposed 0x0000c
#define ID_Amedia 0x0000d
#define ID_Ain 0x0000e

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
#define ID_Cskin_exposed 0x00009
#define ID_Cskin_unexposed 0x0000a
#define ID_Cmedia 0x0000b

/* Parameters */
static double parms[52];

#define skin_depth parms[0]
#define Fskin_exposed parms[1]
#define totalSA parms[2]
#define SA_exposed parms[3]
#define Kp parms[4]
#define Kskin2media parms[5]
#define BW parms[6]
#define Clmetabolismc parms[7]
#define hematocrit parms[8]
#define kgutabs parms[9]
#define Kkidney2pu parms[10]
#define Kliver2pu parms[11]
#define Krest2pu parms[12]
#define Kgut2pu parms[13]
#define Klung2pu parms[14]
#define Kskin2pu parms[15]
#define Qcardiacc parms[16]
#define Qgfrc parms[17]
#define Qskinf parms[18]
#define Qgutf parms[19]
#define Qkidneyf parms[20]
#define Qliverf parms[21]
#define Vartc parms[22]
#define Vgutc parms[23]
#define Vkidneyc parms[24]
#define Vliverc parms[25]
#define Vlungc parms[26]
#define Vrestc parms[27]
#define Vvenc parms[28]
#define Vskinc parms[29]
#define Fraction_unbound_plasma parms[30]
#define Rblood2plasma parms[31]
#define Clmetabolism parms[32]
#define Qcardiac parms[33]
#define Qgfr parms[34]
#define Qskin parms[35]
#define Qskin_exposed parms[36]
#define Qskin_unexposed parms[37]
#define Qgut parms[38]
#define Qkidney parms[39]
#define Qliver parms[40]
#define Qrest parms[41]
#define Vart parms[42]
#define Vgut parms[43]
#define Vkidney parms[44]
#define Vliver parms[45]
#define Vlung parms[46]
#define Vrest parms[47]
#define Vven parms[48]
#define Vskin parms[49]
#define Vskin_exposed parms[50]
#define Vskin_unexposed parms[51]

/* Forcing (Input) functions */
static double forc[1];

#define Vmedia forc[0]

/*----- Initializers */
void initmod_dermal_1subcomp (void (* odeparms)(int *, double *))
{
  int N=52;
  odeparms(&N, parms);
}

void initforc_dermal_1subcomp (void (* odeforcs)(int *, double *))
{
  int N=1;
  odeforcs(&N, forc);
}


/* Calling R code will ensure that input y has same
   dimension as yini */
void initState_dermal_1subcomp (double *y)
{
  int i;

  for (i = 0; i < sizeof(yini) / sizeof(yini[0]); i++)
  {
    yini[i] = y[i];
  }
}

void getParms_dermal_1subcomp (double *inParms, double *out, int *nout) {
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

  SA_exposed = Fskin_exposed * totalSA ;

  Vskin = Vskinc * BW ;
  Vskin_exposed = SA_exposed * skin_depth * 0.001 ;
  Vskin_unexposed = Vskin - Vskin_exposed ;

  Qskin = Qcardiac * Qskinf ;
  Qskin_unexposed = Qskin * Vskin_unexposed / Vskin ;
  Qskin_exposed = Qskin * Vskin_exposed / Vskin ;
  Qrest = Qcardiac - ( Qgut + Qkidney + Qliver + Qskin ) ;

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

void derivs_dermal_1subcomp (int *neq, double *pdTime, double *y, double *ydot, double *yout, int *ip)
{
  /* local */ double Rin;

  yout[ID_Cgut] = y[ID_Agut] / Vgut ;

  yout[ID_Cliver] = y[ID_Aliver] / Vliver ;

  yout[ID_Cven] = y[ID_Aven] / Vven ;

  yout[ID_Clung] = y[ID_Alung] / Vlung ;

  yout[ID_Cart] = y[ID_Aart] / Vart ;

  yout[ID_Crest] = y[ID_Arest] / Vrest ;

  yout[ID_Ckidney] = y[ID_Akidney] / Vkidney ;

  yout[ID_Cplasma] = y[ID_Aven] / Vven / Rblood2plasma ;

  yout[ID_Aplasma] = y[ID_Aven] / Rblood2plasma * ( 1 - hematocrit ) ;

  yout[ID_Cskin_unexposed] = y[ID_Askin_unexposed] / Vskin_unexposed ;

  yout[ID_Cskin_exposed] = y[ID_Askin_exposed] / Vskin_exposed ;

  yout[ID_Cmedia] = y[ID_Amedia] / Vmedia ;

  Rin = ( y[ID_Amedia] ? Kp * SA_exposed * 24 * 0.001 * ( yout[ID_Cmedia] - yout[ID_Cskin_exposed] / Kskin2media ) : 0.0 ) ;

  ydot[ID_Amedia] = - Rin ;

  ydot[ID_Ain] = Rin ;

  ydot[ID_Askin_exposed] = Qskin_exposed * ( yout[ID_Cart] - yout[ID_Cskin_exposed] * Rblood2plasma / Kskin2pu / Fraction_unbound_plasma ) + Rin ;

  ydot[ID_Askin_unexposed] = Qskin_unexposed * ( yout[ID_Cart] - yout[ID_Cskin_unexposed] * Rblood2plasma / Kskin2pu / Fraction_unbound_plasma ) ;

  ydot[ID_Agutlumen] = - kgutabs * y[ID_Agutlumen] ;

  ydot[ID_Agut] = kgutabs * y[ID_Agutlumen] + Qgut * ( yout[ID_Cart] - yout[ID_Cgut] * Rblood2plasma / Kgut2pu / Fraction_unbound_plasma ) ;

  ydot[ID_Aliver] = Qliver * yout[ID_Cart] + Qgut * yout[ID_Cgut] * Rblood2plasma / Kgut2pu / Fraction_unbound_plasma - ( Qliver + Qgut ) * yout[ID_Cliver] / Kliver2pu / Fraction_unbound_plasma * Rblood2plasma - Clmetabolism * yout[ID_Cliver] / Kliver2pu ;

  ydot[ID_Aven] = ( ( Qliver + Qgut ) * yout[ID_Cliver] / Kliver2pu + Qkidney * yout[ID_Ckidney] / Kkidney2pu + Qrest * yout[ID_Crest] / Krest2pu + Qskin_unexposed * yout[ID_Cskin_unexposed] / Kskin2pu + Qskin_exposed * yout[ID_Cskin_exposed] / Kskin2pu ) * Rblood2plasma / Fraction_unbound_plasma - Qcardiac * yout[ID_Cven] ;

  ydot[ID_Alung] = Qcardiac * ( yout[ID_Cven] - yout[ID_Clung] * Rblood2plasma / Klung2pu / Fraction_unbound_plasma ) ;

  ydot[ID_Aart] = Qcardiac * ( y[ID_Alung] / Vlung * Rblood2plasma / Klung2pu / Fraction_unbound_plasma - y[ID_Aart] / Vart ) ;

  ydot[ID_Arest] = Qrest * ( yout[ID_Cart] - y[ID_Arest] / Vrest * Rblood2plasma / Krest2pu / Fraction_unbound_plasma ) ;

  ydot[ID_Akidney] = Qkidney * yout[ID_Cart] - Qkidney * yout[ID_Ckidney] / Kkidney2pu * Rblood2plasma / Fraction_unbound_plasma - Qgfr * yout[ID_Ckidney] / Kkidney2pu ;

  ydot[ID_Atubules] = Qgfr * yout[ID_Ckidney] / Kkidney2pu ;

  ydot[ID_Ametabolized] = Clmetabolism * yout[ID_Cliver] / Kliver2pu ;

  ydot[ID_AUC] = yout[ID_Cven] / Rblood2plasma ;

} /* derivs */


/*----- Jacobian calculations: */
void jac_dermal_1subcomp (int *neq, double *t, double *y, int *ml, int *mu, double *pd, int *nrowpd, double *yout, int *ip)
{

} /* jac */


/*----- Events calculations: */
void event_dermal_1subcomp (int *n, double *t, double *y)
{

} /* event */

/*----- Roots calculations: */
void root_dermal_1subcomp (int *neq, double *t, double *y, int *ng, double *gout, double *out, int *ip)
{

} /* root */

