/* model.c for R deSolve package
   ___________________________________________________

   Model File:  skin_2comp.model

   Date:  Sat Jun 20 00:29:59 2020

   Created by:  "mod v5.6.5"
    -- a model preprocessor by Don Maszle
   ___________________________________________________

   Copyright (c) 1993-2015 Free Software Foundation, Inc.

   Model calculations for compartmental model:

   16 States:
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
     Astratum_corneum_exposed = 0.0,
     Astratum_corneum_unexposed = 0.0,
     Acomposite_dermal_exposed = 0.0,
     Acomposite_dermal_unexposed = 0.0,
     Amedia = 0.0,

   14 Outputs:
    "Cgut",
    "Cliver",
    "Cven",
    "Clung",
    "Cart",
    "Crest",
    "Ckidney",
    "Cplasma",
    "Aplasma",
    "Cstratum_corneum_exposed",
    "Cstratum_corneum_unexposed",
    "Ccomposite_dermal_exposed",
    "Ccomposite_dermal_unexposed",
    "Cmedia",

   2 Inputs:
     forcing (forcing function)
     switch (forcing function)

   64 Parameters:
     skin_depth = 0,
     Fskin_depth_sc = 0,
     Fskin_depth_cd = 0,
     Psurface2sc = 0,
     Psc2cd = 0,
     V0 = 0,
     Fskin_exposed = 0,
     totalSA = 0,
     SA_exposed = 0,
     Kp = 0,
     BW = 70,
     Clmetabolismc = 0.203,
     hematocrit = 0.44,
     kgutabs = 1,
     Ksc2media = 0,
     Ksc2cd = 0,
     Kcd2pu = 0,
     Kkidney2pu = 0,
     Kliver2pu = 0,
     Krest2pu = 0,
     Kgut2pu = 0,
     Klung2pu = 0,
     Qcardiacc = 4.8,
     Qgfrc = 0.108,
     Qcomposite_dermalc = 0,
     Qgutc = 0,
     Qkidneyc = 0,
     Qliverc = 0,
     Vartc = 0,
     Vgutc = 0,
     Vkidneyc = 0,
     Vliverc = 0,
     Vlungc = 0,
     Vrestc = 0,
     Vvenc = 0,
     Vskinc = 0,
     Vstratum_corneumc = 0,
     Vcomposite_dermalc = 0,
     Fraction_unbound_plasma = 0.0,
     Rblood2plasma = 0.0,
     Clmetabolism = 0.0,
     Qcardiac = 0.0,
     Qcomposite_dermal = 0,
     Qcomposite_dermal_exposed = 0,
     Qcomposite_dermal_unexposed = 0,
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
     Vskin = 0,
     Vstratum_corneum = 0,
     Vstratum_corneum_exposed = 0,
     Vstratum_corneum_unexposed = 0,
     Vcomposite_dermal = 0,
     Vcomposite_dermal_exposed = 0,
     Vcomposite_dermal_unexposed = 0,
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
#define ID_Astratum_corneum_exposed 0x0000b
#define ID_Astratum_corneum_unexposed 0x0000c
#define ID_Acomposite_dermal_exposed 0x0000d
#define ID_Acomposite_dermal_unexposed 0x0000e
#define ID_Amedia 0x0000f

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
#define ID_Cstratum_corneum_exposed 0x00009
#define ID_Cstratum_corneum_unexposed 0x0000a
#define ID_Ccomposite_dermal_exposed 0x0000b
#define ID_Ccomposite_dermal_unexposed 0x0000c
#define ID_Cmedia 0x0000d

/* Parameters */
static double parms[64];

#define skin_depth parms[0]
#define Fskin_depth_sc parms[1]
#define Fskin_depth_cd parms[2]
#define Psurface2sc parms[3]
#define Psc2cd parms[4]
#define V0 parms[5]
#define Fskin_exposed parms[6]
#define totalSA parms[7]
#define SA_exposed parms[8]
#define Kp parms[9]
#define BW parms[10]
#define Clmetabolismc parms[11]
#define hematocrit parms[12]
#define kgutabs parms[13]
#define Ksc2media parms[14]
#define Ksc2cd parms[15]
#define Kcd2pu parms[16]
#define Kkidney2pu parms[17]
#define Kliver2pu parms[18]
#define Krest2pu parms[19]
#define Kgut2pu parms[20]
#define Klung2pu parms[21]
#define Qcardiacc parms[22]
#define Qgfrc parms[23]
#define Qcomposite_dermalc parms[24]
#define Qgutc parms[25]
#define Qkidneyc parms[26]
#define Qliverc parms[27]
#define Vartc parms[28]
#define Vgutc parms[29]
#define Vkidneyc parms[30]
#define Vliverc parms[31]
#define Vlungc parms[32]
#define Vrestc parms[33]
#define Vvenc parms[34]
#define Vskinc parms[35]
#define Vstratum_corneumc parms[36]
#define Vcomposite_dermalc parms[37]
#define Fraction_unbound_plasma parms[38]
#define Rblood2plasma parms[39]
#define Clmetabolism parms[40]
#define Qcardiac parms[41]
#define Qcomposite_dermal parms[42]
#define Qcomposite_dermal_exposed parms[43]
#define Qcomposite_dermal_unexposed parms[44]
#define Qgfr parms[45]
#define Qgut parms[46]
#define Qkidney parms[47]
#define Qliver parms[48]
#define Qrest parms[49]
#define Vart parms[50]
#define Vgut parms[51]
#define Vkidney parms[52]
#define Vliver parms[53]
#define Vlung parms[54]
#define Vrest parms[55]
#define Vven parms[56]
#define Vskin parms[57]
#define Vstratum_corneum parms[58]
#define Vstratum_corneum_exposed parms[59]
#define Vstratum_corneum_unexposed parms[60]
#define Vcomposite_dermal parms[61]
#define Vcomposite_dermal_exposed parms[62]
#define Vcomposite_dermal_unexposed parms[63]

/* Forcing (Input) functions */
static double forc[2];

#define forcing forc[0]
#define switch forc[1]

/*----- Initializers */
void initmod (void (* odeparms)(int *, double *))
{
  int N=64;
  odeparms(&N, parms);
}

void initforc (void (* odeforcs)(int *, double *))
{
  int N=2;
  odeforcs(&N, forc);
}


void getParms (double *inParms, double *out, int *nout) {
/*----- Model scaling */

  /* local */ double Vskin_exposed;
  /* local */ double Vskin_unexposed;
  int i;

  for (i = 0; i < *nout; i++) {
    parms[i] = inParms[i];
  }


  kgutabs = kgutabs * 24 ;
  Clmetabolism = Clmetabolismc * 24 * BW ;
  Qcardiac = Qcardiacc * 24 * pow ( BW , 0.75 ) ;
  Qgfr = Qgfrc * pow ( BW , 0.75 ) * 24 ;
  Qgut = Qcardiac * Qgutc ;
  Qkidney = Qcardiac * Qkidneyc ;
  Qliver = Qcardiac * Qliverc ;
  SA_exposed = Fskin_exposed * totalSA ;
  Vskin = BW * Vskinc ;
  Vstratum_corneum = Vskin * Fskin_depth_sc ;
  Vstratum_corneum_exposed = Vstratum_corneum * Fskin_exposed ;
  Vstratum_corneum_unexposed = Vstratum_corneum * ( 1 - Fskin_exposed ) ;
  Vcomposite_dermal = Vskin * Fskin_depth_cd ;
  Vcomposite_dermal_exposed = Vcomposite_dermal * Fskin_exposed ;
  Vcomposite_dermal_unexposed = Vcomposite_dermal * ( 1 - Fskin_exposed ) ;
  Qcomposite_dermal = Qcardiac * Qcomposite_dermalc ;
  Qcomposite_dermal_unexposed = Qcomposite_dermal * ( 1 - Vskin_exposed / Vskin ) ;
  Qcomposite_dermal_exposed = Qcomposite_dermal * Vskin_exposed / Vskin ;
  Qrest = Qcardiac - ( Qgut + Qkidney + Qliver + Qcomposite_dermal ) ;
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

void derivs (int *neq, double *pdTime, double *y, double *ydot, double *yout, int *ip)
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

  yout[ID_Cstratum_corneum_unexposed] = y[ID_Astratum_corneum_unexposed] / Vstratum_corneum_unexposed ;

  yout[ID_Cstratum_corneum_exposed] = y[ID_Astratum_corneum_exposed] / Vstratum_corneum_exposed ;

  yout[ID_Ccomposite_dermal_unexposed] = y[ID_Acomposite_dermal_unexposed] / Vcomposite_dermal_unexposed ;

  yout[ID_Ccomposite_dermal_exposed] = y[ID_Acomposite_dermal_exposed] / Vcomposite_dermal_exposed ;

  Vmedia = V0 * forcing ;

  yout[ID_Cmedia] = y[ID_Amedia] / Vmedia ;

  ydot[ID_Agutlumen] = - kgutabs * y[ID_Agutlumen] ;

  ydot[ID_Amedia] = - Kp * SA_exposed * 24 * 0.001 * ( yout[ID_Cmedia] - yout[ID_Cstratum_corneum_exposed] / Ksc2media ) * switch ;

  ydot[ID_Agut] = kgutabs * y[ID_Agutlumen] + Qgut * ( y[ID_Aart] / Vart - y[ID_Agut] / Vgut * Rblood2plasma / Kgut2pu / Fraction_unbound_plasma ) ;

  ydot[ID_Aliver] = Qliver * y[ID_Aart] / Vart + Qgut * y[ID_Agut] / Vgut * Rblood2plasma / Kgut2pu / Fraction_unbound_plasma - ( Qliver + Qgut ) * y[ID_Aliver] / Vliver / Kliver2pu / Fraction_unbound_plasma * Rblood2plasma - Clmetabolism * y[ID_Aliver] / Vliver / Kliver2pu ;

  ydot[ID_Aven] = ( ( Qliver + Qgut ) * y[ID_Aliver] / Vliver / Kliver2pu + Qkidney * y[ID_Akidney] / Vkidney / Kkidney2pu + Qrest * y[ID_Arest] / Vrest / Krest2pu + Qcomposite_dermal_unexposed * yout[ID_Ccomposite_dermal_unexposed] / Kcd2pu + Qcomposite_dermal_exposed * yout[ID_Ccomposite_dermal_exposed] / Kcd2pu ) * Rblood2plasma / Fraction_unbound_plasma - Qcardiac * y[ID_Aven] / Vven ;

  ydot[ID_Alung] = Qcardiac * ( y[ID_Aven] / Vven - y[ID_Alung] / Vlung * Rblood2plasma / Klung2pu / Fraction_unbound_plasma ) ;

  ydot[ID_Aart] = Qcardiac * ( y[ID_Alung] / Vlung * Rblood2plasma / Klung2pu / Fraction_unbound_plasma - y[ID_Aart] / Vart ) ;

  ydot[ID_Arest] = Qrest * ( y[ID_Aart] / Vart - y[ID_Arest] / Vrest * Rblood2plasma / Krest2pu / Fraction_unbound_plasma ) ;

  ydot[ID_Akidney] = Qkidney * y[ID_Aart] / Vart - Qkidney * y[ID_Akidney] / Vkidney / Kkidney2pu * Rblood2plasma / Fraction_unbound_plasma - Qgfr * y[ID_Akidney] / Vkidney / Kkidney2pu ;

  ydot[ID_Atubules] = Qgfr * y[ID_Akidney] / Vkidney / Kkidney2pu ;

  ydot[ID_Ametabolized] = Clmetabolism * y[ID_Aliver] / Vliver / Kliver2pu ;

  ydot[ID_AUC] = y[ID_Aven] / Vven / Rblood2plasma ;

  ydot[ID_Astratum_corneum_exposed] = Psurface2sc * SA_exposed * 24 * 0.001 * ( yout[ID_Cmedia] - yout[ID_Cstratum_corneum_exposed] / Ksc2media ) * switch + Psc2cd * SA_exposed * 24 * 0.001 * ( yout[ID_Ccomposite_dermal_exposed] - yout[ID_Cstratum_corneum_exposed] / Ksc2cd ) ;

  ydot[ID_Astratum_corneum_unexposed] = Psc2cd * ( totalSA - SA_exposed ) * 24 * 0.001 * ( yout[ID_Ccomposite_dermal_unexposed] - yout[ID_Cstratum_corneum_unexposed] / Ksc2cd ) ;

  ydot[ID_Acomposite_dermal_exposed] = Qcomposite_dermal_exposed * ( yout[ID_Cart] - yout[ID_Ccomposite_dermal_exposed] * Rblood2plasma / Kcd2pu / Fraction_unbound_plasma ) + Psc2cd * SA_exposed * 24 * 0.001 * ( yout[ID_Cstratum_corneum_exposed] / Ksc2cd - yout[ID_Ccomposite_dermal_exposed] ) ;

  ydot[ID_Acomposite_dermal_unexposed] = Qcomposite_dermal_unexposed * ( yout[ID_Cart] - yout[ID_Ccomposite_dermal_unexposed] * Rblood2plasma / Kcd2pu / Fraction_unbound_plasma ) + Psc2cd * ( totalSA - SA_exposed ) * 24 * 0.001 * ( yout[ID_Cstratum_corneum_unexposed] / Ksc2cd - yout[ID_Ccomposite_dermal_unexposed] ) ;

} /* derivs */


/*----- Jacobian calculations: */
void jac (int *neq, double *t, double *y, int *ml, int *mu, double *pd, int *nrowpd, double *yout, int *ip)
{

} /* jac */


/*----- Events calculations: */
void event (int *n, double *t, double *y)
{

} /* event */

/*----- Roots calculations: */
void root (int *neq, double *t, double *y, int *ng, double *gout, double *out, int *ip)
{

} /* root */

