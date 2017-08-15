/* vLiverPBPK.c for R deSolve package

   ___________________________________________________

   Model File:  vLiverPBPK.model

   Date:  Mon Jan 26 15:57:27 2015

   Created by:  "L:/Lab/NCCT_E~1/MCSim/mod/mod.exe v5.5.0"
    -- a model preprocessor by Don Maszle
   ___________________________________________________

   Copyright (c) 1993-2013 Free Software Foundation, Inc.

   Model calculations for compartmental model:

   11 States:
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

   9 Outputs:
    "Cgut",
    "Cliver",
    "Cven",
    "Clung",
    "Cart",
    "Crest",
    "Ckidney",
    "Cserum",
    "Aserum",

   0 Inputs:

   37 Parameters:
     BW = 70,
     CLmetabolismc = 0.203,
     hematocrit = 0.44,
     kgutabs = 1,
     Kkidney2plasma = 0,
     Kliver2plasma = 0,
     Krest2plasma = 0,
     Kgut2plasma = 0,
     Klung2plasma = 0,
     Qcardiacc = 4.8,
     Qgfrc = 0.108,
     Qgutf = 0.205,
     Qkidneyf = 0.221,
     Qliverf = 0.0536,
     Vartc = 0.0487,
     Vgutc = 0.0158,
     Vkidneyc = 0.00119,
     Vliverc = 0.02448,
     Vlungc = 0.00723,
     Vrestc = 0.77654,
     Vvenc = 0.0487,
     Fraction_unbound_plasma = 0.0682,
     Ratioblood2plasma = 0.0,
     CLmetabolism = 0.0,
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

/* Parameters */
static double parms[37];

#define BW parms[0]
#define CLmetabolismc parms[1]
#define hematocrit parms[2]
#define kgutabs parms[3]
#define Kkidney2plasma parms[4]
#define Kliver2plasma parms[5]
#define Krest2plasma parms[6]
#define Kgut2plasma parms[7]
#define Klung2plasma parms[8]
#define Qcardiacc parms[9]
#define Qgfrc parms[10]
#define Qgutf parms[11]
#define Qkidneyf parms[12]
#define Qliverf parms[13]
#define Vartc parms[14]
#define Vgutc parms[15]
#define Vkidneyc parms[16]
#define Vliverc parms[17]
#define Vlungc parms[18]
#define Vrestc parms[19]
#define Vvenc parms[20]
#define Fraction_unbound_plasma parms[21]
#define Ratioblood2plasma parms[22]
#define CLmetabolism parms[23]
#define Qcardiac parms[24]
#define Qgfr parms[25]
#define Qgut parms[26]
#define Qkidney parms[27]
#define Qliver parms[28]
#define Qrest parms[29]
#define Vart parms[30]
#define Vgut parms[31]
#define Vkidney parms[32]
#define Vliver parms[33]
#define Vlung parms[34]
#define Vrest parms[35]
#define Vven parms[36]




/*----- Initializers */
void initmod (void (* odeparms)(int *, double *))
{
  int N=37;
  odeparms(&N, parms);
}


void getParms (double *inParms, double *out, int *nout) {
/*----- Model scaling */

  int i;

  for (i = 0; i < *nout; i++) {
    parms[i] = inParms[i];
  }


  kgutabs = kgutabs * 24 ;
  CLmetabolism = CLmetabolismc * 24 * BW ;
  Qcardiac = Qcardiacc * 24 * pow ( BW , 0.75 ) ;
  Qgfr = Qgfrc * pow ( BW , 0.75 ) * 24 ;
  Qgut = Qcardiac * Qgutf ;
  Qkidney = Qcardiac * Qkidneyf ;
  Qliver = Qcardiac * Qliverf ;
  Qrest = Qcardiac - ( Qgut + Qkidney + Qliver ) ;
  Vart = Vartc * BW ;
  Vgut = Vgutc * BW ;
  Vkidney = Vkidneyc * BW;
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

  yout[ID_Cgut] = y[ID_Agut] / Vgut ;

  yout[ID_Cliver] = y[ID_Aliver] / Vliver ;

  yout[ID_Cven] = y[ID_Aven] / Vven ;

  yout[ID_Clung] = y[ID_Alung] / Vlung ;

  yout[ID_Cart] = y[ID_Aart] / Vart ;

  yout[ID_Crest] = y[ID_Arest] / Vrest ;

  yout[ID_Ckidney] = y[ID_Akidney] / Vkidney ;

  yout[ID_Cserum] = y[ID_Aven] / Vven / Ratioblood2plasma ;

  yout[ID_Aserum] = y[ID_Aven] / Ratioblood2plasma * ( 1 - hematocrit ) ;

  ydot[ID_Agutlumen] = - kgutabs * y[ID_Agutlumen] ;

  ydot[ID_Agut] = kgutabs * y[ID_Agutlumen] + Qgut * ( y[ID_Aart] / Vart - y[ID_Agut] / Vgut * Ratioblood2plasma / Kgut2plasma / Fraction_unbound_plasma ) ;

  ydot[ID_Aliver] = Qliver * y[ID_Aart] / Vart + Qgut * y[ID_Agut] / Vgut * Ratioblood2plasma / Kgut2plasma / Fraction_unbound_plasma - ( Qliver + Qgut ) * y[ID_Aliver] / Vliver / Kliver2plasma / Fraction_unbound_plasma * Ratioblood2plasma - CLmetabolism * y[ID_Aliver] / Vliver / Kliver2plasma ;

  ydot[ID_Aven] = ( ( Qliver + Qgut ) * y[ID_Aliver] / Vliver / Kliver2plasma + Qkidney * y[ID_Akidney] / Vkidney / Kkidney2plasma + Qrest * y[ID_Arest] / Vrest / Krest2plasma ) * Ratioblood2plasma / Fraction_unbound_plasma - Qcardiac * y[ID_Aven] / Vven ;

  ydot[ID_Alung] = Qcardiac * ( y[ID_Aven] / Vven - y[ID_Alung] / Vlung * Ratioblood2plasma / Klung2plasma / Fraction_unbound_plasma ) ;

  ydot[ID_Aart] = Qcardiac * ( y[ID_Alung] / Vlung * Ratioblood2plasma / Klung2plasma / Fraction_unbound_plasma - y[ID_Aart] / Vart ) ;

  ydot[ID_Arest] = Qrest * ( y[ID_Aart] / Vart - y[ID_Arest] / Vrest * Ratioblood2plasma / Krest2plasma / Fraction_unbound_plasma ) ;

  ydot[ID_Akidney] = Qkidney * y[ID_Aart] / Vart - Qkidney * y[ID_Akidney] / Vkidney / Kkidney2plasma * Ratioblood2plasma / Fraction_unbound_plasma - Qgfr * y[ID_Akidney] / Vkidney / Kkidney2plasma ;

  ydot[ID_Atubules] = Qgfr * y[ID_Akidney] / Vkidney / Kkidney2plasma ;

  ydot[ID_Ametabolized] = CLmetabolism * y[ID_Aliver] / Vliver / Kliver2plasma ;

  ydot[ID_AUC] = y[ID_Aven] / Vven / Ratioblood2plasma ;

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

