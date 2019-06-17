/* 3compPBPKmodel.c for R deSolve package
   ___________________________________________________

   Model File:  3compPBPKmodel.model

   Date:  Mon Mar 23 15:12:36 2015

   Created by:  "L:/Lab/NCCT_E~1/MCSim/mod/mod.exe v5.5.0"
    -- a model preprocessor by Don Maszle
   ___________________________________________________

   Copyright (c) 1993-2013 Free Software Foundation, Inc.

   Model calculations for compartmental model:

   7 States:
     Agutlumen = 0.0,
     Agut = 0.0,
     Aliver = 0.0,
     Arest = 0.0,
     Ametabolized = 0.0,
     Atubules = 0.0,
     AUC = 0.0,

   4 Outputs:
    "Cgut",
    "Cliver",
    "Crest",
    "Cserum",

   0 Inputs:

   20 Parameters:
     BW = 70,
     Clmetabolismc = 0.203,
     kgutabs = 1,
     Qcardiacc = 0,
     Qgfrc = 0.108,
     Qgutf = 0.205,
     Qliverf = 0.0536,
     Vgut = 0,
     Vliver = 0,
     Vrest = 0,
     Fraction_unbound_plasma = 0.0682,
     Clmetabolism = 0.0,
     Qcardiac = 0,
     Qgfr = 0.0,
     Qgut = 0.0,
     Qliver = 0.0,
     Kliver2plasma = 0,
     Krest2plasma = 0,
     Kgut2plasma = 0,
     Ratioblood2plasma = 0,
*/

#include <R.h>

/* Model variables: States */
#define ID_Agutlumen 0x0000
#define ID_Agut 0x0001
#define ID_Aliver 0x0002
#define ID_Arest 0x0003
#define ID_Ametabolized 0x0004
#define ID_Atubules 0x0005
#define ID_AUC 0x0006

/* Model variables: Outputs */
#define ID_Cgut 0x0000
#define ID_Cliver 0x0001
#define ID_Crest 0x0002
#define ID_Cserum 0x0003

/* Parameters */
static double parms[20];

#define BW parms[0]
#define Clmetabolismc parms[1]
#define kgutabs parms[2]
#define Qcardiacc parms[3]
#define Qgfrc parms[4]
#define Qgutf parms[5]
#define Qliverf parms[6]
#define Vgut parms[7]
#define Vliver parms[8]
#define Vrest parms[9]
#define Fraction_unbound_plasma parms[10]
#define Clmetabolism parms[11]
#define Qcardiac parms[12]
#define Qgfr parms[13]
#define Qgut parms[14]
#define Qliver parms[15]
#define Kliver2plasma parms[16]
#define Krest2plasma parms[17]
#define Kgut2plasma parms[18]
#define Ratioblood2plasma parms[19]



/*----- Initializers */
void initmod3comp (void (* odeparms)(int *, double *))
{
  int N=20;
  odeparms(&N, parms);
}


void getParms3comp (double *inParms, double *out, int *nout) {
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
  Qliver = Qcardiac * Qliverf ;

  for (i = 0; i < *nout; i++) {
    out[i] = parms[i];
  }
  }
/*----- Dynamics section */

void derivs3comp (int *neq, double *pdTime, double *y, double *ydot, double *yout, int *ip)
{

  yout[ID_Cgut] = y[ID_Agut] / Vgut ;

  yout[ID_Cliver] = y[ID_Aliver] / Vliver ;

  yout[ID_Crest] = y[ID_Arest] / Vrest ;

  yout[ID_Cserum] = y[ID_Aliver] / Vliver / Kliver2plasma / Fraction_unbound_plasma ;

  ydot[ID_Agutlumen] = - kgutabs * y[ID_Agutlumen] ;

  ydot[ID_Agut] = kgutabs * y[ID_Agutlumen] + Qgut * ( y[ID_Arest] / Vrest * Ratioblood2plasma / Fraction_unbound_plasma / Krest2plasma - y[ID_Agut] / Vgut * Ratioblood2plasma / Kgut2plasma / Fraction_unbound_plasma ) ;

  ydot[ID_Aliver] = Qgut * y[ID_Agut] / Vgut * Ratioblood2plasma / Kgut2plasma / Fraction_unbound_plasma + Qliver * y[ID_Arest] / Vrest * Ratioblood2plasma / Fraction_unbound_plasma / Krest2plasma - ( Qliver + Qgut ) * y[ID_Aliver] / Vliver * Ratioblood2plasma / Fraction_unbound_plasma / Kliver2plasma - Clmetabolism / Kliver2plasma * y[ID_Aliver] / Vliver ;

  ydot[ID_Arest] = ( Qgut + Qliver ) * y[ID_Aliver] / Vliver * Ratioblood2plasma / Fraction_unbound_plasma / Kliver2plasma - Qgut * y[ID_Arest] / Vrest * Ratioblood2plasma / Fraction_unbound_plasma / Krest2plasma - Qliver * y[ID_Arest] / Vrest * Ratioblood2plasma / Fraction_unbound_plasma / Krest2plasma - Qgfr / Krest2plasma * y[ID_Arest] / Vrest ;

  ydot[ID_Ametabolized] = Clmetabolism / Kliver2plasma * y[ID_Aliver] / Vliver ;

  ydot[ID_Atubules] = Qgfr / Krest2plasma * y[ID_Arest] / Vrest ;

  ydot[ID_AUC] = y[ID_Aliver] / Vliver / Kliver2plasma / Fraction_unbound_plasma ;

} /* derivs */


/*----- Jacobian calculations: */
void jac3comp (int *neq, double *t, double *y, int *ml, int *mu, double *pd, int *nrowpd, double *yout, int *ip)
{

} /* jac */


/*----- Events calculations: */
void event3comp (int *n, double *t, double *y)
{

} /* event */

/*----- Roots calculations: */
void root3comp (int *neq, double *t, double *y, int *ng, double *gout, double *out, int *ip)
{

} /* root */

