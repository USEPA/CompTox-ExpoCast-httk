/* 3comp.C for R deSolve package
   ___________________________________________________

   Model File:  git/httkmodels/3compPBPKmodel.model

   Date:  Thu Jul 25 13:04:54 2019

   Created by:  "c:/users/jwambaug/MCSim under R/mod/mod v6.1.0"
    -- a model preprocessor by Don Maszle
   ___________________________________________________

   Copyright (c) 1993-2019 Free Software Foundation, Inc.

   Model calculations for compartmental model:

   7 States:
     Aintestine = 0.0,
     Aportven = 0.0,
     Aliver = 0.0,
     Asyscomp = 0.0,
     Ametabolized = 0.0,
     Atubules = 0.0,
     AUC = 0.0,
   3 Outputs:
    "Cportven",
    "Cliver",
    "Csyscomp",

   0 Inputs:

   22 Parameters:
     BW = 0.0,
     CLmetabolismc = 0.0,
     kgutabs = 0.0,
     Qcardiacc = 0.0,
     Qgfrc = 0.0,
     Qgutf = 0.0,
     Qliverf = 0.0,
     Vportvenc = 0.0,
     Vliverc = 0.0,
     Vsyscompc = 0.0,
     Vportven = 0.0,
     Vliver = 0.0,
     Vsyscomp = 0.0,
     Fraction_unbound_plasma = 0.0,
     CLmetabolism = 0.0,
     Qcardiac = 0.0,
     Qgfr = 0.0,
     Qgut = 0.0,
     Qliver = 0.0,
     Kliver2plasma = 0.0,
     Krest2plasma = 0.0,
     Ratioblood2plasma = 0.0,


*/

#include <R.h>
#include <Rinternals.h>
#include <Rdefines.h>
#include <R_ext/Rdynload.h>

/* Model variables: States */
#define ID_Aintestine 0x00000
#define ID_Aportven 0x00001
#define ID_Aliver 0x00002
#define ID_Asyscomp 0x00003
#define ID_Ametabolized 0x00004
#define ID_Atubules 0x00005
#define ID_AUC 0x00006

/* Model variables: Outputs */
#define ID_Cportven 0x00000
#define ID_Cliver 0x00001
#define ID_Csyscomp 0x00002
#define ID_Cplasma 0x00003

/* Parameters */
static double parms[22];

#define BW parms[0]
#define CLmetabolismc parms[1]
#define kgutabs parms[2]
#define Qcardiacc parms[3]
#define Qgfrc parms[4]
#define Qgutf parms[5]
#define Qliverf parms[6]
#define Vportvenc parms[7]
#define Vliverc parms[8]
#define Vsyscompc parms[9]
#define Vportven parms[10]
#define Vliver parms[11]
#define Vsyscomp parms[12]
#define Fraction_unbound_plasma parms[13]
#define CLmetabolism parms[14]
#define Qcardiac parms[15]
#define Qgfr parms[16]
#define Qgut parms[17]
#define Qliver parms[18]
#define Kliver2plasma parms[19]
#define Krest2plasma parms[20]
#define Ratioblood2plasma parms[21]

/* Forcing (Input) functions */
static double forc[8];

#define d_BW forc[0]
#define d_CLmetabolismc forc[1]
#define d_Krest2plasma forc[2]
#define d_Qcardiacc forc[3]
#define d_Qgfrc forc[4]
#define d_Vliverc forc[5]
#define d_Vportvenc forc[6]
#define d_Vsyscompc forc[7]

/*Array of initial state variables*/
/* NOT NEEDED FOR HTTK:
static double yini3comp[7] = {0.0, 
  0.0,
  0.0,
  0.0,
  0.0,
  0.0,
  0.0
  }; 
*/

/*----- Initializers */
void initmod3comp_lifestage (void (* odeparms)(int *, double *))
{
  int N=22;
  odeparms(&N, parms);
}

void initforc3comp_lifestage (void (* odeforcs)(int *, double *))
{
  int N=8;
  odeforcs(&N, forc);
}

/* Calling R code will ensure that input y has same
   dimension as yini */
/* NOT NEEDED FOR HTTK
void initState3comp (double *y)
{
  int i;

  for (i = 0; i < sizeof(yini3comp) / sizeof(yini3comp[0]); i++)
  {
    yini3comp[i] = y[i];
  }
}
*/

void getParms3comp_lifestage (double *inParms, double *out, int *nout) {
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
  Qliver = Qcardiac * Qliverf ;
  Vportven = Vportvenc * BW ;
  Vsyscomp = Vsyscompc * BW ;
  Vliver = Vliverc * BW ;

  for (i = 0; i < *nout; i++) {
    out[i] = parms[i];
  }
  }
/*----- Dynamics section */

void derivs3comp_lifestage (int *neq, double *pdTime, double *y, double *ydot, double *yout, int *ip)
{
  
  yout[ID_Cportven] = y[ID_Aportven] / ( Vportvenc + d_Vportvenc ) / ( BW + d_BW ) / 1;
  
  yout[ID_Cliver] = y[ID_Aliver] / ( Vliverc + d_Vliverc ) / ( BW + d_BW ) * Ratioblood2plasma / Fraction_unbound_plasma / Kliver2plasma;
  
  yout[ID_Csyscomp] = y[ID_Asyscomp] / ( Vsyscompc + d_Vsyscompc ) / ( BW + d_BW ) * Ratioblood2plasma / Fraction_unbound_plasma / ( Krest2plasma + d_Krest2plasma );
  
  yout[ID_Cplasma] = yout[ID_Csyscomp] / Ratioblood2plasma;
  
  ydot[ID_Aintestine] = - kgutabs * y[ID_Aintestine] ;
  
  ydot[ID_Aportven] = kgutabs * y[ID_Aintestine] + Qgutf * ( Qcardiacc + d_Qcardiacc ) * 24 * pow ( BW + d_BW , 0.75 ) * yout[ID_Csyscomp] - Qgutf * ( Qcardiacc + d_Qcardiacc ) * 24 * pow ( BW + d_BW , 0.75 ) * yout[ID_Cportven];
  
  ydot[ID_Aliver] = Qgutf * ( Qcardiacc + d_Qcardiacc ) * 24 * pow ( BW + d_BW , 0.75 ) * yout[ID_Cportven] + Qliverf * ( Qcardiacc + d_Qcardiacc ) * 24 * pow ( BW + d_BW , 0.75 ) * yout[ID_Csyscomp] - ( Qgutf + Qliverf ) * ( Qcardiacc + d_Qcardiacc ) * 24 * pow ( BW + d_BW , 0.75 ) * yout[ID_Cliver]  - ( CLmetabolism / BW + 24 * d_CLmetabolismc ) * (BW + d_BW) / Ratioblood2plasma * yout[ID_Cliver];
  
  ydot[ID_Asyscomp] = ( Qgutf + Qliverf ) * ( Qcardiacc + d_Qcardiacc ) * 24 * pow ( BW + d_BW , 0.75 ) * yout[ID_Cliver] - ( Qgutf + Qliverf ) * ( Qcardiacc + d_Qcardiacc ) * 24 * pow ( BW + d_BW , 0.75 ) * yout[ID_Csyscomp] - Fraction_unbound_plasma / Ratioblood2plasma * ( Qgfrc + d_Qgfrc ) * 24 * pow ( BW + d_BW , 0.75 ) * yout[ID_Csyscomp];
  
  ydot[ID_Ametabolized] = ( CLmetabolism / BW + 24 * d_CLmetabolismc ) * (BW + d_BW) / Ratioblood2plasma * yout[ID_Cliver];
  
  ydot[ID_Atubules] = Fraction_unbound_plasma / Ratioblood2plasma * ( Qgfrc + d_Qgfrc ) * 24 * pow ( BW + d_BW , 0.75 ) * yout[ID_Csyscomp] ;
  
  ydot[ID_AUC] = yout[ID_Csyscomp] ;
  

} /* derivs */


/*----- Jacobian calculations: */
void jac3comp_lifestage (int *neq, double *t, double *y, int *ml, int *mu, double *pd, int *nrowpd, double *yout, int *ip)
{

} /* jac */


/*----- Events calculations: */
void event3comp_lifestage (int *n, double *t, double *y)
{

} /* event */

/*----- Roots calculations: */
void root3comp_lifestage (int *neq, double *t, double *y, int *ng, double *gout, double *out, int *ip)
{

} /* root */
