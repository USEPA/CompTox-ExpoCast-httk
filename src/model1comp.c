/* pbtk1comp.c for R deSolve package
   ___________________________________________________

   Model File:  pbtk1comp.model

   Date:  Tue Mar 17 18:43:00 2015

   Created by:  "L:/Lab/NCCT_E~1/MCSim/mod/mod.exe v5.5.0"
    -- a model preprocessor by Don Maszle
   ___________________________________________________

   Copyright (c) 1993-2013 Free Software Foundation, Inc.

   Model calculations for compartmental model:

   4 States:
     Agutlumen = 0.0,
     Acompartment = 0.0,
     Ametabolized = 0.0,
     AUC = 0.0,

   1 Output:
    "Ccompartment",

   0 Inputs:

   3 Parameters:
     vdist = 0,
     ke = 0,
     kgutabs = 1,
*/

#include <R.h>

/* Model variables: States */
#define ID_Agutlumen 0x0000
#define ID_Acompartment 0x0001
#define ID_Ametabolized 0x0002
#define ID_AUC 0x0003

/* Model variables: Outputs */
#define ID_Ccompartment 0x0000

/* Parameters */
static double parms[3];

#define vdist parms[0]
#define ke parms[1]
#define kgutabs parms[2]




/*----- Initializers */
void initmod1comp (void (* odeparms)(int *, double *))
{
  int N=3;
  odeparms(&N, parms);
}




void getParms1comp (double *inParms, double *out, int *nout) {
/*----- Model scaling */

  int i;

  for (i = 0; i < *nout; i++) {
    parms[i] = inParms[i];
  }


  kgutabs = kgutabs * 24 ;
  ke = ke * 24 ;

  for (i = 0; i < *nout; i++) {
    out[i] = parms[i];
  }
  }
/*----- Dynamics section */

void derivs1comp (int *neq, double *pdTime, double *y, double *ydot, double *yout, int *ip)
{

  yout[ID_Ccompartment] = y[ID_Acompartment] / vdist ;

  ydot[ID_Agutlumen] = - kgutabs * y[ID_Agutlumen] ;

  ydot[ID_Acompartment] = kgutabs * y[ID_Agutlumen] - ke * y[ID_Acompartment];

  ydot[ID_Ametabolized] = ke * y[ID_Acompartment];

  ydot[ID_AUC] = yout[ID_Ccompartment] ;

} /* derivs */


/*----- Jacobian calculations: */
void jac1comp (int *neq, double *t, double *y, int *ml, int *mu, double *pd, int *nrowpd, double *yout, int *ip)
{

} /* jac */


/*----- Events calculations: */
void event1comp (int *n, double *t, double *y)
{

} /* event */

/*----- Roots calculations: */
void root1comp (int *neq, double *t, double *y, int *ng, double *gout, double *out, int *ip)
{

} /* root */

