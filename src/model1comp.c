/* 1comp.C for R deSolve package
   ___________________________________________________

   Model File:  pbtk1comp.model

   Date:  Wed Jul 24 14:54:59 2019

   Created by:  "c:/users/jwambaug/MCSim under R/mod/mod v6.1.0"
    -- a model preprocessor by Don Maszle
   ___________________________________________________

   Copyright (c) 1993-2019 Free Software Foundation, Inc.

   Model calculations for compartmental model:

   4 States:
     Agutlumen = 0.0,
     Acompartment = 0.0,
     Ametabolized = 0.0,
     AUC = 0.0,

   1 Output:
    "Ccompartment",

   0 Inputs:

   4 Parameters:
     vdist = 0,
     ke = 0,
     kgutabs = 1,
     BW = 70,
*/

#include <R.h>
#include <Rinternals.h>
#include <Rdefines.h>
#include <R_ext/Rdynload.h>

/* Model variables: States */
#define ID_Agutlumen 0x00000
#define ID_Acompartment 0x00001
#define ID_Ametabolized 0x00002
#define ID_AUC 0x00003

/* Model variables: Outputs */
#define ID_Ccompartment 0x00000

/* Parameters */
static double parms[4];

#define vdist parms[0]
#define ke parms[1]
#define kgutabs parms[2]
#define BW parms[3]

/* Function definitions for delay differential equations */

int Nout=1;
int nr[1]={0};
double ytau[1] = {0.0};

static double yini[4] = {0.0, 0.0, 0.0, 0.0}; /*Array of initial state variables*/

/*----- Initializers */
void initmod1comp (void (* odeparms)(int *, double *))
{
  int N=4;
  odeparms(&N, parms);
}

/* Calling R code will ensure that input y has same
   dimension as yini */
void initState (double *y)
{
  int i;

  for (i = 0; i < sizeof(yini) / sizeof(yini[0]); i++)
  {
    yini[i] = y[i];
  }
}

void getParms1comp (double *inParms, double *out, int *nout) {
/*----- Model scaling */

  int i;

  for (i = 0; i < *nout; i++) {
    parms[i] = inParms[i];
  }


  vdist = vdist * BW ;
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

  ydot[ID_Acompartment] = kgutabs * y[ID_Agutlumen] - ke * y[ID_Acompartment] ;

  ydot[ID_Ametabolized] = ke * y[ID_Acompartment] ;

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
