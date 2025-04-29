/* model2comp-raw.c for R deSolve package
   ___________________________________________________

   Model File:  model2comp.model

   Date:  Sun Jul 11 20:35:26 2021

   Created by:  "mod v6.1.0"
    -- a model preprocessor by Don Maszle
   ___________________________________________________

   Copyright (c) 1993-2019 Free Software Foundation, Inc.

   Model calculations for compartmental model:

   5 States:
     Agutlumen = 0.0,
     Acompartment = 0.0,
     Arest = 0.0,
     Ametabolized = 0.0,
     AUC = 0.0,

   2 Outputs:
    "Ccompartment",
    "Crest",

   0 Inputs:

   5 Parameters:
     Vdist = 0,
     Cltotal = 0,
     kgutabs = 1,
     Vrest = 0,
     Qrest = 0,
*/

#include <R.h>
#include <Rinternals.h>
#include <Rdefines.h>
#include <R_ext/Rdynload.h>

/* Model variables: States */
#define ID_Agutlumen 0x00000
#define ID_Acompartment 0x00001
#define ID_Arest 0x00002
#define ID_Ametabolized 0x00003
#define ID_AUC 0x00004

/* Model variables: Outputs */
#define ID_Ccompartment 0x00000
#define ID_Crest 0x00001

/* Parameters */
static double parms[5];

#define Vdist parms[0]
#define Cltotal parms[1]
#define kgutabs parms[2]
#define Vrest parms[3]
#define Qrest parms[4]

/* Forcing (Input) functions */
static double forc[0];


/* Function definitions for delay differential equations */

int Nout=1;
int nr[1]={0};
double ytau[1] = {0.0};

static double yini[5] = {0.0, 0.0, 0.0, 0.0, 0.0}; /*Array of initial state variables*/

void lagvalue(double T, int *nr, int N, double *ytau) {
  static void(*fun)(double, int*, int, double*) = NULL;
  if (fun == NULL)
    fun = (void(*)(double, int*, int, double*))R_GetCCallable("deSolve", "lagvalue");
  return fun(T, nr, N, ytau);
}

double CalcDelay(int hvar, double dTime, double delay) {
  double T = dTime-delay;
  if (dTime > delay){
    nr[0] = hvar;
    lagvalue( T, nr, Nout, ytau );
}
  else{
    ytau[0] = yini[hvar];
}
  return(ytau[0]);
}

/*----- Initializers */
void initmod (void (* odeparms)(int *, double *))
{
  int N=5;
  odeparms(&N, parms);
}

void initforc (void (* odeforcs)(int *, double *))
{
  int N=0;
  odeforcs(&N, forc);
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

void getParms (double *inParms, double *out, int *nout) {
/*----- Model scaling */

  int i;

  for (i = 0; i < *nout; i++) {
    parms[i] = inParms[i];
  }


  kgutabs = kgutabs * 24 ;
  Cltotal = Cltotal * 24 ;

  for (i = 0; i < *nout; i++) {
    out[i] = parms[i];
  }
  }
/*----- Dynamics section */

void derivs (int *neq, double *pdTime, double *y, double *ydot, double *yout, int *ip)
{

  yout[ID_Ccompartment] = y[ID_Acompartment] / Vdist ;

  yout[ID_Crest] = y[ID_Arest] / Vrest ;

  ydot[ID_Agutlumen] = - kgutabs * y[ID_Agutlumen] ;

  ydot[ID_Acompartment] = kgutabs * y[ID_Agutlumen] - Cltotal * yout[ID_Ccompartment] + Qrest * ( yout[ID_Crest] - yout[ID_Ccompartment] ) ;

  ydot[ID_Arest] = Qrest * ( yout[ID_Ccompartment] - yout[ID_Crest] ) ;

  ydot[ID_Ametabolized] = Cltotal * yout[ID_Ccompartment] ;

  ydot[ID_AUC] = yout[ID_Ccompartment] ;

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

