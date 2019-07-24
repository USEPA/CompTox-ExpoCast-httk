/* 3comp.C for R deSolve package
   ___________________________________________________

   Model File:  3compPBPKmodel.model

   Date:  Wed Jul 24 16:26:21 2019

   Created by:  "c:/users/jwambaug/MCSim under R/mod/mod v6.1.0"
    -- a model preprocessor by Don Maszle
   ___________________________________________________

   Copyright (c) 1993-2019 Free Software Foundation, Inc.

   Model calculations for compartmental model:

   6 States:
     Aintestine = 0.0,
     Aportven = 0.0,
     Aliver = 0.0,
     Asyscomp = 0.0,
     Ametabolized = 0.0,
     Atubules = 0.0,

   3 Outputs:
    "Cportven",
    "Cliver",
    "Csyscomp",

   0 Inputs:

   19 Parameters:
     BW = 0,
     CLmetabolismc = 0,
     kgutabs = 0,
     Qcardiacc = 0,
     Qgfrc = 0,
     Qgutf = 0,
     Qliverf = 0,
     Vportvenc = 0,
     Vliverc = 0,
     Vsyscompc = 0,
     Fraction_unbound_plasma = 0,
     CLmetabolism = 0.0,
     Qcardiac = 0,
     Qgfr = 0.0,
     Qgut = 0.0,
     Qliver = 0.0,
     Kliver2plasma = 0,
     Krest2plasma = 0,
     Ratioblood2plasma = 0,
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

/* Model variables: Outputs */
#define ID_Cportven 0x00000
#define ID_Cliver 0x00001
#define ID_Csyscomp 0x00002

/* Parameters */
static double parms[19];

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
#define Fraction_unbound_plasma parms[10]
#define CLmetabolism parms[11]
#define Qcardiac parms[12]
#define Qgfr parms[13]
#define Qgut parms[14]
#define Qliver parms[15]
#define Kliver2plasma parms[16]
#define Krest2plasma parms[17]
#define Ratioblood2plasma parms[18]

/* Forcing (Input) functions */
static double forc[0];


/* Function definitions for delay differential equations */

int Nout=1;
int nr[1]={0};
double ytau[1] = {0.0};

static double yini[6] = {0.0, 0.0, 0.0, 0.0, 0.0, 0.0}; /*Array of initial state variables*/

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
  int N=19;
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

  /* local */ double Vportven;
  /* local */ double Vsyscomp;
  /* local */ double Vliver;
  int i;

  for (i = 0; i < *nout; i++) {
    parms[i] = inParms[i];
  }


  kgutabs = kgutabs * 24 ;
  CLmetabolism = CLmetabolismc * 24 * BW ;
  Qcardiac = Qcardiacc * 24 * BW ;
  Qgfr = Qgfrc * BW * 24 ;
  Qgut = Qcardiac * Qgutf ;
  Qliver = Qcardiac * Qliverf ;

  for (i = 0; i < *nout; i++) {
    out[i] = parms[i];
  }
  }
/*----- Dynamics section */

void derivs (int *neq, double *pdTime, double *y, double *ydot, double *yout, int *ip)
{

  yout[ID_Cportven] = y[ID_Aportven] / Vportven ;

  yout[ID_Cliver] = y[ID_Aliver] / Vliver ;

  yout[ID_Csyscomp] = y[ID_Asyscomp] / Vsyscomp ;

  ydot[ID_Aintestine] = - kgutabs * y[ID_Aintestine] ;

  ydot[ID_Aportven] = kgutabs * y[ID_Aintestine] + Qgut * ( y[ID_Asyscomp] / Vsyscomp * Ratioblood2plasma / Fraction_unbound_plasma / Krest2plasma - y[ID_Aportven] / Vportven ) ;

  ydot[ID_Aliver] = Qgut * y[ID_Aportven] / Vportven + Qliver * y[ID_Asyscomp] / Vsyscomp * Ratioblood2plasma / Fraction_unbound_plasma / Krest2plasma - ( Qliver + Qgut ) * y[ID_Aliver] / Vliver * Ratioblood2plasma / Fraction_unbound_plasma / Kliver2plasma - CLmetabolism / Kliver2plasma * y[ID_Aliver] / Vliver ;

  ydot[ID_Asyscomp] = ( Qgut + Qliver ) * y[ID_Aliver] / Vliver * Ratioblood2plasma / Fraction_unbound_plasma / Kliver2plasma - Qgut * y[ID_Asyscomp] / Vsyscomp * Ratioblood2plasma / Fraction_unbound_plasma / Krest2plasma - Qliver * y[ID_Asyscomp] / Vsyscomp * Ratioblood2plasma / Fraction_unbound_plasma / Krest2plasma - Qgfr / Krest2plasma * y[ID_Asyscomp] / Vsyscomp ;

  ydot[ID_Ametabolized] = CLmetabolism / Kliver2plasma * y[ID_Aliver] / Vliver ;

  ydot[ID_Atubules] = Qgfr / Krest2plasma * y[ID_Asyscomp] / Vsyscomp ;

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

