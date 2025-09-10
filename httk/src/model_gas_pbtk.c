/* model_gas_pbtk_raw.c for R deSolve package
   ___________________________________________________

   Model File:  model_gas_pbtk.model

   Date:  Thu Sep 16 09:47:48 2021

   Created by:  "mod v6.1.0"
    -- a model preprocessor by Don Maszle
   ___________________________________________________

   Copyright (c) 1993-2019 Free Software Foundation, Inc.

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
     Ainh = 0.0,
     Aexh = 0.0,
     Amuc = 0.0,

   16 Outputs:
    "Cgut",
    "Cliver",
    "Cven",
    "Clung",
    "Cart",
    "Crest",
    "Ckidney",
    "Cplasma",
    "Aplasma",
    "Calvppmv",
    "Calv",
    "Cendexhppmv",
    "Cendexh",
    "Cmixexhppmv",
    "Cmixexh",
    "Cmuc",

   1 Input:
     Cinhppmv (forcing function)

   54 Parameters:
     BW = 70,
     Clmetabolismc = 0.203,
     vmax = 0,
     km = 1,
     hematocrit = 0.44,
     kgutabs = 1,
     Kkidney2pu = 0,
     Kliver2pu = 0,
     Krest2pu = 0,
     Kgut2pu = 0,
     Klung2pu = 0,
     Qcardiacc = 4.8,
     Qgfrc = 0.108,
     Qgutf = 0.205,
     Qkidneyf = 0.221,
     Qliverf = 0.0536,
     Qlungf = 0,
     Vartc = 0.0487,
     Vgutc = 0.0158,
     Vkidneyc = 0.00119,
     Vliverc = 0.02448,
     Vlungc = 0.00723,
     Vrestc = 0.77654,
     Vvenc = 0.0487,
     Fraction_unbound_plasma = 0.0682,
     Rblood2plasma = 0.0,
     Clmetabolism = 0.0,
     Qcardiac = 0.0,
     Qgfr = 0.0,
     Qgut = 0.0,
     Qkidney = 0.0,
     Qliver = 0.0,
     Qlung = 0.0,
     Qrest = 0.0,
     Vart = 0.0,
     Vgut = 0.0,
     Vkidney = 0.0,
     Vliver = 0.0,
     Vlung = 0.0,
     Vrest = 0.0,
     Vven = 0.0,
     Qalvc = 0.0,
     Qalv = 0.0,
     Kblood2air = 0.0,
     InhMag = 0.0,
     Period = 0.0,
     Exposure = 0.0,
     kUrtc = 11.0,
     kUrt = 0.0,
     Kmuc2air = 0.0,
     Vmucc = 0.0001,
     Vmuc = 0.0,
     Vmax = 0.0,
     Km = 1.0,
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
#define ID_Ainh 0x0000b
#define ID_Aexh 0x0000c
#define ID_Amuc 0x0000d

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
#define ID_Calvppmv 0x00009
#define ID_Calv 0x0000a
#define ID_Cendexhppmv 0x0000b
#define ID_Cendexh 0x0000c
#define ID_Cmixexhppmv 0x0000d
#define ID_Cmixexh 0x0000e
#define ID_Cmuc 0x0000f

/* Parameters */
static double parms[54];

#define BW parms[0]
#define Clmetabolismc parms[1]
#define vmax parms[2]
#define km parms[3]
#define hematocrit parms[4]
#define kgutabs parms[5]
#define Kkidney2pu parms[6]
#define Kliver2pu parms[7]
#define Krest2pu parms[8]
#define Kgut2pu parms[9]
#define Klung2pu parms[10]
#define Qcardiacc parms[11]
#define Qgfrc parms[12]
#define Qgutf parms[13]
#define Qkidneyf parms[14]
#define Qliverf parms[15]
#define Qlungf parms[16]
#define Vartc parms[17]
#define Vgutc parms[18]
#define Vkidneyc parms[19]
#define Vliverc parms[20]
#define Vlungc parms[21]
#define Vrestc parms[22]
#define Vvenc parms[23]
#define Fraction_unbound_plasma parms[24]
#define Rblood2plasma parms[25]
#define Clmetabolism parms[26]
#define Qcardiac parms[27]
#define Qgfr parms[28]
#define Qgut parms[29]
#define Qkidney parms[30]
#define Qliver parms[31]
#define Qlung parms[32]
#define Qrest parms[33]
#define Vart parms[34]
#define Vgut parms[35]
#define Vkidney parms[36]
#define Vliver parms[37]
#define Vlung parms[38]
#define Vrest parms[39]
#define Vven parms[40]
#define Qalvc parms[41]
#define Qalv parms[42]
#define Kblood2air parms[43]
#define InhMag parms[44]
#define Period parms[45]
#define Exposure parms[46]
#define kUrtc parms[47]
#define kUrt parms[48]
#define Kmuc2air parms[49]
#define Vmucc parms[50]
#define Vmuc parms[51]
#define Vmax parms[52]
#define Km parms[53]

/* Forcing (Input) functions */
static double forc[1];

#define Cinhppmv forc[0]

/* Function definitions for delay differential equations */
//
//int Nout=1;
//int nr[1]={0};
//double ytau[1] = {0.0};
//
//static double yini[14] = {0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0}; /*Array of initial state variables*/

//void lagvalue(double T, int *nr, int N, double *ytau) {
//  static void(*fun)(double, int*, int, double*) = NULL;
//  if (fun == NULL)
//    fun = (void(*)(double, int*, int, double*))R_GetCCallable("deSolve", "lagvalue");
//  return fun(T, nr, N, ytau);
//}
//
//double CalcDelay(int hvar, double dTime, double delay) {
//  double T = dTime-delay;
//  if (dTime > delay){
//    nr[0] = hvar;
//    lagvalue( T, nr, Nout, ytau );
//}
//  else{
//    ytau[0] = yini[hvar];
//}
//  return(ytau[0]);
//}

/*----- Initializers */
void initmod_gas_pbtk (void (* odeparms)(int *, double *))
{
  int N=54;
  odeparms(&N, parms);
}

void initforc_gas_pbtk (void (* odeforcs)(int *, double *))
{
  int N=1;
  odeforcs(&N, forc);
}


///* Calling R code will ensure that input y has same
//   dimension as yini */
//void initState_gas_pbtk (double *y)
//{
//  int i;
//
//  for (i = 0; i < sizeof(yini) / sizeof(yini[0]); i++)
//  {
//    yini[i] = y[i];
//  }
//}

void getParms_gas_pbtk (double *inParms, double *out, int *nout) {
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
  Qlung = Qcardiac * Qlungf ;
  Qrest = Qcardiac - ( Qgut + Qkidney + Qliver + Qlung ) ;
  Vart = Vartc * BW ;
  Vgut = Vgutc * BW ;
  Vkidney = Vkidneyc * BW ;
  Vliver = Vliverc * BW ;
  Vlung = Vlungc * BW ;
  Vrest = Vrestc * BW ;
  Vven = Vvenc * BW ;
  Qalv = Qalvc * 24 * pow ( BW , 0.75 ) ;
  kUrt = fmin ( kUrtc , Qalvc ) * pow ( BW , 0.75 ) * 24 ;
  Vmuc = Vmucc * BW ;
  Vmax = vmax * 60 * 24 ;
  Km = km ;

  for (i = 0; i < *nout; i++) {
    out[i] = parms[i];
  }
  }
/*----- Dynamics section */

void derivs_gas_pbtk (int *neq, double *pdTime, double *y, double *ydot, double *yout, int *ip)
{
  /* local */ double Cinh;
  /* local */ double Rin_oral;
  /* local */ double Rout_gfr;
  /* local */ double Rout_metabolism;
  /* local */ double Rin_mucus;
  /* local */ double Rout_mucus;
  /* local */ double Rin_alv;
  /* local */ double Rout_alv;
  
  Cinh = Cinhppmv / 24.45 ; // ppmv -> umol/L
  
  // OUTPUTS (RHS can contain vector y and parameters):
  yout[ID_Cgut] = y[ID_Agut] / Vgut ;
  yout[ID_Cgut] = (yout[ID_Cgut] >= 0.0 ? yout[ID_Cgut] : 0.0) ;

  yout[ID_Cliver] = y[ID_Aliver] / Vliver ;
  yout[ID_Cliver] = (yout[ID_Cliver] >= 0.0 ? yout[ID_Cliver] : 0.0) ;

  yout[ID_Cven] = y[ID_Aven] / Vven ;
  yout[ID_Cven] = (yout[ID_Cven] >= 0.0 ? yout[ID_Cven] : 0.0) ;

  yout[ID_Clung] = y[ID_Alung] / Vlung ;
  yout[ID_Clung] = (yout[ID_Clung] >= 0.0 ? yout[ID_Clung] : 0.0) ;

  yout[ID_Cart] = y[ID_Aart] / Vart ;
  yout[ID_Cart] = (yout[ID_Cart] >= 0.0 ? yout[ID_Cart] : 0.0) ;

  yout[ID_Crest] = y[ID_Arest] / Vrest ;
  yout[ID_Crest] = (yout[ID_Crest] >= 0.0 ? yout[ID_Crest] : 0.0) ;

  yout[ID_Ckidney] = y[ID_Akidney] / Vkidney ;
  yout[ID_Ckidney] = (yout[ID_Ckidney] >= 0.0 ? yout[ID_Ckidney] : 0.0) ;

  yout[ID_Cplasma] = y[ID_Aven] / Vven / Rblood2plasma ;
  yout[ID_Cplasma] = (yout[ID_Cplasma] >= 0.0 ? yout[ID_Cplasma] : 0.0) ;

  yout[ID_Aplasma] = y[ID_Aven] / Rblood2plasma * ( 1 - hematocrit ) ;
  yout[ID_Aplasma] = (yout[ID_Aplasma] >= 0.0 ? yout[ID_Aplasma] : 0.0) ;

  yout[ID_Calv] = yout[ID_Cart] / Kblood2air ;
  yout[ID_Calv] = (yout[ID_Calv] >= 0.0 ? yout[ID_Calv] : 0.0) ;

  yout[ID_Calvppmv] = yout[ID_Calv] * 24.45 ; // umol/L -> ppmv

  yout[ID_Cendexh] = ( Qalv * yout[ID_Calv] + kUrt * ( ( yout[ID_Cmuc] / Kmuc2air ) - yout[ID_Calv] ) ) / Qalv ;
  yout[ID_Cendexh] = (yout[ID_Cendexh] >= 0.0 ? yout[ID_Cendexh] : 0.0) ;
   
  yout[ID_Cendexhppmv] = yout[ID_Cendexh] * 24.45 ;  // umol/L -> ppmv
  yout[ID_Cendexhppmv] = (yout[ID_Cendexhppmv] >= 0.0 ? yout[ID_Cendexhppmv] : 0.0) ;

  yout[ID_Cmixexh] = 0.7 * yout[ID_Cendexh] + 0.3 * Cinh ;
  yout[ID_Cmixexh] = (yout[ID_Cmixexh] >= 0.0 ? yout[ID_Cmixexh] : 0.0) ;

  yout[ID_Cmixexhppmv] = yout[ID_Cmixexh] * 24.45 ;  // umol/L -> ppmv
  yout[ID_Cmixexhppmv] = (yout[ID_Cmixexhppmv] >= 0.0 ? yout[ID_Cmixexhppmv] : 0.0) ;

  yout[ID_Cmuc] = y[ID_Amuc] / Vmuc ;
  yout[ID_Cmuc] = (yout[ID_Cmuc] >= 0.0 ? yout[ID_Cmuc] : 0.0) ;
  
  // LOCAL VARIABLES (RHS can contain vector y and parameters):
  Rin_oral = (y[ID_Agutlumen] > 0.0 ?
              kgutabs * y[ID_Agutlumen] :
              0.0);
  
  Rout_gfr = (y[ID_Aart] > 0.0 ?
              Qgfr * Fraction_unbound_plasma / Rblood2plasma * y[ID_Aart] / Vart :
              0.0); 

  Rout_metabolism = (yout[ID_Cliver] >= 0.0 ?
              Clmetabolism * yout[ID_Cliver] / Kliver2pu / Fraction_unbound_plasma :
              0.0);  
              
  Rin_mucus = kUrt * ( Cinh - ( yout[ID_Cmuc] / Kmuc2air ) );
  Rin_mucus = (Rin_mucus >= 0.0 ?
              Rin_mucus :
              0.0); 
              
  Rout_mucus = kUrt * ( ( yout[ID_Cmuc] / Kmuc2air ) - yout[ID_Calv] ) ;
  Rout_mucus = (Rout_mucus >= 0.0 ?
              Rout_mucus :
              0.0); 
              
  Rin_alv = Qalv * Cinh;
  Rin_alv = (Rin_alv >= 0.0 ?
             Rin_alv :
             0.0);
             
  Rout_alv = Qalv * yout[ID_Calv];
  Rout_alv = (Rout_alv > 0.0 ?
              Rout_alv :
              0.0);
              
  // DERIVATIVE (one ydot for each element of y, RHS can contain vectors yout, y, and parameters):
  ydot[ID_Agutlumen] = - Rin_oral ;

  ydot[ID_Agut] = Rin_oral + Qgut * ( yout[ID_Cart] - yout[ID_Cgut] * Rblood2plasma / Kgut2pu / Fraction_unbound_plasma ) ;

  ydot[ID_Aliver] = Qliver * yout[ID_Cart] + Qgut * yout[ID_Cgut] * Rblood2plasma / Kgut2pu / Fraction_unbound_plasma - ( Qliver + Qgut ) * yout[ID_Cliver] / Kliver2pu / Fraction_unbound_plasma * Rblood2plasma - Rout_metabolism - Vmax * yout[ID_Cliver] / Kliver2pu / ( Km + yout[ID_Cliver] / Kliver2pu ) ;

  ydot[ID_Aven] = ( ( Qliver + Qgut ) * yout[ID_Cliver] / Kliver2pu + Qkidney * yout[ID_Ckidney] / Kkidney2pu + Qrest * yout[ID_Crest] / Krest2pu + Qlung * yout[ID_Clung] / Klung2pu ) * Rblood2plasma / Fraction_unbound_plasma - Qcardiac * yout[ID_Cven] ;

  ydot[ID_Alung] = Qlung * ( yout[ID_Cart] - yout[ID_Clung] * Rblood2plasma / Klung2pu / Fraction_unbound_plasma ) ;

  ydot[ID_Aart] = ( Qcardiac * ( yout[ID_Cven] - yout[ID_Cart] ) ) + ( Rin_alv - Rout_alv ) - Rin_mucus ;

  ydot[ID_Arest] = Qrest * ( yout[ID_Cart] - yout[ID_Crest] * Rblood2plasma / Krest2pu / Fraction_unbound_plasma ) ;

  ydot[ID_Akidney] = Qkidney * yout[ID_Cart] - Qkidney * yout[ID_Ckidney] / Kkidney2pu * Rblood2plasma / Fraction_unbound_plasma - Rout_gfr ;

  ydot[ID_Atubules] = Rout_gfr ;

  ydot[ID_Ametabolized] = Rout_metabolism + Vmax * yout[ID_Cliver] / Kliver2pu / ( Km + yout[ID_Cliver] / Kliver2pu ) ;

  ydot[ID_AUC] = yout[ID_Cven] / Rblood2plasma ;

  ydot[ID_Ainh] = ( Rin_alv - Rout_alv ) - Rin_mucus ;
  
  ydot[ID_Aexh] = Rout_alv + Rout_mucus ;
               
  ydot[ID_Amuc] = Rin_mucus - Rout_mucus ;

    } /* derivs */


/*----- Jacobian calculations: */
void jac_gas_pbtk (int *neq, double *t, double *y, int *ml, int *mu, double *pd, int *nrowpd, double *yout, int *ip)
{

} /* jac */


/*----- Events calculations: */
void event_gas_pbtk (int *n, double *t, double *y)
{

} /* event */

/*----- Roots calculations: */
void root_gas_pbtk (int *neq, double *t, double *y, int *ng, double *gout, double *out, int *ip)
{

} /* root */

