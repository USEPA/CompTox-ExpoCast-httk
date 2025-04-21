/* modelpbtk_lifestage-raw.c for R deSolve package
   ___________________________________________________

   Model File:  modelpbtk_lifestage.model

   Date:  Tue Nov 19 14:24:47 2024

   Created by:  "C:/Users/cthomson/httk-dev/models/mod.exe v6.1.0"
    -- a model preprocessor by Don Maszle
   ___________________________________________________

   Copyright (c) 1993-2019 Free Software Foundation, Inc.

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
    "Cplasma",
    "Aplasma"

   13 Inputs:
     d_BW (forcing function)
     d_Clmetabolismc (forcing function)
     d_hematocrit (forcing function)
     d_Krest2pu (forcing function)
     d_Qcardiacc (forcing function)
     d_Qgfrc (forcing function)
     d_Vartc (forcing function)
     d_Vgutc (forcing function)
     d_Vkidneyc (forcing function)
     d_Vliverc (forcing function)
     d_Vlungc (forcing function)
     d_Vrestc (forcing function)
     d_Vvenc (forcing function)

   37 Parameters:
     BW = 70,
     Clmetabolismc = 0.203,
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
#define ID_c_BW 0x00009
#define ID_c_Clmetabolism 0x0000a
#define ID_c_hematocrit 0x0000b
#define ID_c_Krest2pu 0x0000c
#define ID_c_Qcardiac 0x0000d
#define ID_c_Qgfr 0x0000e
#define ID_c_Qgut 0x0000f
#define ID_c_Qkidney 0x00010
#define ID_c_Qliver 0x00011
#define ID_c_Qrest 0x00012
#define ID_c_Vart 0x00013
#define ID_c_Vgut 0x00014
#define ID_c_Vkidney 0x00015
#define ID_c_Vliver 0x00016
#define ID_c_Vlung 0x00017
#define ID_c_Vrest 0x00018
#define ID_c_Vven 0x00019

/* Parameters */
static double parms[37];

#define BW parms[0]
#define Clmetabolismc parms[1]
#define hematocrit parms[2]
#define kgutabs parms[3]
#define Kkidney2pu parms[4]
#define Kliver2pu parms[5]
#define Krest2pu parms[6]
#define Kgut2pu parms[7]
#define Klung2pu parms[8]
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
#define Rblood2plasma parms[22]
#define Clmetabolism parms[23]
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

/* Forcing (Input) functions */
static double forc[13];

#define d_BW forc[0]
#define d_Clmetabolismc forc[1]
#define d_hematocrit forc[2]
#define d_Krest2pu forc[3]
#define d_Qcardiacc forc[4]
#define d_Qgfrc forc[5]
#define d_Vartc forc[6]
#define d_Vgutc forc[7]
#define d_Vkidneyc forc[8]
#define d_Vliverc forc[9]
#define d_Vlungc forc[10]
#define d_Vrestc forc[11]
#define d_Vvenc forc[12]

/* Function definitions for delay differential equations */

// int Nout=1;
// int nr[1]={0};
// double ytau[1] = {0.0};
// 
// static double yini[11] = {0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0}; /*Array of initial state variables*/
// 
// void lagvalue(double T, int *nr, int N, double *ytau) {
//   static void(*fun)(double, int*, int, double*) = NULL;
//   if (fun == NULL)
//     fun = (void(*)(double, int*, int, double*))R_GetCCallable("deSolve", "lagvalue");
//   return fun(T, nr, N, ytau);
// }
// 
// double CalcDelay(int hvar, double dTime, double delay) {
//   double T = dTime-delay;
//   if (dTime > delay){
//     nr[0] = hvar;
//     lagvalue( T, nr, Nout, ytau );
// }
//   else{
//     ytau[0] = yini[hvar];
// }
//   return(ytau[0]);
// }

/*----- Initializers */
void initmodpbtk_lifestage (void (* odeparms)(int *, double *))
{
  int N=37;
  odeparms(&N, parms);
}

void initforcpbtk_lifestage (void (* odeforcs)(int *, double *))
{
  int N=13;
  odeforcs(&N, forc);
}


/* Calling R code will ensure that input y has same
   dimension as yini */
// void initState (double *y)
// {
//   int i;
// 
//   for (i = 0; i < sizeof(yini) / sizeof(yini[0]); i++)
//   {
//     yini[i] = y[i];
//   }
// }

void getParmspbtk_lifestage (double *inParms, double *out, int *nout) {
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
  Qrest = Qcardiac - ( Qgut + Qkidney + Qliver ) ;
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

void derivspbtk_lifestage (int *neq, double *pdTime, double *y, double *ydot, double *yout, int *ip)
{

  yout[ID_Cgut] = y[ID_Agut] / (Vgut / BW + d_Vgutc) / (BW + d_BW) ;
  
  yout[ID_Cliver] = y[ID_Aliver] / (Vliver / BW + d_Vliverc) / (BW + d_BW) ;
  
  yout[ID_Cven] = y[ID_Aven] / (Vven / BW + d_Vvenc) / (BW + d_BW) ;
  
  yout[ID_Clung] = y[ID_Alung] / (Vlung / BW + d_Vlungc) / (BW + d_BW) ;
  
  yout[ID_Cart] = y[ID_Aart] / (Vart / BW + d_Vartc) / (BW + d_BW) ;
  
  yout[ID_Crest] = y[ID_Arest] / (Vrest / BW + d_Vrestc) / (BW + d_BW) ;
  
  yout[ID_Ckidney] = y[ID_Akidney] / (Vkidney / BW + d_Vkidneyc) / (BW + d_BW) ;
  
  yout[ID_Cplasma] = y[ID_Aven] / (Vven / BW + d_Vvenc) / (BW + d_BW) / Rblood2plasma ;
  
  yout[ID_Aplasma] = y[ID_Aven] / Rblood2plasma * ( 1 - hematocrit - d_hematocrit ) ;
  
  ydot[ID_Agutlumen] = - kgutabs * y[ID_Agutlumen] ;
  
  ydot[ID_Agut] = kgutabs * y[ID_Agutlumen] + Qgutf * (Qcardiacc + d_Qcardiacc) * 24 * pow(BW + d_BW, 0.75) * ( y[ID_Aart] / (Vart / BW + d_Vartc) / (BW + d_BW) - y[ID_Agut] / (Vgut / BW + d_Vgutc) / (BW + d_BW) * Rblood2plasma / Kgut2pu / Fraction_unbound_plasma ) ;
  
  ydot[ID_Aliver] = Qliverf * (Qcardiacc + d_Qcardiacc) * 24 * pow(BW + d_BW, 0.75) * y[ID_Aart] / (Vart / BW + d_Vartc) / (BW + d_BW) + Qgutf * (Qcardiacc + d_Qcardiacc) * 24 * pow(BW + d_BW, 0.75) * y[ID_Agut] / (Vgut / BW + d_Vgutc) / (BW + d_BW) * Rblood2plasma / Kgut2pu / Fraction_unbound_plasma -  (Qliverf + Qgutf) * (Qcardiacc + d_Qcardiacc) * 24 * pow(BW + d_BW, 0.75)  * y[ID_Aliver] / (Vliver / BW + d_Vliverc) / (BW + d_BW) / Kliver2pu / Fraction_unbound_plasma * Rblood2plasma - (Clmetabolism / BW + d_Clmetabolismc * 24) * (BW + d_BW) * y[ID_Aliver] / (Vliver / BW + d_Vliverc) / (BW + d_BW) / Kliver2pu / Fraction_unbound_plasma;
  
  ydot[ID_Aven] = ( ( Qliverf + Qgutf ) * (Qcardiacc + d_Qcardiacc) * 24 * pow(BW + d_BW, 0.75)  * y[ID_Aliver] / (Vliver / BW + d_Vliverc) / (BW + d_BW) / Kliver2pu + Qkidneyf * (Qcardiacc + d_Qcardiacc) * 24 * pow(BW + d_BW, 0.75) * y[ID_Akidney] / (Vkidney / BW + d_Vkidneyc) / (BW + d_BW) / Kkidney2pu + (1 - Qgutf - Qkidneyf - Qliverf) * (Qcardiacc + d_Qcardiacc) * 24 * pow(BW + d_BW, 0.75) * y[ID_Arest] / (Vrest / BW + d_Vrestc) / (BW + d_BW) / (Krest2pu + d_Krest2pu) ) * Rblood2plasma / Fraction_unbound_plasma - (Qcardiacc + d_Qcardiacc) * 24 * pow(BW + d_BW, 0.75) * y[ID_Aven] / (Vven / BW + d_Vvenc) / (BW + d_BW) ;
  
  ydot[ID_Alung] = (Qcardiacc + d_Qcardiacc) * 24 * pow(BW + d_BW, 0.75) * ( y[ID_Aven] / (Vven / BW + d_Vvenc) / (BW + d_BW) - y[ID_Alung] / (Vlung / BW + d_Vlungc) / (BW + d_BW) * Rblood2plasma / Klung2pu / Fraction_unbound_plasma ) ;
  
  ydot[ID_Aart] = (Qcardiacc + d_Qcardiacc) * 24 * pow(BW + d_BW, 0.75) * ( y[ID_Alung] / (Vlung / BW + d_Vlungc) / (BW + d_BW) * Rblood2plasma / Klung2pu / Fraction_unbound_plasma - y[ID_Aart] / (Vart / BW + d_Vartc) / (BW + d_BW) ) ;
  
  ydot[ID_Arest] = (1 - Qgutf - Qkidneyf - Qliverf) * (Qcardiacc + d_Qcardiacc) * 24 * pow(BW + d_BW, 0.75) * ( y[ID_Aart] / (Vart / BW + d_Vartc) / (BW + d_BW) - y[ID_Arest] / (Vrest / BW + d_Vrestc) / (BW + d_BW) * Rblood2plasma / (Krest2pu + d_Krest2pu) / Fraction_unbound_plasma ) ;
  
  ydot[ID_Akidney] = Qkidneyf * (Qcardiacc + d_Qcardiacc) * 24 * pow(BW + d_BW, 0.75) * y[ID_Aart] / (Vart / BW + d_Vartc) / (BW + d_BW) - Qkidneyf * (Qcardiacc + d_Qcardiacc) * 24 * pow(BW + d_BW, 0.75) * y[ID_Akidney] / (Vkidney / BW + d_Vkidneyc) / (BW + d_BW) / Kkidney2pu * Rblood2plasma / Fraction_unbound_plasma - (Qgfrc + d_Qgfrc) * pow(BW + d_BW, 0.75) * 24 * y[ID_Aart] / (Vart / BW + d_Vartc) / (BW + d_BW) / Rblood2plasma * Fraction_unbound_plasma ;
  
  ydot[ID_Atubules] = (Qgfrc + d_Qgfrc) * pow(BW + d_BW, 0.75) * 24 * y[ID_Aart] / (Vart / BW + d_Vartc) / (BW + d_BW) / Rblood2plasma * Fraction_unbound_plasma ;
  
  ydot[ID_Ametabolized] = (Clmetabolism / BW + d_Clmetabolismc * 24) * (BW + d_BW) * y[ID_Aliver] / (Vliver / BW + d_Vliverc) / (BW + d_BW) / Kliver2pu ;
  
  ydot[ID_AUC] = y[ID_Aven] / (Vven / BW + d_Vvenc) / (BW + d_BW) / Rblood2plasma ;
  
  yout[ID_c_BW] = BW + d_BW ;
  
  yout[ID_c_Clmetabolism] = ( Clmetabolismc + d_Clmetabolismc ) * 24 * yout[ID_c_BW] ;
  
  yout[ID_c_hematocrit] = hematocrit + d_hematocrit ;
  
  yout[ID_c_Krest2pu] = Krest2pu + d_Krest2pu ;
  
  yout[ID_c_Qcardiac] = ( Qcardiacc + d_Qcardiacc ) * 24 * pow ( yout[ID_c_BW] , 0.75 ) ;
  
  yout[ID_c_Qgfr] = ( Qgfrc + d_Qgfrc ) * pow ( yout[ID_c_BW] , 0.75 ) * 24 ;
  
  yout[ID_c_Qgut] = yout[ID_c_Qcardiac] * Qgutf ;
  
  yout[ID_c_Qkidney] = yout[ID_c_Qcardiac] * Qkidneyf ;
  
  yout[ID_c_Qliver] = yout[ID_c_Qcardiac] * Qliverf ;
  
  yout[ID_c_Qrest] = yout[ID_c_Qcardiac] - ( yout[ID_c_Qgut] + yout[ID_c_Qkidney] + yout[ID_c_Qliver] ) ;
  
  yout[ID_c_Vart] = ( Vartc + d_Vartc ) * yout[ID_c_BW] ;
  
  yout[ID_c_Vgut] = ( Vgutc + d_Vgutc ) * yout[ID_c_BW] ;
  
  yout[ID_c_Vkidney] = ( Vkidneyc + d_Vkidneyc ) * yout[ID_c_BW] ;
  
  yout[ID_c_Vliver] = ( Vliverc + d_Vliverc ) * yout[ID_c_BW] ;
  
  yout[ID_c_Vlung] = ( Vlungc + d_Vlungc ) * yout[ID_c_BW] ;
  
  yout[ID_c_Vrest] = ( Vrestc + d_Vrestc ) * yout[ID_c_BW] ;
  
  yout[ID_c_Vven] = ( Vvenc + d_Vvenc ) * yout[ID_c_BW] ;

} /* derivs */


/*----- Jacobian calculations: */
void jacpbtk_lifestage (int *neq, double *t, double *y, int *ml, int *mu, double *pd, int *nrowpd, double *yout, int *ip)
{

} /* jac */


/*----- Events calculations: */
void eventpbtk_lifestage (int *n, double *t, double *y)
{

} /* event */

/*----- Roots calculations: */
void rootpbtk_lifestage (int *neq, double *t, double *y, int *ng, double *gout, double *out, int *ip)
{

} /* root */

