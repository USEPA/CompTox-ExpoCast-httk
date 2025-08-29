/* dermal_1subcomp_model.c for R deSolve package
   ___________________________________________________

   Model File:  dermal_1subcomp.model

   Date:  Wed Mar 09 10:12:41 2022

   Created by:  "C:/Users/AMEADE/modbin/mod.exe v6.1.0"
    -- a model preprocessor by Don Maszle
   ___________________________________________________

   Copyright (c) 1993-2019 Free Software Foundation, Inc.

   Model calculations for compartmental model:

   17 States:
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
     Askin_exposed = 0.0,
     Askin_unexposed = 0.0,
     Avehicle = 0.0,
     Ain = 0.0,
     Aexhaled = 0.0,
     Cvehicle_infinite = 0.0,

   12 Outputs:
    "Cgut",
    "Cliver",
    "Cven",
    "Clung",
    "Cart",
    "Crest",
    "Ckidney",
    "Cplasma",
    "Aplasma",
    "Cskin_exposed",
    "Cskin_unexposed",
    "Cvehicle",

   1 Input:
     Vvehicle (forcing function)

   56 Parameters:
     skin_depth = 0,
     InfiniteDose = 0,
     Fskin_exposed = 0,
     totalSA = 0,
     SA_exposed = 0,
     P = 0,
     Kskin2vehicle = 0,
     BW = 70,
     Clmetabolismc = 0.0,
     hematocrit = 0.0,
     kgutabs = 0.0,
     Kkidney2pu = 0,
     Kliver2pu = 0,
     Krest2pu = 0,
     Kgut2pu = 0,
     Klung2pu = 0,
     Kskin2pu = 0,
     Kblood2air = 0,
     Qalvc = 0,
     Qcardiacc = 0,
     Qgfrc = 0,
     Qskinf = 0,
     Qgutf = 0,
     Qkidneyf = 0,
     Qliverf = 0,
     Vartc = 0,
     Vgutc = 0,
     Vkidneyc = 0,
     Vliverc = 0,
     Vlungc = 0,
     Vrestc = 0,
     Vvenc = 0,
     Vskinc = 0,
     Fraction_unbound_plasma = 0.0,
     Rblood2plasma = 0.0,
     Clmetabolism = 0.0,
     Qalv = 0.0,
     Qcardiac = 0.0,
     Qgfr = 0.0,
     Qskin = 0.0,
     Qskin_exposed = 0.0,
     Qskin_unexposed = 0.0,
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
     Vskin = 0.0,
     Vskin_exposed = 0.0,
     Vskin_unexposed = 0.0,
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
#define ID_Askin_exposed 0x0000b
#define ID_Askin_unexposed 0x0000c
#define ID_Avehicle 0x0000d
#define ID_Ain 0x0000e
#define ID_Aexhaled 0x0000f
#define ID_Cvehicle_infinite 0x00010

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
#define ID_Cskin_exposed 0x00009
#define ID_Cskin_unexposed 0x0000a
#define ID_Cvehicle 0x0000b

/* Parameters */
static double parms[56];

#define skin_depth parms[0]
#define InfiniteDose parms[1]
#define Fskin_exposed parms[2]
#define totalSA parms[3]
#define SA_exposed parms[4]
#define P parms[5]
#define Kskin2vehicle parms[6]
#define BW parms[7]
#define Clmetabolismc parms[8]
#define hematocrit parms[9]
#define kgutabs parms[10]
#define Kkidney2pu parms[11]
#define Kliver2pu parms[12]
#define Krest2pu parms[13]
#define Kgut2pu parms[14]
#define Klung2pu parms[15]
#define Kskin2pu parms[16]
#define Kblood2air parms[17]
#define Qalvc parms[18]
#define Qcardiacc parms[19]
#define Qgfrc parms[20]
#define Qskinf parms[21]
#define Qgutf parms[22]
#define Qkidneyf parms[23]
#define Qliverf parms[24]
#define Vartc parms[25]
#define Vgutc parms[26]
#define Vkidneyc parms[27]
#define Vliverc parms[28]
#define Vlungc parms[29]
#define Vrestc parms[30]
#define Vvenc parms[31]
#define Vskinc parms[32]
#define Fraction_unbound_plasma parms[33]
#define Rblood2plasma parms[34]
#define Clmetabolism parms[35]
#define Qalv parms[36]
#define Qcardiac parms[37]
#define Qgfr parms[38]
#define Qskin parms[39]
#define Qskin_exposed parms[40]
#define Qskin_unexposed parms[41]
#define Qgut parms[42]
#define Qkidney parms[43]
#define Qliver parms[44]
#define Qrest parms[45]
#define Vart parms[46]
#define Vgut parms[47]
#define Vkidney parms[48]
#define Vliver parms[49]
#define Vlung parms[50]
#define Vrest parms[51]
#define Vven parms[52]
#define Vskin parms[53]
#define Vskin_exposed parms[54]
#define Vskin_unexposed parms[55]

/* Forcing (Input) functions */
static double forc[1];

#define Vvehicle forc[0]

/*----- Initializers */
void initmod_dermal_1subcomp (void (* odeparms)(int *, double *))
{
  int N=56;
  odeparms(&N, parms);
}

void initforc_dermal_1subcomp (void (* odeforcs)(int *, double *))
{
  int N=1;
  odeforcs(&N, forc);
}

void getParms_dermal_1subcomp (double *inParms, double *out, int *nout) {
/*----- Model scaling */

  int i;

  for (i = 0; i < *nout; i++) {
    parms[i] = inParms[i];
  }


  kgutabs = kgutabs * 24 ;
  Clmetabolism = Clmetabolismc * 24 * BW ;
  Qalv = Qalvc * 24 * pow ( BW , 0.75 ) ;
  Qcardiac = Qcardiacc * 24 * pow ( BW , 0.75 ) ;
  Qgfr = Qgfrc * pow ( BW , 0.75 ) * 24 ;

  Qgut = Qcardiac * Qgutf ;
  Qkidney = Qcardiac * Qkidneyf ;
  Qliver = Qcardiac * Qliverf ;

  SA_exposed = Fskin_exposed * totalSA ;

  Vskin = Vskinc * BW ;
  Vskin_exposed = SA_exposed * skin_depth * 0.001 ;
  Vskin_unexposed = Vskin - Vskin_exposed ;

  Qskin = Qcardiac * Qskinf ;
  Qskin_unexposed = Qskin * Vskin_unexposed / Vskin ;
  Qskin_exposed = Qskin * Vskin_exposed / Vskin ;
  Qrest = Qcardiac - ( Qgut + Qkidney + Qliver + Qskin ) ;

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

void derivs_dermal_1subcomp (int *neq, double *pdTime, double *y, double *ydot, double *yout, int *ip)
{
  /* local */ double Rin_dermal;
  /* local */ double Rin_oral;
  /* local */ double Rout_exhaled;
  /* local */ double Rout_gfr;
  /* local */ double Rout_metabolism;
     
  // OUTPUTS (RHS can contain vector y and parameters):
  yout[ID_Cgut] = y[ID_Agut] / Vgut;

  yout[ID_Cliver] = y[ID_Aliver] / Vliver;

  yout[ID_Cven] = y[ID_Aven] / Vven;

  yout[ID_Clung] = y[ID_Alung] / Vlung;

  yout[ID_Cart] =  y[ID_Aart] / Vart;

  yout[ID_Crest] = y[ID_Arest] / Vrest;

  yout[ID_Ckidney] = y[ID_Akidney] / Vkidney;

  yout[ID_Cplasma] = y[ID_Aven] / Vven / Rblood2plasma;

  yout[ID_Aplasma] = y[ID_Aven] / Rblood2plasma * ( 1 - hematocrit );

  yout[ID_Cskin_unexposed] = y[ID_Askin_unexposed] / Vskin_unexposed;

  yout[ID_Cskin_exposed] = y[ID_Askin_exposed] / Vskin_exposed;

  yout[ID_Cvehicle] = (InfiniteDose ? 
                       y[ID_Cvehicle_infinite] : 
                       ( Vvehicle ? y[ID_Avehicle] / Vvehicle : 0.0 ) ) ;

  // LOCAL VARIABLES (RHS can contain vectors yout, y, and parameters):
  Rin_dermal = (( yout[ID_Cvehicle] - yout[ID_Cskin_exposed] / Kskin2vehicle ) < 0.0 ? 
                 0.0 :
                 ((InfiniteDose > 0.0) | (Vvehicle > 0.0) ? 
                   P * SA_exposed * 24 * 0.001 * ( yout[ID_Cvehicle] - yout[ID_Cskin_exposed] / Kskin2vehicle ) : 
                   0.0)
                 ) ;

  Rin_oral = (y[ID_Agutlumen] > 0.0 ?
              kgutabs * y[ID_Agutlumen] :
              0.0);
  
  Rout_exhaled = (yout[ID_Clung] > 0.0 ?
                  Qalv * yout[ID_Clung] * Rblood2plasma / ( Klung2pu * Fraction_unbound_plasma * Kblood2air ) :
                  0.0);

  Rout_gfr = (y[ID_Aart] > 0.0 ?
              Qgfr * Fraction_unbound_plasma / Rblood2plasma * y[ID_Aart] / Vart :
              0.0); 

  Rout_metabolism = (yout[ID_Cliver] >= 0.0 ?
              Clmetabolism * yout[ID_Cliver] / Kliver2pu :
              0.0); 

  // DERIVATIVE (one ydot for each element of y, RHS can contain vectors yout, y, and parameters):
  ydot[ID_Avehicle] = (yout[ID_Cvehicle] >= 0.0 ?
                       - Rin_dermal * (1 - InfiniteDose) :
                       0.0);
  

  ydot[ID_Ain] = Rin_dermal + Rin_oral;
  
  ydot[ID_Aexhaled] = Rout_exhaled;

  ydot[ID_Askin_exposed] = Qskin_exposed * ( yout[ID_Cart] - yout[ID_Cskin_exposed] * Rblood2plasma / Kskin2pu / Fraction_unbound_plasma ) + Rin_dermal;

  ydot[ID_Askin_unexposed] = Qskin_unexposed * ( yout[ID_Cart] - yout[ID_Cskin_unexposed] * Rblood2plasma / Kskin2pu / Fraction_unbound_plasma);

  ydot[ID_Agutlumen] = -Rin_oral;

  ydot[ID_Agut] = Rin_oral + Qgut * ( yout[ID_Cart] - yout[ID_Cgut] * Rblood2plasma / Kgut2pu / Fraction_unbound_plasma);

  ydot[ID_Aliver] = Qliver * yout[ID_Cart] + Qgut * yout[ID_Cgut] * Rblood2plasma / Kgut2pu / Fraction_unbound_plasma - ( Qliver + Qgut ) * yout[ID_Cliver] / Kliver2pu / Fraction_unbound_plasma * Rblood2plasma - Rout_metabolism;

  ydot[ID_Aven] = ( ( Qliver + Qgut ) * yout[ID_Cliver] / Kliver2pu + Qkidney * yout[ID_Ckidney] / Kkidney2pu + Qrest * yout[ID_Crest] / Krest2pu + Qskin_unexposed * yout[ID_Cskin_unexposed] / Kskin2pu + Qskin_exposed * yout[ID_Cskin_exposed] / Kskin2pu ) * Rblood2plasma / Fraction_unbound_plasma - Qcardiac * yout[ID_Cven];

  ydot[ID_Alung] = Qcardiac * ( yout[ID_Cven] - yout[ID_Clung] * Rblood2plasma / Klung2pu / Fraction_unbound_plasma ) - Rout_exhaled;

  ydot[ID_Aart] = Qcardiac * ( yout[ID_Clung] * Rblood2plasma / Klung2pu / Fraction_unbound_plasma - yout[ID_Cart] );

  ydot[ID_Arest] = Qrest * ( yout[ID_Cart] - yout[ID_Crest] * Rblood2plasma / Krest2pu / Fraction_unbound_plasma );

  ydot[ID_Akidney] = Qkidney * yout[ID_Cart] - Qkidney * yout[ID_Ckidney] / Kkidney2pu * Rblood2plasma / Fraction_unbound_plasma - Rout_gfr;

  ydot[ID_Atubules] = Rout_gfr ;

  ydot[ID_Ametabolized] = Rout_metabolism;

  ydot[ID_AUC] = yout[ID_Cven] / Rblood2plasma;
  
  ydot[ID_Cvehicle_infinite] = 0.0;  

} /* derivs */


/*----- Jacobian calculations: */
void jac_dermal_1subcomp (int *neq, double *t, double *y, int *ml, int *mu, double *pd, int *nrowpd, double *yout, int *ip)
{

} /* jac */


/*----- Events calculations: */
void event_dermal_1subcomp (int *n, double *t, double *y)
{

} /* event */

/*----- Roots calculations: */
void root_dermal_1subcomp (int *neq, double *t, double *y, int *ng, double *gout, double *out, int *ip)
{

} /* root */

