/* dermal.c for R deSolve package
   ___________________________________________________

   Model File:  dermal.model

   Date:  Wed Mar 09 12:09:11 2022

   Created by:  "C:/Users/AMEADE/modbin/mod.exe v6.1.0"
    -- a model preprocessor by Don Maszle
   ___________________________________________________

   Copyright (c) 1993-2019 Free Software Foundation, Inc.

   Model calculations for compartmental model:

   19 States:
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
     Askin_sc_exposed = 0.0,
     Askin_sc_unexposed = 0.0,
     Askin_ed_exposed = 0.0,
     Askin_ed_unexposed = 0.0,
     Avehicle = 0.0,
     Ain = 0.0,
     Aexhaled = 0.0,
     Cvehicle_infinite = 0.0,

   14 Outputs:
    "Cgut",
    "Cliver",
    "Cven",
    "Clung",
    "Cart",
    "Crest",
    "Ckidney",
    "Cplasma",
    "Aplasma",
    "Cskin_sc_exposed",
    "Cskin_sc_unexposed",
    "Cskin_ed_exposed",
    "Cskin_ed_unexposed",
    "Cvehicle",

   1 Input:
     Vvehicle (forcing function)

   68 Parameters:
     skin_depth = 0,
     InfiniteDose = 0,
     Fskin_depth_sc = 0,
     Fskin_depth_ed = 0,
     Pvehicle2sc = 0,
     Psc2ed = 0,
     Fskin_exposed = 0,
     totalSA = 0,
     SA_exposed = 0,
     BW = 0,
     Clmetabolismc = 0.0,
     hematocrit = 0.0,
     kgutabs = 0.0,
     Ksc2vehicle = 0,
     Ksc2ed = 0,
     Ked2pu = 0,
     Kkidney2pu = 0,
     Kliver2pu = 0,
     Krest2pu = 0,
     Kgut2pu = 0,
     Klung2pu = 0,
     Kblood2air = 0,
     Qalvc = 0,
     Qcardiacc = 0.0,
     Qgfrc = 0.0,
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
     Vskin_scc = 0,
     Vskin_edc = 0,
     Fraction_unbound_plasma = 0.0,
     Rblood2plasma = 0.0,
     Clmetabolism = 0.0,
     Qalv = 0.0,
     Qcardiac = 0.0,
     Qskin = 0,
     Qskin_exposed = 0,
     Qskin_unexposed = 0,
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
     Vskin = 0,
     Vskin_exposed = 0.0,
     Vskin_unexposed = 0.0,
     Vskin_sc = 0,
     Vskin_sc_exposed = 0,
     Vskin_sc_unexposed = 0,
     Vskin_ed = 0,
     Vskin_ed_exposed = 0,
     Vskin_ed_unexposed = 0,
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
#define ID_Askin_sc_exposed 0x0000b
#define ID_Askin_sc_unexposed 0x0000c
#define ID_Askin_ed_exposed 0x0000d
#define ID_Askin_ed_unexposed 0x0000e
#define ID_Avehicle 0x0000f
#define ID_Ain 0x00010
#define ID_Aexhaled 0x00011
#define ID_Cvehicle_infinite 0x00012

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
#define ID_Cskin_sc_exposed 0x00009
#define ID_Cskin_sc_unexposed 0x0000a
#define ID_Cskin_ed_exposed 0x0000b
#define ID_Cskin_ed_unexposed 0x0000c
#define ID_Cvehicle 0x0000d

/* Parameters */
static double parms[68];

#define skin_depth parms[0]
#define InfiniteDose parms[1]
#define Fskin_depth_sc parms[2]
#define Fskin_depth_ed parms[3]
#define Pvehicle2sc parms[4]
#define Psc2ed parms[5]
#define Fskin_exposed parms[6]
#define totalSA parms[7]
#define SA_exposed parms[8]
#define BW parms[9]
#define Clmetabolismc parms[10]
#define hematocrit parms[11]
#define kgutabs parms[12]
#define Ksc2vehicle parms[13]
#define Ksc2ed parms[14]
#define Ked2pu parms[15]
#define Kkidney2pu parms[16]
#define Kliver2pu parms[17]
#define Krest2pu parms[18]
#define Kgut2pu parms[19]
#define Klung2pu parms[20]
#define Kblood2air parms[21]
#define Qalvc parms[22]
#define Qcardiacc parms[23]
#define Qgfrc parms[24]
#define Qskinf parms[25]
#define Qgutf parms[26]
#define Qkidneyf parms[27]
#define Qliverf parms[28]
#define Vartc parms[29]
#define Vgutc parms[30]
#define Vkidneyc parms[31]
#define Vliverc parms[32]
#define Vlungc parms[33]
#define Vrestc parms[34]
#define Vvenc parms[35]
#define Vskinc parms[36]
#define Vskin_scc parms[37]
#define Vskin_edc parms[38]
#define Fraction_unbound_plasma parms[39]
#define Rblood2plasma parms[40]
#define Clmetabolism parms[41]
#define Qalv parms[42]
#define Qcardiac parms[43]
#define Qskin parms[44]
#define Qskin_exposed parms[45]
#define Qskin_unexposed parms[46]
#define Qgfr parms[47]
#define Qgut parms[48]
#define Qkidney parms[49]
#define Qliver parms[50]
#define Qrest parms[51]
#define Vart parms[52]
#define Vgut parms[53]
#define Vkidney parms[54]
#define Vliver parms[55]
#define Vlung parms[56]
#define Vrest parms[57]
#define Vven parms[58]
#define Vskin parms[59]
#define Vskin_exposed parms[60]
#define Vskin_unexposed parms[61]
#define Vskin_sc parms[62]
#define Vskin_sc_exposed parms[63]
#define Vskin_sc_unexposed parms[64]
#define Vskin_ed parms[65]
#define Vskin_ed_exposed parms[66]
#define Vskin_ed_unexposed parms[67]

/* Forcing (Input) functions */
static double forc[1];

#define Vvehicle forc[0]

/*----- Initializers */
void initmod_dermal (void (* odeparms)(int *, double *))
{
  int N=68;
  odeparms(&N, parms);
}

void initforc_dermal (void (* odeforcs)(int *, double *))
{
  int N=1;
  odeforcs(&N, forc);
}

void getParms_dermal (double *inParms, double *out, int *nout) {
/*----- Model scaling */

  int i;

  for (i = 0; i < *nout; i++) {
    parms[i] = inParms[i];
  }


  Vskin_edc = Vskinc - Vskin_scc ;
  Fskin_depth_ed = 1 - Fskin_depth_sc ;

  kgutabs = kgutabs * 24 ;
  Clmetabolism = Clmetabolismc * 24 * BW ;
  Qalv = Qalvc * 24 * pow ( BW , 0.75 ) ;
  Qcardiac = Qcardiacc * 24 * pow ( BW , 0.75 ) ;
  Qgfr = Qgfrc * pow ( BW , 0.75 ) * 24 ;

  Qgut = Qcardiac * Qgutf ;
  Qkidney = Qcardiac * Qkidneyf ;
  Qliver = Qcardiac * Qliverf ;

  SA_exposed = Fskin_exposed * totalSA ;

  Vskin = BW * Vskinc ;
  Vskin_exposed = SA_exposed * skin_depth * 0.001 ;
  Vskin_unexposed = Vskin - Vskin_exposed ;
  Vskin_sc = Vskin * Fskin_depth_sc ;
  Vskin_sc_exposed = Vskin_sc * Fskin_exposed ;
  Vskin_sc_unexposed = Vskin_sc * ( 1 - Fskin_exposed ) ;
  Vskin_ed = Vskin * Fskin_depth_ed ;
  Vskin_ed_exposed = Vskin_ed * Fskin_exposed ;
  Vskin_ed_unexposed = Vskin_ed * ( 1 - Fskin_exposed ) ;

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

void derivs_dermal (int *neq, double *pdTime, double *y, double *ydot, double *yout, int *ip)
{
  /* local */ double Rin_dermal;
  /* local */ double Rin_oral;
  /* local */ double Rout_exhaled;

  yout[ID_Cgut] = y[ID_Agut] / Vgut ;

  yout[ID_Cliver] = y[ID_Aliver] / Vliver ;

  yout[ID_Cven] = y[ID_Aven] / Vven ;

  yout[ID_Clung] = y[ID_Alung] / Vlung ;

  yout[ID_Cart] = y[ID_Aart] / Vart ;

  yout[ID_Crest] = y[ID_Arest] / Vrest ;

  yout[ID_Ckidney] = y[ID_Akidney] / Vkidney ;

  yout[ID_Cplasma] = y[ID_Aven] / Vven / Rblood2plasma ;

  yout[ID_Aplasma] = y[ID_Aven] / Rblood2plasma * ( 1 - hematocrit ) ;

  yout[ID_Cskin_sc_unexposed] = y[ID_Askin_sc_unexposed] / Vskin_sc_unexposed ;

  yout[ID_Cskin_sc_exposed] = y[ID_Askin_sc_exposed] / Vskin_sc_exposed ;

  yout[ID_Cskin_ed_unexposed] = y[ID_Askin_ed_unexposed] / Vskin_ed_unexposed ;

  yout[ID_Cskin_ed_exposed] = y[ID_Askin_ed_exposed] / Vskin_ed_exposed ;

  yout[ID_Cvehicle] = (Vvehicle ? y[ID_Avehicle] / Vvehicle : 0.0 ) ;

  Rin_dermal = (InfiniteDose ? Pvehicle2sc * SA_exposed * 24 * 0.001 * ( yout[ID_Cvehicle] - yout[ID_Cskin_sc_exposed] / Ksc2vehicle ) : ( Vvehicle ? Pvehicle2sc * SA_exposed * 24 * 0.001 * ( yout[ID_Cvehicle] - yout[ID_Cskin_sc_exposed] / Ksc2vehicle ) : 0.0 ) ) ;

  Rin_oral = kgutabs * y[ID_Agutlumen] ;

  Rout_exhaled = Qalv * yout[ID_Clung] * Rblood2plasma / ( Klung2pu * Fraction_unbound_plasma * Kblood2air ) ;

  ydot[ID_Avehicle] = - Rin_dermal * (1-InfiniteDose);

  ydot[ID_Ain] = Rin_dermal + Rin_oral ;
  
  ydot[ID_Aexhaled] = Rout_exhaled ;

  ydot[ID_Askin_sc_exposed] = Rin_dermal + Psc2ed * SA_exposed * 24 * 0.001 * ( yout[ID_Cskin_ed_exposed] - yout[ID_Cskin_sc_exposed] / Ksc2ed ) ;

  ydot[ID_Askin_sc_unexposed] = Psc2ed * ( totalSA - SA_exposed ) * 24 * 0.001 * ( yout[ID_Cskin_ed_unexposed] - yout[ID_Cskin_sc_unexposed] / Ksc2ed ) ;

  ydot[ID_Askin_ed_exposed] = Qskin_exposed * ( yout[ID_Cart] - yout[ID_Cskin_ed_exposed] * Rblood2plasma / Ked2pu / Fraction_unbound_plasma ) + Psc2ed * SA_exposed * 24 * 0.001 * ( yout[ID_Cskin_sc_exposed] / Ksc2ed - yout[ID_Cskin_ed_exposed] ) ;

  ydot[ID_Askin_ed_unexposed] = Qskin_unexposed * ( yout[ID_Cart] - yout[ID_Cskin_ed_unexposed] * Rblood2plasma / Ked2pu / Fraction_unbound_plasma ) + Psc2ed * ( totalSA - SA_exposed ) * 24 * 0.001 * ( yout[ID_Cskin_sc_unexposed] / Ksc2ed - yout[ID_Cskin_ed_unexposed] ) ;

  ydot[ID_Agutlumen] = - Rin_oral ;

  ydot[ID_Agut] = Rin_oral + Qgut * ( yout[ID_Cart] - yout[ID_Cgut] * Rblood2plasma / Kgut2pu / Fraction_unbound_plasma ) ;

  ydot[ID_Aliver] = Qliver * yout[ID_Cart] + Qgut * yout[ID_Cgut] * Rblood2plasma / Kgut2pu / Fraction_unbound_plasma - ( Qliver + Qgut ) * yout[ID_Cliver] / Kliver2pu / Fraction_unbound_plasma * Rblood2plasma - Clmetabolism * yout[ID_Cliver] / Kliver2pu ;

  ydot[ID_Aven] = ( ( Qliver + Qgut ) * yout[ID_Cliver] / Kliver2pu + Qkidney * yout[ID_Ckidney] / Kkidney2pu + Qrest * yout[ID_Crest] / Krest2pu + Qskin_unexposed * yout[ID_Cskin_ed_unexposed] / Ked2pu + Qskin_exposed * yout[ID_Cskin_ed_exposed] / Ked2pu ) * Rblood2plasma / Fraction_unbound_plasma - Qcardiac * yout[ID_Cven] ;

  ydot[ID_Alung] = Qcardiac * ( yout[ID_Cven] - yout[ID_Clung] * Rblood2plasma / Klung2pu / Fraction_unbound_plasma ) ;

  ydot[ID_Aart] = Qcardiac * ( yout[ID_Clung] * Rblood2plasma / Klung2pu / Fraction_unbound_plasma - yout[ID_Cart] ) ;

  ydot[ID_Arest] = Qrest * ( yout[ID_Cart] - yout[ID_Crest] * Rblood2plasma / Krest2pu / Fraction_unbound_plasma ) ;

  ydot[ID_Akidney] = Qkidney * yout[ID_Cart] - Qkidney * yout[ID_Ckidney] / Kkidney2pu * Rblood2plasma / Fraction_unbound_plasma - Qgfr *  Fraction_unbound_plasma / Rblood2plasma * y[ID_Aart] / Vart ;

  ydot[ID_Atubules] = (y[ID_Aart] > 0.0 ?
                       Qgfr *  Fraction_unbound_plasma / Rblood2plasma * y[ID_Aart] / Vart :
                       0.0);

  ydot[ID_Ametabolized] = Clmetabolism * yout[ID_Cliver] / Kliver2pu ;

  ydot[ID_AUC] = yout[ID_Cven] / Rblood2plasma ;

} /* derivs */


/*----- Jacobian calculations: */
void jac_dermal (int *neq, double *t, double *y, int *ml, int *mu, double *pd, int *nrowpd, double *yout, int *ip)
{

} /* jac */


/*----- Events calculations: */
void event_dermal (int *n, double *t, double *y)
{

} /* event */

/*----- Roots calculations: */
void root_dermal (int *neq, double *t, double *y, int *ng, double *gout, double *out, int *ip)
{

} /* root */

