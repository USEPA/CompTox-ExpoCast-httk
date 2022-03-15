/* dermal.c for R deSolve package
   ___________________________________________________

   Model File:  dermal.model

   Date:  Wed Mar 09 12:09:11 2022

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
     Astratum_corneum_exposed = 0.0,
     Astratum_corneum_unexposed = 0.0,
     Aviable_epidermis_exposed = 0.0,
     Aviable_epidermis_unexposed = 0.0,
     Amedia = 0.0,
     Ain = 0.0,

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
    "Cstratum_corneum_exposed",
    "Cstratum_corneum_unexposed",
    "Cviable_epidermis_exposed",
    "Cviable_epidermis_unexposed",
    "Cmedia",

   1 Input:
     Vmedia (forcing function)

   64 Parameters:
     skin_depth = 0,
     Fskin_depth_sc = 0,
     Fskin_depth_ve = 0,
     Pmedia2sc = 0,
     Psc2ve = 0,
     Fskin_exposed = 0,
     totalSA = 0,
     SA_exposed = 0,
     BW = 0,
     Clmetabolismc = 0.0,
     hematocrit = 0.0,
     kgutabs = 0.0,
     Ksc2media = 0,
     Ksc2ve = 0,
     Kve2pu = 0,
     Kkidney2pu = 0,
     Kliver2pu = 0,
     Krest2pu = 0,
     Kgut2pu = 0,
     Klung2pu = 0,
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
     Vstratum_corneumc = 0,
     Vviable_epidermisc = 0,
     Fraction_unbound_plasma = 0.0,
     Rblood2plasma = 0.0,
     Clmetabolism = 0.0,
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
     Vstratum_corneum = 0,
     Vstratum_corneum_exposed = 0,
     Vstratum_corneum_unexposed = 0,
     Vviable_epidermis = 0,
     Vviable_epidermis_exposed = 0,
     Vviable_epidermis_unexposed = 0,
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
#define ID_Astratum_corneum_exposed 0x0000b
#define ID_Astratum_corneum_unexposed 0x0000c
#define ID_Aviable_epidermis_exposed 0x0000d
#define ID_Aviable_epidermis_unexposed 0x0000e
#define ID_Amedia 0x0000f
#define ID_Ain 0x00010

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
#define ID_Cstratum_corneum_exposed 0x00009
#define ID_Cstratum_corneum_unexposed 0x0000a
#define ID_Cviable_epidermis_exposed 0x0000b
#define ID_Cviable_epidermis_unexposed 0x0000c
#define ID_Cmedia 0x0000d

/* Parameters */
static double parms[64];

#define skin_depth parms[0]
#define Fskin_depth_sc parms[1]
#define Fskin_depth_ve parms[2]
#define Pmedia2sc parms[3]
#define Psc2ve parms[4]
#define Fskin_exposed parms[5]
#define totalSA parms[6]
#define SA_exposed parms[7]
#define BW parms[8]
#define Clmetabolismc parms[9]
#define hematocrit parms[10]
#define kgutabs parms[11]
#define Ksc2media parms[12]
#define Ksc2ve parms[13]
#define Kve2pu parms[14]
#define Kkidney2pu parms[15]
#define Kliver2pu parms[16]
#define Krest2pu parms[17]
#define Kgut2pu parms[18]
#define Klung2pu parms[19]
#define Qcardiacc parms[20]
#define Qgfrc parms[21]
#define Qskinf parms[22]
#define Qgutf parms[23]
#define Qkidneyf parms[24]
#define Qliverf parms[25]
#define Vartc parms[26]
#define Vgutc parms[27]
#define Vkidneyc parms[28]
#define Vliverc parms[29]
#define Vlungc parms[30]
#define Vrestc parms[31]
#define Vvenc parms[32]
#define Vskinc parms[33]
#define Vstratum_corneumc parms[34]
#define Vviable_epidermisc parms[35]
#define Fraction_unbound_plasma parms[36]
#define Rblood2plasma parms[37]
#define Clmetabolism parms[38]
#define Qcardiac parms[39]
#define Qskin parms[40]
#define Qskin_exposed parms[41]
#define Qskin_unexposed parms[42]
#define Qgfr parms[43]
#define Qgut parms[44]
#define Qkidney parms[45]
#define Qliver parms[46]
#define Qrest parms[47]
#define Vart parms[48]
#define Vgut parms[49]
#define Vkidney parms[50]
#define Vliver parms[51]
#define Vlung parms[52]
#define Vrest parms[53]
#define Vven parms[54]
#define Vskin parms[55]
#define Vskin_exposed parms[56]
#define Vskin_unexposed parms[57]
#define Vstratum_corneum parms[58]
#define Vstratum_corneum_exposed parms[59]
#define Vstratum_corneum_unexposed parms[60]
#define Vviable_epidermis parms[61]
#define Vviable_epidermis_exposed parms[62]
#define Vviable_epidermis_unexposed parms[63]

/* Forcing (Input) functions */
static double forc[1];

#define Vmedia forc[0]

/*----- Initializers */
void initmod_dermal (void (* odeparms)(int *, double *))
{
  int N=64;
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


  Vviable_epidermisc = Vskinc - Vstratum_corneumc ;
  Fskin_depth_ve = 1 - Fskin_depth_sc ;

  kgutabs = kgutabs * 24 ;
  Clmetabolism = Clmetabolismc * 24 * BW ;
  Qcardiac = Qcardiacc * 24 * pow ( BW , 0.75 ) ;
  Qgfr = Qgfrc * pow ( BW , 0.75 ) * 24 ;

  Qgut = Qcardiac * Qgutf ;
  Qkidney = Qcardiac * Qkidneyf ;
  Qliver = Qcardiac * Qliverf ;

  SA_exposed = Fskin_exposed * totalSA ;

  Vskin = BW * Vskinc ;
  Vskin_exposed = SA_exposed * skin_depth * 0.001 ;
  Vskin_unexposed = Vskin - Vskin_exposed ;
  Vstratum_corneum = Vskin * Fskin_depth_sc ;
  Vstratum_corneum_exposed = Vstratum_corneum * Fskin_exposed ;
  Vstratum_corneum_unexposed = Vstratum_corneum * ( 1 - Fskin_exposed ) ;
  Vviable_epidermis = Vskin * Fskin_depth_ve ;
  Vviable_epidermis_exposed = Vviable_epidermis * Fskin_exposed ;
  Vviable_epidermis_unexposed = Vviable_epidermis * ( 1 - Fskin_exposed ) ;

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

  yout[ID_Cgut] = y[ID_Agut] / Vgut ;

  yout[ID_Cliver] = y[ID_Aliver] / Vliver ;

  yout[ID_Cven] = y[ID_Aven] / Vven ;

  yout[ID_Clung] = y[ID_Alung] / Vlung ;

  yout[ID_Cart] = y[ID_Aart] / Vart ;

  yout[ID_Crest] = y[ID_Arest] / Vrest ;

  yout[ID_Ckidney] = y[ID_Akidney] / Vkidney ;

  yout[ID_Cplasma] = y[ID_Aven] / Vven / Rblood2plasma ;

  yout[ID_Aplasma] = y[ID_Aven] / Rblood2plasma * ( 1 - hematocrit ) ;

  yout[ID_Cstratum_corneum_unexposed] = y[ID_Astratum_corneum_unexposed] / Vstratum_corneum_unexposed ;

  yout[ID_Cstratum_corneum_exposed] = y[ID_Astratum_corneum_exposed] / Vstratum_corneum_exposed ;

  yout[ID_Cviable_epidermis_unexposed] = y[ID_Aviable_epidermis_unexposed] / Vviable_epidermis_unexposed ;

  yout[ID_Cviable_epidermis_exposed] = y[ID_Aviable_epidermis_exposed] / Vviable_epidermis_exposed ;

  yout[ID_Cmedia] = y[ID_Amedia] / Vmedia ;

  Rin_dermal = ( y[ID_Amedia] ? Pmedia2sc * SA_exposed * 24 * 0.001 * ( yout[ID_Cmedia] - yout[ID_Cstratum_corneum_exposed] / Ksc2media ) : 0.0 ) ;

  Rin_oral = kgutabs * y[ID_Agutlumen] ;

  ydot[ID_Amedia] = - Rin_dermal ;

  ydot[ID_Ain] = Rin_dermal + Rin_oral ;

  ydot[ID_Astratum_corneum_exposed] = Rin_dermal + Psc2ve * SA_exposed * 24 * 0.001 * ( yout[ID_Cviable_epidermis_exposed] - yout[ID_Cstratum_corneum_exposed] / Ksc2ve ) ;

  ydot[ID_Astratum_corneum_unexposed] = Psc2ve * ( totalSA - SA_exposed ) * 24 * 0.001 * ( yout[ID_Cviable_epidermis_unexposed] - yout[ID_Cstratum_corneum_unexposed] / Ksc2ve ) ;

  ydot[ID_Aviable_epidermis_exposed] = Qskin_exposed * ( yout[ID_Cart] - yout[ID_Cviable_epidermis_exposed] * Rblood2plasma / Kve2pu / Fraction_unbound_plasma ) + Psc2ve * SA_exposed * 24 * 0.001 * ( yout[ID_Cstratum_corneum_exposed] / Ksc2ve - yout[ID_Cviable_epidermis_exposed] ) ;

  ydot[ID_Aviable_epidermis_unexposed] = Qskin_unexposed * ( yout[ID_Cart] - yout[ID_Cviable_epidermis_unexposed] * Rblood2plasma / Kve2pu / Fraction_unbound_plasma ) + Psc2ve * ( totalSA - SA_exposed ) * 24 * 0.001 * ( yout[ID_Cstratum_corneum_unexposed] / Ksc2ve - yout[ID_Cviable_epidermis_unexposed] ) ;

  ydot[ID_Agutlumen] = - Rin_oral ;

  ydot[ID_Agut] = Rin_oral + Qgut * ( yout[ID_Cart] - yout[ID_Cgut] * Rblood2plasma / Kgut2pu / Fraction_unbound_plasma ) ;

  ydot[ID_Aliver] = Qliver * yout[ID_Cart] + Qgut * yout[ID_Cgut] * Rblood2plasma / Kgut2pu / Fraction_unbound_plasma - ( Qliver + Qgut ) * yout[ID_Cliver] / Kliver2pu / Fraction_unbound_plasma * Rblood2plasma - Clmetabolism * yout[ID_Cliver] / Kliver2pu ;

  ydot[ID_Aven] = ( ( Qliver + Qgut ) * yout[ID_Cliver] / Kliver2pu + Qkidney * yout[ID_Ckidney] / Kkidney2pu + Qrest * yout[ID_Crest] / Krest2pu + Qskin_unexposed * yout[ID_Cviable_epidermis_unexposed] / Kve2pu + Qskin_exposed * yout[ID_Cviable_epidermis_exposed] / Kve2pu ) * Rblood2plasma / Fraction_unbound_plasma - Qcardiac * yout[ID_Cven] ;

  ydot[ID_Alung] = Qcardiac * ( yout[ID_Cven] - yout[ID_Clung] * Rblood2plasma / Klung2pu / Fraction_unbound_plasma ) ;

  ydot[ID_Aart] = Qcardiac * ( yout[ID_Clung] * Rblood2plasma / Klung2pu / Fraction_unbound_plasma - yout[ID_Cart] ) ;

  ydot[ID_Arest] = Qrest * ( yout[ID_Cart] - yout[ID_Crest] * Rblood2plasma / Krest2pu / Fraction_unbound_plasma ) ;

  ydot[ID_Akidney] = Qkidney * yout[ID_Cart] - Qkidney * yout[ID_Ckidney] / Kkidney2pu * Rblood2plasma / Fraction_unbound_plasma - Qgfr * yout[ID_Ckidney] / Kkidney2pu ;

  ydot[ID_Atubules] = Qgfr * yout[ID_Ckidney] / Kkidney2pu ;

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

