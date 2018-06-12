/* inhalation.c for R deSolve package
   ___________________________________________________

   Model File:  inhalation.model

   Date:  Mon Apr 02 16:08:44 2018

   Created by:  "mod v5.5.0"
    -- a model preprocessor by Don Maszle
   ___________________________________________________

   Copyright (c) 1993-2013 Free Software Foundation, Inc.

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

   10 Outputs:
    "Cgut",
    "Cliver",
    "Cven",
    "Clung",
    "Cart",
    "Crest",
    "Ckidney",
    "Cplasma",
    "Aplasma",
    "Clungart",

   1 Input:
     Cinh (forcing function)

   44 Parameters:
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
     Qalv = 0,
     Kblood2air = 0,
     InhMag = 0,
     Period = 0,
     Exposure = 0,
*/

#include <R.h>

/* Model variables: States */
#define ID_Agutlumen 0x0000
#define ID_Agut 0x0001
#define ID_Aliver 0x0002
#define ID_Aven 0x0003
#define ID_Alung 0x0004
#define ID_Aart 0x0005
#define ID_Arest 0x0006
#define ID_Akidney 0x0007
#define ID_Atubules 0x0008
#define ID_Ametabolized 0x0009
#define ID_AUC 0x000a

/* Model variables: Outputs */
#define ID_Cgut 0x0000
#define ID_Cliver 0x0001
#define ID_Cven 0x0002
#define ID_Clung 0x0003
#define ID_Cart 0x0004
#define ID_Crest 0x0005
#define ID_Ckidney 0x0006
#define ID_Cplasma 0x0007
#define ID_Aplasma 0x0008
#define ID_Clungart 0x0009

/* Parameters */
static double parms[44];

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
#define Qlungf parms[14]
#define Vartc parms[15]
#define Vgutc parms[16]
#define Vkidneyc parms[17]
#define Vliverc parms[18]
#define Vlungc parms[19]
#define Vrestc parms[20]
#define Vvenc parms[21]
#define Fraction_unbound_plasma parms[22]
#define Rblood2plasma parms[23]
#define Clmetabolism parms[24]
#define Qcardiac parms[25]
#define Qgfr parms[26]
#define Qgut parms[27]
#define Qkidney parms[28]
#define Qliver parms[29]
#define Qlung parms[30]
#define Qrest parms[31]
#define Vart parms[32]
#define Vgut parms[33]
#define Vkidney parms[34]
#define Vliver parms[35]
#define Vlung parms[36]
#define Vrest parms[37]
#define Vven parms[38]
#define Qalv parms[39]
#define Kblood2air parms[40]
#define InhMag parms[41]
#define Period parms[42]
#define Exposure parms[43]

/* Forcing (Input) functions */
static double forc[1];

#define Cinh forc[0]

/*----- Initializers */
void initmod_inh (void (* odeparms)(int *, double *))
{
  int N=44;
  odeparms(&N, parms);
}

void initforc_inh (void (* odeforcs)(int *, double *))
{
  int N=1;
  odeforcs(&N, forc);
}


void getParms_inh (double *inParms, double *out, int *nout) {
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
  Qalv = Qalv * 24 * pow ( BW , 0.75 ) ;

  for (i = 0; i < *nout; i++) {
    out[i] = parms[i];
  }
  }
/*----- Dynamics section */

void derivs_inh (int *neq, double *pdTime, double *y, double *ydot, double *yout, int *ip)
{

  yout[ID_Cgut] = y[ID_Agut] / Vgut ;

  yout[ID_Cliver] = y[ID_Aliver] / Vliver ;

  yout[ID_Cven] = y[ID_Aven] / Vven ;

  yout[ID_Clung] = y[ID_Alung] / Vlung ;

  yout[ID_Cart] = y[ID_Aart] / Vart ;

  yout[ID_Crest] = y[ID_Arest] / Vrest ;

  yout[ID_Ckidney] = y[ID_Akidney] / Vkidney ;

  yout[ID_Clungart] = ( Qcardiac * yout[ID_Cven] + Qalv * Cinh ) / ( Qcardiac + Qalv / Kblood2air ) ;

  yout[ID_Cplasma] = y[ID_Aven] / Vven / Rblood2plasma ;

  yout[ID_Aplasma] = y[ID_Aven] / Rblood2plasma * ( 1 - hematocrit ) ;

  ydot[ID_Agutlumen] = - kgutabs * y[ID_Agutlumen] ;

  ydot[ID_Agut] = kgutabs * y[ID_Agutlumen] + Qgut * ( yout[ID_Cart] - yout[ID_Cgut] * Rblood2plasma / Kgut2pu / Fraction_unbound_plasma ) ;

  ydot[ID_Aliver] = Qliver * yout[ID_Cart] + Qgut * yout[ID_Cgut] * Rblood2plasma / Kgut2pu / Fraction_unbound_plasma - ( Qliver + Qgut ) * yout[ID_Cliver] / Kliver2pu / Fraction_unbound_plasma * Rblood2plasma - Clmetabolism * yout[ID_Cliver] / Kliver2pu ;

  ydot[ID_Aven] = ( ( Qliver + Qgut ) * yout[ID_Cliver] / Kliver2pu + Qkidney * yout[ID_Ckidney] / Kkidney2pu + Qrest * yout[ID_Crest] / Krest2pu + Qlung * yout[ID_Clung] / Klung2pu ) * Rblood2plasma / Fraction_unbound_plasma - Qcardiac * yout[ID_Cven] ;

  ydot[ID_Alung] = Qlung * ( yout[ID_Cart] - yout[ID_Clung] * Rblood2plasma / Klung2pu / Fraction_unbound_plasma ) ;

  ydot[ID_Aart] = Qcardiac * ( yout[ID_Clungart] - yout[ID_Cart] ) ;

  ydot[ID_Arest] = Qrest * ( yout[ID_Cart] - yout[ID_Crest] * Rblood2plasma / Krest2pu / Fraction_unbound_plasma ) ;

  ydot[ID_Akidney] = Qkidney * yout[ID_Cart] - Qkidney * yout[ID_Ckidney] / Kkidney2pu * Rblood2plasma / Fraction_unbound_plasma - Qgfr * yout[ID_Ckidney] / Kkidney2pu ;

  ydot[ID_Atubules] = Qgfr * yout[ID_Ckidney] / Kkidney2pu ;

  ydot[ID_Ametabolized] = Clmetabolism * yout[ID_Cliver] / Kliver2pu ;

  ydot[ID_AUC] = yout[ID_Cven] / Rblood2plasma ;

} /* derivs */


/*----- Jacobian calculations: */
void jac_inh (int *neq, double *t, double *y, int *ml, int *mu, double *pd, int *nrowpd, double *yout, int *ip)
{

} /* jac */


/*----- Events calculations: */
void event_inh (int *n, double *t, double *y)
{

} /* event */

/*----- Roots calculations: */
void root_inh (int *neq, double *t, double *y, int *ng, double *gout, double *out, int *ip)
{

} /* root */

