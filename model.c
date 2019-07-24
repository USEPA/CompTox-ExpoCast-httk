/* model.c
   ___________________________________________________

   Model File:  pbtk1comp.model

   Date:  Wed Jul 24 14:44:51 2019

   Created by:  "c:/users/jwambaug/MCSim under R/mod/mod v6.1.0"
    -- a model preprocessor by Don Maszle
   ___________________________________________________

   Copyright (c) 1993-2019 Free Software Foundation, Inc.

   Model calculations for compartmental model:

   4 States:
     Agutlumen -> 0.0;
     Acompartment -> 0.0;
     Ametabolized -> 0.0;
     AUC -> 0.0;

   1 Output:
     Ccompartment -> 0.0;

   0 Inputs:

   4 Parameters:
     vdist = 0;
     ke = 0;
     kgutabs = 1;
     BW = 70;
*/


#include <stdlib.h>
#include <stdio.h>
#include <math.h>
#include <string.h>
#include <float.h>
#include "modelu.h"
#include "random.h"
#include "yourcode.h"


/*----- Indices to Global Variables */

/* Model variables: States and other outputs */
#define ID_Agutlumen 0x00000
#define ID_Acompartment 0x00001
#define ID_Ametabolized 0x00002
#define ID_AUC 0x00003
#define ID_Ccompartment 0x00004

/* Inputs */

/* Parameters */
#define ID_vdist 0x00005
#define ID_ke 0x00006
#define ID_kgutabs 0x00007
#define ID_BW 0x00008


/*----- Global Variables */

/* For export. Keep track of who we are. */
char szModelDescFilename[] = "pbtk1comp.model";
char szModelSourceFilename[] = __FILE__;
char szModelGenAndVersion[] = "c:/users/jwambaug/MCSim under R/mod/mod v6.1.0";

/* Externs */
extern BOOL vbModelReinitd;

/* Model Dimensions */
int vnStates = 4;
int vnOutputs = 1;
int vnModelVars = 5;
int vnInputs = 0;
int vnParms = 4;

/* States and Outputs*/
double vrgModelVars[5];

/* Inputs */
IFN vrgInputs[1];

/* Parameters */
double vdist;
double ke;
double kgutabs;
double BW;

BOOL bDelays = 0;


/*----- Global Variable Map */

VMMAPSTRCT vrgvmGlo[] = {
  {"Agutlumen", (PVOID) &vrgModelVars[ID_Agutlumen], ID_STATE | ID_Agutlumen},
  {"Acompartment", (PVOID) &vrgModelVars[ID_Acompartment], ID_STATE | ID_Acompartment},
  {"Ametabolized", (PVOID) &vrgModelVars[ID_Ametabolized], ID_STATE | ID_Ametabolized},
  {"AUC", (PVOID) &vrgModelVars[ID_AUC], ID_STATE | ID_AUC},
  {"Ccompartment", (PVOID) &vrgModelVars[ID_Ccompartment], ID_OUTPUT | ID_Ccompartment},
  {"vdist", (PVOID) &vdist, ID_PARM | ID_vdist},
  {"ke", (PVOID) &ke, ID_PARM | ID_ke},
  {"kgutabs", (PVOID) &kgutabs, ID_PARM | ID_kgutabs},
  {"BW", (PVOID) &BW, ID_PARM | ID_BW},
  {"", NULL, 0} /* End flag */
};  /* vrgpvmGlo[] */


/*----- InitModel
   Should be called to initialize model variables at
   the beginning of experiment before reading
   variants from the simulation spec file.
*/

void InitModel(void)
{
  /* Initialize things in the order that they appear in
     model definition file so that dependencies are
     handled correctly. */

  vrgModelVars[ID_Agutlumen] = 0.0;
  vrgModelVars[ID_Acompartment] = 0.0;
  vrgModelVars[ID_Ametabolized] = 0.0;
  vrgModelVars[ID_AUC] = 0.0;
  vrgModelVars[ID_Ccompartment] = 0.0;
  vdist = 0;
  ke = 0;
  kgutabs = 1;
  BW = 70;

  vbModelReinitd = TRUE;

} /* InitModel */


/*----- Dynamics section */

void CalcDeriv (double  rgModelVars[], double  rgDerivs[], PDOUBLE pdTime)
{

  CalcInputs (pdTime); /* Get new input vals */


  rgModelVars[ID_Ccompartment] = rgModelVars[ID_Acompartment] / vdist ;

  rgDerivs[ID_Agutlumen] = - kgutabs * rgModelVars[ID_Agutlumen] ;

  rgDerivs[ID_Acompartment] = kgutabs * rgModelVars[ID_Agutlumen] - ke * rgModelVars[ID_Acompartment] ;

  rgDerivs[ID_Ametabolized] = ke * rgModelVars[ID_Acompartment] ;

  rgDerivs[ID_AUC] = rgModelVars[ID_Ccompartment] ;

} /* CalcDeriv */


/*----- Model scaling */

void ScaleModel (PDOUBLE pdTime)
{

  vdist = vdist * BW ;
  kgutabs = kgutabs * 24 ;
  ke = ke * 24 ;

} /* ScaleModel */


/*----- Jacobian calculations */

void CalcJacob (PDOUBLE pdTime, double rgModelVars[],
                long column, double rgdJac[])
{

} /* CalcJacob */


/*----- Outputs calculations */

void CalcOutputs (double  rgModelVars[], double  rgDerivs[], PDOUBLE pdTime)
{

}  /* CalcOutputs */


