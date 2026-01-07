#include <R.h>
#include <Rinternals.h>
         #include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

/* FIXME:
   Check these declarations against the C/Fortran source code.
*/

/* .C calls */
extern void getParmspbtk(double *, double *, int *);
extern void initmodpbtk(void *);
extern void derivspbtk(int *, double *, double *, double *, double *, int *);
extern void jacpbtk(int *, double *, double *, int *, int *, double *, int *, double *, int *);
extern void eventpbtk(int *, double *, double *);
extern void rootpbtk(int *, double *, double *, int *, double *, double *, int *);
extern void initforcpbtk(void *);
#
extern void getParmspbtk_lifestage(double *, double *, int *);
extern void initmodpbtk_lifestage(void *);
extern void initforcpbtk_lifestage(void *);
extern void derivspbtk_lifestage(int *, double *, double *, double *, double *, int *);
extern void jacpbtk_lifestage(int *, double *, double *, int *, int *, double *, int *, double *, int *);
extern void eventpbtk_lifestage(int *, double *, double *);
extern void rootpbtk_lifestage(int *, double *, double *, int *, double *, double *, int *);
#
extern void getParms1comp(double *, double *, int *);
extern void initmod1comp(void *);
extern void derivs1comp(int *, double *, double *, double *, double *, int *);
extern void jac1comp(int *, double *, double *, int *, int *, double *, int *, double *, int *);
extern void event1comp(int *, double *, double *);
extern void root1comp(int *, double *, double *, int *, double *, double *, int *);
extern void initforc1comp(void *);
#
extern void getParms1comp_lifestage(double *, double *, int *);
extern void initmod1comp_lifestage(void *);
extern void initforc1comp_lifestage(void *);
extern void derivs1comp_lifestage(int *, double *, double *, double *, double *, int *);
extern void jac1comp_lifestage(int *, double *, double *, int *, int *, double *, int *, double *, int *);
extern void event1comp_lifestage(int *, double *, double *);
extern void root1comp_lifestage(int *, double *, double *, int *, double *, double *, int *);
#
extern void getParms3comp(double *, double *, int *);
extern void initmod3comp(void *);
extern void derivs3comp(int *, double *, double *, double *, double *, int *);
extern void jac3comp(int *, double *, double *, int *, int *, double *, int *, double *, int *);
extern void event3comp(int *, double *, double *);
extern void root3comp(int *, double *, double *, int *, double *, double *, int *);
extern void initforc3comp(void *);
#
extern void getParms3comp_lifestage(double *, double *, int *);
extern void initmod3comp_lifestage(void *);
extern void initforc3comp_lifestage(void *);
extern void derivs3comp_lifestage(int *, double *, double *, double *, double *, int *);
extern void jac3comp_lifestage(int *, double *, double *, int *, int *, double *, int *, double *, int *);
extern void event3comp_lifestage(int *, double *, double *);
extern void root3comp_lifestage(int *, double *, double *, int *, double *, double *, int *);
#
extern void getParms3comp2(double *, double *, int *);
extern void initmod3comp2(void *);
extern void derivs3comp2(int *, double *, double *, double *, double *, int *);
extern void jac3comp2(int *, double *, double *, int *, int *, double *, int *, double *, int *);
extern void event3comp2(int *, double *, double *);
extern void root3comp2(int *, double *, double *, int *, double *, double *, int *);
extern void initforc3comp2(void *);
#
extern void getParms_gas_pbtk(double *, double *, int *);
extern void initmod_gas_pbtk(void *);
extern void derivs_gas_pbtk(int *, double *, double *, double *, double *, int *);
extern void jac_gas_pbtk(int *, double *, double *, int *, int *, double *, int *, double *, int *);
extern void event_gas_pbtk(int *, double *, double *);
extern void root_gas_pbtk(int *, double *, double *, int *, double *, double *, int *);
extern void initforc_gas_pbtk(void *);
#
extern void getParmsfetal_pbtk(double *, double *, int *);
extern void initmodfetal_pbtk(void *);
extern void derivsfetal_pbtk(int *, double *, double *, double *, double *, int *);
extern void jacfetal_pbtk(int *, double *, double *, int *, int *, double *, int *, double *, int *);
extern void eventfetal_pbtk(int *, double *, double *);
extern void rootfetal_pbtk(int *, double *, double *, int *, double *, double *, int *);
extern void initforcfetal_pbtk(void *);
#
extern void getParms_dermal_1subcomp(double *, double *, int *);
extern void initmod_dermal_1subcomp(void *);
extern void derivs_dermal_1subcomp(int *, double *, double *, double *, double *, int *);
extern void jac_dermal_1subcomp(int *, double *, double *, int *, int *, double *, int *, double *, int *);
extern void event_dermal_1subcomp(int *, double *, double *);
extern void root_dermal_1subcomp(int *, double *, double *, int *, double *, double *, int *);
extern void initforc_dermal_1subcomp(void *);
#
extern void getParms_dermal(double *, double *, int *);
extern void initmod_dermal(void *);
extern void derivs_dermal(int *, double *, double *, double *, double *, int *);
extern void jac_dermal(int *, double *, double *, int *, int *, double *, int *, double *, int *);
extern void event_dermal(int *, double *, double *);
extern void root_dermal(int *, double *, double *, int *, double *, double *, int *);
extern void initforc_dermal(void *);
#
extern void getParms_firsttrimester(double *, double *, int *);
extern void initmod_firsttrimester(void *);
extern void derivs_firsttrimester(int *, double *, double *, double *, double *, int *);
extern void jac_firsttrimester(int *, double *, double *, int *, int *, double *, int *, double *, int *);
extern void event_firsttrimester(int *, double *, double *);
extern void root_firsttrimester(int *, double *, double *, int *, double *, double *, int *);
extern void initforc_firsttrimester(void *);
#
extern void rootpbtk_lifestage(int *, double *, double *, int *, double *, double *, int *);
extern void getParmspbtk_lifestage(double *, double *, int *);
extern void initmodpbtk_lifestage(void *);
extern void initforcpbtk_lifestage(void *);
extern void derivspbtk_lifestage(int *, double *, double *, double *, double *, int *);
extern void jacpbtk_lifestage(int *, double *, double *, int *, int *, double *, int *, double *, int *);
extern void eventpbtk_lifestage(int *, double *, double *);
extern void rootpbtk_lifestage(int *, double *, double *, int *, double *, double *, int *);
#
extern void root_aerosol_pbtk(int *, double *, double *, int *, double *, double *, int *);
extern void getParms_aerosol_pbtk(double *, double *, int *);
extern void initmod_aerosol_pbtk(void *);
extern void initforc_aerosol_pbtk(void *);
extern void derivs_aerosol_pbtk(int *, double *, double *, double *, double *, int *);
extern void jac_aerosol_pbtk(int *, double *, double *, int *, int *, double *, int *, double *, int *);
extern void event_aerosol_pbtk(int *, double *, double *);
extern void root_aerosol_pbtk(int *, double *, double *, int *, double *, double *, int *);
#
static const R_CMethodDef CEntries[] = {
    {"getParmspbtk",       (DL_FUNC) &getParmspbtk,       3},
    {"initmodpbtk", (DL_FUNC) &initmodpbtk, 1},
    {"derivspbtk", (DL_FUNC) &derivspbtk, 6},
    {"jacpbtk", (DL_FUNC) &jacpbtk, 9},
    {"eventpbtk",       (DL_FUNC) &eventpbtk,       3},
    {"rootpbtk",       (DL_FUNC) &rootpbtk,       7},
    {"initforcpbtk", (DL_FUNC) &initforcpbtk, 1},
 #
    {"getParms1comp", (DL_FUNC) &getParms1comp, 3},
    {"initmod1comp", (DL_FUNC) &initmod1comp, 1},
    {"derivs1comp", (DL_FUNC) &derivs1comp, 6},
    {"jac1comp", (DL_FUNC) &jac1comp, 9},
    {"event1comp", (DL_FUNC) &event1comp, 3},
    {"root1comp", (DL_FUNC) &root1comp, 7},
    {"initforc1comp", (DL_FUNC) &initforc1comp, 1},
#
    {"getParms3comp", (DL_FUNC) &getParms3comp, 3},
    {"initmod3comp", (DL_FUNC) &initmod3comp, 1},
    {"derivs3comp", (DL_FUNC) &derivs3comp, 6},
    {"jac3comp", (DL_FUNC) &jac3comp, 9},
    {"event3comp", (DL_FUNC) &event3comp, 3},
    {"root3comp", (DL_FUNC) &root3comp, 7},
    {"initforc3comp", (DL_FUNC) &initforc3comp, 1},
#
    {"getParms3comp2", (DL_FUNC) &getParms3comp2, 3},
    {"initmod3comp2", (DL_FUNC) &initmod3comp2, 1},
    {"derivs3comp2", (DL_FUNC) &derivs3comp2, 6},
    {"jac3comp2", (DL_FUNC) &jac3comp2, 9},
    {"event3comp2", (DL_FUNC) &event3comp2, 3},
    {"root3comp2", (DL_FUNC) &root3comp2, 7},
    {"initforc3comp2", (DL_FUNC) &initforc3comp2, 1},
#
    {"getParms_gas_pbtk", (DL_FUNC) &getParms_gas_pbtk, 3},
    {"initmod_gas_pbtk", (DL_FUNC) &initmod_gas_pbtk, 1},
    {"derivs_gas_pbtk", (DL_FUNC) &derivs_gas_pbtk, 6},
    {"jac_gas_pbtk", (DL_FUNC) &jac_gas_pbtk, 9},
    {"event_gas_pbtk", (DL_FUNC) &event_gas_pbtk, 3},
    {"root_gas_pbtk", (DL_FUNC) &root_gas_pbtk, 7},
    {"initforc_gas_pbtk", (DL_FUNC) &initforc_gas_pbtk, 1},
#
    {"getParmsfetal_pbtk", (DL_FUNC) &getParmsfetal_pbtk, 3},
    {"initmodfetal_pbtk", (DL_FUNC) &initmodfetal_pbtk, 1},
    {"derivsfetal_pbtk", (DL_FUNC) &derivsfetal_pbtk, 6},
    {"jacfetal_pbtk", (DL_FUNC) &jacfetal_pbtk, 9},
    {"eventfetal_pbtk", (DL_FUNC) &eventfetal_pbtk, 3},
    {"rootfetal_pbtk", (DL_FUNC) &rootfetal_pbtk, 7},
    {"initforcfetal_pbtk", (DL_FUNC) &initforcfetal_pbtk, 1},
#
    {"getParms_dermal_1subcomp", (DL_FUNC) &getParms_dermal_1subcomp, 3},
    {"initmod_dermal", (DL_FUNC) &initmod_dermal, 1},
    {"derivs_dermal", (DL_FUNC) &derivs_dermal, 6},
    {"jac_dermal", (DL_FUNC) &jac_dermal, 9},
    {"event_dermal_1subcomp", (DL_FUNC) &event_dermal_1subcomp, 3},
    {"root_dermal_1subcomp", (DL_FUNC) &root_dermal_1subcomp, 7},
    {"initforc_dermal_1subcomp", (DL_FUNC) &initforc_dermal_1subcomp, 1},
#
    {"derivs_dermal_1subcomp", (DL_FUNC) &derivs_dermal_1subcomp, 6},
    {"initmod_dermal_1subcomp", (DL_FUNC) &initmod_dermal_1subcomp, 1},
    {"jac_dermal_1subcomp", (DL_FUNC) &jac_dermal_1subcomp, 9},
    {"getParms_dermal", (DL_FUNC) &getParms_dermal, 3},
    {"event_dermal", (DL_FUNC) &event_dermal, 3},
    {"root_dermal", (DL_FUNC) &root_dermal, 7},
    {"initforc_dermal", (DL_FUNC) &initforc_dermal, 1},
#
    {"getParms_firsttrimester", (DL_FUNC) &getParms_firsttrimester, 3},
    {"initmod_firsttrimester", (DL_FUNC) &initmod_firsttrimester, 1}, 	
    {"derivs_firsttrimester", (DL_FUNC) &derivs_firsttrimester, 6},
    {"jac_firsttrimester", (DL_FUNC) &jac_firsttrimester, 9},
    {"event_firsttrimester", (DL_FUNC) &event_firsttrimester, 3},
    {"root_firsttrimester", (DL_FUNC) &root_firsttrimester, 7},
#
    {"getParms1comp_lifestage", (DL_FUNC) &getParms1comp_lifestage, 3},
    {"initmod1comp_lifestage", (DL_FUNC) &initmod1comp_lifestage, 1},
    {"initforc1comp_lifestage", (DL_FUNC) &initforc1comp_lifestage, 1},
    {"derivs1comp_lifestage", (DL_FUNC) &derivs1comp_lifestage, 6},
    {"jac1comp_lifestage", (DL_FUNC) &jac1comp_lifestage, 9},
    {"event1comp_lifestage", (DL_FUNC) &event1comp_lifestage, 3},
    {"root1comp_lifestage", (DL_FUNC) &root1comp_lifestage, 7},
#   
    {"getParms3comp_lifestage", (DL_FUNC) &getParms3comp_lifestage, 3},
    {"initmod3comp_lifestage", (DL_FUNC) &initmod3comp_lifestage, 1},
    {"initforc3comp_lifestage", (DL_FUNC) &initforc3comp_lifestage, 1},
    {"derivs3comp_lifestage", (DL_FUNC) &derivs3comp_lifestage, 6},
    {"jac3comp_lifestage", (DL_FUNC) &jac3comp_lifestage, 9},
    {"event3comp_lifestage", (DL_FUNC) &event3comp_lifestage, 3},
    {"root3comp_lifestage", (DL_FUNC) &root3comp_lifestage, 7},
#    
    {"getParmspbtk_lifestage", (DL_FUNC) &getParmspbtk_lifestage, 3},
    {"initmodpbtk_lifestage", (DL_FUNC) &initmodpbtk_lifestage, 1},
    {"initforcpbtk_lifestage", (DL_FUNC) &initforcpbtk_lifestage, 1},
    {"derivspbtk_lifestage", (DL_FUNC) &derivspbtk_lifestage, 6},
    {"jacpbtk_lifestage", (DL_FUNC) &jacpbtk_lifestage, 9},
    {"eventpbtk_lifestage", (DL_FUNC) &eventpbtk_lifestage, 3},
    {"rootpbtk_lifestage", (DL_FUNC) &rootpbtk_lifestage, 7},
#
    {"getParms_aerosol_pbtk", (DL_FUNC) &getParms_aerosol_pbtk, 3},
    {"initmod_aerosol_pbtk", (DL_FUNC) &initmod_aerosol_pbtk, 1},
    {"initforc_aerosol_pbtk", (DL_FUNC) &initforc_aerosol_pbtk, 1},
    {"derivs_aerosol_pbtk", (DL_FUNC) &derivs_aerosol_pbtk, 6},
    {"jac_aerosol_pbtk", (DL_FUNC) &jac_aerosol_pbtk, 9},
    {"event_aerosol_pbtk", (DL_FUNC) &event_aerosol_pbtk, 3},
    {"root_aerosol_pbtk", (DL_FUNC) &root_aerosol_pbtk, 7},
#
    {NULL, NULL, 0}
};

void R_init_httk(DllInfo *dll)
{
    R_registerRoutines(dll, CEntries, NULL, NULL, NULL);
    R_useDynamicSymbols(dll, TRUE);
}

///*----- Initializers */
//void getParms(void *parmsInit(double *, double *, int *), double *inParms, double *out, int *nout)
//{
//  parmsInit(inParms, out, nout);
//}
