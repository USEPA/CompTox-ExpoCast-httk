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
extern void rootpbtk (int *, double *, double *, int *, double *, double *, int *);

extern void getParms1comp(double *, double *, int *);
extern void initmod1comp(void *);
extern void derivs1comp(int *, double *, double *, double *, double *, int *);
extern void jac1comp(int *, double *, double *, int *, int *, double *, int *, double *, int *);
extern void event1comp(int *, double *, double *);
extern void root1comp (int *, double *, double *, int *, double *, double *, int *);

extern void getParms3comp(double *, double *, int *);
extern void initmod3comp(void *);
extern void derivs3comp(int *, double *, double *, double *, double *, int *);
extern void jac3comp(int *, double *, double *, int *, int *, double *, int *, double *, int *);
extern void event3comp(int *, double *, double *);
extern void root3comp (int *, double *, double *, int *, double *, double *, int *);

extern void getParms_gas_pbtk(double *, double *, int *);
extern void initmod_gas_pbtk(void *);
extern void derivs_gas_pbtk(int *, double *, double *, double *, double *, int *);
extern void jac_gas_pbtk(int *, double *, double *, int *, int *, double *, int *, double *, int *);
extern void event_gas_pbtk(int *, double *, double *);
extern void root_gas_pbtk (int *, double *, double *, int *, double *, double *, int *);

extern void getParmsfetal_pbtk(double *, double *, int *);
extern void initmodfetal_pbtk(void *);
extern void derivsfetal_pbtk(int *, double *, double *, double *, double *, int *);
extern void jacfetal_pbtk(int *, double *, double *, int *, int *, double *, int *, double *, int *);
extern void eventfetal_pbtk(int *, double *, double *);
extern void rootfetal_pbtk (int *, double *, double *, int *, double *, double *, int *);

static const R_CMethodDef CEntries[] = {
    {"getParmspbtk",       (DL_FUNC) &getParmspbtk,       3},
    {"initmodpbtk", (DL_FUNC) &initmodpbtk, 1},
    {"derivspbtk", (DL_FUNC) &derivspbtk, 6},
    {"jacpbtk", (DL_FUNC) &jacpbtk, 9},
    {"eventpbtk",       (DL_FUNC) &eventpbtk,       3},
    {"rootpbtk",       (DL_FUNC) &rootpbtk,       7},

    {"getParms1comp", (DL_FUNC) &getParms1comp, 3},
    {"initmod1comp", (DL_FUNC) &initmod1comp, 1},
    {"derivs1comp", (DL_FUNC) &derivs1comp, 6},
    {"jac1comp", (DL_FUNC) &jac1comp, 9},
    {"event1comp", (DL_FUNC) &event1comp, 3},
    {"root1comp", (DL_FUNC) &root1comp, 7},

    {"getParms3comp", (DL_FUNC) &getParms3comp, 3},
    {"initmod3comp", (DL_FUNC) &initmod3comp, 1},
    {"derivs3comp", (DL_FUNC) &derivs3comp, 6},
    {"jac3comp", (DL_FUNC) &jac3comp, 9},
    {"event3comp", (DL_FUNC) &event3comp, 3},
    {"root3comp", (DL_FUNC) &root3comp, 7},

    {"getParms_gas_pbtk", (DL_FUNC) &getParms_gas_pbtk, 3},
    {"initmod_gas_pbtk", (DL_FUNC) &initmod_gas_pbtk, 1},
    {"derivs_gas_pbtk", (DL_FUNC) &derivs_gas_pbtk, 6},
    {"jac_gas_pbtk", (DL_FUNC) &jac_gas_pbtk, 9},
    {"event_gas_pbtk", (DL_FUNC) &event_gas_pbtk, 3},
    {"root_gas_pbtk", (DL_FUNC) &root_gas_pbtk, 7},

    {"getParmsfetal_pbtk", (DL_FUNC) &getParmsfetal_pbtk, 3},
    {"initmodfetal_pbtk", (DL_FUNC) &initmodfetal_pbtk, 1},
    {"derivsfetal_pbtk", (DL_FUNC) &derivsfetal_pbtk, 6},
    {"jacfetal_pbtk", (DL_FUNC) &jacfetal_pbtk, 9},
    {"eventfetal_pbtk", (DL_FUNC) &eventfetal_pbtk, 3},
    {"rootfetal_pbtk", (DL_FUNC) &rootfetal_pbtk, 7},

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
