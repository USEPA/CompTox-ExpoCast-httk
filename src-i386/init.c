#include <R.h>
#include <Rinternals.h>
         #include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

/* FIXME:
   Check these declarations against the C/Fortran source code.
*/

/* .C calls */
extern void getParms(double *, double *, void *);
extern void getParms_1comp(double *, double *, void *);
extern void getParms_3comp(double *, double *, void *);
extern void initmod(void *);
extern void initmod1comp(void *);
extern void initmod3comp(void *);
extern void derivs(int *, double *, double *, double *, double *, int *);
extern void derivs1comp(int *, double *, double *, double *, double *, int *);
extern void derivs3comp(int *, double *, double *, double *, double *, int *);
extern void jac(int *, double *, double *, int *, int *, double *, int *, double *, int *);
extern void jac1comp(int *, double *, double *, int *, int *, double *, int *, double *, int *);
extern void jac3comp(int *, double *, double *, int *, int *, double *, int *, double *, int *);
extern void event(int *, double *, double *);
extern void event1comp(int *, double *, double *);
extern void event3comp(int *, double *, double *);
extern void root (int *, double *, double *, int *, double *, double *, int *);
extern void root1comp (int *, double *, double *, int *, double *, double *, int *);
extern void root3comp (int *, double *, double *, int *, double *, double *, int *);

static const R_CMethodDef CEntries[] = {
    {"getParms",       (DL_FUNC) &getParms,       3},
    {"getParms_1comp", (DL_FUNC) &getParms_1comp, 3},
    {"getParms_3comp", (DL_FUNC) &getParms_3comp, 3},
    {"initmod", (DL_FUNC) &initmod, 1},
    {"initmod1comp", (DL_FUNC) &initmod1comp, 1},
    {"initmod3comp", (DL_FUNC) &initmod3comp, 1},
    {"derivs", (DL_FUNC) &derivs, 6},
    {"derivs1comp", (DL_FUNC) &derivs1comp, 6},
    {"derivs3comp", (DL_FUNC) &derivs3comp, 6},
    {"jac", (DL_FUNC) &jac, 9},
    {"jac1comp", (DL_FUNC) &jac1comp, 9},
    {"jac3comp", (DL_FUNC) &jac3comp, 9},
    {"event",       (DL_FUNC) &event,       3},
    {"event1comp", (DL_FUNC) &event1comp, 3},
    {"event3comp", (DL_FUNC) &event3comp, 3},
    {"root",       (DL_FUNC) &root,       7},
    {"root1comp", (DL_FUNC) &root1comp, 7},
    {"root3comp", (DL_FUNC) &root3comp, 7},
    {NULL, NULL, 0}
};

void R_init_httk(DllInfo *dll)
{
    R_registerRoutines(dll, CEntries, NULL, NULL, NULL);
    R_useDynamicSymbols(dll, TRUE);
}
