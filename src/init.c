#include <R.h>
#include <Rinternals.h>
         #include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

/* FIXME:
   Check these declarations against the C/Fortran source code.
*/

/* .C calls */
extern void getParmspbtk(double *, double *, void *);
extern void getParms1comp(double *, double *, void *);
extern void getParms3comp(double *, double *, void *);
extern void initmodpbtk(void *);
extern void initmod1comp(void *);
extern void initmod3comp(void *);
extern void derivspbtk(int *, double *, double *, double *, double *, int *);
extern void derivs1comp(int *, double *, double *, double *, double *, int *);
extern void derivs3comp(int *, double *, double *, double *, double *, int *);
extern void jacpbtk(int *, double *, double *, int *, int *, double *, int *, double *, int *);
extern void jac1comp(int *, double *, double *, int *, int *, double *, int *, double *, int *);
extern void jac3comp(int *, double *, double *, int *, int *, double *, int *, double *, int *);
extern void eventpbtk(int *, double *, double *);
extern void event1comp(int *, double *, double *);
extern void event3comp(int *, double *, double *);
extern void rootpbtk (int *, double *, double *, int *, double *, double *, int *);
extern void root1comp (int *, double *, double *, int *, double *, double *, int *);
extern void root3comp (int *, double *, double *, int *, double *, double *, int *);

static const R_CMethodDef CEntries[] = {
    {"getParmspbtk",       (DL_FUNC) &getParmspbtk,       3},
    {"getParms1comp", (DL_FUNC) &getParms1comp, 3},
    {"getParms3comp", (DL_FUNC) &getParms3comp, 3},
    {"initmodpbtk", (DL_FUNC) &initmodpbtk, 1},
    {"initmod1comp", (DL_FUNC) &initmod1comp, 1},
    {"initmod3comp", (DL_FUNC) &initmod3comp, 1},
    {"derivspbtk", (DL_FUNC) &derivspbtk, 6},
    {"derivs1comp", (DL_FUNC) &derivs1comp, 6},
    {"derivs3comp", (DL_FUNC) &derivs3comp, 6},
    {"jacpbtk", (DL_FUNC) &jacpbtk, 9},
    {"jac1comp", (DL_FUNC) &jac1comp, 9},
    {"jac3comp", (DL_FUNC) &jac3comp, 9},
    {"eventpbtk",       (DL_FUNC) &eventpbtk,       3},
    {"event1comp", (DL_FUNC) &event1comp, 3},
    {"event3comp", (DL_FUNC) &event3comp, 3},
    {"rootpbtk",       (DL_FUNC) &rootpbtk,       7},
    {"root1comp", (DL_FUNC) &root1comp, 7},
    {"root3comp", (DL_FUNC) &root3comp, 7},
    {NULL, NULL, 0}
};

void R_init_httk(DllInfo *dll)
{
    R_registerRoutines(dll, CEntries, NULL, NULL, NULL);
    R_useDynamicSymbols(dll, TRUE);
}
