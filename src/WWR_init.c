#include <R_ext/RS.h>
#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

/* FIXME: 
Check these declarations against the C/Fortran source code.
*/

/* .Fortran calls */
extern void F77_NAME(logrank2)(void *, void *, void *, void *, void *, void *, void *);
extern void F77_NAME(wwrnullb)(void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *);
extern void F77_NAME(xgenwr)(void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *);
extern void F77_NAME(xwinratio)(void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *);

static const R_FortranMethodDef FortranEntries[] = {
  {"logrank2",  (DL_FUNC) &F77_NAME(logrank2),   7},
  {"wwrnullb",  (DL_FUNC) &F77_NAME(wwrnullb),  14},
  {"xgenwr",    (DL_FUNC) &F77_NAME(xgenwr),    16},
  {"xwinratio", (DL_FUNC) &F77_NAME(xwinratio), 16},
  {NULL, NULL, 0}
};

void R_init_WWR(DllInfo *dll)
{
  R_registerRoutines(dll, NULL, NULL, FortranEntries, NULL);
  R_useDynamicSymbols(dll, FALSE);
}
