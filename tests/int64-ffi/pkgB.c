/* opts in via R_useInt64: receives INT64SXP directly */
#include <R.h>
#include <Rinternals.h>
#include <R_ext/Rdynload.h>

SEXP b_typecode(SEXP x) { return ScalarInteger((int) TYPEOF(x)); }
SEXP b_first64(SEXP x) { return ScalarReal((double) INT64_ELT(x, 0)); }

void R_init_pkgB(DllInfo *dll)
{
    R_useInt64(dll, TRUE);
}
