/* no opt-in: .Call/.C should receive narrowed 32-bit integer data */
#include <R.h>
#include <Rinternals.h>

SEXP a_typecode(SEXP x) { return ScalarInteger((int) TYPEOF(x)); }
SEXP a_first(SEXP x) { return ScalarInteger(INTEGER(x)[0]); }
void a_csum(int *x, int *n, double *out)
{
    double s = 0;
    for (int i = 0; i < *n; i++) s += x[i];
    *out = s;
}
