/* Stands in for an ordinary package that knows nothing about BYTESXP.
   Each function is a shape real package C code actually takes. */
#include <R.h>
#include <Rinternals.h>

/* the common pattern: check the type, then take the pointer */
SEXP typed_switch(SEXP x)
{
    switch (TYPEOF(x)) {
    case INTSXP:  return ScalarInteger(INTEGER(x)[0]);
    case REALSXP: return ScalarReal(REAL(x)[0]);
    default:      error("unsupported type");
    }
}

/* the lazier pattern: assume it is a double vector */
SEXP assume_real(SEXP x) { return ScalarReal(REAL(x)[0]); }

/* the predicate pattern */
SEXP via_isinteger(SEXP x)
{
    if (isInteger(x)) return ScalarInteger(INTEGER(x)[0]);
    return ScalarLogical(0);
}

/* the container pattern: move it around without reading it */
SEXP container(SEXP x) { return Rf_lang2(install("identity"), x); }

/* length-only */
SEXP just_length(SEXP x) { return ScalarInteger((int) XLENGTH(x)); }

/* the untyped escape hatch */
SEXP via_dataptr(SEXP x) { return ScalarInteger(((const int *) DATAPTR_RO(x))[0]); }
