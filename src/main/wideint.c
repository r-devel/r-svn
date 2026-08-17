/*
 *  R : A Computer Language for Statistical Data Analysis
 *
 *  wideint.c -- prototype support for wide (64-bit) integer vectors.
 *
 *  A wide integer vector is a standard INTSXP whose payload holds
 *  R_wideint_t (currently long long) elements, marked with the WIDEINT
 *  gp bit.  There is
 *  deliberately no 64-bit data-pointer accessor; all access is
 *  element-based (INTEGER64_ELT / SET_INTEGER64_ELT).
 */

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <Defn.h>
#include <Internal.h>

#include <errno.h>
#include <math.h>
#include <stdlib.h>
#include <string.h>

SEXP ScalarWideInt(R_wideint_t v)
{
    SEXP ans = allocWideIntVector(1);
    SET_INTEGER64_ELT(ans, 0, v);
    return ans;
}

static R_wideint_t wideFromDouble(double v)
{
    if (ISNAN(v))
	return NA_INTEGER64;
    if (v != floor(v))
	error("cannot coerce fractional value %g to a wide integer", v);
    if (v >= 9223372036854775808.0 /* 2^63 */ ||
	v <= -9223372036854775808.0)
	error("value %g is out of range for a wide integer", v);

    return (R_wideint_t) v;
}

static R_wideint_t wideFromString(SEXP ch)
{
    if (ch == NA_STRING)
	return NA_INTEGER64;

    const char *s = CHAR(ch);
    char *endp;
    errno = 0;
    R_wideint_t v = strtoll(s, &endp, 10);
    if (errno == ERANGE)
	error("value '%s' is out of range for a wide integer", s);
    if (endp == s || *endp != '\0')
	error("cannot coerce '%s' to a wide integer", s);

    return v;
}

/* .Internal(as.wideint(x)) */
attribute_hidden SEXP do_aswideint(SEXP call, SEXP op, SEXP args, SEXP env)
{
    checkArity(op, args);
    SEXP x = CAR(args);

    if (R_isWideInteger(x))
	return x;

    R_xlen_t n = XLENGTH(x);
    SEXP ans = PROTECT(allocWideIntVector(n));

    switch (TYPEOF(x)) {
    case LGLSXP:
	for (R_xlen_t i = 0; i < n; i++) {
	    int v = LOGICAL_ELT(x, i);
	    SET_INTEGER64_ELT(ans, i,
			      v == NA_LOGICAL ? NA_INTEGER64 : (R_wideint_t) v);
	}
	break;
    case INTSXP:
	for (R_xlen_t i = 0; i < n; i++)
	    SET_INTEGER64_ELT(ans, i, INTEGER64_ELT(x, i));
	break;
    case REALSXP:
	for (R_xlen_t i = 0; i < n; i++)
	    SET_INTEGER64_ELT(ans, i, wideFromDouble(REAL_ELT(x, i)));
	break;
    case STRSXP:
	for (R_xlen_t i = 0; i < n; i++)
	    SET_INTEGER64_ELT(ans, i, wideFromString(STRING_ELT(x, i)));
	break;
    default:
	error("cannot coerce type '%s' to a wide integer vector",
	      R_typeToChar(x));
    }

    SHALLOW_DUPLICATE_ATTRIB(ans, x);
    UNPROTECT(1);
    return ans;
}

/* .Internal(is.wideint(x)) */
attribute_hidden SEXP do_iswideint(SEXP call, SEXP op, SEXP args, SEXP env)
{
    checkArity(op, args);
    return ScalarLogical(R_isWideInteger(CAR(args)));
}

/* Coercion of a wide integer vector to other atomic types; called
   from coerceVector().  Coercing to INTSXP is the identity there and
   never reaches this function. */
attribute_hidden SEXP R_wideIntCoerce(SEXP v, SEXPTYPE type)
{
    R_xlen_t n = XLENGTH(v);
    SEXP ans;
    int warn_precision = 0;

    PROTECT(v);
    switch (type) {
    case LGLSXP:
	ans = PROTECT(allocVector(LGLSXP, n));
	for (R_xlen_t i = 0; i < n; i++) {
	    R_wideint_t x = INTEGER64_ELT(v, i);
	    SET_LOGICAL_ELT(ans, i,
			    x == NA_INTEGER64 ? NA_LOGICAL : (x != 0));
	}
	break;
    case REALSXP:
	ans = PROTECT(allocVector(REALSXP, n));
	for (R_xlen_t i = 0; i < n; i++) {
	    R_wideint_t x = INTEGER64_ELT(v, i);
	    if (x == NA_INTEGER64)
		SET_REAL_ELT(ans, i, NA_REAL);
	    else {
		if (x > 9007199254740992LL || x < -9007199254740992LL)
		    warn_precision = 1;
		SET_REAL_ELT(ans, i, (double) x);
	    }
	}
	break;
    case STRSXP:
	ans = PROTECT(allocVector(STRSXP, n));
	for (R_xlen_t i = 0; i < n; i++) {
	    R_wideint_t x = INTEGER64_ELT(v, i);
	    if (x == NA_INTEGER64)
		SET_STRING_ELT(ans, i, NA_STRING);
	    else {
		char buf[32];
		snprintf(buf, sizeof(buf), "%lld", (long long) x);
		SET_STRING_ELT(ans, i, mkChar(buf));
	    }
	}
	break;
    case CPLXSXP:
	ans = PROTECT(allocVector(CPLXSXP, n));
	for (R_xlen_t i = 0; i < n; i++) {
	    R_wideint_t x = INTEGER64_ELT(v, i);
	    Rcomplex z;
	    if (x == NA_INTEGER64) {
		z.r = NA_REAL; z.i = NA_REAL;
	    }
	    else {
		if (x > 9007199254740992LL || x < -9007199254740992LL)
		    warn_precision = 1;
		z.r = (double) x; z.i = 0.0;
	    }
	    SET_COMPLEX_ELT(ans, i, z);
	}
	break;
    case VECSXP:
	ans = PROTECT(allocVector(VECSXP, n));
	for (R_xlen_t i = 0; i < n; i++)
	    SET_VECTOR_ELT(ans, i, ScalarWideInt(INTEGER64_ELT(v, i)));
	break;
    default:
	error("cannot coerce a wide integer vector to type '%s'",
	      type2char(type));
    }

    if (warn_precision)
	warning("coercing wide integers above 2^53 loses precision");

    SHALLOW_DUPLICATE_ATTRIB(ans, v);
    UNPROTECT(2);
    return ans;
}

/* Format a wide integer vector as a right-justified character vector,
   used by printing. */
attribute_hidden SEXP R_formatWideInt(SEXP x)
{
    R_xlen_t n = XLENGTH(x);
    int w = 0;

    for (R_xlen_t i = 0; i < n; i++) {
	R_wideint_t v = INTEGER64_ELT(x, i);
	char buf[32];
	int len;
	if (v == NA_INTEGER64)
	    len = 2; /* NA */
	else
	    len = snprintf(buf, sizeof(buf), "%lld", (long long) v);
	if (len > w)
	    w = len;
    }

    SEXP ans = PROTECT(allocVector(STRSXP, n));
    for (R_xlen_t i = 0; i < n; i++) {
	R_wideint_t v = INTEGER64_ELT(x, i);
	char buf[40];
	if (v == NA_INTEGER64)
	    snprintf(buf, sizeof(buf), "%*s", w, "NA");
	else
	    snprintf(buf, sizeof(buf), "%*lld", w, (long long) v);
	SET_STRING_ELT(ans, i, mkChar(buf));
    }
    UNPROTECT(1);
    return ans;
}
