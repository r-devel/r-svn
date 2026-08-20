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

/* ------------------------------------------------------------------
   The opt-in side: a package that deliberately produces and consumes
   'bytes' vectors, as an ingest package (Arrow, Parquet, a database
   driver) would.  Everything below uses only what Rinternals.h
   declares.
   ------------------------------------------------------------------ */

#include <stdint.h>
#include <string.h>

/* build a uint64 vector the way a column reader would: allocate, then
   copy native-order payload straight in, marking nulls as it goes */
SEXP make_uint64(SEXP n_)
{
    R_xlen_t n = (R_xlen_t) asInteger(n_);
    SEXP ans = PROTECT(R_allocBytesVector(n, 8, BYTES_UNSIGNED, TRUE));

    for (R_xlen_t i = 0; i < n; i++) {
	uint64_t v = ((uint64_t) 1 << 62) + (uint64_t) i;
	memcpy(R_bytesElt(ans, i), &v, sizeof v);
    }
    if (n > 0) R_bytesSetNA(ans, 0);

    UNPROTECT(1);

    return ans;
}

/* what a consumer can learn about a vector it was handed */
SEXP describe(SEXP x)
{
    if (!R_isBytes(x)) error("not a 'bytes' vector");

    SEXP ans = PROTECT(allocVector(INTSXP, 4));
    INTEGER(ans)[0] = (int) XLENGTH(x);
    INTEGER(ans)[1] = R_bytesWidth(x);
    INTEGER(ans)[2] = R_bytesKind(x);
    INTEGER(ans)[3] = R_bytesHasNA(x);
    UNPROTECT(1);

    return ans;
}

/* the consumer pattern: read the payload, skipping missing values */
SEXP sum_uint64(SEXP x)
{
    if (!R_isBytes(x) || R_bytesKind(x) != BYTES_UNSIGNED ||
	R_bytesWidth(x) != 8)
	error("expected a uint64 vector");

    uint64_t total = 0;
    for (R_xlen_t i = 0; i < XLENGTH(x); i++) {
	if (R_bytesIsNA(x, i)) continue;
	uint64_t v;
	memcpy(&v, R_bytesEltRO(x, i), sizeof v);
	total += v;
    }

    return ScalarReal((double) total);
}

/* a whole-payload copy, which is what BYTES_RO() is for */
SEXP first_byte_of_each(SEXP x)
{
    if (!R_isBytes(x)) error("not a 'bytes' vector");

    R_xlen_t n = XLENGTH(x);
    int w = R_bytesWidth(x);
    const Rbyte *p = BYTES_RO(x);

    SEXP ans = PROTECT(allocVector(INTSXP, n));
    for (R_xlen_t i = 0; i < n; i++)
	INTEGER(ans)[i] = p[i * w];
    UNPROTECT(1);

    return ans;
}

/* the accessors type-check, so reaching for the wrong one is an error
   rather than a misreading */
SEXP width_of_anything(SEXP x) { return ScalarInteger(R_bytesWidth(x)); }
SEXP bytes_of_anything(SEXP x) { return ScalarInteger(BYTES_RO(x)[0]); }
