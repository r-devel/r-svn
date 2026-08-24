/* Stands in for an ordinary package that knows nothing about XINTSXP.
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
   'xinteger' vectors, as an ingest package (Arrow, Parquet, a database
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
    SEXP ans = PROTECT(R_allocXIntVector(n, 8, XINT_UNSIGNED, TRUE));

    for (R_xlen_t i = 0; i < n; i++) {
	uint64_t v = ((uint64_t) 1 << 62) + (uint64_t) i;
	memcpy(R_xintElt(ans, i), &v, sizeof v);
    }
    if (n > 0) R_xintSetNA(ans, 0);

    UNPROTECT(1);

    return ans;
}

/* what a consumer can learn about a vector it was handed */
SEXP describe(SEXP x)
{
    if (!R_isXInt(x)) error("not an 'xinteger' vector");

    SEXP ans = PROTECT(allocVector(INTSXP, 4));
    INTEGER(ans)[0] = (int) XLENGTH(x);
    INTEGER(ans)[1] = R_xintWidth(x);
    INTEGER(ans)[2] = R_xintKind(x);
    INTEGER(ans)[3] = R_xintHasNA(x);
    UNPROTECT(1);

    return ans;
}

/* the consumer pattern: read the payload, skipping missing values */
SEXP sum_uint64(SEXP x)
{
    if (!R_isXInt(x) || R_xintKind(x) != XINT_UNSIGNED ||
	R_xintWidth(x) != 8)
	error("expected a uint64 vector");

    uint64_t total = 0;
    for (R_xlen_t i = 0; i < XLENGTH(x); i++) {
	if (R_xintIsNA(x, i)) continue;
	uint64_t v;
	memcpy(&v, R_xintEltRO(x, i), sizeof v);
	total += v;
    }

    return ScalarReal((double) total);
}

/* a whole-payload copy, which is what XINTEGER_RO() is for */
SEXP first_byte_of_each(SEXP x)
{
    if (!R_isXInt(x)) error("not an 'xinteger' vector");

    R_xlen_t n = XLENGTH(x);
    int w = R_xintWidth(x);
    const Rbyte *p = XINTEGER_RO(x);

    SEXP ans = PROTECT(allocVector(INTSXP, n));
    for (R_xlen_t i = 0; i < n; i++)
	INTEGER(ans)[i] = p[i * w];
    UNPROTECT(1);

    return ans;
}

/* the accessors type-check, so reaching for the wrong one is an error
   rather than a misreading */
SEXP width_of_anything(SEXP x) { return ScalarInteger(R_xintWidth(x)); }
SEXP xinteger_of_anything(SEXP x) { return ScalarInteger(XINTEGER_RO(x)[0]); }

/* ------------------------------------------------------------------
   An ALTREP class whose serialized state carries an 'xinteger' vector.
   Serialization writes that state and never the object's own elements,
   so its own SEXPTYPE says nothing about what will reach the stream --
   which is what the writer has to know before it picks a version.
   ------------------------------------------------------------------ */

#include <R_ext/Altrep.h>

static R_altrep_class_t xint_state_class;
static int serialized_state_calls;

static SEXP bsc_Serialized_state(SEXP x)
{
    serialized_state_calls++;
    SEXP state = PROTECT(allocVector(VECSXP, 2));
    SET_VECTOR_ELT(state, 0, R_altrep_data1(x));
    SET_VECTOR_ELT(state, 1, R_altrep_data2(x));
    UNPROTECT(1);

    return state;
}

SEXP reset_serialized_state_calls(void)
{
    serialized_state_calls = 0;
    return R_NilValue;
}

SEXP get_serialized_state_calls(void)
{
    return ScalarInteger(serialized_state_calls);
}

static SEXP bsc_Unserialize(SEXP class_, SEXP state)
{
    return R_new_altrep(xint_state_class, VECTOR_ELT(state, 0),
			VECTOR_ELT(state, 1));
}

static R_xlen_t bsc_Length(SEXP x) { return XLENGTH(R_altrep_data1(x)); }

static void *bsc_Dataptr(SEXP x, Rboolean writeable)
{
    return (void *) INTEGER(R_altrep_data1(x));
}

static const void *bsc_Dataptr_or_null(SEXP x)
{
    return (const void *) INTEGER_RO(R_altrep_data1(x));
}

static int bsc_Elt(SEXP x, R_xlen_t i)
{
    return INTEGER_RO(R_altrep_data1(x))[i];
}

SEXP init_altrep(void)
{
    xint_state_class = R_make_altinteger_class("xint_state", "pkg", NULL);

    R_set_altrep_Serialized_state_method(xint_state_class,
					 bsc_Serialized_state);
    R_set_altrep_Unserialize_method(xint_state_class, bsc_Unserialize);
    R_set_altrep_Length_method(xint_state_class, bsc_Length);
    R_set_altvec_Dataptr_method(xint_state_class, bsc_Dataptr);
    R_set_altvec_Dataptr_or_null_method(xint_state_class,
					bsc_Dataptr_or_null);
    R_set_altinteger_Elt_method(xint_state_class, bsc_Elt);

    return R_NilValue;
}

/* an integer vector to all appearances, with a uint64 vector hidden in
   the state it will be written as */
SEXP make_altrep_with_xint(SEXP payload)
{
    SEXP hidden = PROTECT(make_uint64(ScalarInteger(2)));
    SEXP ans = R_new_altrep(xint_state_class, payload, hidden);
    UNPROTECT(1);

    return ans;
}

/* the same class with nothing hidden in it.  What a package's class
   writes is whatever its method builds, and the preflight that settles
   the serialization version cannot see that without calling the method
   a second time, so an object of one is version 4 whether or not it is
   carrying anything. */
SEXP make_altrep_plain(SEXP payload)
{
    return R_new_altrep(xint_state_class, payload, R_NilValue);
}

/* The resizable-vector API in Rinternals.h: R_allocResizableVector()
   has no way to name a width and a kind, so the type is not one it can
   make, and R_duplicateAsResizable() must not accept what its sibling
   cannot produce. */
SEXP resizable(SEXP x) { return R_duplicateAsResizable(x); }

/* A reader choosing what to map a source column onto has to know
   whether an element type is allocatable BEFORE it allocates, because
   the allocator's refusal is an R error and unwinding out of a partly
   built column reader is not what such code wants -- in C++ it skips
   destructors outright.  R_xintTypeSupported() answers without
   allocating and without raising anything. */
SEXP type_supported(SEXP width, SEXP kind)
{
    return ScalarLogical(R_xintTypeSupported(asInteger(width),
					      asInteger(kind)));
}

/* and it has to agree with what R_allocXIntVector() actually does */
SEXP alloc_succeeds(SEXP width, SEXP kind)
{
    SEXP v = PROTECT(R_allocXIntVector(1, asInteger(width), asInteger(kind),
					TRUE));
    UNPROTECT(1);

    return ScalarLogical(TYPEOF(v) == XINTSXP);
}
