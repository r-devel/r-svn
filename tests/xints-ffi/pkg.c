/* Stands in for an ordinary package that knows nothing about ALTSXP.
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
static R_altrep_class_t dispatch_left_class;
static R_altrep_class_t dispatch_right_class;
static int serialized_state_calls;
static int binary_dispatch_calls;

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

/* Two package ALTSXP classes used to pin binary negotiation.  The left
   class declines; the right class reports whether it was the left or right
   operand without changing operand order. */
static R_xlen_t dispatch_Length(SEXP x) { return XLENGTH(R_altrep_data1(x)); }
static size_t dispatch_Element_size(SEXP x) { return 1; }
static void dispatch_Elt(SEXP x, R_xlen_t i, void *buf)
{
    *(Rbyte *) buf = RAW_ELT(R_altrep_data1(x), i);
}
static void dispatch_Set_elt(SEXP x, R_xlen_t i, const void *buf)
{
    SET_RAW_ELT(R_altrep_data1(x), i, *(const Rbyte *) buf);
}
static SEXP dispatch_decline_binary(SEXP dispatch, SEXP x, SEXP y,
				    int op, SEXP call)
{
    binary_dispatch_calls++;
    return NULL;
}
static SEXP dispatch_handle_binary(SEXP dispatch, SEXP x, SEXP y,
				   int op, SEXP call)
{
    binary_dispatch_calls++;
    return ScalarInteger(dispatch == x ? 1 : 2);
}
static SEXP dispatch_decline_compare(SEXP dispatch, SEXP x, SEXP y,
				     int op, SEXP call)
{
    return NULL;
}
static SEXP dispatch_handle_compare(SEXP dispatch, SEXP x, SEXP y,
				    int op, SEXP call)
{
    return ScalarLogical(dispatch == y);
}

static SEXP dispatch_unary(SEXP x, int op, SEXP call)
{
    return ScalarInteger(10 + op);
}

static SEXP dispatch_coerce(SEXP x, int type)
{
    if (type != INTSXP) return NULL;
    R_xlen_t n = XLENGTH(x);
    SEXP ans = PROTECT(allocVector(INTSXP, n));
    for (R_xlen_t i = 0; i < n; i++)
	INTEGER(ans)[i] = RAW_ELT(R_altrep_data1(x), i);
    UNPROTECT(1);
    return ans;
}

static unsigned int dispatch_hash(SEXP x, R_xlen_t i)
{
    return 2166136261U ^ RAW_ELT(R_altrep_data1(x), i);
}

static SEXP dispatch_format(SEXP x, SEXP options)
{
    R_xlen_t n = XLENGTH(x);
    SEXP ans = PROTECT(allocVector(STRSXP, n));
    char buf[32];
    for (R_xlen_t i = 0; i < n; i++) {
	snprintf(buf, sizeof buf, "opaque:%u",
		 (unsigned int) RAW_ELT(R_altrep_data1(x), i));
	SET_STRING_ELT(ans, i, mkChar(buf));
    }
    UNPROTECT(1);
    return ans;
}

static SEXP dispatch_summary(SEXP x, int op, SEXP args,
			     Rboolean narm, SEXP call)
{
    return ScalarInteger(100 + op);
}

static SEXP dispatch_combine(SEXP x, SEXP args, SEXP call)
{
    return ScalarInteger(length(args));
}

SEXP reset_binary_dispatch_calls(void)
{
    binary_dispatch_calls = 0;
    return R_NilValue;
}

SEXP get_binary_dispatch_calls(void)
{
    return ScalarInteger(binary_dispatch_calls);
}

SEXP make_dispatch_pair(void)
{
    SEXP data = PROTECT(allocVector(RAWSXP, 1));
    RAW(data)[0] = 1;
    SEXP left = PROTECT(R_new_altrep(dispatch_left_class, data, R_NilValue));
    SEXP right = PROTECT(R_new_altrep(dispatch_right_class, data, R_NilValue));
    SEXP ans = PROTECT(allocVector(VECSXP, 2));
    SET_VECTOR_ELT(ans, 0, left);
    SET_VECTOR_ELT(ans, 1, right);
    UNPROTECT(4);
    return ans;
}

SEXP make_dispatch_right(SEXP data)
{
    if (TYPEOF(data) != RAWSXP) error("expected raw data");
    return R_new_altrep(dispatch_right_class, data, R_NilValue);
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

    dispatch_left_class = R_make_alt_class("dispatch_left", "pkg", NULL);
    dispatch_right_class = R_make_alt_class("dispatch_right", "pkg", NULL);
#define SET_DISPATCH_REP_METHODS(cls) do {                              \
    R_set_altrep_Length_method(cls, dispatch_Length);                   \
    R_set_alt_Element_size_method(cls, dispatch_Element_size);          \
    R_set_alt_Elt_method(cls, dispatch_Elt);                            \
    R_set_alt_Set_elt_method(cls, dispatch_Set_elt);                    \
} while (0)
    SET_DISPATCH_REP_METHODS(dispatch_left_class);
    SET_DISPATCH_REP_METHODS(dispatch_right_class);
#undef SET_DISPATCH_REP_METHODS
    R_set_alt_Binary_op_method(dispatch_left_class,
				       dispatch_decline_binary);
    R_set_alt_Binary_op_method(dispatch_right_class,
				       dispatch_handle_binary);
    R_set_alt_Compare_method(dispatch_left_class,
				     dispatch_decline_compare);
    R_set_alt_Compare_method(dispatch_right_class,
				     dispatch_handle_compare);
    R_set_alt_Unary_op_method(dispatch_right_class, dispatch_unary);
    R_set_altrep_Coerce_method(dispatch_right_class, dispatch_coerce);
    R_set_alt_Hash_method(dispatch_right_class, dispatch_hash);
    R_set_alt_Format_method(dispatch_right_class, dispatch_format);
    R_set_alt_Summary_method(dispatch_right_class, dispatch_summary);
    R_set_alt_Combine_method(dispatch_right_class, dispatch_combine);

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

/* The same class with nothing hidden in it.  The outer ALTINTEGER object
   continues to use the ordinary ALTREP state path. */
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

    return ScalarLogical(TYPEOF(v) == ALTSXP);
}
