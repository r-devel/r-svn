/* Test-only ALTSXP classes for tests/altsxp.R.

   The classes deliberately have no data pointer and report short positive
   region counts.  This exercises the generic consumer API independently of
   the base int64 class, whose region methods always complete a request. */

#include <R.h>
#include <Rinternals.h>
#include <R_ext/Altrep.h>
#include <stdint.h>
#include <R_ext/Memory.h>
#include <R_ext/Rdynload.h>
#include <R_ext/Visibility.h>

#include <string.h>

enum { META_GET_CALLS, META_SET_CALLS, META_N };
enum { GET_CHUNK = 3, SET_CHUNK = 2, WIDE_ELT_SIZE = 4096 };

/* The classes differ only in what they promise about their elements:
   K_BYTE and K_WIDE are two unrelated element types; K_TWIN reports
   K_BYTE's element type at sixteen times the width, which nothing may
   take as licence to read one at the other's size; K_PLAIN registers no
   Elt_type method at all, so it exercises the default; K_CMP is the
   one with an ordering, so that sort() can reach its Set_region; and K_BARE
   registers neither a Traits method nor a Compare, which is the class
   R_ext/Altrep.h says has no notion of equality R could use; and K_HASH
   hashes and compares for itself, so its bytes need not decide equality.
   K_MOD hashes for itself like K_HASH but reports K_CMP's element type at
   K_CMP's width, so the two are matchable against each other while
   disagreeing about how equality is decided -- the one pairing that can
   put two different hash routes on the same table.  K_BOTH declares
   BITWISE_EQ *and* registers Hash and Compare, at K_CMP's element type
   again: the header says the bit wins and the Hash is not consulted, so it
   has to keep interoperating with K_CMP through the byte route.  K_FAKE64
   claims base int64's element type at int64's width without being one of
   its objects, which is what the header offers as the interop mechanism: it
   must be read through its region method, never by casting whatever it
   keeps in data1 (one byte per element, here).  K_SHORTFMT answers one
   element fewer than it was asked for, from Format and from Coerce_from
   alike, which pins the count contract for those two as the region methods
   above pin theirs.  It takes the open traits so that match() can reach its
   Coerce_from at all: a class that cannot be NA is never promoted into.

   The last three register no Elt_type method either, so they take the
   qualified pkg::class default and can adopt a published name instead:
   K_SHARE claims its own with R_altsxp_register_type(), K_SHARE2 adopts it
   with R_altsxp_share_type() at the same width, and K_SHAREW tries to adopt
   it at a different one, which is the promise R can actually check. */
enum { K_BYTE, K_WIDE, K_TWIN, K_PLAIN, K_CMP, K_BARE, K_HASH, K_MOD, K_BOTH,
       K_FAKE64, K_SHORTFMT, K_SHARE, K_SHARE2, K_SHAREW, K_N };

static R_altrep_class_t test_classes[K_N];
static SEXP test_type_syms[K_N];

static const size_t test_elt_sizes[K_N] = {
    1, WIDE_ELT_SIZE, WIDE_ELT_SIZE, 1, 1, 1, WIDE_ELT_SIZE, 1, 1,
    sizeof(int64_t), 1, 1, 1, WIDE_ELT_SIZE
};

static const char *const test_class_names[K_N] = {
    "short_byte", "wide_byte", "twin_byte", "plain_byte", "cmp_byte",
    "bare_byte", "hash_byte", "mod_byte", "both_byte", "fake_int64",
    "shortfmt_byte", "share_byte", "share2_byte", "sharew_byte"
};

static int test_kind(SEXP x)
{
    for (int k = 0; k < K_N; k++) {
	if (ALTREP(x) ? R_altrep_inherits(x, test_classes[k])
	    : x == R_SEXP(test_classes[k])) /* class object passed to New() */
	    return k;
    }
    error("not a test ALTSXP object");
}

static R_altrep_class_t test_class(SEXP proto)
{
    return test_classes[test_kind(proto)];
}

static R_xlen_t test_ncopy(SEXP x, R_xlen_t i, R_xlen_t n)
{
    R_xlen_t size = XLENGTH(R_altrep_data1(x));
    if (i < 0 || i >= size || n <= 0)
	return 0;
    return size - i < n ? size - i : n;
}

static SEXP test_make(R_altrep_class_t cls, SEXP data)
{
    SEXP meta = PROTECT(allocVector(INTSXP, META_N));
    INTEGER(meta)[META_GET_CALLS] = 0;
    INTEGER(meta)[META_SET_CALLS] = 0;
    SEXP ans = R_new_altrep(cls, data, meta);
    UNPROTECT(1);
    return ans;
}

static R_xlen_t test_length(SEXP x)
{
    return XLENGTH(R_altrep_data1(x));
}

static SEXP test_elt_type(SEXP x)
{
    return test_type_syms[test_kind(x)];
}

static size_t test_elt_size(SEXP x)
{
    return test_elt_sizes[test_kind(x)];
}

/* Only K_CMP has one, and R asks only about elements of one element type */
static int test_compare(SEXP x, R_xlen_t i, SEXP y, R_xlen_t j)
{
    Rbyte a = RAW(R_altrep_data1(x))[i], b = RAW(R_altrep_data1(y))[j];
    return (a > b) - (a < b);
}

/* K_HASH's value is its byte modulo 16, so 0x01 and 0x11 are one value in two
   spellings -- the shape a floating element type has with +0 and -0.
   R_ALTREP_TRAITS_BITWISE_EQ would be a lie for it, so it supplies its own
   Hash and Compare, which read the element where it lies and therefore also
   sidestep the staging-buffer width cap. */
static int test_mod_value(SEXP x, R_xlen_t i)
{
    return RAW(R_altrep_data1(x))[i] % 16;
}

static int test_mod_compare(SEXP x, R_xlen_t i, SEXP y, R_xlen_t j)
{
    int a = test_mod_value(x, i), b = test_mod_value(y, j);
    return (a > b) - (a < b);
}

static unsigned int test_mod_hash(SEXP x, R_xlen_t i)
{
    return (unsigned int) test_mod_value(x, i) * 2654435761u;
}

/* deliberately one element short of what it was asked for: every consumer
   indexes the answer at the count it requested */
static SEXP test_short_format(SEXP x, R_xlen_t i, R_xlen_t n)
{
    R_xlen_t give = n > 0 ? n - 1 : 0;
    SEXP ans = PROTECT(allocVector(STRSXP, give));
    const Rbyte *p = RAW(R_altrep_data1(x));
    char buf[32];
    for (R_xlen_t k = 0; k < give; k++) {
	snprintf(buf, sizeof buf, "%d", (int) p[i + k]);
	SET_STRING_ELT(ans, k, mkChar(buf));
    }
    UNPROTECT(1);
    return ans;
}

/* the same one short, for the other method whose count consumers rely on */
static SEXP test_short_coerce_from(SEXP proto, SEXP from)
{
    R_xlen_t n = XLENGTH(from);
    R_xlen_t give = n > 0 ? n - 1 : 0;
    SEXP data = PROTECT(allocVector(RAWSXP, give));
    for (R_xlen_t k = 0; k < give; k++)
	RAW(data)[k] = (Rbyte) (k + 1);
    SEXP ans = test_make(test_class(proto), data);
    UNPROTECT(1);
    return ans;
}

static SEXP test_new(SEXP proto, R_xlen_t n, Rboolean zeroinit)
{
    if (n < 0)
	error("negative test ALTSXP length");
    SEXP data = PROTECT(allocVector(RAWSXP, n));
    if (zeroinit && n > 0) memset(RAW(data), 0, (size_t) n);
    SEXP ans = test_make(test_class(proto), data);
    UNPROTECT(1);
    return ans;
}

static R_xlen_t
test_get_region(SEXP x, R_xlen_t i, R_xlen_t n, void *buf)
{
    R_xlen_t ncopy = test_ncopy(x, i, n);
    if (ncopy > GET_CHUNK) ncopy = GET_CHUNK;

    size_t esz = test_elt_size(x);
    const Rbyte *src = RAW(R_altrep_data1(x));
    unsigned char *dst = buf;
    for (R_xlen_t k = 0; k < ncopy; k++)
	memset(dst + (size_t) k * esz, src[i + k], esz);

    INTEGER(R_altrep_data2(x))[META_GET_CALLS]++;
    return ncopy;
}

static R_xlen_t
test_set_region(SEXP x, R_xlen_t i, R_xlen_t n, const void *buf)
{
    R_xlen_t ncopy = test_ncopy(x, i, n);
    if (ncopy > SET_CHUNK) ncopy = SET_CHUNK;

    size_t esz = test_elt_size(x);
    Rbyte *dst = RAW(R_altrep_data1(x));
    const unsigned char *src = buf;
    for (R_xlen_t k = 0; k < ncopy; k++)
	dst[i + k] = src[(size_t) k * esz];

    INTEGER(R_altrep_data2(x))[META_SET_CALLS]++;
    return ncopy;
}

static R_xlen_t
test_is_na_region(SEXP x, R_xlen_t i, R_xlen_t n, int *buf)
{
    R_xlen_t ncopy = test_ncopy(x, i, n);
    if (ncopy > GET_CHUNK) ncopy = GET_CHUNK;
    for (R_xlen_t k = 0; k < ncopy; k++) buf[k] = FALSE;
    return ncopy;
}

static unsigned int test_traits(SEXP x)
{
    return R_ALTREP_TRAITS_BITWISE_EQ | R_ALTREP_TRAITS_NOT_NULLABLE;
}

/* K_FAKE64's: no BITWISE_EQ, so identical() has to ask Compare rather than
   memcmp, and nullable, so it is comparable with a default as.int64() vector
   at all. */
static unsigned int test_open_traits(SEXP x)
{
    return 0u;
}

/* Sharing an element type means this is handed base int64 objects as well as
   its own, and the only thing the shared name promises is the C type of an
   element -- not where the other class keeps it.  So both operands are read
   through the region method, which is exactly what i64_Compare() has to do
   in the other direction. */
static int test_fake64_compare(SEXP x, R_xlen_t i, SEXP y, R_xlen_t j)
{
    int64_t a, b;
    R_altsxp_get_region(x, i, 1, &a);
    R_altsxp_get_region(y, j, 1, &b);
    return (a > b) - (a < b);
}

static void init_test_class(R_altrep_class_t cls)
{
    R_set_altrep_Length_method(cls, test_length);
    R_set_altsxp_Elt_size_method(cls, test_elt_size);
    R_set_altsxp_New_method(cls, test_new);
    R_set_altsxp_Get_region_method(cls, test_get_region);
    R_set_altsxp_Set_region_method(cls, test_set_region);
    R_set_altsxp_Is_na_region_method(cls, test_is_na_region);
}

static SEXP test_constructor(SEXP data, SEXP wide)
{
    if (TYPEOF(data) != RAWSXP)
	error("test ALTSXP data must be raw");
    int is_wide = asLogical(wide);
    if (is_wide == NA_LOGICAL)
	error("invalid wide flag");

    SEXP copy = PROTECT(duplicate(data));
    SEXP ans = test_make(test_classes[is_wide ? K_WIDE : K_BYTE], copy);
    UNPROTECT(1);
    return ans;
}

static int test_kind_by_name(SEXP kind)
{
    if (TYPEOF(kind) != STRSXP || XLENGTH(kind) != 1)
	error("invalid test class name");

    const char *name = CHAR(STRING_ELT(kind, 0));
    int k = 0;
    while (k < K_N && strcmp(name, test_class_names[k]) != 0)
	k++;
    if (k == K_N)
	error("no such test class: %s", name);

    return k;
}

/* the same, naming any of the classes above */
static SEXP test_constructor2(SEXP kind, SEXP data)
{
    if (TYPEOF(data) != RAWSXP)
	error("test ALTSXP data must be raw");

    int k = test_kind_by_name(kind);

    SEXP copy = PROTECT(duplicate(data));
    SEXP ans = test_make(test_classes[k], copy);
    UNPROTECT(1);
    return ans;
}

static SEXP test_contents(SEXP x)
{
    return duplicate(R_altrep_data1(x));
}

static SEXP test_counts(SEXP x)
{
    return duplicate(R_altrep_data2(x));
}

static R_xlen_t test_index(SEXP x, const char *what)
{
    double value = asReal(x);
    if (!R_FINITE(value) || value < 0 || value > (double) R_XLEN_T_MAX ||
	value != (double) (R_xlen_t) value)
	error("invalid %s", what);
    return (R_xlen_t) value;
}

static SEXP test_copy(SEXP x, SEXP di, SEXP si, SEXP n)
{
    R_xlen_t moved = R_altsxp_copy_region(x, test_index(di, "destination"),
					x, test_index(si, "source"),
					test_index(n, "count"));
    return ScalarReal((double) moved);
}

/* R_altsxp_set_na_region() on any ALTSXP, base classes included: no base
   caller reaches it with an object whose sortedness or no-NA answer has
   already been cached, but a package can, and a class that writes behind
   its own Dataptr would then keep the stale answer. */
static SEXP test_set_na(SEXP x, SEXP i, SEXP n)
{
    R_xlen_t set = R_altsxp_set_na_region(x, test_index(i, "index"),
					  test_index(n, "count"));
    return ScalarReal((double) set);
}

static SEXP test_as_list_vmax(SEXP x)
{
    const void *before = vmaxget();
    SEXP value = PROTECT(coerceVector(x, VECSXP));
    int restored = vmaxget() == before;
    vmaxset(before); /* also clean up when testing a broken implementation */

    SEXP ans = PROTECT(allocVector(VECSXP, 2));
    SET_VECTOR_ELT(ans, 0, value);
    SET_VECTOR_ELT(ans, 1, ScalarLogical(restored));
    UNPROTECT(2);
    return ans;
}

/* R_altsxp_register_type() and R_altsxp_share_type() are load-time calls in
   real package code.  They are reachable from R here so that the rules they
   enforce -- and the ones that raise -- can be tested from tests/altsxp.R. */
static SEXP test_register_type(SEXP kind)
{
    R_altsxp_register_type(test_classes[test_kind_by_name(kind)]);
    return R_NilValue;
}

static SEXP test_share_type(SEXP kind, SEXP name)
{
    if (TYPEOF(name) != STRSXP || XLENGTH(name) != 1)
	error("invalid element type name");

    R_altsxp_share_type(test_classes[test_kind_by_name(kind)],
			CHAR(STRING_ELT(name, 0)));
    return R_NilValue;
}

static SEXP test_type_supported(SEXP name)
{
    if (TYPEOF(name) != STRSXP || XLENGTH(name) != 1)
	error("invalid element type name");

    return ScalarLogical(R_altsxp_type_supported(CHAR(STRING_ELT(name, 0))));
}

/* allocating by name, with no instance to build from -- the shape an ingest
   package has when it is filling a column it was not handed */
static SEXP test_alloc_by_name(SEXP name, SEXP n)
{
    if (TYPEOF(name) != STRSXP || XLENGTH(name) != 1)
	error("invalid element type name");

    return R_altsxp_alloc(CHAR(STRING_ELT(name, 0)),
			  (R_xlen_t) asInteger(n), TRUE);
}

/* the class object as a prototype: a class making its first instance */
static SEXP test_new_from_class(SEXP kind, SEXP n)
{
    return R_altsxp_new(R_SEXP(test_classes[test_kind_by_name(kind)]),
			(R_xlen_t) asInteger(n), TRUE);
}

/* Whether the guarded pointer opens for this (element type, width) pair.
   The test classes have no Dataptr method at all, so it never does for one
   of them -- which is the case R_altsxp_dataptr_or_copy() below exists to
   spare every consumer from handling twice. */
static SEXP test_dataptr(SEXP x, SEXP name, SEXP size)
{
    if (TYPEOF(name) != STRSXP || XLENGTH(name) != 1)
	error("invalid element type name");

    const void *p = R_altsxp_dataptr_ro(x, install(CHAR(STRING_ELT(name, 0))),
					(size_t) asInteger(size));
    return ScalarLogical(p != NULL);
}

static SEXP test_dataptr_or_copy(SEXP x, SEXP name, SEXP size)
{
    if (TYPEOF(name) != STRSXP || XLENGTH(name) != 1)
	error("invalid element type name");

    size_t esz = (size_t) asInteger(size);
    const void *vmax = vmaxget();
    const void *p = R_altsxp_dataptr_or_copy(x,
					     install(CHAR(STRING_ELT(name, 0))),
					     esz);
    if (p == NULL) {
	vmaxset(vmax);
	return R_NilValue;
    }

    R_xlen_t n = XLENGTH(x);
    SEXP ans = PROTECT(allocVector(RAWSXP, n * (R_xlen_t) esz));
    if (n > 0) memcpy(RAW(ans), p, (size_t) n * esz);
    UNPROTECT(1);
    vmaxset(vmax);

    return ans;
}

static const R_CallMethodDef call_methods[] = {
    {"C_altsxp_test_constructor", (DL_FUNC) &test_constructor, 2},
    {"C_altsxp_test_constructor2", (DL_FUNC) &test_constructor2, 2},
    {"C_altsxp_test_contents", (DL_FUNC) &test_contents, 1},
    {"C_altsxp_test_counts", (DL_FUNC) &test_counts, 1},
    {"C_altsxp_test_copy", (DL_FUNC) &test_copy, 4},
    {"C_altsxp_test_set_na", (DL_FUNC) &test_set_na, 3},
    {"C_altsxp_test_as_list_vmax", (DL_FUNC) &test_as_list_vmax, 1},
    {"C_altsxp_test_register_type", (DL_FUNC) &test_register_type, 1},
    {"C_altsxp_test_share_type", (DL_FUNC) &test_share_type, 2},
    {"C_altsxp_test_type_supported", (DL_FUNC) &test_type_supported, 1},
    {"C_altsxp_test_alloc_by_name", (DL_FUNC) &test_alloc_by_name, 2},
    {"C_altsxp_test_new_from_class", (DL_FUNC) &test_new_from_class, 2},
    {"C_altsxp_test_dataptr", (DL_FUNC) &test_dataptr, 3},
    {"C_altsxp_test_dataptr_or_copy", (DL_FUNC) &test_dataptr_or_copy, 3},
    {NULL, NULL, 0}
};

void attribute_visible R_init_altsxp_test(DllInfo *dll)
{
    SEXP byte_type = install("altsxp_test_byte");
    test_type_syms[K_BYTE] = byte_type;
    test_type_syms[K_WIDE] = install("altsxp_test_wide");
    /* deliberately K_BYTE's element type at a different width */
    test_type_syms[K_TWIN] = byte_type;
    /* no Elt_type method: see below */
    test_type_syms[K_PLAIN] = NULL;
    test_type_syms[K_SHARE] = NULL;
    test_type_syms[K_SHARE2] = NULL;
    test_type_syms[K_SHAREW] = NULL;
    SEXP cmp_type = install("altsxp_test_cmp");
    test_type_syms[K_CMP] = cmp_type;
    /* deliberately K_CMP's element type, at K_CMP's width */
    test_type_syms[K_MOD] = cmp_type;
    test_type_syms[K_BOTH] = cmp_type;
    /* deliberately base int64's element type, at base int64's width */
    test_type_syms[K_FAKE64] = install("int64");
    test_type_syms[K_SHORTFMT] = install("altsxp_test_shortfmt");
    test_type_syms[K_BARE] = install("altsxp_test_bare");
    test_type_syms[K_HASH] = install("altsxp_test_hash");

    for (int k = 0; k < K_N; k++) {
	test_classes[k] = R_make_altsxp_class(test_class_names[k],
					      "altsxpTest", dll);
	init_test_class(test_classes[k]);
	/* K_PLAIN and the K_SHARE* trio take the default, which has to name
	   the package as well as the class or it would collide with any other
	   "plain_byte" */
	if (k != K_PLAIN && k != K_SHARE && k != K_SHARE2 && k != K_SHAREW)
	    R_set_altsxp_Elt_type_method(test_classes[k], test_elt_type);
	/* K_BARE, K_HASH and K_MOD take the default Traits: no BITWISE_EQ.
	   K_BOTH does declare it, and registers Hash and Compare as well. */
	if (k == K_FAKE64 || k == K_SHORTFMT)
	    R_set_altsxp_Traits_method(test_classes[k], test_open_traits);
	else if (k != K_BARE && k != K_HASH && k != K_MOD)
	    R_set_altsxp_Traits_method(test_classes[k], test_traits);
    }
    R_set_altsxp_Compare_method(test_classes[K_CMP], test_compare);
    R_set_altsxp_Compare_method(test_classes[K_HASH], test_mod_compare);
    R_set_altsxp_Hash_method(test_classes[K_HASH], test_mod_hash);
    R_set_altsxp_Compare_method(test_classes[K_MOD], test_mod_compare);
    R_set_altsxp_Hash_method(test_classes[K_MOD], test_mod_hash);
    /* deliberately the modulo pair, which BITWISE_EQ contradicts: R must
       take the bit and never call these */
    R_set_altsxp_Compare_method(test_classes[K_BOTH], test_mod_compare);
    R_set_altsxp_Hash_method(test_classes[K_BOTH], test_mod_hash);
    R_set_altsxp_Compare_method(test_classes[K_FAKE64], test_fake64_compare);
    R_set_altsxp_Format_method(test_classes[K_SHORTFMT], test_short_format);
    R_set_altsxp_Coerce_from_method(test_classes[K_SHORTFMT],
				    test_short_coerce_from);
    R_set_altsxp_Compare_method(test_classes[K_SHORTFMT], test_compare);
    R_set_altsxp_Hash_method(test_classes[K_SHORTFMT], test_mod_hash);

    R_registerRoutines(dll, NULL, call_methods, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
