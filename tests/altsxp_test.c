/* Test-only ALTSXP classes for tests/altsxp.R.

   The classes deliberately have no data pointer and report short positive
   region counts.  This exercises the generic consumer API independently of
   the base int64 class, whose region methods always complete a request. */

#include <R.h>
#include <Rinternals.h>
#include <R_ext/Altrep.h>
#include <R_ext/Memory.h>
#include <R_ext/Rdynload.h>
#include <R_ext/Visibility.h>

#include <string.h>

enum { META_GET_CALLS, META_SET_CALLS, META_N };
enum { GET_CHUNK = 3, SET_CHUNK = 2, WIDE_ELT_SIZE = 4096 };

static R_altrep_class_t byte_class;
static R_altrep_class_t wide_class;
static SEXP byte_type;
static SEXP wide_type;

static int test_is_wide(SEXP x)
{
    if (ALTREP(x))
	return R_altrep_inherits(x, wide_class);
    return x == R_SEXP(wide_class); /* class object passed to New() */
}

static R_altrep_class_t test_class(SEXP proto)
{
    return test_is_wide(proto) ? wide_class : byte_class;
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
    return test_is_wide(x) ? wide_type : byte_type;
}

static size_t test_elt_size(SEXP x)
{
    return test_is_wide(x) ? WIDE_ELT_SIZE : 1;
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

static void init_test_class(R_altrep_class_t cls)
{
    R_set_altrep_Length_method(cls, test_length);
    R_set_altsxp_Elt_type_method(cls, test_elt_type);
    R_set_altsxp_Elt_size_method(cls, test_elt_size);
    R_set_altsxp_New_method(cls, test_new);
    R_set_altsxp_Get_region_method(cls, test_get_region);
    R_set_altsxp_Set_region_method(cls, test_set_region);
    R_set_altsxp_Is_na_region_method(cls, test_is_na_region);
    R_set_altsxp_Traits_method(cls, test_traits);
}

static SEXP test_constructor(SEXP data, SEXP wide)
{
    if (TYPEOF(data) != RAWSXP)
	error("test ALTSXP data must be raw");
    int is_wide = asLogical(wide);
    if (is_wide == NA_LOGICAL)
	error("invalid wide flag");

    SEXP copy = PROTECT(duplicate(data));
    SEXP ans = test_make(is_wide ? wide_class : byte_class, copy);
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

static const R_CallMethodDef call_methods[] = {
    {"C_altsxp_test_constructor", (DL_FUNC) &test_constructor, 2},
    {"C_altsxp_test_contents", (DL_FUNC) &test_contents, 1},
    {"C_altsxp_test_counts", (DL_FUNC) &test_counts, 1},
    {"C_altsxp_test_copy", (DL_FUNC) &test_copy, 4},
    {"C_altsxp_test_as_list_vmax", (DL_FUNC) &test_as_list_vmax, 1},
    {NULL, NULL, 0}
};

void attribute_visible R_init_altsxp_test(DllInfo *dll)
{
    byte_type = install("altsxp_test_byte");
    wide_type = install("altsxp_test_wide");
    byte_class = R_make_altsxp_class("short_byte", "altsxpTest", dll);
    wide_class = R_make_altsxp_class("wide_byte", "altsxpTest", dll);
    init_test_class(byte_class);
    init_test_class(wide_class);

    R_registerRoutines(dll, NULL, call_methods, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
