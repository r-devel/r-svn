/* Two raw ALTREP views on a single buffer for testing Rf_copyVector. */
#include <Rinternals.h>
#include <R_ext/Rdynload.h>
#include <R_ext/Altrep.h>
#include <R_ext/Visibility.h>

static R_altrep_class_t raw_view;

static R_xlen_t view_length(SEXP x)
{
    return INTEGER(R_altrep_data2(x))[1];
}

static void *view_dataptr(SEXP x, Rboolean writeable)
{
    return RAW(R_altrep_data1(x)) + INTEGER(R_altrep_data2(x))[0];
}

static SEXP make_view(SEXP buf, SEXP range)
{
    if (TYPEOF(range) != INTSXP || XLENGTH(range) != 2)
	Rf_error("expected an integer offset and length");
    int off = INTEGER(range)[0], len = INTEGER(range)[1];
    if (off < 0 || len < 0 || off > XLENGTH(buf) - len)
	Rf_error("view outside buffer");
    return R_new_altrep(raw_view, buf, range);
}

static SEXP copy_views(SEXP x, SEXP from, SEXP to)
{
    if (TYPEOF(x) != RAWSXP)
	Rf_error("expected a raw buffer");
    SEXP buf = PROTECT(Rf_duplicate(x));
    SEXP src = PROTECT(make_view(buf, from));
    SEXP dst = PROTECT(make_view(buf, to));
    Rf_copyVector(dst, src);
    UNPROTECT(3);
    return buf;
}

void attribute_visible R_init_copyVector(DllInfo *dll)
{
    static const R_CallMethodDef calls[] = {
	{"copy_views", (DL_FUNC) &copy_views, 3},
	{NULL, NULL, 0}
    };
    R_registerRoutines(dll, NULL, calls, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
    R_forceSymbols(dll, TRUE);
    raw_view = R_make_altraw_class("raw_view", "copyVector", dll);
    R_set_altrep_Length_method(raw_view, view_length);
    R_set_altvec_Dataptr_method(raw_view, view_dataptr);
}
