#include <R.h>
#include <Rinternals.h>
#include <R_ext/Rdynload.h>
#include <R_ext/Visibility.h>
#include <string.h>

static void check_bytes(const char *p, size_t n, int value)
{
    for (size_t i = 0; i < n; i++) {
	if ((unsigned char) p[i] != value)
	    error("buffer contents changed at byte %zu", i);
    }
}

static SEXP test_basic(void)
{
    const void *base = vmaxget();
    if (R_realloc(NULL, 0, 1) != NULL || vmaxget() != base)
	error("zero-size allocation changed the stack");

    char *p = R_realloc(NULL, 16 * sizeof(double), 1);
    memset(p, 42, 16 * sizeof(double));
    const void *mark = vmaxget();
    char *q = R_alloc(32, 1);
    memset(q, 17, 32);
    const void *top = vmaxget();
    R_gc();

    p = R_realloc(p, 10000, 1);
    R_gc();
    check_bytes(p, 16 * sizeof(double), 42);
    check_bytes(q, 32, 17);
    if (vmaxget() != top)
	error("growing an older buffer moved the stack");
    memset(p, 42, 10000);

    if (R_realloc(p, 10000, 1) != p)
	error("same-size reallocation moved the buffer");
    p = R_realloc(p, 8, 1);
    R_gc();
    check_bytes(p, 8, 42);
    check_bytes(q, 32, 17);

    /* A mark taken after the original allocation retains its replacement. */
    vmaxset(mark);
    R_gc();
    check_bytes(p, 8, 42);
    p = R_realloc(p, 64, 1);
    R_gc();
    check_bytes(p, 8, 42);
    if (vmaxget() != mark)
	error("growing the newest buffer moved its mark");

    /* A zero size keeps the block, its mark, and the ability to regrow. */
    p = R_realloc(p, 1, 0);
    if (p == NULL || R_realloc(p, 0, 1) != p)
	error("zero size did not shrink in place");
    R_alloc(10000, 1);
    vmaxset(mark);
    R_gc();
    if (vmaxget() != mark)
	error("shrinking a buffer invalidated its mark");
    p = R_realloc(p, 16, 1);
    memset(p, 42, 16);
    R_gc();
    check_bytes(p, 16, 42);
    if (vmaxget() != mark)
	error("regrowing a shrunk buffer moved its mark");

    /* S_realloc still preserves its old block and zeroes the extension. */
    p = S_alloc(16, 1);
    check_bytes(p, 16, 0);
    memset(p, 42, 16);
    q = S_realloc(p, 32, 16, 1);
    R_gc();
    check_bytes(p, 16, 42);
    check_bytes(q, 16, 42);
    check_bytes(q + 16, 16, 0);
    vmaxset(base);
    return R_NilValue;
}

typedef struct {
    char *p;
    char *other;
    size_t size;
    const void *mark;
    int fail;
    int cleaned;
} resize_data;

static void check_resize(resize_data *d)
{
    R_gc();
    if (d->p == NULL)
	error("resize returned NULL");
    check_bytes(d->p, d->size, 42);
    check_bytes(d->other, 32, 17);
    if (vmaxget() != d->mark)
	error("saved context mark changed");
}

static void resize(void *data)
{
    resize_data *d = data;
    d->p = R_realloc(d->p, d->size, 1);
    check_bytes(d->p, d->size < 16 ? d->size : 16, 42);
    memset(d->p, 42, d->size);
    check_resize(d);
    /* Error unwinding must discard this allocation, but retain d->p. */
    R_alloc(128, 1);
    if (d->fail)
	error("expected resize error");
    vmaxset(d->mark);
}

static SEXP resize_unwind(void *data)
{
    resize(data);
    return R_NilValue;
}

static void cleanup(void *data, Rboolean jump)
{
    resize_data *d = data;
    if (jump != d->fail)
	error("unexpected cleanup jump status");
    check_resize(d);
    /* Also verify stack membership after the context has been restored. */
    d->p = R_realloc(d->p, d->size, 1);
    d->cleaned++;
}

static void unwind(void *data)
{
    R_UnwindProtect(resize_unwind, data, cleanup, data, NULL);
}

static void test_context(size_t size, int older, int fail, int use_unwind)
{
    const void *base = vmaxget();
    resize_data d = {0};
    if (!older)
	d.other = R_alloc(32, 1);
    d.p = R_realloc(NULL, 16, 1);
    if (older)
	d.other = R_alloc(32, 1);
    memset(d.p, 42, 16);
    memset(d.other, 17, 32);
    d.size = size;
    d.mark = vmaxget();
    d.fail = fail;
    R_gc();

    Rboolean ok = R_ToplevelExec(use_unwind ? unwind : resize, &d);
    if (ok == fail || d.cleaned != use_unwind)
	error("unexpected context result");
    check_resize(&d);
    d.p = R_realloc(d.p, d.size, 1);
    vmaxset(base);
}

static SEXP test_contexts(void)
{
    const size_t sizes[] = {10000, 8, 0};
    for (int older = 0; older < 2; older++)
	for (int fail = 0; fail < 2; fail++)
	    for (int use_unwind = 0; use_unwind < 2; use_unwind++)
		for (int i = 0; i < 3; i++)
		    test_context(sizes[i], older, fail, use_unwind);
    return R_NilValue;
}

typedef struct {
    void *p;
    size_t count;
    int size;
} request_data;

static void request(void *data)
{
    request_data *d = data;
    R_realloc(d->p, d->count, d->size);
}

static SEXP test_errors(SEXP too_big)
{
    const void *base = vmaxget();
    char *p = R_realloc(NULL, 16, 1);
    memset(p, 42, 16);
    const void *mark = vmaxget();
    /* Foreign pointers: zeroed memory, a plain R vector, an R_alloc block. */
    char invalid[128] = {0};
    SEXP raw = PROTECT(allocVector(RAWSXP, 16));
    request_data requests[] = {
	{p, (size_t) -1, 2},
	{p, 1, -1},
	{NULL, 1, -1},
	{NULL, (size_t) -1, 2},
	{invalid + 64, 16, 1},
	{RAW(raw), 32, 1},
	{R_alloc(16, 1), 32, 1},
	{p, (size_t) asReal(too_big), 1}
    };
    const void *top = vmaxget();
    for (int i = 0; i < 8; i++) {
	if (R_ToplevelExec(request, &requests[i]))
	    error("invalid reallocation succeeded");
	R_gc();
	check_bytes(p, 16, 42);
	if (vmaxget() != top)
	    error("failed reallocation changed the stack");
    }
    vmaxset(mark);
    p = R_realloc(p, 32, 1);
    check_bytes(p, 16, 42);
    vmaxset(base);
    UNPROTECT(1);
    return R_NilValue;
}

/* Vcells in use after each step of a grow, shrink, regrow sequence. */
static SEXP test_reclaim(SEXP gc)
{
    SEXP call = PROTECT(lang1(gc));
    SEXP ans = PROTECT(allocVector(REALSXP, 5));
    const void *base = vmaxget();
    REAL(ans)[0] = asReal(eval(call, R_GlobalEnv));
    char *p = R_realloc(NULL, 8 * 1024 * 1024, 1);
    REAL(ans)[1] = asReal(eval(call, R_GlobalEnv));
    p = R_realloc(p, 16 * 1024 * 1024, 1);
    REAL(ans)[2] = asReal(eval(call, R_GlobalEnv));
    p = R_realloc(p, 0, 1);
    REAL(ans)[3] = asReal(eval(call, R_GlobalEnv));
    p = R_realloc(NULL, 8 * 1024 * 1024, 1);
    p = R_realloc(p, 16 * 1024 * 1024, 1);
    vmaxset(base);
    REAL(ans)[4] = asReal(eval(call, R_GlobalEnv));
    UNPROTECT(2);
    return ans;
}

static const R_CallMethodDef callMethods[] = {
    {"test_basic", (DL_FUNC) &test_basic, 0},
    {"test_contexts", (DL_FUNC) &test_contexts, 0},
    {"test_errors", (DL_FUNC) &test_errors, 1},
    {"test_reclaim", (DL_FUNC) &test_reclaim, 1},
    {NULL, NULL, 0}
};

void attribute_visible R_init_realloc(DllInfo *dll)
{
    R_registerRoutines(dll, NULL, callMethods, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
