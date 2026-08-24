/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1997--2025  The R Core Team
 *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, a copy is available at
 *  https://www.R-project.org/Licenses/
 */


#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <Defn.h>
#include <Internal.h>
#include <Rmath.h>
#include <errno.h>
#include <R_ext/Itermacros.h>

/* interval at which to check interrupts, a guess */
#define NINTERRUPT 10000000

static SEXP numeric_relop(RELOP_TYPE code, SEXP s1, SEXP s2);
static SEXP complex_relop(RELOP_TYPE code, SEXP s1, SEXP s2, SEXP call);
static SEXP string_relop (RELOP_TYPE code, SEXP s1, SEXP s2);
static SEXP raw_relop    (RELOP_TYPE code, SEXP s1, SEXP s2);
static SEXP xint_relop  (RELOP_TYPE code, SEXP s1, SEXP s2, SEXP call);
static SEXP xint_numeric_relop(RELOP_TYPE code, SEXP s1, SEXP s2,
				SEXP call);

#define DO_SCALAR_RELOP(oper, x, y) do {		\
	switch (oper) {					\
	case EQOP: return ScalarLogical((x) == (y));	\
	case NEOP: return ScalarLogical((x) != (y));	\
	case LTOP: return ScalarLogical((x) < (y));	\
	case GTOP: return ScalarLogical((x) > (y));	\
	case LEOP: return ScalarLogical((x) <= (y));	\
	case GEOP: return ScalarLogical((x) >= (y));	\
	}						\
    } while (0)

attribute_hidden SEXP do_relop(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP ans, arg1, arg2;
    int argc;

    if (args != R_NilValue &&
	CDR(args) != R_NilValue &&
	CDDR(args) == R_NilValue)
	argc = 2;
    else
	argc = length(args);
    arg1 = CAR(args);
    arg2 = CADR(args);

    if (ATTRIB(arg1) != R_NilValue || ATTRIB(arg2) != R_NilValue) {
	if (DispatchGroup("Ops", call, op, args, env, &ans))
	    return ans;
    }

    if (argc != 2)
	error("operator needs two arguments");

    return do_relop_dflt(call, op, arg1, arg2);
}

#define IS_SCALAR_STRING(x) (TYPEOF(x) == STRSXP && XLENGTH(x) == 1)
#define SYMBOL_STRING_MATCH(x, y) \
    (isSymbol(x) && IS_SCALAR_STRING(y) && Seql(PRINTNAME(x), STRING_ELT(y, 0)))


static R_INLINE bool compute_lang_equal(SEXP x, SEXP y)
{
    if (isSymbol(x))
	return y == x ||
	    (IS_SCALAR_STRING(y) && Seql(PRINTNAME(x), STRING_ELT(y, 0)));
    else if (isSymbol(y))
	return x == y ||
	    (IS_SCALAR_STRING(x) && Seql(STRING_ELT(x, 0), PRINTNAME(y)));

    if (TYPEOF(x) == LANGSXP && ATTRIB(x) != R_NilValue)
	x = LCONS(CAR(x), CDR(x));
    PROTECT(x);
    if (TYPEOF(y) == LANGSXP && ATTRIB(y) != R_NilValue)
	y = LCONS(CAR(y), CDR(y));
    PROTECT(y);

    bool val = R_compute_identical(x, y, 16);
    UNPROTECT(2);
    return val;
}

static SEXP compute_language_relop(SEXP call, SEXP op, SEXP x, SEXP y)
{
    static enum {
	UNINITIALIZED,
	EQONLY,
	IDENTICAL_CALLS,
	IDENTICAL_CALLS_ATTR,
	IDENTICAL,
	ERROR_CALLS,
	ERROR
    } option = UNINITIALIZED;

    if (option == UNINITIALIZED) {
	option = EQONLY;
	const char *val = getenv("_R_COMPARE_LANG_OBJECTS");
	if (val != NULL) {
	    if (strcmp(val, "eqonly") == 0)
		option = EQONLY;
	    else if (strcmp(val, "identical_calls") == 0)
		option = IDENTICAL_CALLS;
	    else if (strcmp(val, "identical_calls_attr") == 0)
		option = IDENTICAL_CALLS_ATTR;
	    else if (strcmp(val, "identical") == 0)
		option = IDENTICAL;
	    else if (strcmp(val, "error_calls") == 0)
		option = ERROR_CALLS;
	    else if (strcmp(val, "error") == 0)
		option = ERROR;
	}
    }

    switch(option) {
    case EQONLY:
	switch(PRIMVAL(op)) {
	case EQOP: return NULL;
	case NEOP: return NULL;
	default:
	    errorcall(call,
		      _("comparison (%s) is not possible for language types"),
		      PRIMNAME(op));
	}
    case IDENTICAL_CALLS:
	/* this should reproduce the current behavior of == and != for
	   language objects, while signaling errors for <, <=, >, and
	   >=. */
	switch(PRIMVAL(op)) {
	case EQOP:
	    return compute_lang_equal(x, y) ? R_TrueValue : R_FalseValue;
	case NEOP:
	    return compute_lang_equal(x, y) ? R_FalseValue : R_TrueValue;
	default:
	    errorcall(call,
		      _("comparison (%s) is not possible for language types"),
		      PRIMNAME(op));
	}
    case IDENTICAL_CALLS_ATTR:
	if (isSymbol(x) && IS_SCALAR_STRING(y))
	    y = Seql(STRING_ELT(y, 0), PRINTNAME(x)) ? x : R_NilValue;
	else if (isSymbol(y) && IS_SCALAR_STRING(x))
	    x = Seql(STRING_ELT(x, 0), PRINTNAME(y)) ? y : R_NilValue;
	switch(PRIMVAL(op)) {
	case EQOP:
	    return R_compute_identical(x, y, 16) ? R_TrueValue : R_FalseValue;
	case NEOP:
	    return R_compute_identical(x, y, 16) ? R_FalseValue : R_TrueValue;
	default:
	    errorcall(call,
		      _("comparison (%s) is not possible for language types"),
		      PRIMNAME(op));
	}
    case IDENTICAL:
	if (SYMBOL_STRING_MATCH(x, y) || SYMBOL_STRING_MATCH(y, x))
	    /* identical(x, y) and the default x == y implementation
	       would disagree, so signal an error instead */
	    errorcall(call,
		      _("comparing this symbol and string pair "
			"is not supported"));
	switch(PRIMVAL(op)) {
	case EQOP:
	    return R_compute_identical(x, y, 16) ? R_TrueValue : R_FalseValue;
	case NEOP:
	    return R_compute_identical(x, y, 16) ? R_FalseValue : R_TrueValue;
	default: errorcall(call,
			   _("comparison (%s) is not possible for language types"),
			   PRIMNAME(op));
	}
    case ERROR_CALLS:
	if (TYPEOF(x) == LANGSXP || TYPEOF(y) == LANGSXP)
	    errorcall(call, _("comparison of call objects is not supported"));
	return NULL;
    case ERROR:
	errorcall(call, _("comparison of language objects is not supported"));
    default: return NULL;
    }
}

// also called from cmp_relop() in eval.c :
attribute_hidden SEXP do_relop_dflt(SEXP call, SEXP op, SEXP x, SEXP y)
{
    /* handle the REALSXP/INTSXP simple scalar case quickly */
    if (IS_SIMPLE_SCALAR(x, INTSXP)) {
	int ix = SCALAR_IVAL(x);
	if (IS_SIMPLE_SCALAR(y, INTSXP)) {
	    int iy = SCALAR_IVAL(y);
	    if (ix == NA_INTEGER || iy == NA_INTEGER)
		return ScalarLogical(NA_LOGICAL);
	    DO_SCALAR_RELOP(PRIMVAL(op), ix, iy);
	}
	else if (IS_SIMPLE_SCALAR(y, REALSXP)) {
	    double dy = SCALAR_DVAL(y);
	    if (ix == NA_INTEGER || ISNAN(dy))
		return ScalarLogical(NA_LOGICAL);
	    DO_SCALAR_RELOP(PRIMVAL(op), ix, dy);
	}
    }
    else if (IS_SIMPLE_SCALAR(x, REALSXP)) {
	double dx = SCALAR_DVAL(x);
	if (IS_SIMPLE_SCALAR(y, INTSXP)) {
	    int iy = SCALAR_IVAL(y);
	    if (ISNAN(dx) || iy == NA_INTEGER)
		return ScalarLogical(NA_LOGICAL);
	    DO_SCALAR_RELOP(PRIMVAL(op), dx, iy);
	}
	else if (IS_SIMPLE_SCALAR(y, REALSXP)) {
	    double dy = SCALAR_DVAL(y);
	    if (ISNAN(dx) || ISNAN(dy))
		return ScalarLogical(NA_LOGICAL);
	    DO_SCALAR_RELOP(PRIMVAL(op), dx, dy);
	}
    }

    R_xlen_t
	nx = xlength(x),
	ny = xlength(y);
    SEXPTYPE
	typex = TYPEOF(x),
	typey = TYPEOF(y);

    /* handle the REALSXP/INTSXP simple vector/scalar case quickly. */
    if (ATTRIB(x) == R_NilValue && ATTRIB(y) == R_NilValue &&
	(typex == REALSXP || typex == INTSXP) &&
	(typey == REALSXP || typey == INTSXP) &&
	nx > 0 && ny > 0 && (nx == 1 || ny == 1)) {

	PROTECT(x);
	PROTECT(y);
	SEXP ans;
	ans = numeric_relop(PRIMVAL(op), x, y);
	UNPROTECT(2);
	return ans;
    }

    /* handle the general case */
    PROTECT_INDEX xpi, ypi;
    PROTECT_WITH_INDEX(x, &xpi);
    PROTECT_WITH_INDEX(y, &ypi);

    if (isSymbol(x) || TYPEOF(x) == LANGSXP ||
	isSymbol(y) || TYPEOF(y) == LANGSXP) {
	SEXP ans = compute_language_relop(call, op, x, y);
	if (ans != NULL) {
	    UNPROTECT(2);
	    return ans;
	}
    }

    bool iS;
    /* That symbols and calls were allowed was undocumented prior to
       R 2.5.0.  We deparse them as deparse() would, minus attributes */
    if ((iS = isSymbol(x)) || TYPEOF(x) == LANGSXP) {
	SEXP tmp = allocVector(STRSXP, 1);
	PROTECT(tmp);
	SET_STRING_ELT(tmp, 0, (iS) ? PRINTNAME(x) :
		       STRING_ELT(deparse1line_ex(x, false,
						  DEFAULTDEPARSE | DIGITS17),
				  0));
	REPROTECT(x = tmp, xpi);
	nx = xlength(x);
	UNPROTECT(1);
    }
    if ((iS = isSymbol(y)) || TYPEOF(y) == LANGSXP) {
	SEXP tmp = allocVector(STRSXP, 1);
	PROTECT(tmp);
	SET_STRING_ELT(tmp, 0, (iS) ? PRINTNAME(y) :
		       STRING_ELT(deparse1line_ex(y, false,
						  DEFAULTDEPARSE | DIGITS17),
				  0));
	REPROTECT(y = tmp, ypi);
	ny = xlength(y);
	UNPROTECT(1);
    }

    if (isNull(x)) REPROTECT(x = allocVector(INTSXP,0), xpi);
    if (isNull(y)) REPROTECT(y = allocVector(INTSXP,0), ypi);
    if (!isVector(x) || !isVector(y))
	errorcall(call,
		  _("comparison (%s) is possible only for atomic and list types"),
		  PRIMNAME(op));

#ifdef previous_R_versions
    if (TYPEOF(x) == EXPRSXP || TYPEOF(y) == EXPRSXP)
	errorcall(call, _("comparison is not allowed for expressions"));
#endif

    /* ELSE :  x and y are both atomic or list */

    bool
	xarray = isArray(x),
	yarray = isArray(y),
	xts = isTs(x),
	yts = isTs(y);
    SEXP dims, xnames, ynames;
    if (xarray || yarray) {
	if (xarray && yarray) {
	    if (!conformable(x, y))
		errorcall(call, _("non-conformable arrays"));
	    PROTECT(dims = getAttrib(x, R_DimSymbol));
	}
	else if (xarray && (ny != 0 || nx == 0)) {
	    PROTECT(dims = getAttrib(x, R_DimSymbol));
	}
	else if (yarray && (nx != 0 || ny == 0)) {
	    PROTECT(dims = getAttrib(y, R_DimSymbol));
	} else
	    PROTECT(dims = R_NilValue);

	PROTECT(xnames = getAttrib(x, R_DimNamesSymbol));
	PROTECT(ynames = getAttrib(y, R_DimNamesSymbol));
    }
    else {
	PROTECT(dims = R_NilValue);
	PROTECT(xnames = getAttrib(x, R_NamesSymbol));
	PROTECT(ynames = getAttrib(y, R_NamesSymbol));
    }

    SEXP klass = NULL, tsp = NULL; // -Wall
    if (xts || yts) {
	if (xts && yts) {
	    /* could check ts conformance here */
	    PROTECT(tsp = getAttrib(x, R_TspSymbol));
	    PROTECT(klass = getAttrib(x, R_ClassSymbol));
	}
	else if (xts) {
	    if (xlength(x) < xlength(y))
		ErrorMessage(call, ERROR_TSVEC_MISMATCH);
	    PROTECT(tsp = getAttrib(x, R_TspSymbol));
	    PROTECT(klass = getAttrib(x, R_ClassSymbol));
	}
	else /*(yts)*/ {
	    if (xlength(y) < xlength(x))
		ErrorMessage(call, ERROR_TSVEC_MISMATCH);
	    PROTECT(tsp = getAttrib(y, R_TspSymbol));
	    PROTECT(klass = getAttrib(y, R_ClassSymbol));
	}
    }

  SEXP altans = NULL;
  if (TYPEOF(x) == ALTSXP || TYPEOF(y) == ALTSXP)
      altans = ALT_COMPARE(x, y, PRIMVAL(op), call);

  if (nx > 0 && ny > 0) {
	if(((nx > ny) ? nx % ny : ny % nx) != 0) // mismatch
            warningcall(call, _(
		"longer object length is not a multiple of shorter object length"));

    /* A character operand wins here as it does for every other type,
       and before the 'xinteger' arms: as.character() of an element is
       exact and reversible at every width, so this is the one promotion
       an 'xinteger' operand can take without losing anything.  It is
       also what c() and x[i] <- value already do, and having ==,
       match() and %in% disagree with them about the same pair of
       operands is worse than inheriting string collation. */
    if (altans != NULL) {
	x = altans;
    }
    else if (isString(x) || isString(y)) {
	REPROTECT(x = coerceVector(x, STRSXP), xpi);
	REPROTECT(y = coerceVector(y, STRSXP), ypi);
	x = string_relop((RELOP_TYPE) PRIMVAL(op), x, y);
    }
    else if (TYPEOF(x) == ALTSXP || TYPEOF(y) == ALTSXP)
	errorcall(call, _("comparison of these ALTSXP classes is not implemented"));
    else if (isComplex(x) || isComplex(y)) {
	REPROTECT(x = coerceVector(x, CPLXSXP), xpi);
	REPROTECT(y = coerceVector(y, CPLXSXP), ypi);
	x = complex_relop((RELOP_TYPE) PRIMVAL(op), x, y, call);
    }
    else if ((isNumeric(x) || isLogical(x)) && (isNumeric(y) || isLogical(y))) {
        x = numeric_relop((RELOP_TYPE) PRIMVAL(op), x, y);
    } // rest of cases only apply when 'x' or 'y' is raw
    else if (isReal(x) || isReal(y)) {
	REPROTECT(x = coerceVector(x, REALSXP), xpi);
	REPROTECT(y = coerceVector(y, REALSXP), ypi);
	x = numeric_relop((RELOP_TYPE) PRIMVAL(op), x, y);
    }
    else if (isInteger(x) || isInteger(y)) {
	REPROTECT(x = coerceVector(x, INTSXP), xpi);
	REPROTECT(y = coerceVector(y, INTSXP), ypi);
	x = numeric_relop((RELOP_TYPE) PRIMVAL(op), x, y);
    }
    else if (isLogical(x) || isLogical(y)) {
	REPROTECT(x = coerceVector(x, LGLSXP), xpi);
	REPROTECT(y = coerceVector(y, LGLSXP), ypi);
	x = numeric_relop((RELOP_TYPE) PRIMVAL(op), x, y);
    }
    else if (TYPEOF(x) == RAWSXP || TYPEOF(y) == RAWSXP) {
	REPROTECT(x = coerceVector(x, RAWSXP), xpi);
	REPROTECT(y = coerceVector(y, RAWSXP), ypi);
	x = raw_relop((RELOP_TYPE) PRIMVAL(op), x, y);
    } else errorcall(call, _("comparison of these types is not implemented"));
  } else if (altans != NULL) { // nx == 0 || ny == 0
	x = altans;
  } else {
	if (R_isXInt(x) && R_isXInt(y))
	    /* an empty operand still has a type, and answering
	       logical(0) for a pair that c(), min() and union() refuse
	       is the silent divergence these checks exist to prevent */
	    R_xintCheckPair(call, x, y, "compare");
	else if (R_isXInt(x) || R_isXInt(y)) {
	    /* the same rule at length zero as at any other: a pairing
	       the narrowing refuses is refused here too, without paying
	       for the narrowing itself */
	    SEXP o = R_isXInt(x) ? y : x;
	    if (TYPEOF(o) == STRSXP)
		;		/* both go to character; see above */
	    else if (TYPEOF(o) == REALSXP || TYPEOF(o) == CPLXSXP) {
		if (TYPEOF(o) == CPLXSXP && PRIMVAL(op) != EQOP && PRIMVAL(op) != NEOP)
		    errorcall(call, _("invalid comparison with complex values"));
	    }
	    else R_xintCheckOperand(o, call);
	}
	else if (TYPEOF(x) == ALTSXP || TYPEOF(y) == ALTSXP)
	    errorcall(call, _("comparison of these ALTSXP classes is not implemented"));
	x = allocVector(LGLSXP, 0);
  }

    PROTECT(x);
    if (dims != R_NilValue) {
	setAttrib(x, R_DimSymbol, dims);
	if (xnames != R_NilValue)
	    setAttrib(x, R_DimNamesSymbol, xnames);
	else if (ynames != R_NilValue)
	    setAttrib(x, R_DimNamesSymbol, ynames);
    }
    else {
	if (xnames != R_NilValue && xlength(x) == xlength(xnames))
	    setAttrib(x, R_NamesSymbol, xnames);
	else if (ynames != R_NilValue && xlength(x) == xlength(ynames))
	    setAttrib(x, R_NamesSymbol, ynames);
    }
    if (xts || yts) {
	setAttrib(x, R_TspSymbol, tsp);
	setAttrib(x, R_ClassSymbol, klass);
	UNPROTECT(2);
    }

    UNPROTECT(6);
    return x;
}

#define ISNA_INT(x) x == NA_INTEGER

#define NR_HELPER(OP, type1, ACCESSOR1, ISNA1, type2, ACCESSOR2, ISNA2) do { \
	type1 x1, *px1 = ACCESSOR1(s1);					\
	type2 x2, *px2 = ACCESSOR2(s2);					\
	int *pa = LOGICAL(ans);						\
        MOD_ITERATE2(n, n1, n2, i, i1, i2, {                            \
	    x1 = px1[i1];						\
	    x2 = px2[i2];						\
            if (ISNA1(x1) || ISNA2(x2))                                 \
                pa[i] = NA_LOGICAL;					\
            else                                                        \
                pa[i] = (x1 OP x2);					\
        });                                                             \
    } while (0)

#define NUMERIC_RELOP(type1, ACCESSOR1, ISNA1, type2, ACCESSOR2, ISNA2) do { \
    switch (code) {                                                     \
    case EQOP:                                                          \
	NR_HELPER(==, type1, ACCESSOR1, ISNA1, type2, ACCESSOR2, ISNA2); \
        break;                                                          \
    case NEOP:                                                          \
	NR_HELPER(!=, type1, ACCESSOR1, ISNA1, type2, ACCESSOR2, ISNA2); \
        break;                                                          \
    case LTOP:                                                          \
	NR_HELPER(<, type1, ACCESSOR1, ISNA1, type2, ACCESSOR2, ISNA2); \
        break;                                                          \
    case GTOP:                                                          \
	NR_HELPER(>, type1, ACCESSOR1, ISNA1, type2, ACCESSOR2, ISNA2); \
        break;                                                          \
    case LEOP:                                                          \
	NR_HELPER(<=, type1, ACCESSOR1, ISNA1, type2, ACCESSOR2, ISNA2); \
        break;                                                          \
    case GEOP:                                                          \
	NR_HELPER(>=, type1, ACCESSOR1, ISNA1, type2, ACCESSOR2, ISNA2); \
        break;                                                          \
    }                                                                   \
} while(0)

static int relopFromCmp(RELOP_TYPE code, int cmp)
{
    switch (code) {
    case EQOP: return cmp == 0;
    case NEOP: return cmp != 0;
    case LTOP: return cmp < 0;
    case GTOP: return cmp > 0;
    case LEOP: return cmp <= 0;
    case GEOP: return cmp >= 0;
    }
    return FALSE;
}

/* Compare in the exact integer/double domain.  Converting the integer
   first would make 2^53+1 equal to 2^53, so even a precision warning
   could not rescue the resulting logical answer. */
static SEXP xint_numeric_relop(RELOP_TYPE code, SEXP s1, SEXP s2,
				SEXP call)
{
    bool left = TYPEOF(s1) == ALTSXP;
    SEXP b = left ? s1 : s2, o = left ? s2 : s1;
    int w = XINT_WIDTH(b), kind = XINT_KIND(b);
    bool hasNA = XINT_HAS_NA(b);
    bool complex = TYPEOF(o) == CPLXSXP;
    if (complex && code != EQOP && code != NEOP)
	errorcall(call, _("invalid comparison with complex values"));

    R_xlen_t nb = XLENGTH(b), no = XLENGTH(o), n = nb > no ? nb : no;
    SEXP ans = PROTECT(allocVector(LGLSXP, n));
    for (R_xlen_t i = 0; i < n; i++) {
	double value, imaginary = 0.0;
	if (complex) {
	    Rcomplex z = COMPLEX_ELT(o, i % no);
	    value = z.r; imaginary = z.i;
	}
	else value = REAL_ELT(o, i % no);

	bool isNA;
	int cmp = R_xintEltCompareReal(XINT_ELT_RO(b, i % nb), w, kind,
					       hasNA, value, &isNA);
	if (complex && ISNAN(imaginary)) isNA = true;
	if (isNA) LOGICAL(ans)[i] = NA_LOGICAL;
	else if (complex && imaginary != 0.0)
	    LOGICAL(ans)[i] = code == NEOP;
	else {
	    if (!left) cmp = -cmp;
	    LOGICAL(ans)[i] = relopFromCmp(code, cmp);
	}
    }
    UNPROTECT(1);
    return ans;
}

static SEXP numeric_relop(RELOP_TYPE code, SEXP s1, SEXP s2)
{
    R_xlen_t i, i1, i2, n, n1, n2;
    SEXP ans;

    n1 = XLENGTH(s1);
    n2 = XLENGTH(s2);
    n = (n1 > n2) ? n1 : n2;
    PROTECT(s1);
    PROTECT(s2);
    ans = allocVector(LGLSXP, n);

    if (isInteger(s1) || isLogical(s1)) {
        if (isInteger(s2) || isLogical(s2)) {
            NUMERIC_RELOP(int, INTEGER, ISNA_INT, int, INTEGER, ISNA_INT);
        } else {
            NUMERIC_RELOP(int, INTEGER, ISNA_INT, double, REAL, ISNAN);
        }
    } else if (isInteger(s2) || isLogical(s2)) {
        NUMERIC_RELOP(double, REAL, ISNAN, int, INTEGER, ISNA_INT);
    } else {
        NUMERIC_RELOP(double, REAL, ISNAN, double, REAL, ISNAN);
    }

    UNPROTECT(2);
    return ans;
}

static SEXP complex_relop(RELOP_TYPE code, SEXP s1, SEXP s2, SEXP call)
{
    R_xlen_t i, i1, i2, n, n1, n2;
    Rcomplex x1, x2;
    SEXP ans;

    if (code != EQOP && code != NEOP) {
	errorcall(call, _("invalid comparison with complex values"));
    }

    n1 = XLENGTH(s1);
    n2 = XLENGTH(s2);
    n = (n1 > n2) ? n1 : n2;
    PROTECT(s1);
    PROTECT(s2);
    ans = allocVector(LGLSXP, n);

    const Rcomplex *px1 = COMPLEX_RO(s1);
    const Rcomplex *px2 = COMPLEX_RO(s2);
    int *pa = LOGICAL(ans);

    switch (code) {
    case EQOP:
	MOD_ITERATE2(n, n1, n2, i, i1, i2, {
//	    if ((i+1) % NINTERRUPT == 0) R_CheckUserInterrupt();
	    x1 = px1[i1];
	    x2 = px2[i2];
	    if (ISNAN(x1.r) || ISNAN(x1.i) ||
		ISNAN(x2.r) || ISNAN(x2.i))
		pa[i] = NA_LOGICAL;
	    else
		pa[i] = (x1.r == x2.r && x1.i == x2.i);
	});
	break;
    case NEOP:
	MOD_ITERATE2(n, n1, n2, i, i1, i2, {
//	    if ((i+1) % NINTERRUPT == 0) R_CheckUserInterrupt();
	    x1 = px1[i1];
	    x2 = px2[i2];
	    if (ISNAN(x1.r) || ISNAN(x1.i) ||
		ISNAN(x2.r) || ISNAN(x2.i))
		pa[i] = NA_LOGICAL;
	    else
		pa[i] = (x1.r != x2.r || x1.i != x2.i);
	});
	break;
    default:
	/* never happens (-Wall) */
	break;
    }
    UNPROTECT(2);
    return ans;
}


/* POSIX allows EINVAL when one of the strings contains characters
   outside the collation domain. */
static SEXP string_relop(RELOP_TYPE code, SEXP s1, SEXP s2)
{
    R_xlen_t i, n, n1, n2, res, i1, i2;
    SEXP ans, c1, c2;
    const void *vmax = vmaxget(); // for Scollate

    n1 = XLENGTH(s1);
    n2 = XLENGTH(s2);
    n = (n1 > n2) ? n1 : n2;
    PROTECT(s1);
    PROTECT(s2);
    PROTECT(ans = allocVector(LGLSXP, n));
    int *pa = LOGICAL(ans);

    switch (code) {
    case EQOP:
	MOD_ITERATE2(n, n1, n2, i, i1, i2, {
//	    if ((i+1) % NINTERRUPT == 0) R_CheckUserInterrupt();
	    c1 = STRING_ELT(s1, i1);
	    c2 = STRING_ELT(s2, i2);
	    if (c1 == NA_STRING || c2 == NA_STRING)
		pa[i] = NA_LOGICAL;
	    else
		pa[i] = Seql(c1, c2) ? 1 : 0;
	});
	break;
    case NEOP:
	MOD_ITERATE2(n, n1, n2, i, i1, i2, {
//	    if ((i+1) % NINTERRUPT == 0) R_CheckUserInterrupt();
	    c1 = STRING_ELT(s1, i1);
	    c2 = STRING_ELT(s2, i2);
	    if (c1 == NA_STRING || c2 == NA_STRING)
		pa[i] = NA_LOGICAL;
	    else
		pa[i] = Seql(c1, c2) ? 0 : 1;
	});
	break;
    case LTOP:
	MOD_ITERATE2(n, n1, n2, i, i1, i2, {
//	    if ((i+1) % NINTERRUPT == 0) R_CheckUserInterrupt();
	    c1 = STRING_ELT(s1, i1);
	    c2 = STRING_ELT(s2, i2);
	    if (c1 == NA_STRING || c2 == NA_STRING)
		pa[i] = NA_LOGICAL;
	    else if (c1 == c2)
		pa[i] = 0;
	    else {
		errno = 0;
		res = Scollate(c1, c2);
		if(errno)
		    pa[i] = NA_LOGICAL;
		else
		    pa[i] = (res < 0) ? 1 : 0;
	    }
	});
	break;
    case GTOP:
	MOD_ITERATE2(n, n1, n2, i ,i1, i2, {
//	    if ((i+1) % NINTERRUPT == 0) R_CheckUserInterrupt();
	    c1 = STRING_ELT(s1, i1);
	    c2 = STRING_ELT(s2, i2);
	    if (c1 == NA_STRING || c2 == NA_STRING)
		pa[i] = NA_LOGICAL;
	    else if (c1 == c2)
		pa[i] = 0;
	    else {
		errno = 0;
		res = Scollate(c1, c2);
		if(errno)
		    pa[i] = NA_LOGICAL;
		else
		    pa[i] = (res > 0) ? 1 : 0;
	    }
	});
	break;
    case LEOP:
	MOD_ITERATE2(n, n1, n2, i, i1, i2, {
//	    if ((i+1) % NINTERRUPT == 0) R_CheckUserInterrupt();
	    c1 = STRING_ELT(s1, i1);
	    c2 = STRING_ELT(s2, i2);
	    if (c1 == NA_STRING || c2 == NA_STRING)
		pa[i] = NA_LOGICAL;
	    else if (c1 == c2)
		pa[i] = 1;
	    else {
		errno = 0;
		res = Scollate(c1, c2);
		if(errno)
		    pa[i] = NA_LOGICAL;
		else
		    pa[i] = (res <= 0) ? 1 : 0;
	    }
	});
	break;
    case GEOP:
	MOD_ITERATE2(n, n1, n2, i, i1, i2, {
//	    if ((i+1) % NINTERRUPT == 0) R_CheckUserInterrupt();
	    c1 = STRING_ELT(s1, i1);
	    c2 = STRING_ELT(s2, i2);
	    if (c1 == NA_STRING || c2 == NA_STRING)
		pa[i] = NA_LOGICAL;
	    else if (c1 == c2)
		pa[i] = 1;
	    else {
		errno = 0;
		res = Scollate(c1, c2);
		if(errno)
		    pa[i] = NA_LOGICAL;
		else
		    pa[i] = (res >= 0) ? 1 : 0;
	    }
	});
	break;
    }
    UNPROTECT(3);
    vmaxset(vmax);
    return ans;
}

/* R_xintEltCmp() compares numeric values: most-significant byte first,
   interpreting the top byte as signed for the signed kind.  This is the
   same order bcmp_() in sort.c uses, and is portable across byte orders
   even though the in-memory payload is native-endian. */
static SEXP xint_relop(RELOP_TYPE code, SEXP s1, SEXP s2, SEXP call)
{
    R_xlen_t i, i1, i2, n, n1, n2;
    SEXP ans;

    SEXP b = (TYPEOF(s1) == ALTSXP) ? s1 : s2;
    int w = XINT_WIDTH(b), k = XINT_KIND(b);
    bool hasNA = XINT_HAS_NA(b);

    /* which side narrowed, and where its operands fell outside the
       type; see R_xintNarrowCmp() */
    int *dir = NULL, side = 0;
    const void *vmax = vmaxget();

    PROTECT_INDEX p1, p2;
    PROTECT_WITH_INDEX(s1, &p1);
    PROTECT_WITH_INDEX(s2, &p2);
    if (TYPEOF(s1) == ALTSXP && TYPEOF(s2) == ALTSXP)
	R_xintCheckPair(call, s1, s2, "compare");
    else if (TYPEOF(s1) == ALTSXP) {
	dir = (int *) R_alloc(XLENGTH(s2) + 1, sizeof(int));
	REPROTECT(s2 = R_xintNarrowCmp(s2, w, k, XINT_HAS_NA(s1), dir, call), p2);
	side = 2;
    }
    else {
	dir = (int *) R_alloc(XLENGTH(s1) + 1, sizeof(int));
	REPROTECT(s1 = R_xintNarrowCmp(s1, w, k, XINT_HAS_NA(s2), dir, call), p1);
	side = 1;
    }

    n1 = XLENGTH(s1);
    n2 = XLENGTH(s2);
    n = (n1 > n2) ? n1 : n2;
    PROTECT(ans = allocVector(LGLSXP, n));

    const Rbyte *px1 = XINT_DATA_RO(s1);
    const Rbyte *px2 = XINT_DATA_RO(s2);
    int *pa = LOGICAL(ans);

    /* every operator is a function of the comparison's sign, so the
       per-element dispatch the sibling kernels hoist into one loop per
       operator collapses here to a table settled before the loop */
    int map[3];
    map[0] = (code == NEOP || code == LTOP || code == LEOP);	/* c < 0 */
    map[1] = (code == EQOP || code == LEOP || code == GEOP);	/* c == 0 */
    map[2] = (code == NEOP || code == GTOP || code == GEOP);	/* c > 0 */

    MOD_ITERATE2(n, n1, n2, i, i1, i2, {
	const Rbyte *p1 = px1 + i1 * w;
	const Rbyte *p2 = px2 + i2 * w;
	int d = dir ? dir[(side == 1) ? i1 : i2] : 0;
	/* missing either as the type's own reserved pattern or, where it
	   reserves none, as a mark from the narrowing */
	if (d == XINT_CMP_NA ||
	    (hasNA && (R_xintEltIsNAFast(p1, w, k) ||
		       R_xintEltIsNAFast(p2, w, k)))) {
	    pa[i] = NA_LOGICAL;
	    continue;
	}
	/* an operand the type cannot hold is not missing: it lies below
	   or above every element, so the comparison is settled by which
	   side it was on and which way it fell */
	int c = d ? ((side == 1) ? d : -d) : R_xintEltCmp(p1, p2, w, k);
	pa[i] = map[(c < 0) ? 0 : ((c == 0) ? 1 : 2)];
    });

    UNPROTECT(3); /* ans, s2, s1 */
    vmaxset(vmax);

    return ans;
}

attribute_hidden SEXP
R_xintCompare(SEXP call, int oper, SEXP x, SEXP y)
{
    if (TYPEOF(x) == REALSXP || TYPEOF(y) == REALSXP ||
	TYPEOF(x) == CPLXSXP || TYPEOF(y) == CPLXSXP)
	return xint_numeric_relop((RELOP_TYPE) oper, x, y, call);
    return xint_relop((RELOP_TYPE) oper, x, y, call);
}

static SEXP raw_relop(RELOP_TYPE code, SEXP s1, SEXP s2)
{
    R_xlen_t i, i1, i2, n, n1, n2;
    Rbyte x1, x2;
    SEXP ans;

    n1 = XLENGTH(s1);
    n2 = XLENGTH(s2);
    n = (n1 > n2) ? n1 : n2;
    PROTECT(s1);
    PROTECT(s2);
    ans = allocVector(LGLSXP, n);

    const Rbyte *px1 = RAW_RO(s1);
    const Rbyte *px2 = RAW_RO(s2);
    int *pa = LOGICAL(ans);

    switch (code) {
    case EQOP:
	MOD_ITERATE2(n, n1, n2, i, i1, i2, {
//	    if ((i+1) % NINTERRUPT == 0) R_CheckUserInterrupt();
	    x1 = px1[i1];
	    x2 = px2[i2];
	    pa[i] = (x1 == x2);
	});
	break;
    case NEOP:
	MOD_ITERATE2(n, n1, n2, i, i1, i2, {
//	    if ((i+1) % NINTERRUPT == 0) R_CheckUserInterrupt();
	    x1 = px1[i1];
	    x2 = px2[i2];
	    pa[i] = (x1 != x2);
	});
	break;
    case LTOP:
	MOD_ITERATE2(n, n1, n2, i, i1, i2, {
//	    if ((i+1) % NINTERRUPT == 0) R_CheckUserInterrupt();
	    x1 = px1[i1];
	    x2 = px2[i2];
	    pa[i] = (x1 < x2);
	});
	break;
    case GTOP:
	MOD_ITERATE2(n, n1, n2, i, i1, i2, {
//	    if ((i+1) % NINTERRUPT == 0) R_CheckUserInterrupt();
	    x1 = px1[i1];
	    x2 = px2[i2];
	    pa[i] = (x1 > x2);
	});
	break;
    case LEOP:
	MOD_ITERATE2(n, n1, n2, i, i1, i2, {
//	    if ((i+1) % NINTERRUPT == 0) R_CheckUserInterrupt();
	    x1 = px1[i1];
	    x2 = px2[i2];
	    pa[i] = (x1 <= x2);
	});
	break;
    case GEOP:
	MOD_ITERATE2(n, n1, n2, i, i1, i2, {
//	    if ((i+1) % NINTERRUPT == 0) R_CheckUserInterrupt();
	    x1 = px1[i1];
	    x2 = px2[i2];
	    pa[i] = (x1 >= x2);
	});
	break;
    }
    UNPROTECT(2);
    return ans;
}


static SEXP bitwiseNot(SEXP a)
{
    SEXP ans;
    int np = 0;
    if(isReal(a)) {a = PROTECT(coerceVector(a, INTSXP)); np++;}

    switch(TYPEOF(a)) {
    case INTSXP:
	{
	    R_xlen_t m = XLENGTH(a);
	    ans = allocVector(INTSXP, m);
	    int *pans = INTEGER(ans);
	    const int *pa = INTEGER_RO(a);
	    for(R_xlen_t i = 0; i < m; i++) {
		int aa = pa[i];
		pans[i] = (aa == NA_INTEGER) ? aa : ~aa;
	    }
	}
	break;
    default:
	UNIMPLEMENTED_TYPE("bitwNot", a);
    }
    if(np) UNPROTECT(np);
    return ans;
}

#define mymax(x, y) ((x >= y) ? x : y)

#define BIT(op, name)							\
    SEXP ans;								\
    int np = 0;								\
    if(isReal(a)) {a = PROTECT(coerceVector(a, INTSXP)); np++;}		\
    if(isReal(b)) {b = PROTECT(coerceVector(b, INTSXP)); np++;}		\
    if (TYPEOF(a) != TYPEOF(b))						\
	error(_("'a' and 'b' must have the same type"));		\
    switch(TYPEOF(a)) {							\
    case INTSXP:							\
	{								\
	    R_xlen_t i, ia, ib;						\
	    R_xlen_t m = XLENGTH(a), n = XLENGTH(b),			\
		mn = (m && n) ? mymax(m, n) : 0;			\
	    ans = allocVector(INTSXP, mn);				\
	    int *pans = INTEGER(ans);					\
	    const int *pa = INTEGER_RO(a), *pb = INTEGER_RO(b);		\
	    MOD_ITERATE2(mn, m, n, i, ia, ib, {				\
		    int aa = pa[ia]; int bb = pb[ib];			\
		    pans[i] = (aa == NA_INTEGER || bb == NA_INTEGER) ?	\
			NA_INTEGER : aa op bb;				\
		});							\
	}								\
	break;								\
    default:								\
	UNIMPLEMENTED_TYPE(name, a);					\
    }									\
    if(np) UNPROTECT(np);						\
    return ans

static SEXP bitwiseAnd(SEXP a, SEXP b)
{
    BIT(&, "bitwAnd");
}

static SEXP bitwiseOr(SEXP a, SEXP b)
{
    BIT(|, "bitwOr");
}

static SEXP bitwiseXor(SEXP a, SEXP b)
{
    BIT(^, "bitwXor");
}

static SEXP bitwiseShiftL(SEXP a, SEXP b)
{
    SEXP ans;
    int np = 0;
    if(isReal(a)) {a = PROTECT(coerceVector(a, INTSXP)); np++;}
    if(!isInteger(b)) {b = PROTECT(coerceVector(b, INTSXP)); np++;}
    if (TYPEOF(a) != TYPEOF(b))
	error(_("'a' and 'b' must have the same type"));

    switch(TYPEOF(a)) {
    case INTSXP:
	{
	    R_xlen_t i, ia, ib;
	    R_xlen_t m = XLENGTH(a), n = XLENGTH(b),
		mn = (m && n) ? mymax(m, n) : 0;
	    ans = allocVector(INTSXP, mn);
	    int *pans = INTEGER(ans);
	    const int *pa = INTEGER_RO(a), *pb = INTEGER_RO(b);
	    MOD_ITERATE2(mn, m, n, i, ia, ib, {
		    int aa = pa[ia]; int bb = pb[ib];
		    pans[i] =
			(aa == NA_INTEGER || bb == NA_INTEGER ||
			 bb < 0 || bb > 31) ?
			NA_INTEGER : ((unsigned int)aa << bb);
		});
	}
	break;
    default:
	UNIMPLEMENTED_TYPE("bitShiftL", a);
    }
    if(np) UNPROTECT(np);
    return ans;
}

static SEXP bitwiseShiftR(SEXP a, SEXP b)
{
    SEXP ans;
    int np = 0;
    if(isReal(a)) {a = PROTECT(coerceVector(a, INTSXP)); np++;}
    if(!isInteger(b)) {b = PROTECT(coerceVector(b, INTSXP)); np++;}
    if (TYPEOF(a) != TYPEOF(b))
	error(_("'a' and 'b' must have the same type"));

    switch(TYPEOF(a)) {
    case INTSXP:
	{
	    R_xlen_t i, ia, ib;
	    R_xlen_t m = XLENGTH(a), n = XLENGTH(b),
		mn = (m && n) ? mymax(m, n) : 0;
	    ans = allocVector(TYPEOF(a), mn);
	    int *pans = INTEGER(ans);
	    const int *pa = INTEGER_RO(a), *pb = INTEGER_RO(b);
	    MOD_ITERATE2(mn, m, n, i, ia, ib, {
		    int aa = pa[ia]; int bb = pb[ib];
		    pans[i] =
			(aa == NA_INTEGER || bb == NA_INTEGER ||
			 bb < 0 || bb > 31) ?
			NA_INTEGER : ((unsigned int)aa >> bb);
		});
	}
	break;
    default:
	UNIMPLEMENTED_TYPE("bitShiftR", a);
    }
    if(np) UNPROTECT(np);
    return ans;
}

attribute_hidden SEXP do_bitwise(SEXP call, SEXP op, SEXP args, SEXP env)
{
    checkArity(op, args);
    SEXP ans = R_NilValue; /* -Wall */

    /* 'xinteger' vectors use their own width-aware path. */
    if(TYPEOF(CAR(args)) == ALTSXP || TYPEOF(CADR(args)) == ALTSXP)
	return R_xintBitwise(call, PRIMVAL(op), CAR(args), CADR(args));

    switch(PRIMVAL(op)) {
    case 1: ans = bitwiseAnd(CAR(args), CADR(args)); break;
    case 2: ans = bitwiseNot(CAR(args)); break;
    case 3: ans = bitwiseOr(CAR(args), CADR(args)); break;
    case 4: ans = bitwiseXor(CAR(args), CADR(args)); break;
    case 5: ans = bitwiseShiftL(CAR(args), CADR(args)); break;
    case 6: ans = bitwiseShiftR(CAR(args), CADR(args)); break;
    }
    return ans;
}
