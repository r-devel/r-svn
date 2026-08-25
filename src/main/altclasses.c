/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 2016--2026   The R Core Team
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
#include <R_ext/Altrep.h>
#include <ctype.h> /* for isspace */
#include <errno.h>
#include <float.h> /* for DBL_DIG */
#include <stdint.h> /* for int64_t */
#include <Print.h> /* for R_print */
#include <R_ext/Itermacros.h>

#ifdef Win32
#include <trioremap.h> /* for %lld */
#endif

/* as in arithmetic.c and relop.c: how often a long element loop stops to
   let the user interrupt it */
#define NINTERRUPT 10000000


/***
 *** ALTREP Concrete Class Implementations
 ***/

/**
 ** Compact Integer Sequences
 **/

/*
 * Methods
 */

#define COMPACT_SEQ_INFO(x) R_altrep_data1(x)
#define COMPACT_SEQ_EXPANDED(x) R_altrep_data2(x)
#define SET_COMPACT_SEQ_EXPANDED(x, v) R_set_altrep_data2(x, v)

/* needed for now for objects serialized with INTSXP state */
#define COMPACT_INTSEQ_SERIALIZED_STATE_LENGTH(info) \
    (TYPEOF(info) == INTSXP ? INTEGER0(info)[0] : (R_xlen_t) REAL0(info)[0])
#define COMPACT_INTSEQ_SERIALIZED_STATE_FIRST(info) \
    (TYPEOF(info) == INTSXP ? INTEGER0(info)[1] : (int) REAL0(info)[1])
#define COMPACT_INTSEQ_SERIALIZED_STATE_INCR(info) \
    (TYPEOF(info) == INTSXP ? INTEGER0(info)[2] : (int) REAL0(info)[2])

/* info is stored as REALSXP to allow for long vector length */
#define COMPACT_INTSEQ_INFO_LENGTH(info) ((R_xlen_t) REAL0(info)[0])
#define COMPACT_INTSEQ_INFO_FIRST(info) ((int) REAL0(info)[1])
#define COMPACT_INTSEQ_INFO_INCR(info) ((int) REAL0(info)[2])

/* By default, compact integer sequences are marked as not mutable at
   creation time. Thus even when expanded the expanded data will
   correspond to the original integer sequence (unless it runs into
   mis-behaving C code). If COMPACT_INTSEQ_MUTABLE is defined, then
   the sequence is not marked as not mutable. Once the DATAPTR has
   been requested and releases, the expanded data might be modified by
   an assignment and no longer correspond to the original sequence. */
//#define COMPACT_INTSEQ_MUTABLE

static SEXP compact_intseq_Serialized_state(SEXP x)
{
#ifdef COMPACT_INTSEQ_MUTABLE
    /* This drops through to standard serialization for expanded
       compact vectors */
    if (COMPACT_SEQ_EXPANDED(x) != R_NilValue)
	return NULL;
#endif
    return COMPACT_SEQ_INFO(x);
}

static SEXP new_compact_intseq(R_xlen_t, int, int);
static SEXP new_compact_realseq(R_xlen_t, double, double);

static SEXP compact_intseq_Unserialize(SEXP class, SEXP state)
{
    R_xlen_t n = COMPACT_INTSEQ_SERIALIZED_STATE_LENGTH(state);
    int n1 = COMPACT_INTSEQ_SERIALIZED_STATE_FIRST(state);
    int inc = COMPACT_INTSEQ_SERIALIZED_STATE_INCR(state);

    if (inc == 1)
	return new_compact_intseq(n, n1,  1);
    else if (inc == -1)
	return new_compact_intseq(n, n1,  -1);
    else
	error("compact sequences with increment %d not supported yet", inc);
}
 
static SEXP compact_intseq_Coerce(SEXP x, int type)
{
#ifdef COMPACT_INTSEQ_MUTABLE
    /* This drops through to standard coercion for expanded compact
       vectors */
    if (COMPACT_SEQ_EXPANDED(x) != R_NilValue)
	return NULL;
#endif
    if (type == REALSXP) {
	SEXP info = COMPACT_SEQ_INFO(x);
	R_xlen_t n = COMPACT_INTSEQ_INFO_LENGTH(info);
	int n1 = COMPACT_INTSEQ_INFO_FIRST(info);
	int inc = COMPACT_INTSEQ_INFO_INCR(info);
	return new_compact_realseq(n, n1, inc);
    }
    else return NULL;
}

static SEXP compact_intseq_Duplicate(SEXP x, Rboolean deep)
{
    R_xlen_t n = XLENGTH(x);
    SEXP val = allocVector(INTSXP, n);
    INTEGER_GET_REGION(x, 0, n, INTEGER0(val));
    return val;
}

static
Rboolean compact_intseq_Inspect(SEXP x, int pre, int deep, int pvec,
				void (*inspect_subtree)(SEXP, int, int, int))
{
    int inc = COMPACT_INTSEQ_INFO_INCR(COMPACT_SEQ_INFO(x));
    if (inc != 1 && inc != -1)
	error("compact sequences with increment %d not supported yet", inc);

#ifdef COMPACT_INTSEQ_MUTABLE
    if (COMPACT_SEQ_EXPANDED(x) != R_NilValue) {
	Rprintf("  <expanded compact integer sequence>\n");
	inspect_subtree(COMPACT_SEQ_EXPANDED(x), pre, deep, pvec);
	return TRUE;
    }
#endif

    R_xlen_t n = XLENGTH(x); // int .. LENGTH(.) not ok, e.g. for -1e9:2e9
    int n1 = INTEGER_ELT(x, 0);
    int n2 = (int) ((inc == 1) ? n1 + n - 1 : n1 - n + 1);
    Rprintf(" %d : %d (%s)", n1, n2,
	    COMPACT_SEQ_EXPANDED(x) == R_NilValue ? "compact" : "expanded");
    Rprintf("\n");
    return TRUE;
}

static R_INLINE R_xlen_t compact_intseq_Length(SEXP x)
{
    SEXP info = COMPACT_SEQ_INFO(x);
    return COMPACT_INTSEQ_INFO_LENGTH(info);
}

static void *compact_intseq_Dataptr(SEXP x, Rboolean writeable)
{
    if (COMPACT_SEQ_EXPANDED(x) == R_NilValue) {
	/* no need to re-run if expanded data exists */
	PROTECT(x);
	SEXP info = COMPACT_SEQ_INFO(x);
	R_xlen_t n = COMPACT_INTSEQ_INFO_LENGTH(info);
	int n1 = COMPACT_INTSEQ_INFO_FIRST(info);
	int inc = COMPACT_INTSEQ_INFO_INCR(info);
	SEXP val = allocVector(INTSXP, n);
	int *data = INTEGER(val);

	if (inc == 1) {
	    /* compact sequences n1 : n2 with n1 <= n2 */
	    for (R_xlen_t i = 0; i < n; i++)
		data[i] = (int) (n1 + i);
	}
	else if (inc == -1) {
	    /* compact sequences n1 : n2 with n1 > n2 */
	    for (R_xlen_t i = 0; i < n; i++)
		data[i] = (int) (n1 - i);
	}
	else
	    error("compact sequences with increment %d not supported yet", inc);

	SET_COMPACT_SEQ_EXPANDED(x, val);
	UNPROTECT(1);
    }
    return DATAPTR_RW(COMPACT_SEQ_EXPANDED(x));
}

static const void *compact_intseq_Dataptr_or_null(SEXP x)
{
    SEXP val = COMPACT_SEQ_EXPANDED(x);
    return val == R_NilValue ? NULL : DATAPTR_RO(val);
}

static int compact_intseq_Elt(SEXP x, R_xlen_t i)
{
    SEXP ex = COMPACT_SEQ_EXPANDED(x);
    if (ex != R_NilValue)
	return INTEGER0(ex)[i];
    else {
	SEXP info = COMPACT_SEQ_INFO(x);
	int n1 = COMPACT_INTSEQ_INFO_FIRST(info);
	int inc = COMPACT_INTSEQ_INFO_INCR(info);
	return (int) (n1 + inc * i);
    }
}

#define CHECK_NOT_EXPANDED(x)					\
    if (DATAPTR_OR_NULL(x) != NULL)				\
	error("method should only handle unexpanded vectors")

static R_xlen_t
compact_intseq_Get_region(SEXP sx, R_xlen_t i, R_xlen_t n, int *buf)
{
    /* should not get here if x is already expanded */
    CHECK_NOT_EXPANDED(sx);

    SEXP info = COMPACT_SEQ_INFO(sx);
    R_xlen_t size = COMPACT_INTSEQ_INFO_LENGTH(info);
    R_xlen_t n1 = COMPACT_INTSEQ_INFO_FIRST(info);
    int inc = COMPACT_INTSEQ_INFO_INCR(info);

    R_xlen_t ncopy = size - i > n ? n : size - i;
    if (inc == 1) {
	for (R_xlen_t k = 0; k < ncopy; k++)
	    buf[k] = (int) (n1 + k + i);
	return ncopy;
    }
    else if (inc == -1) {
	for (R_xlen_t k = 0; k < ncopy; k++)
	    buf[k] = (int) (n1 - k - i);
	return ncopy;
    }
    else
	error("compact sequences with increment %d not supported yet", inc);
}

static int compact_intseq_Is_sorted(SEXP x)
{
#ifdef COMPACT_INTSEQ_MUTABLE
    /* If the vector has been expanded it may have been modified. */
    if (COMPACT_SEQ_EXPANDED(x) != R_NilValue)
	return UNKNOWN_SORTEDNESS;
#endif
    int inc = COMPACT_INTSEQ_INFO_INCR(COMPACT_SEQ_INFO(x));
    return inc < 0 ? SORTED_DECR : SORTED_INCR;
}

static int compact_intseq_No_NA(SEXP x)
{
#ifdef COMPACT_INTSEQ_MUTABLE
    /* If the vector has been expanded it may have been modified. */
    if (COMPACT_SEQ_EXPANDED(x) != R_NilValue)
	return FALSE;
#endif
    return TRUE;
}

/* XXX this also appears in summary.c. move to header file?*/
#define R_INT_MIN (1 + INT_MIN)

static SEXP compact_intseq_Sum(SEXP x, Rboolean narm)
{
#ifdef COMPACT_INTSEQ_MUTABLE
    /* If the vector has been expanded it may have been modified. */
    if (COMPACT_SEQ_EXPANDED(x) != R_NilValue) 
	return NULL;
#endif
    double tmp;
    SEXP info = COMPACT_SEQ_INFO(x);
    R_xlen_t size = COMPACT_INTSEQ_INFO_LENGTH(info);
    R_xlen_t n1 = COMPACT_INTSEQ_INFO_FIRST(info);
    int inc = COMPACT_INTSEQ_INFO_INCR(info);
    tmp = (size / 2.0) * (n1 + n1 + inc * (size - 1));
    if(tmp > INT_MAX || tmp < R_INT_MIN)
	/**** check for overflow of exact integer range? */
	return ScalarReal(tmp);
    else
	return ScalarInteger((int) tmp);
}


/*
 * Class Objects and Method Tables
 */

R_altrep_class_t R_compact_intseq_class;

static void InitCompactIntegerClass(void)
{
    R_altrep_class_t cls = R_make_altinteger_class("compact_intseq", "base",
						   NULL);
    R_compact_intseq_class = cls;

    /* override ALTREP methods */
    R_set_altrep_Unserialize_method(cls, compact_intseq_Unserialize);
    R_set_altrep_Serialized_state_method(cls, compact_intseq_Serialized_state);
    R_set_altrep_Duplicate_method(cls, compact_intseq_Duplicate);
    R_set_altrep_Coerce_method(cls, compact_intseq_Coerce);
    R_set_altrep_Inspect_method(cls, compact_intseq_Inspect);
    R_set_altrep_Length_method(cls, compact_intseq_Length);

    /* override ALTVEC methods */
    R_set_altvec_Dataptr_method(cls, compact_intseq_Dataptr);
    R_set_altvec_Dataptr_or_null_method(cls, compact_intseq_Dataptr_or_null);

    /* override ALTINTEGER methods */
    R_set_altinteger_Elt_method(cls, compact_intseq_Elt);
    R_set_altinteger_Get_region_method(cls, compact_intseq_Get_region);
    R_set_altinteger_Is_sorted_method(cls, compact_intseq_Is_sorted);
    R_set_altinteger_No_NA_method(cls, compact_intseq_No_NA);
    R_set_altinteger_Sum_method(cls, compact_intseq_Sum);
}


/*
 * Constructor
 */

static SEXP new_compact_intseq(R_xlen_t n, int n1, int inc)
{
    if (n == 1) return ScalarInteger(n1);

    if (inc != 1 && inc != -1)
	error("compact sequences with increment %d not supported yet", inc);

    /* info used REALSXP to allow for long vectors */
    SEXP info = allocVector(REALSXP, 3);
    REAL0(info)[0] = (double) n;
    REAL0(info)[1] = (double) n1;
    REAL0(info)[2] = (double) inc;

    SEXP ans = R_new_altrep(R_compact_intseq_class, info, R_NilValue);
#ifndef COMPACT_INTSEQ_MUTABLE
    MARK_NOT_MUTABLE(ans); /* force duplicate on modify */
#endif

    return ans;
}

attribute_hidden Rboolean R_is_compact_intseq(SEXP x)
{
    return R_altrep_inherits(x, R_compact_intseq_class);
}


/**
 ** Compact Real Sequences
 **/

/*
 * Methods
 */

#define COMPACT_REALSEQ_INFO_LENGTH(info) ((R_xlen_t) REAL0(info)[0])
#define COMPACT_REALSEQ_INFO_FIRST(info) REAL0(info)[1]
#define COMPACT_REALSEQ_INFO_INCR(info) REAL0(info)[2]

static SEXP compact_realseq_Serialized_state(SEXP x)
{
    return COMPACT_SEQ_INFO(x);
}

static SEXP compact_realseq_Unserialize(SEXP class, SEXP state)
{
    double inc = COMPACT_REALSEQ_INFO_INCR(state);
    R_xlen_t len = COMPACT_REALSEQ_INFO_LENGTH(state);
    double n1 = COMPACT_REALSEQ_INFO_FIRST(state);

    if (inc == 1)
	return new_compact_realseq(len, n1,  1);
    else if (inc == -1)
	return new_compact_realseq(len, n1, -1);
    else
	error("compact sequences with increment %f not supported yet", inc);
}

static SEXP compact_realseq_Duplicate(SEXP x, Rboolean deep)
{
    R_xlen_t n = XLENGTH(x);
    SEXP val = allocVector(REALSXP, n);
    REAL_GET_REGION(x, 0, n, REAL0(val));
    return val;
}

static
Rboolean compact_realseq_Inspect(SEXP x, int pre, int deep, int pvec,
				 void (*inspect_subtree)(SEXP, int, int, int))
{
    double inc = COMPACT_REALSEQ_INFO_INCR(COMPACT_SEQ_INFO(x));
    if (inc != 1 && inc != -1)
	error("compact sequences with increment %f not supported yet", inc);

    R_xlen_t n = XLENGTH(x);
    R_xlen_t n1 = (R_xlen_t) REAL_ELT(x, 0);
    R_xlen_t n2 = inc == 1 ? n1 + n - 1 : n1 - n + 1;
    Rprintf(" %lld : %lld (%s)", (long long)n1, (long long)n2,
	    COMPACT_SEQ_EXPANDED(x) == R_NilValue ? "compact" : "expanded");
    Rprintf("\n");
    return TRUE;
}

static R_INLINE R_xlen_t compact_realseq_Length(SEXP x)
{
    return (R_xlen_t) REAL0(COMPACT_SEQ_INFO(x))[0];
}

static void *compact_realseq_Dataptr(SEXP x, Rboolean writeable)
{
    if (COMPACT_SEQ_EXPANDED(x) == R_NilValue) {
	PROTECT(x);
	SEXP info = COMPACT_SEQ_INFO(x);
	R_xlen_t n = COMPACT_REALSEQ_INFO_LENGTH(info);
	double n1 = COMPACT_REALSEQ_INFO_FIRST(info);
	double inc = COMPACT_REALSEQ_INFO_INCR(info);
	
	SEXP val = allocVector(REALSXP, (R_xlen_t) n);
	double *data = REAL(val);

	if (inc == 1) {
	    /* compact sequences n1 : n2 with n1 <= n2 */
	    for (R_xlen_t i = 0; i < n; i++)
		data[i] = n1 + i;
	}
	else if (inc == -1) {
	    /* compact sequences n1 : n2 with n1 > n2 */
	    for (R_xlen_t i = 0; i < n; i++)
		data[i] = n1 - i;
	}
	else
	    error("compact sequences with increment %f not supported yet", inc);

	SET_COMPACT_SEQ_EXPANDED(x, val);
	UNPROTECT(1);
    }
    return DATAPTR_RW(COMPACT_SEQ_EXPANDED(x));
}

static const void *compact_realseq_Dataptr_or_null(SEXP x)
{
    SEXP val = COMPACT_SEQ_EXPANDED(x);
    return val == R_NilValue ? NULL : DATAPTR_RO(val);
}

static double compact_realseq_Elt(SEXP x, R_xlen_t i)
{
    SEXP ex = COMPACT_SEQ_EXPANDED(x);
    if (ex != R_NilValue)
	return REAL0(ex)[i];
    else {
	SEXP info = COMPACT_SEQ_INFO(x);
	double n1 = COMPACT_REALSEQ_INFO_FIRST(info);
	double inc = COMPACT_REALSEQ_INFO_INCR(info);
	return n1 + inc * i;
    }
}

static R_xlen_t
compact_realseq_Get_region(SEXP sx, R_xlen_t i, R_xlen_t n, double *buf)
{
    /* should not get here if x is already expanded */
    CHECK_NOT_EXPANDED(sx);

    SEXP info = COMPACT_SEQ_INFO(sx);
    R_xlen_t size = COMPACT_REALSEQ_INFO_LENGTH(info);
    double n1 = COMPACT_REALSEQ_INFO_FIRST(info);
    double inc = COMPACT_REALSEQ_INFO_INCR(info);

    R_xlen_t ncopy = size - i > n ? n : size - i;
    if (inc == 1) {
	for (R_xlen_t k = 0; k < ncopy; k++)
	    buf[k] = n1 + k + i;
	return ncopy;
    }
    else if (inc == -1) {
	for (R_xlen_t k = 0; k < ncopy; k++)
	    buf[k] = n1 - k - i;
	return ncopy;
    }
    else
	error("compact sequences with increment %f not supported yet", inc);
}
    
static int compact_realseq_Is_sorted(SEXP x)
{
#ifdef COMPACT_REALSEQ_MUTABLE
    /* If the vector has been expanded it may have been modified. */
    if (COMPACT_SEQ_EXPANDED(x) != R_NilValue)
	return UNKNOWN_SORTEDNESS;
#endif
    double inc = COMPACT_REALSEQ_INFO_INCR(COMPACT_SEQ_INFO(x));
    return inc < 0 ? SORTED_DECR : SORTED_INCR;
}

static int compact_realseq_No_NA(SEXP x)
{
#ifdef COMPACT_REALSEQ_MUTABLE
    /* If the vector has been expanded it may have been modified. */
    if (COMPACT_SEQ_EXPANDED(x) != R_NilValue)
	return FALSE;
#endif
    return TRUE;
}

static SEXP compact_realseq_Sum(SEXP x, Rboolean narm)
{
#ifdef COMPACT_INTSEQ_MUTABLE
    /* If the vector has been expanded it may have been modified. */
    if (COMPACT_SEQ_EXPANDED(x) != R_NilValue) 
	return NULL;
#endif
    SEXP info = COMPACT_SEQ_INFO(x);
    double size = (double) COMPACT_REALSEQ_INFO_LENGTH(info);
    double n1 = COMPACT_REALSEQ_INFO_FIRST(info);
    double inc = COMPACT_REALSEQ_INFO_INCR(info);
    return ScalarReal((size / 2.0) *(n1 + n1 + inc * (size - 1)));
}


/*
 * Class Objects and Method Tables
 */


R_altrep_class_t R_compact_realseq_class;

static void InitCompactRealClass(void)
{
    R_altrep_class_t cls = R_make_altreal_class("compact_realseq", "base",
						NULL);
    R_compact_realseq_class = cls;

    /* override ALTREP methods */
    R_set_altrep_Unserialize_method(cls, compact_realseq_Unserialize);
    R_set_altrep_Serialized_state_method(cls, compact_realseq_Serialized_state);
    R_set_altrep_Duplicate_method(cls, compact_realseq_Duplicate);
    R_set_altrep_Inspect_method(cls, compact_realseq_Inspect);
    R_set_altrep_Length_method(cls, compact_realseq_Length);

    /* override ALTVEC methods */
    R_set_altvec_Dataptr_method(cls, compact_realseq_Dataptr);
    R_set_altvec_Dataptr_or_null_method(cls, compact_realseq_Dataptr_or_null);

    /* override ALTREAL methods */
    R_set_altreal_Elt_method(cls, compact_realseq_Elt);
    R_set_altreal_Get_region_method(cls, compact_realseq_Get_region);
    R_set_altreal_Is_sorted_method(cls, compact_realseq_Is_sorted);
    R_set_altreal_No_NA_method(cls, compact_realseq_No_NA);
    R_set_altreal_Sum_method(cls, compact_realseq_Sum);
}


/*
 * Constructor
 */

static SEXP new_compact_realseq(R_xlen_t n, double n1, double inc)
{
    if (n == 1) return ScalarReal(n1);

    if (inc != 1 && inc != -1)
	error("compact sequences with increment %f not supported yet", inc);

    SEXP info = allocVector(REALSXP, 3);
    REAL(info)[0] = n;
    REAL(info)[1] = n1;
    REAL(info)[2] = inc;

    SEXP ans = R_new_altrep(R_compact_realseq_class, info, R_NilValue);
    MARK_NOT_MUTABLE(ans); /* force duplicate on modify */

    return ans;
}


/**
 ** Compact Integer/Real Sequences
 **/

attribute_hidden SEXP R_compact_intrange(R_xlen_t n1, R_xlen_t n2)
{
    R_xlen_t n = n1 <= n2 ? n2 - n1 + 1 : n1 - n2 + 1;

    if (n >= R_XLEN_T_MAX)
	error("result would be too long a vector");

    if (n1 <= INT_MIN || n1 > INT_MAX || n2 <= INT_MIN || n2 > INT_MAX)
	return new_compact_realseq(n, n1, n1 <= n2 ? 1 : -1);
    else
	return new_compact_intseq(n, (int) n1, n1 <= n2 ? 1 : -1);
}


/**
 ** Deferred String Coercions
 **/

/*
 * Methods
 */

#define DEFERRED_STRING_STATE(x) R_altrep_data1(x)
#define	CLEAR_DEFERRED_STRING_STATE(x) R_set_altrep_data1(x, R_NilValue)
#define DEFERRED_STRING_EXPANDED(x) R_altrep_data2(x)
#define SET_DEFERRED_STRING_EXPANDED(x, v) R_set_altrep_data2(x, v)

#define MAKE_DEFERRED_STRING_STATE(v, sp) CONS(v, sp)
#define DEFERRED_STRING_STATE_ARG(s) CAR(s)
#define DEFERRED_STRING_STATE_INFO(s) CDR(s)

#define DEFERRED_STRING_ARG(x) \
    DEFERRED_STRING_STATE_ARG(DEFERRED_STRING_STATE(x))
#define DEFERRED_STRING_INFO(x) \
    DEFERRED_STRING_STATE_INFO(DEFERRED_STRING_STATE(x))
#define DEFERRED_STRING_SCIPEN(x) \
    INTEGER0(DEFERRED_STRING_STATE_INFO(DEFERRED_STRING_STATE(x)))[0]

/* work-around for package code that mutates things it shouldn't and
   makes serialize and inspect infinite-loop */
#define DEFERRED_STRING_FIXUP_ARG_ATTRIBS(state) do {			\
	if (state != R_NilValue && ATTRIB(CAR(state)) != R_NilValue) {	\
	    SETCAR(state, shallow_duplicate(CAR(state)));		\
	    SET_ATTRIB(CAR(state), R_NilValue);				\
	}								\
    } while (0)
    
static SEXP R_OutDecSym = NULL;

static R_INLINE const char *DEFERRED_STRING_OUTDEC(SEXP x)
{
    /* The default value of OutDec at startup is ".". If it is
       something different at the time the deferred string conversion
       is created then the current value is stored as an attribute. */
    if (R_OutDecSym == NULL)
	R_OutDecSym = install("OutDec");
    SEXP info = DEFERRED_STRING_INFO(x);
    if (ATTRIB(info) != R_NilValue) {
	SEXP outdecattr = getAttrib(info, R_OutDecSym);
	if (TYPEOF(outdecattr) == STRSXP && XLENGTH(outdecattr) == 1)
	    return CHAR(STRING_ELT(outdecattr, 0));
    }
    return ".";
}

static SEXP deferred_string_Serialized_state(SEXP x)
{
    /* This drops through to standard serialization for fully expanded
       deferred string conversions. Partial expansions are OK since
       they still correspond to the original data. An assignment to
       the object will access the DATAPTR and force a full expansion
       and dropping the original data. */
    SEXP state = DEFERRED_STRING_STATE(x);
    DEFERRED_STRING_FIXUP_ARG_ATTRIBS(state);
    return state != R_NilValue ? state : NULL;
}

static SEXP deferred_string_Unserialize(SEXP class, SEXP state)
{
    SEXP arg = DEFERRED_STRING_STATE_ARG(state);
    SEXP info = DEFERRED_STRING_STATE_INFO(state);
    return R_deferred_coerceToString(arg, info);
}

static
Rboolean deferred_string_Inspect(SEXP x, int pre, int deep, int pvec,
				 void (*inspect_subtree)(SEXP, int, int, int))
{
    SEXP state = DEFERRED_STRING_STATE(x);
    if (state != R_NilValue) {
	DEFERRED_STRING_FIXUP_ARG_ATTRIBS(state);
	SEXP arg = DEFERRED_STRING_STATE_ARG(state);
	Rprintf("  <deferred string conversion>\n");
	inspect_subtree(arg, pre, deep, pvec);
    }
    else {
	Rprintf("  <expanded string conversion>\n");
	inspect_subtree(DEFERRED_STRING_EXPANDED(x), pre, deep, pvec);
    }
    return TRUE;
}

static R_INLINE R_xlen_t deferred_string_Length(SEXP x)
{
    SEXP state = DEFERRED_STRING_STATE(x);
    return state == R_NilValue ?
	XLENGTH(DEFERRED_STRING_EXPANDED(x)) :
	XLENGTH(DEFERRED_STRING_STATE_ARG(state));
}

static R_INLINE SEXP ExpandDeferredStringElt(SEXP x, R_xlen_t i)
{
    /* make sure the STRSXP for the expanded string is allocated */
    /* not yet expanded strings are NULL in the STRSXP */
    SEXP val = DEFERRED_STRING_EXPANDED(x);
    if (val == R_NilValue) {
	R_xlen_t n = XLENGTH(x);
	val = allocVector(STRSXP, n);
	if (n)
	    memset(STDVEC_DATAPTR(val), 0, n * sizeof(SEXP));
	SET_DEFERRED_STRING_EXPANDED(x, val);
    }

    SEXP elt = STRING_ELT(val, i);
    if (elt == NULL) {
	int warn; /* not used by the coercion functions */
	int savedigits, savescipen;
	SEXP data = DEFERRED_STRING_ARG(x);
	switch(TYPEOF(data)) {
	case INTSXP:
	    elt = StringFromInteger(INTEGER_ELT(data, i), &warn);
	    break;
	case REALSXP:
	    savedigits = R_print.digits;
	    savescipen = R_print.scipen;
	    R_print.digits = DBL_DIG;/* MAX precision */
	    R_print.scipen = DEFERRED_STRING_SCIPEN(x);
	    const char *myoutdec = DEFERRED_STRING_OUTDEC(x);
	    if (strcmp(OutDec, myoutdec)) {
		/* The current and saved OutDec values differ. The
		   value to use is put in a static buffer and OutDec
		   temporarily points to this buffer while
		   StringFromReal is called and then reset. The buffer
		   originally pointed to by OutDec cannot be used as
		   it wil not be writable if the default "." has not
		   been changed. */
		static char buf[10];
		strncpy(buf, myoutdec, sizeof buf);
		buf[sizeof(buf) - 1] = '\0';
		char *savedOutDec = OutDec;
		OutDec = buf;
		elt = StringFromReal(REAL_ELT(data, i), &warn);
		OutDec = savedOutDec;
	    }
	    else
		elt = StringFromReal(REAL_ELT(data, i), &warn);
	    R_print.digits = savedigits;
	    R_print.scipen = savescipen;
	    break;
	default:
	    error("unsupported type for deferred string coercion");
	}
	SET_STRING_ELT(val, i, elt);
    }
    return elt;
}

static R_INLINE void expand_deferred_string(SEXP x)
{
    SEXP state = DEFERRED_STRING_STATE(x);
    if (state != R_NilValue) {
	/* expanded data may be incomplete until original data is removed */
	PROTECT(x);
	R_xlen_t n = XLENGTH(x), i;
	if (n == 0)
	    SET_DEFERRED_STRING_EXPANDED(x, allocVector(STRSXP, 0));
	else
	    for (i = 0; i < n; i++)
		ExpandDeferredStringElt(x, i);
	CLEAR_DEFERRED_STRING_STATE(x); /* allow arg to be reclaimed */
	UNPROTECT(1);
    }
}

static void *deferred_string_Dataptr(SEXP x, Rboolean writeable)
{
    expand_deferred_string(x);
    return DATAPTR_RW(DEFERRED_STRING_EXPANDED(x));
}

static const void *deferred_string_Dataptr_or_null(SEXP x)
{
    SEXP state = DEFERRED_STRING_STATE(x);
    return state != R_NilValue ? NULL : DATAPTR_RO(DEFERRED_STRING_EXPANDED(x));
}

static SEXP deferred_string_Elt(SEXP x, R_xlen_t i)
{
    SEXP state = DEFERRED_STRING_STATE(x);
    if (state == R_NilValue)
	/* string is fully expanded */
	return STRING_ELT(DEFERRED_STRING_EXPANDED(x), i);
    else {
	/* expand only the requested element */
	PROTECT(x);
	SEXP elt = ExpandDeferredStringElt(x, i);
	UNPROTECT(1);
	return elt;
    }
}

static void deferred_string_Set_elt(SEXP x, R_xlen_t i, SEXP v)
{
    expand_deferred_string(x);
    SET_STRING_ELT(DEFERRED_STRING_EXPANDED(x), i, v);
}

static int deferred_string_Is_sorted(SEXP x)
{
    /* same as the default method; sortedness of the numeric is not relevant  */
    return UNKNOWN_SORTEDNESS;
}

static int deferred_string_No_NA(SEXP x)
{
    SEXP state = DEFERRED_STRING_STATE(x);
    if (state == R_NilValue)
	/* string is fully expanded and may have been modified. */
	return FALSE;
    else {
	/* defer to the argument */
	SEXP arg = DEFERRED_STRING_STATE_ARG(state);
	switch(TYPEOF(arg)) {
	case INTSXP: return INTEGER_NO_NA(arg);
	case REALSXP: return REAL_NO_NA(arg);
	default: return FALSE;
	}
    }
}

static SEXP deferred_string_Extract_subset(SEXP x, SEXP indx, SEXP call)
{
    SEXP result = NULL;

    if (! OBJECT(x) && ATTRIB(x) == R_NilValue &&
	DEFERRED_STRING_STATE(x) != R_NilValue) {
	/* For deferred string coercions, create a new conversion
	   using the subset of the argument.  Could try to
	   preserve/share coercions already done, if there are any. */
	SEXP data = DEFERRED_STRING_ARG(x);
	SEXP info = DEFERRED_STRING_INFO(x);
	PROTECT(result = ExtractSubset(data, indx, call));
	result = R_deferred_coerceToString(result, info);
	UNPROTECT(1);
	return result;
    }

    return result;
}


/*
 * Class Object and Method Table
 */

static R_altrep_class_t R_deferred_string_class;

static void InitDefferredStringClass(void)
{
    R_altrep_class_t cls = R_make_altstring_class("deferred_string", "base",
						  NULL);
    R_deferred_string_class = cls;

    /* override ALTREP methods */
    R_set_altrep_Unserialize_method(cls, deferred_string_Unserialize);
    R_set_altrep_Serialized_state_method(cls, deferred_string_Serialized_state);
    R_set_altrep_Inspect_method(cls, deferred_string_Inspect);
    R_set_altrep_Length_method(cls, deferred_string_Length);

    /* override ALTVEC methods */
    R_set_altvec_Dataptr_method(cls, deferred_string_Dataptr);
    R_set_altvec_Dataptr_or_null_method(cls, deferred_string_Dataptr_or_null);
    R_set_altvec_Extract_subset_method(cls, deferred_string_Extract_subset);

    /* override ALTSTRING methods */
    R_set_altstring_Elt_method(cls, deferred_string_Elt);
    R_set_altstring_Set_elt_method(cls, deferred_string_Set_elt);
    R_set_altstring_Is_sorted_method(cls, deferred_string_Is_sorted);
    R_set_altstring_No_NA_method(cls, deferred_string_No_NA);
}


/*
 * Constructor
 */

attribute_hidden SEXP R_deferred_coerceToString(SEXP v, SEXP info)
{
    SEXP ans = R_NilValue;
    switch (TYPEOF(v)) {
    case INTSXP:
    case REALSXP:
	PROTECT(v); /* may not be needed, but to be safe ... */
	if (info == NULL) {
	    PrintDefaults(); /* to set R_print from options */
	    info = ScalarInteger(R_print.scipen);
	    if (strcmp(OutDec, ".")) {
		/* non-default OutDec setting -- attach as an attribute */
		PROTECT(info);
		if (R_OutDecSym == NULL)
		    R_OutDecSym = install("OutDec");
		setAttrib(info, R_OutDecSym, GetOption1(R_OutDecSym));
		UNPROTECT(1); /* info */
	    }
	}
	MARK_NOT_MUTABLE(v); /* make sure it can't change once captured */
	ans = PROTECT(MAKE_DEFERRED_STRING_STATE(v, info));
	ans = R_new_altrep(R_deferred_string_class, ans, R_NilValue);
	UNPROTECT(2); /* ans, v */
	break;
    default:
	error("unsupported type for deferred string coercion");
    }
    return ans;
}


/**
 ** Memory Mapped Vectors
 **/

/* For now, this code is designed to work both in base R and in a
   package. Some simplifications would be possible if it was only to
   be used in base. in particular, the issue of finalizing objects
   before unloading the library would not need to be addressed, and
   ordinary finalizers in the external pointers could be used instead
   of maintaining a weak reference list of the live mmap objects. */

/*
 * MMAP Object State
 */

/* State is held in a LISTSXP of length 3, and includes
   
       file
       size and length in a REALSXP
       type, ptrOK, wrtOK, serOK in an INTSXP

   These are used by the methods, and also represent the serialized
   state object.
 */

#ifndef Win32
static SEXP make_mmap_state(SEXP file, size_t size, int type,
			    Rboolean ptrOK, Rboolean wrtOK, Rboolean serOK)
{
    SEXP sizes = PROTECT(allocVector(REALSXP, 2));
    double *dsizes = REAL(sizes);
    dsizes[0] = size;
    switch(type) {
    case INTSXP: dsizes[1] = size / sizeof(int); break;
    case REALSXP: dsizes[1] = size / sizeof(double); break;
    default: error("mmap for %s not supported yet", type2char(type));
    }

    SEXP info = PROTECT(allocVector(INTSXP, 4));
    INTEGER(info)[0] = type;
    INTEGER(info)[1] = ptrOK;
    INTEGER(info)[2] = wrtOK;
    INTEGER(info)[3] = serOK;

    SEXP state = list3(file, sizes, info);

    UNPROTECT(2);
    return state;
}
#endif

#define MMAP_STATE_FILE(x) CAR(x)
#define MMAP_STATE_SIZE(x) ((size_t) REAL_ELT(CADR(x), 0))
#define MMAP_STATE_LENGTH(x) ((size_t) REAL_ELT(CADR(x), 1))
#define MMAP_STATE_TYPE(x) INTEGER(CADDR(x))[0]
#define MMAP_STATE_PTROK(x) INTEGER(CADDR(x))[1]
#define MMAP_STATE_WRTOK(x) INTEGER(CADDR(x))[2]
#define MMAP_STATE_SEROK(x) INTEGER(CADDR(x))[3]


/*
 * MMAP Classes and Objects
 */

static R_altrep_class_t mmap_integer_class;
static R_altrep_class_t mmap_real_class;

/* MMAP objects are ALTREP objects with data fields

       data1: an external pointer to the mmaped address
       data2: the MMAP object's state

   The state is also stored in the Protected field of the external
   pointer for use by the finalizer.
*/

#ifndef Win32
static void register_mmap_eptr(SEXP eptr);
static SEXP make_mmap(void *p, SEXP file, size_t size, int type,
		      Rboolean ptrOK, Rboolean wrtOK, Rboolean serOK)
{
    SEXP state = PROTECT(make_mmap_state(file, size,
					 type, ptrOK, wrtOK, serOK));
    SEXP eptr = PROTECT(R_MakeExternalPtr(p, R_NilValue, state));
    register_mmap_eptr(eptr);

    R_altrep_class_t class;
    switch(type) {
    case INTSXP:
	class = mmap_integer_class;
	break;
    case REALSXP:
	class = mmap_real_class;
	break;
    default: error("mmap for %s not supported yet", type2char(type));
    }

    SEXP ans = R_new_altrep(class, eptr, state);
    if (ptrOK && ! wrtOK)
	MARK_NOT_MUTABLE(ans);

    UNPROTECT(2); /* state, eptr */
    return ans;
}
#endif

#define MMAP_EPTR(x) R_altrep_data1(x)
#define MMAP_STATE(x) R_altrep_data2(x)
#define MMAP_LENGTH(x) MMAP_STATE_LENGTH(MMAP_STATE(x))
#define MMAP_PTROK(x) MMAP_STATE_PTROK(MMAP_STATE(x))
#define MMAP_WRTOK(x) MMAP_STATE_WRTOK(MMAP_STATE(x))
#define MMAP_SEROK(x) MMAP_STATE_SEROK(MMAP_STATE(x))

#define MMAP_EPTR_STATE(x) R_ExternalPtrProtected(x)

static R_INLINE void *MMAP_ADDR(SEXP x)
{
    SEXP eptr = MMAP_EPTR(x);
    void *addr = R_ExternalPtrAddr(eptr);

    if (addr == NULL)
	error("object has been unmapped");
    return addr;
}

/* We need to maintain a list of weak references to the external
   pointers of memory-mapped objects so a request to unload the shared
   library can finalize them before unloading; otherwise, attempting
   to run a finalizer after unloading would result in an illegal
   instruction. */


#ifndef Win32
static SEXP mmap_list = NULL;

#define MAXCOUNT 10

static void mmap_finalize(SEXP eptr);
static void register_mmap_eptr(SEXP eptr)
{
    if (mmap_list == NULL) {
	mmap_list = CONS(R_NilValue, R_NilValue);
	R_PreserveObject(mmap_list);
    }
    
    /* clean out the weak list every MAXCOUNT calls*/
    static int cleancount = MAXCOUNT;
    if (--cleancount <= 0) {
	cleancount = MAXCOUNT;
	for (SEXP last = mmap_list, next = CDR(mmap_list);
	     next != R_NilValue;
	     next = CDR(next))
	    if (R_WeakRefKey(CAR(next)) == R_NilValue)
		SETCDR(last, CDR(next));
	    else
		last = next;
    }

    /* add a weak reference with a finalizer to the list */
    SETCDR(mmap_list, 
	   CONS(R_MakeWeakRefC(eptr, R_NilValue, mmap_finalize, TRUE),
		CDR(mmap_list)));

    /* store the weak reference in the external pointer for do_munmap_file */
    R_SetExternalPtrTag(eptr, CAR(CDR(mmap_list)));
}
#endif

#ifdef SIMPLEMMAP
static void finalize_mmap_objects()
{
    if (mmap_list == NULL)
	return;
    
    /* finalize any remaining mmap objects before unloading */
    for (SEXP next = CDR(mmap_list); next != R_NilValue; next = CDR(next))
	R_RunWeakRefFinalizer(CAR(next));
    R_ReleaseObject(mmap_list);
}
#endif


/*
 * ALTREP Methods
 */

static SEXP mmap_Serialized_state(SEXP x)
{
    /* If serOK is FALSE then serialize as a regular typed vector. If
       serOK is true, then serialize information to allow the mmap to
       be reconstructed. The original file name is serialized; it will
       be expanded again when unserializing, in a context where the
       result may be different. */
    if (MMAP_SEROK(x))
	return MMAP_STATE(x);
    else
	return NULL;
}

static SEXP mmap_file(SEXP, int, Rboolean, Rboolean, Rboolean, Rboolean);

static SEXP mmap_Unserialize(SEXP class, SEXP state)
{
    SEXP file = MMAP_STATE_FILE(state);
    int type = MMAP_STATE_TYPE(state);
    Rboolean ptrOK = (Rboolean) MMAP_STATE_PTROK(state);
    Rboolean wrtOK = (Rboolean) MMAP_STATE_WRTOK(state);
    Rboolean serOK = (Rboolean) MMAP_STATE_SEROK(state);

    SEXP val = mmap_file(file, type, ptrOK, wrtOK, serOK, TRUE);
    if (val == NULL) {
	/**** The attempt to memory map failed. Eventually it would be
	      good to have a mechanism to allow the user to try to
	      resolve this.  For now, return a length zero vector with
	      another warning. */
	warning("memory mapping failed; returning vector of length zero");
	return allocVector(type, 0);
    }
    return val;
}

static Rboolean mmap_Inspect(SEXP x, int pre, int deep, int pvec,
			     void (*inspect_subtree)(SEXP, int, int, int))
{
    Rboolean ptrOK = (Rboolean) MMAP_PTROK(x);
    Rboolean wrtOK = (Rboolean) MMAP_WRTOK(x);
    Rboolean serOK = (Rboolean) MMAP_SEROK(x);
    Rprintf(" mmaped %s", R_typeToChar(x));
    Rprintf(" [ptr=%d,wrt=%d,ser=%d]\n", ptrOK, wrtOK, serOK);
    return TRUE;
}


/*
 * ALTVEC Methods
 */

static R_xlen_t mmap_Length(SEXP x)
{
    return MMAP_LENGTH(x);
}

static void *mmap_Dataptr(SEXP x, Rboolean writeable)
{
    /* get addr first to get error if the object has been unmapped */
    void *addr = MMAP_ADDR(x);

    if (MMAP_PTROK(x))
	return addr;
    else
	error("cannot access data pointer for this mmaped vector");
}

static const void *mmap_Dataptr_or_null(SEXP x)
{
    return MMAP_PTROK(x) ? MMAP_ADDR(x) : NULL;
}


/*
 * ALTINTEGER Methods
 */

static int mmap_integer_Elt(SEXP x, R_xlen_t i)
{
    int *p = MMAP_ADDR(x);
    return p[i];
}

static
R_xlen_t mmap_integer_Get_region(SEXP sx, R_xlen_t i, R_xlen_t n, int *buf)
{
    int *x = MMAP_ADDR(sx);
    R_xlen_t size = XLENGTH(sx);
    R_xlen_t ncopy = size - i > n ? n : size - i;
    for (R_xlen_t k = 0; k < ncopy; k++)
	buf[k] = x[k + i];
    //memcpy(buf, x + i, ncopy * sizeof(int));
    return ncopy;
}


/*
 * ALTREAL Methods
 */

static double mmap_real_Elt(SEXP x, R_xlen_t i)
{
    double *p = MMAP_ADDR(x);
    return p[i];
}

static
R_xlen_t mmap_real_Get_region(SEXP sx, R_xlen_t i, R_xlen_t n, double *buf)
{
    double *x = MMAP_ADDR(sx);
    R_xlen_t size = XLENGTH(sx);
    R_xlen_t ncopy = size - i > n ? n : size - i;
    for (R_xlen_t k = 0; k < ncopy; k++)
	buf[k] = x[k + i];
    //memcpy(buf, x + i, ncopy * sizeof(double));
    return ncopy;
}


/*
 * Class Objects and Method Tables
 */

#ifdef SIMPLEMMAP
# define MMAPPKG "simplemmap"
#else
# define MMAPPKG "base"
#endif

static void InitMmapIntegerClass(DllInfo *dll)
{
    R_altrep_class_t cls =
	R_make_altinteger_class("mmap_integer", MMAPPKG, dll);
    mmap_integer_class = cls;
 
    /* override ALTREP methods */
    R_set_altrep_Unserialize_method(cls, mmap_Unserialize);
    R_set_altrep_Serialized_state_method(cls, mmap_Serialized_state);
    R_set_altrep_Inspect_method(cls, mmap_Inspect);
    R_set_altrep_Length_method(cls, mmap_Length);

    /* override ALTVEC methods */
    R_set_altvec_Dataptr_method(cls, mmap_Dataptr);
    R_set_altvec_Dataptr_or_null_method(cls, mmap_Dataptr_or_null);

    /* override ALTINTEGER methods */
    R_set_altinteger_Elt_method(cls, mmap_integer_Elt);
    R_set_altinteger_Get_region_method(cls, mmap_integer_Get_region);
}

static void InitMmapRealClass(DllInfo *dll)
{
    R_altrep_class_t cls =
	R_make_altreal_class("mmap_real", MMAPPKG, dll);
    mmap_real_class = cls;

    /* override ALTREP methods */
    R_set_altrep_Unserialize_method(cls, mmap_Unserialize);
    R_set_altrep_Serialized_state_method(cls, mmap_Serialized_state);
    R_set_altrep_Inspect_method(cls, mmap_Inspect);
    R_set_altrep_Length_method(cls, mmap_Length);

    /* override ALTVEC methods */
    R_set_altvec_Dataptr_method(cls, mmap_Dataptr);
    R_set_altvec_Dataptr_or_null_method(cls, mmap_Dataptr_or_null);

    /* override ALTREAL methods */
    R_set_altreal_Elt_method(cls, mmap_real_Elt);
    R_set_altreal_Get_region_method(cls, mmap_real_Get_region);
}


/*
 * Constructor
 */

#ifdef Win32
/* unused
static void mmap_finalize(SEXP eptr)
{
    error("mmap objects not supported on Windows yet");
}
*/

static SEXP mmap_file(SEXP file, int type, Rboolean ptrOK, Rboolean wrtOK,
		      Rboolean serOK, Rboolean warn)
{
    error("mmap objects not supported on Windows yet");
}
#else
/* derived from the example in
  https://www.safaribooksonline.com/library/view/linux-system-programming/0596009585/ch04s03.html */

#include <sys/stat.h>
#include <fcntl.h>
#include <errno.h>
#include <unistd.h>
#include <sys/mman.h>

//#define DEBUG_PRINT(x) REprintf(x);
#define DEBUG_PRINT(x) do { } while (0)

static void mmap_finalize(SEXP eptr)
{
    DEBUG_PRINT("finalizing ... ");
    void *p = R_ExternalPtrAddr(eptr);
    size_t size = MMAP_STATE_SIZE(MMAP_EPTR_STATE(eptr));

    if (p != NULL) {
	munmap(p, size); /* don't check for errors */
	R_SetExternalPtrAddr(eptr, NULL);
    }
    DEBUG_PRINT("done\n");
}

#define MMAP_FILE_WARNING_OR_ERROR(str, ...) do {	\
	if (warn) {					\
	    warning(str, __VA_ARGS__);			\
	    return NULL;				\
	}						\
	else error(str, __VA_ARGS__);			\
    } while (0)
	    
static SEXP mmap_file(SEXP file, int type, Rboolean ptrOK, Rboolean wrtOK,
		      Rboolean serOK, Rboolean warn)
{
    const char *efn = R_ExpandFileName(translateCharFP(STRING_ELT(file, 0)));
    struct stat sb;

    /* Target not link */
    if (stat(efn, &sb) != 0)
	MMAP_FILE_WARNING_OR_ERROR("stat: %s", strerror(errno));

    if (! S_ISREG(sb.st_mode))
	MMAP_FILE_WARNING_OR_ERROR("%s is not a regular file", efn);

    int oflags = wrtOK ? O_RDWR : O_RDONLY;
    int fd = open(efn, oflags);
    if (fd == -1)
	MMAP_FILE_WARNING_OR_ERROR("open: %s", strerror(errno));

    int pflags = wrtOK ? PROT_READ | PROT_WRITE : PROT_READ;
    void *p = mmap(0, sb.st_size, pflags, MAP_SHARED, fd, 0);
    close(fd); /* don't care if this fails */
    if (p == MAP_FAILED)
	MMAP_FILE_WARNING_OR_ERROR("mmap: %s", strerror(errno));

    return make_mmap(p, file, sb.st_size, type, ptrOK, wrtOK, serOK);
}
#endif

static Rboolean asLogicalNA(SEXP x, Rboolean dflt)
{
    int val = asLogical(x);
    return val == NA_LOGICAL ? dflt : (Rboolean) val;
}

#ifdef SIMPLEMMAP
SEXP do_mmap_file(SEXP args)
{
    args = CDR(args);
#else
attribute_hidden SEXP do_mmap_file(SEXP call, SEXP op, SEXP args, SEXP env)
{
#endif
    SEXP file = CAR(args);
    SEXP stype = CADR(args);
    SEXP sptrOK = CADDR(args);
    SEXP swrtOK = CADDDR(args);
    SEXP sserOK = CAD4R(args);

    int type = REALSXP;
    if (stype != R_NilValue) {
	const char *typestr = CHAR(asChar(stype));
	if (strcmp(typestr, "double") == 0)
	    type = REALSXP;
	else if (strcmp(typestr, "integer") == 0 ||
		 strcmp(typestr, "int") == 0)
	    type = INTSXP;
	else
	    error("type '%s' is not supported", typestr);
    }    

    Rboolean ptrOK = sptrOK == R_NilValue ? TRUE : asLogicalNA(sptrOK, FALSE);
    Rboolean wrtOK = swrtOK == R_NilValue ? FALSE : asLogicalNA(swrtOK, FALSE);
    Rboolean serOK = sserOK == R_NilValue ? FALSE : asLogicalNA(sserOK, FALSE);

    if (TYPEOF(file) != STRSXP || LENGTH(file) != 1 || file == NA_STRING)
	error("invalud 'file' argument");

    return mmap_file(file, type, ptrOK, wrtOK, serOK, FALSE);
}

#ifdef SIMPLEMMAP
static SEXP do_munmap_file(SEXP args)
{
    args = CDR(args);
#else
attribute_hidden SEXP do_munmap_file(SEXP call, SEXP op, SEXP args, SEXP env)
{
#endif
    SEXP x = CAR(args);

    /**** would be useful to have R_mmap_class virtual class as parent here */
    if (! (R_altrep_inherits(x, mmap_integer_class) ||
	   R_altrep_inherits(x, mmap_real_class)))
	error("not a memory-mapped object");

    /* using the finalizer is a cheat to avoid yet another #ifdef Windows */
    SEXP eptr = MMAP_EPTR(x);
    errno = 0;
    R_RunWeakRefFinalizer(R_ExternalPtrTag(eptr));
    if (errno)
	error("munmap: %s", strerror(errno));
    return R_NilValue;
}


/**
 ** Attribute and Meta Data Wrappers
 **/

/*
 * Wrapper Classes and Objects
 */

#define NMETA 2

static R_altrep_class_t wrap_integer_class;
static R_altrep_class_t wrap_logical_class;
static R_altrep_class_t wrap_real_class;
static R_altrep_class_t wrap_complex_class;
static R_altrep_class_t wrap_raw_class;
static R_altrep_class_t wrap_string_class;
static R_altrep_class_t wrap_list_class;

/* Wrapper objects are ALTREP objects designed to hold the attributes
   of a potentially large object and/or meta data for the object. */

#define WRAPPER_WRAPPED(x) R_altrep_data1(x)
#define WRAPPER_SET_WRAPPED(x, v) R_set_altrep_data1(x, v)
#define WRAPPER_METADATA(x) R_altrep_data2(x)

#define WRAPPER_SORTED(x) INTEGER(WRAPPER_METADATA(x))[0]
#define WRAPPER_NO_NA(x) INTEGER(WRAPPER_METADATA(x))[1]

/* When a wrapper is created, e.g. using structure(), the data may
   initially be shared. Once it is modified to be modified or a
   DATAPTR is requested the data has to be remain unchanged and the
   wrapper should be the only reference. The metadata is marked to
   reflecth this. The data then has to be duplicated by the duplicate
   method to ensure that no new references are created. This ensures
   that a DATAPTR, once obtained, remains valid while the wrapper
   object is reachable.
   
   For now the sxpinfo.gp field is used via PRSEEN for the lock.
   Allow for cases where shallow_duplicate() returns a value with
   non-zero REFCNT (e.g. returns a value marked not mutable.
 */
#define WRAPPER_DATA_LOCK(x) PRSEEN(WRAPPER_METADATA(x))
#define WRAPPER_SET_DATA_LOCK(x, v) SET_PRSEEN(WRAPPER_METADATA(x), v)
#define WRAPPER_DATA_IS_LOCKED(x) (WRAPPER_DATA_LOCK(x) > 0)
#define WRAPPER_LOCK_DATA(x) WRAPPER_SET_DATA_LOCK(x, 1)
#define WRAPPER_UNLOCK_DATA(x) WRAPPER_SET_DATA_LOCK(x, 0)

 
static R_INLINE SEXP WRAPPER_WRAPPED_RW(SEXP x)
{
    SEXP data = WRAPPER_WRAPPED(x);
    if (WRAPPER_DATA_IS_LOCKED(x)) {
	/* Once data is locked it's reference count should remain at one. */
	/* Unless duplicate() doesn't produce a zero reference count object */ 
	if (MAYBE_SHARED(data) && WRAPPER_DATA_LOCK(x) == 1)
	    error("REFCNT on locked WRAPPER data increased to %d",
		  REFCNT(data));
    }
    else {
	/* If the data might be shared and is accessed for possible
	   modification, then it needs to be duplicated now. */
	if (MAYBE_SHARED(data)) {
	    PROTECT(x);
	    WRAPPER_SET_WRAPPED(x, shallow_duplicate(data));
	    UNPROTECT(1);
	    if (REFCNT(WRAPPER_WRAPPED(x)) == 1)
		WRAPPER_LOCK_DATA(x);
	    else
		WRAPPER_SET_DATA_LOCK(x, 2);
	}
	else WRAPPER_LOCK_DATA(x);
    }

    /* The meta data also needs to be cleared as it may no longer be
       valid after a write. */
    SEXP meta = WRAPPER_METADATA(x);
    INTEGER(meta)[0] = UNKNOWN_SORTEDNESS;
    for (int i = 1; i < NMETA; i++)
	INTEGER(meta)[i] = 0;

    return WRAPPER_WRAPPED(x);
}


/*
 * ALTREP Methods
 */

static SEXP wrapper_Serialized_state(SEXP x)
{
    /* If the wrapped value is not an ALTREP and there is no useful
       metadata then return NULL to allow this to be serialized as a
       standard object. This avoids serializing potentially large
       attributes on the wrapped value (PR18142). */
    if (! ALTREP(WRAPPER_WRAPPED(x)) &&
	WRAPPER_SORTED(x) == UNKNOWN_SORTEDNESS &&
	! WRAPPER_NO_NA(x))
	return NULL;

    return CONS(WRAPPER_WRAPPED(x), WRAPPER_METADATA(x));
}

static SEXP make_wrapper(SEXP, SEXP);

static SEXP wrapper_Unserialize(SEXP class, SEXP state)
{
    return make_wrapper(CAR(state), CDR(state));
}

static SEXP wrapper_Duplicate(SEXP x, Rboolean deep)
{
    SEXP data = WRAPPER_WRAPPED(x);

    /* For a deep copy, duplicate the data. */
    /* For a shallow copy, mark as immutable in the NAMED world; with
       reference counting the reference count will be incremented when
       the data is installed in the new wrapper object. */
    if (deep || WRAPPER_DATA_IS_LOCKED(x)) // **** shallow duplicate if only locked?
	data = duplicate(data);
#ifndef SWITCH_TO_REFCNT
    else
	/* not needed with reference counting */
	MARK_NOT_MUTABLE(data);
#endif
    PROTECT(data);

    /* always duplicate the meta data */
    SEXP meta = PROTECT(duplicate(WRAPPER_METADATA(x)));

    SEXP ans = make_wrapper(data, meta);

    UNPROTECT(2); /* data, meta */
    return ans;
}

static Rboolean wrapper_Inspect(SEXP x, int pre, int deep, int pvec,
				void (*inspect_subtree)(SEXP, int, int, int))
{
    Rboolean srt = (Rboolean) WRAPPER_SORTED(x);
    Rboolean no_na = (Rboolean) WRAPPER_NO_NA(x);
    Rboolean lck = (Rboolean) WRAPPER_DATA_IS_LOCKED(x);
    Rprintf(" wrapper [srt=%d,no_na=%d,lck=%d]\n", srt, no_na, lck);
    inspect_subtree(WRAPPER_WRAPPED(x), pre, deep, pvec);
    return TRUE;
}

static R_xlen_t wrapper_Length(SEXP x)
{
    return XLENGTH(WRAPPER_WRAPPED(x));
}


/*
 * ALTVEC Methods
 */

static void *wrapper_Dataptr(SEXP x, Rboolean writeable)
{
    if (writeable)
	return DATAPTR_RW(WRAPPER_WRAPPED_RW(x));
    else
	/* This has to use WRAPPER_WRAPPED_RW even for a read-only
	   pointer to make sure a later request for a writable pointer
	   will return the same address. */
	/**** could avoid the cast by having separate methods */
	return (void *) DATAPTR_RO(WRAPPER_WRAPPED_RW(x));
}

static const void *wrapper_Dataptr_or_null(SEXP x)
{
    /* This has to use WRAPPER_WRAPPED_RW as above. */
    return DATAPTR_OR_NULL(WRAPPER_WRAPPED_RW(x));
}

static SEXP wrapper_Extract_subset(SEXP x, SEXP indx, SEXP call)
{
  return ExtractSubset(WRAPPER_WRAPPED(x), indx, call);
}


/*
 * ALTINTEGER Methods
 */

static int wrapper_integer_Elt(SEXP x, R_xlen_t i)
{
    return INTEGER_ELT(WRAPPER_WRAPPED(x), i);
}

static
R_xlen_t wrapper_integer_Get_region(SEXP x, R_xlen_t i, R_xlen_t n, int *buf)
{
    return INTEGER_GET_REGION(WRAPPER_WRAPPED(x), i, n, buf);
}

static int wrapper_integer_Is_sorted(SEXP x)
{
    if (WRAPPER_SORTED(x) != UNKNOWN_SORTEDNESS)
	return WRAPPER_SORTED(x);
    else
	/* If the  meta data bit is not set, defer to the wrapped object. */
	return INTEGER_IS_SORTED(WRAPPER_WRAPPED(x));
}

static int wrapper_integer_no_NA(SEXP x)
{
    if (WRAPPER_NO_NA(x))
	return TRUE;
    else
	/* If the  meta data bit is not set, defer to the wrapped object. */
	return INTEGER_NO_NA(WRAPPER_WRAPPED(x));
}


/*
 * ALTLOGICAL Methods
 */

static int wrapper_logical_Elt(SEXP x, R_xlen_t i)
{
    return LOGICAL_ELT(WRAPPER_WRAPPED(x), i);
}

static
R_xlen_t wrapper_logical_Get_region(SEXP x, R_xlen_t i, R_xlen_t n, int *buf)
{
    return LOGICAL_GET_REGION(WRAPPER_WRAPPED(x), i, n, buf);
}

static int wrapper_logical_Is_sorted(SEXP x)
{
    if (WRAPPER_SORTED(x) != UNKNOWN_SORTEDNESS)
	return WRAPPER_SORTED(x);
    else
	/* If the  meta data bit is not set, defer to the wrapped object. */
	return LOGICAL_IS_SORTED(WRAPPER_WRAPPED(x));
}

static int wrapper_logical_no_NA(SEXP x)
{
    if (WRAPPER_NO_NA(x))
	return TRUE;
    else
	/* If the  meta data bit is not set, defer to the wrapped object. */
	return LOGICAL_NO_NA(WRAPPER_WRAPPED(x));
}


/*
 * ALTREAL Methods
 */

static double wrapper_real_Elt(SEXP x, R_xlen_t i)
{
    return REAL_ELT(WRAPPER_WRAPPED(x), i);
}

static
R_xlen_t wrapper_real_Get_region(SEXP x, R_xlen_t i, R_xlen_t n, double *buf)
{
    return REAL_GET_REGION(WRAPPER_WRAPPED(x), i, n, buf);
}

static int wrapper_real_Is_sorted(SEXP x)
{
    if (WRAPPER_SORTED(x) != UNKNOWN_SORTEDNESS)
	return WRAPPER_SORTED(x);
    else
	/* If the  meta data bit is not set, defer to the wrapped object. */
	return REAL_IS_SORTED(WRAPPER_WRAPPED(x));
}

static int wrapper_real_no_NA(SEXP x)
{
    if (WRAPPER_NO_NA(x))
	return TRUE;
    else
	/* If the  meta data bit is not set, defer to the wrapped object. */
	return REAL_NO_NA(WRAPPER_WRAPPED(x));
}


/*
 * ALTCOMPLEX Methods
 */

static Rcomplex wrapper_complex_Elt(SEXP x, R_xlen_t i)
{
    return COMPLEX_ELT(WRAPPER_WRAPPED(x), i);
}

static
R_xlen_t wrapper_complex_Get_region(SEXP x, R_xlen_t i, R_xlen_t n,
				    Rcomplex *buf)
{
    return COMPLEX_GET_REGION(WRAPPER_WRAPPED(x), i, n, buf);
}


/*
 * ALTRAW Methods
 */

static Rbyte wrapper_raw_Elt(SEXP x, R_xlen_t i)
{
    return RAW_ELT(WRAPPER_WRAPPED(x), i);
}

static
R_xlen_t wrapper_raw_Get_region(SEXP x, R_xlen_t i, R_xlen_t n, Rbyte *buf)
{
    return RAW_GET_REGION(WRAPPER_WRAPPED(x), i, n, buf);
}


/*
 * ALTSTRING Methods
 */

static SEXP wrapper_string_Elt(SEXP x, R_xlen_t i)
{
    return STRING_ELT(WRAPPER_WRAPPED(x), i);
}

static void wrapper_string_Set_elt(SEXP x, R_xlen_t i, SEXP v)
{
    SET_STRING_ELT(WRAPPER_WRAPPED_RW(x), i, v);
}

static int wrapper_string_Is_sorted(SEXP x)
{
    if (WRAPPER_SORTED(x) != UNKNOWN_SORTEDNESS)
	return WRAPPER_SORTED(x);
    else
	/* If the  meta data bit is not set, defer to the wrapped object. */
	return STRING_IS_SORTED(WRAPPER_WRAPPED(x));
}

static int wrapper_string_no_NA(SEXP x)
{
    if (WRAPPER_NO_NA(x))
	return TRUE;
    else
	/* If the  meta data bit is not set, defer to the wrapped object. */
	return STRING_NO_NA(WRAPPER_WRAPPED(x));
}


/*
 * ALTLIST Methods
 */

static SEXP wrapper_list_Elt(SEXP x, R_xlen_t i)
{
    return VECTOR_ELT(WRAPPER_WRAPPED(x), i);
}

static void wrapper_list_Set_elt(SEXP x, R_xlen_t i, SEXP v)
{
    SET_VECTOR_ELT(WRAPPER_WRAPPED_RW(x), i, v);
}

/*
 * Class Objects and Method Tables
 */

#define WRAPPKG "base"

static void InitWrapIntegerClass(DllInfo *dll)
{
    R_altrep_class_t cls =
	R_make_altinteger_class("wrap_integer", WRAPPKG, dll);
    wrap_integer_class = cls;
 
    /* override ALTREP methods */
    R_set_altrep_Unserialize_method(cls, wrapper_Unserialize);
    R_set_altrep_Serialized_state_method(cls, wrapper_Serialized_state);
    R_set_altrep_Duplicate_method(cls, wrapper_Duplicate);
    R_set_altrep_Inspect_method(cls, wrapper_Inspect);
    R_set_altrep_Length_method(cls, wrapper_Length);

    /* override ALTVEC methods */
    R_set_altvec_Dataptr_method(cls, wrapper_Dataptr);
    R_set_altvec_Dataptr_or_null_method(cls, wrapper_Dataptr_or_null);
    R_set_altvec_Extract_subset_method(cls, wrapper_Extract_subset);

    /* override ALTINTEGER methods */
    R_set_altinteger_Elt_method(cls, wrapper_integer_Elt);
    R_set_altinteger_Get_region_method(cls, wrapper_integer_Get_region);
    R_set_altinteger_Is_sorted_method(cls, wrapper_integer_Is_sorted);
    R_set_altinteger_No_NA_method(cls, wrapper_integer_no_NA);
}

static void InitWrapLogicalClass(DllInfo *dll)
{
    R_altrep_class_t cls =
	R_make_altlogical_class("wrap_logical", WRAPPKG, dll);
    wrap_logical_class = cls;
 
    /* override ALTREP methods */
    R_set_altrep_Unserialize_method(cls, wrapper_Unserialize);
    R_set_altrep_Serialized_state_method(cls, wrapper_Serialized_state);
    R_set_altrep_Duplicate_method(cls, wrapper_Duplicate);
    R_set_altrep_Inspect_method(cls, wrapper_Inspect);
    R_set_altrep_Length_method(cls, wrapper_Length);

    /* override ALTVEC methods */
    R_set_altvec_Dataptr_method(cls, wrapper_Dataptr);
    R_set_altvec_Dataptr_or_null_method(cls, wrapper_Dataptr_or_null);
    R_set_altvec_Extract_subset_method(cls, wrapper_Extract_subset);

    /* override ALTLOGICAL methods */
    R_set_altlogical_Elt_method(cls, wrapper_logical_Elt);
    R_set_altlogical_Get_region_method(cls, wrapper_logical_Get_region);
    R_set_altlogical_Is_sorted_method(cls, wrapper_logical_Is_sorted);
    R_set_altlogical_No_NA_method(cls, wrapper_logical_no_NA);
}

static void InitWrapRealClass(DllInfo *dll)
{
    R_altrep_class_t cls =
	R_make_altreal_class("wrap_real", WRAPPKG, dll);
    wrap_real_class = cls;
 
    /* override ALTREP methods */
    R_set_altrep_Unserialize_method(cls, wrapper_Unserialize);
    R_set_altrep_Serialized_state_method(cls, wrapper_Serialized_state);
    R_set_altrep_Duplicate_method(cls, wrapper_Duplicate);
    R_set_altrep_Inspect_method(cls, wrapper_Inspect);
    R_set_altrep_Length_method(cls, wrapper_Length);

    /* override ALTVEC methods */
    R_set_altvec_Dataptr_method(cls, wrapper_Dataptr);
    R_set_altvec_Dataptr_or_null_method(cls, wrapper_Dataptr_or_null);
    R_set_altvec_Extract_subset_method(cls, wrapper_Extract_subset);

    /* override ALTREAL methods */
    R_set_altreal_Elt_method(cls, wrapper_real_Elt);
    R_set_altreal_Get_region_method(cls, wrapper_real_Get_region);
    R_set_altreal_Is_sorted_method(cls, wrapper_real_Is_sorted);
    R_set_altreal_No_NA_method(cls, wrapper_real_no_NA);
}

static void InitWrapComplexClass(DllInfo *dll)
{
    R_altrep_class_t cls =
	R_make_altcomplex_class("wrap_complex", WRAPPKG, dll);
    wrap_complex_class = cls;
 
    /* override ALTREP methods */
    R_set_altrep_Unserialize_method(cls, wrapper_Unserialize);
    R_set_altrep_Serialized_state_method(cls, wrapper_Serialized_state);
    R_set_altrep_Duplicate_method(cls, wrapper_Duplicate);
    R_set_altrep_Inspect_method(cls, wrapper_Inspect);
    R_set_altrep_Length_method(cls, wrapper_Length);

    /* override ALTVEC methods */
    R_set_altvec_Dataptr_method(cls, wrapper_Dataptr);
    R_set_altvec_Dataptr_or_null_method(cls, wrapper_Dataptr_or_null);
    R_set_altvec_Extract_subset_method(cls, wrapper_Extract_subset);

    /* override ALTCOMPLEX methods */
    R_set_altcomplex_Elt_method(cls, wrapper_complex_Elt);
    R_set_altcomplex_Get_region_method(cls, wrapper_complex_Get_region);
}

static void InitWrapRawClass(DllInfo *dll)
{
    R_altrep_class_t cls =
	R_make_altraw_class("wrap_raw", WRAPPKG, dll);
    wrap_raw_class = cls;
 
    /* override ALTREP methods */
    R_set_altrep_Unserialize_method(cls, wrapper_Unserialize);
    R_set_altrep_Serialized_state_method(cls, wrapper_Serialized_state);
    R_set_altrep_Duplicate_method(cls, wrapper_Duplicate);
    R_set_altrep_Inspect_method(cls, wrapper_Inspect);
    R_set_altrep_Length_method(cls, wrapper_Length);

    /* override ALTVEC methods */
    R_set_altvec_Dataptr_method(cls, wrapper_Dataptr);
    R_set_altvec_Dataptr_or_null_method(cls, wrapper_Dataptr_or_null);
    R_set_altvec_Extract_subset_method(cls, wrapper_Extract_subset);

    /* override ALTRAW methods */
    R_set_altraw_Elt_method(cls, wrapper_raw_Elt);
    R_set_altraw_Get_region_method(cls, wrapper_raw_Get_region);
}

static void InitWrapStringClass(DllInfo *dll)
{
    R_altrep_class_t cls =
	R_make_altstring_class("wrap_string", WRAPPKG, dll);
    wrap_string_class = cls;
 
    /* override ALTREP methods */
    R_set_altrep_Unserialize_method(cls, wrapper_Unserialize);
    R_set_altrep_Serialized_state_method(cls, wrapper_Serialized_state);
    R_set_altrep_Duplicate_method(cls, wrapper_Duplicate);
    R_set_altrep_Inspect_method(cls, wrapper_Inspect);
    R_set_altrep_Length_method(cls, wrapper_Length);

    /* override ALTVEC methods */
    R_set_altvec_Dataptr_method(cls, wrapper_Dataptr);
    R_set_altvec_Dataptr_or_null_method(cls, wrapper_Dataptr_or_null);
    R_set_altvec_Extract_subset_method(cls, wrapper_Extract_subset);

    /* override ALTSTRING methods */
    R_set_altstring_Elt_method(cls, wrapper_string_Elt);
    R_set_altstring_Set_elt_method(cls, wrapper_string_Set_elt);
    R_set_altstring_Is_sorted_method(cls, wrapper_string_Is_sorted);
    R_set_altstring_No_NA_method(cls, wrapper_string_no_NA);
}

static void InitWrapListClass(DllInfo *dll)
{
    R_altrep_class_t cls =
	R_make_altlist_class("wrap_list", WRAPPKG, dll);
    wrap_list_class = cls;

    /* override ALTREP methods */
    R_set_altrep_Unserialize_method(cls, wrapper_Unserialize);
    R_set_altrep_Serialized_state_method(cls, wrapper_Serialized_state);
    R_set_altrep_Duplicate_method(cls, wrapper_Duplicate);
    R_set_altrep_Inspect_method(cls, wrapper_Inspect);
    R_set_altrep_Length_method(cls, wrapper_Length);

    /* override ALTVEC methods */
    R_set_altvec_Dataptr_method(cls, wrapper_Dataptr);
    R_set_altvec_Dataptr_or_null_method(cls, wrapper_Dataptr_or_null);
    R_set_altvec_Extract_subset_method(cls, wrapper_Extract_subset);

    /* override ALTLIST methods */
    R_set_altlist_Elt_method(cls, wrapper_list_Elt);
    R_set_altlist_Set_elt_method(cls, wrapper_list_Set_elt);
}


/*
 * Constructor
 */

static SEXP make_wrapper(SEXP x, SEXP meta)
{
    /* If x is itself a wrapper it might be a good idea to fuse */
    R_altrep_class_t cls;
    switch(TYPEOF(x)) {
    case INTSXP: cls = wrap_integer_class; break;
    case LGLSXP: cls = wrap_logical_class; break;
    case REALSXP: cls = wrap_real_class; break;
    case CPLXSXP: cls = wrap_complex_class; break;
    case RAWSXP: cls = wrap_raw_class; break;
    case STRSXP: cls = wrap_string_class; break;
    case VECSXP: cls = wrap_list_class; break;
    default: error("unsupported type");
    }

    SEXP ans = R_new_altrep(cls, x, meta);

#define WRAPATTRIB
#ifdef WRAPATTRIB
    if (ATTRIB(x) != R_NilValue) {
	/* could just move attributes if there are no references to x */
	PROTECT(ans);
	SET_ATTRIB(ans, shallow_duplicate(ATTRIB(x)));
	SET_OBJECT(ans, OBJECT(x));
	IS_S4_OBJECT(x) ? SET_S4_OBJECT(ans) : UNSET_S4_OBJECT(ans);
	UNPROTECT(1); /* ans */
    }
#endif

#ifndef SWITCH_TO_REFCNT
    if (MAYBE_REFERENCED(x))
	/* make sure no mutation can happen through another reference */
	MARK_NOT_MUTABLE(x);
#endif

    WRAPPER_UNLOCK_DATA(ans);
    return ans;
}

static R_INLINE int is_wrapper(SEXP x)
{
    if (ALTREP(x))
	switch(TYPEOF(x)) {
	case INTSXP: return R_altrep_inherits(x, wrap_integer_class);
	case LGLSXP: return R_altrep_inherits(x, wrap_logical_class);
	case REALSXP: return R_altrep_inherits(x, wrap_real_class);
	case CPLXSXP: return R_altrep_inherits(x, wrap_complex_class);
	case RAWSXP: return R_altrep_inherits(x, wrap_raw_class);
	case STRSXP: return R_altrep_inherits(x, wrap_string_class);
	case VECSXP: return R_altrep_inherits(x, wrap_list_class);
	default: return FALSE;
	}
    else return FALSE;
}

static SEXP wrap_meta(SEXP x, int srt, int no_na)
{
    switch(TYPEOF(x)) {
    case INTSXP:
    case REALSXP:
    case LGLSXP:
    case CPLXSXP:
    case RAWSXP:
    case STRSXP:
    case VECSXP: break;
    default: return x;
    }

    /* avoid wrappers of wrappers, at least in some cases */
    if (is_wrapper(x) && srt == UNKNOWN_SORTEDNESS && no_na == FALSE)
	return shallow_duplicate(x);

#ifndef WRAPATTRIB
    if (ATTRIB(x) != R_NilValue)
	/* For objects without references we could move the attributes
	   to the wrapper. For objects with references the attributes
	   would have to be shallow duplicated at least. The object/S4
	   bits would need to be moved as well.	*/
	/* For now, just return the original object. */
	return x;
#endif

    if (!KNOWN_SORTED(srt) && srt != KNOWN_UNSORTED &&
	srt != UNKNOWN_SORTEDNESS)
	error("srt must be -2, -1, 0, or +1, +2, or NA");
    
    if (no_na < 0 || no_na > 1)
	error("no_na must be 0 or +1");

    SEXP meta = allocVector(INTSXP, NMETA);
    INTEGER(meta)[0] = srt;
    INTEGER(meta)[1] = no_na;

    return make_wrapper(x, meta);
}

attribute_hidden SEXP do_wrap_meta(SEXP call, SEXP op, SEXP args, SEXP env)
{
    checkArity(op, args);
    SEXP x = CAR(args);
    int srt = asInteger(CADR(args));
    int no_na = asInteger(CADDR(args));
    return wrap_meta(x, srt, no_na);
}

/*attribute_hidden*/ SEXP R_tryWrap(SEXP x)
{
    return wrap_meta(x, UNKNOWN_SORTEDNESS, FALSE);
}

attribute_hidden SEXP do_tryWrap(SEXP call, SEXP op, SEXP args, SEXP env)
{
    checkArity(op, args);
    SEXP x = CAR(args);
    return R_tryWrap(x);
}

/* When a wrapper has no useful meta-data, is no longer referenced
   anywhere, and its data is only accessible from the wrapper, then
   the wrapper can be unwrapped to its wrapped data, and its
   attributes can be transferred to the data.

   It is ESSENTIAL that the wrapper no longer be accessed after
   this function is called!

   This function can be used at the end of a complex assignment
   operation. It could be used in other places, but extreme caution is
   needed to make sure there is no possibility that the wrapper object
   will be referenced from C code after it is cleared. */
attribute_hidden SEXP R_tryUnwrap(SEXP x)
{
    if (! MAYBE_SHARED(x) && is_wrapper(x) &&
	WRAPPER_SORTED(x) == UNKNOWN_SORTEDNESS && ! WRAPPER_NO_NA(x)) {
	SEXP data = WRAPPER_WRAPPED(x);
	if (! MAYBE_SHARED(data)) {
	    SET_ATTRIB(data, ATTRIB(x));
	    SET_OBJECT(data, OBJECT(x));
	    IS_S4_OBJECT(x) ? SET_S4_OBJECT(data) : UNSET_S4_OBJECT(data);

	    /* Clear the fields to drop reference counts and set the
	       type to LISTSXP to limit errors in case the object is
	       still live. */
	    void ALTREP_SET_TYPEOF(SEXP, int); /* in memory.c */
	    ALTREP_SET_TYPEOF(x, LISTSXP);
	    SETALTREP(x, 0);
	    SET_ATTRIB(x, R_NilValue);
	    SETCAR(x, R_NilValue);
	    SETCDR(x, R_NilValue);
	    SET_TAG(x, R_NilValue);
	    SET_OBJECT(x, 0);
	    UNSET_S4_OBJECT(x);
	    /* NAMED should be zero */

	    return data;
	}
    }
    return x;
}


/**
 ** 64-Bit Integer Vectors
 **/

/* int64 and uint64 are ALTSXP classes, so TYPEOF() reports ALTSXP and says
 * nothing about the payload: code that switches on SEXPTYPE fails rather
 * than reading 64-bit values as doubles and rounding them.
 *
 *   data1  RAWSXP of 8*n bytes, the payload
 *   data2  INTSXP(3), { sortedness, no_na, nullable }
 *
 * Extract_subset, Duplicate, Get_region and Set_region are all left to the
 * generic ALTSXP defaults in altrep.c, which need only Elt_size, New and a
 * data pointer.  What is defined here is the part that knows what an element
 * *means*, plus a serialised state, because whether a vector reserves a bit
 * pattern for NA is per-object and the generic state does not carry it.
 */

#define NA_INT64  INT64_MIN
#define NA_UINT64 UINT64_MAX

static R_altrep_class_t int64_class;
static R_altrep_class_t uint64_class;
static SEXP Int64Symbol = NULL;
static SEXP UInt64Symbol = NULL;

#define I64_DATA(x) R_altrep_data1(x)
#define I64_META(x) R_altrep_data2(x)

enum { I64_SORTED = 0, I64_NO_NA, I64_NULLABLE_FIELD, I64_META_N };

/* Whether the domain includes NA is a property of the object, not of the
   class or of the element type.  A nullable vector reserves one bit pattern
   (INT64_MIN, or UINT64_MAX when unsigned) as NA; a non-nullable one has the
   whole 64-bit range for data, which is what a column read from a source
   with no concept of a missing value wants. */
#define I64_NULLABLE(x) (INTEGER(I64_META(x))[I64_NULLABLE_FIELD])

static R_INLINE int i64_unsigned(SEXP x)
{
    return R_altrep_inherits(x, uint64_class);
}

static R_INLINE int i64_is(SEXP x)
{
    return R_altrep_inherits(x, int64_class) ||
	R_altrep_inherits(x, uint64_class);
}

static R_INLINE int64_t *i64_data(SEXP x)
{
    return (int64_t *) RAW(I64_DATA(x));
}

static R_INLINE R_xlen_t i64_length(SEXP x)
{
    return XLENGTH(I64_DATA(x)) / (R_xlen_t) sizeof(int64_t);
}

static R_INLINE int64_t i64_na(SEXP x)
{
    return i64_unsigned(x) ? (int64_t) NA_UINT64 : NA_INT64;
}

static R_INLINE const char *i64_name(SEXP x)
{
    return i64_unsigned(x) ? "uint64" : "int64";
}

/* the NA bit pattern, and whether this object reserves one at all */
static R_INLINE int64_t i64_na_test(SEXP x, int *has_na)
{
    *has_na = I64_NULLABLE(x);
    return i64_na(x);
}

/* How many of the n elements at i are really there; never negative, so an
   out-of-range start is a no-op rather than a huge count. */
static R_INLINE R_xlen_t i64_ncopy(R_xlen_t size, R_xlen_t i, R_xlen_t n)
{
    if (i < 0 || i >= size || n <= 0)
	return 0;
    return size - i > n ? n : size - i;
}

static R_altrep_class_t i64_class_of(SEXP proto)
{
    if (ALTREP(proto))
	return i64_unsigned(proto) ? uint64_class : int64_class;

    /* the class object, passed by the default Unserialize method */
    return (proto == R_SEXP(uint64_class)) ? uint64_class : int64_class;
}

/* returns an unprotected object; PROTECT at the call site */
static SEXP i64_alloc(SEXP proto, R_xlen_t n)
{
    if (n < 0 || n > R_XLEN_T_MAX / (R_xlen_t) sizeof(int64_t))
	error(_("invalid length for a 64-bit integer vector"));

    SEXP data = PROTECT(allocVector(RAWSXP, n * (R_xlen_t) sizeof(int64_t)));
    SEXP meta = PROTECT(allocVector(INTSXP, I64_META_N));
    INTEGER(meta)[I64_SORTED] = UNKNOWN_SORTEDNESS;
    INTEGER(meta)[I64_NO_NA] = 0;
    INTEGER(meta)[I64_NULLABLE_FIELD] = ALTREP(proto) ? I64_NULLABLE(proto) : 1;

    SEXP ans = R_new_altrep(i64_class_of(proto), data, meta);
    UNPROTECT(2);

    return ans;
}

/* the class object is an acceptable prototype for New(); see the note on the
   New method in R_ext/Altrep.h */
#define I64_PROTO(uns) R_SEXP((uns) ? uint64_class : int64_class)

/*
 * Portable checked arithmetic.  R cannot rely on __builtin_*_overflow, and
 * signed overflow is undefined, so every operation is range-checked before
 * it is performed.  Each returns non-zero when the result is not
 * representable.
 */

static int i64_add(int64_t a, int64_t b, int64_t *r)
{
    if ((b > 0 && a > INT64_MAX - b) || (b < 0 && a < INT64_MIN - b))
	return TRUE;
    *r = a + b;
    return FALSE;
}

static int i64_sub(int64_t a, int64_t b, int64_t *r)
{
    if ((b < 0 && a > INT64_MAX + b) || (b > 0 && a < INT64_MIN + b))
	return TRUE;
    *r = a - b;
    return FALSE;
}

static int i64_mul(int64_t a, int64_t b, int64_t *r)
{
    if (a > 0) {
	if (b > 0 && a > INT64_MAX / b) return TRUE;
	if (b < 0 && b < INT64_MIN / a) return TRUE;
    }
    else if (a < 0) {
	if (b > 0 && a < INT64_MIN / b) return TRUE;
	if (b < 0 && a < INT64_MAX / b) return TRUE;
    }
    *r = a * b;
    return FALSE;
}

static int u64_add(uint64_t a, uint64_t b, uint64_t *r)
{
    if (a > UINT64_MAX - b)
	return TRUE;
    *r = a + b;
    return FALSE;
}

static int u64_sub(uint64_t a, uint64_t b, uint64_t *r)
{
    if (a < b)
	return TRUE;
    *r = a - b;
    return FALSE;
}

static int u64_mul(uint64_t a, uint64_t b, uint64_t *r)
{
    if (a != 0 && b > UINT64_MAX / a)
	return TRUE;
    *r = a * b;
    return FALSE;
}

/* *acc += v as unsigned, without aliasing an int64_t through a uint64_t * */
static int u64_acc(int64_t *acc, int64_t v)
{
    uint64_t r;

    if (u64_add((uint64_t) *acc, (uint64_t) v, &r))
	return TRUE;
    *acc = (int64_t) r;

    return FALSE;
}

static R_INLINE int i64_cmp(int64_t a, int64_t b, int uns)
{
    if (uns) {
	uint64_t ua = (uint64_t) a, ub = (uint64_t) b;
	return (ua > ub) - (ua < ub);
    }
    return (a > b) - (a < b);
}

/*
 * Conversion
 */

static SEXP i64_from(SEXP x, int uns, int nullable);

/* PROTECT at the call site.  An operand that is not already of this class is
   built with the NA domain 'nullable', which the caller takes from the
   opaque operand on the other side: promoting an ordinary vector as nullable
   regardless would make every operation look as though it were mixing
   domains, and force a whole-range operand to be widened -- or refused --
   for an operand that has no missing value in it. */
static SEXP i64_materialize(SEXP x, int uns, int nullable)
{
    if (i64_is(x) && i64_unsigned(x) == uns)
	return x;
    return i64_from(x, uns, nullable);
}

static SEXP i64_from(SEXP x, int uns, int nullable)
{
    R_xlen_t n = xlength(x);
    SEXP ans = PROTECT(i64_alloc(I64_PROTO(uns), n));
    INTEGER(I64_META(ans))[I64_NULLABLE_FIELD] = nullable;

    int64_t *out = i64_data(ans);
    int64_t na = uns ? (int64_t) NA_UINT64 : NA_INT64;
    int warn = FALSE, na_seen = FALSE;

    switch (TYPEOF(x)) {
    case RAWSXP: {
	const Rbyte *p = RAW_RO(x);
	for (R_xlen_t i = 0; i < n; i++)
	    out[i] = (int64_t) p[i];
	break;
    }
    case LGLSXP:
    case INTSXP: {
	const int *p = INTEGER_RO(x);
	for (R_xlen_t i = 0; i < n; i++) {
	    if (p[i] == NA_INTEGER) {
		out[i] = na;
		na_seen = TRUE;
	    }
	    else if (uns && p[i] < 0) {
		out[i] = na;
		warn = TRUE;
	    }
	    else
		out[i] = (int64_t) p[i];
	}
	break;
    }
    case REALSXP: {
	const double *p = REAL_RO(x);
	for (R_xlen_t i = 0; i < n; i++) {
	    double v = p[i];
	    if (ISNAN(v)) {
		out[i] = na;
		na_seen = TRUE;
	    }
	    else if (uns ? (v < 0 || v >= 18446744073709551616.0)
		     : (v >= 9223372036854775808.0 ||
			v < -9223372036854775808.0)) {
		out[i] = na;
		warn = TRUE;
	    }
	    else {
		out[i] = uns ? (int64_t) (uint64_t) v : (int64_t) v;
		if (nullable && out[i] == na)
		    warn = TRUE; /* the value reserved for NA */
	    }
	}
	break;
    }
    case STRSXP: {
	for (R_xlen_t i = 0; i < n; i++) {
	    SEXP s = STRING_ELT(x, i);
	    if (s == NA_STRING) {
		out[i] = na;
		na_seen = TRUE;
		continue;
	    }

	    /* the same leading and trailing whitespace as as.integer(), so
	       that a column of blank-padded numbers reads the same way */
	    const char *cs = CHAR(s), *q = cs;
	    char *end;
	    while (isspace((unsigned char) *q)) q++;

	    errno = 0;
	    if (uns) {
		/* strtoull() silently wraps a negative literal */
		uint64_t v = (*q == '-') ? 0 : (uint64_t) strtoull(q, &end, 10);
		out[i] = (int64_t) v;
		if (*q == '-') end = (char *) q;
	    }
	    else
		out[i] = (int64_t) strtoll(q, &end, 10);

	    const char *tail = end;
	    while (isspace((unsigned char) *tail)) tail++;

	    if (end == q || *tail != '\0' || errno == ERANGE ||
		(nullable && out[i] == na)) {
		out[i] = na;
		warn = TRUE;
	    }
	}
	break;
    }
    case ALTSXP:
	if (i64_is(x)) {
	    if (i64_unsigned(x) == uns) {
		memcpy(out, i64_data(x), (size_t) n * sizeof(int64_t));
		/* The bytes are the same either way; what differs is what the
		   NA pattern means.  Going to a wider domain, a datum that
		   collides with the pattern would read back as NA; going to a
		   narrower one, an NA has nowhere to go.  Both are errors,
		   and neither is detectable after the copy. */
		if (nullable != I64_NULLABLE(x))
		    for (R_xlen_t i = 0; i < n; i++)
			if (out[i] == na) {
			    if (! nullable) {
				na_seen = TRUE;
				break;
			    }
			    UNPROTECT(1);
			    error(_("element %lld uses the value this %s vector reserves for NA"),
				  (long long) (i + 1), uns ? "uint64" : "int64");
			}
		break;
	    }
	    /* the two do not share a representation: check every value */
	    const int64_t *p = i64_data(x);
	    int has_na;
	    int64_t from_na = i64_na_test(x, &has_na);
	    for (R_xlen_t i = 0; i < n; i++) {
		if (has_na && p[i] == from_na) {
		    out[i] = na;
		    na_seen = TRUE;
		}
		else if (uns ? (p[i] < 0)
			 : ((uint64_t) p[i] > (uint64_t) INT64_MAX)) {
		    out[i] = na;
		    warn = TRUE;
		}
		else
		    out[i] = p[i];
	    }
	    break;
	}
	/* fall through */
    default:
	UNPROTECT(1);
	error(_("cannot coerce type '%s' to a 64-bit integer vector"),
	      R_typeToChar(x));
    }

    if (na_seen && !nullable) {
	UNPROTECT(1);
	error(_("cannot store NA in this %s vector: it uses the whole 64-bit range, including the value reserved for NA"),
	      uns ? "uint64" : "int64");
    }

    if (warn) {
	if (!nullable) {
	    UNPROTECT(1);
	    error(_("value out of range, and this %s vector cannot represent NA"),
		  uns ? "uint64" : "int64");
	}
	warning(_("NAs introduced by coercion"));
    }

    UNPROTECT(1);
    return ans;
}

/*
 * ALTREP and ALTVEC methods
 */

static R_xlen_t i64_Length(SEXP x)
{
    return i64_length(x);
}

static Rboolean i64_Inspect(SEXP x, int pre, int deep, int pvec,
			    void (*inspect_subtree)(SEXP, int, int, int))
{
    const int *m = INTEGER(I64_META(x));
    Rprintf(" %s [n=%lld, srt=%d, no_na=%d, nullable=%d]\n",
	    i64_name(x), (long long) i64_length(x),
	    m[I64_SORTED], m[I64_NO_NA], m[I64_NULLABLE_FIELD]);
    return TRUE;
}

/* The payload is the object: no shadow copy, so no coherency problem.  A
   writable pointer invalidates what we cached about the contents. */
static void *i64_Dataptr(SEXP x, Rboolean writable)
{
    if (writable) {
	int *m = INTEGER(I64_META(x));
	m[I64_SORTED] = UNKNOWN_SORTEDNESS;
	m[I64_NO_NA] = 0;
    }
    return DATAPTR_RW(I64_DATA(x));
}

static const void *i64_Dataptr_or_null(SEXP x)
{
    return DATAPTR_RO(I64_DATA(x));
}

static SEXP i64_Format(SEXP x, R_xlen_t i, R_xlen_t n)
{
    R_xlen_t size = i64_length(x);
    R_xlen_t ncopy = i64_ncopy(size, i, n);
    int uns = i64_unsigned(x);
    int has_na;
    int64_t na = i64_na_test(x, &has_na);
    const int64_t *p = i64_data(x);

    SEXP ans = PROTECT(allocVector(STRSXP, ncopy));
    char buf[32];
    for (R_xlen_t k = 0; k < ncopy; k++) {
	if (has_na && p[i + k] == na)
	    SET_STRING_ELT(ans, k, NA_STRING);
	else {
	    if (uns)
		snprintf(buf, sizeof buf, "%llu",
			 (unsigned long long) (uint64_t) p[i + k]);
	    else
		snprintf(buf, sizeof buf, "%lld", (long long) p[i + k]);
	    SET_STRING_ELT(ans, k, mkChar(buf));
	}
    }
    UNPROTECT(1);

    return ans;
}

static SEXP i64_Coerce(SEXP x, int type)
{
    R_xlen_t n = i64_length(x);
    const int64_t *p = i64_data(x);
    int uns = i64_unsigned(x);
    int has_na;
    int64_t na = i64_na_test(x, &has_na);

    switch (type) {
    case STRSXP:
	return i64_Format(x, 0, n);
    case REALSXP: {
	SEXP ans = PROTECT(allocVector(REALSXP, n));
	double *out = REAL(ans);
	for (R_xlen_t k = 0; k < n; k++)
	    out[k] = (has_na && p[k] == na) ? NA_REAL
		: (uns ? (double) (uint64_t) p[k] : (double) p[k]);
	UNPROTECT(1);
	return ans;
    }
    case INTSXP: {
	SEXP ans = PROTECT(allocVector(INTSXP, n));
	int *out = INTEGER(ans);
	int warn = FALSE;
	for (R_xlen_t k = 0; k < n; k++) {
	    int64_t v = p[k];
	    if (has_na && v == na) {
		out[k] = NA_INTEGER;
		continue;
	    }
	    if (uns ? ((uint64_t) v > (uint64_t) INT_MAX)
		: (v > INT_MAX || v <= INT_MIN)) {
		out[k] = NA_INTEGER;
		warn = TRUE;
	    }
	    else
		out[k] = (int) v;
	}
	if (warn)
	    warning(_("NAs introduced by coercion to integer range"));
	UNPROTECT(1);
	return ans;
    }
    case CPLXSXP: {
	SEXP ans = PROTECT(allocVector(CPLXSXP, n));
	Rcomplex *out = COMPLEX(ans);
	for (R_xlen_t k = 0; k < n; k++) {
	    out[k].r = (has_na && p[k] == na) ? NA_REAL
		: (uns ? (double) (uint64_t) p[k] : (double) p[k]);
	    out[k].i = 0.0;
	}
	UNPROTECT(1);
	return ans;
    }
    case LGLSXP: {
	SEXP ans = PROTECT(allocVector(LGLSXP, n));
	int *out = LOGICAL(ans);
	for (R_xlen_t k = 0; k < n; k++)
	    out[k] = (has_na && p[k] == na) ? NA_LOGICAL : (p[k] != 0);
	UNPROTECT(1);
	return ans;
    }
    default:
	return NULL;
    }
}

/*
 * ALTSXP methods: shape
 */

/* The generic ALTSXP state carries element type, length and payload, which
   is enough for a class whose objects differ only in their contents.  These
   two differ in whether they reserve a bit pattern for NA, and that has to
   survive a round trip: restoring a whole-range vector as a nullable one
   would silently turn its extreme value into NA. */
static SEXP i64_Serialized_state(SEXP x)
{
    SEXP state = PROTECT(allocVector(VECSXP, 2));
    SET_VECTOR_ELT(state, 0, I64_DATA(x));
    SET_VECTOR_ELT(state, 1, ScalarLogical(I64_NULLABLE(x)));
    UNPROTECT(1);

    return state;
}

static SEXP i64_Unserialize(SEXP class, SEXP state)
{
    if (TYPEOF(state) != VECSXP || XLENGTH(state) != 2 ||
	TYPEOF(VECTOR_ELT(state, 0)) != RAWSXP)
	error(_("unexpected serialised state for a 64-bit integer vector"));

    SEXP payload = VECTOR_ELT(state, 0);
    R_xlen_t n = XLENGTH(payload) / (R_xlen_t) sizeof(int64_t);

    /* i64_alloc() is passed the class object here rather than an instance;
       see the note on the New method in R_ext/Altrep.h. */
    SEXP ans = PROTECT(i64_alloc(class, n));
    INTEGER(I64_META(ans))[I64_NULLABLE_FIELD] =
	asLogical(VECTOR_ELT(state, 1)) == TRUE;
    if (n > 0)
	memcpy(i64_data(ans), RAW(payload), (size_t) n * sizeof(int64_t));
    UNPROTECT(1);

    return ans;
}

static SEXP i64_Elt_type(SEXP x)
{
    return i64_unsigned(x) ? UInt64Symbol : Int64Symbol;
}

static size_t i64_Elt_size(SEXP x)
{
    return sizeof(int64_t);
}

static SEXP i64_New(SEXP proto, R_xlen_t n)
{
    return i64_alloc(proto, n);
}

static R_xlen_t i64_Set_na_region(SEXP x, R_xlen_t i, R_xlen_t n)
{
    if (!I64_NULLABLE(x))
	error(_("this %s vector cannot represent NA"), i64_name(x));

    R_xlen_t size = i64_length(x);
    R_xlen_t ncopy = i64_ncopy(size, i, n);
    int64_t na = i64_na(x), *p = i64_data(x);

    for (R_xlen_t k = 0; k < ncopy; k++)
	p[i + k] = na;

    return ncopy;
}

/*
 * ALTSXP methods: element semantics
 */

static R_xlen_t i64_Is_na_region(SEXP x, R_xlen_t i, R_xlen_t n, int *buf)
{
    R_xlen_t size = i64_length(x);
    R_xlen_t ncopy = i64_ncopy(size, i, n);
    int has_na;
    int64_t na = i64_na_test(x, &has_na);
    const int64_t *p = i64_data(x);

    for (R_xlen_t k = 0; k < ncopy; k++)
	buf[k] = has_na && p[i + k] == na;

    return ncopy;
}

static int i64_Compare(SEXP x, R_xlen_t i, SEXP y, R_xlen_t j)
{
    return i64_cmp(i64_data(x)[i], i64_data(y)[j], i64_unsigned(x));
}

static unsigned int i64_Traits(SEXP x)
{
    /* NUMERIC: arithmetic is meaningful and is.numeric() is TRUE.
       BITWISE_EQ: equal values have equal bytes, so R may hash and compare
       elements generically -- true of a two's complement integer, and the
       reason match(), unique() and table() need no help from this class. */
    unsigned int t = R_ALTREP_TRAITS_NUMERIC | R_ALTREP_TRAITS_BITWISE_EQ;

    if (!I64_NULLABLE(x))
	t |= R_ALTREP_TRAITS_NOT_NULLABLE;

    return t;
}

/* R asks for this before it must put an NA into a vector whose domain
   excludes NA: growing it, subsetting out of bounds, assigning NA. */
static SEXP i64_Na_widen(SEXP x)
{
    if (I64_NULLABLE(x))
	return x;

    R_xlen_t n = i64_length(x);
    const int64_t *p = i64_data(x);
    int64_t na = i64_na(x);

    for (R_xlen_t i = 0; i < n; i++)
	if (p[i] == na)
	    error(_("cannot introduce NA into this %s vector: it uses the whole 64-bit range, including the value reserved for NA"),
		  i64_name(x));

    SEXP ans = PROTECT(i64_alloc(x, n));
    INTEGER(I64_META(ans))[I64_NULLABLE_FIELD] = TRUE;
    memcpy(i64_data(ans), p, (size_t) n * sizeof(int64_t));
    UNPROTECT(1);

    return ans;
}

/* Promote an ordinary R vector into this class, so that c(x, 1L) and
   x[i] <- 1L work.  Returning NULL declines, and R reports a type error. */
static SEXP i64_Coerce_from(SEXP proto, SEXP from)
{
    switch (TYPEOF(from)) {
    case RAWSXP: case LGLSXP: case INTSXP: case REALSXP: case STRSXP:
	break;
    case ALTSXP:
	/* int64 and uint64 have no common representation; declining here is
	   what makes c(int64, uint64) fall back to a list */
	if (i64_is(from) && i64_unsigned(from) == i64_unsigned(proto))
	    break;
	return NULL;
    default:
	return NULL;
    }

    return i64_from(from, i64_unsigned(proto), I64_NULLABLE(proto));
}

static int i64_Is_sorted(SEXP x)
{
    int *m = INTEGER(I64_META(x));
    if (m[I64_SORTED] != UNKNOWN_SORTEDNESS)
	return m[I64_SORTED];

    R_xlen_t n = i64_length(x);
    const int64_t *p = i64_data(x);
    int has_na;
    int64_t na = i64_na_test(x, &has_na);
    int uns = i64_unsigned(x), incr = TRUE, decr = TRUE;

    for (R_xlen_t i = 0; i < n; i++) {
	if (has_na && p[i] == na)
	    return UNKNOWN_SORTEDNESS;
	if (i > 0) {
	    int c = i64_cmp(p[i], p[i - 1], uns);
	    if (c < 0) incr = FALSE;
	    if (c > 0) decr = FALSE;
	}
    }

    m[I64_SORTED] = incr ? SORTED_INCR : (decr ? SORTED_DECR : KNOWN_UNSORTED);
    return m[I64_SORTED];
}

static int i64_No_NA(SEXP x)
{
    int *m = INTEGER(I64_META(x));
    if (m[I64_NO_NA])
	return TRUE;

    R_xlen_t n = i64_length(x);
    const int64_t *p = i64_data(x);
    int has_na;
    int64_t na = i64_na_test(x, &has_na);

    for (R_xlen_t i = 0; i < n; i++)
	if (has_na && p[i] == na)
	    return FALSE;

    m[I64_NO_NA] = TRUE;
    return TRUE;
}

/*
 * Reductions
 */

static SEXP i64_reduce(SEXP x, Rboolean narm, int what)
{
    R_xlen_t n = i64_length(x);
    const int64_t *p = i64_data(x);
    int has_na, uns = i64_unsigned(x);
    int64_t na = i64_na_test(x, &has_na);

    /* The result keeps the input's NA domain: a whole-range vector must be
       able to report its own extreme value, and in exchange a reduction
       that cannot produce a number is an error rather than an NA -- the
       same trade the arithmetic operators make. */
    SEXP ans = PROTECT(i64_alloc(x, 1));
    int nullable = I64_NULLABLE(ans);
    int64_t nav = i64_na(ans);
    int64_t acc = 0;
    int have = FALSE, overflow = FALSE;

    for (R_xlen_t i = 0; i < n; i++) {
	int64_t v = p[i];
	if (has_na && v == na) {
	    if (narm)
		continue;
	    i64_data(ans)[0] = nav;
	    UNPROTECT(1);
	    return ans;
	}

	if (what == 0) { /* sum */
	    if (!have) {
		acc = v;
		have = TRUE;
	    }
	    else if (uns ? u64_acc(&acc, v) : i64_add(acc, v, &acc)) {
		overflow = TRUE;
		break;
	    }
	}
	else {
	    int want = (what < 0) ? -1 : 1;
	    if (!have) {
		acc = v;
		have = TRUE;
	    }
	    else if (i64_cmp(v, acc, uns) == want)
		acc = v;
	}
    }

    if (!have && what == 0) {
	acc = 0;
	have = TRUE;
    }

    /* a result that lands on the NA pattern is as unrepresentable as one
       that overflowed; only a vector without a pattern can return it */
    if (have && nullable && acc == nav)
	overflow = TRUE;

    if (overflow || !have) {
	if (!nullable) {
	    UNPROTECT(1);
	    if (overflow)
		error(_("64-bit integer overflow, and this %s vector cannot represent NA"),
		      i64_name(x));
	    error(_("no non-missing arguments, and this %s vector cannot represent NA"),
		  i64_name(x));
	}
	if (overflow)
	    warning(_("NAs produced by 64-bit integer overflow"));
	i64_data(ans)[0] = nav;
    }
    else
	i64_data(ans)[0] = acc;

    UNPROTECT(1);
    return ans;
}

static SEXP i64_Sum(SEXP x, Rboolean narm) { return i64_reduce(x, narm, 0); }
static SEXP i64_Min(SEXP x, Rboolean narm) { return i64_reduce(x, narm, -1); }
static SEXP i64_Max(SEXP x, Rboolean narm) { return i64_reduce(x, narm, 1); }

/*
 * Arithmetic, comparison and the Math group
 *
 * These are what let a bare ALTSXP -- one that never had a class attribute,
 * or whose attributes some base function has dropped -- still compute.  An
 * ALTSXP has no base type to fall back on, so without them the object would
 * simply be inert.
 */

SEXP R_binary(SEXP, SEXP, SEXP, SEXP); /* in arithmetic.c */

/* an operand that takes part in exact 64-bit arithmetic */
static int i64_exact_operand(SEXP e)
{
    switch (TYPEOF(e)) {
    case ALTSXP: return i64_is(e);
    case RAWSXP: case LGLSXP: case INTSXP: return TRUE;
    default: return FALSE;
    }
}

static int i64_numeric_operand(SEXP e)
{
    return i64_exact_operand(e) || TYPEOF(e) == REALSXP;
}

/* PROTECT at the call site */
static SEXP i64_as_double(SEXP x)
{
    if (TYPEOF(x) == ALTSXP && i64_is(x))
	return i64_Coerce(x, REALSXP);
    return coerceVector(x, REALSXP);
}

/* A double operand promotes the whole operation, as it does for integers.
   Re-entering R_binary() is safe: neither operand is an ALTSXP any more, so
   the hook at the top of it does not fire again. */
static SEXP i64_double_binop(SEXP call, SEXP opsym, SEXP x, SEXP y)
{
    SEXP a = PROTECT(i64_as_double(x));
    SEXP b = PROTECT(i64_as_double(y));
    SEXP ans = R_binary(call, R_Primitive(CHAR(PRINTNAME(opsym))), a, b);
    UNPROTECT(2);

    return ans;
}

/* Which operation, resolved once rather than per element. */
enum i64_op { I64_ADD, I64_SUB, I64_MUL, I64_IDIV, I64_MOD, I64_NO_OP };

static enum i64_op i64_op_code(const char *op)
{
    if (!strcmp(op, "+"))   return I64_ADD;
    if (!strcmp(op, "-"))   return I64_SUB;
    if (!strcmp(op, "*"))   return I64_MUL;
    if (!strcmp(op, "%/%")) return I64_IDIV;
    if (!strcmp(op, "%%"))  return I64_MOD;
    return I64_NO_OP;
}

static SEXP i64_binary(SEXP call, const char *op, SEXP x, SEXP y, int uns)
{
    enum i64_op code = i64_op_code(op);
    if (code == I64_NO_OP)
	errorcall(call, _("operator '%s' is not defined for %s"),
		  op, uns ? "uint64" : "int64");

    /* An ordinary operand is rendered in the opaque one's NA domain, the
       same choice i64_Coerce_from() makes for c(), pmin() and x[i] <- v.
       When both are opaque each keeps its own and the widening below
       reconciles them. */
    int nullable = i64_is(x) ? I64_NULLABLE(x)
	: (i64_is(y) ? I64_NULLABLE(y) : TRUE);

    SEXP p1, p2;
    PROTECT_INDEX pi1, pi2;
    PROTECT_WITH_INDEX(p1 = i64_materialize(x, uns, nullable), &pi1);
    PROTECT_WITH_INDEX(p2 = i64_materialize(y, uns, nullable), &pi2);
    R_xlen_t nx = i64_length(p1), ny = i64_length(p2);

    if (nx == 0 || ny == 0) {
	SEXP z = i64_alloc(I64_PROTO(uns), 0);
	UNPROTECT(2);
	return z;
    }

    /* The two operands must agree on what the NA pattern means before a
       single loop can read both: otherwise a whole-range operand's extreme
       value would be read as missing, or a missing value as data.  Widening
       is what c() does in the same situation, and reports the same clash. */
    int has_na = I64_NULLABLE(p1) || I64_NULLABLE(p2);
    if (has_na && !I64_NULLABLE(p1))
	REPROTECT(p1 = i64_Na_widen(p1), pi1);
    if (has_na && !I64_NULLABLE(p2))
	REPROTECT(p2 = i64_Na_widen(p2), pi2);

    int64_t nav = uns ? (int64_t) NA_UINT64 : NA_INT64;
    R_xlen_t n = nx > ny ? nx : ny;

    SEXP ans = PROTECT(i64_alloc(I64_PROTO(uns), n));
    INTEGER(I64_META(ans))[I64_NULLABLE_FIELD] = has_na;
    const int64_t *pa = i64_data(p1), *pb = i64_data(p2);
    int64_t *out = i64_data(ans);
    int overflow = FALSE;
    R_xlen_t ia = 0, ib = 0;

    for (R_xlen_t i = 0; i < n; i++, ia++, ib++) {
	if ((i + 1) % NINTERRUPT == 0) R_CheckUserInterrupt();
	if (ia == nx) ia = 0;
	if (ib == ny) ib = 0;

	int64_t a = pa[ia], b = pb[ib], r = 0;
	if (has_na && (a == nav || b == nav)) {
	    out[i] = nav;
	    continue;
	}

	/* base R's integer %% and %/% give NA for a zero divisor and say
	   nothing about it; an object with no NA to fall back on has to
	   report that, rather than blaming an overflow that did not happen */
	if (b == 0 && (code == I64_IDIV || code == I64_MOD)) {
	    if (!has_na) {
		UNPROTECT(3);
		errorcall(call, _("division by zero, and this %s vector cannot represent NA"),
			  uns ? "uint64" : "int64");
	    }
	    out[i] = nav;
	    continue;
	}

	int bad = FALSE;
	if (uns) {
	    uint64_t ua = (uint64_t) a, ub = (uint64_t) b, ur = 0;
	    switch (code) {
	    case I64_ADD:  bad = u64_add(ua, ub, &ur); break;
	    case I64_SUB:  bad = u64_sub(ua, ub, &ur); break;
	    case I64_MUL:  bad = u64_mul(ua, ub, &ur); break;
	    case I64_IDIV: ur = ua / ub; break;
	    default:       ur = ua % ub; break;
	    }
	    r = (int64_t) ur;
	}
	else {
	    switch (code) {
	    case I64_ADD: bad = i64_add(a, b, &r); break;
	    case I64_SUB: bad = i64_sub(a, b, &r); break;
	    case I64_MUL: bad = i64_mul(a, b, &r); break;
	    case I64_IDIV:
		bad = (a == INT64_MIN && b == -1);
		if (!bad) {
		    r = a / b;
		    if (a % b != 0 && (a < 0) != (b < 0)) r--;
		}
		break;
	    default:
		bad = (a == INT64_MIN && b == -1);
		if (!bad) {
		    r = a % b;
		    if (r != 0 && (r < 0) != (b < 0)) r += b;
		}
		break;
	    }
	}

	/* a result equal to the NA pattern is as unrepresentable as one that
	   overflowed, and must not be handed back as data */
	if (!bad && has_na && r == nav)
	    bad = TRUE;

	if (bad) {
	    if (!has_na) {
		UNPROTECT(3);
		errorcall(call, _("64-bit integer overflow, and this %s vector cannot represent NA"),
			  uns ? "uint64" : "int64");
	    }
	    out[i] = nav;
	    overflow = TRUE;
	}
	else
	    out[i] = r;
    }

    if (overflow)
	warning(_("NAs produced by 64-bit integer overflow"));

    UNPROTECT(3);
    return ans;
}

static SEXP i64_unary(SEXP call, const char *op, SEXP x)
{
    if (!strcmp(op, "+"))
	return x;
    if (strcmp(op, "-"))
	return NULL;
    if (i64_unsigned(x))
	errorcall(call, _("unary '%s' is not defined for %s"), "-", "uint64");

    R_xlen_t n = i64_length(x);
    const int64_t *p = i64_data(x);
    int has_na;
    int64_t na = i64_na_test(x, &has_na);

    SEXP ans = PROTECT(i64_alloc(x, n));
    int64_t *out = i64_data(ans);

    for (R_xlen_t i = 0; i < n; i++) {
	int64_t v = p[i];
	if (has_na && v == na)
	    out[i] = na;
	else if (v == INT64_MIN) {
	    /* -INT64_MIN is not representable, and INT64_MIN is only data at
	       all in a vector that gave up its NA, so there is nowhere to put
	       the result */
	    UNPROTECT(1);
	    errorcall(call, _("64-bit integer overflow, and this %s vector cannot represent NA"),
		      "int64");
	}
	else
	    out[i] = -v;
    }

    UNPROTECT(1);
    return ans;
}

static SEXP i64_Arith(SEXP call, SEXP opsym, SEXP x, SEXP y)
{
    const char *op = CHAR(PRINTNAME(opsym));

    if (y == NULL)
	return i64_unary(call, op, x);

    if (i64_is(x) && i64_is(y) && i64_unsigned(x) != i64_unsigned(y))
	errorcall(call, _("cannot mix '%s' and '%s' operands"),
		  "int64", "uint64");

    if (!i64_numeric_operand(x) || !i64_numeric_operand(y))
	return NULL; /* let R report the type error */

    /* A raw byte is exact, and c() and the comparisons take one, but base R
       does not admit raw to arithmetic -- 1L + as.raw(2) is an error -- so
       neither does this class. */
    if (TYPEOF(x) == RAWSXP || TYPEOF(y) == RAWSXP)
	return NULL; /* let R report the type error */

    /* division and powers leave the integers behind, and so does a double
       operand: an exact 64-bit result is only possible between exact
       64-bit operands */
    if (!strcmp(op, "/") || !strcmp(op, "^") ||
	!i64_exact_operand(x) || !i64_exact_operand(y))
	return i64_double_binop(call, opsym, x, y);

    return i64_binary(call, op, x, y,
		      i64_is(x) ? i64_unsigned(x) : i64_unsigned(y));
}

static SEXP i64_Relop(SEXP call, SEXP opsym, SEXP x, SEXP y)
{
    if (i64_is(x) && i64_is(y) && i64_unsigned(x) != i64_unsigned(y))
	errorcall(call, _("cannot mix '%s' and '%s' operands"),
		  "int64", "uint64");

    if (!i64_numeric_operand(x) || !i64_numeric_operand(y))
	return NULL; /* let R report the type error */

    if (!i64_exact_operand(x) || !i64_exact_operand(y)) {
	SEXP a = PROTECT(i64_as_double(x));
	SEXP b = PROTECT(i64_as_double(y));
	SEXP ans = do_relop_dflt(call, R_Primitive(CHAR(PRINTNAME(opsym))),
				 a, b);
	UNPROTECT(2);
	return ans;
    }

    const char *op = CHAR(PRINTNAME(opsym));
    int uns = i64_is(x) ? i64_unsigned(x) : i64_unsigned(y);
    /* resolved once, not once per element */
    enum { REL_EQ, REL_NE, REL_LT, REL_LE, REL_GT, REL_GE } rel;
    if (!strcmp(op, "=="))      rel = REL_EQ;
    else if (!strcmp(op, "!=")) rel = REL_NE;
    else if (!strcmp(op, "<"))  rel = REL_LT;
    else if (!strcmp(op, "<=")) rel = REL_LE;
    else if (!strcmp(op, ">"))  rel = REL_GT;
    else if (!strcmp(op, ">=")) rel = REL_GE;
    else
	errorcall(call, _("operator '%s' is not defined for %s"),
		  op, uns ? "uint64" : "int64");

    /* Unlike i64_binary(), an ordinary operand is rendered as nullable here
       whatever the other side reserves: a comparison builds no opaque
       result whose domain would have to accommodate it, and x == NA has to
       answer NA rather than refuse.  The loop below then reads each operand
       in its own domain, so a whole-range operand keeps its extremes. */
    SEXP p1 = PROTECT(i64_materialize(x, uns, TRUE));
    SEXP p2 = PROTECT(i64_materialize(y, uns, TRUE));
    R_xlen_t nx = i64_length(p1), ny = i64_length(p2);

    if (nx == 0 || ny == 0) {
	UNPROTECT(2);
	return allocVector(LGLSXP, 0);
    }

    /* Each operand is read in its own NA domain: a vector that gave up its
       missing value has data all the way to the extremes, whatever the
       other operand reserves.  Comparison, unlike arithmetic, has no result
       domain to reconcile, so neither operand needs widening. */
    int na1 = I64_NULLABLE(p1), na2 = I64_NULLABLE(p2);
    int64_t nav = uns ? (int64_t) NA_UINT64 : NA_INT64;
    R_xlen_t n = nx > ny ? nx : ny;

    SEXP ans = PROTECT(allocVector(LGLSXP, n));
    const int64_t *pa = i64_data(p1), *pb = i64_data(p2);
    int *out = LOGICAL(ans);
    R_xlen_t ia = 0, ib = 0;

    for (R_xlen_t i = 0; i < n; i++, ia++, ib++) {
	if ((i + 1) % NINTERRUPT == 0) R_CheckUserInterrupt();
	if (ia == nx) ia = 0;
	if (ib == ny) ib = 0;

	int64_t a = pa[ia], b = pb[ib];
	if ((na1 && a == nav) || (na2 && b == nav)) {
	    out[i] = NA_LOGICAL;
	    continue;
	}

	int c = i64_cmp(a, b, uns);
	switch (rel) {
	case REL_EQ: out[i] = c == 0; break;
	case REL_NE: out[i] = c != 0; break;
	case REL_LT: out[i] = c <  0; break;
	case REL_LE: out[i] = c <= 0; break;
	case REL_GT: out[i] = c >  0; break;
	default:     out[i] = c >= 0; break;
	}
    }

    UNPROTECT(3);
    return ans;
}

static SEXP i64_cumulate(SEXP call, const char *op, SEXP x)
{
    /* resolved once rather than per element; i64_Math() sends nothing else */
    enum { CUM_SUM, CUM_MAX, CUM_MIN } code =
	!strcmp(op, "cumsum") ? CUM_SUM
	: (!strcmp(op, "cummax") ? CUM_MAX : CUM_MIN);

    R_xlen_t n = i64_length(x);
    const int64_t *p = i64_data(x);
    int has_na, uns = i64_unsigned(x);
    int64_t na = i64_na_test(x, &has_na);

    /* as in i64_reduce(): the result keeps the input's NA domain, so a
       whole-range vector reports its own extreme value and an overflow
       there is an error rather than a silent NA */
    SEXP ans = PROTECT(i64_alloc(x, n));
    int nullable = I64_NULLABLE(ans);
    int64_t *out = i64_data(ans), acc = 0;
    int seen_na = FALSE, overflow = FALSE;
    int64_t nav = i64_na(ans);

    for (R_xlen_t i = 0; i < n; i++) {
	int64_t v = p[i];
	if (seen_na || (has_na && v == na)) {
	    seen_na = TRUE;
	    out[i] = nav;
	    continue;
	}

	int bad = FALSE;
	if (i == 0)
	    acc = v;
	else switch (code) {
	case CUM_SUM:
	    bad = uns ? u64_acc(&acc, v) : i64_add(acc, v, &acc);
	    break;
	case CUM_MAX:
	    if (i64_cmp(v, acc, uns) > 0) acc = v;
	    break;
	default: /* CUM_MIN */
	    if (i64_cmp(v, acc, uns) < 0) acc = v;
	    break;
	}

	if (!bad && nullable && acc == nav)
	    bad = TRUE; /* the running total landed on the NA pattern */

	if (bad) {
	    if (!nullable) {
		UNPROTECT(1);
		errorcall(call, _("64-bit integer overflow, and this %s vector cannot represent NA"),
			  i64_name(x));
	    }
	    overflow = seen_na = TRUE;
	    out[i] = nav;
	    continue;
	}
	out[i] = acc;
    }

    if (overflow)
	warning(_("NAs produced by 64-bit integer overflow"));

    UNPROTECT(1);
    return ans;
}

static SEXP i64_absolute(SEXP call, const char *op, SEXP x)
{
    R_xlen_t n = i64_length(x);
    const int64_t *p = i64_data(x);
    int has_na, uns = i64_unsigned(x);
    int64_t na = i64_na_test(x, &has_na);
    int sign = !strcmp(op, "sign");

    SEXP ans = PROTECT(i64_alloc(x, n));
    int64_t *out = i64_data(ans);

    for (R_xlen_t i = 0; i < n; i++) {
	int64_t v = p[i];
	if (has_na && v == na)
	    out[i] = na;
	else if (sign)
	    out[i] = uns ? (v != 0) : ((v > 0) - (v < 0));
	else if (uns || v >= 0)
	    out[i] = v;
	else if (v == INT64_MIN) {
	    /* as in i64_unary(): reachable only for a vector with no NA to
	       fall back on */
	    UNPROTECT(1);
	    errorcall(call, _("64-bit integer overflow, and this %s vector cannot represent NA"),
		      "int64");
	}
	else
	    out[i] = -v;
    }

    UNPROTECT(1);
    return ans;
}

/* 10^k as a uint64, or 0 when that is out of range, i.e. for k > 19.  The
   divisor can be wider than the type being rounded: 10^19 is past INT64_MAX
   but still divides an int64 exactly, and the quotient decides whether the
   result is zero or an overflow. */
static uint64_t i64_pow10(int k)
{
    uint64_t p = 1;

    for (int i = 0; i < k; i++) {
	if (p > UINT64_MAX / 10)
	    return 0;
	p *= 10;
    }

    return p;
}

/* round(x, digits) and signif(x, digits) for an exact integer type.  Only a
   negative 'digits' can change a whole number, and R rounds a half to even,
   so this is exact arithmetic rather than a trip through double -- which is
   the point of the type. */
static SEXP i64_round(SEXP call, const char *op, SEXP x, SEXP args)
{
    int is_signif = !strcmp(op, "signif");
    SEXP darg = CADR(args);
    int digits = (darg == R_NilValue || darg == R_MissingArg)
	? (is_signif ? 6 : 0) : asInteger(darg);

    if (digits == NA_INTEGER)
	errorcall(call, _("invalid '%s' argument"), "digits");
    if (is_signif && digits < 1)
	digits = 1; /* as in do_Math2() for the base types */
    if (!is_signif && digits >= 0)
	return x; /* an integer is already rounded to any decimal place */

    R_xlen_t n = i64_length(x);
    const int64_t *p = i64_data(x);
    int has_na, uns = i64_unsigned(x);
    int64_t na = i64_na_test(x, &has_na);

    SEXP ans = PROTECT(i64_alloc(x, n));
    int64_t *out = i64_data(ans);
    int overflow = FALSE;

    for (R_xlen_t i = 0; i < n; i++) {
	int64_t v = p[i];
	if (has_na && v == na) {
	    out[i] = na;
	    continue;
	}

	int k;
	if (is_signif) {
	    /* how many digits to drop to leave 'digits' of them */
	    int nd = 0;
	    for (uint64_t u = uns ? (uint64_t) v
		     : (uint64_t) (v < 0 ? -(uint64_t) v : (uint64_t) v);
		 u != 0; u /= 10)
		nd++;
	    k = nd - digits;
	    if (k <= 0) {
		out[i] = v;
		continue;
	    }
	}
	else
	    k = -digits;

	uint64_t pow = i64_pow10(k);
	if (pow == 0) {
	    /* 10^k is past UINT64_MAX and so more than twice any magnitude
	       this type holds: everything rounds to zero */
	    out[i] = 0;
	    continue;
	}

	/* |v| / pow rounded half to even, then scaled back, all exactly.  The
	   halfway test is a subtraction because 2 * ur overflows once pow is
	   above 2^63. */
	uint64_t uv = uns ? (uint64_t) v
	    : (uint64_t) (v < 0 ? -(uint64_t) v : (uint64_t) v);
	uint64_t uq = uv / pow, ur = uv % pow, ures = 0;
	if (ur > pow - ur || (ur == pow - ur && (uq & 1)))
	    uq++;

	int64_t res = 0;
	int bad = u64_mul(uq, pow, &ures);
	if (!bad) {
	    if (uns)
		res = (int64_t) ures;
	    else if (v < 0) {
		bad = ures > (uint64_t) INT64_MAX + 1; /* -2^63 is a value */
		res = (int64_t) (0 - ures);
	    }
	    else {
		bad = ures > (uint64_t) INT64_MAX;
		res = (int64_t) ures;
	    }
	}

	if (!bad && has_na && res == na)
	    bad = TRUE; /* landed on the value reserved for NA */

	if (bad) {
	    if (!has_na) {
		UNPROTECT(1);
		errorcall(call, _("64-bit integer overflow, and this %s vector cannot represent NA"),
			  i64_name(x));
	    }
	    out[i] = na;
	    overflow = TRUE;
	}
	else
	    out[i] = res;
    }

    if (overflow)
	warning(_("NAs produced by 64-bit integer overflow"));

    UNPROTECT(1);
    return ans;
}

static SEXP i64_Math(SEXP call, SEXP opsym, SEXP args)
{
    const char *op = CHAR(PRINTNAME(opsym));
    SEXP x = CAR(args);

    if (!strcmp(op, "cumsum") || !strcmp(op, "cummax") || !strcmp(op, "cummin"))
	return i64_cumulate(call, op, x);

    if (!strcmp(op, "abs") || !strcmp(op, "sign"))
	return i64_absolute(call, op, x);

    /* an integer is already rounded to any non-negative number of places */
    if (!strcmp(op, "floor") || !strcmp(op, "ceiling") || !strcmp(op, "trunc"))
	return x;

    if (!strcmp(op, "round") || !strcmp(op, "signif"))
	return i64_round(call, op, x, args);

    return NULL;
}

/* What dput() and deparse() emit.  as.int64() and as.uint64() are the only
   constructors these classes have, and the "na" argument matters: a vector
   that gave up its NA is a different object from one that did not, and
   restoring it as nullable would silently turn its extreme value into NA.

   Values are handed over as integer where they all fit -- so the common
   case reads as as.int64(1:3) -- and as character otherwise, because text
   is the only exact form for the rest of the 64-bit range. */
static SEXP i64_Deparse(SEXP x)
{
    R_xlen_t n = i64_length(x);
    const int64_t *p = i64_data(x);
    int has_na, uns = i64_unsigned(x);
    int64_t na = i64_na_test(x, &has_na);

    /* INT_MIN itself is excluded: as an integer literal it would be NA.
       An unsigned value has to be compared as one -- the top half of the
       uint64 range is negative when read as int64. */
    Rboolean small = TRUE;
    for (R_xlen_t i = 0; i < n && small; i++) {
	if (has_na && p[i] == na)
	    continue;
	if (uns ? ((uint64_t) p[i] > (uint64_t) INT_MAX)
	        : (p[i] <= INT_MIN || p[i] > INT_MAX))
	    small = FALSE;
    }

    SEXP arg;
    if (small) {
	arg = PROTECT(allocVector(INTSXP, n));
	int *out = INTEGER(arg);
	for (R_xlen_t i = 0; i < n; i++)
	    out[i] = (has_na && p[i] == na) ? NA_INTEGER : (int) p[i];
    }
    else
	arg = PROTECT(i64_Format(x, 0, n));

    SEXP fun = install(uns ? "as.uint64" : "as.int64");
    SEXP call;
    if (I64_NULLABLE(x))
	call = PROTECT(lang2(fun, arg));
    else {
	call = PROTECT(lang3(fun, arg, ScalarLogical(FALSE)));
	SET_TAG(CDDR(call), install("na"));
    }
    UNPROTECT(2); /* call, arg */

    return call;
}

/*
 * Registration and the R-level constructor
 */

static void InitOne64Class(R_altrep_class_t cls)
{
    R_set_altrep_Length_method(cls, i64_Length);
    R_set_altrep_Inspect_method(cls, i64_Inspect);
    R_set_altrep_Coerce_method(cls, i64_Coerce);
    R_set_altrep_Serialized_state_method(cls, i64_Serialized_state);
    R_set_altrep_Unserialize_method(cls, i64_Unserialize);

    R_set_altvec_Dataptr_method(cls, i64_Dataptr);
    R_set_altvec_Dataptr_or_null_method(cls, i64_Dataptr_or_null);

    R_set_altsxp_Elt_type_method(cls, i64_Elt_type);
    R_set_altsxp_Elt_size_method(cls, i64_Elt_size);
    R_set_altsxp_New_method(cls, i64_New);
    R_set_altsxp_Set_na_region_method(cls, i64_Set_na_region);
    R_set_altsxp_Is_na_region_method(cls, i64_Is_na_region);
    R_set_altsxp_Compare_method(cls, i64_Compare);
    R_set_altsxp_Format_method(cls, i64_Format);
    R_set_altsxp_Arith_method(cls, i64_Arith);
    R_set_altsxp_Relop_method(cls, i64_Relop);
    R_set_altsxp_Traits_method(cls, i64_Traits);
    R_set_altsxp_Coerce_from_method(cls, i64_Coerce_from);
    R_set_altsxp_Na_widen_method(cls, i64_Na_widen);
    R_set_altsxp_Sum_method(cls, i64_Sum);
    R_set_altsxp_Min_method(cls, i64_Min);
    R_set_altsxp_Max_method(cls, i64_Max);
    R_set_altsxp_Is_sorted_method(cls, i64_Is_sorted);
    R_set_altsxp_No_NA_method(cls, i64_No_NA);
    R_set_altsxp_Math_method(cls, i64_Math);
    R_set_altsxp_Deparse_method(cls, i64_Deparse);
}

static void Init64BitIntegerClasses(void)
{
    /* Elt_type must not allocate and must outlive every object, so the
       symbols are installed once here rather than per call */
    Int64Symbol = install("int64");
    UInt64Symbol = install("uint64");

    int64_class = R_make_altsxp_class("int64", "base", NULL);
    uint64_class = R_make_altsxp_class("uint64", "base", NULL);

    InitOne64Class(int64_class);
    InitOne64Class(uint64_class);
}

/* .Internal(as.int64(x, unsigned, na)) -- the one entry point behind
   as.int64(), as.uint64(), int64() and uint64() */
attribute_hidden SEXP do_as_int64(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    checkArity(op, args);

    int uns = asLogical(CADR(args)) == TRUE;
    int nullable = asLogical(CADDR(args)) != FALSE;

    return i64_from(CAR(args), uns, nullable);
}


/**
 ** Initialize ALTREP Classes
 **/

attribute_hidden void R_init_altrep(void)
{
    InitCompactIntegerClass();
    InitCompactRealClass();
    InitDefferredStringClass();
    InitMmapIntegerClass(NULL);
    InitMmapRealClass(NULL);
    InitWrapIntegerClass(NULL);
    InitWrapLogicalClass(NULL);
    InitWrapRealClass(NULL);
    InitWrapComplexClass(NULL);
    InitWrapRawClass(NULL);
    InitWrapStringClass(NULL);
    InitWrapListClass(NULL);
    Init64BitIntegerClasses();
}
