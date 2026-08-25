/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 2016-2024  The R Core Team.
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU Lesser General Public License as published by
 *  the Free Software Foundation; either version 2.1 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU Lesser General Public License for more details.
 *
 *  You should have received a copy of the GNU Lesser General Public License
 *  along with this program; if not, a copy is available at
 *  https://www.R-project.org/Licenses/
 */

/* 
   This API is experimental and may change on short notice.
   Package authors using this API should be prepared to adapt to changes
   when they occur.
*/

#ifndef R_EXT_ALTREP_H_
#define R_EXT_ALTREP_H_

#include <R_ext/Complex.h>

#ifdef  __cplusplus
extern "C" {
#endif

#define STRUCT_SUBTYPES
#ifdef STRUCT_SUBTYPES
# define R_SEXP(x) (x).ptr
# define R_SUBTYPE_INIT(x) { x }
  typedef struct { SEXP ptr; } R_altrep_class_t;
#else
# define R_SEXP(x) ((SEXP) (x))
# define R_SUBTYPE_INIT(x) (void *) (x)
  typedef struct R_altcls *R_altrep_class_t;
#endif

SEXP
R_new_altrep(R_altrep_class_t aclass, SEXP data1, SEXP data2);

R_altrep_class_t
R_make_altstring_class(const char *cname, const char *pname, DllInfo *info);
R_altrep_class_t
R_make_altinteger_class(const char *cname, const char *pname, DllInfo *info);
R_altrep_class_t
R_make_altreal_class(const char *cname, const char *pname, DllInfo *info);
R_altrep_class_t
R_make_altlogical_class(const char *cname, const char *pname, DllInfo *info);
R_altrep_class_t
R_make_altraw_class(const char *cname, const char *pname, DllInfo *info);
R_altrep_class_t
R_make_altcomplex_class(const char *cname, const char *pname, DllInfo *info);
R_altrep_class_t
R_make_altlist_class(const char *cname, const char *pname, DllInfo *info);

/* ALTSXP classes.  An ALTSXP is an opaque vector: TYPEOF() reports ALTSXP
   and says nothing about the element type, which is instead a run-time
   property of the class (see the Elt_type/Elt_size methods below).  Code
   that has not been taught about a particular element type therefore fails
   rather than silently reinterpreting the payload. */
R_altrep_class_t
R_make_altsxp_class(const char *cname, const char *pname, DllInfo *info);

Rboolean R_altrep_inherits(SEXP x, R_altrep_class_t);

typedef SEXP (*R_altrep_UnserializeEX_method_t)(SEXP, SEXP, SEXP, int, int);
typedef SEXP (*R_altrep_Unserialize_method_t)(SEXP, SEXP);
typedef SEXP (*R_altrep_Serialized_state_method_t)(SEXP);
typedef SEXP (*R_altrep_DuplicateEX_method_t)(SEXP, Rboolean);
typedef SEXP (*R_altrep_Duplicate_method_t)(SEXP, Rboolean);
typedef SEXP (*R_altrep_Coerce_method_t)(SEXP, int);
typedef Rboolean (*R_altrep_Inspect_method_t)(SEXP, int, int, int,
					      void (*)(SEXP, int, int, int));
typedef R_xlen_t (*R_altrep_Length_method_t)(SEXP);

typedef void *(*R_altvec_Dataptr_method_t)(SEXP, Rboolean);
typedef const void *(*R_altvec_Dataptr_or_null_method_t)(SEXP);
typedef SEXP (*R_altvec_Extract_subset_method_t)(SEXP, SEXP, SEXP);

typedef int (*R_altinteger_Elt_method_t)(SEXP, R_xlen_t);
typedef R_xlen_t
(*R_altinteger_Get_region_method_t)(SEXP, R_xlen_t, R_xlen_t, int *);
typedef int (*R_altinteger_Is_sorted_method_t)(SEXP);
typedef int (*R_altinteger_No_NA_method_t)(SEXP);
typedef SEXP (*R_altinteger_Sum_method_t)(SEXP, Rboolean); 
typedef SEXP (*R_altinteger_Min_method_t)(SEXP, Rboolean);
typedef SEXP (*R_altinteger_Max_method_t)(SEXP, Rboolean);

typedef double (*R_altreal_Elt_method_t)(SEXP, R_xlen_t);
typedef R_xlen_t
(*R_altreal_Get_region_method_t)(SEXP, R_xlen_t, R_xlen_t, double *);
typedef int (*R_altreal_Is_sorted_method_t)(SEXP);
typedef int (*R_altreal_No_NA_method_t)(SEXP);
typedef SEXP (*R_altreal_Sum_method_t)(SEXP, Rboolean); 
typedef SEXP (*R_altreal_Min_method_t)(SEXP, Rboolean);
typedef SEXP (*R_altreal_Max_method_t)(SEXP, Rboolean);

typedef int (*R_altlogical_Elt_method_t)(SEXP, R_xlen_t);
typedef R_xlen_t
(*R_altlogical_Get_region_method_t)(SEXP, R_xlen_t, R_xlen_t, int *);
typedef int (*R_altlogical_Is_sorted_method_t)(SEXP);
typedef int (*R_altlogical_No_NA_method_t)(SEXP);
typedef SEXP (*R_altlogical_Sum_method_t)(SEXP, Rboolean);

typedef Rbyte (*R_altraw_Elt_method_t)(SEXP, R_xlen_t);
typedef R_xlen_t
(*R_altraw_Get_region_method_t)(SEXP, R_xlen_t, R_xlen_t, Rbyte *);

typedef Rcomplex (*R_altcomplex_Elt_method_t)(SEXP, R_xlen_t);
typedef R_xlen_t
(*R_altcomplex_Get_region_method_t)(SEXP, R_xlen_t, R_xlen_t, Rcomplex *);

typedef SEXP (*R_altstring_Elt_method_t)(SEXP, R_xlen_t);
typedef void (*R_altstring_Set_elt_method_t)(SEXP, R_xlen_t, SEXP);
typedef int (*R_altstring_Is_sorted_method_t)(SEXP);
typedef int (*R_altstring_No_NA_method_t)(SEXP);

typedef SEXP (*R_altlist_Elt_method_t)(SEXP, R_xlen_t);
typedef void (*R_altlist_Set_elt_method_t)(SEXP, R_xlen_t, SEXP);

/* ALTSXP methods.
 *
 * The first group describes the *shape* of the data and is enough for R to
 * implement subsetting, concatenation, duplication and serialisation
 * generically, without knowing what an element means:
 *
 *   Elt_type       an installed symbol naming the C element type, e.g.
 *                  install("int64").  Two classes that report the same
 *                  symbol promise the same in-memory representation, so a
 *                  consumer may cast a data pointer to the matching C type.
 *   Elt_size       sizeof() that C type.
 *   New            allocate a new, uninitialised object of this class with
 *                  the given length.  The first argument is normally an
 *                  existing instance to use as a prototype, but is the class
 *                  object itself when called from the default Unserialize
 *                  method, where no instance exists yet; a class that cares
 *                  can tell the two apart with ALTREP().
 *   Get_region     copy n elements starting at i into buf.
 *   Set_region     copy n elements from buf into positions i..i+n-1.
 *   Set_na_region  set positions i..i+n-1 to the class's NA element.
 *
 * The second group is element-type specific:
 *
 *   Is_na_region   fill buf with 0/1 for positions i..i+n-1.
 *   Compare        three-way compare of x[i] and y[j]; both must have the
 *                  same Elt_type.  NA ordering is the caller's business.
 *   Format         a character vector rendering positions i..i+n-1.
 *   Traits         a bitmask of R_ALTSXP_* below, describing what R may
 *                  assume about the element type.
 *   Coerce_from    build an object of this class from an ordinary R vector,
 *                  or return NULL.  This is what lets c() and x[i] <- v mix
 *                  an opaque vector with base types.
 *   Na_widen       return an object with the same contents whose domain does
 *                  include NA, or NULL if the class has no such form.  R
 *                  calls this before an operation that must introduce NA
 *                  into an object whose traits say it has no NA (growing it,
 *                  subsetting out of bounds, assigning NA into it).
 *   Sum/Min/Max    whole-vector reductions, or NULL to decline.
 *   Is_sorted      one of the SORTED_* constants; UNKNOWN_SORTEDNESS if not
 *                  known.  No_NA is TRUE only if the vector is known to
 *                  contain no NA.
 *   Math           handle a Math-group function (abs, cumsum, round, ...) or
 *                  return NULL to decline.  The third argument is the whole
 *                  argument list, so two-argument members such as round(x, d)
 *                  and signif(x, d) are reachable too.
 *   Arith, Relop   handle an arithmetic or comparison operation, or return
 *                  NULL to decline.  The second argument is the operator as
 *                  an installed symbol ("+", "<", ...); the fourth is NULL
 *                  for a unary operator.  Consulted after S3/S4 group dispatch,
 *                  so a class attribute still wins; without them an ALTSXP
 *                  that has lost its class attribute has no arithmetic at
 *                  all, since there is no base type to fall back on.
 */
typedef SEXP (*R_altsxp_Elt_type_method_t)(SEXP);
typedef size_t (*R_altsxp_Elt_size_method_t)(SEXP);
typedef SEXP (*R_altsxp_New_method_t)(SEXP, R_xlen_t);
typedef R_xlen_t
(*R_altsxp_Get_region_method_t)(SEXP, R_xlen_t, R_xlen_t, void *);
typedef R_xlen_t
(*R_altsxp_Set_region_method_t)(SEXP, R_xlen_t, R_xlen_t, const void *);
typedef R_xlen_t
(*R_altsxp_Set_na_region_method_t)(SEXP, R_xlen_t, R_xlen_t);
typedef R_xlen_t
(*R_altsxp_Is_na_region_method_t)(SEXP, R_xlen_t, R_xlen_t, int *);
typedef int (*R_altsxp_Compare_method_t)(SEXP, R_xlen_t, SEXP, R_xlen_t);
typedef SEXP (*R_altsxp_Format_method_t)(SEXP, R_xlen_t, R_xlen_t);
typedef SEXP (*R_altsxp_Arith_method_t)(SEXP, SEXP, SEXP, SEXP);
typedef SEXP (*R_altsxp_Relop_method_t)(SEXP, SEXP, SEXP, SEXP);
typedef unsigned int (*R_altsxp_Traits_method_t)(SEXP);
typedef SEXP (*R_altsxp_Coerce_from_method_t)(SEXP, SEXP);
typedef SEXP (*R_altsxp_Na_widen_method_t)(SEXP);
typedef SEXP (*R_altsxp_Sum_method_t)(SEXP, Rboolean);
typedef SEXP (*R_altsxp_Min_method_t)(SEXP, Rboolean);
typedef SEXP (*R_altsxp_Max_method_t)(SEXP, Rboolean);
typedef int (*R_altsxp_Is_sorted_method_t)(SEXP);
typedef int (*R_altsxp_No_NA_method_t)(SEXP);
typedef SEXP (*R_altsxp_Math_method_t)(SEXP, SEXP, SEXP);

/* The trait bits themselves are in Rinternals.h, next to the ALTSXP type:

   R_ALTSXP_NUMERIC       is.numeric() is TRUE and arithmetic is meaningful.
   R_ALTSXP_BITWISE_EQ    two *non-NA* elements are equal exactly when their
                          bytes are equal, so R may hash and compare elements
                          generically.  Do not set this for a floating type:
                          NaN and signed zero break it.
   R_ALTSXP_NO_NA_DOMAIN  this object's value domain does not include NA at
                          all, so its whole width is available for data.
                          This is a property of the object, not of the class
                          or of the element type: a column read from a source
                          with no concept of a missing value can set it while
                          a sibling object of the same class does not.  R
                          calls Na_widen() before introducing an NA into such
                          an object.  Note this is about what the object
                          *can* hold; the No_NA method is about what it
                          currently *does* hold. */

#define DECLARE_METHOD_SETTER(CNAME, MNAME)				\
    void								\
    R_set_##CNAME##_##MNAME##_method(R_altrep_class_t cls,		\
				     R_##CNAME##_##MNAME##_method_t fun);

DECLARE_METHOD_SETTER(altrep, UnserializeEX)
DECLARE_METHOD_SETTER(altrep, Unserialize)
DECLARE_METHOD_SETTER(altrep, Serialized_state)
DECLARE_METHOD_SETTER(altrep, DuplicateEX)
DECLARE_METHOD_SETTER(altrep, Duplicate)
DECLARE_METHOD_SETTER(altrep, Coerce)
DECLARE_METHOD_SETTER(altrep, Inspect)
DECLARE_METHOD_SETTER(altrep, Length)

DECLARE_METHOD_SETTER(altvec, Dataptr)
DECLARE_METHOD_SETTER(altvec, Dataptr_or_null)
DECLARE_METHOD_SETTER(altvec, Extract_subset)

DECLARE_METHOD_SETTER(altinteger, Elt)
DECLARE_METHOD_SETTER(altinteger, Get_region)
DECLARE_METHOD_SETTER(altinteger, Is_sorted)
DECLARE_METHOD_SETTER(altinteger, No_NA)
DECLARE_METHOD_SETTER(altinteger, Sum)
DECLARE_METHOD_SETTER(altinteger, Min)
DECLARE_METHOD_SETTER(altinteger, Max)

DECLARE_METHOD_SETTER(altreal, Elt)
DECLARE_METHOD_SETTER(altreal, Get_region)
DECLARE_METHOD_SETTER(altreal, Is_sorted)
DECLARE_METHOD_SETTER(altreal, No_NA)
DECLARE_METHOD_SETTER(altreal, Sum)
DECLARE_METHOD_SETTER(altreal, Min)
DECLARE_METHOD_SETTER(altreal, Max)

DECLARE_METHOD_SETTER(altlogical, Elt)
DECLARE_METHOD_SETTER(altlogical, Get_region)
DECLARE_METHOD_SETTER(altlogical, Is_sorted)
DECLARE_METHOD_SETTER(altlogical, No_NA)
DECLARE_METHOD_SETTER(altlogical, Sum)

DECLARE_METHOD_SETTER(altraw, Elt)
DECLARE_METHOD_SETTER(altraw, Get_region)

DECLARE_METHOD_SETTER(altcomplex, Elt)
DECLARE_METHOD_SETTER(altcomplex, Get_region)

DECLARE_METHOD_SETTER(altstring, Elt)
DECLARE_METHOD_SETTER(altstring, Set_elt)
DECLARE_METHOD_SETTER(altstring, Is_sorted)
DECLARE_METHOD_SETTER(altstring, No_NA)

DECLARE_METHOD_SETTER(altlist, Elt)
DECLARE_METHOD_SETTER(altlist, Set_elt)

DECLARE_METHOD_SETTER(altsxp, Elt_type)
DECLARE_METHOD_SETTER(altsxp, Elt_size)
DECLARE_METHOD_SETTER(altsxp, New)
DECLARE_METHOD_SETTER(altsxp, Get_region)
DECLARE_METHOD_SETTER(altsxp, Set_region)
DECLARE_METHOD_SETTER(altsxp, Set_na_region)
DECLARE_METHOD_SETTER(altsxp, Is_na_region)
DECLARE_METHOD_SETTER(altsxp, Compare)
DECLARE_METHOD_SETTER(altsxp, Format)
DECLARE_METHOD_SETTER(altsxp, Arith)
DECLARE_METHOD_SETTER(altsxp, Relop)
DECLARE_METHOD_SETTER(altsxp, Traits)
DECLARE_METHOD_SETTER(altsxp, Coerce_from)
DECLARE_METHOD_SETTER(altsxp, Na_widen)
DECLARE_METHOD_SETTER(altsxp, Sum)
DECLARE_METHOD_SETTER(altsxp, Min)
DECLARE_METHOD_SETTER(altsxp, Max)
DECLARE_METHOD_SETTER(altsxp, Is_sorted)
DECLARE_METHOD_SETTER(altsxp, No_NA)
DECLARE_METHOD_SETTER(altsxp, Math)

/* ALTSXP consumer API.  ALTSXP_ELT_TYPE() returns R_NilValue for anything
   that is not an ALTSXP, so it is safe to call on an arbitrary SEXP.

   R_altsxp_dataptr_ro() is the misuse-resistant form of DATAPTR_RO(): it
   returns NULL unless the object really is an ALTSXP whose element type is
   `elt_type`, so a caller cannot cast the result to the wrong C type by
   accident.  It also returns NULL if the class cannot supply a contiguous
   pointer, in which case use R_altsxp_get_region(). */
SEXP ALTSXP_ELT_TYPE(SEXP x);
size_t ALTSXP_ELT_SIZE(SEXP x);
unsigned int ALTSXP_TRAITS(SEXP x);
SEXP R_altsxp_coerce_from(SEXP proto, SEXP from);
SEXP R_altsxp_na_widen(SEXP x);

const void *R_altsxp_dataptr_ro(SEXP x, SEXP elt_type);
void *R_altsxp_dataptr_rw(SEXP x, SEXP elt_type);
R_xlen_t R_altsxp_get_region(SEXP x, R_xlen_t i, R_xlen_t n, void *buf);
R_xlen_t R_altsxp_set_region(SEXP x, R_xlen_t i, R_xlen_t n, const void *buf);
R_xlen_t R_altsxp_set_na_region(SEXP x, R_xlen_t i, R_xlen_t n);
R_xlen_t R_altsxp_is_na_region(SEXP x, R_xlen_t i, R_xlen_t n, int *buf);
SEXP R_altsxp_new(SEXP proto, R_xlen_t n);

/* DATAPTR_RW is declared here since it should only be used to
   implement Dataptr methods. */
void *DATAPTR_RW(SEXP);

#ifdef  __cplusplus
}
#endif

#endif /* R_EXT_ALTREP_H_ */
