/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 2016--2023   The R Core Team
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
#include <R_ext/Altrep.h>


/***
 *** ALTREP Abstract Class Framework 
 ***/

/**
 **  ALTREP Class Registry for Serialization
 **/

/* Use ATTRIB field to hold class info. OK since not visible outside. */
#define ALTREP_CLASS_SERIALIZED_CLASS(x) ATTRIB(x)
#define SET_ALTREP_CLASS_SERIALIZED_CLASS(x, csym, psym, stype) \
    SET_ATTRIB(x, list3(csym, psym, stype))
#define ALTREP_SERIALIZED_CLASS_CLSSYM(x) CAR(x)
#define ALTREP_SERIALIZED_CLASS_PKGSYM(x) CADR(x)
#define ALTREP_SERIALIZED_CLASS_TYPE(x) INTEGER0(CADDR(x))[0]
#define ALTREP_OBJECT_CLSSYM(x) ALTREP_SERIALIZED_CLASS_CLSSYM( \
	ALTREP_SERIALIZED_CLASS(x))
#define ALTREP_OBJECT_PKGSYM(x) ALTREP_SERIALIZED_CLASS_PKGSYM( \
	ALTREP_SERIALIZED_CLASS(x))

#define ALTREP_CLASS_BASE_TYPE(x) \
    ALTREP_SERIALIZED_CLASS_TYPE(ALTREP_CLASS_SERIALIZED_CLASS(x))

static SEXP Registry = NULL;

static SEXP LookupClassEntry(SEXP csym, SEXP psym)
{
    for (SEXP chain = CDR(Registry); chain != R_NilValue; chain = CDR(chain))
	if (TAG(CAR(chain)) == csym && CADR(CAR(chain)) == psym)
	    return CAR(chain);
    return NULL;
}

static void
RegisterClass(SEXP class, int type, const char *cname, const char *pname,
	      DllInfo *dll)
{
    PROTECT(class);
    if (Registry == NULL) {
	Registry = CONS(R_NilValue, R_NilValue);
	R_PreserveObject(Registry);
    }

    SEXP csym = install(cname);
    SEXP psym = install(pname);
    SEXP stype = PROTECT(ScalarInteger(type));
    SEXP iptr = R_MakeExternalPtr(dll, R_NilValue, R_NilValue);
    SEXP entry = LookupClassEntry(csym, psym);
    if (entry == NULL) {
	entry = list4(class, psym, stype, iptr);
	SET_TAG(entry, csym);
	SETCDR(Registry, CONS(entry, CDR(Registry)));
    }
    else {
	SETCAR(entry, class);
	SETCAR(CDR(CDR(entry)), stype);
	SETCAR(CDR(CDR(CDR(entry))), iptr);
    }
    SET_ALTREP_CLASS_SERIALIZED_CLASS(class, csym, psym, stype);
    UNPROTECT(2); /* class, stype */
}

static SEXP LookupClass(SEXP csym, SEXP psym)
{
    SEXP entry = LookupClassEntry(csym, psym);
    return entry != NULL ? CAR(entry) : NULL;
}

static void reinit_altrep_class(SEXP sclass);
attribute_hidden void R_reinit_altrep_classes(DllInfo *dll)
{
    for (SEXP chain = CDR(Registry); chain != R_NilValue; chain = CDR(chain)) {
	SEXP entry = CAR(chain);
	SEXP iptr = CAR(CDR(CDR(CDR(entry))));
	if (R_ExternalPtrAddr(iptr) == dll)
	    reinit_altrep_class(CAR(entry));
    }
}


/**
 **  ALTREP Method Tables and Class Objects
 **/

#define ALTREP_ERROR_IN_CLASS(msg, x) do {			\
	error("%s [class: %s, pkg: %s]",			\
	      msg,						\
	      CHAR(PRINTNAME(ALTREP_OBJECT_CLSSYM(x))),		\
	      CHAR(PRINTNAME(ALTREP_OBJECT_PKGSYM(x))));	\
    } while(0)

static void SET_ALTREP_CLASS(SEXP x, SEXP class)
{
    SETALTREP(x, 1);
    SET_TAG(x, class);
}

#define CLASS_METHODS_TABLE(class) STDVEC_DATAPTR(class)
#define GENERIC_METHODS_TABLE(x, class) \
    ((class##_methods_t *) CLASS_METHODS_TABLE(ALTREP_CLASS(x)))

#define ALTREP_METHODS_TABLE(x) GENERIC_METHODS_TABLE(x, altrep)
#define ALTVEC_METHODS_TABLE(x) GENERIC_METHODS_TABLE(x, altvec)
#define ALTINTEGER_METHODS_TABLE(x) GENERIC_METHODS_TABLE(x, altinteger)
#define ALTREAL_METHODS_TABLE(x) GENERIC_METHODS_TABLE(x, altreal)
#define ALTLOGICAL_METHODS_TABLE(x) GENERIC_METHODS_TABLE(x, altlogical)
#define ALTRAW_METHODS_TABLE(x) GENERIC_METHODS_TABLE(x, altraw)
#define ALTCOMPLEX_METHODS_TABLE(x) GENERIC_METHODS_TABLE(x, altcomplex)
#define ALTSTRING_METHODS_TABLE(x) GENERIC_METHODS_TABLE(x, altstring)
#define ALTLIST_METHODS_TABLE(x) GENERIC_METHODS_TABLE(x, altlist)
#define ALTSXP_METHODS_TABLE(x) GENERIC_METHODS_TABLE(x, altsxp)

#define ALTREP_METHODS						\
    R_altrep_UnserializeEX_method_t UnserializeEX;		\
    R_altrep_Unserialize_method_t Unserialize;			\
    R_altrep_Serialized_state_method_t Serialized_state;	\
    R_altrep_DuplicateEX_method_t DuplicateEX;			\
    R_altrep_Duplicate_method_t Duplicate;			\
    R_altrep_Coerce_method_t Coerce;				\
    R_altrep_Inspect_method_t Inspect;				\
    R_altrep_Length_method_t Length

#define ALTVEC_METHODS					\
    ALTREP_METHODS;					\
    R_altvec_Dataptr_method_t Dataptr;			\
    R_altvec_Dataptr_or_null_method_t Dataptr_or_null;	\
    R_altvec_Extract_subset_method_t Extract_subset

#define ALTINTEGER_METHODS				\
    ALTVEC_METHODS;					\
    R_altinteger_Elt_method_t Elt;			\
    R_altinteger_Get_region_method_t Get_region;	\
    R_altinteger_Is_sorted_method_t Is_sorted;		\
    R_altinteger_No_NA_method_t No_NA;			\
    R_altinteger_Sum_method_t Sum ;			\
    R_altinteger_Min_method_t Min;			\
    R_altinteger_Max_method_t Max

#define ALTREAL_METHODS				\
    ALTVEC_METHODS;				\
    R_altreal_Elt_method_t Elt;			\
    R_altreal_Get_region_method_t Get_region;	\
    R_altreal_Is_sorted_method_t Is_sorted;	\
    R_altreal_No_NA_method_t No_NA;		\
    R_altreal_Sum_method_t Sum;			\
    R_altreal_Min_method_t Min;			\
    R_altreal_Max_method_t Max

#define ALTLOGICAL_METHODS			\
    ALTVEC_METHODS;				\
    R_altlogical_Elt_method_t Elt;              \
    R_altlogical_Get_region_method_t Get_region;\
    R_altlogical_Is_sorted_method_t Is_sorted;  \
    R_altlogical_No_NA_method_t No_NA;		\
    R_altlogical_Sum_method_t Sum

#define ALTRAW_METHODS				\
    ALTVEC_METHODS;				\
    R_altraw_Elt_method_t Elt;			\
    R_altraw_Get_region_method_t Get_region

#define ALTCOMPLEX_METHODS			\
    ALTVEC_METHODS;				\
    R_altcomplex_Elt_method_t Elt;              \
    R_altcomplex_Get_region_method_t Get_region

#define ALTSTRING_METHODS			\
    ALTVEC_METHODS;				\
    R_altstring_Elt_method_t Elt;		\
    R_altstring_Set_elt_method_t Set_elt;	\
    R_altstring_Is_sorted_method_t Is_sorted;	\
    R_altstring_No_NA_method_t No_NA

#define ALTLIST_METHODS                         \
    ALTVEC_METHODS;                             \
    R_altlist_Elt_method_t Elt;                 \
    R_altlist_Set_elt_method_t Set_elt

#define ALTSXP_METHODS				\
    ALTVEC_METHODS;					\
    R_altsxp_Elt_type_method_t Elt_type;		\
    R_altsxp_Elt_size_method_t Elt_size;		\
    R_altsxp_New_method_t New;			\
    R_altsxp_Get_region_method_t Get_region;		\
    R_altsxp_Set_region_method_t Set_region;		\
    R_altsxp_Set_na_region_method_t Set_na_region;	\
    R_altsxp_Is_na_region_method_t Is_na_region;	\
    R_altsxp_Compare_method_t Compare;		\
    R_altsxp_Format_method_t Format;			\
    R_altsxp_Arith_method_t Arith;			\
    R_altsxp_Relop_method_t Relop;			\
    R_altsxp_Traits_method_t Traits;			\
    R_altsxp_Coerce_from_method_t Coerce_from;	\
    R_altsxp_Na_widen_method_t Na_widen;		\
    R_altsxp_Sum_method_t Sum;			\
    R_altsxp_Min_method_t Min;			\
    R_altsxp_Max_method_t Max;			\
    R_altsxp_Is_sorted_method_t Is_sorted;		\
    R_altsxp_No_NA_method_t No_NA;			\
    R_altsxp_Math_method_t Math

typedef struct { ALTREP_METHODS; } altrep_methods_t;
typedef struct { ALTVEC_METHODS; } altvec_methods_t;
typedef struct { ALTINTEGER_METHODS; } altinteger_methods_t;
typedef struct { ALTREAL_METHODS; } altreal_methods_t;
typedef struct { ALTLOGICAL_METHODS; } altlogical_methods_t;
typedef struct { ALTRAW_METHODS; } altraw_methods_t;
typedef struct { ALTCOMPLEX_METHODS; } altcomplex_methods_t;
typedef struct { ALTSTRING_METHODS; } altstring_methods_t;
typedef struct { ALTLIST_METHODS; } altlist_methods_t;
typedef struct { ALTSXP_METHODS; } altsxp_methods_t;

/* Macro to extract first element from ... macro argument.
   From Richard Hansen's answer in
   http://stackoverflow.com/questions/5588855/standard-alternative-to-gccs-va-args-trick 
*/
#define DISPATCH_TARGET(...) DISPATCH_TARGET_HELPER(__VA_ARGS__, dummy)
#define DISPATCH_TARGET_HELPER(x, ...) x

#define DO_DISPATCH(type, fun, ...)					\
    type##_METHODS_TABLE(DISPATCH_TARGET(__VA_ARGS__))->fun(__VA_ARGS__)

#define ALTREP_DISPATCH(fun, ...) DO_DISPATCH(ALTREP, fun, __VA_ARGS__)
#define ALTVEC_DISPATCH(fun, ...) DO_DISPATCH(ALTVEC, fun, __VA_ARGS__)
#define ALTINTEGER_DISPATCH(fun, ...) DO_DISPATCH(ALTINTEGER, fun, __VA_ARGS__)
#define ALTREAL_DISPATCH(fun, ...) DO_DISPATCH(ALTREAL, fun, __VA_ARGS__)
#define ALTLOGICAL_DISPATCH(fun, ...) DO_DISPATCH(ALTLOGICAL, fun, __VA_ARGS__)
#define ALTRAW_DISPATCH(fun, ...) DO_DISPATCH(ALTRAW, fun, __VA_ARGS__)
#define ALTCOMPLEX_DISPATCH(fun, ...) DO_DISPATCH(ALTCOMPLEX, fun, __VA_ARGS__)
#define ALTSTRING_DISPATCH(fun, ...) DO_DISPATCH(ALTSTRING, fun, __VA_ARGS__)
#define ALTLIST_DISPATCH(fun, ...) DO_DISPATCH(ALTLIST, fun, __VA_ARGS__)
#define ALTSXP_DISPATCH(fun, ...) DO_DISPATCH(ALTSXP, fun, __VA_ARGS__)


/*
 * Generic ALTREP support
 */

attribute_hidden SEXP ALTREP_COERCE(SEXP x, int type)
{
    return ALTREP_DISPATCH(Coerce, x, type);
}

static SEXP ALTREP_DUPLICATE(SEXP x, Rboolean deep)
{
    return ALTREP_DISPATCH(Duplicate, x, deep);
}

attribute_hidden SEXP ALTREP_DUPLICATE_EX(SEXP x, Rboolean deep)
{
    return ALTREP_DISPATCH(DuplicateEX, x, deep);
}

attribute_hidden Rboolean
ALTREP_INSPECT(SEXP x, int pre, int deep, int pvec,
	       void (*inspect_subtree)(SEXP, int, int, int))
{
    return ALTREP_DISPATCH(Inspect, x, pre, deep, pvec, inspect_subtree);
}


attribute_hidden SEXP
ALTREP_SERIALIZED_STATE(SEXP x)
{
    return ALTREP_DISPATCH(Serialized_state, x);
}

attribute_hidden SEXP
ALTREP_SERIALIZED_CLASS(SEXP x)
{
    SEXP val = ALTREP_CLASS_SERIALIZED_CLASS(ALTREP_CLASS(x));
    return val != R_NilValue ? val : NULL;
}

static SEXP find_namespace(void *data) { return R_FindNamespace((SEXP) data); }
static SEXP handle_namespace_error(SEXP cond, void *data) { return R_NilValue; }

static SEXP ALTREP_UNSERIALIZE_CLASS(SEXP info)
{
    if (TYPEOF(info) == LISTSXP) {
	SEXP csym = ALTREP_SERIALIZED_CLASS_CLSSYM(info);
	SEXP psym = ALTREP_SERIALIZED_CLASS_PKGSYM(info);
	SEXP class = LookupClass(csym, psym);
	if (class == NULL) {
	    SEXP pname = ScalarString(PRINTNAME(psym));
	    PROTECT(pname);
	    R_tryCatchError(find_namespace, pname,
			    handle_namespace_error, NULL);
	    class = LookupClass(csym, psym);
	    UNPROTECT(1);
	}
	return class;
    }
    return NULL;
}

attribute_hidden SEXP
ALTREP_UNSERIALIZE_EX(SEXP info, SEXP state, SEXP attr, int objf, int levs)
{
    SEXP csym = ALTREP_SERIALIZED_CLASS_CLSSYM(info);
    SEXP psym = ALTREP_SERIALIZED_CLASS_PKGSYM(info);
    int type = ALTREP_SERIALIZED_CLASS_TYPE(info);

    /* look up the class in the registry and handle failure */
    SEXP class = ALTREP_UNSERIALIZE_CLASS(info);
    if (class == NULL) {
	switch(type) {
	case LGLSXP:
	case INTSXP:
	case REALSXP:
	case CPLXSXP:
	case STRSXP:
	case RAWSXP:
	case VECSXP:
	case EXPRSXP:
	    warning("cannot unserialize ALTVEC object of class '%s' from "
		    "package '%s'; returning length zero vector",
		    CHAR(PRINTNAME(csym)), CHAR(PRINTNAME(psym)));
	    return allocVector(type, 0);
	default:
	    error("cannot unserialize this ALTREP object");
	}
    }

    /* check the registered and unserialized types match */
    int rtype = ALTREP_CLASS_BASE_TYPE(class);
    if (type != rtype)
	warning("serialized class '%s' from package '%s' has type %s; "
		"registered class has type %s",
		CHAR(PRINTNAME(csym)), CHAR(PRINTNAME(psym)),
		type2char(type), type2char(rtype));
    
    /* dispatch to a class method */
    altrep_methods_t *m = CLASS_METHODS_TABLE(class);
    SEXP val = m->UnserializeEX(class, state, attr, objf, levs);
    return val;
}

/*attribute_hidden*/ R_xlen_t ALTREP_LENGTH(SEXP x)
{
    return ALTREP_DISPATCH(Length, x);
}

attribute_hidden R_xlen_t ALTREP_TRUELENGTH(SEXP x) { return 0; }


/*
 * Generic ALTVEC support
 */

static R_INLINE void *ALTVEC_DATAPTR_EX(SEXP x, Rboolean writable)
{
    /* Disallow taking the writable `DATAPTR()` of an ALTLIST. This
       check could be moved to `DATAPTR()` to catch more faulty
       usages. */
    if (TYPEOF(x) == VECSXP && writable)
        ALTREP_ERROR_IN_CLASS("cannot take a writable DATAPTR of an ALTLIST",
			      x);

    /**** move GC disabling into methods? */
    if (R_in_gc)
	error("cannot get ALTVEC DATAPTR during GC");
    R_CHECK_THREAD;
    int enabled = R_GCEnabled;
    R_GCEnabled = FALSE;

    void *val = ALTVEC_DISPATCH(Dataptr, x, writable);

    R_GCEnabled = enabled;
    return val;
}

/*attribute_hidden*/ void *ALTVEC_DATAPTR(SEXP x)
{
    return ALTVEC_DATAPTR_EX(x, TRUE);
}

/*attribute_hidden*/ const void *ALTVEC_DATAPTR_RO(SEXP x)
{
    return ALTVEC_DATAPTR_EX(x, FALSE);
}

attribute_hidden const void *ALTVEC_DATAPTR_OR_NULL(SEXP x)
{
    return ALTVEC_DISPATCH(Dataptr_or_null, x);
}

attribute_hidden SEXP ALTVEC_EXTRACT_SUBSET(SEXP x, SEXP indx, SEXP call)
{
    return ALTVEC_DISPATCH(Extract_subset, x, indx, call);
}


/*
 * Typed ALTVEC support
 */

attribute_hidden int ALTINTEGER_ELT(SEXP x, R_xlen_t i)
{
    return ALTINTEGER_DISPATCH(Elt, x, i);
}

R_xlen_t INTEGER_GET_REGION(SEXP sx, R_xlen_t i, R_xlen_t n, int *buf)
{
    const int *x = INTEGER_OR_NULL(sx);
    if (x != NULL) {
	R_xlen_t size = XLENGTH(sx);
	R_xlen_t ncopy = size - i > n ? n : size - i;
	for (R_xlen_t k = 0; k < ncopy; k++)
	    buf[k] = x[k + i];
	//memcpy(buf, x + i, ncopy * sizeof(int));
	return ncopy;
    }
    else
	return ALTINTEGER_DISPATCH(Get_region, sx, i, n, buf);
}

int INTEGER_IS_SORTED(SEXP x)
{
    return ALTREP(x) ? ALTINTEGER_DISPATCH(Is_sorted, x) : UNKNOWN_SORTEDNESS;
}

int INTEGER_NO_NA(SEXP x)
{
    return ALTREP(x) ? ALTINTEGER_DISPATCH(No_NA, x) : 0;
}

attribute_hidden double ALTREAL_ELT(SEXP x, R_xlen_t i)
{
    return ALTREAL_DISPATCH(Elt, x, i);
}

R_xlen_t REAL_GET_REGION(SEXP sx, R_xlen_t i, R_xlen_t n, double *buf)
{
    const double *x = REAL_OR_NULL(sx);
    if (x != NULL) {
	R_xlen_t size = XLENGTH(sx);
	R_xlen_t ncopy = size - i > n ? n : size - i;
	for (R_xlen_t k = 0; k < ncopy; k++)
	    buf[k] = x[k + i];
	//memcpy(buf, x + i, ncopy * sizeof(double));
	return ncopy;
    }
    else
	return ALTREAL_DISPATCH(Get_region, sx, i, n, buf);
}

int REAL_IS_SORTED(SEXP x)
{
    return ALTREP(x) ? ALTREAL_DISPATCH(Is_sorted, x) : UNKNOWN_SORTEDNESS;
}

int REAL_NO_NA(SEXP x)
{
    return ALTREP(x) ? ALTREAL_DISPATCH(No_NA, x) : 0;
}

R_xlen_t LOGICAL_GET_REGION(SEXP sx, R_xlen_t i, R_xlen_t n, int *buf)
{
    const int *x = DATAPTR_OR_NULL(sx);
    if (x != NULL) {
	R_xlen_t size = XLENGTH(sx);
	R_xlen_t ncopy = size - i > n ? n : size - i;
	for (R_xlen_t k = 0; k < ncopy; k++)
	    buf[k] = x[k + i];
	//memcpy(buf, x + i, ncopy * sizeof(int));
	return ncopy;
    }
    else
	return ALTLOGICAL_DISPATCH(Get_region, sx, i, n, buf);
}

attribute_hidden int LOGICAL_IS_SORTED(SEXP x)
{
    return ALTREP(x) ? ALTLOGICAL_DISPATCH(Is_sorted, x) : UNKNOWN_SORTEDNESS;
}


int LOGICAL_NO_NA(SEXP x)
{
    return ALTREP(x) ? ALTLOGICAL_DISPATCH(No_NA, x) : 0;
}


R_xlen_t RAW_GET_REGION(SEXP sx, R_xlen_t i, R_xlen_t n, Rbyte *buf)
{
    const Rbyte *x = DATAPTR_OR_NULL(sx);
    if (x != NULL) {
	R_xlen_t size = XLENGTH(sx);
	R_xlen_t ncopy = size - i > n ? n : size - i;
	for (R_xlen_t k = 0; k < ncopy; k++)
	    buf[k] = x[k + i];
	//memcpy(buf, x + i, ncopy * sizeof(int));
	return ncopy;
    }
    else
	return ALTRAW_DISPATCH(Get_region, sx, i, n, buf);
}


R_xlen_t COMPLEX_GET_REGION(SEXP sx, R_xlen_t i, R_xlen_t n, Rcomplex *buf)
{
    const Rcomplex *x = DATAPTR_OR_NULL(sx);
    if (x != NULL) {
	R_xlen_t size = XLENGTH(sx);
	R_xlen_t ncopy = size - i > n ? n : size - i;
	for (R_xlen_t k = 0; k < ncopy; k++)
	    buf[k] = x[k + i];
	//memcpy(buf, x + i, ncopy * sizeof(int));
	return ncopy;
    }
    else
	return ALTCOMPLEX_DISPATCH(Get_region, sx, i, n, buf);
}


SEXP /*attribute_hidden*/ ALTSTRING_ELT(SEXP x, R_xlen_t i)
{
    SEXP val = NULL;

    /**** move GC disabling into method? */
    if (R_in_gc)
	error("cannot get ALTSTRING_ELT during GC");
    R_CHECK_THREAD;
    int enabled = R_GCEnabled;
    R_GCEnabled = FALSE;

    val = ALTSTRING_DISPATCH(Elt, x, i);

    R_GCEnabled = enabled;
    return val;
}

attribute_hidden void ALTSTRING_SET_ELT(SEXP x, R_xlen_t i, SEXP v)
{
    /**** move GC disabling into method? */
    if (R_in_gc)
	error("cannot set ALTSTRING_ELT during GC");
    R_CHECK_THREAD;
    int enabled = R_GCEnabled;
    R_GCEnabled = FALSE;

    ALTSTRING_DISPATCH(Set_elt, x, i, v);

    R_GCEnabled = enabled;
}

int STRING_IS_SORTED(SEXP x)
{
    return ALTREP(x) ? ALTSTRING_DISPATCH(Is_sorted, x) : UNKNOWN_SORTEDNESS;
}

int STRING_NO_NA(SEXP x)
{
    return ALTREP(x) ? ALTSTRING_DISPATCH(No_NA, x) : 0;
}

attribute_hidden SEXP ALTLIST_ELT(SEXP x, R_xlen_t i)
{
    SEXP val = NULL;

    /**** move GC disabling into method? */
    if (R_in_gc)
	error("cannot get ALTLIST_ELT during GC");
    R_CHECK_THREAD;
    int enabled = R_GCEnabled;
    R_GCEnabled = FALSE;

    val = ALTLIST_DISPATCH(Elt, x, i);

    R_GCEnabled = enabled;
    return val;
}

attribute_hidden void ALTLIST_SET_ELT(SEXP x, R_xlen_t i, SEXP v)
{
    /**** move GC disabling into method? */
    if (R_in_gc)
	error("cannot set ALTLIST_ELT during GC");
    R_CHECK_THREAD;
    int enabled = R_GCEnabled;
    R_GCEnabled = FALSE;

    ALTLIST_DISPATCH(Set_elt, x, i, v);

    R_GCEnabled = enabled;
}

attribute_hidden SEXP ALTINTEGER_SUM(SEXP x, Rboolean narm)
{
    return ALTINTEGER_DISPATCH(Sum, x, narm);
}

attribute_hidden SEXP ALTINTEGER_MIN(SEXP x, Rboolean narm)
{
    return ALTINTEGER_DISPATCH(Min, x, narm);
}

attribute_hidden SEXP ALTINTEGER_MAX(SEXP x, Rboolean narm)
{
    return ALTINTEGER_DISPATCH(Max, x, narm);

}

attribute_hidden SEXP ALTREAL_SUM(SEXP x, Rboolean narm)
{
    return ALTREAL_DISPATCH(Sum, x, narm);
}

attribute_hidden SEXP ALTREAL_MIN(SEXP x, Rboolean narm)
{
    return ALTREAL_DISPATCH(Min, x, narm);
}

attribute_hidden SEXP ALTREAL_MAX(SEXP x, Rboolean narm)
{
    return ALTREAL_DISPATCH(Max, x, narm);

}

attribute_hidden SEXP ALTLOGICAL_SUM(SEXP x, Rboolean narm)
{
    return ALTLOGICAL_DISPATCH(Sum, x, narm);
}

attribute_hidden int ALTLOGICAL_ELT(SEXP x, R_xlen_t i)
{
    return ALTLOGICAL_DISPATCH(Elt, x, i);
}

attribute_hidden Rcomplex ALTCOMPLEX_ELT(SEXP x, R_xlen_t i)
{
    return ALTCOMPLEX_DISPATCH(Elt, x, i);
}

attribute_hidden Rbyte ALTRAW_ELT(SEXP x, R_xlen_t i)
{
    return ALTRAW_DISPATCH(Elt, x, i);
}


/*
 * ALTSXP (opaque vector) support
 */

#define IS_ALTSXP(x) (TYPEOF(x) == ALTSXP && ALTREP(x))

SEXP ALTSXP_ELT_TYPE(SEXP x)
{
    return IS_ALTSXP(x) ? ALTSXP_DISPATCH(Elt_type, x) : R_NilValue;
}

size_t ALTSXP_ELT_SIZE(SEXP x)
{
    if (! IS_ALTSXP(x))
	error("%s can only be applied to an ALTSXP object", "ALTSXP_ELT_SIZE");
    return ALTSXP_DISPATCH(Elt_size, x);
}

/* Allocate a vector of the same kind as proto: an ordinary vector of the
   same SEXPTYPE, or -- because an ALTSXP cannot be allocated from its type
   alone -- a new object of proto's own ALTSXP class.  This is the shape that
   generic code wants when it is building a result "like" its input. */
SEXP R_allocVectorLike(SEXP proto, R_xlen_t n)
{
    if (TYPEOF(proto) == ALTSXP)
	return R_altsxp_new(proto, n);
    return allocVector(TYPEOF(proto), n);
}

SEXP R_altsxp_new(SEXP proto, R_xlen_t n)
{
    if (! IS_ALTSXP(proto))
	error("%s can only be applied to an ALTSXP object", "R_altsxp_new");
    return ALTSXP_DISPATCH(New, proto, n);
}

R_xlen_t R_altsxp_get_region(SEXP x, R_xlen_t i, R_xlen_t n, void *buf)
{
    if (! IS_ALTSXP(x))
	error("%s can only be applied to an ALTSXP object",
	      "R_altsxp_get_region");
    return ALTSXP_DISPATCH(Get_region, x, i, n, buf);
}

R_xlen_t R_altsxp_set_region(SEXP x, R_xlen_t i, R_xlen_t n, const void *buf)
{
    if (! IS_ALTSXP(x))
	error("%s can only be applied to an ALTSXP object",
	      "R_altsxp_set_region");
    return ALTSXP_DISPATCH(Set_region, x, i, n, buf);
}

R_xlen_t R_altsxp_set_na_region(SEXP x, R_xlen_t i, R_xlen_t n)
{
    if (! IS_ALTSXP(x))
	error("%s can only be applied to an ALTSXP object",
	      "R_altsxp_set_na_region");
    return ALTSXP_DISPATCH(Set_na_region, x, i, n);
}

R_xlen_t R_altsxp_is_na_region(SEXP x, R_xlen_t i, R_xlen_t n, int *buf)
{
    if (! IS_ALTSXP(x))
	error("%s can only be applied to an ALTSXP object",
	      "R_altsxp_is_na_region");
    return ALTSXP_DISPATCH(Is_na_region, x, i, n, buf);
}

attribute_hidden int ALTSXP_COMPARE(SEXP x, R_xlen_t i, SEXP y, R_xlen_t j)
{
    if (! IS_ALTSXP(x) || ! IS_ALTSXP(y))
	error("ALTSXP comparison needs two ALTSXP objects");
    if (ALTSXP_ELT_TYPE(x) != ALTSXP_ELT_TYPE(y))
	error("cannot compare ALTSXP objects with different element types");
    return ALTSXP_DISPATCH(Compare, x, i, y, j);
}

attribute_hidden SEXP ALTSXP_FORMAT(SEXP x, R_xlen_t i, R_xlen_t n)
{
    return IS_ALTSXP(x) ? ALTSXP_DISPATCH(Format, x, i, n) : NULL;
}

/* R_binary() normally applies dim/dimnames/names to its result; a class hook
   bypasses that, so do the common part here. */
static void altsxp_binop_attribs(SEXP ans, SEXP x, SEXP y)
{
    if (ans == NULL || ans == R_NilValue) return;
    if (ATTRIB(x) == R_NilValue && (y == NULL || ATTRIB(y) == R_NilValue))
	return;

    PROTECT(ans);
    SEXP dims = R_NilValue, dnms = R_NilValue;
    if (isArray(x) && (y == NULL || !isArray(y) || xlength(y) <= xlength(x))) {
	dims = getAttrib(x, R_DimSymbol);
	dnms = getAttrib(x, R_DimNamesSymbol);
    }
    else if (y != NULL && isArray(y)) {
	dims = getAttrib(y, R_DimSymbol);
	dnms = getAttrib(y, R_DimNamesSymbol);
    }

    if (dims != R_NilValue) {
	setAttrib(ans, R_DimSymbol, dims);
	if (dnms != R_NilValue)
	    setAttrib(ans, R_DimNamesSymbol, dnms);
    }
    else {
	R_xlen_t n = xlength(ans);
	SEXP nms = getAttrib(x, R_NamesSymbol);
	if ((nms == R_NilValue || xlength(nms) != n) && y != NULL)
	    nms = getAttrib(y, R_NamesSymbol);
	if (nms != R_NilValue && xlength(nms) == n)
	    setAttrib(ans, R_NamesSymbol, nms);
    }
    UNPROTECT(1);
}

/* Try the left operand's method, then the right one's.  A class that does
   not recognise the other operand returns NULL and the caller carries on to
   its ordinary error. */
attribute_hidden SEXP ALTSXP_ARITH(SEXP call, SEXP op, SEXP x, SEXP y)
{
    /* Methods are handed the operator as a symbol rather than the PRIMSXP,
       which packages have no supported way to inspect. */
    SEXP sym = install(PRIMNAME(op));
    SEXP val = NULL;
    if (IS_ALTSXP(x))
	val = ALTSXP_METHODS_TABLE(x)->Arith(call, sym, x, y);
    if (val == NULL && y != NULL && IS_ALTSXP(y))
	val = ALTSXP_METHODS_TABLE(y)->Arith(call, sym, x, y);
    altsxp_binop_attribs(val, x, y);
    return val;
}

/* An opaque class renders one element at a time, so the strings come back
   unpadded.  format() and print() both want the numeric look -- every
   element right-justified in a common width, NA spelled out rather than
   left as NA_STRING -- which is what formatting an integer vector gives.
   as.character() wants neither, so this is not the Format method's job. */
attribute_hidden SEXP R_altsxp_format_common(SEXP fmt, Rboolean trim)
{
    R_xlen_t n = XLENGTH(fmt);
    int w = 0;

    for (R_xlen_t i = 0; i < n; i++) {
	SEXP e = STRING_ELT(fmt, i);
	int wi = (e == NA_STRING) ? 2 : Rstrlen(e, 0);
	if (wi > w) w = wi;
    }
    if (trim) w = 0;

    SEXP ans = PROTECT(allocVector(STRSXP, n));
    const void *vmax = vmaxget();
    for (R_xlen_t i = 0; i < n; i++) {
	SEXP e = STRING_ELT(fmt, i);
	const char *s = (e == NA_STRING) ? "NA" : CHAR(e);
	int wi = (e == NA_STRING) ? 2 : Rstrlen(e, 0);

	if (wi >= w)
	    SET_STRING_ELT(ans, i, (e == NA_STRING) ? mkChar(s) : e);
	else {
	    char *buf = R_alloc((size_t) w + strlen(s) + 1, 1);
	    snprintf(buf, (size_t) w + strlen(s) + 1, "%*s%s", w - wi, "", s);
	    SET_STRING_ELT(ans, i, mkChar(buf));
	}
    }
    vmaxset(vmax);
    UNPROTECT(1);

    return ans;
}

unsigned int ALTREP_TRAITS(SEXP x)
{
    return IS_ALTSXP(x) ? ALTSXP_DISPATCH(Traits, x) : 0;
}

SEXP R_altsxp_coerce_from(SEXP proto, SEXP from)
{
    if (! IS_ALTSXP(proto))
	error("%s can only be applied to an ALTSXP object",
	      "R_altsxp_coerce_from");
    return ALTSXP_DISPATCH(Coerce_from, proto, from);
}

/* Can x hold NA?  True for every ordinary vector, and for any ALTSXP that
   has not deliberately given up its NA.  The trait is stated negatively so
   that an empty mask -- what an ordinary vector and a class that declares no
   traits both produce -- already means nullable. */
Rboolean R_altsxp_nullable(SEXP x)
{
    return (ALTREP_TRAITS(x) & R_ALTREP_TRAITS_NOT_NULLABLE) ? FALSE : TRUE;
}

/* An object that cannot be NA must be widened before R can put an NA in it.
   Returns x unchanged when it already accepts NA, and NULL when the class
   offers no NA-capable form. */
SEXP R_altsxp_na_widen(SEXP x)
{
    if (R_altsxp_nullable(x))
	return x;
    return ALTSXP_DISPATCH(Na_widen, x);
}

attribute_hidden SEXP ALTSXP_SUM(SEXP x, Rboolean narm)
{
    return IS_ALTSXP(x) ? ALTSXP_DISPATCH(Sum, x, narm) : NULL;
}

attribute_hidden SEXP ALTSXP_MIN(SEXP x, Rboolean narm)
{
    return IS_ALTSXP(x) ? ALTSXP_DISPATCH(Min, x, narm) : NULL;
}

attribute_hidden SEXP ALTSXP_MAX(SEXP x, Rboolean narm)
{
    return IS_ALTSXP(x) ? ALTSXP_DISPATCH(Max, x, narm) : NULL;
}

attribute_hidden int ALTSXP_IS_SORTED(SEXP x)
{
    return IS_ALTSXP(x) ? ALTSXP_DISPATCH(Is_sorted, x)
			   : UNKNOWN_SORTEDNESS;
}

attribute_hidden int ALTSXP_NO_NA(SEXP x)
{
    return IS_ALTSXP(x) ? ALTSXP_DISPATCH(No_NA, x) : FALSE;
}

attribute_hidden SEXP ALTSXP_MATH(SEXP call, SEXP op, SEXP args)
{
    SEXP x = CAR(args);
    if (! IS_ALTSXP(x)) return NULL;
    return ALTSXP_METHODS_TABLE(x)->Math(call, install(PRIMNAME(op)), args);
}

attribute_hidden SEXP ALTSXP_RELOP(SEXP call, SEXP op, SEXP x, SEXP y)
{
    SEXP sym = install(PRIMNAME(op));
    SEXP val = NULL;
    if (IS_ALTSXP(x))
	val = ALTSXP_METHODS_TABLE(x)->Relop(call, sym, x, y);
    if (val == NULL && y != NULL && IS_ALTSXP(y))
	val = ALTSXP_METHODS_TABLE(y)->Relop(call, sym, x, y);
    altsxp_binop_attribs(val, x, y);
    return val;
}

/* The point of the element type symbol: a consumer that does not know the
   class can still recognise the representation and cast safely. */
const void *R_altsxp_dataptr_ro(SEXP x, SEXP elt_type)
{
    if (! IS_ALTSXP(x) || ALTSXP_DISPATCH(Elt_type, x) != elt_type)
	return NULL;
    return ALTVEC_DATAPTR_OR_NULL(x);
}

void *R_altsxp_dataptr_rw(SEXP x, SEXP elt_type)
{
    if (! IS_ALTSXP(x) || ALTSXP_DISPATCH(Elt_type, x) != elt_type)
	return NULL;
    return ALTVEC_DATAPTR(x);
}


/*
 * Not yet implemented
 */

attribute_hidden void ALTINTEGER_SET_ELT(SEXP x, R_xlen_t i, int v)
{
    INTEGER(x)[i] = v; /* dispatch here */
}

attribute_hidden void ALTLOGICAL_SET_ELT(SEXP x, R_xlen_t i, int v)
{
    LOGICAL(x)[i] = v; /* dispatch here */
}

attribute_hidden void ALTREAL_SET_ELT(SEXP x, R_xlen_t i, double v)
{
    REAL(x)[i] = v; /* dispatch here */
}

attribute_hidden void ALTCOMPLEX_SET_ELT(SEXP x, R_xlen_t i, Rcomplex v)
{
    COMPLEX(x)[i] = v; /* dispatch here */
}

attribute_hidden void ALTRAW_SET_ELT(SEXP x, R_xlen_t i, Rbyte v)
{
    RAW(x)[i] = v; /* dispatch here */
}


/**
 ** ALTREP Default Methods
 **/

static SEXP altrep_UnserializeEX_default(SEXP class, SEXP state, SEXP attr,
					 int objf, int levs)
{
    altrep_methods_t *m = CLASS_METHODS_TABLE(class);
    SEXP val = m->Unserialize(class, state);
    SET_ATTRIB(val, attr);
    SET_OBJECT(val, objf);
    SETLEVELS(val, levs);
    return val;
}

static SEXP altrep_Serialized_state_default(SEXP x) { return NULL; }

static SEXP altrep_Unserialize_default(SEXP class, SEXP state)
{
    error("cannot unserialize this ALTREP object yet");
}

static SEXP altrep_Coerce_default(SEXP x, int type) { return NULL; }

static SEXP altrep_Duplicate_default(SEXP x, Rboolean deep)
{
    return NULL;
}

static SEXP altrep_DuplicateEX_default(SEXP x, Rboolean deep)
{
    SEXP ans = ALTREP_DUPLICATE(x, deep);

    if (ans != NULL &&
	ans != x) { /* leave attributes alone if returning original */
	/* handle attributes generically */
	SEXP attr = ATTRIB(x);
	if (attr != R_NilValue) {
	    PROTECT(ans);
	    SET_ATTRIB(ans, deep ? duplicate(attr) : shallow_duplicate(attr));
	    SET_OBJECT(ans, OBJECT(x));
	    IS_S4_OBJECT(x) ? SET_S4_OBJECT(ans) : UNSET_S4_OBJECT(ans);
	    UNPROTECT(1);
	}
	else if (ATTRIB(ans) != R_NilValue) {
	    SET_ATTRIB(ans, R_NilValue);
	    SET_OBJECT(ans, FALSE);
	    UNSET_S4_OBJECT(ans);
	}
    }
    return ans;
}

static
Rboolean altrep_Inspect_default(SEXP x, int pre, int deep, int pvec,
				void (*inspect_subtree)(SEXP, int, int, int))
{
    return FALSE;
}

static R_xlen_t altrep_Length_default(SEXP x)
{
    ALTREP_ERROR_IN_CLASS("no ALTREP Length method defined", x);
}

static void *altvec_Dataptr_default(SEXP x, Rboolean writable)
{
    ALTREP_ERROR_IN_CLASS("cannot access data pointer for this ALTVEC object", x);
}

static const void *altvec_Dataptr_or_null_default(SEXP x)
{
    return NULL;
}

static SEXP altvec_Extract_subset_default(SEXP x, SEXP indx, SEXP call)
{
    return NULL;
}

static int altinteger_Elt_default(SEXP x, R_xlen_t i) { return INTEGER(x)[i]; }

static R_xlen_t
altinteger_Get_region_default(SEXP sx, R_xlen_t i, R_xlen_t n, int *buf)
{
    R_xlen_t size = XLENGTH(sx);
    R_xlen_t ncopy = size - i > n ? n : size - i;
    for (R_xlen_t k = 0; k < ncopy; k++)
	buf[k] = INTEGER_ELT(sx, k + i);
    return ncopy;
}

static int altinteger_Is_sorted_default(SEXP x) { return UNKNOWN_SORTEDNESS; }
static int altinteger_No_NA_default(SEXP x) { return 0; }

static SEXP altinteger_Sum_default(SEXP x, Rboolean narm) { return NULL; }
static SEXP altinteger_Min_default(SEXP x, Rboolean narm) { return NULL; }
static SEXP altinteger_Max_default(SEXP x, Rboolean narm) { return NULL; }

static double altreal_Elt_default(SEXP x, R_xlen_t i) { return REAL(x)[i]; }

static R_xlen_t
altreal_Get_region_default(SEXP sx, R_xlen_t i, R_xlen_t n, double *buf)
{
    R_xlen_t size = XLENGTH(sx);
    R_xlen_t ncopy = size - i > n ? n : size - i;
    for (R_xlen_t k = 0; k < ncopy; k++)
	buf[k] = REAL_ELT(sx, k + i);
    return ncopy;
}

static int altreal_Is_sorted_default(SEXP x) { return UNKNOWN_SORTEDNESS; }
static int altreal_No_NA_default(SEXP x) { return 0; }

static SEXP altreal_Sum_default(SEXP x, Rboolean narm) { return NULL; }
static SEXP altreal_Min_default(SEXP x, Rboolean narm) { return NULL; }
static SEXP altreal_Max_default(SEXP x, Rboolean narm) { return NULL; }

static int altlogical_Elt_default(SEXP x, R_xlen_t i) { return LOGICAL(x)[i]; }

static R_xlen_t
altlogical_Get_region_default(SEXP sx, R_xlen_t i, R_xlen_t n, int *buf)
{
    R_xlen_t size = XLENGTH(sx);
    R_xlen_t ncopy = size - i > n ? n : size - i;
    for (R_xlen_t k = 0; k < ncopy; k++)
	buf[k] = LOGICAL_ELT(sx, k + i);
    return ncopy;
}

static int altlogical_Is_sorted_default(SEXP x) { return UNKNOWN_SORTEDNESS; }
static int altlogical_No_NA_default(SEXP x) { return 0; }

static SEXP altlogical_Sum_default(SEXP x, Rboolean narm) { return NULL; }


static Rbyte altraw_Elt_default(SEXP x, R_xlen_t i) { return RAW(x)[i]; }

static R_xlen_t
altraw_Get_region_default(SEXP sx, R_xlen_t i, R_xlen_t n, Rbyte *buf)
{
    R_xlen_t size = XLENGTH(sx);
    R_xlen_t ncopy = size - i > n ? n : size - i;
    for (R_xlen_t k = 0; k < ncopy; k++)
	buf[k] = RAW_ELT(sx, k + i);
    return ncopy;
}


static Rcomplex altcomplex_Elt_default(SEXP x, R_xlen_t i)
{
    return COMPLEX(x)[i];
}

static R_xlen_t
altcomplex_Get_region_default(SEXP sx, R_xlen_t i, R_xlen_t n, Rcomplex *buf)
{
    R_xlen_t size = XLENGTH(sx);
    R_xlen_t ncopy = size - i > n ? n : size - i;
    for (R_xlen_t k = 0; k < ncopy; k++)
	buf[k] = COMPLEX_ELT(sx, k + i);
    return ncopy;
}

static SEXP altstring_Elt_default(SEXP x, R_xlen_t i)
{
    ALTREP_ERROR_IN_CLASS("No Elt method found for ALTSTRING class", x);
}

static void altstring_Set_elt_default(SEXP x, R_xlen_t i, SEXP v)
{
    ALTREP_ERROR_IN_CLASS("No Set_elt found for ALTSTRING class", x);
}

static int altstring_Is_sorted_default(SEXP x) { return UNKNOWN_SORTEDNESS; }
static int altstring_No_NA_default(SEXP x) { return 0; }

static SEXP altlist_Elt_default(SEXP x, R_xlen_t i)
{
    ALTREP_ERROR_IN_CLASS("ALTLIST classes must provide an Elt method", x);
}

static void altlist_Set_elt_default(SEXP x, R_xlen_t i, SEXP v)
{
    ALTREP_ERROR_IN_CLASS("ALTLIST classes must provide a Set_elt method", x);
}

static void *altlist_Dataptr_default(SEXP x, Rboolean writable)
{
    ALTREP_ERROR_IN_CLASS("No Dataptr method found for ALTLIST class", x);
}

static const void *altlist_Dataptr_or_null_default(SEXP x)
{
    return NULL;
}

/**
 ** ALTSXP Default Methods
 **
 ** These are what make ALTSXP worth a SEXPTYPE: given only Elt_size, New and
 ** Get_region/Set_region, R can subset, duplicate and serialise an opaque
 ** vector without knowing anything about its elements.  The same code serves
 ** int64, uint64, float16, decimal128, fixed-width UUIDs, and so on.
 **/

static SEXP altsxp_Elt_type_default(SEXP x)
{
    /* A class that declares no element type is its own element type. */
    return ALTREP_OBJECT_CLSSYM(x);
}

static size_t altsxp_Elt_size_default(SEXP x)
{
    ALTREP_ERROR_IN_CLASS("ALTSXP classes must provide an Elt_size method", x);
}

static SEXP altsxp_New_default(SEXP proto, R_xlen_t n)
{
    ALTREP_ERROR_IN_CLASS("ALTSXP classes must provide a New method", proto);
}

static R_xlen_t
altsxp_Get_region_default(SEXP x, R_xlen_t i, R_xlen_t n, void *buf)
{
    const void *p = ALTVEC_DATAPTR_OR_NULL(x);
    if (p == NULL)
	ALTREP_ERROR_IN_CLASS("no Get_region method and no data pointer", x);

    size_t esz = ALTSXP_DISPATCH(Elt_size, x);
    R_xlen_t size = ALTREP_LENGTH(x);
    R_xlen_t ncopy = size - i > n ? n : size - i;
    memcpy(buf, (const char *) p + (size_t) i * esz, (size_t) ncopy * esz);
    return ncopy;
}

static R_xlen_t
altsxp_Set_region_default(SEXP x, R_xlen_t i, R_xlen_t n, const void *buf)
{
    void *p = ALTVEC_DATAPTR(x);
    if (p == NULL)
	ALTREP_ERROR_IN_CLASS("no Set_region method and no data pointer", x);

    size_t esz = ALTSXP_DISPATCH(Elt_size, x);
    R_xlen_t size = ALTREP_LENGTH(x);
    R_xlen_t ncopy = size - i > n ? n : size - i;
    memcpy((char *) p + (size_t) i * esz, buf, (size_t) ncopy * esz);
    return ncopy;
}

static R_xlen_t altsxp_Set_na_region_default(SEXP x, R_xlen_t i, R_xlen_t n)
{
    ALTREP_ERROR_IN_CLASS("ALTSXP classes must provide a Set_na_region method",
			  x);
}

static R_xlen_t
altsxp_Is_na_region_default(SEXP x, R_xlen_t i, R_xlen_t n, int *buf)
{
    /* A class with no notion of a missing value has none. */
    R_xlen_t size = ALTREP_LENGTH(x);
    R_xlen_t ncopy = size - i > n ? n : size - i;
    for (R_xlen_t k = 0; k < ncopy; k++)
	buf[k] = FALSE;
    return ncopy;
}

static int altsxp_Compare_default(SEXP x, R_xlen_t i, SEXP y, R_xlen_t j)
{
    ALTREP_ERROR_IN_CLASS("ALTSXP class provides no ordering", x);
}

static SEXP altsxp_Format_default(SEXP x, R_xlen_t i, R_xlen_t n)
{
    return NULL; /* the printer falls back to a summary line */
}

static SEXP altsxp_Arith_default(SEXP call, SEXP op, SEXP x, SEXP y)
{
    return NULL; /* decline */
}

static SEXP altsxp_Relop_default(SEXP call, SEXP op, SEXP x, SEXP y)
{
    return NULL; /* decline */
}

static unsigned int altsxp_Traits_default(SEXP x)
{
    /* No bits: assume nothing beyond what an ordinary R vector offers. */
    return 0;
}

static SEXP altsxp_Coerce_from_default(SEXP proto, SEXP from) { return NULL; }

static SEXP altsxp_Na_widen_default(SEXP x) { return NULL; }

static SEXP altsxp_Sum_default(SEXP x, Rboolean narm) { return NULL; }
static SEXP altsxp_Min_default(SEXP x, Rboolean narm) { return NULL; }
static SEXP altsxp_Max_default(SEXP x, Rboolean narm) { return NULL; }

static int altsxp_Is_sorted_default(SEXP x) { return UNKNOWN_SORTEDNESS; }

/* generic: ask the class which elements are NA */
static int altsxp_No_NA_default(SEXP x)
{
    R_xlen_t n = ALTREP_LENGTH(x);
    if (n == 0) return TRUE;

    R_xlen_t nb = n > 512 ? 512 : n;
    const void *vmax = vmaxget();
    int *buf = (int *) R_alloc((size_t) nb, sizeof(int));
    int ans = TRUE;
    for (R_xlen_t i = 0; i < n && ans; i += nb) {
	R_xlen_t k = n - i > nb ? nb : n - i;
	ALTSXP_DISPATCH(Is_na_region, x, i, k, buf);
	for (R_xlen_t j = 0; j < k; j++)
	    if (buf[j]) { ans = FALSE; break; }
    }
    vmaxset(vmax);
    return ans;
}

static SEXP altsxp_Math_default(SEXP call, SEXP op, SEXP x) { return NULL; }

/* Generic subsetting: copy whole elements by index, filling NA where the
   subscript is NA or out of bounds. */
static SEXP altsxp_Extract_subset_default(SEXP x, SEXP indx, SEXP call)
{
    R_xlen_t n = XLENGTH(indx);
    R_xlen_t nx = ALTREP_LENGTH(x);

    /* If any subscript is NA or out of bounds the result needs an NA, so
       the source may first have to be widened to an NA-capable form. */
    if (! R_altsxp_nullable(x)) {
	Rboolean needs_na = FALSE;
	if (TYPEOF(indx) == INTSXP) {
	    const int *pi = INTEGER_RO(indx);
	    for (R_xlen_t k = 0; k < n && !needs_na; k++)
		if (!(0 < pi[k] && pi[k] <= nx)) needs_na = TRUE;
	}
	else {
	    const double *pd = REAL_RO(indx);
	    for (R_xlen_t k = 0; k < n && !needs_na; k++) {
		R_xlen_t ii = (R_xlen_t) (pd[k] - 1);
		if (!(R_FINITE(pd[k]) && 0 <= ii && ii < nx)) needs_na = TRUE;
	    }
	}
	if (needs_na) {
	    SEXP w = R_altsxp_na_widen(x);
	    if (w == NULL)
		error(_("'%s' cannot represent NA"), R_typeToChar(x));
	    x = w;
	}
    }
    PROTECT(x);

    altsxp_methods_t *m = ALTSXP_METHODS_TABLE(x);
    size_t esz = m->Elt_size(x);

    SEXP ans = PROTECT(m->New(x, n));

    const char *src = (const char *) ALTVEC_DATAPTR_OR_NULL(x);
    char *dst = (src == NULL) ? NULL : (char *) ALTVEC_DATAPTR(ans);
    void *buf = (src == NULL) ? R_alloc(1, esz) : NULL;

#define ALTSXP_COPY_ONE(k, ii) do {				\
	if (src != NULL)					\
	    memcpy(dst + (size_t) (k) * esz,			\
		   src + (size_t) (ii) * esz, esz);		\
	else {							\
	    m->Get_region(x, ii, 1, buf);			\
	    m->Set_region(ans, k, 1, buf);			\
	}							\
    } while (0)

    if (TYPEOF(indx) == INTSXP) {
	const int *pi = INTEGER_RO(indx);
	for (R_xlen_t k = 0; k < n; k++) {
	    R_xlen_t ii = pi[k];
	    if (0 < ii && ii <= nx)
		ALTSXP_COPY_ONE(k, ii - 1);
	    else
		m->Set_na_region(ans, k, 1);
	}
    }
    else {
	const double *pd = REAL_RO(indx);
	for (R_xlen_t k = 0; k < n; k++) {
	    double di = pd[k];
	    R_xlen_t ii = (R_xlen_t) (di - 1);
	    if (R_FINITE(di) && 0 <= ii && ii < nx)
		ALTSXP_COPY_ONE(k, ii);
	    else
		m->Set_na_region(ans, k, 1);
	}
    }
#undef ALTSXP_COPY_ONE

    UNPROTECT(2); /* ans, x */
    return ans;
}

static SEXP altsxp_Duplicate_default(SEXP x, Rboolean deep)
{
    altsxp_methods_t *m = ALTSXP_METHODS_TABLE(x);
    R_xlen_t n = ALTREP_LENGTH(x);
    size_t esz = m->Elt_size(x);

    SEXP ans = PROTECT(m->New(x, n));

    const void *src = ALTVEC_DATAPTR_OR_NULL(x);
    if (src != NULL)
	memcpy(ALTVEC_DATAPTR(ans), src, (size_t) n * esz);
    else {
	R_xlen_t bufn = n > 512 ? 512 : n;
	void *buf = R_alloc((size_t) bufn, esz);
	for (R_xlen_t i = 0; i < n; i += bufn) {
	    R_xlen_t nb = n - i > bufn ? bufn : n - i;
	    m->Get_region(x, i, nb, buf);
	    m->Set_region(ans, i, nb, buf);
	}
    }

    UNPROTECT(1);
    return ans;
}

/* Serialised state is (element type name, length, raw payload), which is
   byte exact in every serialisation format and needs no class cooperation. */
static SEXP altsxp_Serialized_state_default(SEXP x)
{
    altsxp_methods_t *m = ALTSXP_METHODS_TABLE(x);
    R_xlen_t n = ALTREP_LENGTH(x);
    size_t esz = m->Elt_size(x);

    SEXP payload = PROTECT(allocVector(RAWSXP, (R_xlen_t) ((size_t) n * esz)));
    if (n > 0)
	m->Get_region(x, 0, n, RAW(payload));

    SEXP state = PROTECT(allocVector(VECSXP, 3));
    SET_VECTOR_ELT(state, 0, ScalarString(PRINTNAME(m->Elt_type(x))));
    SET_VECTOR_ELT(state, 1, ScalarReal((double) n));
    SET_VECTOR_ELT(state, 2, payload);

    UNPROTECT(2);
    return state;
}

static SEXP altsxp_Unserialize_default(SEXP class, SEXP state)
{
    altsxp_methods_t *m = CLASS_METHODS_TABLE(class);

    if (TYPEOF(state) != VECSXP || XLENGTH(state) != 3)
	error("unexpected serialised state for an ALTSXP object");

    R_xlen_t n = (R_xlen_t) asReal(VECTOR_ELT(state, 1));
    SEXP payload = VECTOR_ELT(state, 2);

    /* New() is passed the class object here rather than an instance; see
       the note on the New method in R_ext/Altrep.h. */
    SEXP ans = PROTECT(m->New(class, n));

    SEXP want = m->Elt_type(ans);
    SEXP got = installTrChar(STRING_ELT(VECTOR_ELT(state, 0), 0));
    if (want != got)
	warning("serialised ALTSXP element type '%s' does not match "
		"registered element type '%s'",
		CHAR(PRINTNAME(got)), CHAR(PRINTNAME(want)));

    if (n > 0)
	m->Set_region(ans, 0, n, RAW(payload));

    UNPROTECT(1);
    return ans;
}


/**
 ** ALTREP Initial Method Tables
 **/

static altinteger_methods_t altinteger_default_methods = {
    .UnserializeEX = altrep_UnserializeEX_default,
    .Unserialize = altrep_Unserialize_default,
    .Serialized_state = altrep_Serialized_state_default,
    .DuplicateEX = altrep_DuplicateEX_default,
    .Duplicate = altrep_Duplicate_default,
    .Coerce = altrep_Coerce_default,
    .Inspect = altrep_Inspect_default,
    .Length = altrep_Length_default,
    .Dataptr = altvec_Dataptr_default,
    .Dataptr_or_null = altvec_Dataptr_or_null_default,
    .Extract_subset = altvec_Extract_subset_default,
    .Elt = altinteger_Elt_default,
    .Get_region = altinteger_Get_region_default,
    .Is_sorted = altinteger_Is_sorted_default,
    .No_NA = altinteger_No_NA_default,
    .Sum = altinteger_Sum_default,
    .Min = altinteger_Min_default,
    .Max = altinteger_Max_default
};

static altreal_methods_t altreal_default_methods = {
    .UnserializeEX = altrep_UnserializeEX_default,
    .Unserialize = altrep_Unserialize_default,
    .Serialized_state = altrep_Serialized_state_default,
    .DuplicateEX = altrep_DuplicateEX_default,
    .Duplicate = altrep_Duplicate_default,
    .Coerce = altrep_Coerce_default,
    .Inspect = altrep_Inspect_default,
    .Length = altrep_Length_default,
    .Dataptr = altvec_Dataptr_default,
    .Dataptr_or_null = altvec_Dataptr_or_null_default,
    .Extract_subset = altvec_Extract_subset_default,
    .Elt = altreal_Elt_default,
    .Get_region = altreal_Get_region_default,
    .Is_sorted = altreal_Is_sorted_default,
    .No_NA = altreal_No_NA_default,
    .Sum = altreal_Sum_default,
    .Min = altreal_Min_default,
    .Max = altreal_Max_default
};


static altlogical_methods_t altlogical_default_methods = {
    .UnserializeEX = altrep_UnserializeEX_default,
    .Unserialize = altrep_Unserialize_default,
    .Serialized_state = altrep_Serialized_state_default,
    .DuplicateEX = altrep_DuplicateEX_default,
    .Duplicate = altrep_Duplicate_default,
    .Coerce = altrep_Coerce_default,
    .Inspect = altrep_Inspect_default,
    .Length = altrep_Length_default,
    .Dataptr = altvec_Dataptr_default,
    .Dataptr_or_null = altvec_Dataptr_or_null_default,
    .Extract_subset = altvec_Extract_subset_default,
    .Elt = altlogical_Elt_default,
    .Get_region = altlogical_Get_region_default,
    .Is_sorted = altlogical_Is_sorted_default,
    .No_NA = altlogical_No_NA_default,
    .Sum = altlogical_Sum_default
};


static altraw_methods_t altraw_default_methods = {
    .UnserializeEX = altrep_UnserializeEX_default,
    .Unserialize = altrep_Unserialize_default,
    .Serialized_state = altrep_Serialized_state_default,
    .DuplicateEX = altrep_DuplicateEX_default,
    .Duplicate = altrep_Duplicate_default,
    .Coerce = altrep_Coerce_default,
    .Inspect = altrep_Inspect_default,
    .Length = altrep_Length_default,
    .Dataptr = altvec_Dataptr_default,
    .Dataptr_or_null = altvec_Dataptr_or_null_default,
    .Extract_subset = altvec_Extract_subset_default,
    .Elt = altraw_Elt_default,
    .Get_region = altraw_Get_region_default
};




static altcomplex_methods_t altcomplex_default_methods = {
    .UnserializeEX = altrep_UnserializeEX_default,
    .Unserialize = altrep_Unserialize_default,
    .Serialized_state = altrep_Serialized_state_default,
    .DuplicateEX = altrep_DuplicateEX_default,
    .Duplicate = altrep_Duplicate_default,
    .Coerce = altrep_Coerce_default,
    .Inspect = altrep_Inspect_default,
    .Length = altrep_Length_default,
    .Dataptr = altvec_Dataptr_default,
    .Dataptr_or_null = altvec_Dataptr_or_null_default,
    .Extract_subset = altvec_Extract_subset_default,
    .Elt = altcomplex_Elt_default,
    .Get_region = altcomplex_Get_region_default
};



static altstring_methods_t altstring_default_methods = {
    .UnserializeEX = altrep_UnserializeEX_default,
    .Unserialize = altrep_Unserialize_default,
    .Serialized_state = altrep_Serialized_state_default,
    .DuplicateEX = altrep_DuplicateEX_default,
    .Duplicate = altrep_Duplicate_default,
    .Coerce = altrep_Coerce_default,
    .Inspect = altrep_Inspect_default,
    .Length = altrep_Length_default,
    .Dataptr = altvec_Dataptr_default,
    .Dataptr_or_null = altvec_Dataptr_or_null_default,
    .Extract_subset = altvec_Extract_subset_default,
    .Elt = altstring_Elt_default,
    .Set_elt = altstring_Set_elt_default,
    .Is_sorted = altstring_Is_sorted_default,
    .No_NA = altstring_No_NA_default
};



static altsxp_methods_t altsxp_default_methods = {
    .UnserializeEX = altrep_UnserializeEX_default,
    .Unserialize = altsxp_Unserialize_default,
    .Serialized_state = altsxp_Serialized_state_default,
    .DuplicateEX = altrep_DuplicateEX_default,
    .Duplicate = altsxp_Duplicate_default,
    .Coerce = altrep_Coerce_default,
    .Inspect = altrep_Inspect_default,
    .Length = altrep_Length_default,
    .Dataptr = altvec_Dataptr_default,
    .Dataptr_or_null = altvec_Dataptr_or_null_default,
    .Extract_subset = altsxp_Extract_subset_default,
    .Elt_type = altsxp_Elt_type_default,
    .Elt_size = altsxp_Elt_size_default,
    .New = altsxp_New_default,
    .Get_region = altsxp_Get_region_default,
    .Set_region = altsxp_Set_region_default,
    .Set_na_region = altsxp_Set_na_region_default,
    .Is_na_region = altsxp_Is_na_region_default,
    .Compare = altsxp_Compare_default,
    .Format = altsxp_Format_default,
    .Arith = altsxp_Arith_default,
    .Relop = altsxp_Relop_default,
    .Traits = altsxp_Traits_default,
    .Coerce_from = altsxp_Coerce_from_default,
    .Na_widen = altsxp_Na_widen_default,
    .Sum = altsxp_Sum_default,
    .Min = altsxp_Min_default,
    .Max = altsxp_Max_default,
    .Is_sorted = altsxp_Is_sorted_default,
    .No_NA = altsxp_No_NA_default,
    .Math = altsxp_Math_default
};


static altlist_methods_t altlist_default_methods = {
    .UnserializeEX = altrep_UnserializeEX_default,
    .Unserialize = altrep_Unserialize_default,
    .Serialized_state = altrep_Serialized_state_default,
    .DuplicateEX = altrep_DuplicateEX_default,
    .Duplicate = altrep_Duplicate_default,
    .Coerce = altrep_Coerce_default,
    .Inspect = altrep_Inspect_default,
    .Length = altrep_Length_default,
    .Dataptr = altlist_Dataptr_default,
    .Dataptr_or_null = altlist_Dataptr_or_null_default,
    .Extract_subset = altvec_Extract_subset_default,
    .Elt = altlist_Elt_default,
    .Set_elt = altlist_Set_elt_default
};


/**
 ** Class Constructors
 **/

#define INIT_CLASS(cls, type) do {				\
	*((type##_methods_t *) (CLASS_METHODS_TABLE(cls))) =	\
	    type##_default_methods;				\
    } while (FALSE)

#define MAKE_CLASS(var, type) do {				\
	var = allocVector(RAWSXP, sizeof(type##_methods_t));	\
	R_PreserveObject(var);					\
	INIT_CLASS(var, type);					\
    } while (FALSE)

static R_INLINE R_altrep_class_t R_cast_altrep_class(SEXP x)
{
    /**** some king of optional check? */
    R_altrep_class_t val = R_SUBTYPE_INIT(x);
    return val;
}

static R_altrep_class_t
make_altrep_class(int type, const char *cname, const char *pname, DllInfo *dll)
{
    SEXP class;
    switch(type) {
    case INTSXP:  MAKE_CLASS(class, altinteger); break;
    case REALSXP: MAKE_CLASS(class, altreal);    break;
    case LGLSXP:  MAKE_CLASS(class, altlogical); break;
    case RAWSXP:  MAKE_CLASS(class, altraw);     break;
    case CPLXSXP: MAKE_CLASS(class, altcomplex); break;
    case STRSXP:  MAKE_CLASS(class, altstring);  break;
    case VECSXP:  MAKE_CLASS(class, altlist);    break;
    case ALTSXP:  MAKE_CLASS(class, altsxp);  break;
    default: error("unsupported ALTREP class");
    }
    RegisterClass(class, type, cname, pname, dll);
    return R_cast_altrep_class(class);
}

/*  Using macros like this makes it easier to add new methods, but
    makes searching for source harder. Probably a good idea on
    balance though. */
#define DEFINE_CLASS_CONSTRUCTOR(cls, type)			\
    R_altrep_class_t R_make_##cls##_class(const char *cname,	\
					  const char *pname,	\
					  DllInfo *dll)		\
    {								\
	return  make_altrep_class(type, cname, pname, dll);	\
    }

DEFINE_CLASS_CONSTRUCTOR(altstring, STRSXP)
DEFINE_CLASS_CONSTRUCTOR(altlist, VECSXP)
DEFINE_CLASS_CONSTRUCTOR(altinteger, INTSXP)
DEFINE_CLASS_CONSTRUCTOR(altreal, REALSXP)
DEFINE_CLASS_CONSTRUCTOR(altlogical, LGLSXP)
DEFINE_CLASS_CONSTRUCTOR(altraw, RAWSXP)
DEFINE_CLASS_CONSTRUCTOR(altcomplex, CPLXSXP)
DEFINE_CLASS_CONSTRUCTOR(altsxp, ALTSXP)

static void reinit_altrep_class(SEXP class)
{
    switch (ALTREP_CLASS_BASE_TYPE(class)) {
    case INTSXP: INIT_CLASS(class, altinteger); break;
    case REALSXP: INIT_CLASS(class, altreal); break;
    case STRSXP: INIT_CLASS(class, altstring); break;
    case LGLSXP: INIT_CLASS(class, altlogical); break;
    case RAWSXP: INIT_CLASS(class, altraw); break;
    case CPLXSXP: INIT_CLASS(class, altcomplex); break;
    case VECSXP: INIT_CLASS(class, altlist); break;
    case ALTSXP: INIT_CLASS(class, altsxp); break;
    default: error("unsupported ALTREP class");
    }
}


/**
 ** ALTREP Method Setters
 **/

#define DEFINE_METHOD_SETTER(CNAME, MNAME)				\
    void R_set_##CNAME##_##MNAME##_method(R_altrep_class_t cls,		\
					  R_##CNAME##_##MNAME##_method_t fun) \
    {									\
	CNAME##_methods_t *m = CLASS_METHODS_TABLE(R_SEXP(cls));	\
	m->MNAME = fun;							\
    }

DEFINE_METHOD_SETTER(altrep, UnserializeEX)
DEFINE_METHOD_SETTER(altrep, Unserialize)
DEFINE_METHOD_SETTER(altrep, Serialized_state)
DEFINE_METHOD_SETTER(altrep, DuplicateEX)
DEFINE_METHOD_SETTER(altrep, Duplicate)
DEFINE_METHOD_SETTER(altrep, Coerce)
DEFINE_METHOD_SETTER(altrep, Inspect)
DEFINE_METHOD_SETTER(altrep, Length)

DEFINE_METHOD_SETTER(altvec, Dataptr)
DEFINE_METHOD_SETTER(altvec, Dataptr_or_null)
DEFINE_METHOD_SETTER(altvec, Extract_subset)

DEFINE_METHOD_SETTER(altinteger, Elt)
DEFINE_METHOD_SETTER(altinteger, Get_region)
DEFINE_METHOD_SETTER(altinteger, Is_sorted)
DEFINE_METHOD_SETTER(altinteger, No_NA)
DEFINE_METHOD_SETTER(altinteger, Sum)
DEFINE_METHOD_SETTER(altinteger, Min)
DEFINE_METHOD_SETTER(altinteger, Max)

DEFINE_METHOD_SETTER(altreal, Elt)
DEFINE_METHOD_SETTER(altreal, Get_region)
DEFINE_METHOD_SETTER(altreal, Is_sorted)
DEFINE_METHOD_SETTER(altreal, No_NA)
DEFINE_METHOD_SETTER(altreal, Sum)
DEFINE_METHOD_SETTER(altreal, Min)
DEFINE_METHOD_SETTER(altreal, Max)

DEFINE_METHOD_SETTER(altlogical, Elt)
DEFINE_METHOD_SETTER(altlogical, Get_region)
DEFINE_METHOD_SETTER(altlogical, Is_sorted)
DEFINE_METHOD_SETTER(altlogical, No_NA)
DEFINE_METHOD_SETTER(altlogical, Sum)

DEFINE_METHOD_SETTER(altraw, Elt)
DEFINE_METHOD_SETTER(altraw, Get_region)

DEFINE_METHOD_SETTER(altcomplex, Elt)
DEFINE_METHOD_SETTER(altcomplex, Get_region)

DEFINE_METHOD_SETTER(altstring, Elt)
DEFINE_METHOD_SETTER(altstring, Set_elt)
DEFINE_METHOD_SETTER(altstring, Is_sorted)
DEFINE_METHOD_SETTER(altstring, No_NA)

DEFINE_METHOD_SETTER(altlist, Elt)
DEFINE_METHOD_SETTER(altlist, Set_elt)

DEFINE_METHOD_SETTER(altsxp, Elt_type)
DEFINE_METHOD_SETTER(altsxp, Elt_size)
DEFINE_METHOD_SETTER(altsxp, New)
DEFINE_METHOD_SETTER(altsxp, Get_region)
DEFINE_METHOD_SETTER(altsxp, Set_region)
DEFINE_METHOD_SETTER(altsxp, Set_na_region)
DEFINE_METHOD_SETTER(altsxp, Is_na_region)
DEFINE_METHOD_SETTER(altsxp, Compare)
DEFINE_METHOD_SETTER(altsxp, Format)
DEFINE_METHOD_SETTER(altsxp, Arith)
DEFINE_METHOD_SETTER(altsxp, Relop)
DEFINE_METHOD_SETTER(altsxp, Traits)
DEFINE_METHOD_SETTER(altsxp, Coerce_from)
DEFINE_METHOD_SETTER(altsxp, Na_widen)
DEFINE_METHOD_SETTER(altsxp, Sum)
DEFINE_METHOD_SETTER(altsxp, Min)
DEFINE_METHOD_SETTER(altsxp, Max)
DEFINE_METHOD_SETTER(altsxp, Is_sorted)
DEFINE_METHOD_SETTER(altsxp, No_NA)
DEFINE_METHOD_SETTER(altsxp, Math)

/**
 ** ALTREP Object Constructor and Utility Functions
 **/

SEXP R_new_altrep(R_altrep_class_t aclass, SEXP data1, SEXP data2)
{
    void ALTREP_SET_TYPEOF(SEXP, int); /* in memory.c */
    SEXP sclass = R_SEXP(aclass);
    int type = ALTREP_CLASS_BASE_TYPE(sclass);
    SEXP ans = CONS(data1, data2);
    ALTREP_SET_TYPEOF(ans, type);
    SET_ALTREP_CLASS(ans, sclass);
    return ans;
}

Rboolean R_altrep_inherits(SEXP x, R_altrep_class_t class)
{
    return ALTREP(x) && ALTREP_CLASS(x) == R_SEXP(class);
}

SEXP R_altrep_class_name(SEXP x)
{
    return ALTREP(x) ? CAR(ATTRIB(ALTREP_CLASS(x))) : R_NilValue;
}

SEXP R_altrep_class_package(SEXP x)
{
    return ALTREP(x) ? CADR(ATTRIB(ALTREP_CLASS(x))) : R_NilValue;
}

attribute_hidden SEXP do_altrep_class(SEXP call, SEXP op, SEXP args, SEXP env)
{
    checkArity(op, args);
    SEXP x = CAR(args);
    if (ALTREP(x)) {
	SEXP info = ALTREP_SERIALIZED_CLASS(x);
	SEXP val = allocVector(STRSXP, 2);
	SET_STRING_ELT(val, 0, PRINTNAME(ALTREP_SERIALIZED_CLASS_CLSSYM(info)));
	SET_STRING_ELT(val, 1, PRINTNAME(ALTREP_SERIALIZED_CLASS_PKGSYM(info)));
	return val;
    }
    else
	return R_NilValue;
}

