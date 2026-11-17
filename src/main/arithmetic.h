/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1998--2014	    The R Core Team.
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

SEXP do_math1(SEXP, SEXP, SEXP, SEXP);
SEXP do_math2(SEXP, SEXP, SEXP, SEXP);
SEXP do_math3(SEXP, SEXP, SEXP, SEXP);
SEXP do_math4(SEXP, SEXP, SEXP, SEXP);
#ifdef WHEN_MATH5_IS_THERE
 SEXP do_math5(SEXP, SEXP, SEXP, SEXP);
#endif
SEXP do_cmathfuns(SEXP, SEXP, SEXP, SEXP);

SEXP complex_math1(SEXP, SEXP, SEXP, SEXP);
SEXP complex_math2(SEXP, SEXP, SEXP, SEXP);
SEXP complex_unary(ARITHOP_TYPE, SEXP, SEXP);
SEXP complex_binary(ARITHOP_TYPE, SEXP, SEXP);

double R_pow(double x, double y);
static R_INLINE double R_POW(double x, double y) /* handle x ^ 2 inline */
{
    return y == 2.0 ? x * x : R_pow(x, y);
}

/* some systems get this wrong, possibly depend on what libs are loaded */
static R_INLINE double R_log(double x) {
    return x > 0 ? log(x) : x == 0 ? R_NegInf : R_NaN;
}

/* Note that the behaviour of log(0) required is not necessarily that
   mandated by C99 (-HUGE_VAL), and the behaviour of log(x < 0) is
   optional in C99.  Some systems return -Inf for log(x < 0), e.g.
   libsunmath on Solaris.
*/

/* For tiny |x|, use the leading Taylor terms so that results near 0 do
   not depend on the system math library.  Shared by do_math1() and the
   byte-code engine, so that both give the same results (PR#19286). */

/* Largest double T for which the shortcut is the correctly rounded f(x)
   for all |x| <= T, from an exact search with Rmpfr (PR#19286). */
#define R_EXP_CUTOFF	0x1.fffffffffffffp-54 /* just below 2^-53 */
#define R_EXPM1_CUTOFF	0x1.6a09e667f3bccp-53 /* ~ sqrt(2) * 2^-53 */
#define R_LOG1P_CUTOFF	0x1.6a09e667f3bccp-53
#define R_SIN_CUTOFF	0x1.7137449123ef6p-26 /* ~ cbrt(3) * 2^-26 */
#define R_ASIN_CUTOFF	0x1.7137449123ef5p-26
#define R_TAN_CUTOFF	0x1.d12ed0af1a27ep-27 /* ~ cbrt(6) * 2^-27 */
#define R_ATAN_CUTOFF	0x1.d12ed0af1a27fp-27
#define R_COS_CUTOFF	0x1.deeea11683f48p-26

static R_INLINE double R_exp(double x)
{
    return fabs(x) <= R_EXP_CUTOFF ? 1. + x : exp(x);
}

static R_INLINE double R_expm1(double x)
{
    return fabs(x) <= R_EXPM1_CUTOFF ? x : expm1(x);
}

static R_INLINE double R_log1p(double x)
{
    return fabs(x) <= R_LOG1P_CUTOFF ? x : log1p(x);
}

static R_INLINE double R_sin(double x)
{
    return fabs(x) <= R_SIN_CUTOFF ? x : sin(x);
}

static R_INLINE double R_asin(double x)
{
    return fabs(x) <= R_ASIN_CUTOFF ? x : asin(x);
}

static R_INLINE double R_tan(double x)
{
    return fabs(x) <= R_TAN_CUTOFF ? x : tan(x);
}

static R_INLINE double R_atan(double x)
{
    return fabs(x) <= R_ATAN_CUTOFF ? x : atan(x);
}

static R_INLINE double R_cos(double x)
{
    return fabs(x) <= R_COS_CUTOFF ? 1. - x*x*0.5 : cos(x);
}
static R_INLINE double logbase(double x, double base)
{
#ifdef HAVE_LOG10
    if(base == 10) return x > 0 ? log10(x) : x == 0 ? R_NegInf : R_NaN;
#endif
#ifdef HAVE_LOG2
    if(base == 2) return x > 0 ? log2(x) : x == 0 ? R_NegInf : R_NaN;
#endif
    return R_log(x) / R_log(base);
}

SEXP do_log_builtin(SEXP call, SEXP op, SEXP args, SEXP env);

/* for binary operations */
/* adapted from Radford Neal's pqR */
static R_INLINE SEXP R_allocOrReuseVector(SEXP s1, SEXP s2,
					  SEXPTYPE type , R_xlen_t n)
{
    R_xlen_t n1 = XLENGTH(s1);
    R_xlen_t n2 = XLENGTH(s2);

    /* Try to use space for 2nd arg if both same length, so 1st argument's
       attributes will then take precedence when copied. */

    if (n == n2) {
        if (TYPEOF(s2) == type && NO_REFERENCES(s2)) {
	    if (ATTRIB(s2) != R_NilValue)
		/* need to remove 'names' attribute if present to
		   match what copyMostAttrib does. copyMostAttrib()
		   also skips 'dim' and 'dimnames' but those, here
		   since those, if present, will be replaced by
		   attribute cleanup code in R_Binary) */
		setAttrib(s2, R_NamesSymbol, R_NilValue);
            return s2;
	}
        else
            /* Can use 1st arg's space only if 2nd arg has no attributes, else
               we may not get attributes of result right. */
            if (n == n1 && TYPEOF(s1) == type && NO_REFERENCES(s1)
		&& ATTRIB(s2) == R_NilValue)
                return s1;
    }
    else if (n == n1 && TYPEOF(s1) == type && NO_REFERENCES(s1))
	return s1;

    return allocVector(type, n);
}

// we document that tanpi(0.5) is NaN, but TS 18661-4:2015
// does not require this and the Solaris and macOS versions give Inf.
double Rtanpi(double);
