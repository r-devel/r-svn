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
   not depend on the system math library.  Each cutoff is the largest
   double T such that the shortcut, in IEEE double arithmetic, is the
   correctly rounded f(x) for all |x| <= T.  For f(x) ~= x the next term
   must stay below half an ulp of x, which is tightest just below a power
   of 2.  Shared by do_math1() and the byte-code engine, so that both give
   the same results (PR#19286). */

static R_INLINE double R_exp(double x)
{
    /* exact for |x| < 2^-53; 1 + 2^-53 ties down, exp(2^-53) rounds up */
    return fabs(x) < 0x1p-53 ? 1. + x : exp(x);
}

static R_INLINE double R_expm1(double x)
{
    /* expm1(x) - x ~= x^2/2 < 2^-106  <==>  |x| < sqrt(2) * 2^-53 */
    return fabs(x) <= 0x1.6a09e667f3bccp-53 ? x : expm1(x);
}

static R_INLINE double R_log1p(double x)
{
    /* x - log1p(x) ~= x^2/2: as for expm1() */
    return fabs(x) <= 0x1.6a09e667f3bccp-53 ? x : log1p(x);
}

static R_INLINE double R_sin(double x)
{
    /* x - sin(x) ~= x^3/6 < 2^-79  <==>  |x| < cbrt(3) * 2^-26 */
    return fabs(x) <= 0x1.7137449123ef6p-26 ? x : sin(x);
}

static R_INLINE double R_asin(double x)
{
    /* asin(x) - x ~= x^3/6: as for sin(), one ulp lower from the x^5 term */
    return fabs(x) <= 0x1.7137449123ef5p-26 ? x : asin(x);
}

static R_INLINE double R_tan(double x)
{
    /* tan(x) - x ~= x^3/3 < 2^-80  <==>  |x| < cbrt(6) * 2^-27, just
       below 2^-26 where the ulp is half the one above */
    return fabs(x) <= 0x1.d12ed0af1a27ep-27 ? x : tan(x);
}

static R_INLINE double R_atan(double x)
{
    /* x - atan(x) ~= x^3/3: as for tan() */
    return fabs(x) <= 0x1.d12ed0af1a27fp-27 ? x : atan(x);
}

static R_INLINE double R_cos(double x)
{
    /* at the next double up, x*x*0.5 is 3.5 ulps of 1 - 2^-53 and the
       subtraction ties to even, away from cos(x) */
    return fabs(x) <= 0x1.deeea11683f48p-26 ? 1. - x*x*0.5 : cos(x);
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
