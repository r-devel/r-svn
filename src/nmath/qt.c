/*
 *  Mathlib : A C Library of Special Functions
 *  Copyright (C) 2000-2022 The R Core Team
 *  Copyright (C) 2003-2022 The R Foundation
 *  Copyright (C) 1998 Ross Ihaka
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
 *
 *  DESCRIPTION
 *
 *	The "Student" t distribution quantile function.
 *
 *  NOTES
 *
 *	This is a C translation of the Fortran routine given in:
 *	Hill, G.W (1970) "Algorithm 396: Student's t-quantiles"
 *	CACM 13(10), 619-620.
 *
 *	Supplemented by inversion for 0 < ndf < 1.
 *
 *  ADDITIONS:
 *	- lower_tail, log_p
 *	- using	 expm1() : takes care of  Lozy (1979) "Remark on Algo.", TOMS
 *	- Apply 2-term Taylor expansion as in
 *	  Hill, G.W (1981) "Remark on Algo.396", ACM TOMS 7, 250-1
 *	- Improve the formula decision for 1 < df < 2
 */

#include "nmath.h"
#include "dpq.h"

double qt(double p, double ndf, int lower_tail, int log_p)
{
    const static double eps = 1.e-12;

    double P, q;

#ifdef IEEE_754
    if (ISNAN(p) || ISNAN(ndf))
	return p + ndf;
#endif

    R_Q_P01_boundaries(p, ML_NEGINF, ML_POSINF);

    if (ndf <= 0) ML_WARN_return_NAN;

    if (ndf < 1) { /* based on qnt */
	const static double accu = 1e-13;
	const static double Eps = 1e-11; /* must be > accu */

	double ux, lx, nx, pp;

	int iter = 0;

	p = R_DT_qIv(p);

	/* Invert pt(.) :
	 * 1. finding an upper and lower bound */
	if(p > 1 - DBL_EPSILON) return ML_POSINF;
	pp = fmin2(1 - DBL_EPSILON, p * (1 + Eps));
	for(ux = 1.; ux < DBL_MAX && pt(ux, ndf, TRUE, FALSE) < pp; ux *= 2);
	pp = p * (1 - Eps);
	for(lx =-1.; lx > -DBL_MAX && pt(lx, ndf, TRUE, FALSE) > pp; lx *= 2);

	/* 2. interval (lx,ux)  halving
	   regula falsi failed on qt(0.1, 0.1)
	 */
	do {
	    nx = 0.5 * (lx + ux);
	    if (pt(nx, ndf, TRUE, FALSE) > p) ux = nx; else lx = nx;
	} while ((ux - lx) / fabs(nx) > accu && ++iter < 1000);

	if(iter >= 1000) ML_WARNING(ME_PRECISION, "qt");

	return 0.5 * (lx + ux);
    }

    /* Old comment:
     * FIXME: "This test should depend on  ndf  AND p  !!
     * -----  and in fact should be replaced by
     * something like Abramowitz & Stegun 26.7.5 (p.949)"
     *
     * That would say that if the qnorm value is x then
     * the result is about x + (x^3+x)/4df + (5x^5+16x^3+3x)/96df^2
     * The differences are tiny even if x ~ 1e5, and qnorm is not
     * that accurate in the extreme tails.
     */
    if (ndf > 1e20) return qnorm(p, 0., 1., lower_tail, log_p);

    P = R_D_qIv(p); /* if exp(p) underflows, we fix below */

    bool neg = (!lower_tail || P < 0.5) && (lower_tail || P > 0.5),
	is_neg_lower = (lower_tail == neg); /* both TRUE or FALSE == !xor */
    if(neg)
	P = 2 * (log_p ? (lower_tail ? P : -expm1(p)) : R_D_Lval(p));
    else
	P = 2 * (log_p ? (lower_tail ? -expm1(p) : P) : R_D_Cval(p));
    /* 0 <= P <= 1 ; P = 2*min(P', 1 - P')  in all cases */

     if (fabs(ndf - 2) < eps) {	/* df ~= 2 */
	if(P > DBL_MIN) {
	    if(3* P < DBL_EPSILON) /* P ~= 0 */
		q = 1 / sqrt(P);
	    else if (P > 0.9)	   /* P ~= 1 */
		q = (1 - P) * sqrt(2 /(P * (2 - P)));
	    else /* eps/3 <= P <= 0.9 */
		q = sqrt(2 / (P * (2 - P)) - 2);
	}
	else { /* P << 1, q = 1/sqrt(P) = ... */
	    if(log_p)
		q = is_neg_lower ? exp(- p/2) / M_SQRT2 : 1/sqrt(-expm1(p));
	    else
		q = ML_POSINF;
	}
    }
    else if (ndf < 1 + eps) { /* df ~= 1  (df < 1 excluded above): Cauchy */
	if(P == 1.) q = 0; // some versions of tanpi give Inf, some NaN
	else if(P > 0)
	    q = 1/tanpi(P/2.);/* == - tan((P+1) * M_PI_2) -- suffers for P ~= 0 */

	else { /* P = 0, but maybe = 2*exp(p) ! */
	    if(log_p) /* 1/tan(e) ~ 1/e */
		q = is_neg_lower ? M_1_PI * exp(-p) : -1./(M_PI * expm1(p));
	    else
		q = ML_POSINF;
	}
    }
    else {		/*-- usual case;  including, e.g.,  df = 1.1 */
	double x = 0., y, log_P2 = 0./* -Wall */,
	    a = 1 / (ndf - 0.5),
	    b = 48 / (a * a),
	    c = ((20700 * a / b - 98) * a - 16) * a + 96.36,
	    d = ((94.5 / (b + c) - 3) / b + 1) * sqrt(a * M_PI_2) * ndf;
	bool
	    P_ok1 = P > DBL_MIN || !log_p,
	    P_ok  = P_ok1; // when true (after check below), use "normal scale": log_p=FALSE
	if(P_ok1) {
	    y = pow(d * P, 2.0 / ndf);
	    P_ok = (y >= DBL_EPSILON);
	}
	if(!P_ok) {// log.p && P very.small  ||  (d*P)^(2/df) =: y < eps_c
	    log_P2 = is_neg_lower ? R_D_log(p) : R_D_LExp(p); /* == log(P / 2) */
	    x = (log(d) + M_LN2 + log_P2) / ndf;
	    y = exp(2 * x);
	}

	if ((ndf < 2.1 && P > 0.5) || y > 0.05 + a) { /* P > P0(df) */
	    /* Asymptotic inverse expansion about normal */
	    if(P_ok)
		x = qnorm(0.5 * P, 0., 1., /*lower_tail*/TRUE,  /*log_p*/FALSE);
	    else /* log_p && P underflowed */
		x = qnorm(log_P2,  0., 1., lower_tail,	        /*log_p*/ TRUE);

	    y = x * x;
	    if (ndf < 5)
		c += 0.3 * (ndf - 4.5) * (x + 0.6);
	    c = (((0.05 * d * x - 5) * x - 7) * x - 2) * x + b + c;
	    y = (((((0.4 * y + 6.3) * y + 36) * y + 94.5) / c
		  - y - 3) / b + 1) * x;
	    y = expm1(a * y * y);
	    q = sqrt(ndf * y);
	} else if(!P_ok && x < - M_LN2 * DBL_MANT_DIG) {/* 0.5* log(DBL_EPSILON) */
	    /* y above might have underflown */
	    q = sqrt(ndf) * exp(-x);
	}
	else { /* re-use 'y' from above */
	    y = ((1 / (((ndf + 6) / (ndf * y) - 0.089 * d - 0.822)
		       * (ndf + 2) * 3) + 0.5 / (ndf + 4))
		 * y - 1) * (ndf + 1) / (ndf + 2) + 1 / y;
	    q = sqrt(ndf * y);
	}


	/* Now apply 2-term Taylor expansion improvement (1-term = Newton):
	 * as by Hill (1981) [ref.above] */

	/* FIXME: This can be far from optimal when log_p = TRUE
	 *      but is still needed, e.g. for qt(-2, df=1.01, log=TRUE).
	 *	Probably also improvable when  lower_tail = FALSE */

	if(P_ok1) {
	    double M = fabs(sqrt(DBL_MAX/2.) - ndf);
	    int it=0;
	    while(it++ < 10 && (y = dt(q, ndf, FALSE)) > 0 &&
		  R_FINITE(x = (pt(q, ndf, FALSE, FALSE) - P/2) / y) &&
		  fabs(x) > 1e-14*fabs(q)) {
		/* Newton (=Taylor 1 term):
		 *  q += x;
		 * Taylor 2-term : */
		double F = (fabs(q) < M) ?
		    q * (ndf + 1) / (2 * (q * q + ndf)) :
		        (ndf + 1) / (2 * (q     + ndf/q)),
		    del_q = x * (1. + x * F);
		if(R_FINITE(del_q) && R_FINITE(q + del_q))
		    q += del_q;
		else if(R_FINITE(x) && R_FINITE(q + x))
		    q += x;
		else // FIXME??  if  q+x = +/-Inf is *better* than q should use it
		    break; // cannot improve  q  with a Newton/Taylor step
	    }
	}
    }
     return neg ? -q : q;
}
