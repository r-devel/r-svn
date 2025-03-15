/*
 *  Mathlib : A C Library of Special Functions
 *  Copyright (C) 2000-2025 The R Core Team
 *  Copyright (C) 2005-2020 The R Foundation
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
 *  SYNOPSIS
 *
 *    #include <Rmath.h>
 *    double rhyper(double NR, double NB, double n);
 *
 *  DESCRIPTION
 *
 *    Random variates from the hypergeometric distribution.
 *    Returns the number of white balls drawn when kk balls
 *    are drawn at random from an urn containing nn1 white
 *    and nn2 black balls.
 *
 *  REFERENCE
 *
 *    V. Kachitvichyanukul and B. Schmeiser (1985).
 *    ``Computer generation of hypergeometric random variates,''
 *    Journal of Statistical Computation and Simulation 22, 127-145.
 *
 *    The original algorithm had a bug -- R bug report PR#7314 --
 *    giving numbers slightly too small in case III h2pe
 *    where (m < 100 || ix <= 50) , see below.
 */

#include "nmath.h"
#include "dpq.h"
#include <limits.h>

// afc(i) :=  ln( i! )	[logarithm of the factorial i] = {R:} lgamma(i + 1) = {C:} lgammafn(i + 1)
static double afc(int i)
{
    // If (i > 7), use Stirling's approximation, otherwise use table lookup.
    const static double al[8] =
    {
	0.0,/*ln(0!)=ln(1)*/
	0.0,/*ln(1!)=ln(1)*/
	0.69314718055994530941723212145817,/*ln(2) */
	1.79175946922805500081247735838070,/*ln(6) */
	3.17805383034794561964694160129705,/*ln(24)*/
	4.78749174278204599424770093452324,
	6.57925121201010099506017829290394,
	8.52516136106541430016553103634712
	/* 10.60460290274525022841722740072165, approx. value below =
	   10.6046028788027; rel.error = 2.26 10^{-9}

	  FIXME: Use constants and if(n > ..) decisions from ./stirlerr.c
	  -----  will be even *faster* for n > 500 (or so)
	*/
    };

    if (i < 0) {
	MATHLIB_WARNING(("rhyper.c: afc(i), i=%d < 0 -- SHOULD NOT HAPPEN!\n"), i);
	return -1; // unreached
    }
    if (i <= 7)
	return al[i];
    // else i >= 8 :
    double di = i, i2 = di*di;
    return (di + 0.5) * log(di) - di + M_LN_SQRT_2PI +
	(0.0833333333333333 - 0.00277777777777778 / i2) / di;
}

//     rhyper(NR, NB, n) -- NR 'red', NB 'blue', n drawn, how many are 'red'
double rhyper(double nn1in, double nn2in, double kkin)
{
    /* extern double afc(int); */

    /* check parameter validity */

    if(!R_FINITE(nn1in) || !R_FINITE(nn2in) || !R_FINITE(kkin))
	ML_WARN_return_NAN;

    nn1in = R_forceint(nn1in);
    nn2in = R_forceint(nn2in);
    kkin  = R_forceint(kkin);

    if (nn1in < 0 || nn2in < 0 || kkin < 0 || kkin > nn1in + nn2in)
	ML_WARN_return_NAN;
    if (nn1in >= INT_MAX || nn2in >= INT_MAX || kkin >= INT_MAX) {
	/* large n -- evade integer overflow (and inappropriate algorithms)
	   -------- */
#ifdef DEBUG_rhyper
	REprintf("rhyper(nn1=%.0f, nn2=%.0f, kk=%.0f): 'large n' case\n", nn1in, nn2in, kkin);
#endif
        // FIXME: Much faster to give rbinom() approx when appropriate; -> see Kuensch(1989)
	// Johnson, Kotz,.. p.258 (top) mention the *four* different binomial approximations
	if(kkin == 1.) { // Bernoulli
	    return rbinom(kkin, nn1in / (nn1in + nn2in));
	}
	// Slow, but safe: return  F^{-1}(U)  where F(.) = phyper(.) and  U ~ U[0,1]
	return qhyper(unif_rand(), nn1in, nn2in, kkin,
		      /*lower_tail =*/ false, /*log_p = */ false);
	// lower_tail=false: a thinko, is still "correct" as equiv. to  U <--> 1-U
    }

    int
	nn1 = (int)nn1in,
	nn2 = (int)nn2in,
	kk  = (int)kkin;

    /* These should become 'thread_local globals' : */
    static int ks = -1, n1s = -1, n2s = -1;
    static int m, minjx, maxjx;
    static int k, n1, n2; // <- not allowing larger integer par
    static double N;

    bool setup1, setup2;
    /* if new parameter values, initialize */
    if (nn1 != n1s || nn2 != n2s) { // n1 | n2 is changed: setup all
	setup1 = true;	setup2 = true;
    } else if (kk != ks) { // n1 & n2 are unchanged: setup 'k' only
	setup1 = false;	setup2 = true;
    } else { // all three unchanged ==> no setup
	setup1 = false;	setup2 = false;
    }
    if (setup1) { // n1 & n2
	n1s = nn1; n2s = nn2; // save
	N = nn1 + (double)nn2; // avoid int overflow
	if (nn1 <= nn2) {
	    n1 = nn1; n2 = nn2;
	} else { // nn2 < nn1
	    n1 = nn2; n2 = nn1;
	}
	// now have n1 <= n2
    }
    if (setup2) { // k
	ks = kk; // save
	if ((double)kk + kk >= N) { // this could overflow
	    k = (int)(N - kk);
	} else {
	    k = kk;
	}
	// now have  k < N/2 = (n1+n2)/2
    }
    if (setup1 || setup2) {
	m = (int) ((k + 1.) * (n1 + 1.) / (N + 2.)); // m := floor(adjusted mean E[.])
	minjx = imax2(0, k - n2);
	maxjx = imin2(n1, k);
#ifdef DEBUG_rhyper
	REprintf("rhyper(n1=%d, n2=%d, k=%d), setup: floor(a.mean)=: m = %d, [min,maxjx]= [%d,%d]\n",
		 nn1, nn2, kk, m, minjx, maxjx);
#endif
    }
#ifdef DEBUG_rhyper
    else
	REprintf("rhyper(n1=%d, n2=%d, k=%d); setup UNchanged\n", nn1, nn2, kk);
#endif

    // generate random variate --- Three basic cases ---------------------------

    int ix; // return value (coerced to double at the very end) .. FIXME?? what if overflows

    if (minjx == maxjx) { /* I: degenerate distribution ---------------- */
#ifdef DEBUG_rhyper
	REprintf("rhyper(), branch I (degenerate): ix := maxjx = %d\n", maxjx);
#endif
	ix = maxjx;
	goto L_finis; // return appropriate variate

    } else if (m - minjx < 10) { // II: (Scaled) algorithm HIN (inverse transformation) ----
	const static double scale = 1e25; // scaling factor against (early) underflow
	const static double con = 57.5646273248511421;
	          // 25*log(10) = log(scale) { <==> exp(con) == scale }
	static double w,
	    lw; // = log(w);  w = exp(lw) * scale = exp(lw + log(scale)) = exp(lw + con)
	if (setup1 || setup2) {
	    // NB:  n1 <= n2  here
	    if (k < n2) {
		lw = afc(n2) + afc(n1 + n2 - k) - afc(n2 - k) - afc(n1 + n2);
	    } else {
		lw = afc(n1) + afc(     k     ) - afc(k - n2) - afc(n1 + n2);
	    }
	    w = exp(lw + con);
	}

#ifdef DEBUG_rhyper
	REprintf("  branch II; w = %g %s\n", w,
		 (w > 0) ? "> 0" : "= 0 <==> Underflow __NOT GOOD__");
#endif
	if(w <= 0.)
	    MATHLIB_ERROR(_("w = %g <= 0: Underflow in rhyper()  SHOULD NOT HAPPEN!"), w);
	// FIXME: MM has code "rhyper.c+M3" for switching to log space if w == 0

	double p, u;
      L10:
	p = w;
	ix = minjx;
	u = unif_rand() * scale;
#ifdef DEBUG_rhyper
	REprintf("  _new_ u = %g\n", u);
#endif
	while (u > p) {
	    u -= p;
	    p *= ((double) n1 - ix) * (k - ix);
	    ix++;
	    p = p / ix / (n2 - k + ix);
#ifdef DEBUG_rhyper
	    REprintf("       ix=%3d, u=%11g, p=%20.14g (u-p=%g)\n", ix, u, p, u-p);
#endif
	    if (ix > maxjx)
		goto L10;
	    // FIXME  if(p == 0.)  we also "have lost"  => goto L10
	}

    } else { /* III : H2PE Algorithm --------------------------------------- */

	static double a, xl, xr, lamdl, lamdr, p1, p2, p3;
	if (setup1 || setup2) {
	    double
		s = sqrt((N - k) * k * n1 * n2 / (N - 1) / N / N),

		/* remark: d is defined in reference without int. */
		/* the truncation centers the cell boundaries at 0.5 */
		d = (int) (1.5 * s) + .5;
	    xl = m - d + .5;
	    xr = m + d + .5;
	    a = afc(m) + afc(n1 - m) + afc(k - m) + afc(n2 - k + m);
	    double
		kl = exp(a - afc((int) (xl)) - afc((int) (n1 - xl))
			 - afc((int) (k - xl))
			 - afc((int) (n2 - k + xl))),
		kr = exp(a - afc((int) (xr - 1))
			 - afc((int) (n1 - xr + 1))
			 - afc((int) (k - xr + 1))
			 - afc((int) (n2 - k + xr - 1)));
	    lamdl = -log(xl * (n2 - k + xl) / (n1 - xl + 1) / (k - xl + 1));
	    lamdr = -log((n1 - xr + 1) * (k - xr + 1) / xr / (n2 - k + xr));
	    p1 = d + d;
	    p2 = p1 + kl / lamdl;
	    p3 = p2 + kr / lamdr;
#ifdef DEBUG_rhyper
	    REprintf("rhyper(), branch III {accept/reject}: (xl,xr)= (%g,%g); lamdl,r = (%g,%g)\n",
		     xl, xr, lamdl, lamdr);
	    REprintf(" --------> (p1,p2,p3) = (%g,%g,%g)\n", p1,p2,p3);
	}
	else
	    REprintf("rhyper(), branch III {accept/reject}: NO setup needed; lamdl,r = (%g,%g)\n",
		     lamdl, lamdr);
#else
        }
#endif

	/* acceptance/rejection test */
	bool reject = true;
        int n_uv = 0;
    do { // L30: -------------- acceptance / rejection : loop while reject=true -------------
	double
	    u = unif_rand() * p3,
	    v = unif_rand();
	n_uv++;
	if(n_uv >= 10000) {
	    REprintf("rhyper(*, n1=%d, n2=%d, k=%d): branch III: giving up after %d rejections\n",
		     nn1, nn2, kk, n_uv);
	    ML_WARN_return_NAN;
        }
#ifdef DEBUG_rhyper
	REprintf(" ... L30 [%d]: new (u=%g, v ~ U[0,1]=%g): ", n_uv, u,v);
#endif

	if (u < p1) {		/* rectangular region */
	    ix = (int) (xl + u);
#ifdef DEBUG_rhyper
	    REprintf(" rectangular: ix=%d\n", ix);
#endif
	} else if (u <= p2) {	/* left tail */
	    ix = (int) (xl + log(v) / lamdl);
#ifdef DEBUG_rhyper
	    REprintf(" left tail: ix=%d %s", ix, (ix < minjx)? "< minjx (=> L30)\n" : ">= minjx;");
#endif
	    if (ix < minjx)
		continue; // goto L30
	    v = v * (u - p1) * lamdl;
	} else {		/* right tail */
	    ix = (int) (xr - log(v) / lamdr);
#ifdef DEBUG_rhyper
	    REprintf(" right tail: ix=%d %s", ix, (ix > maxjx)? "> maxjx (=> L30)\n" : "< maxjx;");
#endif
	    if (ix > maxjx)
		continue; // goto L30
	    v = v * (u - p2) * lamdr;
	}
#ifdef DEBUG_rhyper
	if(u >= p1) REprintf(" new v = %g\n", v);
#endif

	if (m < 100 || ix <= 50) {
	    /* explicit evaluation */
	    /* The original algorithm (and TOMS 668) have
		   f = f * i * (n2 - k + i) / (n1 - i) / (k - i);
	       in the (m > ix) case, but the definition of the
	       recurrence relation on p134 shows that the +1 is
	       needed. */
	    double f = 1.0;
	    if (m < ix) {
		for (int i = m + 1; i <= ix; i++)
		    f = f * (n1 - i + 1) * (k - i + 1) / (n2 - k + i) / i;
	    } else if (m > ix) {
		for (int i = ix + 1; i <= m; i++)
		    f = f * i * (n2 - k + i) / (n1 - i + 1) / (k - i + 1);
	    }
#ifdef DEBUG_rhyper
	    REprintf(" small m or ix: f = %g\n", f);
#endif
	    if (v <= f) {
		reject = false;
	    }
	} else {

	    const static double deltal = 0.0078;
	    const static double deltau = 0.0034;

#ifdef DEBUG_rhyper
	    REprintf(" ... accept/reject 'large' case v=%g\n", v);
#endif
	    /* squeeze using upper and lower bounds */
	    double
		y = ix,
		y1 = y + 1.0,
		ym = y - m,
		yn = n1 - y + 1.0,
		yk = k  - y + 1.0,
		nk = n2 - k + y1,
		r = -ym / y1,
		s =  ym / yn,
		t =  ym / yk,
		e = -ym / nk,
		g = yn * yk / (y1 * nk) - 1.0,
		dg = (g < 0.0) ? 1. + g : 1.,
		gu = g * (1.0 + g * (-0.5 + g / 3.0)),
		gl = gu - .25 * (g * g * g * g) / dg,
		xm = m + 0.5,
		xn = n1 - m + 0.5,
		xk = k - m + 0.5,
		nm = n2 - k + xm,
		ub = y * gu - m * gl + deltau
		+ xm * r * (1. + r * (-0.5 + r / 3.0))
		+ xn * s * (1. + s * (-0.5 + s / 3.0))
		+ xk * t * (1. + t * (-0.5 + t / 3.0))
		+ nm * e * (1. + e * (-0.5 + e / 3.0));
	    /* test against upper bound */
	    double alv = log(v);
	    if (alv > ub) {
		reject = true;
	    } else {
		/* test against lower bound */
		double dr = xm * (r * r * r * r);
		if (r < 0.)
		    dr /= (1.0 + r);
		double ds = xn * (s * s * s * s);
		if (s < 0.0)
		    ds /= (1.0 + s);
		double dt = xk * (t * t * t * t);
		if (t < 0.0)
		    dt /= (1.0 + t);
		double de = nm * (e * e * e * e);
		if (e < 0.0)
		    de /= (1.0 + e);
		if (alv < ub - 0.25 * (dr + ds + dt + de)
		    + (y + m) * (gl - gu) - deltal) {
		    reject = false;
		}
		else {
		    /* * Stirling's formula to machine accuracy
		     */
		    if (alv <= (a - afc(ix) - afc(n1 - ix)
				- afc(k - ix) - afc(n2 - k + ix))) {
			reject = false;
		    } else {
			reject = true;
		    }
		}
	    }
	} // end{ III "large" }

    } while (reject);

    } // end{branch III}


L_finis:  /* return appropriate variate */

#ifdef DEBUG_rhyper
    REprintf(" L_finis: ix = %d, then", ix);
#endif
    if ((double)kk + kk >= N) {
	if (nn1 > nn2) {
	    ix = kk - nn2 + ix;
	} else {
	    ix = nn1 - ix;
	}
    } else if (nn1 > nn2) {
	ix = kk - ix;
    }
#ifdef DEBUG_rhyper
    REprintf(" %d\n", ix);
#endif
    return ix;
}
