/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 2026 The R Core Team
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
 *
 *  Arithmetic on BYTESXP vectors of the 'unsigned' and 'signed' kinds.
 *
 *  Storage is in native byte order, but every kernel here works on a
 *  scratch copy held most-significant-byte first.  That costs a copy
 *  per element and makes the algorithms readable in the usual
 *  schoolbook form, which for arithmetic that has to be exactly right
 *  at 128 bits is the trade worth making.  Widths with a native C type
 *  behind them could be specialized later.
 *
 *  Binary operands promote to max(width), which is a total order and so
 *  a far simpler lattice than R's usual one.  Kinds never mix.
 *  Overflow yields NA with a warning, matching integer overflow.
 */

#ifdef HAVE_CONFIG_H
# include <config.h>
#endif

#include <Defn.h>
#include <Internal.h>
#include <Rmath.h>

/* Arithmetic is defined only for the widths that correspond to an
   integer type someone might actually be carrying; wider elements stay
   pure storage. */
#define MAXW BYTEVEC_MAX_ARITH_WIDTH

typedef Rbyte mag_t[2 * MAXW];	/* MSB-first, room for a full product */

static bool arithWidthOK(int w)
{
    return w == 1 || w == 2 || w == 4 || w == 8 || w == 16;
}

/* storage order <-> most-significant-first */
static void toMSB(Rbyte *dst, const Rbyte *src, int w)
{
    for (int i = 0; i < w; i++) dst[i] = src[BYTEVEC_MSB(i, w)];
}

static void fromMSB(Rbyte *dst, const Rbyte *src, int w)
{
    for (int i = 0; i < w; i++) dst[BYTEVEC_MSB(i, w)] = src[i];
}

/* two's complement negate in place, MSB-first */
static void magNegate(Rbyte *a, int w)
{
    int carry = 1;

    for (int i = w - 1; i >= 0; i--) {
	int v = (int) ((Rbyte) ~a[i]) + carry;
	a[i] = (Rbyte) (v & 0xFF);
	carry = v >> 8;
    }
}

static int magCmp(const Rbyte *a, const Rbyte *b, int w)
{
    for (int i = 0; i < w; i++)
	if (a[i] != b[i]) return a[i] < b[i] ? -1 : 1;

    return 0;
}

static bool magIsZero(const Rbyte *a, int w)
{
    for (int i = 0; i < w; i++)
	if (a[i]) return false;

    return true;
}

/* a -= b, MSB-first, assumes a >= b */
static void magSub(Rbyte *a, const Rbyte *b, int w)
{
    int borrow = 0;

    for (int i = w - 1; i >= 0; i--) {
	int v = (int) a[i] - b[i] - borrow;
	if (v < 0) { v += 256; borrow = 1; } else borrow = 0;
	a[i] = (Rbyte) v;
    }
}

static void magShiftLeft1(Rbyte *a, int w)
{
    int carry = 0;

    for (int i = w - 1; i >= 0; i--) {
	int v = (a[i] << 1) | carry;
	a[i] = (Rbyte) (v & 0xFF);
	carry = (v >> 8) & 1;
    }
}

/* Sign of a signed element, and its magnitude in MSB-first form.
   Returns true if it was negative. */
static bool magFromElt(Rbyte *out, const Rbyte *p, int w, int kind)
{
    toMSB(out, p, w);

    if (kind == BYTEVEC_INT && (out[0] & 0x80)) {
	magNegate(out, w);
	return true;
    }

    return false;
}

/* Widen an element into a w-byte MSB-first buffer, zero- or
   sign-extending.  NA is never passed here -- callers handle it -- so
   the reserved patterns need no special case. */
static void widenMSB(Rbyte *out, int w, const Rbyte *p, int pw, int kind)
{
    Rbyte src[MAXW];
    toMSB(src, p, pw);

    Rbyte fill = (kind == BYTEVEC_INT && (src[0] & 0x80)) ? 0xFF : 0x00;
    int pad = w - pw;

    for (int i = 0; i < pad; i++) out[i] = fill;
    for (int i = 0; i < pw; i++) out[pad + i] = src[i];
}

/* Does an MSB-first value fit back into a w-byte element of this kind,
   without landing on the reserved NA pattern?  A result that is
   exactly the reserved value is reported as overflow: it is not
   representable, and saying so is better than silently producing NA. */
static bool resultFits(const Rbyte *v, int w, int kind, bool negative)
{
    if (kind == BYTEVEC_UINT) {
	/* v is the true magnitude; UINT_MAX is reserved */
	for (int i = 0; i < w; i++)
	    if (v[i] != 0xFF) return true;
	return false;
    }

    /* signed: magnitude must be < 2^(8w-1), or exactly 2^(8w-1) when
       negative -- but 2^(8w-1) negated is INT_MIN, which is reserved */
    if (!(v[0] & 0x80)) return true;
    if (!negative) return false;
    for (int i = 0; i < w; i++)
	if (v[i] != (i == 0 ? 0x80 : 0x00)) return false;
    return false;			/* exactly INT_MIN: reserved */
}

/* Write a signed/unsigned result given magnitude + sign.  Returns
   false on overflow. */
static bool storeResult(Rbyte *out, Rbyte *mag, int w, int kind, bool negative)
{
    if (!resultFits(mag, w, kind, negative)) return false;

    if (negative) magNegate(mag, w);
    fromMSB(out, mag, w);

    return true;
}

/* ---- the kernels; all take and return MSB-first magnitudes ---- */

static bool eltAdd(Rbyte *out, const Rbyte *a, const Rbyte *b, int w, int kind)
{
    Rbyte A[MAXW], B[MAXW], R[MAXW];
    toMSB(A, a, w); toMSB(B, b, w);

    unsigned int carry = 0;
    for (int i = w - 1; i >= 0; i--) {
	unsigned int v = (unsigned int) A[i] + B[i] + carry;
	R[i] = (Rbyte) (v & 0xFF);
	carry = v >> 8;
    }

    if (kind == BYTEVEC_UINT) {
	if (carry) return false;
	return storeResult(out, R, w, kind, false);
    }

    /* signed: overflow iff the operands shared a sign the result lacks */
    int sa = A[0] & 0x80, sb = B[0] & 0x80, sr = R[0] & 0x80;
    if (sa == sb && sr != sa) return false;

    bool neg = sr != 0;
    if (neg) magNegate(R, w);

    return storeResult(out, R, w, kind, neg);
}

static bool eltSub(Rbyte *out, const Rbyte *a, const Rbyte *b, int w, int kind)
{
    Rbyte A[MAXW], B[MAXW], R[MAXW];
    toMSB(A, a, w); toMSB(B, b, w);

    int borrow = 0;
    for (int i = w - 1; i >= 0; i--) {
	int v = (int) A[i] - B[i] - borrow;
	if (v < 0) { v += 256; borrow = 1; } else borrow = 0;
	R[i] = (Rbyte) v;
    }

    if (kind == BYTEVEC_UINT) {
	if (borrow) return false;	/* would be negative */
	return storeResult(out, R, w, kind, false);
    }

    int sa = A[0] & 0x80, sb = B[0] & 0x80, sr = R[0] & 0x80;
    if (sa != sb && sr != sa) return false;

    bool neg = sr != 0;
    if (neg) magNegate(R, w);

    return storeResult(out, R, w, kind, neg);
}

static bool eltMul(Rbyte *out, const Rbyte *a, const Rbyte *b, int w, int kind)
{
    Rbyte A[MAXW], B[MAXW];
    bool nega = magFromElt(A, a, w, kind);
    bool negb = magFromElt(B, b, w, kind);
    mag_t P;
    memset(P, 0, sizeof P);

    /* schoolbook, indices counted from the least significant end */
    for (int i = 0; i < w; i++) {
	unsigned int carry = 0;
	unsigned int ai = A[w - 1 - i];
	for (int j = 0; j < w; j++) {
	    int at = 2 * w - 1 - (i + j);
	    unsigned int cur = (unsigned int) P[at] + ai * B[w - 1 - j] + carry;
	    P[at] = (Rbyte) (cur & 0xFF);
	    carry = cur >> 8;
	}
	for (int at = 2 * w - 1 - (i + w); carry && at >= 0; at--) {
	    unsigned int cur = (unsigned int) P[at] + carry;
	    P[at] = (Rbyte) (cur & 0xFF);
	    carry = cur >> 8;
	}
    }

    for (int i = 0; i < w; i++)		/* high half must be empty */
	if (P[i]) return false;

    return storeResult(out, P + w, w, kind, nega != negb);
}

/* Bitwise long division on magnitudes; 8*w iterations is cheap enough
   at these widths and avoids a normalization step that is easy to get
   subtly wrong. */
static void magDivMod(Rbyte *q, Rbyte *r, const Rbyte *a, const Rbyte *b, int w)
{
    memset(q, 0, (size_t) w);
    memset(r, 0, (size_t) w);

    for (int bit = 0; bit < 8 * w; bit++) {
	magShiftLeft1(r, w);
	r[w - 1] |= (a[bit / 8] >> (7 - bit % 8)) & 1;
	if (magCmp(r, b, w) >= 0) {
	    magSub(r, b, w);
	    q[bit / 8] |= (Rbyte) (1 << (7 - bit % 8));
	}
    }
}

/* %/% is floor division and %% is the matching modulo, as for
   integers: the remainder takes the sign of the divisor. */
static bool eltDivMod(Rbyte *out, const Rbyte *a, const Rbyte *b, int w,
		      int kind, bool wantQuotient)
{
    Rbyte A[MAXW], B[MAXW], Q[MAXW], R[MAXW];
    bool nega = magFromElt(A, a, w, kind);
    bool negb = magFromElt(B, b, w, kind);

    if (magIsZero(B, w)) return false;	/* division by zero -> NA */

    magDivMod(Q, R, A, B, w);

    bool negq = (nega != negb);
    if (negq && !magIsZero(R, w)) {
	/* floor rather than truncate: step the quotient away from zero
	   and fold the difference back into the remainder */
	Rbyte one[MAXW];
	memset(one, 0, (size_t) w);
	one[w - 1] = 1;
	int carry = 1;
	for (int i = w - 1; i >= 0 && carry; i--) {
	    unsigned int v = (unsigned int) Q[i] + 1;
	    Q[i] = (Rbyte) (v & 0xFF);
	    carry = (v >> 8) != 0;
	}
	/* remainder becomes |b| - |r|, carrying the divisor's sign */
	Rbyte T[MAXW];
	memcpy(T, B, (size_t) w);
	magSub(T, R, w);
	memcpy(R, T, (size_t) w);
    }

    if (wantQuotient)
	return storeResult(out, Q, w, kind, negq);

    return storeResult(out, R, w, kind, negb);
}

/* Store a C integer value into an element.  Returns false if it is not
   representable -- a negative into an unsigned kind, a magnitude too
   wide, or the reserved NA pattern. */
static bool eltFromLong(Rbyte *out, long long v, int w, int kind)
{
    if (kind == BYTEVEC_UINT && v < 0) return false;

    bool neg = v < 0;
    unsigned long long m = neg
	? (unsigned long long) (-(v + 1)) + 1ULL
	: (unsigned long long) v;
    Rbyte mag[MAXW];

    memset(mag, 0, (size_t) w);
    for (int i = 0; i < w && m; i++) {
	mag[w - 1 - i] = (Rbyte) (m & 0xFF);
	m >>= 8;
    }
    if (m) return false;			/* needed more than w bytes */

    return storeResult(out, mag, w, kind, neg);
}

/* Narrow a logical or integer vector into a 'bytes' vector of the given
   kind and width.

   Only these two types narrow.  A double operand is deliberately
   refused by the callers rather than converted: R's coercion lattice is
   otherwise lossless, and neither double nor a 64-bit integer subsumes
   the other, so there is no answer that is right in general.  Refusing
   keeps both candidate rules -- widen to double, or narrow into bytes
   -- reachable later, since an operation that errors today can start
   working without breaking any code written in the meantime.

   Values that do not fit become NA with a warning, exactly as integer
   overflow does. */
attribute_hidden
SEXP R_bytesNarrow(SEXP x, int w, int kind, SEXP call)
{
    SEXPTYPE t = TYPEOF(x);

    if (kind == BYTEVEC_OPAQUE)
	errorcall(call,
		  _("cannot combine an opaque 'bytes' vector with type '%s'"),
		  R_typeToChar(x));
    if (t != INTSXP && t != LGLSXP)
	errorcall(call,
		  _("'%s' and '%s' cannot be combined; use an integer operand (1L), or as.numeric() for double arithmetic"),
		  "bytes", R_typeToChar(x));

    R_xlen_t n = XLENGTH(x);
    SEXP ans = PROTECT(R_allocBytesVectorKind(n, w, kind));
    R_xlen_t nLost = 0;

    for (R_xlen_t i = 0; i < n; i++) {
	int v = (t == INTSXP) ? INTEGER_ELT(x, i) : LOGICAL_ELT(x, i);
	Rbyte *p = BYTEVEC_ELT(ans, i);
	if (v == NA_INTEGER || !eltFromLong(p, (long long) v, w, kind)) {
	    R_bytesSetEltNA(p, w, kind);
	    if (v != NA_INTEGER) nLost++;
	}
    }

    if (nLost)
	warningcall(call, _("NAs introduced by values outside the range of '%s'"),
		    R_bytesTypeName(ans));
    UNPROTECT(1);

    return ans;
}

/* Settle the operands of a binary operation: at least one is a 'bytes'
   vector, and the other must be one too (same kind) or must narrow. */
static void bytesBinaryOperands(SEXP call, SEXP *px, SEXP *py, int *pw, int *pk)
{
    SEXP x = *px, y = *py;
    SEXP b = (TYPEOF(x) == BYTESXP) ? x : y;
    int kind = BYTEVEC_KIND(b), w = BYTEVEC_WIDTH(b);

    if (TYPEOF(x) == BYTESXP && TYPEOF(y) == BYTESXP) {
	if (BYTEVEC_KIND(x) != BYTEVEC_KIND(y))
	    errorcall(call, _("cannot combine 'bytes' vectors of different kinds"));
	w = BYTEVEC_WIDTH(x) > BYTEVEC_WIDTH(y)
	    ? BYTEVEC_WIDTH(x) : BYTEVEC_WIDTH(y);
    }
    else if (TYPEOF(x) == BYTESXP)
	*py = R_bytesNarrow(y, w, kind, call);
    else
	*px = R_bytesNarrow(x, w, kind, call);

    *pw = w;
    *pk = kind;
}

/* ---- vector level ---- */

attribute_hidden
SEXP R_bytesArith(SEXP call, int oper, SEXP x, SEXP y)
{
    if ((TYPEOF(x) == BYTESXP && BYTEVEC_KIND(x) == BYTEVEC_OPAQUE) ||
	(TYPEOF(y) == BYTESXP && BYTEVEC_KIND(y) == BYTEVEC_OPAQUE))
	errorcall(call, _("arithmetic is not defined for opaque 'bytes' vectors"));

    int w, kx;
    PROTECT_INDEX xi, yi;
    PROTECT_WITH_INDEX(x, &xi);
    PROTECT_WITH_INDEX(y, &yi);
    bytesBinaryOperands(call, &x, &y, &w, &kx);
    REPROTECT(x, xi);
    REPROTECT(y, yi);
    int ky = kx, wx = BYTEVEC_WIDTH(x), wy = BYTEVEC_WIDTH(y);
    if (!arithWidthOK(w))
	errorcall(call,
		  _("arithmetic on 'bytes' vectors is only defined for widths 1, 2, 4, 8 and 16"));

    R_xlen_t nx = XLENGTH(x), ny = XLENGTH(y);
    if (nx == 0 || ny == 0) {
	UNPROTECT(2); /* x, y */
	return R_allocBytesVectorKind(0, w, kx);
    }
    R_xlen_t n = nx > ny ? nx : ny;
    if (((nx > ny) ? nx % ny : ny % nx) != 0)
	warningcall(call,
		    _("longer object length is not a multiple of shorter object length"));

    SEXP ans = PROTECT(R_allocBytesVectorKind(n, w, kx));
    R_xlen_t nOver = 0;

    for (R_xlen_t i = 0; i < n; i++) {
	const Rbyte *px = BYTEVEC_ELT_RO(x, i % nx);
	const Rbyte *py = BYTEVEC_ELT_RO(y, i % ny);
	Rbyte *pa = BYTEVEC_ELT(ans, i);

	if (R_bytesEltIsNA(px, wx, kx) || R_bytesEltIsNA(py, wy, ky)) {
	    R_bytesSetEltNA(pa, w, kx);
	    continue;
	}

	/* promote both to the result width before operating */
	Rbyte ax[MAXW], ay[MAXW], sx[MAXW], sy[MAXW];
	widenMSB(ax, w, px, wx, kx);
	widenMSB(ay, w, py, wy, ky);
	fromMSB(sx, ax, w);
	fromMSB(sy, ay, w);

	bool ok;
	switch (oper) {
	case PLUSOP:  ok = eltAdd(pa, sx, sy, w, kx); break;
	case MINUSOP: ok = eltSub(pa, sx, sy, w, kx); break;
	case TIMESOP: ok = eltMul(pa, sx, sy, w, kx); break;
	case IDIVOP:  ok = eltDivMod(pa, sx, sy, w, kx, true); break;
	case MODOP:   ok = eltDivMod(pa, sx, sy, w, kx, false); break;
	default:
	    UNPROTECT(3); /* x, y, ans */
	    errorcall(call,
		      _("this operator is not defined for 'bytes' vectors"));
	}

	if (!ok) {
	    R_bytesSetEltNA(pa, w, kx);
	    nOver++;
	}
    }

    if (nOver)
	warningcall(call, (oper == IDIVOP || oper == MODOP)
		    ? _("NAs produced by division by zero or overflow")
		    : _("NAs produced by integer overflow"));

    UNPROTECT(3); /* x, y, ans */

    return ans;
}

attribute_hidden
SEXP R_bytesUnary(SEXP call, int oper, SEXP x)
{
    int k = BYTEVEC_KIND(x), w = BYTEVEC_WIDTH(x);

    if (k == BYTEVEC_OPAQUE)
	errorcall(call, _("arithmetic is not defined for opaque 'bytes' vectors"));
    if (oper == PLUSOP) return x;
    if (oper != MINUSOP)
	errorcall(call, _("invalid argument to unary operator"));
    if (k == BYTEVEC_UINT)
	errorcall(call, _("unary minus is not defined for unsigned 'bytes' vectors"));
    if (!arithWidthOK(w))
	errorcall(call,
		  _("arithmetic on 'bytes' vectors is only defined for widths 1, 2, 4, 8 and 16"));

    R_xlen_t n = XLENGTH(x);
    SEXP ans = PROTECT(R_allocBytesVectorKind(n, w, k));
    R_xlen_t nOver = 0;

    for (R_xlen_t i = 0; i < n; i++) {
	const Rbyte *px = BYTEVEC_ELT_RO(x, i);
	Rbyte *pa = BYTEVEC_ELT(ans, i);

	if (R_bytesEltIsNA(px, w, k)) {
	    R_bytesSetEltNA(pa, w, k);
	    continue;
	}

	Rbyte A[MAXW];
	bool neg = magFromElt(A, px, w, k);
	if (!storeResult(pa, A, w, k, !neg)) {
	    R_bytesSetEltNA(pa, w, k);
	    nOver++;
	}
    }

    if (nOver) warningcall(call, _("NAs produced by integer overflow"));
    UNPROTECT(1);

    return ans;
}

/* ---- numeric coercion ---- */

/* Exact for widths with a native type behind them; wider values are
   accumulated, which is why the precision warning below is not
   conditional on the width. */
attribute_hidden double R_bytesEltAsReal(const Rbyte *p, int w, int kind)
{
    Rbyte A[2 * MAXW];
    bool neg = false;

    if (w > (int) sizeof A) return NA_REAL;
    toMSB(A, p, w);
    if (kind == BYTEVEC_INT && (A[0] & 0x80)) {
	neg = true;
	magNegate(A, w);
    }

    double d = 0.0;
    for (int i = 0; i < w; i++) d = d * 256.0 + (double) A[i];

    return neg ? -d : d;
}

attribute_hidden
SEXP R_bytesCoerce(SEXP x, SEXPTYPE type)
{
    int w = BYTEVEC_WIDTH(x), k = BYTEVEC_KIND(x);
    R_xlen_t n = XLENGTH(x);

    if (k == BYTEVEC_OPAQUE)
	error(_("cannot coerce an opaque 'bytes' vector to type '%s'"),
	      type2char(type));

    SEXP ans = PROTECT(allocVector(type, n));
    R_xlen_t nLost = 0;

    for (R_xlen_t i = 0; i < n; i++) {
	const Rbyte *p = BYTEVEC_ELT_RO(x, i);

	if (R_bytesEltIsNA(p, w, k)) {
	    if (type == INTSXP) INTEGER0(ans)[i] = NA_INTEGER;
	    else REAL0(ans)[i] = NA_REAL;
	    continue;
	}

	double d = R_bytesEltAsReal(p, w, k);
	if (type == INTSXP) {
	    if (!(d >= (double) (INT_MIN + 1) && d <= (double) INT_MAX)) {
		INTEGER0(ans)[i] = NA_INTEGER;
		nLost++;
	    }
	    else INTEGER0(ans)[i] = (int) d;
	}
	else {
	    /* 2^53 is where a double stops being able to name every
	       integer; past it the value is approximate */
	    if (fabs(d) > 9007199254740992.0) nLost++;
	    REAL0(ans)[i] = d;
	}
    }

    if (nLost)
	warning(type == INTSXP
		? _("NAs introduced by coercion to integer range")
		: _("'bytes' values above 2^53 lose precision as double"));

    UNPROTECT(1);

    return ans;
}

/* ---- sum / prod / min / max ---- */

/* Handled here rather than inside do_summary's accumulator machinery:
   that machine keeps typed accumulators chosen from a fixed set, and a
   per-vector element width does not fit it.  A self-contained pass is
   both smaller and easier to argue correct. */
attribute_hidden
SEXP R_bytesSummary(SEXP call, int iop, SEXP args, bool narm)
{
    int kind = -1, w = 0;

    /* one pass to settle the result kind and width */
    for (SEXP t = args; t != R_NilValue; t = CDR(t)) {
	SEXP a = CAR(t);
	if (TAG(t) == R_NaRmSymbol) continue;
	if (TYPEOF(a) == NILSXP) continue;
	if (TYPEOF(a) != BYTESXP)
	    errorcall(call, _("cannot mix 'bytes' vectors with other types"));
	if (BYTEVEC_KIND(a) == BYTEVEC_OPAQUE)
	    errorcall(call,
		      _("'%s' is not defined for opaque 'bytes' vectors"),
		      iop == 0 ? "sum" : (iop == 4 ? "prod" :
					  (iop == 2 ? "min" : "max")));
	if (kind == -1) kind = BYTEVEC_KIND(a);
	else if (kind != BYTEVEC_KIND(a))
	    errorcall(call, _("cannot combine 'bytes' vectors of different kinds"));
	if (BYTEVEC_WIDTH(a) > w) w = BYTEVEC_WIDTH(a);
    }

    if (!arithWidthOK(w))
	errorcall(call,
		  _("arithmetic on 'bytes' vectors is only defined for widths 1, 2, 4, 8 and 16"));

    SEXP ans = PROTECT(R_allocBytesVectorKind(1, w, kind));
    Rbyte *acc = BYTEVEC_ELT(ans, 0);
    bool seen = false, isNA = false, over = false;

    /* sum starts at 0, prod at 1; min/max take the first element seen */
    if (iop == 4) {
	Rbyte one[MAXW];
	memset(one, 0, (size_t) w);
	one[w - 1] = 1;
	fromMSB(acc, one, w);
    }

    for (SEXP t = args; t != R_NilValue && !isNA; t = CDR(t)) {
	SEXP a = CAR(t);
	if (TAG(t) == R_NaRmSymbol || TYPEOF(a) == NILSXP) continue;

	int aw = BYTEVEC_WIDTH(a);
	for (R_xlen_t i = 0; i < XLENGTH(a); i++) {
	    const Rbyte *p = BYTEVEC_ELT_RO(a, i);

	    if (R_bytesEltIsNA(p, aw, kind)) {
		if (narm) continue;
		isNA = true;
		break;
	    }

	    Rbyte wide[MAXW], cur[MAXW];
	    widenMSB(wide, w, p, aw, kind);
	    fromMSB(cur, wide, w);

	    if (!seen && (iop == 2 || iop == 3)) {
		memcpy(acc, cur, (size_t) w);
		seen = true;
		continue;
	    }
	    seen = true;

	    switch (iop) {
	    case 0:
		if (!eltAdd(acc, acc, cur, w, kind)) over = true;
		break;
	    case 4:
		if (!eltMul(acc, acc, cur, w, kind)) over = true;
		break;
	    case 2:
		if (R_bytesEltCmp(cur, acc, w, kind) < 0)
		    memcpy(acc, cur, (size_t) w);
		break;
	    case 3:
		if (R_bytesEltCmp(cur, acc, w, kind) > 0)
		    memcpy(acc, cur, (size_t) w);
		break;
	    default:
		UNPROTECT(1);
		errorcall(call, _("this summary is not defined for 'bytes' vectors"));
	    }
	    if (over) { isNA = true; break; }
	}
    }

    if ((iop == 2 || iop == 3) && !seen) {
	UNPROTECT(1);
	errorcall(call, _("no non-missing arguments, returning NA"));
    }
    if (isNA)
	R_bytesSetEltNA(acc, w, kind);
    if (over)
	warningcall(call, _("NAs produced by integer overflow"));

    UNPROTECT(1);

    return ans;
}
