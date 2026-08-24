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
 *  Arithmetic on ALTSXP vectors of the 'unsigned' and 'signed' kinds.
 *
 *  Storage is in native byte order, but every kernel here works on a
 *  scratch copy held most-significant-byte first.  That costs a copy
 *  per element and makes the algorithms readable in the usual
 *  schoolbook form, which for arithmetic that has to be exactly right
 *  at 128 bits is the trade worth making.  They remain the definition
 *  of what these operations mean; the widths with a native C type
 *  behind them are dispatched to that type first, and the two paths
 *  are checked against each other.
 *
 *  Two extended-integer operands must have the same width, kind and NA
 *  reservation.  Integer and logical operands narrow into that type.
 *  Overflow yields NA with a warning, matching integer overflow.
 */

#ifdef HAVE_CONFIG_H
# include <config.h>
#endif

#include <stdint.h>
#include <float.h>

#include <Defn.h>
#include <Internal.h>
#include <Rmath.h>
#include <R_ext/Itermacros.h>  /* MOD_ITERATE2 */

/* Every width an element may have is an arithmetic width -- see
   XINT_WIDTH_OK() -- so the scratch buffers below are simply the widest
   element there is, and no operation has to gate on the width first.
   There used to be a second, smaller bound for arithmetic and a check
   at every entry point to enforce it; both are gone with the widths
   that needed them. */
typedef Rbyte mag_t[2 * XINT_MAX_WIDTH];  /* MSB-first, a full product */

/* ---- native kernels ------------------------------------------------

   The kernels further down work a byte at a time.  That is what makes
   every width work and what makes them readable, and it is a poor way
   to add two numbers the machine can add in one instruction.  Division
   is worse still: the general path runs 8*w iterations of bitwise long
   division.

   So for the widths that have a C integer type behind them the work is
   handed to that type.  Native storage is what makes this cheap -- an
   element already holds the machine's own byte order, so reading one is
   a load rather than a shuffle -- and it is the payoff for a decision
   taken for ingest.

   Nothing else changes.  The general kernels remain the definition of
   what these mean, and the two must agree; tests/xintsxp-dev/archeck.R checks
   both against Python's exact integers. */

#if defined(__has_builtin)
# if __has_builtin(__builtin_add_overflow) &&	\
     __has_builtin(__builtin_sub_overflow) &&	\
     __has_builtin(__builtin_mul_overflow)
#  define XINT_NATIVE_ARITH 1
# endif
#elif defined(__GNUC__) && __GNUC__ >= 5
# define XINT_NATIVE_ARITH 1
#endif

#ifdef XINT_NATIVE_ARITH

# ifdef __SIZEOF_INT128__
typedef unsigned __int128 xint_uint128_t;
typedef __int128 xint_int128_t;
#  define XINT_NATIVE_W16(BODY, T) case 16: BODY(T)
# else
#  define XINT_NATIVE_W16(BODY, T)	/* width 16 stays on the general path */
# endif

/* Where a native type covers every arithmetic width, the general
   kernels become unreachable -- and unreachable code is where bugs
   settle in unnoticed.  Setting R_XINT_GENERIC_ARITH runs them
   anyway, so the two paths can be compared with each other and with
   the exact-integer reference.  One predictable branch per element. */
static bool useNative(void)
{
    static int cached = -1;

    if (cached < 0) {
	/* an empty or false value counts as unset, so that a caller can
	   hand a child process the native setting without having to
	   remove the variable from the environment it inherited.  Empty
	   is the one that bites: getenv() reports it as set, and reading
	   it that way once had the cross-check in archeck.R comparing
	   the general path with itself. */
	const char *e = getenv("R_XINT_GENERIC_ARITH");
	cached = (e == NULL || *e == '\0' || !strcmp(e, "0") ||
		  StringFalse(e));
    }

    return cached != 0;
}

#endif	/* XINT_NATIVE_ARITH */

#ifdef XINT_NATIVE_ARITH

/* The last two steps every body shares.  A result landing on the value
   this vector reserves for NA is not representable, and saying so beats
   returning a silent NA; the element's bytes are the native object's
   bytes, so the reserved-pattern test reads them where they lie. */
static bool storeNative(Rbyte *out, const void *v, int w, int kind,
			bool hasNA)
{
    if (hasNA && R_xintEltIsNAFast((const Rbyte *) v, w, kind)) return false;
    memcpy(out, v, (size_t) w);

    return true;
}

/* Expand UBODY once per unsigned width and SBODY once per signed width
   that has a native type.  Every body ends in a return, so falling out
   of the switch means "not handled here" and the general kernel runs.
   Multiply does not come through here; see XINT_NATIVE_MUL(). */
# define XINT_NATIVE2(UBODY, SBODY, w, kind)			\
    do {							\
	if (!useNative()) break;				\
	if ((kind) == XINT_UNSIGNED)				\
	    switch (w) {					\
	    case 1:  UBODY(uint8_t)				\
	    case 2:  UBODY(uint16_t)				\
	    case 4:  UBODY(uint32_t)				\
	    case 8:  UBODY(uint64_t)				\
	    XINT_NATIVE_W16(UBODY, xint_uint128_t)		\
	    }							\
	else if ((kind) == XINT_SIGNED)				\
	    switch (w) {					\
	    case 1:  SBODY(int8_t)				\
	    case 2:  SBODY(int16_t)				\
	    case 4:  SBODY(int32_t)				\
	    case 8:  SBODY(int64_t)				\
	    XINT_NATIVE_W16(SBODY, xint_int128_t)		\
	    }							\
    } while (0)

/* the usual case: the two kinds differ only in the C type */
# define XINT_NATIVE(BODY, w, kind) XINT_NATIVE2(BODY, BODY, w, kind)

# define XINT_LOAD2(T)				\
    T va, vb, vr;				\
    memcpy(&va, a, sizeof(T));			\
    memcpy(&vb, b, sizeof(T));

# define XINT_ADD_BODY(T)						\
    { XINT_LOAD2(T)							\
      if (__builtin_add_overflow(va, vb, &vr)) return false;		\
      return storeNative(out, &vr, w, kind, hasNA); }

# define XINT_SUB_BODY(T)						\
    { XINT_LOAD2(T)							\
      if (__builtin_sub_overflow(va, vb, &vr)) return false;		\
      return storeNative(out, &vr, w, kind, hasNA); }

# define XINT_MUL_BODY(T)						\
    { XINT_LOAD2(T)							\
      if (__builtin_mul_overflow(va, vb, &vr)) return false;		\
      return storeNative(out, &vr, w, kind, hasNA); }

# ifdef __SIZEOF_INT128__
/* The signed multiply through a 128-bit product: the widening multiply
   is two instructions on a target that has them, and there is no
   checked multiply left for the compiler to turn into a call. */
#  define XINT_MUL_S64_BODY(T)						\
    { XINT_LOAD2(T)							\
      xint_int128_t p = (xint_int128_t) va * vb;			\
      xint_int128_t lim = (xint_int128_t) 1 << (8 * sizeof(T) - 1);	\
      if (p < -lim || p >= lim) return false;				\
      vr = (T) p;							\
      return storeNative(out, &vr, w, kind, hasNA); }

/* The widest signed multiply has nothing above it to promote into, so
   its check is written out: magnitudes in the unsigned domain, where
   wrapping is defined.  It costs a 128-bit division, still far less
   than the general kernel's 256 byte multiplies. */
static bool mulOverflowS128(xint_int128_t a, xint_int128_t b,
			    xint_int128_t *r)
{
    xint_uint128_t ua = (xint_uint128_t) a, ub = (xint_uint128_t) b;
    xint_uint128_t ma = a < 0 ? (xint_uint128_t) 0 - ua : ua;
    xint_uint128_t mb = b < 0 ? (xint_uint128_t) 0 - ub : ub;

    /* the magnitude a product may reach: one more when it is negative */
    xint_uint128_t lim = (xint_uint128_t) 1 << 127;
    if ((a < 0) == (b < 0)) lim--;

    if (mb != 0 && ma > lim / mb) return true;
    *r = (xint_int128_t) (ua * ub);	/* in range, so this is the product */

    return false;
}

#  define XINT_MUL_S128_BODY(T)					\
    { XINT_LOAD2(T)							\
      if (mulOverflowS128(va, vb, &vr)) return false;			\
      return storeNative(out, &vr, w, kind, hasNA); }

#  define XINT_MUL_W8_SIGNED	case 8:  XINT_MUL_S64_BODY(int64_t)
# else
#  define XINT_MUL_W8_SIGNED	/* signed width 8 stays general too */
# endif

/* Multiply dispatches on its own, because which body a width wants is
   not settled by its type alone.  A checked *signed* multiply of a type
   the target cannot multiply in one instruction is the one thing here
   that lowers to a call -- compiler-rt's __mulo?i4, which a clang
   configured against libgcc, the default on Debian, does not have, so
   linking R fails.  That is what clang 19 does at width 16 on aarch64,
   and it is __mulodi4 at width 8 on a 32-bit target.

   Widths up to 4 are safe wherever R runs.  Width 8 goes through a
   128-bit product instead, and a target with no 128-bit type has no
   64-bit multiply either, so there it takes the general kernel.  Width
   16 has nothing above it to promote into and checks itself.  Unsigned
   is never affected: compiler-rt has no unsigned counterpart to call.

   The line is drawn in the preprocessor rather than by a
   `sizeof(T) <= 8` the optimiser would fold, because the branch not
   taken is still compiled, and at -O0 nothing would remove the call. */
# define XINT_NATIVE_MUL(w, kind)				\
    do {							\
	if (!useNative()) break;				\
	if ((kind) == XINT_UNSIGNED)				\
	    switch (w) {					\
	    case 1:  XINT_MUL_BODY(uint8_t)			\
	    case 2:  XINT_MUL_BODY(uint16_t)			\
	    case 4:  XINT_MUL_BODY(uint32_t)			\
	    case 8:  XINT_MUL_BODY(uint64_t)			\
	    XINT_NATIVE_W16(XINT_MUL_BODY, xint_uint128_t)	\
	    }							\
	else if ((kind) == XINT_SIGNED)				\
	    switch (w) {					\
	    case 1:  XINT_MUL_BODY(int8_t)			\
	    case 2:  XINT_MUL_BODY(int16_t)			\
	    case 4:  XINT_MUL_BODY(int32_t)			\
	    XINT_MUL_W8_SIGNED					\
	    XINT_NATIVE_W16(XINT_MUL_S128_BODY, xint_int128_t) \
	    }							\
    } while (0)

/* 0 - v, which overflows only at the most negative value -- and, for
   an unsigned element, at every value but zero */
# define XINT_NEG_BODY(T)						\
    { T va, vr;								\
      memcpy(&va, a, sizeof(T));					\
      if (__builtin_sub_overflow((T) 0, va, &vr)) return false;		\
      return storeNative(out, &vr, w, kind, hasNA); }

/* unsigned: no signs to reconcile, so C's / and % are already the
   floor division and modulo that %/% and %% mean */
# define XINT_DIVMOD_U_BODY(T)						\
    { XINT_LOAD2(T)							\
      if (vb == 0) return false;					\
      vr = wantQuotient ? (T) (va / vb) : (T) (va % vb);		\
      return storeNative(out, &vr, w, kind, hasNA); }

/* signed: C truncates towards zero, so a quotient with a nonzero
   remainder steps down by one and the remainder takes the divisor's
   sign.  T_MIN / -1 is the one division C leaves undefined; its
   quotient overflows, while its remainder is a perfectly good zero. */
# define XINT_DIVMOD_S_BODY(T)						\
    { XINT_LOAD2(T)							\
      if (vb == 0) return false;					\
      if (vb == (T) -1) {						\
	  if (!wantQuotient) vr = 0;					\
	  else if (__builtin_sub_overflow((T) 0, va, &vr)) return false;\
	  return storeNative(out, &vr, w, kind, hasNA);			\
      }									\
      T q = (T) (va / vb), r = (T) (va % vb);				\
      if (r != 0 && ((r < 0) != (vb < 0))) { q--; r = (T) (r + vb); }	\
      vr = wantQuotient ? q : r;					\
      return storeNative(out, &vr, w, kind, hasNA); }

#else	/* no overflow builtins: everything takes the general path */

# define XINT_NATIVE2(UBODY, SBODY, w, kind)	do { } while (0)
# define XINT_NATIVE(BODY, w, kind)		do { } while (0)
# define XINT_NATIVE_MUL(w, kind)		do { } while (0)

#endif

/* storage order <-> most-significant-first */
static void toMSB(Rbyte *dst, const Rbyte *src, int w)
{
    for (int i = 0; i < w; i++) dst[i] = src[XINT_MSB(i, w)];
}

static void fromMSB(Rbyte *dst, const Rbyte *src, int w)
{
    for (int i = 0; i < w; i++) dst[XINT_MSB(i, w)] = src[i];
}

/* Two's complement negate in place, MSB-first.  Shared with the text
   parser and the decimal renderer in xints.c, as R_xintMagFits() is:
   a carry fix here must reach them, or as.xinteger(as.character(x)) == x
   quietly breaks for negative values. */
attribute_hidden void R_xintMagNegate(Rbyte *a, int w)
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

/* a += b, MSB-first; the caller guarantees no carry out of a[0] */
static void magAdd(Rbyte *a, const Rbyte *b, int w)
{
    unsigned int carry = 0;

    for (int i = w - 1; i >= 0; i--) {
	unsigned int v = (unsigned int) a[i] + b[i] + carry;
	a[i] = (Rbyte) (v & 0xFF);
	carry = v >> 8;
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

    if (kind == XINT_SIGNED && (out[0] & 0x80)) {
	R_xintMagNegate(out, w);
	return true;
    }

    return false;
}

/* Does an MSB-first value fit back into a w-byte element of this kind,
   without landing on the reserved NA pattern?  A result that is
   exactly the reserved value is reported as overflow: it is not
   representable, and saying so is better than silently producing NA.

   hasNA is the vector's own answer to whether anything is reserved at
   all: with na = FALSE every bit pattern of the width is a value, so
   the top of the range is reachable rather than overflow.

   Shared with the text parser in xints.c rather than restated there:
   which values a width admits is subtle enough that two copies would
   eventually disagree. */
attribute_hidden
bool R_xintMagFits(const Rbyte *v, int w, int kind, bool negative, bool hasNA)
{
    if (kind == XINT_UNSIGNED) {
	/* v is the true magnitude; UINT_MAX is reserved */
	if (!hasNA) return true;
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

    return !hasNA;			/* exactly INT_MIN */
}

/* Write a signed/unsigned result given magnitude + sign.  Returns
   false on overflow. */
static bool storeResult(Rbyte *out, Rbyte *mag, int w, int kind, bool negative, bool hasNA)
{
    if (!R_xintMagFits(mag, w, kind, negative, hasNA)) return false;

    if (negative) R_xintMagNegate(mag, w);
    fromMSB(out, mag, w);

    return true;
}

/* ---- the kernels; all take and return MSB-first magnitudes ---- */

static bool eltAdd(Rbyte *out, const Rbyte *a, const Rbyte *b, int w, int kind, bool hasNA)
{
    XINT_NATIVE(XINT_ADD_BODY, w, kind);

    Rbyte A[XINT_MAX_WIDTH], B[XINT_MAX_WIDTH], R[XINT_MAX_WIDTH];
    toMSB(A, a, w); toMSB(B, b, w);

    unsigned int carry = 0;
    for (int i = w - 1; i >= 0; i--) {
	unsigned int v = (unsigned int) A[i] + B[i] + carry;
	R[i] = (Rbyte) (v & 0xFF);
	carry = v >> 8;
    }

    if (kind == XINT_UNSIGNED) {
	if (carry) return false;
	return storeResult(out, R, w, kind, false, hasNA);
    }

    /* signed: overflow iff the operands shared a sign the result lacks */
    int sa = A[0] & 0x80, sb = B[0] & 0x80, sr = R[0] & 0x80;
    if (sa == sb && sr != sa) return false;

    bool neg = sr != 0;
    if (neg) R_xintMagNegate(R, w);

    return storeResult(out, R, w, kind, neg, hasNA);
}

static bool eltSub(Rbyte *out, const Rbyte *a, const Rbyte *b, int w, int kind, bool hasNA)
{
    XINT_NATIVE(XINT_SUB_BODY, w, kind);

    Rbyte A[XINT_MAX_WIDTH], B[XINT_MAX_WIDTH], R[XINT_MAX_WIDTH];
    toMSB(A, a, w); toMSB(B, b, w);

    int borrow = 0;
    for (int i = w - 1; i >= 0; i--) {
	int v = (int) A[i] - B[i] - borrow;
	if (v < 0) { v += 256; borrow = 1; } else borrow = 0;
	R[i] = (Rbyte) v;
    }

    if (kind == XINT_UNSIGNED) {
	if (borrow) return false;	/* would be negative */
	return storeResult(out, R, w, kind, false, hasNA);
    }

    int sa = A[0] & 0x80, sb = B[0] & 0x80, sr = R[0] & 0x80;
    if (sa != sb && sr != sa) return false;

    bool neg = sr != 0;
    if (neg) R_xintMagNegate(R, w);

    return storeResult(out, R, w, kind, neg, hasNA);
}

static bool eltMul(Rbyte *out, const Rbyte *a, const Rbyte *b, int w, int kind, bool hasNA)
{
    XINT_NATIVE_MUL(w, kind);

    Rbyte A[XINT_MAX_WIDTH], B[XINT_MAX_WIDTH];
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

    return storeResult(out, P + w, w, kind, nega != negb, hasNA);
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
static bool eltDivMod(Rbyte *out, const Rbyte *a, const Rbyte *b, int w, int kind, bool wantQuotient, bool hasNA)
{
    XINT_NATIVE2(XINT_DIVMOD_U_BODY, XINT_DIVMOD_S_BODY, w, kind);

    Rbyte A[XINT_MAX_WIDTH], B[XINT_MAX_WIDTH];
    Rbyte Q[XINT_MAX_WIDTH], R[XINT_MAX_WIDTH];
    bool nega = magFromElt(A, a, w, kind);
    bool negb = magFromElt(B, b, w, kind);

    if (magIsZero(B, w)) return false;	/* division by zero -> NA */

    magDivMod(Q, R, A, B, w);

    bool negq = (nega != negb);
    if (negq && !magIsZero(R, w)) {
	/* floor rather than truncate: step the quotient away from zero
	   and fold the difference back into the remainder */
	int carry = 1;
	for (int i = w - 1; i >= 0 && carry; i--) {
	    unsigned int v = (unsigned int) Q[i] + 1;
	    Q[i] = (Rbyte) (v & 0xFF);
	    carry = (v >> 8) != 0;
	}
	/* remainder becomes |b| - |r|, carrying the divisor's sign */
	Rbyte T[XINT_MAX_WIDTH];
	memcpy(T, B, (size_t) w);
	magSub(T, R, w);
	memcpy(R, T, (size_t) w);
    }

    if (wantQuotient)
	return storeResult(out, Q, w, kind, negq, hasNA);

    return storeResult(out, R, w, kind, negb, hasNA);
}

/* A zero divisor.  Zero is all zero bytes for both numeric kinds, and
   is never the reserved NA pattern (all ones, or the most negative
   value), so the bytes alone settle it. */
static bool eltIsZero(const Rbyte *p, int w)
{
    for (int i = 0; i < w; i++)
	if (p[i]) return false;

    return true;
}

/* Unary minus, which R_xintUnary() used to do inline.  A kernel of
   its own so that it dispatches to a native type the way the binary
   ones do; the general form is a negation of the magnitude, whose sign
   flips. */
static bool eltNeg(Rbyte *out, const Rbyte *a, int w, int kind, bool hasNA)
{
    /* 0 - v overflows at every nonzero value of an unsigned element.
       The native body reports that; the general path below would store
       the wrapped magnitude instead, since storeResult() does not ask
       about the sign for this kind.  R_xintUnary() refuses unsigned
       before reaching either, but the two kernels are checked against
       each other and must agree without relying on that. */
    if (kind == XINT_UNSIGNED) {
	if (!eltIsZero(a, w)) return false;
	memset(out, 0, (size_t) w);	/* -0 is 0, and never the reserved value */
	return true;
    }

    XINT_NATIVE(XINT_NEG_BODY, w, kind);

    Rbyte A[XINT_MAX_WIDTH];
    bool neg = magFromElt(A, a, w, kind);

    return storeResult(out, A, w, kind, !neg, hasNA);
}

/* Store a C integer value into an element.  Returns false if it is not
   representable -- a negative into an unsigned kind, a magnitude too
   wide, or the reserved NA pattern.

   Reached from c(), subassignment and comparison as well as from
   arithmetic, none of which require the value to fit a native type. */
static bool eltFromLong(Rbyte *out, long long v, int w, int kind, bool hasNA)
{
    if (kind == XINT_UNSIGNED && v < 0) return false;

    bool neg = v < 0;
    unsigned long long m = neg
	? (unsigned long long) (-(v + 1)) + 1ULL
	: (unsigned long long) v;
    Rbyte mag[XINT_MAX_WIDTH];

    memset(mag, 0, (size_t) w);
    for (int i = 0; i < w && m; i++) {
	mag[w - 1 - i] = (Rbyte) (m & 0xFF);
	m >>= 8;
    }
    if (m) return false;			/* needed more than w bytes */

    return storeResult(out, mag, w, kind, neg, hasNA);
}

/* ---- conversion between 'xinteger' types ---- */

/* One element from (iw, ik) to (ow, ok), preserving the value.  Returns
   false if it does not fit, which includes landing on the reserved NA
   pattern.

   Widening and narrowing are defined between any two of the widths, in
   either direction; only the value has to survive. */
static bool eltConvert(Rbyte *out, int ow, int ok, bool ohasNA,
		       const Rbyte *in, int iw, int ik)
{
    Rbyte mag[XINT_MAX_WIDTH];
    bool neg = magFromElt(mag, in, iw, ik);

    if (iw > ow) {
	/* narrowing: what falls off the top must be nothing */
	for (int i = 0; i < iw - ow; i++)
	    if (mag[i]) return false;
	memmove(mag, mag + (iw - ow), (size_t) ow);
    }
    else if (iw < ow) {
	memmove(mag + (ow - iw), mag, (size_t) iw);
	memset(mag, 0, (size_t) (ow - iw));
    }

    /* magFromElt() only reports neg for a nonzero magnitude, so this is
       not rejecting a negative zero */
    if (neg && ok == XINT_UNSIGNED) return false;

    if (!R_xintMagFits(mag, ow, ok, neg, ohasNA)) return false;
    if (neg) R_xintMagNegate(mag, ow);
    fromMSB(out, mag, ow);

    return true;
}

/* x converted to another width, kind, or NA reservation. */
attribute_hidden
SEXP R_xintFromXInt(SEXP x, int w, int kind, int hasNA, SEXP call)
{
    int xw = XINT_WIDTH(x), xk = XINT_KIND(x);
    bool xNA = XINT_HAS_NA(x);
    bool sameType = (xw == w && xk == kind);

    R_xlen_t n = XLENGTH(x);
    SEXP ans = PROTECT(R_allocXIntVector(n, w, kind, hasNA ? TRUE : FALSE));
    R_xlen_t nLost = 0, nReserved = 0;

    for (R_xlen_t i = 0; i < n; i++) {
	const Rbyte *p = XINT_ELT_RO(x, i);
	Rbyte *o = XINT_ELT(ans, i);

	if (xNA && R_xintEltIsNA(p, xw, xk)) {
	    R_xintCheckNA(ans);	/* the target reserves nothing */
	    R_xintSetEltNA(o, w, kind);
	    continue;
	}

	if (sameType) {
	    /* only the reservation changed; a value that now collides
	       with it becomes NA, as it does arriving from raw bytes */
	    memcpy(o, p, (size_t) w);
	    if (hasNA && R_xintEltIsNA(o, w, kind)) nReserved++;
	    continue;
	}

	if (!eltConvert(o, w, kind, hasNA != 0, p, xw, xk)) {
	    R_xintCheckNA(ans);
	    R_xintSetEltNA(o, w, kind);
	    nLost++;
	}
    }

    if (nLost)
	warningcall(call, _("NAs introduced by values outside the range of '%s'"),
		    R_xintTypeName(ans));
    if (nReserved) R_xintWarnReservedCount(nReserved);

    UNPROTECT(1);

    return ans;
}

/* Which types a foreign operand may have, with one wording for the
   refusals.  Integer and logical narrow.  Value-producing operations deliberately
   turn everything else away: neither double nor a 64-bit integer
   subsumes the other, so there is no lossless result type in general.
   Comparisons and matching use their own exact, non-value-producing
   settlement rules below. */
attribute_hidden
void R_xintCheckOperand(SEXP x, SEXP call)
{
    if (TYPEOF(x) != INTSXP && TYPEOF(x) != LGLSXP)
	errorcall(call,
		  _("'%s' and '%s' cannot be combined; use an integer operand (1L), or as.numeric() for double arithmetic"),
		  "xinteger", R_typeToChar(x));
}

/* Narrow a logical or integer vector into an 'xinteger' vector of the given
   kind and width; anything else is refused above.  Values that do not
   fit become NA with a warning, exactly as integer overflow does. */
attribute_hidden
SEXP R_xintNarrow(SEXP x, int w, int kind, int hasNA, SEXP call)
{
    SEXPTYPE t = TYPEOF(x);

    R_xintCheckOperand(x, call);

    R_xlen_t n = XLENGTH(x);
    SEXP ans = PROTECT(R_allocXIntVector(n, w, kind, hasNA ? TRUE : FALSE));
    R_xlen_t nLost = 0;

    for (R_xlen_t i = 0; i < n; i++) {
	int v = (t == INTSXP) ? INTEGER_ELT(x, i) : LOGICAL_ELT(x, i);
	Rbyte *p = XINT_ELT(ans, i);
	if (v == NA_INTEGER || !eltFromLong(p, (long long) v, w, kind, hasNA != 0)) {
	    /* The two failures are different mistakes, and with nothing
	       reserved they earn different errors: a missing operand is
	       R_xintCheckNA()'s to report, while an out-of-range one is
	       not missing and must not be reported as if it were. */
	    if (v != NA_INTEGER && !hasNA)
		errorcall(call,
			  _("value %d is outside the range of '%s'; it was created with na = FALSE, so there is no NA to produce"),
			  v, R_xintTypeName(ans));
	    R_xintCheckNA(ans);
	    R_xintSetEltNA(p, w, kind);
	    if (v != NA_INTEGER) nLost++;
	}
    }

    if (nLost)
	warningcall(call, _("NAs introduced by values outside the range of '%s'"),
		    R_xintTypeName(ans));
    UNPROTECT(1);

    return ans;
}

/* Narrow a logical or integer vector for COMPARISON rather than for
   value.  R_xintNarrow() turns an operand the type cannot hold into
   NA, which is the right answer where the result has to be an element
   of the type -- x + 1000L on a uint8 has none.  A comparison always
   has one: an operand outside the range lies below or above every
   element, whatever the elements are.  So out-of-range operands are
   reported in dir[] instead, as -1 (below) or +1 (above), no NA is
   introduced and no warning given; the element left in the vector for
   them is zero bytes, which is never the reserved NA pattern and is
   never read.

   An NA operand is the same story.  The result of a comparison or a
   match is never an element of the type, so an NA on the way in needs
   somewhere to be recorded, not a value to become: where the type
   reserves a pattern it is used and dir[] stays 0, and where it does
   not, dir[] carries XINT_CMP_NA.  Refusing instead would make
   `x == NA` and `NA %in% x` errors on a vector created with na = FALSE,
   where every other type answers NA or FALSE.

   The reserved pattern counts as out of range, and in the direction it
   sits: a vector that reserves it holds no element equal to it, so
   every element is below UINT_MAX and above INT_MIN. */
attribute_hidden
SEXP R_xintNarrowCmp(SEXP x, int w, int kind, int hasNA, int *dir, SEXP call)
{
    SEXPTYPE t = TYPEOF(x);

    R_xintCheckOperand(x, call);

    R_xlen_t n = XLENGTH(x);
    SEXP ans = PROTECT(R_allocXIntVector(n, w, kind, hasNA ? TRUE : FALSE));

    for (R_xlen_t i = 0; i < n; i++) {
	int v = (t == INTSXP) ? INTEGER_ELT(x, i) : LOGICAL_ELT(x, i);
	Rbyte *p = XINT_ELT(ans, i);

	dir[i] = 0;
	if (v == NA_INTEGER) {
	    if (hasNA)
		R_xintSetEltNA(p, w, kind);
	    else {
		dir[i] = XINT_CMP_NA;
		memset(p, 0, (size_t) w);
	    }
	}
	else if (!eltFromLong(p, (long long) v, w, kind, hasNA != 0)) {
	    dir[i] = (v < 0) ? -1 : 1;
	    memset(p, 0, (size_t) w);
	}
    }

    UNPROTECT(1);

    return ans;
}

/* The integer part of a non-negative finite double, as an MSB-first
   magnitude.  A double is a binary significand shifted by an exponent,
   so this is exact even above 2^53.  false means the integer part is
   wider than the destination; fractional records a discarded fraction. */
static bool realIntegerMagnitude(double x, Rbyte *mag, int w,
				 bool *fractional)
{
    double ip;
    *fractional = modf(x, &ip) != 0.0;
    memset(mag, 0, (size_t) w);
    if (ip == 0.0) return true;

    int exp;
    double f = frexp(ip, &exp);
    uint64_t sig = (uint64_t) ldexp(f, DBL_MANT_DIG);
    int shift = exp - DBL_MANT_DIG;

    /* Below 2^52 the binary point lies inside the significand.  ip is
       integral, so all bits shifted away here are necessarily zero. */
    if (shift < 0) {
	sig >>= -shift;
	shift = 0;
    }

    int nbits = 0;
    for (uint64_t t = sig; t; t >>= 1) nbits++;
    if (nbits + shift > 8 * w) return false;

    for (int bit = shift; sig; bit++, sig >>= 1)
	if (sig & 1) mag[w - 1 - bit / 8] |= (Rbyte) (1U << (bit % 8));
    return true;
}

/* Exact comparison of a fixed-width integer element with a double.
   No integer is first rounded to double: the double's exact binary
   integer part is compared as bytes, with its fractional part settling
   a tie.  The return value orders p against value. */
attribute_hidden
int R_xintEltCompareReal(const Rbyte *p, int w, int kind, bool hasNA,
			  double value, bool *isNA)
{
    *isNA = false;
    if ((hasNA && R_xintEltIsNAFast(p, w, kind)) || ISNAN(value)) {
	*isNA = true;
	return 0;
    }
    if (value == R_PosInf) return -1;
    if (value == R_NegInf) return 1;

    Rbyte pmag[XINT_MAX_WIDTH], dmag[XINT_MAX_WIDTH];
    bool pneg = magFromElt(pmag, p, w, kind);
    bool pzero = magIsZero(pmag, w);
    bool dneg = value < 0.0;
    double a = fabs(value);

    if (!pzero && pneg != dneg) return pneg ? -1 : 1;
    if (pzero && dneg) return 1;
    if (!pzero && pneg && value == 0.0) return -1;

    bool fractional;
    int cmp;
    if (!realIntegerMagnitude(a, dmag, w, &fractional))
	cmp = -1;                 /* |value| is wider than p can be */
    else {
	cmp = magCmp(pmag, dmag, w);
	if (cmp == 0 && fractional) cmp = -1;
    }

    return pneg ? -cmp : cmp;
}

/* Narrow for equality and matching.  A finite double can equal an
   integer only when it is integral and its exact binary value fits the
   fixed-width type.  Anything else is marked drop[] rather than rounded.
   Complex values additionally require a zero imaginary part. */
attribute_hidden
SEXP R_xintNarrowMatch(SEXP x, int w, int kind, int hasNA, int *drop,
			SEXP call)
{
    SEXPTYPE t = TYPEOF(x);
    if (t == INTSXP || t == LGLSXP)
	return R_xintNarrowCmp(x, w, kind, hasNA, drop, call);
    if (t != REALSXP && t != CPLXSXP)
	errorcall(call, _("'%s' and '%s' cannot be combined for matching"),
		  "xinteger", R_typeToChar(x));
    R_xlen_t n = XLENGTH(x);
    SEXP ans = PROTECT(R_allocXIntVector(n, w, kind,
					       hasNA ? TRUE : FALSE));
    for (R_xlen_t i = 0; i < n; i++) {
	double v, im = 0.0;
	bool isNA = false, isNaN = false;
	if (t == REALSXP) {
	    v = REAL_ELT(x, i);
	    isNA = R_IsNA(v);
	    isNaN = R_IsNaN(v);
	}
	else {
	    Rcomplex z = COMPLEX_ELT(x, i);
	    v = z.r; im = z.i;
	    isNA = R_IsNA(v) || R_IsNA(im);
	    isNaN = !isNA && (R_IsNaN(v) || R_IsNaN(im));
	}

	Rbyte *p = XINT_ELT(ans, i);
	drop[i] = 0;
	if (isNA) {
	    if (hasNA) R_xintSetEltNA(p, w, kind);
	    else { memset(p, 0, (size_t) w); drop[i] = 1; }
	    continue;
	}
	if (isNaN || im != 0.0 || !R_FINITE(v)) {
	    memset(p, 0, (size_t) w); drop[i] = 1; continue;
	}

	Rbyte mag[XINT_MAX_WIDTH];
	bool fractional;
	if (!realIntegerMagnitude(fabs(v), mag, w, &fractional) || fractional ||
	    !storeResult(p, mag, w, kind, v < 0.0, hasNA != 0)) {
	    memset(p, 0, (size_t) w);
	    drop[i] = 1;
	}
    }
    UNPROTECT(1);
    return ans;
}

/* Settle the operands of a binary operation: at least one is an 'xinteger'
   vector, and the other must be one too (same kind) or must narrow. */
static void xintBinaryOperands(SEXP call, SEXP *px, SEXP *py, int *pw, int *pk)
{
    SEXP x = *px, y = *py;
    SEXP b = (TYPEOF(x) == ALTSXP) ? x : y;
    int kind = XINT_KIND(b), w = XINT_WIDTH(b);

    if (TYPEOF(x) == ALTSXP && TYPEOF(y) == ALTSXP)
	/* The rule c(), ==, match(), pmin() and subassignment hold to:
	   the width is part of the type, so a pair that disagrees is a
	   mistake to report.  Promoting to max(width) here instead would
	   make arithmetic the one operation that accepts a pair every
	   other one refuses, which is not what xinteger.Rd describes. */
	R_xintCheckPair(call, x, y, "combine");
    else if (TYPEOF(x) == ALTSXP)
	*py = R_xintNarrow(y, w, kind, XINT_HAS_NA(x), call);
    else
	*px = R_xintNarrow(x, w, kind, XINT_HAS_NA(y), call);

    *pw = w;
    *pk = kind;
}

/* ---- vector level ---- */

/* dim, dimnames and names belong to R_binary(), which restores them from
   the operands it was given.  Every other attribute comes from an
   operand of the result's length, x last so that its own win -- the
   rule integer_binary() and real_binary() end with. */
static void xintCopyMostAttrib(SEXP ans, SEXP x, SEXP y, R_xlen_t n)
{
    if (n == XLENGTH(y) && ATTRIB(y) != R_NilValue) copyMostAttrib(y, ans);
    if (n == XLENGTH(x) && ATTRIB(x) != R_NilValue) copyMostAttrib(x, ans);
}

attribute_hidden
SEXP R_xintArith(SEXP call, int oper, SEXP x, SEXP y)
{
    /* xintBinaryOperands() replaces a non-'xinteger' operand with a
       narrowed temporary, which carries none of the caller's
       attributes; those have to be read from the operands as given */
    SEXP ox = PROTECT(x), oy = PROTECT(y);

    int w, kx;
    PROTECT_INDEX xi, yi;
    PROTECT_WITH_INDEX(x, &xi);
    PROTECT_WITH_INDEX(y, &yi);
    xintBinaryOperands(call, &x, &y, &w, &kx);
    REPROTECT(x, xi);
    REPROTECT(y, yi);

    /* The operator settles the kernel once, not per element -- the
       dispatch the sibling kernels hoist into one loop per operator.
       Division keeps its own arm for the zero-divisor probe. */
    bool divmod = (oper == IDIVOP || oper == MODOP);
    bool (*kern)(Rbyte *, const Rbyte *, const Rbyte *, int, int, bool) = NULL;
    switch (oper) {
    case PLUSOP:  kern = eltAdd; break;
    case MINUSOP: kern = eltSub; break;
    case TIMESOP: kern = eltMul; break;
    case IDIVOP:
    case MODOP:   break;
    default:
	UNPROTECT(4); /* y, x, oy, ox */
	errorcall(call, _("this operator is not defined for 'xinteger' vectors"));
    }

    R_xlen_t nx = XLENGTH(x), ny = XLENGTH(y);
    if (nx == 0 || ny == 0) {
	/* x may be a narrowed temporary held only by the index above, so
	   it has to outlive the allocation that reads its NA flag */
	/* bare, as integer_binary() and real_binary() return it: both
	   leave for a zero-length result before their copyMostAttrib()
	   tail, and this is the same operation on the same operands */
	SEXP val = R_allocXIntVector(0, w, kx,
				      XINT_HAS_NA(x) ? TRUE : FALSE);
	UNPROTECT(4); /* y, x, oy, ox */

	return val;
    }
    /* R_binary has already warned about a length mismatch */
    R_xlen_t n = nx > ny ? nx : ny;

    bool hasNA = XINT_HAS_NA(x);
    SEXP ans = PROTECT(R_allocXIntVector(n, w, kx, hasNA ? TRUE : FALSE));
    R_xlen_t nOver = 0;

    /* Hoisted: the element macros re-read the payload pointer and the
       width out of the header every time, which at one machine
       instruction per operand is no longer beneath notice.  Nothing in
       the loop allocates, so the pointers cannot move. */
    const Rbyte *bx = XINT_DATA_RO(x), *by = XINT_DATA_RO(y);
    Rbyte *ba = XINT_DATA(ans);
    R_xlen_t i, ix, iy;

    MOD_ITERATE2(n, nx, ny, i, ix, iy, {
	/* one declaration per statement: this is a macro argument, and
	   braces do not shield a comma from the preprocessor */
	const Rbyte *px = bx + ix * w;
	const Rbyte *py = by + iy * w;
	Rbyte *pa = ba + i * w;

	if (hasNA && (R_xintEltIsNAFast(px, w, kx) || R_xintEltIsNAFast(py, w, kx))) {
	    R_xintSetEltNA(pa, w, kx);
	    continue;
	}

	bool ok;
	if (divmod) {
	    /* Division by zero is a silent NA for integer -- arithmetic.c
	       folds x2 == 0 into the NA test with no warning -- so it is
	       one here too.  Warning instead turns the same expression
	       into an error under options(warn = 2), which is a common
	       setting in package tests. */
	    if (eltIsZero(py, w)) {
		R_xintCheckNA(ans);
		R_xintSetEltNA(pa, w, kx);
		continue;
	    }
	    ok = eltDivMod(pa, px, py, w, kx, oper == IDIVOP, hasNA);
	}
	else
	    ok = kern(pa, px, py, w, kx, hasNA);

	if (!ok) {
	    R_xintCheckNA(ans);	/* nothing to fall back on */
	    R_xintSetEltNA(pa, w, kx);
	    nOver++;
	}
    });

    if (nOver)
	warningcall(call, _("NAs produced by integer overflow"));

    xintCopyMostAttrib(ans, ox, oy, n);
    UNPROTECT(5); /* ans, y, x, oy, ox */

    return ans;
}

attribute_hidden
SEXP R_xintUnary(SEXP call, int oper, SEXP x)
{
    int k = XINT_KIND(x), w = XINT_WIDTH(x);

    if (oper != PLUSOP && oper != MINUSOP)
	errorcall(call, _("invalid argument to unary operator"));

    if (oper == PLUSOP) return x;
    if (k == XINT_UNSIGNED)
	errorcall(call, _("unary minus is not defined for unsigned 'xinteger' vectors"));

    R_xlen_t n = XLENGTH(x);
    bool hasNA = XINT_HAS_NA(x);
    SEXP ans = PROTECT(R_allocXIntVector(n, w, k, hasNA ? TRUE : FALSE));
    R_xlen_t nOver = 0;

    /* R_unary() hands the result straight back to do_arith, so the
       attributes the other unary kernels keep have to be kept here.
       integer_unary() and real_unary() work in a duplicate() of the
       operand, which is every attribute, not just the three R_binary()
       would have restored. */
    SHALLOW_DUPLICATE_ATTRIB(ans, x);

    /* hoisted as in R_xintArith(); nothing here allocates */
    const Rbyte *bx = XINT_DATA_RO(x);
    Rbyte *ba = XINT_DATA(ans);

    for (R_xlen_t i = 0; i < n; i++) {
	const Rbyte *px = bx + i * w;
	Rbyte *pa = ba + i * w;

	if (hasNA && R_xintEltIsNAFast(px, w, k)) {
	    R_xintSetEltNA(pa, w, k);
	    continue;
	}

	if (!eltNeg(pa, px, w, k, hasNA)) {
	    R_xintCheckNA(ans);
	    R_xintSetEltNA(pa, w, k);
	    nOver++;
	}
    }

    if (nOver) warningcall(call, _("NAs produced by integer overflow"));
    UNPROTECT(1);

    return ans;
}

attribute_hidden
SEXP R_xintAbs(SEXP call, SEXP x)
{
    int k = XINT_KIND(x), w = XINT_WIDTH(x);
    if (k == XINT_UNSIGNED) return x;

    R_xlen_t n = XLENGTH(x), nOver = 0;
    bool hasNA = XINT_HAS_NA(x);
    SEXP ans = PROTECT(R_allocXIntVector(n, w, k,
					       hasNA ? TRUE : FALSE));
    SHALLOW_DUPLICATE_ATTRIB(ans, x);
    for (R_xlen_t i = 0; i < n; i++) {
	const Rbyte *p = XINT_ELT_RO(x, i);
	Rbyte *q = XINT_ELT(ans, i);
	if (hasNA && R_xintEltIsNAFast(p, w, k)) {
	    R_xintSetEltNA(q, w, k);
	    continue;
	}
	Rbyte mag[XINT_MAX_WIDTH];
	bool neg = magFromElt(mag, p, w, k);
	if (!neg) memcpy(q, p, (size_t) w);
	else if (!storeResult(q, mag, w, k, false, hasNA)) {
	    R_xintCheckNA(ans);
	    R_xintSetEltNA(q, w, k);
	    nOver++;
	}
    }
    if (nOver) warningcall(call, _("NAs produced by integer overflow"));
    UNPROTECT(1);
    return ans;
}

attribute_hidden
SEXP R_xintSign(SEXP x)
{
    int k = XINT_KIND(x), w = XINT_WIDTH(x);
    R_xlen_t n = XLENGTH(x);
    bool hasNA = XINT_HAS_NA(x);
    SEXP ans = PROTECT(allocVector(REALSXP, n));
    SHALLOW_DUPLICATE_ATTRIB(ans, x);
    for (R_xlen_t i = 0; i < n; i++) {
	const Rbyte *p = XINT_ELT_RO(x, i);
	if (hasNA && R_xintEltIsNAFast(p, w, k))
	    REAL(ans)[i] = NA_REAL;
	else {
	    Rbyte mag[XINT_MAX_WIDTH];
	    bool neg = magFromElt(mag, p, w, k);
	    REAL(ans)[i] = magIsZero(mag, w) ? 0.0 : (neg ? -1.0 : 1.0);
	}
    }
    UNPROTECT(1);
    return ans;
}

/* Exact unit-step sequence between scalar endpoints of one fixed-width
   integer type.  Distance is computed as a magnitude and must fit an
   R_xlen_t before any output is allocated. */
attribute_hidden
SEXP R_xintSeq(SEXP call, SEXP from, SEXP to)
{
    R_xintCheckPair(call, from, to, "sequence");
    int k = XINT_KIND(from), w = XINT_WIDTH(from);
    bool hasNA = XINT_HAS_NA(from);
    if (XLENGTH(from) != 1 || XLENGTH(to) != 1)
	errorcall(call, _("'%s' and '%s' must be of length 1"), "from", "to");

    const Rbyte *pf = XINT_ELT_RO(from, 0), *pt = XINT_ELT_RO(to, 0);
    if (hasNA && (R_xintEltIsNAFast(pf, w, k) ||
		  R_xintEltIsNAFast(pt, w, k)))
	errorcall(call, _("NA argument"));

    Rbyte fm[XINT_MAX_WIDTH + 1] = {0}, tm[XINT_MAX_WIDTH + 1] = {0};
    Rbyte dist[XINT_MAX_WIDTH + 1] = {0};
    bool fn = magFromElt(fm + 1, pf, w, k);
    bool tn = magFromElt(tm + 1, pt, w, k);
    if (fn == tn) {
	if (magCmp(fm, tm, w + 1) >= 0) {
	    memcpy(dist, fm, (size_t) w + 1);
	    magSub(dist, tm, w + 1);
	}
	else {
	    memcpy(dist, tm, (size_t) w + 1);
	    magSub(dist, fm, w + 1);
	}
    }
    else {
	memcpy(dist, fm, (size_t) w + 1);
	magAdd(dist, tm, w + 1);
    }

    R_xlen_t distance = 0;
    for (int i = 0; i < w + 1; i++) {
	if (distance > (R_XLEN_T_MAX - 1 - dist[i]) / 256)
	    errorcall(call, _("result would be too long a vector"));
	distance = distance * 256 + dist[i];
    }
    R_xlen_t n = distance + 1;
    SEXP ans = PROTECT(R_allocXIntVector(n, w, k,
					       hasNA ? TRUE : FALSE));
    memcpy(XINT_ELT(ans, 0), pf, (size_t) w);

    int direction = R_xintEltCmp(pf, pt, w, k) <= 0 ? 1 : -1;
    Rbyte one[XINT_MAX_WIDTH];
    if (!eltFromLong(one, 1, w, k, hasNA))
	errorcall(call, _("cannot represent a unit step in '%s'"),
		  R_xintTypeName(from));
    for (R_xlen_t i = 1; i < n; i++) {
	Rbyte *q = XINT_ELT(ans, i);
	const Rbyte *p = XINT_ELT_RO(ans, i - 1);
	bool ok = direction > 0
	    ? eltAdd(q, p, one, w, k, hasNA)
	    : eltSub(q, p, one, w, k, hasNA);
	if (!ok) errorcall(call, _("integer overflow while constructing sequence"));
    }
    UNPROTECT(1);
    return ans;
}

/* ---- numeric coercion ---- */

/* The nearest double to the element's value.

   Accumulating byte by byte (d = d * 256 + byte) would not do: once the
   running total passes 2^53 every further step rounds again, and the
   errors compound to as much as a whole ulp.  Instead the top eight
   significant bytes go into a uint64_t -- at least 57 significant bits,
   four more than a double keeps -- everything below them is folded into
   a sticky bit, and the single conversion the hardware then does is
   correctly rounded.  Scaling back up by a power of 256 is exact, and
   overflows to Inf, which is what the caller warns about. */
/* the top-eight-bytes-plus-sticky-bit conversion described above, on a
   bare MSB-first magnitude; shared with the mean's wide accumulator */
static double magAsReal(const Rbyte *A, int w)
{
    int s = 0;			/* the first byte that carries a bit */
    while (s < w && A[s] == 0) s++;
    if (s == w) return 0.0;

    int nb = w - s, take = (nb < 8) ? nb : 8;
    uint64_t hi = 0;
    for (int i = 0; i < take; i++) hi = (hi << 8) | A[s + i];

    if (nb > 8) {
	for (int i = s + 8; i < w; i++)
	    if (A[i]) { hi |= 1; break; }	/* sticky */
    }

    return (nb > 8) ? ldexp((double) hi, 8 * (nb - 8)) : (double) hi;
}

attribute_hidden double R_xintEltAsReal(const Rbyte *p, int w, int kind)
{
    Rbyte A[XINT_MAX_WIDTH];
    bool neg = false;

    toMSB(A, p, w);
    if (kind == XINT_SIGNED && (A[0] & 0x80)) {
	neg = true;
	R_xintMagNegate(A, w);
    }

    double d = magAsReal(A, w);

    return neg ? -d : d;
}

/* Whether a magnitude's exact value is changed by conversion to
   double: it survives iff its significant bits -- highest set bit down
   to lowest set bit -- span at most the 53 a double's mantissa keeps,
   so a value like 2^54 converts exactly and must not warn.  Asked of
   the bytes rather than of the double they convert to: a value just
   above 2^53 rounds down onto 2^53 exactly, so a test on the result
   would let through the one value that did lose a digit.

   MSB-first and unsigned, so a caller holding an element passes its
   magnitude; the mean's wide accumulator is already in this form. */
static bool magLosesAsDouble(const Rbyte *A, int w)
{
    int hi = -1, lo = 0;	/* set-bit positions, 0 the least significant */
    for (int i = 0; i < w; i++) {
	Rbyte b = A[i];
	if (!b) continue;

	int base = 8 * (w - 1 - i);
	if (hi < 0) {
	    int t = 7;
	    while (!(b & (1 << t))) t--;
	    hi = base + t;
	}
	int t = 0;
	while (!(b & (1 << t))) t++;
	lo = base + t;
    }

    return hi - lo + 1 > 53;	/* hi is -1 only for zero, which is exact */
}

static bool eltLosesAsDouble(const Rbyte *p, int w, int kind)
{
    if (w < 7) return false;	/* at most 48 bits, so always exact */

    Rbyte A[XINT_MAX_WIDTH];

    toMSB(A, p, w);
    if (kind == XINT_SIGNED && (A[0] & 0x80))
	R_xintMagNegate(A, w);

    return magLosesAsDouble(A, w);
}

attribute_hidden
SEXP R_xintCoerce(SEXP x, SEXPTYPE type)
{
    int w = XINT_WIDTH(x), k = XINT_KIND(x);
    R_xlen_t n = XLENGTH(x);

    /* The loop below writes through RAW0(), INTEGER0() or REAL0() and
       picks between them by elimination, so a target this function does
       not handle has to be turned away before anything is allocated. */
    if (type != RAWSXP && type != INTSXP && type != REALSXP)
	error(_("cannot coerce an 'xinteger' vector to type '%s'"),
	      type2char(type));

    bool hasNA = XINT_HAS_NA(x);
    SEXP ans = PROTECT(allocVector(type, n));
    /* as coerceToReal() and the rest of the coerceToXXX() family do:
       coerceVector() is not as.vector(), and its caller is entitled to
       the dim and names of what it handed in */
    SHALLOW_DUPLICATE_ATTRIB(ans, x);
    R_xlen_t nLost = 0;

    for (R_xlen_t i = 0; i < n; i++) {
	const Rbyte *p = XINT_ELT_RO(x, i);

	if (hasNA && R_xintEltIsNA(p, w, k)) {
	    /* raw has no NA, so a missing value becomes 00 and counts as
	       lost, exactly as as.raw(NA_integer_) does */
	    if (type == RAWSXP) { RAW0(ans)[i] = 0; nLost++; }
	    else if (type == INTSXP) INTEGER0(ans)[i] = NA_INTEGER;
	    else REAL0(ans)[i] = NA_REAL;
	    continue;
	}

	double d = R_xintEltAsReal(p, w, k);
	if (type == RAWSXP) {
	    if (!(d >= 0 && d <= 255)) {
		RAW0(ans)[i] = 0;
		nLost++;
	    }
	    else RAW0(ans)[i] = (Rbyte) d;
	}
	else if (type == INTSXP) {
	    if (!(d >= (double) (INT_MIN + 1) && d <= (double) INT_MAX)) {
		INTEGER0(ans)[i] = NA_INTEGER;
		nLost++;
	    }
	    else INTEGER0(ans)[i] = (int) d;
	}
	else {
	    /* The widest element is under 2^128, so every value has a
	       double to round to; only the rounding can be reported. */
	    if (eltLosesAsDouble(p, w, k)) nLost++;
	    REAL0(ans)[i] = d;
	}
    }

    if (nLost)
	warning(type == RAWSXP
		? _("out-of-range values treated as 0 in coercion to raw")
		: (type == INTSXP
		   ? _("NAs introduced by coercion to integer range")
		   : _("'xinteger' values above 2^53 lose precision as double")));

    UNPROTECT(1);

    return ans;
}

/* ---- sum / prod / min / max ---- */

/* Handled here rather than inside do_summary's accumulator machinery:
   that machine keeps typed accumulators chosen from a fixed set, and a
   per-vector element width does not fit it.  A self-contained pass is
   both smaller and easier to argue correct. */

/* The rule a summary applies to its operands before it reads any of
   them: which types may appear at all, and what the 'xinteger' ones have
   to agree on.  It settles the kind, width and NA flag of the result
   along the way.

   prod() asks this and then goes its own way: its answer is a double
   for every type, as it is for integer, so summary.c converts the
   operands and takes the ordinary path.  Mixed double and complex
   operands have already selected their ordinary result domain; when
   only 'xinteger' operands remain, this check makes them agree before
   they are converted. */
attribute_hidden
void R_xintSummaryType(SEXP call, int iop, SEXP args,
			int *pkind, int *pw, int *phasNA)
{
    SEXP first = NULL;
    int kind = -1, w = 0, hasNA = -1;

    /* one pass to settle the result kind, width and NA flag */
    for (SEXP t = args; t != R_NilValue; t = CDR(t)) {
	SEXP a = CAR(t);
	if (TAG(t) == R_NaRmSymbol) continue;
	if (TYPEOF(a) == NILSXP) continue;

	/* An integer or logical operand narrows into the result type,
	   as it does in arithmetic and in c(): max(x, 5L) has to agree
	   with max(c(x, 5L)).  It settles nothing about that type,
	   which the 'xinteger' operands decide between them. */
	if (TYPEOF(a) == INTSXP || TYPEOF(a) == LGLSXP) continue;
	if (TYPEOF(a) != ALTSXP)
	    errorcall(call, _("cannot mix 'xinteger' vectors with other types"));
	if (first == NULL) {
	    first = a;
	    kind = XINT_KIND(a);
	    hasNA = XINT_HAS_NA(a);
	    w = XINT_WIDTH(a);
	}
	else
	    /* the rule c() and == already use: a width is part of the
	       type, so these refuse exactly the pairs c() refuses.  That
	       is what keeps range() -- whose answer goes through c() --
	       from failing on arguments min() and max() accept. */
	    R_xintCheckPair(call, first, a, "combine");
    }

    if (pkind)  *pkind  = kind;
    if (pw)     *pw     = w;
    if (phasNA) *phasNA = hasNA;
}

/* sum() keeps its running total 8 bytes wider than the element type,
   in sign-magnitude form.  Accumulating in the type itself would make
   the answer depend on the order of the elements -- a prefix of the
   sum can pass the type's range and come back into it -- where R's
   integer sum overflows only when the total does.  No element count
   can carry past 8 extra bytes, so the total is exact and its one
   range check comes at the end, against the type. */

/* fold one element's sign and magnitude into the running total */
static void wideSumAdd(Rbyte *acc, bool *accNeg, const Rbyte *m, bool neg, int w)
{
    int W = w + 8;
    Rbyte wide[XINT_MAX_WIDTH + 8];

    memset(wide, 0, 8);
    memcpy(wide + 8, m, (size_t) w);

    if (*accNeg == neg) {
	magAdd(acc, wide, W);
	return;
    }

    if (magCmp(acc, wide, W) >= 0)
	magSub(acc, wide, W);
    else {
	magSub(wide, acc, W);
	memcpy(acc, wide, (size_t) W);
	*accNeg = neg;
    }
    if (magIsZero(acc, W))
	*accNeg = false;
}

/* the total back into a w-byte element; false when it does not fit */
static bool wideSumStore(Rbyte *out, Rbyte *acc, bool accNeg,
			 int w, int kind, bool hasNA)
{
    /* a magnitude that reaches the extra bytes is past any element */
    for (int i = 0; i < 8; i++)
	if (acc[i]) return false;
    /* nothing unsigned to store a negative total in; R_xintMagFits()
       does not ask about the sign for this kind */
    if (accNeg && kind == XINT_UNSIGNED && !magIsZero(acc + 8, w))
	return false;

    return storeResult(out, acc + 8, w, kind, accNeg, hasNA);
}

/* sum, min and max.  prod() never arrives: see R_xintSummaryType(). */
attribute_hidden
SEXP R_xintSummary(SEXP call, int iop, SEXP args, bool narm)
{
    int kind, w, hasNA;
    bool arith = (iop == 0);	/* sum accumulates; min and max compare */

    R_xintSummaryType(call, iop, args, &kind, &w, &hasNA);

    SEXP ans = PROTECT(R_allocXIntVector(1, w, kind, hasNA ? TRUE : FALSE));
    Rbyte *acc = XINT_ELT(ans, 0);
    bool seen = false, isNA = false, over = false;
    /* an operand outside the range of the type: unrep once it is known
       to be the answer, sawOut while it is only the answer if nothing
       representable turns up; nRange counts the ones sum() turns into
       NA, as the narrowing's own warning would have */
    bool unrep = false, sawOut = false;
    R_xlen_t nRange = 0;
    /* sum's running total, wider than the type; see wideSumAdd() */
    Rbyte sumMag[XINT_MAX_WIDTH + 8] = {0};
    bool sumNeg = false;

    /* sum starts at 0; min and max take the first element seen */


    for (SEXP t = args; t != R_NilValue && !isNA; t = CDR(t)) {
	SEXP a = CAR(t);
	if (TAG(t) == R_NaRmSymbol || TYPEOF(a) == NILSXP) continue;

	/* Narrowed here rather than in the pass above, which settles the
	   type this needs -- and for comparison whatever the summary:
	   dir[] marks what the type cannot hold, out-of-range operands
	   and (where nothing is reserved) NA ones, without refusing
	   them.  min and max read the marks as bounds; sum reads them
	   as the NAs the narrowing would have produced, so na.rm can
	   still drop them and the diagnostics can name the real cause. */
	int *dir = NULL;
	const void *vmax = vmaxget();
	if (TYPEOF(a) != ALTSXP) {
	    dir = (int *) R_alloc(XLENGTH(a) + 1, sizeof(int));
	    a = R_xintNarrowCmp(a, w, kind, hasNA, dir, call);
	}
	PROTECT(a);

	const Rbyte *ba = XINT_DATA_RO(a);	/* hoisted; see R_xintArith */
	R_xlen_t na = XLENGTH(a);

	for (R_xlen_t i = 0; i < na; i++) {
	    const Rbyte *p = ba + i * w;

	    if (dir && dir[i] == XINT_CMP_NA) {
		/* missing, with no pattern in this type to stand for it:
		   na.rm drops it, and without na.rm the answer is an NA
		   the type cannot hold, which R_xintCheckNA() reports
		   below */
		if (narm) continue;
		isNA = true;
		break;
	    }

	    if (dir && dir[i]) {
		if (arith) {
		    /* out of range: for sum it becomes the NA the na =
		       TRUE narrowing would have made of it, so na.rm
		       drops it -- and where nothing is reserved and
		       na.rm does not apply, the error names the range,
		       not a missing value that was never there */
		    if (!narm && !hasNA)
			errorcall(call,
				  _("an operand is outside the range of '%s'; it was created with na = FALSE, so there is no NA to produce"),
				  R_xintTypeName(ans));
		    if (narm) continue;
		    nRange++;
		    isNA = true;
		    break;
		}
		/* below (-1) or above (+1) every element of the type: for
		   min a low bound is the answer straight away, and a high
		   one is the answer only if nothing else turns up */
		if (dir[i] == (iop == 2 ? -1 : 1)) unrep = true;
		else sawOut = true;
		continue;
	    }

	    if (hasNA && R_xintEltIsNAFast(p, w, kind)) {
		if (narm) continue;
		isNA = true;
		break;
	    }

	    if (!seen && (iop == 2 || iop == 3)) {
		memcpy(acc, p, (size_t) w);
		seen = true;
		continue;
	    }
	    seen = true;

	    switch (iop) {
	    case 0:
	    {
		Rbyte m[XINT_MAX_WIDTH];
		bool neg = magFromElt(m, p, w, kind);
		wideSumAdd(sumMag, &sumNeg, m, neg, w);
		break;
	    }
	    case 2:
		if (R_xintEltCmp(p, acc, w, kind) < 0)
		    memcpy(acc, p, (size_t) w);
		break;
	    case 3:
		if (R_xintEltCmp(p, acc, w, kind) > 0)
		    memcpy(acc, p, (size_t) w);
		break;
	    default:
		UNPROTECT(2);	/* a, ans */
		errorcall(call, _("this summary is not defined for 'xinteger' vectors"));
	    }
	}

	UNPROTECT(1); /* a */
	vmaxset(vmax);
    }

    /* the total, checked against the type only now that it is final:
       an intermediate value outside the range is not an overflow when
       later elements bring it back */
    if (iop == 0 && !isNA &&
	!wideSumStore(acc, sumMag, sumNeg, w, kind, hasNA != 0)) {
	over = true;
	isNA = true;
    }

    /* a bound that lost to nothing at all is still the answer */
    if (!seen && sawOut) unrep = true;
    if (unrep && !isNA) {
	warningcall(call,
		    _("NA produced: the %s of these arguments is outside the range of '%s'"),
		    iop == 2 ? "minimum" : "maximum", R_xintTypeName(ans));
	isNA = true;
    }

    if ((iop == 2 || iop == 3) && !seen && !isNA) {
	/* Every other type warns here and returns +/-Inf.  A fixed-width
	   type has no Inf, so NA stands in -- and when the type does not
	   reserve an NA either there is nothing to return, which is the
	   one case that has to stay an error. */
	if (!hasNA) {
	    UNPROTECT(1);
	    errorcall(call,
		      _("no non-missing arguments to '%s', and this 'xinteger' type cannot represent NA"),
		      iop == 2 ? "min" : "max");
	}
	warningcall(call, _("no non-missing arguments to %s; returning NA"),
		    iop == 2 ? "min" : "max");
	isNA = true;
    }
    if (isNA) {
	R_xintCheckNA(ans);
	R_xintSetEltNA(acc, w, kind);
    }
    if (nRange)
	warningcall(call, _("NAs introduced by values outside the range of '%s'"),
		    R_xintTypeName(ans));
    if (over)
	warningcall(call, _("NAs produced by integer overflow"));

    UNPROTECT(1);

    return ans;
}

static void magFromU64(Rbyte *a, int w, uint64_t v)
{
    memset(a, 0, (size_t) w);
    for (int i = w - 1; i >= 0 && v; i--) {
	a[i] = (Rbyte) (v & 0xFF);
	v >>= 8;
    }
}

static void magMul16(Rbyte *p, const Rbyte *a, const Rbyte *b)
{
    memset(p, 0, 32);
    for (int i = 0; i < 16; i++) {
	unsigned int carry = 0, ai = a[15 - i];
	for (int j = 0; j < 16; j++) {
	    int at = 31 - (i + j);
	    unsigned int cur = (unsigned int) p[at] +
		ai * b[15 - j] + carry;
	    p[at] = (Rbyte) (cur & 0xFF);
	    carry = cur >> 8;
	}
	for (int at = 15 - i; carry && at >= 0; at--) {
	    unsigned int cur = (unsigned int) p[at] + carry;
	    p[at] = (Rbyte) (cur & 0xFF);
	    carry = cur >> 8;
	}
    }
}

static bool magShiftLeftChecked(Rbyte *a, int w, int shift)
{
    for (int j = 0; j < shift; j++) {
	if (a[0] & 0x80) return false;
	magShiftLeft1(a, w);
    }
    return true;
}

/* Is result exactly the rational sum/n?  This checks the returned
   double as a dyadic rational rather than using the rounded double sum,
   avoiding both missed warnings and warnings for an exactly representable
   mean whose unreduced sum itself needs more than 53 significant bits. */
static bool meanIsExact(const Rbyte *sum, int sw, R_xlen_t n, double result)
{
    if (n == 0 || !R_FINITE(result)) return false;
    if (magIsZero(sum, sw)) return result == 0.0;
    if (result == 0.0) return false;

    int exp;
    double f = frexp(fabs(result), &exp);
    uint64_t sig = (uint64_t) ldexp(f, DBL_MANT_DIG);
    int shift = exp - DBL_MANT_DIG;
    while (!(sig & 1)) { sig >>= 1; shift++; }

    Rbyte a[16], b[16], product[32], exact[32] = {0};
    magFromU64(a, 16, sig);
    magFromU64(b, 16, (uint64_t) n);
    magMul16(product, a, b);
    memcpy(exact + 32 - sw, sum, (size_t) sw);

    if (shift >= 0) {
	if (!magShiftLeftChecked(product, 32, shift)) return false;
    }
    else if (!magShiftLeftChecked(exact, 32, -shift)) return false;
    return !memcmp(product, exact, 32);
}

/* mean(), behind do_summary's mean switch and so behind mean.default.
   The sum is accumulated exactly in the wide form sum() uses, converted
   to double once and divided once, so mean(x) agrees with
   sum(x) / length(x) wherever that sum is representable -- and, the
   accumulator being wider than the type, still answers where sum()
   would overflow, as the integer mean does.  Rounding each element
   first, as mean.default(as.numeric(x)) did, loses up to a digit per
   element above 2^53. */
attribute_hidden SEXP R_xintMean(SEXP call, SEXP x)
{
    int w = XINT_WIDTH(x), kind = XINT_KIND(x);

    R_xlen_t n = XLENGTH(x);
    bool hasNA = XINT_HAS_NA(x);
    Rbyte sumMag[XINT_MAX_WIDTH + 8] = {0};
    bool sumNeg = false;
    const Rbyte *bx = XINT_DATA_RO(x);	/* hoisted; see R_xintArith */

    for (R_xlen_t i = 0; i < n; i++) {
	const Rbyte *p = bx + i * w;

	if (hasNA && R_xintEltIsNAFast(p, w, kind))
	    return ScalarReal(NA_REAL);

	Rbyte m[XINT_MAX_WIDTH];
	bool neg = magFromElt(m, p, w, kind);
	wideSumAdd(sumMag, &sumNeg, m, neg, w);
    }

    double s = magAsReal(sumMag, w + 8);

    /* 0/0 for an empty vector: NaN, as mean(integer(0)) is */
    double ans = (sumNeg ? -s : s) / (double) n;

    /* Where the sum is a double exactly, s is the total and ans is one
       IEEE division away from the exact rational mean -- correctly
       rounded, which is the best a double holds and is what
       mean(<integer>) returns without comment.  mean() of 1, 2 and 4 is
       7/3 whatever the operands were stored in, and saying that the
       fixed width lost something there is both untrue and, under
       options(warn = 2), fatal.

       A sum too wide for a double is the case this type can reach and
       the ordinary numeric ones cannot: s is then already short of the
       total and the division compounds it, so the result is worth
       checking against the exact rational before it is handed back. */
    if (n && magLosesAsDouble(sumMag, w + 8) &&
	!meanIsExact(sumMag, w + 8, n, ans))
	warningcall(call, _("fixed-width mean loses precision as double"));

    return ScalarReal(ans);
}
/* ---- cumsum / cumprod / cummax / cummin ---- */

/* iop is do_cum()'s PRIMVAL: 1 cumsum, 3 cummax, 4 cummin.  cumprod()
   and cumvar() never arrive: do_cum() converts to double for them, as
   prod() does and as base cumprod() does for its integer operands.
   The running value is kept in the answer itself, so each step reads
   the element before it.  As in cum.c's integer versions, an NA or an
   overflow ends the run and everything from there on is NA. */
attribute_hidden
SEXP R_xintCum(SEXP call, int iop, SEXP x)
{
    int w = XINT_WIDTH(x), kind = XINT_KIND(x);
    bool hasNA = XINT_HAS_NA(x);
    R_xlen_t n = XLENGTH(x);

    if (iop != 1 && iop != 3 && iop != 4)
	errorcall(call, _("'%s' is not defined for 'xinteger' vectors"),
		  iop == 2 ? "cumprod" : "cumvar");

    SEXP ans = PROTECT(R_allocXIntVector(n, w, kind, hasNA ? TRUE : FALSE));
    setAttrib(ans, R_NamesSymbol, getAttrib(x, R_NamesSymbol));

    const Rbyte *bx = XINT_DATA_RO(x);	/* hoisted; see R_xintArith */
    Rbyte *ba = XINT_DATA(ans);
    bool stop = false, over = false;

    for (R_xlen_t i = 0; i < n; i++) {
	const Rbyte *p = bx + i * w;
	Rbyte *acc = ba + i * w;

	if (!stop && hasNA && R_xintEltIsNAFast(p, w, kind))
	    stop = true;

	if (stop) {
	    R_xintSetEltNA(acc, w, kind);
	    continue;
	}

	if (i == 0) {
	    memcpy(acc, p, (size_t) w);
	    continue;
	}

	const Rbyte *prev = ba + (i - 1) * w;
	switch (iop) {
	case 1:
	    if (!eltAdd(acc, prev, p, w, kind, hasNA)) over = true;
	    break;
	case 3:
	    memcpy(acc, R_xintEltCmp(p, prev, w, kind) > 0 ? p : prev,
		   (size_t) w);
	    break;
	default:
	    memcpy(acc, R_xintEltCmp(p, prev, w, kind) < 0 ? p : prev,
		   (size_t) w);
	    break;
	}

	if (over) {
	    /* errors when the type reserves no NA: there is no value to
	       report the overflow with */
	    R_xintCheckNA(ans);
	    R_xintSetEltNA(acc, w, kind);
	    stop = true;
	}
    }

    if (over)
	warningcall(call, _("NAs produced by integer overflow"));

    UNPROTECT(1);

    return ans;
}
/* ---- pmin / pmax ---- */

/* iop is do_pmin()'s PRIMVAL: 0 pmin, 1 pmax.  Result kind, width and
   NA flag are settled as in R_xintSummary(); elements are only ever
   compared, so any width is allowed. */
attribute_hidden
SEXP R_xintParallelMinMax(SEXP call, int iop, SEXP args, bool narm)
{
    SEXP proto = NULL;		/* the first 'xinteger' operand */
    int kind = -1, w = 0, hasNA = -1;
    R_xlen_t len = 0;
    bool anyEmpty = false;

    for (SEXP t = args; t != R_NilValue; t = CDR(t)) {
	SEXP a = CAR(t);
	if (TYPEOF(a) == NILSXP) {
	    anyEmpty = true;
	    continue;
	}

	/* an integer or logical operand narrows into the result type,
	   as it does in sum() and min(); its length still counts */
	if (TYPEOF(a) == INTSXP || TYPEOF(a) == LGLSXP) {
	    R_xlen_t ni = XLENGTH(a);
	    if (ni == 0) anyEmpty = true;
	    if (ni > len) len = ni;
	    continue;
	}
	if (TYPEOF(a) != ALTSXP)
	    errorcall(call, _("cannot mix 'xinteger' vectors with other types"));
	if (proto == NULL) {
	    proto = a;
	    kind = XINT_KIND(a);
	    hasNA = XINT_HAS_NA(a);
	    w = XINT_WIDTH(a);
	}
	else
	    /* refused on the same terms as c() and min(); see
	       R_xintSummary() */
	    R_xintCheckPair(call, proto, a, "combine");

	R_xlen_t n = XLENGTH(a);
	if (n == 0) anyEmpty = true;
	if (n > len) len = n;
    }

    /* as for the other types: one zero-length operand makes the whole
       result zero-length, rather than recycling nothing */
    if (anyEmpty) len = 0;

    if (len)
	for (SEXP t = args; t != R_NilValue; t = CDR(t))
	    if (TYPEOF(CAR(t)) != NILSXP && len % XLENGTH(CAR(t))) {
		warningcall(call, _("an argument will be fractionally recycled"));
		break;
	    }

    SEXP ans = PROTECT(R_allocXIntVector(len, w, kind, hasNA ? TRUE : FALSE));
    bool first = true;

    /* Per result element: 0 once a representable candidate is in place,
       -1 or +1 while the best one so far lies below or above the type.
       A bound the type cannot hold is not missing -- it beats every
       element or loses to every element -- so it can decide the result
       without ever being stored, and only has to become NA if it wins
       outright.  See R_xintNarrowCmp(). */
    const void *vmax = vmaxget();
    signed char *state = (signed char *) R_alloc(len + 1, 1);
    memset(state, 0, (size_t) len);

    for (SEXP t = args; len > 0 && t != R_NilValue; t = CDR(t)) {
	SEXP a = CAR(t);
	if (TYPEOF(a) == NILSXP) continue;

	int *dir = NULL;
	if (TYPEOF(a) != ALTSXP) {
	    dir = (int *) R_alloc(XLENGTH(a) + 1, sizeof(int));
	    a = R_xintNarrowCmp(a, w, kind, hasNA, dir, call);
	}
	/* narrowing allocates, so nothing below may hold a data pointer
	   across this call */
	PROTECT(a);

	R_xlen_t n = XLENGTH(a);
	const Rbyte *ba = XINT_DATA_RO(a);
	Rbyte *ra = XINT_DATA(ans);

	if (first) {
	    for (R_xlen_t i = 0; i < len; i++) {
		R_xlen_t j = i % n;
		state[i] = (signed char) (dir ? dir[j] : 0);
		if (state[i])
		    memset(ra + i * w, 0, (size_t) w);
		else
		    memcpy(ra + i * w, ba + j * w, (size_t) w);
	    }
	    first = false;
	}
	else
	    for (R_xlen_t i = 0; i < len; i++) {
		R_xlen_t j = i % n;
		const Rbyte *p = ba + j * w;
		Rbyte *q = ra + i * w;
		int dp = dir ? dir[j] : 0, dq = state[i];
		/* XINT_CMP_NA is missing rather than a direction; it only
		   arises where the type reserves no NA pattern, so the
		   final pass errors if it ends up winning */
		bool pNA = (dp == XINT_CMP_NA) || (hasNA && R_xintEltIsNAFast(p, w, kind));
		bool qNA = (dq == XINT_CMP_NA) || (hasNA && R_xintEltIsNAFast(q, w, kind));

		/* an out-of-range candidate orders below or above every
		   element without being one, so direction settles the
		   comparison whenever the two differ in it */
		int c = (pNA || qNA) ? 0
		    : (dp != dq) ? (dp < dq ? -1 : 1)
		    : dp ? 0
		    : R_xintEltCmp(p, q, w, kind);

		/* the running element loses to a missing one unless
		   na.rm, and a missing running element is replaced by
		   anything when na.rm -- as the integer arm does */
		if ((narm && qNA) || (!narm && pNA) ||
		    (!pNA && !qNA && (iop == 1 ? c > 0 : c < 0))) {
		    state[i] = (signed char) dp;
		    if (dp)
			memset(q, 0, (size_t) w);
		    else
			memcpy(q, p, (size_t) w);
		}
	    }

	UNPROTECT(1); /* a */
    }

    /* whatever is still out of range won outright, and the type has no
       element for it */
    bool unrep = false;
    Rbyte *ra = XINT_DATA(ans);
    for (R_xlen_t i = 0; i < len; i++)
	if (state[i]) {
	    R_xintCheckNA(ans);
	    R_xintSetEltNA(ra + i * w, w, kind);
	    unrep = true;
	}
    if (unrep)
	warningcall(call,
		    _("NAs produced: the %s is outside the range of '%s'"),
		    iop == 1 ? "maximum" : "minimum", R_xintTypeName(ans));

    UNPROTECT(1); /* ans */
    vmaxset(vmax);

    return ans;
}
