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
 *  at 128 bits is the trade worth making.  They remain the definition
 *  of what these operations mean; the widths with a native C type
 *  behind them are dispatched to that type first, and the two paths
 *  are checked against each other.
 *
 *  Binary operands promote to max(width), which is a total order and so
 *  a far simpler lattice than R's usual one.  Kinds never mix.
 *  Overflow yields NA with a warning, matching integer overflow.
 */

#ifdef HAVE_CONFIG_H
# include <config.h>
#endif

#include <stdint.h>

#include <Defn.h>
#include <Internal.h>
#include <Rmath.h>
#include <R_ext/Itermacros.h>  /* MOD_ITERATE2 */

/* Arithmetic is defined only for the widths that correspond to an
   integer type someone might actually be carrying; wider elements stay
   pure storage. */
#define MAXW BYTEVEC_MAX_ARITH_WIDTH

typedef Rbyte mag_t[2 * MAXW];	/* MSB-first, room for a full product */

static bool arithWidthOK(int w)
{
    return w == 1 || w == 2 || w == 4 || w == 8 || w == 16;
}

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
   what these mean, and the two must agree; bytesxp-archeck.R checks
   both against Python's exact integers. */

#if defined(__has_builtin)
# if __has_builtin(__builtin_add_overflow) &&	\
     __has_builtin(__builtin_sub_overflow) &&	\
     __has_builtin(__builtin_mul_overflow)
#  define BYTES_NATIVE_ARITH 1
# endif
#elif defined(__GNUC__) && __GNUC__ >= 5
# define BYTES_NATIVE_ARITH 1
#endif

#ifdef BYTES_NATIVE_ARITH

# ifdef __SIZEOF_INT128__
typedef unsigned __int128 bytes_uint128_t;
typedef __int128 bytes_int128_t;
#  define BYTES_NATIVE_W16(BODY, T) case 16: BODY(T)
# else
#  define BYTES_NATIVE_W16(BODY, T)	/* width 16 stays on the general path */
# endif

/* Where a native type covers every arithmetic width, the general
   kernels become unreachable -- and unreachable code is where bugs
   settle in unnoticed.  Setting R_BYTES_GENERIC_ARITH runs them
   anyway, so the two paths can be compared with each other and with
   the Python reference.  One predictable branch per element. */
static bool useNative(void)
{
    static int cached = -1;

    if (cached < 0) {
	/* an empty value counts as unset, so that a caller can hand a
	   child process the native setting without having to remove the
	   variable from the environment it inherited */
	const char *e = getenv("R_BYTES_GENERIC_ARITH");
	cached = (e == NULL || *e == '\0');
    }

    return cached != 0;
}

#endif	/* BYTES_NATIVE_ARITH */

/* R_bytesEltIsNA() lives in another translation unit, so it is a real
   call, and at two per element that is now a visible part of an add.
   The first byte it looks at settles the answer for all but one value
   in 256, so testing that byte here leaves only the rare case to the
   call.  Same answer, one branch instead of a call.  The loops below
   call it too, so it sits outside the native block. */
static R_INLINE bool eltIsNA(const Rbyte *p, int w, int kind)
{
    if (kind == BYTEVEC_INT)
	return p[BYTEVEC_MSB(0, w)] == 0x80 && R_bytesEltIsNA(p, w, kind);

    return p[0] == BYTEVEC_NA_BYTE && R_bytesEltIsNA(p, w, kind);
}

#ifdef BYTES_NATIVE_ARITH

/* The last two steps every body shares.  A result landing on the value
   this vector reserves for NA is not representable, and saying so beats
   returning a silent NA; the element's bytes are the native object's
   bytes, so the reserved-pattern test reads them where they lie. */
static bool storeNative(Rbyte *out, const void *v, int w, int kind,
			bool hasNA)
{
    if (hasNA && eltIsNA((const Rbyte *) v, w, kind)) return false;
    memcpy(out, v, (size_t) w);

    return true;
}

/* Expand UBODY once per unsigned width and SBODY once per signed width
   that has a native type.  Every body ends in a return, so falling out
   of the switch means "not handled here" and the general kernel runs.
   Multiply does not come through here; see BYTES_NATIVE_MUL(). */
# define BYTES_NATIVE2(UBODY, SBODY, w, kind)			\
    do {							\
	if (!useNative()) break;				\
	if ((kind) == BYTEVEC_UINT)				\
	    switch (w) {					\
	    case 1:  UBODY(uint8_t)				\
	    case 2:  UBODY(uint16_t)				\
	    case 4:  UBODY(uint32_t)				\
	    case 8:  UBODY(uint64_t)				\
	    BYTES_NATIVE_W16(UBODY, bytes_uint128_t)		\
	    }							\
	else if ((kind) == BYTEVEC_INT)				\
	    switch (w) {					\
	    case 1:  SBODY(int8_t)				\
	    case 2:  SBODY(int16_t)				\
	    case 4:  SBODY(int32_t)				\
	    case 8:  SBODY(int64_t)				\
	    BYTES_NATIVE_W16(SBODY, bytes_int128_t)		\
	    }							\
    } while (0)

/* the usual case: the two kinds differ only in the C type */
# define BYTES_NATIVE(BODY, w, kind) BYTES_NATIVE2(BODY, BODY, w, kind)

# define BYTES_LOAD2(T)				\
    T va, vb, vr;				\
    memcpy(&va, a, sizeof(T));			\
    memcpy(&vb, b, sizeof(T));

# define BYTES_ADD_BODY(T)						\
    { BYTES_LOAD2(T)							\
      if (__builtin_add_overflow(va, vb, &vr)) return false;		\
      return storeNative(out, &vr, w, kind, hasNA); }

# define BYTES_SUB_BODY(T)						\
    { BYTES_LOAD2(T)							\
      if (__builtin_sub_overflow(va, vb, &vr)) return false;		\
      return storeNative(out, &vr, w, kind, hasNA); }

# define BYTES_MUL_BODY(T)						\
    { BYTES_LOAD2(T)							\
      if (__builtin_mul_overflow(va, vb, &vr)) return false;		\
      return storeNative(out, &vr, w, kind, hasNA); }

# ifdef __SIZEOF_INT128__
/* The signed multiply through a 128-bit product: the widening multiply
   is two instructions on a target that has them, and there is no
   checked multiply left for the compiler to turn into a call. */
#  define BYTES_MUL_S64_BODY(T)						\
    { BYTES_LOAD2(T)							\
      bytes_int128_t p = (bytes_int128_t) va * vb;			\
      bytes_int128_t lim = (bytes_int128_t) 1 << (8 * sizeof(T) - 1);	\
      if (p < -lim || p >= lim) return false;				\
      vr = (T) p;							\
      return storeNative(out, &vr, w, kind, hasNA); }

/* The widest signed multiply has nothing above it to promote into, so
   its check is written out: magnitudes in the unsigned domain, where
   wrapping is defined.  It costs a 128-bit division, still far less
   than the general kernel's 256 byte multiplies. */
static bool mulOverflowS128(bytes_int128_t a, bytes_int128_t b,
			    bytes_int128_t *r)
{
    bytes_uint128_t ua = (bytes_uint128_t) a, ub = (bytes_uint128_t) b;
    bytes_uint128_t ma = a < 0 ? (bytes_uint128_t) 0 - ua : ua;
    bytes_uint128_t mb = b < 0 ? (bytes_uint128_t) 0 - ub : ub;

    /* the magnitude a product may reach: one more when it is negative */
    bytes_uint128_t lim = (bytes_uint128_t) 1 << 127;
    if ((a < 0) == (b < 0)) lim--;

    if (mb != 0 && ma > lim / mb) return true;
    *r = (bytes_int128_t) (ua * ub);	/* in range, so this is the product */

    return false;
}

#  define BYTES_MUL_S128_BODY(T)					\
    { BYTES_LOAD2(T)							\
      if (mulOverflowS128(va, vb, &vr)) return false;			\
      return storeNative(out, &vr, w, kind, hasNA); }

#  define BYTES_MUL_W8_SIGNED	case 8:  BYTES_MUL_S64_BODY(int64_t)
# else
#  define BYTES_MUL_W8_SIGNED	/* signed width 8 stays general too */
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
# define BYTES_NATIVE_MUL(w, kind)				\
    do {							\
	if (!useNative()) break;				\
	if ((kind) == BYTEVEC_UINT)				\
	    switch (w) {					\
	    case 1:  BYTES_MUL_BODY(uint8_t)			\
	    case 2:  BYTES_MUL_BODY(uint16_t)			\
	    case 4:  BYTES_MUL_BODY(uint32_t)			\
	    case 8:  BYTES_MUL_BODY(uint64_t)			\
	    BYTES_NATIVE_W16(BYTES_MUL_BODY, bytes_uint128_t)	\
	    }							\
	else if ((kind) == BYTEVEC_INT)				\
	    switch (w) {					\
	    case 1:  BYTES_MUL_BODY(int8_t)			\
	    case 2:  BYTES_MUL_BODY(int16_t)			\
	    case 4:  BYTES_MUL_BODY(int32_t)			\
	    BYTES_MUL_W8_SIGNED					\
	    BYTES_NATIVE_W16(BYTES_MUL_S128_BODY, bytes_int128_t) \
	    }							\
    } while (0)

/* 0 - v, which overflows only at the most negative value -- and, for
   an unsigned element, at every value but zero */
# define BYTES_NEG_BODY(T)						\
    { T va, vr;								\
      memcpy(&va, a, sizeof(T));					\
      if (__builtin_sub_overflow((T) 0, va, &vr)) return false;		\
      return storeNative(out, &vr, w, kind, hasNA); }

/* unsigned: no signs to reconcile, so C's / and % are already the
   floor division and modulo that %/% and %% mean */
# define BYTES_DIVMOD_U_BODY(T)						\
    { BYTES_LOAD2(T)							\
      if (vb == 0) return false;					\
      vr = wantQuotient ? (T) (va / vb) : (T) (va % vb);		\
      return storeNative(out, &vr, w, kind, hasNA); }

/* signed: C truncates towards zero, so a quotient with a nonzero
   remainder steps down by one and the remainder takes the divisor's
   sign.  T_MIN / -1 is the one division C leaves undefined; its
   quotient overflows, while its remainder is a perfectly good zero. */
# define BYTES_DIVMOD_S_BODY(T)						\
    { BYTES_LOAD2(T)							\
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

# define BYTES_NATIVE2(UBODY, SBODY, w, kind)	do { } while (0)
# define BYTES_NATIVE(BODY, w, kind)		do { } while (0)
# define BYTES_NATIVE_MUL(w, kind)		do { } while (0)

#endif

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
   representable, and saying so is better than silently producing NA.

   hasNA is the vector's own answer to whether anything is reserved at
   all: with na = FALSE every bit pattern of the width is a value, so
   the top of the range is reachable rather than overflow.

   Shared with the text parser in bytes.c rather than restated there:
   which values a width admits is subtle enough that two copies would
   eventually disagree.  Nothing here is limited to the arithmetic
   widths -- it just loops over w. */
attribute_hidden
bool R_bytesMagFits(const Rbyte *v, int w, int kind, bool negative, bool hasNA)
{
    if (kind == BYTEVEC_UINT) {
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
    if (!R_bytesMagFits(mag, w, kind, negative, hasNA)) return false;

    if (negative) magNegate(mag, w);
    fromMSB(out, mag, w);

    return true;
}

/* ---- the kernels; all take and return MSB-first magnitudes ---- */

static bool eltAdd(Rbyte *out, const Rbyte *a, const Rbyte *b, int w, int kind, bool hasNA)
{
    BYTES_NATIVE(BYTES_ADD_BODY, w, kind);

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
	return storeResult(out, R, w, kind, false, hasNA);
    }

    /* signed: overflow iff the operands shared a sign the result lacks */
    int sa = A[0] & 0x80, sb = B[0] & 0x80, sr = R[0] & 0x80;
    if (sa == sb && sr != sa) return false;

    bool neg = sr != 0;
    if (neg) magNegate(R, w);

    return storeResult(out, R, w, kind, neg, hasNA);
}

static bool eltSub(Rbyte *out, const Rbyte *a, const Rbyte *b, int w, int kind, bool hasNA)
{
    BYTES_NATIVE(BYTES_SUB_BODY, w, kind);

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
	return storeResult(out, R, w, kind, false, hasNA);
    }

    int sa = A[0] & 0x80, sb = B[0] & 0x80, sr = R[0] & 0x80;
    if (sa != sb && sr != sa) return false;

    bool neg = sr != 0;
    if (neg) magNegate(R, w);

    return storeResult(out, R, w, kind, neg, hasNA);
}

static bool eltMul(Rbyte *out, const Rbyte *a, const Rbyte *b, int w, int kind, bool hasNA)
{
    BYTES_NATIVE_MUL(w, kind);

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
    BYTES_NATIVE2(BYTES_DIVMOD_U_BODY, BYTES_DIVMOD_S_BODY, w, kind);

    Rbyte A[MAXW], B[MAXW], Q[MAXW], R[MAXW];
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
	Rbyte T[MAXW];
	memcpy(T, B, (size_t) w);
	magSub(T, R, w);
	memcpy(R, T, (size_t) w);
    }

    if (wantQuotient)
	return storeResult(out, Q, w, kind, negq, hasNA);

    return storeResult(out, R, w, kind, negb, hasNA);
}

/* Unary minus, which R_bytesUnary() used to do inline.  A kernel of
   its own so that it dispatches to a native type the way the binary
   ones do; the general form is a negation of the magnitude, whose sign
   flips. */
static bool eltNeg(Rbyte *out, const Rbyte *a, int w, int kind, bool hasNA)
{
    BYTES_NATIVE(BYTES_NEG_BODY, w, kind);

    Rbyte A[MAXW];
    bool neg = magFromElt(A, a, w, kind);

    return storeResult(out, A, w, kind, !neg, hasNA);
}

/* Store a C integer value into an element.  Returns false if it is not
   representable -- a negative into an unsigned kind, a magnitude too
   wide, or the reserved NA pattern.

   Unlike the kernels above this is not restricted to the arithmetic
   widths: narrowing an integer into a wide element is well defined and
   is reached from c(), subassignment and comparison, none of which
   require arithmetic.  The scratch buffer is therefore sized for the
   widest element the type allows, not for MAXW. */
static bool eltFromLong(Rbyte *out, long long v, int w, int kind, bool hasNA)
{
    if (kind == BYTEVEC_UINT && v < 0) return false;

    bool neg = v < 0;
    unsigned long long m = neg
	? (unsigned long long) (-(v + 1)) + 1ULL
	: (unsigned long long) v;
    Rbyte mag[BYTEVEC_MAX_WIDTH];

    memset(mag, 0, (size_t) w);
    for (int i = 0; i < w && m; i++) {
	mag[w - 1 - i] = (Rbyte) (m & 0xFF);
	m >>= 8;
    }
    if (m) return false;			/* needed more than w bytes */

    return storeResult(out, mag, w, kind, neg, hasNA);
}

/* ---- conversion between 'bytes' types ---- */

/* One element from (iw, ik) to (ow, ok), preserving the value.  Returns
   false if it does not fit, which includes landing on the reserved NA
   pattern.

   Defined at every width the type allows, not just the arithmetic ones
   -- widening an int64 into an int2040 is not arithmetic, and there is
   no reason to refuse it -- so the scratch buffer is full width, as the
   text conversions in bytes.c are. */
static bool eltConvert(Rbyte *out, int ow, int ok, bool ohasNA,
		       const Rbyte *in, int iw, int ik)
{
    Rbyte mag[BYTEVEC_MAX_WIDTH];
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
    if (neg && ok == BYTEVEC_UINT) return false;

    if (!R_bytesMagFits(mag, ow, ok, neg, ohasNA)) return false;
    if (neg) magNegate(mag, ow);
    fromMSB(out, mag, ow);

    return true;
}

/* x reinterpreted at another width, kind, or NA reservation.  The
   opaque kind takes no part in a width or kind change: its elements are
   byte strings, so there is no value to preserve and no answer to which
   end of a short one to pad -- the same question parseHex() refuses.
   Changing only whether NA is reserved is meaningful for every kind and
   is handled without reading any values. */
attribute_hidden
SEXP R_bytesFromBytes(SEXP x, int w, int kind, int hasNA, SEXP call)
{
    int xw = BYTEVEC_WIDTH(x), xk = BYTEVEC_KIND(x);
    bool xNA = BYTEVEC_HAS_NA(x);
    bool sameType = (xw == w && xk == kind);

    if (!sameType && (xk == BYTEVEC_OPAQUE || kind == BYTEVEC_OPAQUE))
	errorcall(call,
		  _("cannot convert '%s' to '%s': an opaque element is a byte string, not a value"),
		  R_bytesTypeName(x), R_bytesTypeNameOf(w, kind));

    R_xlen_t n = XLENGTH(x);
    SEXP ans = PROTECT(R_allocBytesVector(n, w, kind, hasNA ? TRUE : FALSE));
    R_xlen_t nLost = 0, nReserved = 0;

    for (R_xlen_t i = 0; i < n; i++) {
	const Rbyte *p = BYTEVEC_ELT_RO(x, i);
	Rbyte *o = BYTEVEC_ELT(ans, i);

	if (xNA && R_bytesEltIsNA(p, xw, xk)) {
	    R_bytesCheckNA(ans);	/* the target reserves nothing */
	    R_bytesSetEltNA(o, w, kind);
	    continue;
	}

	if (sameType) {
	    /* only the reservation changed; a value that now collides
	       with it becomes NA, as it does arriving from raw bytes */
	    memcpy(o, p, (size_t) w);
	    if (hasNA && R_bytesEltIsNA(o, w, kind)) nReserved++;
	    continue;
	}

	if (!eltConvert(o, w, kind, hasNA != 0, p, xw, xk)) {
	    R_bytesCheckNA(ans);
	    R_bytesSetEltNA(o, w, kind);
	    nLost++;
	}
    }

    if (nLost)
	warningcall(call, _("NAs introduced by values outside the range of '%s'"),
		    R_bytesTypeName(ans));
    if (nReserved) R_bytesWarnReserved(ans);

    UNPROTECT(1);

    return ans;
}

/* Whether x is a logical or integer vector holding nothing but NA.
   NA_LOGICAL and NA_INTEGER are the same value, so one test serves
   both. */
attribute_hidden
bool R_bytesAllNA(SEXP x)
{
    SEXPTYPE t = TYPEOF(x);

    if (t != INTSXP && t != LGLSXP)
	return false;

    R_xlen_t n = XLENGTH(x);
    for (R_xlen_t i = 0; i < n; i++)
	if ((t == INTSXP ? INTEGER_ELT(x, i) : LOGICAL_ELT(x, i)) != NA_INTEGER)
	    return false;

    return true;
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
SEXP R_bytesNarrow(SEXP x, int w, int kind, int hasNA, SEXP call)
{
    SEXPTYPE t = TYPEOF(x);

    /* An opaque element is a byte string, so there is no number to read
       into one.  NA is the exception: it is the absence of a value
       rather than a value, and every kind reserves a sentinel for it,
       so NA is the one right-hand side an opaque vector can take. */
    if (kind == BYTEVEC_OPAQUE && !R_bytesAllNA(x))
	errorcall(call,
		  _("cannot combine an opaque 'bytes' vector with type '%s'"),
		  R_typeToChar(x));
    if (t != INTSXP && t != LGLSXP)
	errorcall(call,
		  _("'%s' and '%s' cannot be combined; use an integer operand (1L), or as.numeric() for double arithmetic"),
		  "bytes", R_typeToChar(x));

    R_xlen_t n = XLENGTH(x);
    SEXP ans = PROTECT(R_allocBytesVector(n, w, kind, hasNA ? TRUE : FALSE));
    R_xlen_t nLost = 0;

    for (R_xlen_t i = 0; i < n; i++) {
	int v = (t == INTSXP) ? INTEGER_ELT(x, i) : LOGICAL_ELT(x, i);
	Rbyte *p = BYTEVEC_ELT(ans, i);
	if (v == NA_INTEGER || !eltFromLong(p, (long long) v, w, kind, hasNA != 0)) {
	    R_bytesCheckNA(ans);
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
	R_bytesCheckSameNA(x, y);
	w = BYTEVEC_WIDTH(x) > BYTEVEC_WIDTH(y)
	    ? BYTEVEC_WIDTH(x) : BYTEVEC_WIDTH(y);
    }
    else if (TYPEOF(x) == BYTESXP)
	*py = R_bytesNarrow(y, w, kind, BYTEVEC_HAS_NA(x), call);
    else
	*px = R_bytesNarrow(x, w, kind, BYTEVEC_HAS_NA(y), call);

    *pw = w;
    *pk = kind;
}

/* ---- vector level ---- */

/* dim, dimnames and names belong to R_binary(), which restores them from
   the operands it was given.  Every other attribute comes from an
   operand of the result's length, x last so that its own win -- the
   rule integer_binary() and real_binary() end with. */
static void bytesCopyMostAttrib(SEXP ans, SEXP x, SEXP y, R_xlen_t n)
{
    if (n == XLENGTH(y) && ATTRIB(y) != R_NilValue) copyMostAttrib(y, ans);
    if (n == XLENGTH(x) && ATTRIB(x) != R_NilValue) copyMostAttrib(x, ans);
}

attribute_hidden
SEXP R_bytesArith(SEXP call, int oper, SEXP x, SEXP y)
{
    if ((TYPEOF(x) == BYTESXP && BYTEVEC_KIND(x) == BYTEVEC_OPAQUE) ||
	(TYPEOF(y) == BYTESXP && BYTEVEC_KIND(y) == BYTEVEC_OPAQUE))
	errorcall(call, _("arithmetic is not defined for opaque 'bytes' vectors"));

    /* bytesBinaryOperands() replaces a non-'bytes' operand with a
       narrowed temporary, which carries none of the caller's
       attributes; those have to be read from the operands as given */
    SEXP ox = PROTECT(x), oy = PROTECT(y);

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
	/* x may be a narrowed temporary held only by the index above, so
	   it has to outlive the allocation that reads its NA flag */
	/* bare, as integer_binary() and real_binary() return it: both
	   leave for a zero-length result before their copyMostAttrib()
	   tail, and this is the same operation on the same operands */
	SEXP val = R_allocBytesVector(0, w, kx,
				      BYTEVEC_HAS_NA(x) ? TRUE : FALSE);
	UNPROTECT(4); /* y, x, oy, ox */

	return val;
    }
    /* R_binary has already warned about a length mismatch */
    R_xlen_t n = nx > ny ? nx : ny;

    bool hasNA = BYTEVEC_HAS_NA(x);
    SEXP ans = PROTECT(R_allocBytesVector(n, w, kx, hasNA ? TRUE : FALSE));
    R_xlen_t nOver = 0;

    /* Hoisted: the element macros re-read the payload pointer and the
       width out of the header every time, which at one machine
       instruction per operand is no longer beneath notice.  Nothing in
       the loop allocates, so the pointers cannot move. */
    const Rbyte *bx = BYTEVEC_DATA_RO(x), *by = BYTEVEC_DATA_RO(y);
    Rbyte *ba = BYTEVEC_DATA(ans);
    R_xlen_t i, ix, iy;

    MOD_ITERATE2(n, nx, ny, i, ix, iy, {
	/* one declaration per statement: this is a macro argument, and
	   braces do not shield a comma from the preprocessor */
	const Rbyte *px = bx + ix * wx;
	const Rbyte *py = by + iy * wy;
	Rbyte *pa = ba + i * w;

	if (hasNA && (eltIsNA(px, wx, kx) || eltIsNA(py, wy, ky))) {
	    R_bytesSetEltNA(pa, w, kx);
	    continue;
	}

	/* Promote a narrower operand to the result width.  Promotion is
	   by max(width), so at least one operand is already there and
	   usually both are; doing this unconditionally cost four byte
	   loops per element to copy values that were already correct. */
	Rbyte sx[MAXW];
	Rbyte sy[MAXW];
	if (wx != w) {
	    Rbyte t[MAXW];
	    widenMSB(t, w, px, wx, kx);
	    fromMSB(sx, t, w);
	    px = sx;
	}
	if (wy != w) {
	    Rbyte t[MAXW];
	    widenMSB(t, w, py, wy, ky);
	    fromMSB(sy, t, w);
	    py = sy;
	}

	bool ok;
	switch (oper) {
	case PLUSOP:  ok = eltAdd(pa, px, py, w, kx, hasNA); break;
	case MINUSOP: ok = eltSub(pa, px, py, w, kx, hasNA); break;
	case TIMESOP: ok = eltMul(pa, px, py, w, kx, hasNA); break;
	case IDIVOP:  ok = eltDivMod(pa, px, py, w, kx, true, hasNA); break;
	case MODOP:   ok = eltDivMod(pa, px, py, w, kx, false, hasNA); break;
	default:
	    UNPROTECT(5); /* ans, y, x, oy, ox */
	    errorcall(call,
		      _("this operator is not defined for 'bytes' vectors"));
	}

	if (!ok) {
	    R_bytesCheckNA(ans);	/* nothing to fall back on */
	    R_bytesSetEltNA(pa, w, kx);
	    nOver++;
	}
    });

    if (nOver)
	warningcall(call, (oper == IDIVOP || oper == MODOP)
		    ? _("NAs produced by division by zero or overflow")
		    : _("NAs produced by integer overflow"));

    bytesCopyMostAttrib(ans, ox, oy, n);
    UNPROTECT(5); /* ans, y, x, oy, ox */

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
    bool hasNA = BYTEVEC_HAS_NA(x);
    SEXP ans = PROTECT(R_allocBytesVector(n, w, k, hasNA ? TRUE : FALSE));
    R_xlen_t nOver = 0;

    /* R_unary() hands the result straight back to do_arith, so the
       attributes the other unary kernels keep have to be kept here.
       integer_unary() and real_unary() work in a duplicate() of the
       operand, which is every attribute, not just the three R_binary()
       would have restored. */
    SHALLOW_DUPLICATE_ATTRIB(ans, x);

    /* hoisted as in R_bytesArith(); nothing here allocates */
    const Rbyte *bx = BYTEVEC_DATA_RO(x);
    Rbyte *ba = BYTEVEC_DATA(ans);

    for (R_xlen_t i = 0; i < n; i++) {
	const Rbyte *px = bx + i * w;
	Rbyte *pa = ba + i * w;

	if (hasNA && eltIsNA(px, w, k)) {
	    R_bytesSetEltNA(pa, w, k);
	    continue;
	}

	if (!eltNeg(pa, px, w, k, hasNA)) {
	    R_bytesCheckNA(ans);
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
   conditional on the width.  Defined for every width the type allows,
   since a value too large for a double is a precision question rather
   than an arithmetic one -- hence the full-width scratch buffer. */
attribute_hidden double R_bytesEltAsReal(const Rbyte *p, int w, int kind)
{
    Rbyte A[BYTEVEC_MAX_WIDTH];
    bool neg = false;

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

    /* The loop below writes through RAW0(), INTEGER0() or REAL0() and
       picks between them by elimination, so a target this function does
       not handle has to be turned away before anything is allocated. */
    if (type != RAWSXP && type != INTSXP && type != REALSXP)
	error(_("cannot coerce a 'bytes' vector to type '%s'"),
	      type2char(type));

    if (k == BYTEVEC_OPAQUE)
	error(_("cannot coerce an opaque 'bytes' vector to type '%s'"),
	      type2char(type));

    bool hasNA = BYTEVEC_HAS_NA(x);
    SEXP ans = PROTECT(allocVector(type, n));
    /* as coerceToReal() and the rest of the coerceToXXX() family do:
       coerceVector() is not as.vector(), and its caller is entitled to
       the dim and names of what it handed in */
    SHALLOW_DUPLICATE_ATTRIB(ans, x);
    R_xlen_t nLost = 0, nOver = 0;

    for (R_xlen_t i = 0; i < n; i++) {
	const Rbyte *p = BYTEVEC_ELT_RO(x, i);

	if (hasNA && R_bytesEltIsNA(p, w, k)) {
	    /* raw has no NA, so a missing value becomes 00 and counts as
	       lost, exactly as as.raw(NA_integer_) does */
	    if (type == RAWSXP) { RAW0(ans)[i] = 0; nLost++; }
	    else if (type == INTSXP) INTEGER0(ans)[i] = NA_INTEGER;
	    else REAL0(ans)[i] = NA_REAL;
	    continue;
	}

	double d = R_bytesEltAsReal(p, w, k);
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
	    /* Two different failures, and the wider widths reach both:
	       past 2^53 a double can no longer name every integer, and
	       past DBL_MAX it cannot name the value at all.  The second
	       is not a rounding of the value, so it does not report as
	       one. */
	    if (!R_FINITE(d)) nOver++;
	    else if (fabs(d) > 9007199254740992.0) nLost++;
	    REAL0(ans)[i] = d;
	}
    }

    if (nLost)
	warning(type == RAWSXP
		? _("out-of-range values treated as 0 in coercion to raw")
		: (type == INTSXP
		   ? _("NAs introduced by coercion to integer range")
		   : _("'bytes' values above 2^53 lose precision as double")));
    if (nOver)
	warning(_("'bytes' values beyond the range of double coerced to Inf"));

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
    /* sum and prod accumulate; min and max only ever compare */
    bool arith = (iop == 0 || iop == 4);
    int kind = -1, w = 0, wmin = 0, hasNA = -1;

    /* one pass to settle the result kind, width and NA flag */
    for (SEXP t = args; t != R_NilValue; t = CDR(t)) {
	SEXP a = CAR(t);
	if (TAG(t) == R_NaRmSymbol) continue;
	if (TYPEOF(a) == NILSXP) continue;
	if (TYPEOF(a) != BYTESXP)
	    errorcall(call, _("cannot mix 'bytes' vectors with other types"));
	if (arith && BYTEVEC_KIND(a) == BYTEVEC_OPAQUE)
	    errorcall(call,
		      _("'%s' is not defined for opaque 'bytes' vectors"),
		      iop == 0 ? "sum" : "prod");
	if (kind == -1) {
	    kind = BYTEVEC_KIND(a);
	    hasNA = BYTEVEC_HAS_NA(a);
	    wmin = BYTEVEC_WIDTH(a);
	}
	else {
	    if (kind != BYTEVEC_KIND(a))
		errorcall(call, _("cannot combine 'bytes' vectors of different kinds"));
	    if (hasNA != BYTEVEC_HAS_NA(a))
		errorcall(call, _("cannot combine 'bytes' vectors that differ in whether NA is representable"));
	    if (BYTEVEC_WIDTH(a) < wmin) wmin = BYTEVEC_WIDTH(a);
	}
	if (BYTEVEC_WIDTH(a) > w) w = BYTEVEC_WIDTH(a);
    }

    /* Only promoting a narrower operand to the result width needs the
       arithmetic machinery; an element comparison is defined for every
       width and every kind, so min and max over equal widths are not
       restricted the way sum and prod are. */
    bool promote = (w != wmin);
    if ((arith || promote) && !arithWidthOK(w))
	errorcall(call,
		  _("arithmetic on 'bytes' vectors is only defined for widths 1, 2, 4, 8 and 16"));
    if (promote && kind == BYTEVEC_OPAQUE)
	errorcall(call, _("cannot combine opaque 'bytes' vectors of widths %d and %d"),
		  wmin, w);

    SEXP ans = PROTECT(R_allocBytesVector(1, w, kind, hasNA ? TRUE : FALSE));
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
	const Rbyte *ba = BYTEVEC_DATA_RO(a);	/* hoisted; see R_bytesArith */
	R_xlen_t na = XLENGTH(a);

	for (R_xlen_t i = 0; i < na; i++) {
	    const Rbyte *p = ba + i * aw;

	    if (hasNA && eltIsNA(p, aw, kind)) {
		if (narm) continue;
		isNA = true;
		break;
	    }

	    /* widening buffers are MAXW, so this must stay behind the
	       arithWidthOK() check above; at equal widths there is
	       nothing to widen and any width is fine */
	    Rbyte wide[MAXW], buf[MAXW];
	    const Rbyte *cur = p;
	    if (aw != w) {
		widenMSB(wide, w, p, aw, kind);
		fromMSB(buf, wide, w);
		cur = buf;
	    }

	    if (!seen && (iop == 2 || iop == 3)) {
		memcpy(acc, cur, (size_t) w);
		seen = true;
		continue;
	    }
	    seen = true;

	    switch (iop) {
	    case 0:
		if (!eltAdd(acc, acc, cur, w, kind, hasNA)) over = true;
		break;
	    case 4:
		if (!eltMul(acc, acc, cur, w, kind, hasNA)) over = true;
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
	/* Every other type warns here and returns +/-Inf.  A fixed-width
	   type has no Inf, so NA stands in -- and when the type does not
	   reserve an NA either there is nothing to return, which is the
	   one case that has to stay an error. */
	if (!hasNA) {
	    UNPROTECT(1);
	    errorcall(call,
		      _("no non-missing arguments to '%s', and this 'bytes' type cannot represent NA"),
		      iop == 2 ? "min" : "max");
	}
	warningcall(call, _("no non-missing arguments to %s; returning NA"),
		    iop == 2 ? "min" : "max");
	isNA = true;
    }
    if (isNA) {
	R_bytesCheckNA(ans);
	R_bytesSetEltNA(acc, w, kind);
    }
    if (over)
	warningcall(call, _("NAs produced by integer overflow"));

    UNPROTECT(1);

    return ans;
}
