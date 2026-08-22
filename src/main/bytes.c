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
 *  BYTESXP: vectors of fixed-width opaque data.
 *
 *  A 'bytes' vector holds n elements of w bytes each, where w is a
 *  per-vector property recorded in the sxpinfo gp field rather than
 *  implied by the SEXPTYPE.  XLENGTH() counts elements, not bytes:
 *  that is the whole reason this is a distinct type rather than a
 *  flavour of RAWSXP, whose length is unavoidably a byte count.
 *
 *  The type is deliberately opaque.  Elements are compared and hashed
 *  as byte blocks and are never interpreted as numbers, so there is no
 *  coercion hierarchy to join and no arithmetic to define.  Every
 *  operation on the payload reduces to memcmp() or a byte hash over
 *  BYTEVEC_WIDTH(x) bytes.
 */

#ifdef HAVE_CONFIG_H
# include <config.h>
#endif

#include <ctype.h>

#include <Defn.h>
#include <Internal.h>
#include <Print.h>  /* R_print.na_string */
#include <R_ext/Itermacros.h>  /* MOD_ITERATE2 */

/* See BYTEVEC_NA_BYTE in Defn.h for why OPAQUE/UINT reserve all-0xFF
   and INT reserves INT_MIN instead. */
Rboolean R_bytesEltIsNA(const Rbyte *p, int width, int kind)
{
    if (kind == BYTEVEC_INT) {
	if (p[BYTEVEC_MSB(0, width)] != 0x80) return FALSE;
	for (int i = 1; i < width; i++)
	    if (p[BYTEVEC_MSB(i, width)] != 0x00) return FALSE;
	return TRUE;
    }

    for (int i = 0; i < width; i++)
	if (p[i] != BYTEVEC_NA_BYTE) return FALSE;

    return TRUE;
}

void R_bytesSetEltNA(Rbyte *p, int width, int kind)
{
    if (kind == BYTEVEC_INT) {
	memset(p, 0x00, (size_t) width);
	p[BYTEVEC_MSB(0, width)] = 0x80;
	return;
    }

    memset(p, BYTEVEC_NA_BYTE, (size_t) width);
}

/* Order, ignoring NA (callers handle that).  OPAQUE compares bytes in
   storage order; the numeric kinds compare from the most significant
   byte down, with the top byte read as signed for INT.  Working a byte
   at a time keeps this correct for every width rather than only the
   ones with a native C type behind them. */
int R_bytesEltCmp(const Rbyte *a, const Rbyte *b, int width, int kind)
{
    if (kind == BYTEVEC_OPAQUE) {
	int c = memcmp(a, b, (size_t) width);
	return (c < 0) ? -1 : ((c > 0) ? 1 : 0);
    }

    int top = BYTEVEC_MSB(0, width);
    if (kind == BYTEVEC_INT) {
	signed char sa = (signed char) a[top], sb = (signed char) b[top];
	if (sa != sb) return sa < sb ? -1 : 1;
    }
    else if (a[top] != b[top])
	return a[top] < b[top] ? -1 : 1;

    for (int i = 1; i < width; i++) {
	Rbyte x = a[BYTEVEC_MSB(i, width)], y = b[BYTEVEC_MSB(i, width)];
	if (x != y) return x < y ? -1 : 1;
    }

    return 0;
}

/* Decimal rendering, any width, by repeated division by 10 on a
   scratch copy held most-significant-byte first.  Returns a pointer
   into a static buffer, like the other Encode* functions.  A width-255
   value needs at most 615 digits plus a sign. */
const char *R_bytesEltDecimal(const Rbyte *p, int width, int kind)
{
    static char buff[8 * BYTEVEC_MAX_WIDTH / 3 + 4];
    Rbyte tmp[BYTEVEC_MAX_WIDTH];
    bool negative = false;

    for (int i = 0; i < width; i++)
	tmp[i] = p[BYTEVEC_MSB(i, width)];

    if (kind == BYTEVEC_INT && (tmp[0] & 0x80)) {
	negative = true;
	R_bytesMagNegate(tmp, width);
    }

    char digits[sizeof buff];
    int nd = 0;
    for (;;) {
	unsigned int rem = 0;
	bool nonzero = false;
	for (int i = 0; i < width; i++) {
	    unsigned int cur = (rem << 8) | tmp[i];
	    tmp[i] = (Rbyte) (cur / 10u);
	    rem = cur % 10u;
	    if (tmp[i]) nonzero = true;
	}
	digits[nd++] = (char) ('0' + rem);
	if (!nonzero) break;
    }

    char *q = buff;
    if (negative) *q++ = '-';
    while (nd > 0) *q++ = digits[--nd];
    *q = '\0';

    return buff;
}

/* ---- text to element: the inverse of the two renderers above -------

   Text is how a 64-bit identifier usually reaches R -- a column of a
   CSV, a field of a JSON document, a line of a log -- and unlike a raw
   payload it carries no byte order for the reader to get wrong.  It is
   also what makes as.character() reversible, which deparse() relies on.

   Scratch buffers here are BYTEVEC_MAX_WIDTH, not the arithmetic
   MAXW: conversion to and from text is defined at every width, as
   R_bytesEltDecimal() above is, while arithmetic stops at 16 bytes. */

/* mag = mag * mul + add, MSB-first; false if that carried out of w
   bytes, which is the only way a decimal literal can be too big */
static bool magMulAdd(Rbyte *mag, int w, unsigned int mul, unsigned int add)
{
    unsigned int carry = add;

    for (int i = w - 1; i >= 0; i--) {
	unsigned int v = (unsigned int) mag[i] * mul + carry;
	mag[i] = (Rbyte) (v & 0xFF);
	carry = v >> 8;
    }

    return carry == 0;
}

static int hexDigit(char c)
{
    if (c >= '0' && c <= '9') return c - '0';
    if (c >= 'a' && c <= 'f') return c - 'a' + 10;
    if (c >= 'A' && c <= 'F') return c - 'A' + 10;

    return -1;
}

static R_bytes_parse_t parseHex(Rbyte *out, const char *s, int w)
{
    /* exactly the form EncodeBytes() writes: 2*w lower- or upper-case
       hex digits in storage order, nothing else.  A short string is a
       syntax error rather than a zero-padded value: which end to pad
       is the question the opaque kind exists to refuse to answer.
       Surrounding space is skipped, as parseDecimal() skips it and as
       as.raw() tolerates it. */
    while (isspace((int) (unsigned char) *s)) s++;

    for (int i = 0; i < w; i++) {
	if (!s[2 * i] || !s[2 * i + 1])
	    return BYTES_PARSE_SYNTAX;

	int hi = hexDigit(s[2 * i]), lo = hexDigit(s[2 * i + 1]);
	if (hi < 0 || lo < 0)
	    return BYTES_PARSE_SYNTAX;

	out[i] = (Rbyte) ((hi << 4) | lo);
    }

    s += 2 * w;
    while (isspace((int) (unsigned char) *s)) s++;

    return *s ? BYTES_PARSE_SYNTAX : BYTES_PARSE_OK;
}

static R_bytes_parse_t parseDecimal(Rbyte *out, const char *s, int w,
				    int kind, bool hasNA)
{
    bool negative = false;

    while (isspace((int) (unsigned char) *s)) s++;
    if (*s == '+' || *s == '-')
	negative = (*s++ == '-');
    if (!isdigit((int) (unsigned char) *s))
	return BYTES_PARSE_SYNTAX;

    Rbyte mag[BYTEVEC_MAX_WIDTH];
    memset(mag, 0, (size_t) w);

    bool over = false;
    for (; isdigit((int) (unsigned char) *s); s++)
	if (!magMulAdd(mag, w, 10u, (unsigned int) (*s - '0')))
	    over = true;

    while (isspace((int) (unsigned char) *s)) s++;
    if (*s)
	return BYTES_PARSE_SYNTAX;

    /* an unsigned element has no negative values, but "-0" is 0 */
    if (negative && kind == BYTEVEC_UINT) {
	for (int i = 0; i < w; i++)
	    if (mag[i]) return BYTES_PARSE_RANGE;
	negative = false;
    }

    if (over || !R_bytesMagFits(mag, w, kind, negative, hasNA))
	return BYTES_PARSE_RANGE;

    if (negative)
	R_bytesMagNegate(mag, w);

    for (int i = 0; i < w; i++)
	out[BYTEVEC_MSB(i, w)] = mag[i];

    return BYTES_PARSE_OK;
}

R_bytes_parse_t R_bytesEltFromString(Rbyte *out, const char *s, int w,
				     int kind, bool hasNA)
{
    return (kind == BYTEVEC_OPAQUE) ? parseHex(out, s, w)
				    : parseDecimal(out, s, w, kind, hasNA);
}

/* ---- bitwise operations --------------------------------------------

   These are what the opaque kind wants and what arithmetic
   deliberately does not give it: masking an IPv6 prefix, bucketing a
   hash, testing a flag word.  They are also the cheapest thing this
   type can do -- and, or, xor and not are per byte -- so unlike
   arithmetic they carry no width restriction at all.

   NA propagates, as it does for integers.  A result landing exactly on
   the reserved NA value is reported rather than returned quietly,
   which is the rule arithmetic already follows for overflow. */

/* Index of the i-th most significant byte in storage order.  An opaque
   element is a byte string, so its first stored byte is its most
   significant one on every platform; the numeric kinds are stored
   natively and need the usual mapping. */
#define BITMSB(i, w, k) ((k) == BYTEVEC_OPAQUE ? (i) : BYTEVEC_MSB(i, w))

/* logical shift by n bits, either direction, over the whole element */
static void eltShift(Rbyte *out, const Rbyte *x, int w, int k, int n,
		     bool left)
{
    Rbyte tmp[BYTEVEC_MAX_WIDTH], res[BYTEVEC_MAX_WIDTH];
    int bs = n / 8, br = n % 8;

    for (int i = 0; i < w; i++)
	tmp[i] = x[BITMSB(i, w, k)];

    /* br == 0 makes the neighbour term shift by 8, which is zero once
       masked to a byte -- exactly the "no bits carried in" case */
    for (int i = 0; i < w; i++) {
	int j = left ? i + bs : i - bs;
	unsigned int v = 0;

	if (j >= 0 && j < w)
	    v = left ? ((unsigned int) tmp[j] << br)
		     : ((unsigned int) tmp[j] >> br);
	if (left ? (j + 1 < w) : (j - 1 >= 0))
	    v |= left ? ((unsigned int) tmp[j + 1] >> (8 - br))
		      : ((unsigned int) tmp[j - 1] << (8 - br));

	res[i] = (Rbyte) (v & 0xFF);
    }

    for (int i = 0; i < w; i++)
	out[BITMSB(i, w, k)] = res[i];
}

attribute_hidden
SEXP R_bytesBitwise(SEXP call, int oper, SEXP a, SEXP b)
{
    bool unary = (oper == 2), shift = (oper == 5 || oper == 6);

    /* and, or and xor are commutative, so either operand may be the
       'bytes' one; a shift count never is */
    if (!unary && !shift && TYPEOF(a) != BYTESXP) {
	SEXP t = a; a = b; b = t;
    }
    if (TYPEOF(a) != BYTESXP)
	errorcall(call, _("'a' and 'b' must have the same type"));

    int w = BYTEVEC_WIDTH(a), k = BYTEVEC_KIND(a);
    int hasNA = BYTEVEC_HAS_NA(a);

    if (shift) {
	if (TYPEOF(b) == BYTESXP)
	    errorcall(call, _("invalid '%s' argument"), "b");
	if (!isInteger(b)) b = coerceVector(b, INTSXP);
    }
    else if (!unary) {
	if (TYPEOF(b) != BYTESXP)
	    /* an opaque element is a byte string, so there is no number
	       to read into one -- except NA, which is the absence of a
	       value rather than a value.  R_bytesNarrow() draws that
	       line, here as it does for c(), == and [<-. */
	    b = R_bytesNarrow(b, w, k, hasNA, call);
	else
	    /* widths are not promoted the way arithmetic promotes them:
	       a mask that is not the width of what it masks is a
	       mistake, not a value to extend */
	    R_bytesCheckPair(call, a, b, "combine");
    }
    PROTECT(a);
    PROTECT(b);		/* may be the narrowed or coerced temporary */

    R_xlen_t m = XLENGTH(a), n = unary ? m : XLENGTH(b);
    R_xlen_t mn = (m && n) ? (m > n ? m : n) : 0;
    SEXP ans = PROTECT(R_allocBytesVectorUninit(mn, w, k, hasNA ? TRUE : FALSE));
    R_xlen_t i, ia, ib, nOver = 0;

    MOD_ITERATE2(mn, m, n, i, ia, ib, {
	const Rbyte *x = BYTEVEC_ELT_RO(a, ia);
	Rbyte *o = BYTEVEC_ELT(ans, i);
	bool na = hasNA && R_bytesEltIsNA(x, w, k);

	if (shift) {
	    int s = INTEGER_ELT(b, ib);
	    if (na || s == NA_INTEGER || s < 0 || s > 8 * w - 1) {
		/* bitwShiftL(1L, 32L) is NA, and so is this wherever NA
		   exists.  Where it does not, say what actually went
		   wrong: R_bytesCheckNA()'s message is about the data,
		   and here the shift count is the problem. */
		if (!hasNA)
		    errorcall(call,
			      _("shift out of range for a '%s' vector, which has no NA to return"),
			      R_bytesTypeName(a));
		R_bytesSetEltNA(o, w, k);
		continue;
	    }
	    eltShift(o, x, w, k, s, oper == 5);
	}
	else if (unary) {
	    if (na) {
		R_bytesSetEltNA(o, w, k);
		continue;
	    }
	    for (int j = 0; j < w; j++) o[j] = (Rbyte) ~x[j];
	}
	else {
	    const Rbyte *y = BYTEVEC_ELT_RO(b, ib);
	    if (na || (hasNA && R_bytesEltIsNA(y, w, k))) {
		R_bytesSetEltNA(o, w, k);
		continue;
	    }
	    switch (oper) {
	    case 1: for (int j = 0; j < w; j++) o[j] = x[j] & y[j]; break;
	    case 3: for (int j = 0; j < w; j++) o[j] = x[j] | y[j]; break;
	    default: for (int j = 0; j < w; j++) o[j] = x[j] ^ y[j]; break;
	    }
	}

	/* the bits are what they are; it is the reservation that makes
	   this value unavailable, so say so rather than return NA */
	if (hasNA && R_bytesEltIsNA(o, w, k)) {
	    nOver++;
	    R_bytesSetEltNA(o, w, k);
	}
    });

    if (nOver)
	warningcall(call, _("NAs produced by results equal to the reserved NA value"));
    UNPROTECT(3);

    return ans;
}

/* memcpy() of a whole payload, in pieces.  macOS builds have been seen
   not to copy 2^32 bytes or more in one call, which is what the
   chunked DUPLICATE_ATOMIC_VECTOR in duplicate.c is there for; the
   payload of this type reaches that size before any other vector does,
   a width-16 one needing only 2^28 elements.  Elsewhere the same macro
   is sidestepped by copying element by element. */
attribute_hidden void R_bytesMemcpy(Rbyte *dst, const Rbyte *src, size_t n)
{
#ifdef __APPLE__
    /* 1e6 elements at the widest type duplicate.c copies, as there */
    const size_t chunk = 16000000;

    while (n > chunk) {
	memcpy(dst, src, chunk);
	dst += chunk; src += chunk; n -= chunk;
    }
#endif
    if (n) memcpy(dst, src, n);
}

/* Called wherever an NA would be stored.  A vector that declines to
   reserve a value cannot represent one, so the operation stops rather
   than inventing something. */
void R_bytesCheckNA(SEXP x)
{
    if (!BYTEVEC_HAS_NA(x))
	error(_("missing values are not representable in this '%s' vector; it was created with na = FALSE"),
	      R_bytesTypeName(x));
}

/* What CHKVEC() is to DATAPTR(): only reached in the builds that ask for
   the check, which is why BYTEVEC_DATA() can afford to call it. */
void R_CheckBytesVector(SEXP x)
{
    if (TYPEOF(x) != BYTESXP)
	error("cannot get data pointer of '%s' objects", R_typeToChar(x));
}

/* Whether a value is reserved is part of the type, as the kind and the
   width are: combining vectors that disagree would either lose a real
   value or invent a missing one. */
void R_bytesCheckSameNA(SEXP x, SEXP y)
{
    if (TYPEOF(x) == BYTESXP && TYPEOF(y) == BYTESXP &&
	BYTEVEC_HAS_NA(x) != BYTEVEC_HAS_NA(y))
	error(_("cannot combine 'bytes' vectors that differ in whether NA is representable"));
}

/* The whole of that rule for a pair of 'bytes' operands: width, kind
   and NA reservation are all part of the type, and every pairwise
   operation -- c() and arithmetic combine, == compares, match()
   matches, [<- assigns between -- refuses a pair that disagrees.  One
   checker so the wording and the order of the checks cannot drift
   between them; only the verb differs.  Pass R_CurrentExpression as
   the call where none is at hand. */
void R_bytesCheckPair(SEXP call, SEXP x, SEXP y, const char *verb)
{
    if (BYTEVEC_WIDTH(x) != BYTEVEC_WIDTH(y))
	errorcall(call, _("cannot %s 'bytes' vectors of widths %d and %d"),
		  verb, BYTEVEC_WIDTH(x), BYTEVEC_WIDTH(y));
    if (BYTEVEC_KIND(x) != BYTEVEC_KIND(y))
	errorcall(call, _("cannot %s 'bytes' vectors of different kinds"),
		  verb);

    R_bytesCheckSameNA(x, y);
}

/* Rf_copyVector() and Rf_copyMatrix() are public API, and the only check
   they make is that the two SEXPTYPEs agree -- which a width-4 and a
   width-16 'bytes' vector both pass.  Their block copies stride one vector
   by the other's width, so a mismatch reads past the end of the source.
   Callers inside R pair R_allocVectorLike() with the vector being copied
   and so always agree; this is what stops a package from not. */
void R_bytesCheckSameType(SEXP x, SEXP y, const char *fun)
{
    if (BYTEVEC_WIDTH(x) != BYTEVEC_WIDTH(y) ||
	BYTEVEC_KIND(x) != BYTEVEC_KIND(y))
	error(_("'bytes' vectors differ in width or kind in '%s'"), fun);

    /* the reservation is part of the type as much as those two are:
       copying from a vector that reserves nothing into one that does
       would read a legitimate all-0xFF datum back as missing */
    R_bytesCheckSameNA(x, y);
}

/* allocVector(TYPEOF(s), n) cannot reproduce a per-vector width, so the
   generic "another vector like this one" sites go through here */
SEXP R_allocVectorLike(SEXP s, R_xlen_t length)
{
    if (TYPEOF(s) == BYTESXP)
	return R_allocBytesVector(length, BYTEVEC_WIDTH(s), BYTEVEC_KIND(s),
				  BYTEVEC_HAS_NA(s) ? TRUE : FALSE);

    return allocVector(TYPEOF(s), length);
}

/* the no-fill R_allocVectorLike(), for callers that overwrite every
   element; see R_allocBytesVectorUninit() */
SEXP R_allocVectorLikeUninit(SEXP s, R_xlen_t length)
{
    if (TYPEOF(s) == BYTESXP)
	return R_allocBytesVectorUninit(length, BYTEVEC_WIDTH(s),
					BYTEVEC_KIND(s),
					BYTEVEC_HAS_NA(s) ? TRUE : FALSE);

    return allocVector(TYPEOF(s), length);
}

/* The 'bytes' half of allocMatrix(): that allocator cannot carry a
   per-vector width either, so the matrix sites build the vector first
   and shape it here -- one place for the dim dance.  Returns x. */
SEXP R_bytesShapeMatrix(SEXP x, int nrow, int ncol)
{
    PROTECT(x);
    SEXP dim = PROTECT(allocVector(INTSXP, 2));
    INTEGER(dim)[0] = nrow;
    INTEGER(dim)[1] = ncol;
    setAttrib(x, R_DimSymbol, dim);
    UNPROTECT(2);

    return x;
}

/* allocMatrix() for a result like s -- the matrix counterpart of
   R_allocVectorLike(), extent guard included */
SEXP R_allocMatrixLike(SEXP s, int nrow, int ncol)
{
    if (TYPEOF(s) != BYTESXP)
	return allocMatrix(TYPEOF(s), nrow, ncol);

    if ((double) nrow * ncol > R_XLEN_T_MAX)
	error(_("too many elements specified"));

    return R_bytesShapeMatrix(R_allocVectorLike(s, (R_xlen_t) nrow * ncol),
			      nrow, ncol);
}

/* The rest of this section is the package-facing API declared in
   Rinternals.h.  R's own code uses the BYTEVEC_* accessors directly;
   these check the type first, as INTEGER() and RAW() do, because a
   package reaching for the wrong one should hear about it rather than
   read the payload of something else. */

static void checkBytes(SEXP x, const char *what)
{
    if (TYPEOF(x) != BYTESXP)
	error(_("%s() can only be applied to a '%s', not a '%s'"),
	      what, "bytes", R_typeToChar(x));
}

Rboolean R_isBytes(SEXP x)
{
    return TYPEOF(x) == BYTESXP ? TRUE : FALSE;
}

int R_bytesWidth(SEXP x)
{
    checkBytes(x, "R_bytesWidth");

    return BYTEVEC_WIDTH(x);
}

int R_bytesKind(SEXP x)
{
    checkBytes(x, "R_bytesKind");

    return BYTEVEC_KIND(x);
}

Rboolean R_bytesHasNA(SEXP x)
{
    checkBytes(x, "R_bytesHasNA");

    return BYTEVEC_HAS_NA(x) ? TRUE : FALSE;
}

/* Element addresses.  Preferred over BYTES() + i * width in package
   code for the same reason R's own code uses BYTEVEC_ELT: it is the
   one place the width has to be got right. */
Rbyte *R_bytesElt(SEXP x, R_xlen_t i)
{
    checkBytes(x, "R_bytesElt");
    if (i < 0 || i >= XLENGTH(x))
	error(_("subscript out of bounds"));

    return BYTEVEC_ELT(x, i);
}

const Rbyte *R_bytesEltRO(SEXP x, R_xlen_t i)
{
    checkBytes(x, "R_bytesEltRO");
    if (i < 0 || i >= XLENGTH(x))
	error(_("subscript out of bounds"));

    return BYTEVEC_ELT_RO(x, i);
}

Rboolean R_bytesIsNA(SEXP x, R_xlen_t i)
{
    checkBytes(x, "R_bytesIsNA");
    if (i < 0 || i >= XLENGTH(x))
	error(_("subscript out of bounds"));

    if (!BYTEVEC_HAS_NA(x))
	return FALSE;

    return R_bytesEltIsNA(BYTEVEC_ELT_RO(x, i), BYTEVEC_WIDTH(x),
			  BYTEVEC_KIND(x));
}

void R_bytesSetNA(SEXP x, R_xlen_t i)
{
    checkBytes(x, "R_bytesSetNA");
    if (i < 0 || i >= XLENGTH(x))
	error(_("subscript out of bounds"));

    R_bytesCheckNA(x);	/* errors if this vector reserves no NA value */

    R_bytesSetEltNA(BYTEVEC_ELT(x, i), BYTEVEC_WIDTH(x), BYTEVEC_KIND(x));
}

static int checkKind(SEXP skind)
{
    if (length(skind) != 1)
	error(_("invalid '%s' argument"), "kind");

    const char *k = CHAR(asChar(skind));

    if (!strcmp(k, "opaque"))   return BYTEVEC_OPAQUE;
    if (!strcmp(k, "unsigned")) return BYTEVEC_UINT;
    if (!strcmp(k, "signed"))   return BYTEVEC_INT;

    error(_("'kind' must be \"opaque\", \"unsigned\" or \"signed\""));
}

/* shared argument checking for the .Internal()s below */
static int checkWidth(SEXP swidth)
{
    /* as do_makevector() checks 'length': asInteger() would take the
       first element of a longer vector, so bytes(2, c(4L, 8L)) would
       quietly build a type the caller did not ask for */
    if (length(swidth) != 1)
	error(_("invalid '%s' argument"), "width");

    int width = asInteger(swidth);

    if (width == NA_INTEGER || width < 1 || width > BYTEVEC_MAX_WIDTH)
	error(_("'width' must be between 1 and %d"), BYTEVEC_MAX_WIDTH);

    return width;
}

/* Whether NA is representable is part of the type, so NA is not an
   answer to it: taking it as FALSE would hand back the more
   restrictive of the two vectors without being asked. */
static int checkNA(SEXP sna)
{
    if (length(sna) != 1)
	error(_("invalid '%s' argument"), "na");

    int hasNA = asLogical(sna);

    if (hasNA == NA_LOGICAL)
	error(_("'%s' must be TRUE or FALSE"), "na");

    return hasNA;
}

/* bytes(length, width, kind) */
attribute_hidden SEXP do_bytes(SEXP call, SEXP op, SEXP args, SEXP env)
{
    checkArity(op, args);

    /* asVecSize() rather than a cast: a double outside the R_xlen_t range
       is undefined behaviour to convert, and this is where do_makevector
       and the rest already get that check */
    if (length(CAR(args)) != 1)
	error(_("invalid '%s' argument"), "length");
    R_xlen_t len = asVecSize(CAR(args));
    if (len < 0)
	error(_("invalid '%s' argument"), "length");
    int width = checkWidth(CADR(args));
    int kind = checkKind(CADDR(args));
    int hasNA = checkNA(CADDDR(args));

    SEXP val = PROTECT(R_allocBytesVector(len, width, kind,
					  hasNA ? TRUE : FALSE));
    UNPROTECT(1);

    return val;
}

/* Elements that arrived from real data equal to the reserved NA
   pattern.  Only the opaque kind can reach this: the numeric parsers
   and eltFromLong() report that value as out of range instead, since
   for them it is a number the width cannot hold. */
static void warnReserved(R_xlen_t nNA)
{
    if (nNA)
	/* the selector is only singular-or-not, and R_xlen_t overflows
	   the unsigned long ngettext() takes on LLP64 -- so clamp
	   rather than truncate, which would pick the singular form for
	   a count of 2^32 + 1 */
	warning(ngettext("%lld element equal to the reserved NA value became NA",
			 "%lld elements equal to the reserved NA value became NA",
			 (nNA == 1) ? 1UL : 2UL), (long long) nNA);
}

/* The same warning for a payload taken verbatim rather than element by
   element -- as.bytes() on raw, and readBin().  Shared because both are
   the same event: bytes from outside R landed on the value this vector
   reserves, and the caller who chose the reservation should hear so. */
void R_bytesWarnReserved(SEXP val)
{
    if (!BYTEVEC_HAS_NA(val))
	return;			/* nothing is reserved, so nothing collided */

    int w = BYTEVEC_WIDTH(val), k = BYTEVEC_KIND(val);
    R_xlen_t nNA = 0;

    for (R_xlen_t i = 0; i < XLENGTH(val); i++)
	if (R_bytesEltIsNA(BYTEVEC_ELT_RO(val, i), w, k)) nNA++;

    warnReserved(nNA);
}

/* as.bytes(x, width, kind, na) on a character vector: the inverse of
   as.character(), hex for the opaque kind and decimal for the numeric
   ones.  This is the ingest route that does not depend on the
   producer's byte order, and the one a CSV or JSON column arrives by. */
static SEXP bytesFromString(SEXP x, int width, int kind, int hasNA)
{
    R_xlen_t n = XLENGTH(x);
    SEXP val = PROTECT(R_allocBytesVectorUninit(n, width, kind,
						hasNA ? TRUE : FALSE));
    R_xlen_t nBad = 0, nOver = 0, nReserved = 0;

    for (R_xlen_t i = 0; i < n; i++) {
	SEXP s = STRING_ELT(x, i);
	Rbyte *p = BYTEVEC_ELT(val, i);

	if (s == NA_STRING) {
	    R_bytesCheckNA(val);
	    R_bytesSetEltNA(p, width, kind);
	    continue;
	}

	R_bytes_parse_t st = R_bytesEltFromString(p, CHAR(s), width, kind,
						  hasNA != 0);
	if (st == BYTES_PARSE_OK) {
	    /* counted here rather than by scanning the result: an
	       element set to NA because it failed to parse is not a
	       datum that collided with the reserved value */
	    if (hasNA && R_bytesEltIsNA(p, width, kind)) nReserved++;
	    continue;
	}

	if (st == BYTES_PARSE_SYNTAX) nBad++; else nOver++;
	R_bytesCheckNA(val);
	R_bytesSetEltNA(p, width, kind);
    }

    /* the two failures get their own warnings, as they do for
       as.integer(): "abc" and "1e99" are different mistakes */
    if (nBad)
	warning(_("NAs introduced by coercion"));
    if (nOver)
	warning(_("NAs introduced by values outside the range of '%s'"),
		R_bytesTypeName(val));
    warnReserved(nReserved);

    UNPROTECT(1);

    return val;
}

/* Build a 'bytes' vector of this width and kind from x.  Behind
   as.bytes(), and behind as.vector(x, "int64") and its relatives,
   which name the width and kind rather than passing them.

   A raw vector is reinterpreted verbatim as width-byte elements -- for
   the numeric kinds that means the caller supplies native byte order,
   which is what makes ingest from an external source a plain memcpy.
   A character vector is parsed.  Integer and logical vectors narrow,
   as they do in arithmetic.  Double is refused there and is refused
   here for the same reason. */
SEXP R_bytesConvert(SEXP x, int width, int kind, int hasNA, SEXP call)
{
    if (TYPEOF(x) == BYTESXP) {
	if (BYTEVEC_WIDTH(x) == width && BYTEVEC_KIND(x) == kind &&
	    BYTEVEC_HAS_NA(x) == hasNA)
	    return x;

	return R_bytesFromBytes(x, width, kind, hasNA, call);
    }

    if (TYPEOF(x) == STRSXP)
	return bytesFromString(x, width, kind, hasNA);

    if (TYPEOF(x) == INTSXP || TYPEOF(x) == LGLSXP) {
	/* NA is the absence of a value rather than a value, so it is the
	   one thing an opaque element can take from a number vector --
	   as.bytes(NA, width, kind) is how a missing one is written */
	if (kind == BYTEVEC_OPAQUE && !R_bytesAllNA(x))
	    error(_("cannot convert type '%s' to an opaque '%s' vector: its elements are byte strings, not numbers"),
		  R_typeToChar(x), "bytes");

	return R_bytesNarrow(x, width, kind, hasNA, call);
    }

    /* Refused for the reason the whole coercion lattice refuses it: a
       double neither contains nor is contained by a 64-bit integer, so
       there is no conversion that is right in general.  as.character()
       is not the way out either -- a double has already lost the digits
       by the time it could be printed. */
    if (TYPEOF(x) == REALSXP)
	error(_("cannot convert a double vector to '%s'; supply an integer vector, or the text of each value for magnitudes a double cannot hold exactly"),
	      "bytes");

    if (TYPEOF(x) != RAWSXP)
	error(_("cannot convert type '%s' to '%s'; supply raw bytes, or the decimal or hex text of each element"),
	      R_typeToChar(x), "bytes");

    R_xlen_t nbytes = XLENGTH(x);
    if (nbytes % width)
	error(_("length of 'x' (%lld) is not a multiple of 'width' (%d)"),
	      (long long) nbytes, width);

    SEXP val = PROTECT(R_allocBytesVectorUninit(nbytes / width, width, kind,
						hasNA ? TRUE : FALSE));
    if (nbytes > 0)
	R_bytesMemcpy(BYTEVEC_DATA(val), RAW_RO(x), (size_t) nbytes);

    R_bytesWarnReserved(val);

    UNPROTECT(1);

    return val;
}

attribute_hidden SEXP do_asbytes(SEXP call, SEXP op, SEXP args, SEXP env)
{
    checkArity(op, args);

    SEXP val = PROTECT(R_bytesConvert(CAR(args), checkWidth(CADR(args)),
				      checkKind(CADDR(args)),
				      checkNA(CADDDR(args)), call));

    /* as.bytes() is a coercion and drops attributes, as as.numeric() and
       as.character() do.  R_bytesConvert() builds a fresh vector on every
       path but one -- when nothing would change it returns x -- so
       without this the same call would keep names or dim for exactly the
       inputs that needed no conversion.  This is what do_asvector() does
       for the same reason; storage.mode<- wants the attributes and so
       calls R_bytesConvert() directly. */
    if (ATTRIB(val) != R_NilValue) {
	if (MAYBE_REFERENCED(val)) val = duplicate(val);
	CLEAR_ATTRIB(val);
    }
    UNPROTECT(1);

    return val;
}

/* Numeric elements are stored in native byte order, but must go onto
   the wire in a fixed one or a file written on one platform would read
   as different values on another.  Most significant byte first is the
   choice, matching what R already does for integers and reals under
   XDR.  Opaque elements are byte strings and travel verbatim, so on a
   big-endian machine this is a plain copy in every case.

   The mapping is its own inverse, so one function serves both
   directions. */
void R_bytesSwapWire(Rbyte *dst, const Rbyte *src, R_xlen_t n, int w, int kind)
{
    if (kind == BYTEVEC_OPAQUE) {
	memcpy(dst, src, (size_t) n * w);
	return;
    }

    for (R_xlen_t e = 0; e < n; e++) {
	const Rbyte *se = src + e * w;
	Rbyte *de = dst + e * w;
	for (int i = 0; i < w; i++) de[i] = se[BYTEVEC_MSB(i, w)];
    }
}

/* is.bytes(x): typeof() now reports the derived name, so this has to
   ask about the storage type directly */
attribute_hidden SEXP do_bytesis(SEXP call, SEXP op, SEXP args, SEXP env)
{
    checkArity(op, args);

    return ScalarLogical(TYPEOF(CAR(args)) == BYTESXP);
}

/* Nothing to recycle from: the copies below stride the source by the
   element width, so a zero-length one would read w bytes past the end
   of its payload for every element written.  Rf_copyVector() and
   Rf_copyMatrix() are public API and make no such check for any type;
   this one says so rather than copying whatever follows. */
static void checkRecycleSource(R_xlen_t n, R_xlen_t nsrc, const char *fun)
{
    if (n > 0 && nsrc == 0)
	error(_("cannot recycle a zero-length 'bytes' vector in '%s'"), fun);
}

/* Block-copy analogues of xcopyRawWithRecycle and
   xfillRawMatrixWithRecycle: those assign elements, which cannot work
   when the element size is a per-vector property. */

/* n elements into dst at dstart, reading src cyclically from sidx, in
   contiguous runs rather than an element at a time -- c() of two large
   vectors is two block copies, not one memcpy per element.  Returns
   the next source position, so the matrix fill below can carry it
   across columns. */
static R_xlen_t copyRuns(SEXP dst, SEXP src, R_xlen_t dstart, R_xlen_t n,
			 R_xlen_t nsrc, R_xlen_t sidx)
{
    size_t w = (size_t) BYTEVEC_WIDTH(dst);
    Rbyte *d = BYTEVEC_DATA(dst) + (size_t) dstart * w;
    const Rbyte *s = BYTEVEC_DATA_RO(src);

    for (R_xlen_t done = 0; done < n; ) {
	R_xlen_t run = n - done;
	if (run > nsrc - sidx) run = nsrc - sidx;
	R_bytesMemcpy(d + (size_t) done * w, s + (size_t) sidx * w,
		      (size_t) run * w);
	done += run;
	sidx += run;
	if (sidx == nsrc) sidx = 0;
    }

    return sidx;
}

void R_bytesCopyWithRecycle(SEXP dst, SEXP src, R_xlen_t dstart,
			    R_xlen_t n, R_xlen_t nsrc)
{
    R_bytesCheckSameType(dst, src, "copyVector");
    checkRecycleSource(n, nsrc, "copyVector");

    copyRuns(dst, src, dstart, n, nsrc, 0);
}

void R_bytesFillMatrixWithRecycle(SEXP dst, SEXP src, R_xlen_t dstart,
				  R_xlen_t drows, R_xlen_t srows,
				  R_xlen_t cols, R_xlen_t nsrc)
{
    R_bytesCheckSameType(dst, src, "copyMatrix");
    /* srows, not drows: srows rows are written per column, so a source
       contributing no rows at all -- rbind() of a zero-row matrix --
       reads and writes nothing and needs no source */
    checkRecycleSource(srows * cols, nsrc, "copyMatrix");

    /* column by column, in FILL_MATRIX_ITERATE()'s source order:
       column j's rows read positions (j * srows + i) mod nsrc, a
       contiguous run that copyRuns() carries across columns */
    R_xlen_t sidx = 0;
    for (R_xlen_t j = 0; j < cols; j++)
	sidx = copyRuns(dst, src, dstart + j * drows, srows, nsrc, sidx);
}

const char *R_bytesKindName(SEXP x)
{
    switch (BYTEVEC_KIND(x)) {
    case BYTEVEC_UINT: return "unsigned";
    case BYTEVEC_INT:  return "signed";
    default:           return "opaque";
    }
}

/* The .Internal()s below all take one 'bytes' vector as 'x'.  Not
   checkBytes(), whose message names a C entry point: that is the right
   thing to tell a package and the wrong thing to tell someone who
   typed bytesWidth(1L). */
static SEXP checkBytesArg(SEXP args)
{
    SEXP x = CAR(args);

    if (TYPEOF(x) != BYTESXP)
	error(_("'%s' must be a 'bytes' vector"), "x");

    return x;
}

/* bytesHasNA(x) */
attribute_hidden SEXP do_byteshasna(SEXP call, SEXP op, SEXP args, SEXP env)
{
    checkArity(op, args);

    return ScalarLogical(BYTEVEC_HAS_NA(checkBytesArg(args)));
}

/* bytesKind(x) */
attribute_hidden SEXP do_byteskind(SEXP call, SEXP op, SEXP args, SEXP env)
{
    checkArity(op, args);

    return mkString(R_bytesKindName(checkBytesArg(args)));
}

/* bytesRaw(x): the flat payload, for round-tripping and for handing
   the bytes to code that wants an ordinary raw vector */
attribute_hidden SEXP do_bytesraw(SEXP call, SEXP op, SEXP args, SEXP env)
{
    checkArity(op, args);

    SEXP x = checkBytesArg(args);

    R_xlen_t nbytes = XLENGTH(x) * BYTEVEC_WIDTH(x);
    SEXP val = PROTECT(allocVector(RAWSXP, nbytes));
    if (nbytes > 0)
	R_bytesMemcpy(RAW(val), BYTEVEC_DATA_RO(x), (size_t) nbytes);
    UNPROTECT(1);

    return val;
}

/* How one element is shown, in every context: hex for OPAQUE, decimal
   for the numeric kinds.  Returns a pointer into a static buffer, so
   the result must be used before the next call. */
const char *R_bytesEltRender(SEXP x, R_xlen_t i)
{
    int w = BYTEVEC_WIDTH(x), k = BYTEVEC_KIND(x);
    const Rbyte *p = BYTEVEC_ELT_RO(x, i);

    if (BYTEVEC_HAS_NA(x) && R_bytesEltIsNA(p, w, k))
	return CHAR(R_print.na_string);

    return k == BYTEVEC_OPAQUE ? EncodeBytes(p, w) : R_bytesEltDecimal(p, w, k);
}

/* The R-level type name is derived from (kind, width) rather than from
   the SEXPTYPE, so that a width-8 unsigned vector reports "uint64"
   instead of "bytes".  R already does this for OBJSXP, which reports
   "S4" or "object" depending on a gp bit (R_typeToChar in util.c).

   Deriving it is what lets package code dispatch on what it is
   actually holding -- switch(typeof(x), uint64 = ...) -- without the
   type number lying to C code, which is the whole reason this is a
   separate SEXPTYPE. */
/* A small ring of buffers rather than one.  R_typeToChar() reports a
   'bytes' vector by this name, and several of R's messages print two
   type names in one call -- "incompatible types (from %s to %s)" and
   vapply's mismatch report among them.  With a single buffer the
   second call would overwrite the first and both would print the same
   name, silently and only for this type. */
const char *R_bytesTypeNameOf(int w, int kind)
{
    static char buff[4][16];
    static int next = 0;
    char *b = buff[next];

    next = (next + 1) & 3;

    switch (kind) {
    case BYTEVEC_UINT: snprintf(b, sizeof buff[0], "uint%d", 8 * w);  break;
    case BYTEVEC_INT:  snprintf(b, sizeof buff[0], "int%d", 8 * w);   break;
    default:           snprintf(b, sizeof buff[0], "bytes%d", w);     break;
    }

    return b;
}

const char *R_bytesTypeName(SEXP x)
{
    return R_bytesTypeNameOf(BYTEVEC_WIDTH(x), BYTEVEC_KIND(x));
}

/* The inverse, for readBin(con, "int64") and friends.  Kept beside the
   function above so the two spellings of the same rule cannot drift.
   Note that the numeric names count bits and the opaque one counts
   bytes, following how each is usually written. */
Rboolean R_bytesTypeFromName(const char *s, int *width, int *kind)
{
    const char *digits;
    int k;

    if (!strncmp(s, "uint", 4))	     { k = BYTEVEC_UINT;   digits = s + 4; }
    else if (!strncmp(s, "int", 3))  { k = BYTEVEC_INT;    digits = s + 3; }
    else if (!strncmp(s, "bytes", 5)) { k = BYTEVEC_OPAQUE; digits = s + 5; }
    else return FALSE;

    /* so that "int" and "integer" stay readBin's own names.  A leading
       zero is rejected so that the names accepted here are exactly the
       ones R_bytesTypeName() produces, rather than "int064" as well. */
    if (!*digits || (*digits == '0' && digits[1])) return FALSE;
    for (const char *p = digits; *p; p++)
	if (!isdigit((int) (unsigned char) *p)) return FALSE;

    long n = strtol(digits, NULL, 10);
    if (k != BYTEVEC_OPAQUE) {
	if (n % 8) return FALSE;
	n /= 8;
    }
    if (n < 1 || n > BYTEVEC_MAX_WIDTH) return FALSE;

    *width = (int) n;
    *kind = k;

    return TRUE;
}

/* bytesWidth(x) */
attribute_hidden SEXP do_byteswidth(SEXP call, SEXP op, SEXP args, SEXP env)
{
    checkArity(op, args);

    return ScalarInteger(BYTEVEC_WIDTH(checkBytesArg(args)));
}
