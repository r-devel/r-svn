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

#include <Defn.h>
#include <Internal.h>
#include <Print.h>  /* R_print.na_string */
#include "duplicate.h"  /* FILL_MATRIX_ITERATE */

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
	/* two's complement negate, most significant byte first */
	negative = true;
	int carry = 1;
	for (int i = width - 1; i >= 0; i--) {
	    int v = (int) ((Rbyte) ~tmp[i]) + carry;
	    tmp[i] = (Rbyte) (v & 0xFF);
	    carry = v >> 8;
	}
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

/* allocVector(TYPEOF(s), n) cannot reproduce a per-vector width, so the
   generic "another vector like this one" sites go through here */
SEXP R_allocVectorLike(SEXP s, R_xlen_t length)
{
    if (TYPEOF(s) == BYTESXP)
	return R_allocBytesVector(length, BYTEVEC_WIDTH(s), BYTEVEC_KIND(s),
				  BYTEVEC_HAS_NA(s) ? TRUE : FALSE);

    return allocVector(TYPEOF(s), length);
}

/* The rest of this section is the package-facing API declared in
   Rinternals.h.  R's own code uses the BYTEVEC_* accessors directly;
   these check the type first, as INTEGER() and RAW() do, because a
   package reaching for the wrong one should hear about it rather than
   read the payload of something else. */

static void checkBytes(SEXP x, const char *what)
{
    if (TYPEOF(x) != BYTESXP)
	error("%s() can only be applied to a '%s', not a '%s'",
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
    const char *k = CHAR(asChar(skind));

    if (!strcmp(k, "opaque"))   return BYTEVEC_OPAQUE;
    if (!strcmp(k, "unsigned")) return BYTEVEC_UINT;
    if (!strcmp(k, "signed"))   return BYTEVEC_INT;

    error(_("'kind' must be \"opaque\", \"unsigned\" or \"signed\""));
}

/* shared argument checking for the .Internal()s below */
static int checkWidth(SEXP swidth)
{
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

/* as.bytes(x, width, kind): reinterpret a raw vector as width-byte
   elements.  The bytes are taken verbatim -- for the numeric kinds
   that means the caller supplies them in native byte order, which is
   what makes ingest from an external source a plain memcpy. */
attribute_hidden SEXP do_asbytes(SEXP call, SEXP op, SEXP args, SEXP env)
{
    checkArity(op, args);

    SEXP x = CAR(args);
    if (TYPEOF(x) != RAWSXP)
	error(_("'%s' must be a raw vector"), "x");
    int width = checkWidth(CADR(args));
    int kind = checkKind(CADDR(args));
    int hasNA = checkNA(CADDDR(args));

    R_xlen_t nbytes = XLENGTH(x);
    if (nbytes % width)
	error(_("length of 'x' (%lld) is not a multiple of 'width' (%d)"),
	      (long long) nbytes, width);

    SEXP val = PROTECT(R_allocBytesVector(nbytes / width, width, kind,
					  hasNA ? TRUE : FALSE));
    if (nbytes > 0)
	memcpy(BYTEVEC_DATA(val), RAW_RO(x), (size_t) nbytes);

    /* with na = FALSE nothing is reserved, so nothing can collide */
    if (hasNA) {
	R_xlen_t nNA = 0;
	for (R_xlen_t i = 0; i < XLENGTH(val); i++)
	    if (R_bytesEltIsNA(BYTEVEC_ELT_RO(val, i), width, kind)) nNA++;
	if (nNA)
	    warning(ngettext("%lld element equal to the reserved NA value became NA",
			     "%lld elements equal to the reserved NA value became NA",
			     (unsigned long) nNA), (long long) nNA);
    }

    UNPROTECT(1);

    return val;
}

/* bytesNA(length, width, kind) */
attribute_hidden SEXP do_bytesna(SEXP call, SEXP op, SEXP args, SEXP env)
{
    checkArity(op, args);

    R_xlen_t len = asVecSize(CAR(args));	/* see do_bytes */
    if (len < 0)
	error(_("invalid '%s' argument"), "length");
    int width = checkWidth(CADR(args));
    int kind = checkKind(CADDR(args));

    SEXP val = PROTECT(R_allocBytesVector(len, width, kind, TRUE));
    for (R_xlen_t i = 0; i < XLENGTH(val); i++)
	R_bytesSetEltNA(BYTEVEC_ELT(val, i), width, kind);
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

/* Block-copy analogues of xcopyRawWithRecycle and
   xfillRawMatrixWithRecycle: those assign elements, which cannot work
   when the element size is a per-vector property. */
void R_bytesCopyWithRecycle(SEXP dst, SEXP src, R_xlen_t dstart,
			    R_xlen_t n, R_xlen_t nsrc)
{
    size_t w = (size_t) BYTEVEC_WIDTH(dst);
    Rbyte *d = BYTEVEC_DATA(dst);
    const Rbyte *s = BYTEVEC_DATA_RO(src);

    for (R_xlen_t i = 0, sidx = 0; i < n; i++, sidx++) {
	if (sidx == nsrc) sidx = 0;
	memcpy(d + (dstart + i) * w, s + sidx * w, w);
    }
}

void R_bytesFillMatrixWithRecycle(SEXP dst, SEXP src, R_xlen_t dstart,
				  R_xlen_t drows, R_xlen_t srows,
				  R_xlen_t cols, R_xlen_t nsrc)
{
    size_t w = (size_t) BYTEVEC_WIDTH(dst);
    Rbyte *d = BYTEVEC_DATA(dst);
    const Rbyte *s = BYTEVEC_DATA_RO(src);

    FILL_MATRIX_ITERATE(dstart, drows, srows, cols, nsrc)
	memcpy(d + didx * w, s + sidx * w, w);
}

const char *R_bytesKindName(SEXP x)
{
    switch (BYTEVEC_KIND(x)) {
    case BYTEVEC_UINT: return "unsigned";
    case BYTEVEC_INT:  return "signed";
    default:           return "opaque";
    }
}

/* bytesHasNA(x) */
attribute_hidden SEXP do_byteshasna(SEXP call, SEXP op, SEXP args, SEXP env)
{
    checkArity(op, args);

    SEXP x = CAR(args);
    if (TYPEOF(x) != BYTESXP)
	error(_("'%s' must be a 'bytes' vector"), "x");

    return ScalarLogical(BYTEVEC_HAS_NA(x));
}

/* bytesKind(x) */
attribute_hidden SEXP do_byteskind(SEXP call, SEXP op, SEXP args, SEXP env)
{
    checkArity(op, args);

    SEXP x = CAR(args);
    if (TYPEOF(x) != BYTESXP)
	error(_("'%s' must be a 'bytes' vector"), "x");

    return mkString(R_bytesKindName(x));
}

/* bytesRaw(x): the flat payload, for round-tripping and for handing
   the bytes to code that wants an ordinary raw vector */
attribute_hidden SEXP do_bytesraw(SEXP call, SEXP op, SEXP args, SEXP env)
{
    checkArity(op, args);

    SEXP x = CAR(args);
    if (TYPEOF(x) != BYTESXP)
	error(_("'%s' must be a 'bytes' vector"), "x");

    R_xlen_t nbytes = XLENGTH(x) * BYTEVEC_WIDTH(x);
    SEXP val = PROTECT(allocVector(RAWSXP, nbytes));
    if (nbytes > 0)
	memcpy(RAW(val), BYTEVEC_DATA_RO(x), (size_t) nbytes);
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
const char *R_bytesTypeName(SEXP x)
{
    static char buff[16];
    int w = BYTEVEC_WIDTH(x);

    switch (BYTEVEC_KIND(x)) {
    case BYTEVEC_UINT: snprintf(buff, sizeof buff, "uint%d", 8 * w);  break;
    case BYTEVEC_INT:  snprintf(buff, sizeof buff, "int%d", 8 * w);   break;
    default:           snprintf(buff, sizeof buff, "bytes%d", w);     break;
    }

    return buff;
}

/* bytesWidth(x) */
attribute_hidden SEXP do_byteswidth(SEXP call, SEXP op, SEXP args, SEXP env)
{
    checkArity(op, args);

    return ScalarInteger(R_bytesWidth(CAR(args)));
}
