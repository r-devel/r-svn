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

/* Allocated as a RAWSXP of the full byte size so that the standard
   vector allocator does the size-class selection and heap accounting,
   then retyped and given its true element count.  getVecSizeInVEC()
   multiplies the width back out, so the GC sees the same byte size
   both here and at collection time. */
/* Convenience form for the default kind.  Internal code that derives a
   new vector from an existing one must NOT use this -- it silently
   drops the kind.  Use R_allocVectorLike(), which carries both width
   and kind. */
SEXP R_allocBytesVector(R_xlen_t length, int width)
{
    return R_allocBytesVectorKind(length, width, BYTEVEC_OPAQUE);
}

SEXP R_allocBytesVectorKind(R_xlen_t length, int width, int kind)
{
    if (width < 1 || width > BYTEVEC_MAX_WIDTH)
	error(_("'width' must be between 1 and %d"), BYTEVEC_MAX_WIDTH);
    if (length < 0)
	error(_("negative length vectors are not allowed"));
    if (length > R_XLEN_T_MAX / width)
	error(_("cannot allocate vector of length %lld"), (long long) length);

    SEXP val = PROTECT(allocVector(RAWSXP, length * width));
    SET_TYPEOF(val, BYTESXP);
    SET_BYTEVEC_WIDTH(val, width);
    SET_BYTEVEC_KIND(val, kind);
    SET_STDVEC_LENGTH(val, length);

    /* zero-filled: these bytes are user-visible values, and leaving
       them undefined would make results depend on heap history */
    if (length > 0)
	memset(BYTEVEC_DATA(val), 0, (size_t) length * width);

    UNPROTECT(1);

    return val;
}

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

/* allocVector(TYPEOF(s), n) cannot reproduce a per-vector width, so the
   generic "another vector like this one" sites go through here */
SEXP R_allocVectorLike(SEXP s, R_xlen_t length)
{
    if (TYPEOF(s) == BYTESXP)
	return R_allocBytesVectorKind(length, BYTEVEC_WIDTH(s),
				      BYTEVEC_KIND(s));

    return allocVector(TYPEOF(s), length);
}

attribute_hidden int R_bytesWidth(SEXP x)
{
    if (TYPEOF(x) != BYTESXP)
	error(_("'%s' must be a 'bytes' vector"), "x");

    return BYTEVEC_WIDTH(x);
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

/* bytes(length, width, kind) */
attribute_hidden SEXP do_bytes(SEXP call, SEXP op, SEXP args, SEXP env)
{
    checkArity(op, args);

    double len = asReal(CAR(args));
    if (!R_FINITE(len) || len < 0)
	error(_("invalid '%s' argument"), "length");
    int width = checkWidth(CADR(args));
    int kind = checkKind(CADDR(args));

    return R_allocBytesVectorKind((R_xlen_t) len, width, kind);
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

    R_xlen_t nbytes = XLENGTH(x);
    if (nbytes % width)
	error(_("length of 'x' (%lld) is not a multiple of 'width' (%d)"),
	      (long long) nbytes, width);

    SEXP val = PROTECT(R_allocBytesVectorKind(nbytes / width, width, kind));
    if (nbytes > 0)
	memcpy(BYTEVEC_DATA(val), RAW_RO(x), (size_t) nbytes);

    R_xlen_t nNA = 0;
    for (R_xlen_t i = 0; i < XLENGTH(val); i++)
	if (R_bytesEltIsNA(BYTEVEC_ELT_RO(val, i), width, kind)) nNA++;
    if (nNA)
	warning(ngettext("%lld element equal to the reserved NA value became NA",
			 "%lld elements equal to the reserved NA value became NA",
			 (unsigned long) nNA), (long long) nNA);

    UNPROTECT(1);

    return val;
}

/* bytesNA(length, width, kind) */
attribute_hidden SEXP do_bytesna(SEXP call, SEXP op, SEXP args, SEXP env)
{
    checkArity(op, args);

    double len = asReal(CAR(args));
    if (!R_FINITE(len) || len < 0)
	error(_("invalid '%s' argument"), "length");
    int width = checkWidth(CADR(args));
    int kind = checkKind(CADDR(args));

    SEXP val = PROTECT(R_allocBytesVectorKind((R_xlen_t) len, width, kind));
    for (R_xlen_t i = 0; i < XLENGTH(val); i++)
	R_bytesSetEltNA(BYTEVEC_ELT(val, i), width, kind);
    UNPROTECT(1);

    return val;
}

/* bytesKind(x) */
attribute_hidden SEXP do_byteskind(SEXP call, SEXP op, SEXP args, SEXP env)
{
    checkArity(op, args);

    SEXP x = CAR(args);
    if (TYPEOF(x) != BYTESXP)
	error(_("'%s' must be a 'bytes' vector"), "x");

    switch (BYTEVEC_KIND(x)) {
    case BYTEVEC_UINT: return mkString("unsigned");
    case BYTEVEC_INT:  return mkString("signed");
    default:           return mkString("opaque");
    }
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

    if (R_bytesEltIsNA(p, w, k))
	return CHAR(R_print.na_string);

    return k == BYTEVEC_OPAQUE ? EncodeBytes(p, w) : R_bytesEltDecimal(p, w, k);
}

/* bytesWidth(x) */
attribute_hidden SEXP do_byteswidth(SEXP call, SEXP op, SEXP args, SEXP env)
{
    checkArity(op, args);

    return ScalarInteger(R_bytesWidth(CAR(args)));
}
