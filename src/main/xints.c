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
 *  The base int64 and uint64 classes implemented as ALTSXP objects.
 *
 *  An 'xinteger' vector holds n elements of w bytes each, where w is a
 *  per-vector property recorded in the sxpinfo gp field rather than
 *  implied by the SEXPTYPE.  XLENGTH() counts elements, not bytes:
 *  that is the whole reason this is a distinct type rather than a
 *  flavour of RAWSXP, whose length is unavoidably a byte count.
 *
 *  Elements are signed or unsigned integers.  They use value order and
 *  support exact fixed-width arithmetic where implemented.  Defn.h
 *  records the representation invariants shared by those operations.
 */

#ifdef HAVE_CONFIG_H
# include <config.h>
#endif

#include <ctype.h>
#include <float.h>	/* DBL_MAX_10_EXP */
#include <math.h>	/* floor */

#include <Defn.h>
#include <Internal.h>
#include <Print.h>  /* R_print.na_string */
#include <R_ext/Itermacros.h>  /* MOD_ITERATE2 */

/* XINT_UNSIGNED reserves all-0xFF for NA; XINT_SIGNED reserves INT_MIN. */
Rboolean R_xintEltIsNA(const Rbyte *p, int width, int kind)
{
    if (kind == XINT_SIGNED) {
	if (p[XINT_MSB(0, width)] != 0x80) return FALSE;
	for (int i = 1; i < width; i++)
	    if (p[XINT_MSB(i, width)] != 0x00) return FALSE;
	return TRUE;
    }

    for (int i = 0; i < width; i++)
	if (p[i] != XINT_NA_BYTE) return FALSE;

    return TRUE;
}

void R_xintSetEltNA(Rbyte *p, int width, int kind)
{
    if (kind == XINT_SIGNED) {
	memset(p, 0x00, (size_t) width);
	p[XINT_MSB(0, width)] = 0x80;
	return;
    }

    memset(p, XINT_NA_BYTE, (size_t) width);
}

/* Order, ignoring NA (callers handle that).  Compare from the most
   significant byte down, with the top byte read as signed for INT.
   Working a byte at a time also keeps the kernel independent of native
   integer alignment. */
int R_xintEltCmp(const Rbyte *a, const Rbyte *b, int width, int kind)
{
    int top = XINT_MSB(0, width);
    if (kind == XINT_SIGNED) {
	signed char sa = (signed char) a[top], sb = (signed char) b[top];
	if (sa != sb) return sa < sb ? -1 : 1;
    }
    else if (a[top] != b[top])
	return a[top] < b[top] ? -1 : 1;

    for (int i = 1; i < width; i++) {
	Rbyte x = a[XINT_MSB(i, width)], y = b[XINT_MSB(i, width)];
	if (x != y) return x < y ? -1 : 1;
    }

    return 0;
}

/* Decimal rendering, any width, by repeated division by 10 on a
   scratch copy held most-significant-byte first.  Returns a pointer
   into a static buffer, like the other Encode* functions.  A width-16
   value needs at most 39 digits plus a sign. */
const char *R_xintEltDecimal(const Rbyte *p, int width, int kind)
{
    static char buff[8 * XINT_MAX_WIDTH / 3 + 4];
    Rbyte tmp[XINT_MAX_WIDTH];
    bool negative = false;

    for (int i = 0; i < width; i++)
	tmp[i] = p[XINT_MSB(i, width)];

    if (kind == XINT_SIGNED && (tmp[0] & 0x80)) {
	negative = true;
	R_xintMagNegate(tmp, width);
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
   also what makes as.character() reversible, which deparse() relies on. */

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

static R_xint_parse_t parseDecimal(Rbyte *out, const char *s, int w,
				    int kind, bool hasNA)
{
    bool negative = false;

    while (isspace((int) (unsigned char) *s)) s++;
    if (*s == '+' || *s == '-')
	negative = (*s++ == '-');
    if (!isdigit((int) (unsigned char) *s))
	return XINT_PARSE_SYNTAX;

    Rbyte mag[XINT_MAX_WIDTH];
    memset(mag, 0, (size_t) w);

    bool over = false;
    for (; isdigit((int) (unsigned char) *s); s++)
	if (!magMulAdd(mag, w, 10u, (unsigned int) (*s - '0')))
	    over = true;

    while (isspace((int) (unsigned char) *s)) s++;
    if (*s)
	return XINT_PARSE_SYNTAX;

    /* an unsigned element has no negative values, but "-0" is 0 */
    if (negative && kind == XINT_UNSIGNED) {
	for (int i = 0; i < w; i++)
	    if (mag[i]) return XINT_PARSE_RANGE;
	negative = false;
    }

    if (over || !R_xintMagFits(mag, w, kind, negative, hasNA))
	return XINT_PARSE_RANGE;

    if (negative)
	R_xintMagNegate(mag, w);

    for (int i = 0; i < w; i++)
	out[XINT_MSB(i, w)] = mag[i];

    return XINT_PARSE_OK;
}

R_xint_parse_t R_xintEltFromString(Rbyte *out, const char *s, int w,
				     int kind, bool hasNA)
{
    return parseDecimal(out, s, w, kind, hasNA);
}

/* ---- bitwise operations --------------------------------------------

   These operations are per byte, so unlike arithmetic they carry no
   width restriction at all.

   NA propagates, as it does for integers.  A result landing exactly on
   the reserved NA value is reported rather than returned quietly,
   which is the rule arithmetic already follows for overflow. */

/* logical shift by n bits, either direction, over the whole element */
static void eltShift(Rbyte *out, const Rbyte *x, int w, int n,
		     bool left)
{
    Rbyte tmp[XINT_MAX_WIDTH], res[XINT_MAX_WIDTH];
    int bs = n / 8, br = n % 8;

    for (int i = 0; i < w; i++)
	tmp[i] = x[XINT_MSB(i, w)];

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
	out[XINT_MSB(i, w)] = res[i];
}

attribute_hidden
SEXP R_xintBitwise(SEXP call, int oper, SEXP a, SEXP b)
{
    bool unary = (oper == 2), shift = (oper == 5 || oper == 6);

    /* and, or and xor are commutative, so either operand may be the
       'xinteger' one; a shift count never is */
    if (!unary && !shift && !R_isXInt(a)) {
	SEXP t = a; a = b; b = t;
    }
    if (shift && R_isXInt(b))
	errorcall(call, _("invalid '%s' argument"), "b");
    if (!R_isXInt(a))
	errorcall(call, _("'a' and 'b' must have the same type"));

    int w = XINT_WIDTH(a), k = XINT_KIND(a);
    int hasNA = XINT_HAS_NA(a);

    if (shift) {
	if (!isInteger(b)) b = coerceVector(b, INTSXP);
    }
    else if (!unary) {
	if (!R_isXInt(b))
	    b = R_xintNarrow(b, w, k, hasNA, call);
	else
	    /* widths are not promoted the way arithmetic promotes them:
	       a mask that is not the width of what it masks is a
	       mistake, not a value to extend */
	    R_xintCheckPair(call, a, b, "combine");
    }
    PROTECT(a);
    PROTECT(b);		/* may be the narrowed or coerced temporary */

    R_xlen_t m = XLENGTH(a), n = unary ? m : XLENGTH(b);
    R_xlen_t mn = (m && n) ? (m > n ? m : n) : 0;
    SEXP ans = PROTECT(R_allocXIntVector(mn, w, k, hasNA ? TRUE : FALSE));
    R_xlen_t i, ia, ib, nOver = 0;

    MOD_ITERATE2(mn, m, n, i, ia, ib, {
	const Rbyte *x = XINT_ELT_RO(a, ia);
	Rbyte *o = XINT_ELT(ans, i);
	bool na = hasNA && R_xintEltIsNA(x, w, k);

	if (shift) {
	    int s = INTEGER_ELT(b, ib);
	    if (na || s == NA_INTEGER || s < 0 || s > 8 * w - 1) {
		/* bitwShiftL(1L, 32L) is NA, and so is this wherever NA
		   exists.  Where it does not, say what actually went
		   wrong: R_xintCheckNA()'s message is about the data,
		   and here the shift count is the problem. */
		if (!hasNA)
		    errorcall(call,
			      _("shift out of range for a '%s' vector, which has no NA to return"),
			      R_xintTypeName(a));
		R_xintSetEltNA(o, w, k);
		continue;
	    }
	    eltShift(o, x, w, s, oper == 5);
	}
	else if (unary) {
	    if (na) {
		R_xintSetEltNA(o, w, k);
		continue;
	    }
	    for (int j = 0; j < w; j++) o[j] = (Rbyte) ~x[j];
	}
	else {
	    const Rbyte *y = XINT_ELT_RO(b, ib);
	    if (na || (hasNA && R_xintEltIsNA(y, w, k))) {
		R_xintSetEltNA(o, w, k);
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
	if (hasNA && R_xintEltIsNA(o, w, k)) {
	    nOver++;
	    R_xintSetEltNA(o, w, k);
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
attribute_hidden void R_xintMemcpy(Rbyte *dst, const Rbyte *src, size_t n)
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
void R_xintCheckNA(SEXP x)
{
    if (!XINT_HAS_NA(x))
	error(_("missing values are not representable in this '%s' vector; it was created with na = FALSE"),
	      R_xintTypeName(x));
}

/* What CHKVEC() is to DATAPTR(): only reached in the builds that ask for
   the check, which is why XINT_DATA() can afford to call it. */
void R_CheckXIntVector(SEXP x)
{
    if (!R_is_altxint(x))
	error("cannot get data pointer of '%s' objects", R_typeToChar(x));
}

/* Whether a value is reserved is part of the type, as the kind and the
   width are: combining vectors that disagree would either lose a real
   value or invent a missing one. */
void R_xintCheckSameNA(SEXP x, SEXP y)
{
    if (R_isXInt(x) && R_isXInt(y) &&
	XINT_HAS_NA(x) != XINT_HAS_NA(y))
	error(_("cannot combine 'xinteger' vectors that differ in whether NA is representable"));
}

/* The whole of that rule for a pair of 'xinteger' operands: width, kind
   and NA reservation are all part of the type, and every pairwise
   operation -- c() and arithmetic combine, == compares, match()
   matches, [<- assigns between -- refuses a pair that disagrees.  One
   checker so the wording and the order of the checks cannot drift
   between them; only the verb differs.  Pass R_CurrentExpression as
   the call where none is at hand. */
void R_xintCheckPair(SEXP call, SEXP x, SEXP y, const char *verb)
{
    if (XINT_WIDTH(x) != XINT_WIDTH(y))
	errorcall(call, _("cannot %s 'xinteger' vectors of widths %d and %d"),
		  verb, XINT_WIDTH(x), XINT_WIDTH(y));
    if (XINT_KIND(x) != XINT_KIND(y))
	errorcall(call, _("cannot %s 'xinteger' vectors of different kinds"),
		  verb);

    R_xintCheckSameNA(x, y);
}

/* Rf_copyVector() and Rf_copyMatrix() are public API, and the only check
   they make is that the two SEXPTYPEs agree -- which a width-4 and a
   width-16 'xinteger' vector both pass.  Their block copies stride one vector
   by the other's width, so a mismatch reads past the end of the source.
   Callers inside R pair R_allocVectorLike() with the vector being copied
   and so always agree; this is what stops a package from not. */
void R_xintCheckSameType(SEXP x, SEXP y, const char *fun)
{
    if (XINT_WIDTH(x) != XINT_WIDTH(y) ||
	XINT_KIND(x) != XINT_KIND(y))
	error(_("'xinteger' vectors differ in width or kind in '%s'"), fun);

    /* the reservation is part of the type as much as those two are:
       copying from a vector that reserves nothing into one that does
       would read a legitimate all-0xFF datum back as missing */
    R_xintCheckSameNA(x, y);
}

/* allocVector(TYPEOF(s), n) cannot reproduce a per-vector width, so the
   generic "another vector like this one" sites go through here */
SEXP R_allocVectorLike(SEXP s, R_xlen_t length)
{
    if (R_isXInt(s))
	return R_allocXIntVector(length, XINT_WIDTH(s), XINT_KIND(s),
				  XINT_HAS_NA(s) ? TRUE : FALSE);

    return allocVector(TYPEOF(s), length);
}

/* The 'xinteger' half of allocMatrix(): that allocator cannot carry a
   per-vector width either, so the matrix sites build the vector first
   and shape it here -- one place for the dim dance.  Returns x. */
SEXP R_xintShapeMatrix(SEXP x, int nrow, int ncol)
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
    if (!R_isXInt(s))
	return allocMatrix(TYPEOF(s), nrow, ncol);

    if ((double) nrow * ncol > R_XLEN_T_MAX)
	error(_("too many elements specified"));

    return R_xintShapeMatrix(R_allocVectorLike(s, (R_xlen_t) nrow * ncol),
			      nrow, ncol);
}

/* The rest of this section is the package-facing API declared in
   Rinternals.h.  R's own code uses the XINT_* accessors directly;
   these check the type first, as INTEGER() and RAW() do, because a
   package reaching for the wrong one should hear about it rather than
   read the payload of something else. */

static void checkXInt(SEXP x, const char *what)
{
    if (!R_is_altxint(x))
	error(_("%s() can only be applied to a '%s', not a '%s'"),
	      what, "xinteger", R_typeToChar(x));
}

Rboolean R_isXInt(SEXP x)
{
    return R_is_altxint(x);
}

/* Whether R_allocXIntVector() would accept this element type.  A reader
   mapping a source column onto an R type has to decide before it
   allocates, because the allocator's refusal is an R error: that is a
   longjmp out of whatever the caller was in the middle of, which in a
   C++ column reader means skipped destructors.  Asking first lets it
   fall back to a double or a character column instead. */
Rboolean R_xintTypeSupported(int width, int kind)
{
    return (Rboolean) (XINT_WIDTH_OK(width) &&
		       (kind == XINT_UNSIGNED || kind == XINT_SIGNED));
}

/* One wording for a width outside the set, so that the allocator and
   the .Internal argument checks cannot drift apart.  The unserializer
   keeps its own message: a width off a stream is a corrupt file rather
   than a mistyped argument, and says so. */
NORET attribute_hidden void R_xintWidthError(int w)
{
    error(_("'width' must be 8 bytes, not %d"), w);
}

int R_xintWidth(SEXP x)
{
    checkXInt(x, "R_xintWidth");

    return XINT_WIDTH(x);
}

int R_xintKind(SEXP x)
{
    checkXInt(x, "R_xintKind");

    return XINT_KIND(x);
}

Rboolean R_xintHasNA(SEXP x)
{
    checkXInt(x, "R_xintHasNA");

    return XINT_HAS_NA(x) ? TRUE : FALSE;
}

/* Element addresses.  Preferred over XINTEGER() + i * width in package
   code for the same reason R's own code uses XINT_ELT: it is the
   one place the width has to be got right. */
Rbyte *R_xintElt(SEXP x, R_xlen_t i)
{
    checkXInt(x, "R_xintElt");
    if (i < 0 || i >= XLENGTH(x))
	error(_("subscript out of bounds"));

    return XINT_ELT(x, i);
}

const Rbyte *R_xintEltRO(SEXP x, R_xlen_t i)
{
    checkXInt(x, "R_xintEltRO");
    if (i < 0 || i >= XLENGTH(x))
	error(_("subscript out of bounds"));

    return XINT_ELT_RO(x, i);
}

Rboolean R_xintIsNA(SEXP x, R_xlen_t i)
{
    checkXInt(x, "R_xintIsNA");
    if (i < 0 || i >= XLENGTH(x))
	error(_("subscript out of bounds"));

    if (!XINT_HAS_NA(x))
	return FALSE;

    return R_xintEltIsNA(XINT_ELT_RO(x, i), XINT_WIDTH(x),
			  XINT_KIND(x));
}

void R_xintSetNA(SEXP x, R_xlen_t i)
{
    checkXInt(x, "R_xintSetNA");
    if (i < 0 || i >= XLENGTH(x))
	error(_("subscript out of bounds"));

    R_xintCheckNA(x);	/* errors if this vector reserves no NA value */

    R_xintSetEltNA(XINT_ELT(x, i), XINT_WIDTH(x), XINT_KIND(x));
}

static int checkKind(SEXP skind)
{
    if (length(skind) != 1)
	error(_("invalid '%s' argument"), "kind");

    const char *k = CHAR(asChar(skind));

    if (!strcmp(k, "unsigned")) return XINT_UNSIGNED;
    if (!strcmp(k, "signed"))   return XINT_SIGNED;

    error(_("'kind' must be \"unsigned\" or \"signed\""));
}

/* shared argument checking for the .Internal()s below */
static int checkWidth(SEXP swidth)
{
    /* as do_makevector() checks 'length': asInteger() would take the
       first element of a longer vector, so xinteger(2, c(4L, 8L)) would
       quietly build a type the caller did not ask for */
    if (length(swidth) != 1)
	error(_("invalid '%s' argument"), "width");

    int width = asInteger(swidth);

    if (width == NA_INTEGER)
	error(_("invalid '%s' argument"), "width");
    if (!XINT_WIDTH_OK(width))
	R_xintWidthError(width);

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

/* xinteger(length, width, kind) */
attribute_hidden SEXP do_xinteger(SEXP call, SEXP op, SEXP args, SEXP env)
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

    SEXP val = PROTECT(R_allocXIntVector(len, width, kind,
					  hasNA ? TRUE : FALSE));
    /* R-level atomic constructors promise a zero-filled value. */
    if (len > 0)
	memset(XINT_DATA(val), 0, (size_t) len * width);
    UNPROTECT(1);

    return val;
}

/* Elements that arrived from raw data equal to the reserved NA pattern. */
void R_xintWarnReservedCount(R_xlen_t nNA)
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
   element -- as.xinteger() on raw, and readBin().  Shared because both are
   the same event: bytes from outside R landed on the value this vector
   reserves, and the caller who chose the reservation should hear so. */
void R_xintWarnReserved(SEXP val)
{
    if (!XINT_HAS_NA(val))
	return;			/* nothing is reserved, so nothing collided */

    int w = XINT_WIDTH(val), k = XINT_KIND(val);
    R_xlen_t nNA = 0;

    for (R_xlen_t i = 0; i < XLENGTH(val); i++)
	if (R_xintEltIsNA(XINT_ELT_RO(val, i), w, k)) nNA++;

    R_xintWarnReservedCount(nNA);
}

/* as.xinteger(x, width, kind, na) on a character vector: the inverse of
   as.character().  This is the ingest route that does not depend on the
   producer's byte order, and the one a CSV or JSON column arrives by. */
static SEXP xintFromString(SEXP x, int width, int kind, int hasNA)
{
    R_xlen_t n = XLENGTH(x);
    SEXP val = PROTECT(R_allocXIntVector(n, width, kind,
					 hasNA ? TRUE : FALSE));
    R_xlen_t nBad = 0, nOver = 0;

    for (R_xlen_t i = 0; i < n; i++) {
	SEXP s = STRING_ELT(x, i);
	Rbyte *p = XINT_ELT(val, i);

	if (s == NA_STRING) {
	    R_xintCheckNA(val);
	    R_xintSetEltNA(p, width, kind);
	    continue;
	}

	R_xint_parse_t st = R_xintEltFromString(p, CHAR(s), width, kind,
						  hasNA != 0);
	if (st == XINT_PARSE_OK) continue;

	if (st == XINT_PARSE_SYNTAX) nBad++; else nOver++;
	R_xintCheckNA(val);
	R_xintSetEltNA(p, width, kind);
    }

    /* the two failures get their own warnings, as they do for
       as.integer(): "abc" and "1e99" are different mistakes */
    if (nBad)
	warning(_("NAs introduced by coercion"));
    if (nOver)
	warning(_("NAs introduced by values outside the range of '%s'"),
		R_xintTypeName(val));

    UNPROTECT(1);

    return val;
}

/* Build an 'xinteger' vector from a double vector.  Only a value that is
   exactly the integer it appears to be is taken: a finite double whose
   value is integral and in range converts exactly, while a fraction, an
   infinity, a NaN or a magnitude the type cannot hold becomes NA with a
   warning -- the two failures counted apart, as as.integer() does,
   because 1.5 and 1e99 are different mistakes.

   What this CANNOT see is a double that was already wrong before it got
   here.  The literal 9007199254740993 is the double 9007199254740992 by
   the time as.xinteger() is called, and nothing at this point can tell it
   from a value that was always ...992.  That is a property of double
   literals rather than of this conversion, and the character form is
   what carries such magnitudes intact -- which is why ?xinteger points at
   it for anything past 2^53.

   Every element goes through the decimal text parser.  A finite double
   has a finite exact decimal expansion, so printing it and parsing that
   is exact throughout the uint64 domain. */
static SEXP xintFromReal(SEXP x, int width, int kind, int hasNA)
{
    R_xlen_t n = XLENGTH(x);
    SEXP val = PROTECT(R_allocXIntVector(n, width, kind,
					 hasNA ? TRUE : FALSE));
    R_xlen_t nBad = 0, nOver = 0;

    for (R_xlen_t i = 0; i < n; i++) {
	double v = REAL_ELT(x, i);
	Rbyte *p = XINT_ELT(val, i);
	/* the widest exact decimal expansion a double has, plus sign */
	char buf[DBL_MAX_10_EXP + 8];

	if (ISNAN(v)) {
	    R_xintCheckNA(val);
	    R_xintSetEltNA(p, width, kind);
	    continue;
	}

	if (!R_FINITE(v) || v != floor(v)) {
	    nBad++;
	    R_xintCheckNA(val);
	    R_xintSetEltNA(p, width, kind);
	    continue;
	}

	snprintf(buf, sizeof buf, "%.0f", v);

	/* the text came from a double, so it cannot fail to parse */
	if (R_xintEltFromString(p, buf, width, kind, hasNA != 0)
	    == XINT_PARSE_OK) continue;

	nOver++;
	R_xintCheckNA(val);
	R_xintSetEltNA(p, width, kind);
    }

    if (nBad)
	warning(_("NAs introduced by coercion"));
    if (nOver)
	warning(_("NAs introduced by values outside the range of '%s'"),
		R_xintTypeName(val));

    UNPROTECT(1);

    return val;
}

/* Build an 'xinteger' vector of this width and kind from x.  Behind
   as.xinteger(), and behind as.vector(x, "int64") and its relatives,
   which name the width and kind rather than passing them.

   A raw vector is reinterpreted verbatim as width-byte elements, so the
   caller supplies native byte order.  This makes ingest from an external
   source a plain memcpy.
   A character vector is parsed.  Integer and logical vectors narrow,
   as they do in arithmetic.  A double is taken only where it is exactly
   the integer it looks like; see xintFromReal() above. */
SEXP R_xintConvert(SEXP x, int width, int kind, int hasNA, SEXP call)
{
    if (R_isXInt(x)) {
	if (XINT_WIDTH(x) == width && XINT_KIND(x) == kind &&
	    XINT_HAS_NA(x) == hasNA)
	    return x;

	return R_xintFromXInt(x, width, kind, hasNA, call);
    }

    if (TYPEOF(x) == STRSXP)
	return xintFromString(x, width, kind, hasNA);

    if (TYPEOF(x) == INTSXP || TYPEOF(x) == LGLSXP) {
	return R_xintNarrow(x, width, kind, hasNA, call);
    }

    if (TYPEOF(x) == REALSXP) {
	return xintFromReal(x, width, kind, hasNA);
    }

    if (TYPEOF(x) != RAWSXP)
	error(_("cannot convert type '%s' to '%s'; supply raw bytes or decimal text"),
	      R_typeToChar(x), "xinteger");

    R_xlen_t nbytes = XLENGTH(x);
    if (nbytes % width)
	error(_("length of 'x' (%lld) is not a multiple of 'width' (%d)"),
	      (long long) nbytes, width);

    SEXP val = PROTECT(R_allocXIntVector(nbytes / width, width, kind,
					 hasNA ? TRUE : FALSE));
    if (nbytes > 0)
	R_xintMemcpy(XINT_DATA(val), RAW_RO(x), (size_t) nbytes);

    R_xintWarnReserved(val);

    UNPROTECT(1);

    return val;
}

attribute_hidden SEXP do_asxinteger(SEXP call, SEXP op, SEXP args, SEXP env)
{
    checkArity(op, args);

    SEXP val = PROTECT(R_xintConvert(CAR(args), checkWidth(CADR(args)),
				      checkKind(CADDR(args)),
				      checkNA(CADDDR(args)), call));

    /* as.xinteger() is a coercion and drops attributes, as as.numeric() and
       as.character() do.  R_xintConvert() builds a fresh vector on every
       path but one -- when nothing would change it returns x -- so
       without this the same call would keep names or dim for exactly the
       inputs that needed no conversion.  This is what do_asvector() does
       for the same reason; storage.mode<- wants the attributes and so
       calls R_xintConvert() directly. */
    if (ATTRIB(val) != R_NilValue) {
	if (MAYBE_REFERENCED(val)) val = duplicate(val);
	CLEAR_ATTRIB(val);
    }
    UNPROTECT(1);

    return val;
}

/* Elements are stored in native byte order, but must go onto
   the wire in a fixed one or a file written on one platform would read
   as different values on another.  Most significant byte first is the
   choice, matching what R already does for integers and reals under XDR.

   The mapping is its own inverse, so one function serves both
   directions. */
void R_xintSwapWire(Rbyte *dst, const Rbyte *src, R_xlen_t n, int w)
{
    for (R_xlen_t e = 0; e < n; e++) {
	const Rbyte *se = src + e * w;
	Rbyte *de = dst + e * w;
	for (int i = 0; i < w; i++) de[i] = se[XINT_MSB(i, w)];
    }
}

/* is.xinteger() recognizes the two built-in integer classes, not every
   package-defined ALTSXP class. */
attribute_hidden SEXP do_xintegeris(SEXP call, SEXP op, SEXP args, SEXP env)
{
    checkArity(op, args);
    return ScalarLogical(R_isXInt(CAR(args)));
}

/* Nothing to recycle from: the copies below stride the source by the
   element width, so a zero-length one would read w bytes past the end
   of its payload for every element written.  Rf_copyVector() and
   Rf_copyMatrix() are public API and make no such check for any type;
   this one says so rather than copying whatever follows. */
static void checkRecycleSource(R_xlen_t n, R_xlen_t nsrc, const char *fun)
{
    if (n > 0 && nsrc == 0)
	error(_("cannot recycle a zero-length 'xinteger' vector in '%s'"), fun);
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
    size_t w = (size_t) XINT_WIDTH(dst);
    Rbyte *d = XINT_DATA(dst) + (size_t) dstart * w;
    const Rbyte *s = XINT_DATA_RO(src);

    for (R_xlen_t done = 0; done < n; ) {
	R_xlen_t run = n - done;
	if (run > nsrc - sidx) run = nsrc - sidx;
	R_xintMemcpy(d + (size_t) done * w, s + (size_t) sidx * w,
		      (size_t) run * w);
	done += run;
	sidx += run;
	if (sidx == nsrc) sidx = 0;
    }

    return sidx;
}

void R_xintCopyWithRecycle(SEXP dst, SEXP src, R_xlen_t dstart,
			    R_xlen_t n, R_xlen_t nsrc)
{
    R_xintCheckSameType(dst, src, "copyVector");
    checkRecycleSource(n, nsrc, "copyVector");

    copyRuns(dst, src, dstart, n, nsrc, 0);
}

void R_xintFillMatrixWithRecycle(SEXP dst, SEXP src, R_xlen_t dstart,
				  R_xlen_t drows, R_xlen_t srows,
				  R_xlen_t cols, R_xlen_t nsrc)
{
    R_xintCheckSameType(dst, src, "copyMatrix");
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

const char *R_xintKindName(SEXP x)
{
    switch (XINT_KIND(x)) {
    case XINT_UNSIGNED: return "unsigned";
    case XINT_SIGNED:  return "signed";
    default:           return "invalid";
    }
}

/* The .Internal()s below all take one 'xinteger' vector as 'x'.  Not
   checkXInt(), whose message names a C entry point: that is the right
   thing to tell a package and the wrong thing to tell someone who
   typed xintegerWidth(1L). */
static SEXP checkXIntArg(SEXP args)
{
    SEXP x = CAR(args);

    if (!R_isXInt(x))
	error(_("'%s' must be an 'xinteger' vector"), "x");

    return x;
}

/* xintegerHasNA(x) */
attribute_hidden SEXP do_xintegerhasna(SEXP call, SEXP op, SEXP args, SEXP env)
{
    checkArity(op, args);

    return ScalarLogical(XINT_HAS_NA(checkXIntArg(args)));
}

/* xintegerKind(x) */
attribute_hidden SEXP do_xintegerkind(SEXP call, SEXP op, SEXP args, SEXP env)
{
    checkArity(op, args);

    return mkString(R_xintKindName(checkXIntArg(args)));
}

/* xintegerRaw(x): the flat payload, for round-tripping and for handing
   the bytes to code that wants an ordinary raw vector */
attribute_hidden SEXP do_xintegerraw(SEXP call, SEXP op, SEXP args, SEXP env)
{
    checkArity(op, args);

    SEXP x = checkXIntArg(args);

    R_xlen_t nbytes = XLENGTH(x) * XINT_WIDTH(x);
    SEXP val = PROTECT(allocVector(RAWSXP, nbytes));
    if (nbytes > 0)
	R_xintMemcpy(RAW(val), XINT_DATA_RO(x), (size_t) nbytes);
    UNPROTECT(1);

    return val;
}

/* How one element is shown in every context.  Returns a pointer into a
   static buffer, so
   the result must be used before the next call. */
const char *R_xintEltRender(SEXP x, R_xlen_t i)
{
    int w = XINT_WIDTH(x), k = XINT_KIND(x);
    const Rbyte *p = XINT_ELT_RO(x, i);

    if (XINT_HAS_NA(x) && R_xintEltIsNA(p, w, k))
	return CHAR(R_print.na_string);

    return R_xintEltDecimal(p, w, k);
}

/* storage.mode(), implicit classes and diagnostics distinguish the two
   built-in classes, while typeof() follows the SEXPTYPE and reports "alt". */
/* The closed set of detailed names, in one table for both directions.
   String literals give R_typeToChar() the stable pointers its diagnostics
   need without writable storage or lazy initialization. */
static const struct {
    int width;
    const char *names[2];
} xintTypeNames[] = {
    { 8, { "uint64", "int64" } }
};

const char *R_xintTypeNameOf(int w, int kind)
{
    if (kind != XINT_UNSIGNED && kind != XINT_SIGNED)
	return "xinteger";

    for (size_t i = 0; i < sizeof xintTypeNames / sizeof xintTypeNames[0]; i++)
	if (xintTypeNames[i].width == w)
	    return xintTypeNames[i].names[kind - 1];

    return "xinteger";
}

const char *R_xintTypeName(SEXP x)
{
    return R_xintTypeNameOf(XINT_WIDTH(x), XINT_KIND(x));
}

/* The inverse, for readBin(con, "int64") and friends. */
Rboolean R_xintTypeFromName(const char *s, int *width, int *kind)
{
    for (size_t i = 0; i < sizeof xintTypeNames / sizeof xintTypeNames[0]; i++)
	for (int k = XINT_UNSIGNED; k <= XINT_SIGNED; k++)
	    if (!strcmp(s, xintTypeNames[i].names[k - 1])) {
		*width = xintTypeNames[i].width;
		*kind = k;
		return TRUE;
	    }

    return FALSE;
}

/* xintegerWidth(x) */
attribute_hidden SEXP do_xintegerwidth(SEXP call, SEXP op, SEXP args, SEXP env)
{
    checkArity(op, args);

    return ScalarInteger(XINT_WIDTH(checkXIntArg(args)));
}
