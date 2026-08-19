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

/* Allocated as a RAWSXP of the full byte size so that the standard
   vector allocator does the size-class selection and heap accounting,
   then retyped and given its true element count.  getVecSizeInVEC()
   multiplies the width back out, so the GC sees the same byte size
   both here and at collection time. */
SEXP R_allocBytesVector(R_xlen_t length, int width)
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
    SET_STDVEC_LENGTH(val, length);

    /* zero-filled: these bytes are user-visible values, and leaving
       them undefined would make results depend on heap history */
    if (length > 0)
	memset(BYTEVEC_DATA(val), 0, (size_t) length * width);

    UNPROTECT(1);

    return val;
}

/* NA is the all-0xFF element.  See BYTEVEC_NA_BYTE in Defn.h for why
   0xFF rather than 0x00. */
Rboolean R_bytesEltIsNA(const Rbyte *p, int width)
{
    for (int i = 0; i < width; i++)
	if (p[i] != BYTEVEC_NA_BYTE) return FALSE;

    return TRUE;
}

void R_bytesSetEltNA(Rbyte *p, int width)
{
    memset(p, BYTEVEC_NA_BYTE, (size_t) width);
}

/* allocVector(TYPEOF(s), n) cannot reproduce a per-vector width, so the
   generic "another vector like this one" sites go through here */
SEXP R_allocVectorLike(SEXP s, R_xlen_t length)
{
    if (TYPEOF(s) == BYTESXP)
	return R_allocBytesVector(length, BYTEVEC_WIDTH(s));

    return allocVector(TYPEOF(s), length);
}

attribute_hidden int R_bytesWidth(SEXP x)
{
    if (TYPEOF(x) != BYTESXP)
	error(_("'%s' must be a 'bytes' vector"), "x");

    return BYTEVEC_WIDTH(x);
}

/* shared argument checking for the .Internal()s below */
static int checkWidth(SEXP swidth)
{
    int width = asInteger(swidth);

    if (width == NA_INTEGER || width < 1 || width > BYTEVEC_MAX_WIDTH)
	error(_("'width' must be between 1 and %d"), BYTEVEC_MAX_WIDTH);

    return width;
}

/* bytes(length, width) */
attribute_hidden SEXP do_bytes(SEXP call, SEXP op, SEXP args, SEXP env)
{
    checkArity(op, args);

    double len = asReal(CAR(args));
    if (!R_FINITE(len) || len < 0)
	error(_("invalid '%s' argument"), "length");
    int width = checkWidth(CADR(args));

    return R_allocBytesVector((R_xlen_t) len, width);
}

/* as.bytes(x, width): reinterpret a raw vector as width-byte elements */
attribute_hidden SEXP do_asbytes(SEXP call, SEXP op, SEXP args, SEXP env)
{
    checkArity(op, args);

    SEXP x = CAR(args);
    if (TYPEOF(x) != RAWSXP)
	error(_("'%s' must be a raw vector"), "x");
    int width = checkWidth(CADR(args));

    R_xlen_t nbytes = XLENGTH(x);
    if (nbytes % width)
	error(_("length of 'x' (%lld) is not a multiple of 'width' (%d)"),
	      (long long) nbytes, width);

    SEXP val = PROTECT(R_allocBytesVector(nbytes / width, width));
    if (nbytes > 0)
	memcpy(BYTEVEC_DATA(val), RAW_RO(x), (size_t) nbytes);

    R_xlen_t nNA = 0;
    for (R_xlen_t i = 0; i < XLENGTH(val); i++)
	if (R_bytesEltIsNA(BYTEVEC_ELT_RO(val, i), width)) nNA++;
    if (nNA)
	warning(ngettext("%lld element equal to the reserved NA value became NA",
			 "%lld elements equal to the reserved NA value became NA",
			 (unsigned long) nNA), (long long) nNA);

    UNPROTECT(1);

    return val;
}

/* bytesNA(length, width) */
attribute_hidden SEXP do_bytesna(SEXP call, SEXP op, SEXP args, SEXP env)
{
    checkArity(op, args);

    double len = asReal(CAR(args));
    if (!R_FINITE(len) || len < 0)
	error(_("invalid '%s' argument"), "length");
    int width = checkWidth(CADR(args));

    SEXP val = PROTECT(R_allocBytesVector((R_xlen_t) len, width));
    if (XLENGTH(val) > 0)
	memset(BYTEVEC_DATA(val), BYTEVEC_NA_BYTE,
	       (size_t) XLENGTH(val) * width);
    UNPROTECT(1);

    return val;
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

/* bytesWidth(x) */
attribute_hidden SEXP do_byteswidth(SEXP call, SEXP op, SEXP args, SEXP env)
{
    checkArity(op, args);

    return ScalarInteger(R_bytesWidth(CAR(args)));
}
