/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 2015-2026 R Core Team
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
 */

/* Formatted output on Windows.

   R used the trio library here because msvcrt.dll was not C99 (no %lld,
   no hex floats, three-digit exponents).  The UCRT is, but the mingw-w64
   wrappers for it round exact ties away from zero ("%.0f" of 2.5 gives
   "3") and ignore positional arguments ("%2$s"), which some translations
   use.  So call the UCRT directly, asking for IEEE rounding and using the
   variants which accept positional arguments.

   Include this after the system headers: the names are function-like
   macros, so struct members such as con->vfprintf are left alone. */

#ifndef R_TRIOREMAP_H
#define R_TRIOREMAP_H

#include <stdio.h>
#include <stdarg.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>

/* Not in the mingw-w64 headers; honoured by the UCRT of Windows 10 2004
   and later */
#ifndef _CRT_INTERNAL_PRINTF_STANDARD_ROUNDING
# define _CRT_INTERNAL_PRINTF_STANDARD_ROUNDING 0x0020ULL
#endif

#define R_PRINTF_OPTIONS (_CRT_INTERNAL_PRINTF_STANDARD_SNPRINTF_BEHAVIOR | \
			  _CRT_INTERNAL_PRINTF_STANDARD_ROUNDING)

/* in printutils.c */
int Rwin_vsnprintf_wide(char *buf, size_t size, const char *format,
			va_list ap);

static inline int Rwin_vsnprintf(char *buf, size_t size, const char *format,
				 va_list ap)
{
    va_list aq;

    va_copy(aq, ap);
    int res = __stdio_common_vsprintf_p(R_PRINTF_OPTIONS, buf, size, format,
					NULL, aq);
    va_end(aq);

    /* a %ls or %lc argument not representable in the current locale */
    if (res < 0 && errno == EILSEQ)
	res = Rwin_vsnprintf_wide(buf, size, format, ap);
    return res;
}

static inline int Rwin_snprintf(char *buf, size_t size, const char *format,
				...)
{
    va_list ap;

    va_start(ap, format);
    int res = Rwin_vsnprintf(buf, size, format, ap);
    va_end(ap);
    return res;
}

static inline int Rwin_vasprintf(char **strp, const char *format, va_list ap)
{
    va_list aq;

    va_copy(aq, ap);
    int len = Rwin_vsnprintf(NULL, 0, format, aq);
    va_end(aq);
    if (len < 0)
	return -1;

    *strp = malloc((size_t) len + 1);
    if (!*strp)
	return -1;
    int res = Rwin_vsnprintf(*strp, (size_t) len + 1, format, ap);
    if (res < 0) {
	free(*strp);
	*strp = NULL;
    }
    return res;
}

static inline int Rwin_vfprintf(FILE *fp, const char *format, va_list ap)
{
    return __stdio_common_vfprintf_p(_CRT_INTERNAL_PRINTF_STANDARD_ROUNDING,
				     fp, format, NULL, ap);
}

static inline int Rwin_fprintf(FILE *fp, const char *format, ...)
{
    va_list ap;

    va_start(ap, format);
    int res = Rwin_vfprintf(fp, format, ap);
    va_end(ap);
    return res;
}

/* The number of hex digits glibc prints after the point for "%a"; the
   UCRT always prints all 13 */
static inline int Rwin_hexdigits(double x)
{
    unsigned long long u;

    memcpy(&u, &x, sizeof(u));
    u &= 0xFFFFFFFFFFFFFULL;
    if (!u)
	return 0;

    int n = 13;
    for (; !(u & 0xF); u >>= 4)
	n--;
    return n;
}

#undef snprintf
#undef vsnprintf
#undef fprintf
#undef vfprintf
#undef vasprintf
#define snprintf(...) Rwin_snprintf(__VA_ARGS__)
#define vsnprintf(...) Rwin_vsnprintf(__VA_ARGS__)
#define fprintf(...) Rwin_fprintf(__VA_ARGS__)
#define vfprintf(...) Rwin_vfprintf(__VA_ARGS__)
#define vasprintf(...) Rwin_vasprintf(__VA_ARGS__)

#endif  /* not R_TRIOREMAP_H */
