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

   mingw-w64's own formatter, which does not accept positional arguments,
   takes the formats the UCRT gets wrong: long double (which it reads as
   double), "%a" (always 13 hex digits, unlike glibc), floating point when
   the UCRT ignores the rounding option (before Windows 10 2004), and those
   it rejects (%n, the ' flag, unknown conversions).  A rejected format
   calls the invalid parameter handler, which can abort R when it is
   embedded (see R-ints), so that is turned off around the UCRT calls:
   they then fail as trio did.

   Include this after the system headers: the names are function-like
   macros, so struct members such as con->vfprintf are left alone.
   See also 'Formatted output on Windows' in the 'R Internals' manual. */

#ifndef R_WINPRINTF_H
#define R_WINPRINTF_H

#include <stdio.h>
#include <stdarg.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>

/* in the UCRT, but not in the mingw-w64 headers */
#ifndef _CRT_INTERNAL_PRINTF_STANDARD_ROUNDING
# define _CRT_INTERNAL_PRINTF_STANDARD_ROUNDING 0x0020ULL
#endif
_invalid_parameter_handler __cdecl
_set_thread_local_invalid_parameter_handler(_invalid_parameter_handler);

#define R_PRINTF_OPTIONS (_CRT_INTERNAL_PRINTF_STANDARD_SNPRINTF_BEHAVIOR | \
			  _CRT_INTERNAL_PRINTF_STANDARD_ROUNDING)

/* in printutils.c */
int Rwin_vsnprintf_wide(char *buf, size_t size, const char *format,
			va_list ap);

static inline void Rwin_ignore_invalid_parameter(const wchar_t *expression,
						 const wchar_t *function,
						 const wchar_t *file,
						 unsigned int line,
						 uintptr_t reserved)
{
}

static inline int Rwin_ucrt_vsnprintf(char *buf, size_t size,
				      const char *format, _locale_t locale,
				      va_list ap)
{
    _invalid_parameter_handler old =
	_set_thread_local_invalid_parameter_handler(Rwin_ignore_invalid_parameter);
    int res = __stdio_common_vsprintf_p(R_PRINTF_OPTIONS, buf, size, format,
					locale, ap);
    _set_thread_local_invalid_parameter_handler(old);
    return res;
}

static inline int Rwin_ucrt_vfprintf(FILE *fp, const char *format,
				     va_list ap)
{
    _invalid_parameter_handler old =
	_set_thread_local_invalid_parameter_handler(Rwin_ignore_invalid_parameter);
    int res = __stdio_common_vfprintf_p(_CRT_INTERNAL_PRINTF_STANDARD_ROUNDING,
					fp, format, NULL, ap);
    _set_thread_local_invalid_parameter_handler(old);
    return res;
}

/* What a format asks for, as far as choosing a formatter goes */
#define RWIN_FMT_POSITIONAL  1
#define RWIN_FMT_WIDE        2
#define RWIN_FMT_LONG_DOUBLE 4
#define RWIN_FMT_HEX_FLOAT   8
#define RWIN_FMT_FLOAT      16
#define RWIN_FMT_REJECTED   32 /* by the UCRT */

static inline int Rwin_scan_format(const char *f)
{
    int res = 0;

    while ((f = strchr(f, '%'))) {
	int l = 0;

	f++;
	for (; *f && strchr("0123456789$#-+ '.*", *f); f++)
	    if (*f == '$')
		res |= RWIN_FMT_POSITIONAL;
	    else if (*f == '\'')
		res |= RWIN_FMT_REJECTED;
	for (; *f && strchr("hlLjztwI0123456789", *f); f++)
	    if (*f == 'L')
		res |= RWIN_FMT_LONG_DOUBLE;
	    else if (*f == 'l' || *f == 'w')
		l = 1;

	switch (*f) {
	case '\0':
	    return res | RWIN_FMT_REJECTED;
	case 'a': case 'A':
	    res |= RWIN_FMT_HEX_FLOAT;
	    break;
	case 'e': case 'E': case 'f': case 'F': case 'g': case 'G':
	    res |= RWIN_FMT_FLOAT;
	    break;
	case 's': case 'c':
	    if (l)
		res |= RWIN_FMT_WIDE;
	    break;
	case 'S': case 'C':
	    res |= RWIN_FMT_WIDE;
	    break;
	case 'd': case 'i': case 'o': case 'u': case 'x': case 'X':
	case 'p': case 'Z': case '%':
	    break;
	default:
	    res |= RWIN_FMT_REJECTED;
	}
	f++;
    }
    return res;
}

static inline int Rwin_ucrt_snprintf(char *buf, size_t size,
				     const char *format, ...)
{
    va_list ap;

    va_start(ap, format);
    int res = Rwin_ucrt_vsnprintf(buf, size, format, NULL, ap);
    va_end(ap);
    return res;
}

/* Whether the UCRT honours _CRT_INTERNAL_PRINTF_STANDARD_ROUNDING */
static inline int Rwin_ucrt_rounds(void)
{
    static int rounds = -1;

    if (rounds < 0) {
	char buf[8];

	Rwin_ucrt_snprintf(buf, sizeof(buf), "%.0f", 2.5);
	rounds = !strcmp(buf, "2");
    }
    return rounds;
}

/* In order of precedence, as mingw-w64 ignores positional arguments and
   cannot escape wide characters but does not reject formats */
static inline int Rwin_use_mingw(int fmt)
{
    if (fmt & RWIN_FMT_LONG_DOUBLE)
	return 1;
    if (fmt & RWIN_FMT_POSITIONAL)
	return 0;
    if (fmt & RWIN_FMT_REJECTED)
	return 1;
    if (fmt & RWIN_FMT_WIDE)
	return 0;
    return (fmt & RWIN_FMT_HEX_FLOAT) ||
	((fmt & RWIN_FMT_FLOAT) && !Rwin_ucrt_rounds());
}

static inline int Rwin_vsnprintf(char *buf, size_t size, const char *format,
				 va_list ap)
{
    if (Rwin_use_mingw(Rwin_scan_format(format)))
	return __mingw_vsnprintf(buf, size, format, ap);

    va_list aq;

    va_copy(aq, ap);
    int res = Rwin_ucrt_vsnprintf(buf, size, format, NULL, aq);
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
    /* more than measured (say %n changed a later argument) did not fit */
    int res = Rwin_vsnprintf(*strp, (size_t) len + 1, format, ap);
    if (res < 0 || res > len) {
	free(*strp);
	*strp = NULL;
	return -1;
    }
    return res;
}

static inline int Rwin_vfprintf(FILE *fp, const char *format, va_list ap)
{
    int fmt = Rwin_scan_format(format);

    if (Rwin_use_mingw(fmt))
	return __mingw_vfprintf(fp, format, ap);

    /* Format these first, as Rwin_vsnprintf can retry wide arguments and
       the UCRT may fail after writing part of the output (as it can for
       positional arguments, which mingw-w64 does not take) */
    if (fmt & (RWIN_FMT_WIDE | RWIN_FMT_POSITIONAL)) {
	char *s;
	int len = Rwin_vasprintf(&s, format, ap);

	if (len < 0)
	    return len;
	size_t res = fwrite(s, 1, len, fp);
	free(s);
	return res == (size_t) len ? len : -1;
    }

    return Rwin_ucrt_vfprintf(fp, format, ap);
}

static inline int Rwin_fprintf(FILE *fp, const char *format, ...)
{
    va_list ap;

    va_start(ap, format);
    int res = Rwin_vfprintf(fp, format, ap);
    va_end(ap);
    return res;
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

#endif  /* not R_WINPRINTF_H */
