#ifndef R_INT64_UTILS_H
#define R_INT64_UTILS_H

#include <ctype.h>
#include <errno.h>
#include <inttypes.h>
#include <stdbool.h>

typedef enum {
    INT64_PARSE_INVALID,
    INT64_PARSE_EXACT,
    INT64_PARSE_FRACTION,
    INT64_PARSE_RANGE
} int64_parse_status_t;

static R_INLINE R_int64_t int64_from_integer(int x)
{
    return x == NA_INTEGER ? NA_INT64 : (R_int64_t) x;
}

static R_INLINE Rboolean int64_fits_integer(R_int64_t x)
{
    return x != NA_INT64 && x >= -INT_MAX && x <= INT_MAX;
}

static R_INLINE Rboolean int64_add_ok(R_int64_t x, R_int64_t y,
				      R_int64_t *out)
{
    if (x == NA_INT64 || y == NA_INT64) {
	*out = NA_INT64;
	return TRUE;
    }
    if ((y > 0 && x > R_INT64_MAX - y) ||
	(y < 0 && x < R_INT64_MIN - y))
	return FALSE;
    *out = x + y;
    return TRUE;
}

static R_INLINE Rboolean int64_sub_ok(R_int64_t x, R_int64_t y,
				      R_int64_t *out)
{
    if (x == NA_INT64 || y == NA_INT64) {
	*out = NA_INT64;
	return TRUE;
    }
    if ((y > 0 && x < R_INT64_MIN + y) ||
	(y < 0 && x > R_INT64_MAX + y))
	return FALSE;
    *out = x - y;
    return TRUE;
}

static R_INLINE Rboolean int64_mul_ok(R_int64_t x, R_int64_t y,
				      R_int64_t *out)
{
    if (x == NA_INT64 || y == NA_INT64) {
	*out = NA_INT64;
	return TRUE;
    }
#ifdef __SIZEOF_INT128__
    __int128 z = (__int128) x * (__int128) y;
    if (z < R_INT64_MIN || z > R_INT64_MAX)
	return FALSE;
    *out = (R_int64_t) z;
    return TRUE;
#else
    if (x == 0 || y == 0) {
	*out = 0;
	return TRUE;
    }
    if (x > 0 ? (y > 0 ? x > R_INT64_MAX / y :
			  y < R_INT64_MIN / x) :
	x < 0 ? (y > 0 ? x < R_INT64_MIN / y :
			 y < R_INT64_MAX / x) :
	false)
	return FALSE;
    *out = x * y;
    return TRUE;
#endif
}

static R_INLINE Rboolean int64_mul_xlen_ok(R_int64_t x, R_xlen_t y,
					   R_int64_t *out)
{
#ifdef __SIZEOF_INT128__
    __int128 z = (__int128) x * (__int128) y;
    if (z < R_INT64_MIN || z > R_INT64_MAX)
	return FALSE;
    *out = (R_int64_t) z;
    return TRUE;
#else
    if (y == 0) {
	*out = 0;
	return TRUE;
    }
    if ((x > 0 && y > R_INT64_MAX / x) ||
	(x < 0 && y > R_INT64_MIN / x))
	return FALSE;
    *out = x * (R_int64_t) y;
    return TRUE;
#endif
}

static R_INLINE R_int64_t int64_plus(R_int64_t x, R_int64_t y,
				     bool *overflow)
{
    R_int64_t out;
    if (!int64_add_ok(x, y, &out)) {
	*overflow = true;
	return NA_INT64;
    }
    return out;
}

static R_INLINE R_int64_t int64_minus(R_int64_t x, R_int64_t y,
				      bool *overflow)
{
    R_int64_t out;
    if (!int64_sub_ok(x, y, &out)) {
	*overflow = true;
	return NA_INT64;
    }
    return out;
}

static R_INLINE R_int64_t int64_times(R_int64_t x, R_int64_t y,
				      bool *overflow)
{
    R_int64_t out;
    if (!int64_mul_ok(x, y, &out)) {
	*overflow = true;
	return NA_INT64;
    }
    return out;
}

static R_INLINE Rboolean int64_parse_integer_string(const char *s,
						    Rboolean allow_leading_space,
						    Rboolean allow_trailing_space,
						    R_int64_t *out)
{
    const char *p = s;
    if (allow_leading_space)
	while (isspace((unsigned char) *p)) p++;

    const char *q = p;
    if (*q == '+' || *q == '-') q++;
    Rboolean is_hex = q[0] == '0' && (q[1] == 'x' || q[1] == 'X');

    char *endp;
    errno = 0;
    intmax_t val = strtoimax(p, &endp, is_hex ? 0 : 10);
    if (endp == p || errno == ERANGE ||
	val < R_INT64_MIN || val > R_INT64_MAX)
	return FALSE;
    if (allow_trailing_space ? !isBlankString(endp) : *endp != '\0')
	return FALSE;

    *out = (R_int64_t) val;
    return TRUE;
}

static R_INLINE int64_parse_status_t
int64_parse_decimal_string(const char *s, Rboolean allow_leading_space,
			   Rboolean allow_trailing_space,
			   Rboolean allow_trailing_L, R_int64_t *out)
{
    const char *p = s;
    if (allow_leading_space)
	while (isspace((unsigned char) *p)) p++;

    Rboolean neg = FALSE, seen_digit = FALSE, seen_dot = FALSE;
    if (*p == '+' || *p == '-') {
	neg = *p == '-';
	p++;
    }

    const char *mantissa = p;
    size_t ndigits = 0, frac_digits = 0;
    while (*p) {
	if (*p >= '0' && *p <= '9') {
	    ndigits++;
	    if (seen_dot) frac_digits++;
	    seen_digit = TRUE;
	    p++;
	} else if (*p == '.' && !seen_dot) {
	    seen_dot = TRUE;
	    p++;
	} else {
	    break;
	}
    }
    const char *mantissa_end = p;
    if (!seen_digit) return INT64_PARSE_INVALID;

    Rboolean exp_neg = FALSE;
    long exp = 0;
    if (*p == 'e' || *p == 'E') {
	p++;
	if (*p == '+' || *p == '-') {
	    exp_neg = *p == '-';
	    p++;
	}
	if (*p < '0' || *p > '9') return INT64_PARSE_INVALID;
	while (*p >= '0' && *p <= '9') {
	    if (exp < 1000000) {
		exp = 10 * exp + (*p - '0');
		if (exp > 1000000) exp = 1000000;
	    }
	    p++;
	}
    }
    if (allow_trailing_L && *p == 'L') p++;
    if (allow_trailing_space ? !isBlankString(p) : *p != '\0')
	return INT64_PARSE_INVALID;

    long scale = (exp_neg ? -exp : exp) - (long) frac_digits;
    size_t digits_to_use = ndigits;
    if (scale < 0) {
	long trim = -scale;
	if ((size_t) trim >= ndigits) {
	    for (const char *r = mantissa; r < mantissa_end; r++)
		if (*r >= '1' && *r <= '9')
		    return INT64_PARSE_FRACTION;
	    *out = 0;
	    return INT64_PARSE_EXACT;
	}
	digits_to_use -= (size_t) trim;
	size_t i = 0;
	for (const char *r = mantissa; r < mantissa_end; r++) {
	    if (*r < '0' || *r > '9') continue;
	    if (i++ >= digits_to_use && *r != '0')
		return INT64_PARSE_FRACTION;
	}
	scale = 0;
    }

    const uintmax_t limit = (uintmax_t) R_INT64_MAX;
    uintmax_t mag = 0;
    Rboolean seen_nonzero = FALSE;
    size_t i = 0;
    for (const char *r = mantissa; r < mantissa_end; r++) {
	if (*r < '0' || *r > '9') continue;
	if (i++ >= digits_to_use) break;
	uintmax_t digit = (uintmax_t) (*r - '0');
	if (!seen_nonzero && digit == 0) continue;
	seen_nonzero = TRUE;
	if (mag > (limit - digit) / 10)
	    return INT64_PARSE_RANGE;
	mag = 10 * mag + digit;
    }
    if (!seen_nonzero) {
	*out = 0;
	return INT64_PARSE_EXACT;
    }
    while (scale-- > 0) {
	if (mag > limit / 10)
	    return INT64_PARSE_RANGE;
	mag *= 10;
    }

    *out = neg ? -(R_int64_t) mag : (R_int64_t) mag;
    return INT64_PARSE_EXACT;
}

#endif
