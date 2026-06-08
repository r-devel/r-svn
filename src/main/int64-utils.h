#ifndef R_INT64_UTILS_H
#define R_INT64_UTILS_H

#include <stdbool.h>

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

#endif
