#ifndef R_INT64_ACCUM_H
#define R_INT64_ACCUM_H

typedef struct {
    uint64_t hi;
    uint64_t lo;
} R_int64_accum_t;

static R_INLINE void int64_accum_init(R_int64_accum_t *sum)
{
    sum->hi = 0;
    sum->lo = 0;
}

static R_INLINE void int64_accum_add(R_int64_accum_t *sum, R_int64_t x)
{
    uint64_t old_lo = sum->lo;
    uint64_t ux = (uint64_t) x;
    sum->lo += ux;
    sum->hi += (x < 0 ? ~(uint64_t) 0 : (uint64_t) 0) + (sum->lo < old_lo);
}

static R_INLINE void int64_accum_add_accum(R_int64_accum_t *sum,
					   const R_int64_accum_t *x)
{
    uint64_t old_lo = sum->lo;
    sum->lo += x->lo;
    sum->hi += x->hi + (sum->lo < old_lo);
}

static R_INLINE double int64_accum_to_double(const R_int64_accum_t *sum)
{
    const double two64 = 18446744073709551616.0;
    if (sum->hi >> 63) {
	uint64_t lo = ~sum->lo + 1;
	uint64_t hi = ~sum->hi + (lo == 0);
	return -((double) hi * two64 + (double) lo);
    }
    return (double) sum->hi * two64 + (double) sum->lo;
}

#endif
