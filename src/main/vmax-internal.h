/* Internal R_alloc stack accessors.  Not for use by loadable modules. */
#ifndef R_VMAX_INTERNAL_H
#define R_VMAX_INTERNAL_H

extern attribute_hidden SEXP R_VStack;

static R_INLINE void *R_VMaxGet(void)
{
    return (void *) R_VStack;
}

static R_INLINE void R_VMaxSet(const void *vmax)
{
    R_VStack = (SEXP) vmax;
}

#endif
