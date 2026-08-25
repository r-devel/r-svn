/* included  twice  from  ./split.c   after defining
    _L_INTSXP_, _L_INTEG_,  _L_int_
    ===========  =========  ========    as
      INTSXP  ,  INTEGER ,  int         or
      REALSXP ,  REAL    ,  R_xlen_t
*/
{
    PROTECT(counts = allocVector(_L_INTSXP_, nlevs));
    for (int i = 0; i < nlevs; i++) _L_INTEG_(counts)[i] = 0;
    R_xlen_t i, i1;
    MOD_ITERATE1(nobs, nfac, i, i1, {
	int j = INTEGER(f)[i1];
	if (j != NA_INTEGER) {
	    /* protect against malformed factors */
	    if (j > nlevs || j < 1) error(_("factor has bad level"));
	    _L_INTEG_(counts)[j - 1]++;
	}
    });
    /* Allocate a generic vector to hold the results. */
    /* The i-th element will hold the split-out data */
    /* for the ith group. */
    PROTECT(vec = allocVector(VECSXP, nlevs));
    for (R_xlen_t i = 0;  i < nlevs; i++) {
	SET_VECTOR_ELT(vec, i,
		       TYPEOF(x) == ALTSXP
		       ? R_altsxp_new(x, (_L_int_)_L_INTEG_(counts)[i])
		       : allocVector(TYPEOF(x), (_L_int_)_L_INTEG_(counts)[i]));
	setAttrib(VECTOR_ELT(vec, i), R_LevelsSymbol,
		  getAttrib(x, R_LevelsSymbol));
	if(have_names)
	    setAttrib(VECTOR_ELT(vec, i), R_NamesSymbol,
		      allocVector(STRSXP, (_L_int_)_L_INTEG_(counts)[i]));
    }
    for (int i = 0; i < nlevs; i++) _L_INTEG_(counts)[i] = 0;
    size_t altsxp_esz = (TYPEOF(x) == ALTSXP) ? ALTSXP_ELT_SIZE(x) : 0;
    void *altsxp_buf = altsxp_esz ? R_alloc(1, altsxp_esz) : NULL;
    MOD_ITERATE1(nobs, nfac, i, i1, {
	int j = INTEGER(f)[i1];
	if (j != NA_INTEGER) {
	    _L_int_ k = (_L_int_)_L_INTEG_(counts)[j - 1];
	    switch (TYPEOF(x)) {
	    case LGLSXP:
	    case INTSXP:
		INTEGER(VECTOR_ELT(vec, j - 1))[k] = INTEGER(x)[i];
		break;
	    case REALSXP:
		REAL(VECTOR_ELT(vec, j - 1))[k] = REAL(x)[i];
		break;
	    case CPLXSXP:
		COMPLEX(VECTOR_ELT(vec, j - 1))[k] = COMPLEX(x)[i];
		break;
	    case STRSXP:
		SET_STRING_ELT(VECTOR_ELT(vec, j - 1), k, STRING_ELT(x, i));
		break;
	    case VECSXP:
		SET_VECTOR_ELT(VECTOR_ELT(vec, j - 1), k, VECTOR_ELT(x, i));
		break;
	    case RAWSXP:
		RAW(VECTOR_ELT(vec, j - 1))[k] = RAW(x)[i];
		break;
	    case ALTSXP:
		R_altsxp_get_region(x, i, 1, altsxp_buf);
		R_altsxp_set_region(VECTOR_ELT(vec, j - 1), k, 1, altsxp_buf);
		break;
	    default:
		UNIMPLEMENTED_TYPE("split", x);
	    }
	    if(have_names) {
		nmj = getAttrib(VECTOR_ELT(vec, j - 1), R_NamesSymbol);
		SET_STRING_ELT(nmj, k, STRING_ELT(nm, i));
	    }
	    _L_INTEG_(counts)[j - 1] += 1;
	}
    });
}
