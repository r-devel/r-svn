/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 2026  The R Core Team.
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

/*
 * A C implementation of the byte code compiler in the 'compiler'
 * package (src/library/compiler/R/cmp.R).
 *
 * The code generator mirrors the R implementation function by function:
 * the sequence of constant pool insertions and code buffer writes is the
 * same, so the byte code produced is identical to that produced by
 * compiler::cmpfun() and compiler::compile().  The differences are in
 * the implementation: the constant pool is indexed by a hash table, the
 * compiler environment and inline handler tables are hashed symbol sets,
 * and call matching and constant folding call into R only where the R
 * compiler does.
 *
 * With BCCOMPILE_STANDALONE defined the file builds as a shared library
 * against the public API (R CMD SHLIB); the few internals it needs are
 * reached through .Internal() calls.  Otherwise it is part of libR.
 */

#ifdef HAVE_CONFIG_H
# include <config.h>
#endif

#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <stdint.h>

#include <Defn.h>
#include <Internal.h>
#include <Rinternals.h>

#ifdef BCCOMPILE_STANDALONE
/* built as a shared library: the internals hidden in libR are reached
   through .Internal() calls below, and messages are not translated */
# undef _
# define _(String) (String)
/* the inlined element accessors call hidden ALTREP accessors, so this
   build reads through the data pointer (and so expands ALTREP constants) */
# undef LOGICAL_ELT
# define LOGICAL_ELT(x, i) (LOGICAL(x)[i])
# undef INTEGER_ELT
# define INTEGER_ELT(x, i) (INTEGER(x)[i])
# undef REAL_ELT
# define REAL_ELT(x, i) (REAL(x)[i])
# undef COMPLEX_ELT
# define COMPLEX_ELT(x, i) (COMPLEX(x)[i])
# undef RAW_ELT
# define RAW_ELT(x, i) (RAW(x)[i])
#endif

/* opcodes; the values must match the enum in eval.c */
enum {
    BCMISMATCH_OP, RETURN_OP, GOTO_OP, BRIFNOT_OP, POP_OP, DUP_OP,
    PRINTVALUE_OP, STARTLOOPCNTXT_OP, ENDLOOPCNTXT_OP, DOLOOPNEXT_OP,
    DOLOOPBREAK_OP, STARTFOR_OP, STEPFOR_OP, ENDFOR_OP, SETLOOPVAL_OP,
    INVISIBLE_OP, LDCONST_OP, LDNULL_OP, LDTRUE_OP, LDFALSE_OP, GETVAR_OP,
    DDVAL_OP, SETVAR_OP, GETFUN_OP, GETGLOBFUN_OP, GETSYMFUN_OP,
    GETBUILTIN_OP, GETINTLBUILTIN_OP, CHECKFUN_OP, MAKEPROM_OP,
    DOMISSING_OP, SETTAG_OP, DODOTS_OP, PUSHARG_OP, PUSHCONSTARG_OP,
    PUSHNULLARG_OP, PUSHTRUEARG_OP, PUSHFALSEARG_OP, CALL_OP,
    CALLBUILTIN_OP, CALLSPECIAL_OP, MAKECLOSURE_OP, UMINUS_OP, UPLUS_OP,
    ADD_OP, SUB_OP, MUL_OP, DIV_OP, EXPT_OP, SQRT_OP, EXP_OP, EQ_OP, NE_OP,
    LT_OP, LE_OP, GE_OP, GT_OP, AND_OP, OR_OP, NOT_OP, DOTSERR_OP,
    STARTASSIGN_OP, ENDASSIGN_OP, STARTSUBSET_OP, DFLTSUBSET_OP,
    STARTSUBASSIGN_OP, DFLTSUBASSIGN_OP, STARTC_OP, DFLTC_OP,
    STARTSUBSET2_OP, DFLTSUBSET2_OP, STARTSUBASSIGN2_OP, DFLTSUBASSIGN2_OP,
    DOLLAR_OP, DOLLARGETS_OP, ISNULL_OP, ISLOGICAL_OP, ISINTEGER_OP,
    ISDOUBLE_OP, ISCOMPLEX_OP, ISCHARACTER_OP, ISSYMBOL_OP, ISOBJECT_OP,
    ISNUMERIC_OP, VECSUBSET_OP, MATSUBSET_OP, VECSUBASSIGN_OP,
    MATSUBASSIGN_OP, AND1ST_OP, AND2ND_OP, OR1ST_OP, OR2ND_OP,
    GETVAR_MISSOK_OP, DDVAL_MISSOK_OP, VISIBLE_OP, SETVAR2_OP,
    STARTASSIGN2_OP, ENDASSIGN2_OP, SETTER_CALL_OP, GETTER_CALL_OP, SWAP_OP,
    DUP2ND_OP, SWITCH_OP, RETURNJMP_OP, STARTSUBSET_N_OP,
    STARTSUBASSIGN_N_OP, VECSUBSET2_OP, MATSUBSET2_OP, VECSUBASSIGN2_OP,
    MATSUBASSIGN2_OP, STARTSUBSET2_N_OP, STARTSUBASSIGN2_N_OP, SUBSET_N_OP,
    SUBSET2_N_OP, SUBASSIGN_N_OP, SUBASSIGN2_N_OP, LOG_OP, LOGBASE_OP,
    MATH1_OP, DOTCALL_OP, COLON_OP, SEQALONG_OP, SEQLEN_OP, BASEGUARD_OP,
    INCLNK_OP, DECLNK_OP, DECLNK_N_OP, INCLNKSTK_OP, DECLNKSTK_OP,
    BC_OPCOUNT
};

/* inline handlers (setInlineHandler) */
enum {
    H_NONE = 0, H_FUNCTION, H_BRACE, H_IF, H_AND, H_OR, H_ASSIGN, H_BREAK,
    H_NEXT, H_REPEAT, H_WHILE, H_FOR, H_PLUS, H_MINUS, H_PRIM2, H_PRIM1,
    H_LOG, H_MATH1, H_PAREN, H_BUILTIN, H_SPECIAL, H_DOTINTERNAL, H_DOLLAR,
    H_LOCAL, H_RETURN, H_IS, H_SIMPLEINTERNAL, H_SWITCH, H_DOTCALL,
    H_MULTICOLON, H_SUPPRESSUNDEF, H_SUBSET, H_SUBSET2
};

/* setter (setSetterInlineHandler) and getter (setGetterInlineHandler)
   handlers */
enum { S_NONE = 0, S_DOLLAR, S_AT, S_SUBASSIGN, S_SUBASSIGN2 };
enum { G_NONE = 0, G_DOLLAR, G_SUBSET, G_SUBSET2 };

/* handler packages */
enum { PKG_NONE = 0, PKG_BASE, PKG_STATS };

/* symbol classifications (the character vectors of cmp.R) */
#define SI_FOLDFUN	0x0001	/* foldFuns */
#define SI_LANGFUN	0x0002	/* languageFuns */
#define SI_CONSTNAME	0x0004	/* constNames: pi, T, F */
#define SI_NOINLINE	0x0008	/* noInlineSymbols */
#define SI_LOOPSTOP	0x0010	/* function, for, while, repeat */
#define SI_LOOPTOP	0x0020	/* (, {, if */
#define SI_EVALFUN	0x0040	/* eval, evalq, source */
#define SI_BREAKNEXT	0x0080	/* break, next */
#define SI_INITSHADOW	0x0100	/* expression, local, quote */
#define SI_SPECIALSYNTAX 0x0200	/* ~, <-, =, for, function */
#define SI_NSE		0x0400	/* maybeNSESymbols: bquote */

typedef struct {
    SEXP sym;
    unsigned flags;
    unsigned char handler, handlerPkg, setter, getter;
    signed char op;	    /* opcode for H_PRIM1/H_PRIM2/H_IS, index for H_MATH1 */
    signed char builtinInternal; /* -1 unknown, 0 no, 1 yes */
} SymInfo;

/* open addressing table keyed by symbol address */
static SymInfo *symtab = NULL;
static int symtab_size = 0;	/* power of two */
static int symtab_count = 0;

static R_INLINE unsigned ptr_hash(SEXP p)
{
    uintptr_t v = (uintptr_t) p;
    v ^= v >> 29;
    v *= 0xbf58476d1ce4e5b9ULL;
    v ^= v >> 32;
    return (unsigned) v;
}

static SymInfo *symtab_lookup(SEXP sym, int create)
{
    if (symtab == NULL) {
	symtab_size = 4096;
	symtab = (SymInfo *) calloc(symtab_size, sizeof(SymInfo));
	if (symtab == NULL)
	    Rf_error("allocation failed");
    }
    if (create && 2 * (symtab_count + 1) > symtab_size) {
	int osize = symtab_size;
	SymInfo *otab = symtab;
	symtab_size = 2 * osize;
	symtab = (SymInfo *) calloc(symtab_size, sizeof(SymInfo));
	if (symtab == NULL)
	    Rf_error("allocation failed");
	for (int i = 0; i < osize; i++)
	    if (otab[i].sym != NULL) {
		unsigned h = ptr_hash(otab[i].sym) & (symtab_size - 1);
		while (symtab[h].sym != NULL)
		    h = (h + 1) & (symtab_size - 1);
		symtab[h] = otab[i];
	    }
	free(otab);
    }
    unsigned h = ptr_hash(sym) & (symtab_size - 1);
    while (symtab[h].sym != NULL) {
	if (symtab[h].sym == sym)
	    return &symtab[h];
	h = (h + 1) & (symtab_size - 1);
    }
    if (! create)
	return NULL;
    symtab[h].sym = sym;
    symtab[h].builtinInternal = -1;
    symtab_count++;
    return &symtab[h];
}

static R_INLINE unsigned sym_flags(SEXP sym)
{
    SymInfo *si = symtab_lookup(sym, FALSE);
    return si == NULL ? 0 : si->flags;
}

static void set_flags(const char *name, unsigned flags)
{
    SymInfo *si = symtab_lookup(Rf_install(name), TRUE);
    si->flags |= flags;
}

static void set_handler(const char *name, int handler, int pkg, int op)
{
    SymInfo *si = symtab_lookup(Rf_install(name), TRUE);
    si->handler = (unsigned char) handler;
    si->handlerPkg = (unsigned char) pkg;
    si->op = (signed char) op;
}

/* symbols */
static SEXP s_dotInternal, s_break, s_next, s_tmp, s_vtmp, s_value,
    s_NAMESPACE, s_imports, s_exports, s_spec, s_stop, s_local, s_bquote,
    s_standardGeneric, s_browser, s_srcref, s_srcfile, s_filename,
    s_compilerOptions, s_optimize, s_suppressAll, s_suppressNoSuperAssignVar,
    s_suppressUndefined, s_compiler;
static SEXP emptySwitchCall;	/* quote(stop("empty alternative in numeric switch")) */
static SEXP foldConditions;	/* c("error", "warning") */
static SEXP checkCallFun = NULL; /* R closure used for call checking notes */
static int bc_version = 0;
static int bc_initialized = FALSE;


static const char *math1funs[] = {
    "floor", "ceiling", "sign", "expm1", "log1p",
    "cos", "sin", "tan", "acos", "asin", "atan",
    "cosh", "sinh", "tanh", "acosh", "asinh", "atanh",
    "lgamma", "gamma", "digamma", "trigamma",
    "cospi", "sinpi", "tanpi", NULL
};

static const char *foldFuns[] = {
    "+", "-", "*", "/", "^", "(",
    ">", ">=", "==", "!=", "<", "<=", "||", "&&", "!",
    "|", "&", "%%",
    "c", "rep", ":",
    "abs", "acos", "acosh", "asin", "asinh", "atan", "atan2",
    "atanh", "ceiling", "choose", "cos", "cosh", "exp", "expm1",
    "floor", "gamma", "lbeta", "lchoose", "lgamma", "log", "log10",
    "log1p", "log2", "max", "min", "prod", "range", "round",
    "seq_along", "seq.int", "seq_len", "sign", "signif",
    "sin", "sinh", "sqrt", "sum", "tan", "tanh", "trunc",
    "baseenv", "emptyenv", "globalenv",
    "Arg", "Conj", "Im", "Mod", "Re", NULL
};

static const char *languageFuns[] = {
    "^", "~", "<", "<<-", "<=", "<-", "=", "==", ">", ">=",
    "|", "||", "-", ":", "!", "!=", "/", "(", "[", "[<-", "[[",
    "[[<-", "{", "@", "$", "$<-", "*", "&", "&&", "%/%", "%*%",
    "%%", "+",
    "::", ":::", "@<-",
    "break", "for", "function", "if", "next", "repeat", "while",
    "local", "return", "switch", NULL
};

static const char *safeBaseInternals[] = {
    "atan2", "besselY", "beta", "choose",
    "drop", "inherits", "is.vector", "lbeta", "lchoose",
    "nchar", "polyroot", "typeof", "vector", "which.max",
    "which.min", "is.loaded", "identical",
    "match", "rep.int", "rep_len", NULL
};

static const char *safeStatsInternals[] = {
    "dbinom", "dcauchy", "dgeom", "dhyper", "dlnorm",
    "dlogis", "dnorm", "dpois", "dunif", "dweibull",
    "fft", "mvfft", "pbinom", "pcauchy",
    "pgeom", "phyper", "plnorm", "plogis", "pnorm",
    "ppois", "punif", "pweibull", "qbinom", "qcauchy",
    "qgeom", "qhyper", "qlnorm", "qlogis", "qnorm",
    "qpois", "qunif", "qweibull", "rbinom", "rcauchy",
    "rgeom", "rhyper", "rlnorm", "rlogis", "rnorm",
    "rpois", "rsignrank",  "runif", "rweibull",
    "rwilcox", "ptukey", "qtukey", NULL
};

static void set_setter(const char *name, int setter)
{
    SymInfo *si = symtab_lookup(Rf_install(name), TRUE);
    si->setter = (unsigned char) setter;
}

static void set_getter(const char *name, int getter)
{
    SymInfo *si = symtab_lookup(Rf_install(name), TRUE);
    si->getter = (unsigned char) getter;
}

static SEXP eval_internal(SEXP call);

static void bc_init(void)
{
    if (bc_initialized)
	return;
    bc_initialized = TRUE;

    s_dotInternal = Rf_install(".Internal");
    s_break = Rf_install("break");
    s_next = Rf_install("next");
    s_tmp = Rf_install("*tmp*");
    s_vtmp = Rf_install("*vtmp*");
    s_value = Rf_install("value");
    s_NAMESPACE = Rf_install(".__NAMESPACE__.");
    s_imports = Rf_install("imports");
    s_exports = Rf_install("exports");
    s_spec = Rf_install("spec");
    s_stop = Rf_install("stop");
    s_local = Rf_install("local");
    s_bquote = Rf_install("bquote");
    s_standardGeneric = Rf_install("standardGeneric");
    s_browser = Rf_install("browser");
    s_srcref = Rf_install("srcref");
    s_srcfile = Rf_install("srcfile");
    s_filename = Rf_install("filename");
    s_compilerOptions = Rf_install("compilerOptions");
    s_optimize = Rf_install("optimize");
    s_suppressAll = Rf_install("suppressAll");
    s_suppressNoSuperAssignVar = Rf_install("suppressNoSuperAssignVar");
    s_suppressUndefined = Rf_install("suppressUndefined");
    s_compiler = Rf_install("compiler");

    emptySwitchCall = Rf_lang2(s_stop,
			       Rf_mkString("empty alternative in numeric switch"));
    R_PreserveObject(emptySwitchCall);
    foldConditions = Rf_allocVector(STRSXP, 2);
    R_PreserveObject(foldConditions);
    SET_STRING_ELT(foldConditions, 0, Rf_mkChar("error"));
    SET_STRING_ELT(foldConditions, 1, Rf_mkChar("warning"));

#ifdef BCCOMPILE_STANDALONE
    {
	SEXP call = PROTECT(Rf_lang1(Rf_install("bcVersion")));
	bc_version = Rf_asInteger(eval_internal(call));
	UNPROTECT(1);
    }
#else
    bc_version = R_bcVersionNumber();
#endif

    for (int i = 0; foldFuns[i]; i++)
	set_flags(foldFuns[i], SI_FOLDFUN);
    for (int i = 0; languageFuns[i]; i++)
	set_flags(languageFuns[i], SI_LANGFUN);
    set_flags("pi", SI_CONSTNAME);
    set_flags("T", SI_CONSTNAME);
    set_flags("F", SI_CONSTNAME);
    set_flags("standardGeneric", SI_NOINLINE);
    set_flags("function", SI_LOOPSTOP | SI_SPECIALSYNTAX);
    set_flags("for", SI_LOOPSTOP | SI_SPECIALSYNTAX);
    set_flags("while", SI_LOOPSTOP);
    set_flags("repeat", SI_LOOPSTOP);
    set_flags("(", SI_LOOPTOP);
    set_flags("{", SI_LOOPTOP);
    set_flags("if", SI_LOOPTOP);
    set_flags("eval", SI_EVALFUN);
    set_flags("evalq", SI_EVALFUN);
    set_flags("source", SI_EVALFUN);
    set_flags("break", SI_BREAKNEXT);
    set_flags("next", SI_BREAKNEXT);
    set_flags("expression", SI_INITSHADOW);
    set_flags("local", SI_INITSHADOW);
    set_flags("quote", SI_INITSHADOW);
    set_flags("~", SI_SPECIALSYNTAX);
    set_flags("<-", SI_SPECIALSYNTAX);
    set_flags("=", SI_SPECIALSYNTAX);
    set_flags("bquote", SI_NSE);

    /* explicit handlers, in the order of cmp.R */
    set_handler("function", H_FUNCTION, PKG_BASE, 0);
    set_handler("{", H_BRACE, PKG_BASE, 0);
    set_handler("if", H_IF, PKG_BASE, 0);
    set_handler("&&", H_AND, PKG_BASE, 0);
    set_handler("||", H_OR, PKG_BASE, 0);
    set_handler("<-", H_ASSIGN, PKG_BASE, 0);
    set_handler("=", H_ASSIGN, PKG_BASE, 0);
    set_handler("<<-", H_ASSIGN, PKG_BASE, 0);
    set_handler("break", H_BREAK, PKG_BASE, 0);
    set_handler("next", H_NEXT, PKG_BASE, 0);
    set_handler("repeat", H_REPEAT, PKG_BASE, 0);
    set_handler("while", H_WHILE, PKG_BASE, 0);
    set_handler("for", H_FOR, PKG_BASE, 0);
    set_handler("+", H_PLUS, PKG_BASE, 0);
    set_handler("-", H_MINUS, PKG_BASE, 0);
    set_handler("*", H_PRIM2, PKG_BASE, MUL_OP);
    set_handler("/", H_PRIM2, PKG_BASE, DIV_OP);
    set_handler("^", H_PRIM2, PKG_BASE, EXPT_OP);
    set_handler("exp", H_PRIM1, PKG_BASE, EXP_OP);
    set_handler("sqrt", H_PRIM1, PKG_BASE, SQRT_OP);
    set_handler("log", H_LOG, PKG_BASE, 0);
    for (int i = 0; math1funs[i]; i++)
	set_handler(math1funs[i], H_MATH1, PKG_BASE, i);
    set_handler("==", H_PRIM2, PKG_BASE, EQ_OP);
    set_handler("!=", H_PRIM2, PKG_BASE, NE_OP);
    set_handler("<", H_PRIM2, PKG_BASE, LT_OP);
    set_handler("<=", H_PRIM2, PKG_BASE, LE_OP);
    set_handler(">=", H_PRIM2, PKG_BASE, GE_OP);
    set_handler(">", H_PRIM2, PKG_BASE, GT_OP);
    set_handler("&", H_PRIM2, PKG_BASE, AND_OP);
    set_handler("|", H_PRIM2, PKG_BASE, OR_OP);
    set_handler("!", H_PRIM1, PKG_BASE, NOT_OP);
    set_handler("(", H_PAREN, PKG_BASE, 0);
    set_handler(".Internal", H_DOTINTERNAL, PKG_BASE, 0);
    set_handler("$", H_DOLLAR, PKG_BASE, 0);
    set_handler("local", H_LOCAL, PKG_BASE, 0);
    set_handler("return", H_RETURN, PKG_BASE, 0);
    set_handler("is.character", H_IS, PKG_BASE, ISCHARACTER_OP);
    set_handler("is.complex", H_IS, PKG_BASE, ISCOMPLEX_OP);
    set_handler("is.double", H_IS, PKG_BASE, ISDOUBLE_OP);
    set_handler("is.integer", H_IS, PKG_BASE, ISINTEGER_OP);
    set_handler("is.logical", H_IS, PKG_BASE, ISLOGICAL_OP);
    set_handler("is.name", H_IS, PKG_BASE, ISSYMBOL_OP);
    set_handler("is.null", H_IS, PKG_BASE, ISNULL_OP);
    set_handler("is.object", H_IS, PKG_BASE, ISOBJECT_OP);
    set_handler("is.symbol", H_IS, PKG_BASE, ISSYMBOL_OP);

    /* default handlers for the BUILTINs and SPECIALs of base */
    {
	SEXP names = PROTECT(R_lsInternal3(R_BaseEnv, TRUE, FALSE));
	for (R_xlen_t i = 0; i < XLENGTH(names); i++) {
	    SEXP sym = Rf_installTrChar(STRING_ELT(names, i));
	    SEXP val = Rf_findVarInFrame3(R_BaseEnv, sym, TRUE);
	    if (TYPEOF(val) == PROMSXP)
		val = Rf_eval(val, R_BaseEnv);
	    int h = TYPEOF(val) == SPECIALSXP ? H_SPECIAL :
		TYPEOF(val) == BUILTINSXP ? H_BUILTIN : H_NONE;
	    if (h != H_NONE) {
		SymInfo *si = symtab_lookup(sym, TRUE);
		if (si->handler == H_NONE) {
		    si->handler = (unsigned char) h;
		    si->handlerPkg = PKG_BASE;
		}
	    }
	}
	UNPROTECT(1);
    }

    for (int i = 0; safeBaseInternals[i]; i++)
	set_handler(safeBaseInternals[i], H_SIMPLEINTERNAL, PKG_BASE, 0);
    for (int i = 0; safeStatsInternals[i]; i++)
	set_handler(safeStatsInternals[i], H_SIMPLEINTERNAL, PKG_STATS, 0);
    set_handler("switch", H_SWITCH, PKG_BASE, 0);
    set_handler(".Call", H_DOTCALL, PKG_BASE, 0);
    set_handler(":", H_PRIM2, PKG_BASE, COLON_OP);
    set_handler("seq_along", H_PRIM1, PKG_BASE, SEQALONG_OP);
    set_handler("seq_len", H_PRIM1, PKG_BASE, SEQLEN_OP);
    set_handler("::", H_MULTICOLON, PKG_BASE, 0);
    set_handler(":::", H_MULTICOLON, PKG_BASE, 0);
    set_handler("with", H_SUPPRESSUNDEF, PKG_BASE, 0);
    set_handler("require", H_SUPPRESSUNDEF, PKG_BASE, 0);
    set_handler("[", H_SUBSET, PKG_BASE, 0);
    set_handler("[[", H_SUBSET2, PKG_BASE, 0);

    set_setter("$<-", S_DOLLAR);
    set_setter("@<-", S_AT);
    set_setter("[<-", S_SUBASSIGN);
    set_setter("[[<-", S_SUBASSIGN2);
    set_getter("$", G_DOLLAR);
    set_getter("[", G_SUBSET);
    set_getter("[[", G_SUBSET2);
}

/* In cmp.R the compiler package's defaults for the base functions were
   computed when the package was built; the explicit handlers above are
   registered first and the defaults do not override them, as in
   setInlineHandler(). The safe internals and later handlers override
   the defaults as in cmp.R, where they are registered after the
   defaults. */


/*
 * Internals reached through .Internal() in a standalone build
 */

static SEXP eval_internal(SEXP call)
{
    SEXP icall = PROTECT(Rf_lang2(s_dotInternal, call));
    SEXP val = Rf_eval(icall, R_BaseEnv);
    UNPROTECT(1);
    return val;
}

static SEXP bc_mkcode(SEXP bytes, SEXP consts)
{
#ifdef BCCOMPILE_STANDALONE
    SEXP call = PROTECT(Rf_lang3(Rf_install("mkCode"), bytes, consts));
    SEXP val = eval_internal(call);
    UNPROTECT(1);
    return val;
#else
    SEXP ans = PROTECT(CONS(R_bcEncode(bytes), consts));
    SET_TYPEOF(ans, BCODESXP);
    R_registerBC(bytes, ans);
    UNPROTECT(1);
    return ans;
#endif
}

static int is_builtin_internal(SEXP sym)
{
    SymInfo *si = symtab_lookup(sym, TRUE);
    if (si->builtinInternal < 0) {
	signed char val;
#ifdef BCCOMPILE_STANDALONE
	SEXP qsym = PROTECT(Rf_lang2(R_QuoteSymbol, sym));
	SEXP call = PROTECT(Rf_lang2(Rf_install("is.builtin.internal"), qsym));
	val = Rf_asLogical(eval_internal(call)) == TRUE;
	UNPROTECT(2);
	si = symtab_lookup(sym, TRUE); /* the table may have grown */
#else
	SEXP i = INTERNAL(sym);
	val = (i != R_NilValue && TYPEOF(i) == BUILTINSXP);
#endif
	si->builtinInternal = val;
    }
    return si->builtinInternal;
}


/*
 * Small utilities on language objects
 */

/* The compiler runs in C for the whole of a function, so interrupts are
   polled every so many expressions visited, as the evaluator does. */
static int bc_tick_count = 0;

static R_INLINE void bc_tick(void)
{
    if (++bc_tick_count >= 1000) {
	bc_tick_count = 0;
	R_CheckUserInterrupt();
    }
}

static R_INLINE int lang_length(SEXP e)
{
    int n = 0;
    for (; e != R_NilValue; e = CDR(e)) {
	bc_tick();
	n++;
    }
    return n;
}

static R_INLINE SEXP lang_elt(SEXP e, int i) /* 0-based */
{
    while (i-- > 0)
	e = CDR(e);
    return CAR(e);
}

/* dots.or.missing(args) */
static int dots_or_missing(SEXP args)
{
    for (SEXP p = args; p != R_NilValue; p = CDR(p)) {
	bc_tick();
	if (CAR(p) == R_MissingArg || CAR(p) == R_DotsSymbol)
	    return TRUE;
    }
    return FALSE;
}

/* anyDots(args) */
static int any_dots(SEXP args)
{
    for (SEXP p = args; p != R_NilValue; p = CDR(p)) {
	bc_tick();
	if (CAR(p) == R_DotsSymbol)
	    return TRUE;
    }
    return FALSE;
}

/* ! is.null(names(e)) */
static int has_tags(SEXP e)
{
    for (SEXP p = e; p != R_NilValue; p = CDR(p)) {
	bc_tick();
	if (TAG(p) != R_NilValue)
	    return TRUE;
    }
    return FALSE;
}

/* is.ddsym(name): ..N */
static int is_ddsym(SEXP sym)
{
    const char *s;
    if (TYPEOF(sym) == SYMSXP)
	s = CHAR(PRINTNAME(sym));
    else if (TYPEOF(sym) == STRSXP && XLENGTH(sym) >= 1)
	s = CHAR(STRING_ELT(sym, 0));
    else
	return FALSE;
    if (s[0] != '.' || s[1] != '.' || s[2] == '\0')
	return FALSE;
    for (s += 2; *s; s++)
	if (*s < '0' || *s > '9')
	    return FALSE;
    return TRUE;
}


/* as.character(x) for a symbol or string used as a name */
static SEXP name_as_symbol(SEXP x)
{
    if (TYPEOF(x) == SYMSXP)
	return x;
    if (TYPEOF(x) == STRSXP && XLENGTH(x) == 1)
	return Rf_installTrChar(STRING_ELT(x, 0));
    return NULL;
}

/* the string name of a symbol or length-one character */
static const char *name_string(SEXP x)
{
    if (TYPEOF(x) == SYMSXP)
	return CHAR(PRINTNAME(x));
    if (TYPEOF(x) == STRSXP && XLENGTH(x) >= 1)
	return CHAR(STRING_ELT(x, 0));
    return "";
}

/* a shallow copy of a call: the spine and attributes, elements shared */
static SEXP copy_call(SEXP e)
{
    SEXP ans = PROTECT(Rf_allocList(lang_length(e)));
    SET_TYPEOF(ans, TYPEOF(e));
    SEXP p = ans;
    for (SEXP q = e; q != R_NilValue; q = CDR(q), p = CDR(p)) {
	SETCAR(p, CAR(q));
	SET_TAG(p, TAG(q));
    }
    SET_ATTRIB(ans, ATTRIB(e));
    SET_OBJECT(ans, OBJECT(e));
    UNPROTECT(1);
    return ans;
}

static SEXP set_lang_elt(SEXP e, int i, SEXP val) /* copy with e[[i+1]] <- val */
{
    SEXP ans = PROTECT(copy_call(e));
    SEXP p = ans;
    while (i-- > 0)
	p = CDR(p);
    SETCAR(p, val);
    UNPROTECT(1);
    return ans;
}


/*
 * Symbol sets: the variables of a compiler environment frame
 */

typedef struct {
    SEXP *tab;
    int size;	/* power of two, or 0 */
    int count;
} SymSet;

static void symset_init(SymSet *s)
{
    s->tab = NULL;
    s->size = 0;
    s->count = 0;
}

static int symset_has(const SymSet *s, SEXP sym)
{
    if (s->size == 0)
	return FALSE;
    unsigned h = ptr_hash(sym) & (s->size - 1);
    while (s->tab[h] != NULL) {
	if (s->tab[h] == sym)
	    return TRUE;
	h = (h + 1) & (s->size - 1);
    }
    return FALSE;
}

static void symset_add(SymSet *s, SEXP sym)
{
    if (2 * (s->count + 1) > s->size) {
	int osize = s->size;
	SEXP *otab = s->tab;
	s->size = osize == 0 ? 16 : 2 * osize;
	s->tab = (SEXP *) R_alloc(s->size, sizeof(SEXP));
	memset(s->tab, 0, s->size * sizeof(SEXP));
	for (int i = 0; i < osize; i++)
	    if (otab[i] != NULL) {
		unsigned h = ptr_hash(otab[i]) & (s->size - 1);
		while (s->tab[h] != NULL)
		    h = (h + 1) & (s->size - 1);
		s->tab[h] = otab[i];
	    }
    }
    unsigned h = ptr_hash(sym) & (s->size - 1);
    while (s->tab[h] != NULL) {
	if (s->tab[h] == sym)
	    return;
	h = (h + 1) & (s->size - 1);
    }
    s->tab[h] = sym;
    s->count++;
}


/*
 * Compiler environments (makeCenv, addCenvFrame, addCenvVars, findCenvVar)
 *
 * A frame is the base frame, holding the R environment the code will
 * be evaluated in, or a local frame added for a function being compiled.
 * Each frame has a set of variables the compiler knows may be bound in
 * it. The frame types of the environments on the chain from the base
 * environment are summarized by three counts as in frameTypes().
 */

enum { FT_LOCAL, FT_NAMESPACE, FT_GLOBAL };

typedef struct CEnv {
    struct CEnv *parent;	/* NULL for the base frame */
    SymSet vars;
    SEXP env;			/* NULL for a local frame */
    int nl, nn, ng;		/* base frame only */
} CEnv;

static CEnv *make_cenv(SEXP env)
{
    if (TYPEOF(env) != ENVSXP)
	Rf_error(_("not a proper evaluation environment"));
    CEnv *cenv = (CEnv *) R_alloc(1, sizeof(CEnv));
    cenv->parent = NULL;
    symset_init(&cenv->vars);
    cenv->env = env;

    /* frameTypes(env) */
    SEXP top = Rf_topenv(R_NilValue, env);
    SEXP e = env;
    int nl = 0, nn = 0, ng = 0;
    while (e != top) {
	if (R_IsNamespaceEnv(e))
	    Rf_error(_("namespace found within local environments"));
	if (e == R_EmptyEnv)
	    Rf_error(_("the empty environment has no parent"));
	e = ENCLOS(e);
	nl++;
	if (e == R_EmptyEnv)
	    Rf_error(_("not a proper evaluation environment"));
    }
    if (R_IsNamespaceEnv(e)) {
	while (e != R_GlobalEnv) {
	    if (! R_IsNamespaceEnv(e)) {
		SEXP name = Rf_getAttrib(e, R_NameSymbol);
		if (TYPEOF(name) != STRSXP || XLENGTH(name) < 1 ||
		    strncmp(CHAR(STRING_ELT(name, 0)), "imports:", 8) != 0)
		    Rf_error(_("non-namespace found within namespace environments"));
	    }
	    e = ENCLOS(e);
	    nn++;
	    if (e == R_EmptyEnv)
		Rf_error(_("not a proper evaluation environment"));
	}
    }
    while (e != R_EmptyEnv) {
	if (R_IsNamespaceEnv(e))
	    Rf_error(_("namespace found within global environments"));
	e = ENCLOS(e);
	ng++;
    }
    cenv->nl = nl;
    cenv->nn = nn;
    cenv->ng = ng;
    return cenv;
}

static CEnv *add_cenv_frame(CEnv *parent)
{
    CEnv *cenv = (CEnv *) R_alloc(1, sizeof(CEnv));
    cenv->parent = parent;
    symset_init(&cenv->vars);
    cenv->env = NULL;
    cenv->nl = cenv->nn = cenv->ng = 0;
    return cenv;
}

static R_INLINE void add_cenv_var(CEnv *cenv, SEXP sym)
{
    symset_add(&cenv->vars, sym);
}

static R_INLINE CEnv *cenv_base(CEnv *cenv)
{
    while (cenv->parent != NULL)
	cenv = cenv->parent;
    return cenv;
}

typedef struct {
    int found;
    SEXP frame;		/* NULL for a local frame added by the compiler */
    int ftype;
} VarInfo;

static VarInfo find_cenv_var(SEXP sym, CEnv *cenv)
{
    VarInfo info = { FALSE, NULL, FT_LOCAL };
    int depth = 0;	/* number of compiler frames passed */
    CEnv *c = cenv;
    for (; c->parent != NULL; c = c->parent, depth++)
	if (symset_has(&c->vars, sym)) {
	    info.found = TRUE;
	    return info;
	}
    /* c is now the base frame */
    SEXP env = c->env;
    int d = 0;	/* depth in the environment chain from the base env */
    if (symset_has(&c->vars, sym) || R_existsVarInFrame(env, sym)) {
	info.found = TRUE;
	info.frame = env;
    }
    else {
	for (env = ENCLOS(env), d = 1; env != R_EmptyEnv;
	     env = ENCLOS(env), d++)
	    if (R_existsVarInFrame(env, sym)) {
		info.found = TRUE;
		info.frame = env;
		break;
	    }
	if (! info.found)
	    return info;
    }
    if (d < c->nl)
	info.ftype = FT_LOCAL;
    else if (d < c->nl + c->nn)
	info.ftype = FT_NAMESPACE;
    else
	info.ftype = FT_GLOBAL;
    return info;
}

/* the value in info$value: get(var, frame, inherits = FALSE), or
   R_UnboundValue when findCenvVar() would not have provided one */
static SEXP var_info_value(const VarInfo *info, SEXP sym)
{
    if (info->frame == NULL || sym == R_DotsSymbol ||
	! R_existsVarInFrame(info->frame, sym))
	return R_UnboundValue;
    SEXP val = Rf_findVarInFrame3(info->frame, sym, TRUE);
    if (TYPEOF(val) == PROMSXP) {
	PROTECT(val);
	val = Rf_eval(val, R_EmptyEnv);
	UNPROTECT(1);
    }
    if (val == R_MissingArg)
	Rf_error(_("argument \"%s\" is missing, with no default"),
		 CHAR(PRINTNAME(sym)));
    return val;
}

/* findLocVar */
static int find_loc_var(SEXP sym, CEnv *cenv)
{
    VarInfo info = find_cenv_var(sym, cenv);
    return info.found && info.ftype == FT_LOCAL;
}

/* findVar */
static int find_var(SEXP sym, CEnv *cenv)
{
    return find_cenv_var(sym, cenv).found;
}

/* findFunDef: the function bound to the variable, or NULL */
static SEXP find_fun_def(SEXP sym, CEnv *cenv)
{
    VarInfo info = find_cenv_var(sym, cenv);
    if (! info.found)
	return NULL;
    SEXP val = var_info_value(&info, sym);
    if (val == R_UnboundValue || ! Rf_isFunction(val))
	return NULL;
    return val;
}


/*
 * Namespaces (findHomeNS, nsName, packFrameName)
 */

static SEXP get_namespace(SEXP name) /* getNamespace(name), name a CHARSXP */
{
    SEXP arg = PROTECT(Rf_ScalarString(name));
    SEXP ns = R_FindNamespace(arg);
    UNPROTECT(1);
    return ns;
}

static SEXP find_home_ns(SEXP sym, SEXP ns)
{
    if (R_existsVarInFrame(ns, sym))
	return ns;
    SEXP nsinfo = Rf_findVarInFrame3(ns, s_NAMESPACE, TRUE);
    if (nsinfo == R_UnboundValue || TYPEOF(nsinfo) != ENVSXP)
	return NULL;
    SEXP imports = Rf_findVarInFrame3(nsinfo, s_imports, TRUE);
    if (imports == R_UnboundValue || TYPEOF(imports) != VECSXP)
	return NULL;
    PROTECT(imports);
    SEXP inames = Rf_getAttrib(imports, R_NamesSymbol);
    if (TYPEOF(inames) != STRSXP || XLENGTH(inames) != XLENGTH(imports)) {
	UNPROTECT(1);
	return NULL;
    }
    const char *symname = CHAR(PRINTNAME(sym));
    for (R_xlen_t i = XLENGTH(imports) - 1; i >= 0; i--) {
	SEXP ins = PROTECT(get_namespace(STRING_ELT(inames, i)));
	SEXP imp = VECTOR_ELT(imports, i);
	if (TYPEOF(imp) == LGLSXP && XLENGTH(imp) == 1 &&
	    LOGICAL(imp)[0] == TRUE && ATTRIB(imp) == R_NilValue) {
	    SEXP exports;
	    if (ins == R_BaseNamespace) {
		exports = R_BaseNamespace;
	    }
	    else {
		SEXP iinfo = Rf_findVarInFrame3(ins, s_NAMESPACE, TRUE);
		exports = TYPEOF(iinfo) == ENVSXP ?
		    Rf_findVarInFrame3(iinfo, s_exports, TRUE) : R_UnboundValue;
	    }
	    if (TYPEOF(exports) == ENVSXP && R_existsVarInFrame(exports, sym)) {
		SEXP ans = find_home_ns(sym, ins);
		UNPROTECT(2);
		return ans;
	    }
	}
	else {
	    SEXP enames = Rf_getAttrib(imp, R_NamesSymbol);
	    if (TYPEOF(enames) == STRSXP && TYPEOF(imp) == STRSXP) {
		R_xlen_t pos = -1;
		for (R_xlen_t j = 0; j < XLENGTH(enames); j++)
		    if (strcmp(CHAR(STRING_ELT(enames, j)), symname) == 0) {
			pos = j;
			break;
		    }
		if (pos >= 0) {
		    SEXP ans = NULL;
		    if (pos < XLENGTH(imp) &&
			strcmp(CHAR(STRING_ELT(imp, pos)), symname) == 0)
			ans = find_home_ns(sym, ins);
		    UNPROTECT(2);
		    return ans;
		}
	    }
	}
	UNPROTECT(1);
    }
    UNPROTECT(1);
    return NULL;
}

/* nsName(ns): "base", the namespace's name, or "" */
static const char *ns_name(SEXP ns)
{
    if (ns == NULL)
	return "";
    if (ns == R_BaseNamespace)
	return "base";
    SEXP nsinfo = Rf_findVarInFrame3(ns, s_NAMESPACE, TRUE);
    if (TYPEOF(nsinfo) != ENVSXP)
	return "";
    SEXP spec = Rf_findVarInFrame3(nsinfo, s_spec, TRUE);
    if (TYPEOF(spec) != STRSXP)
	return "";
    SEXP snames = Rf_getAttrib(spec, R_NamesSymbol);
    if (TYPEOF(snames) != STRSXP)
	return "";
    for (R_xlen_t i = 0; i < XLENGTH(snames); i++)
	if (strcmp(CHAR(STRING_ELT(snames, i)), "name") == 0)
	    return CHAR(STRING_ELT(spec, i));
    return "";
}

/* packFrameName(frame): the package name of a search path frame */
static const char *pack_frame_name(SEXP frame)
{
    SEXP fname = Rf_getAttrib(frame, R_NameSymbol);
    if (TYPEOF(fname) == STRSXP && XLENGTH(fname) >= 1) {
	const char *s = CHAR(STRING_ELT(fname, 0));
	const char *p = strstr(s, "package:");
	if (p == NULL)
	    return s;
	/* sub("package:", "", fname): remove the first occurrence */
	size_t n = strlen(s) - 8;
	char *buf = R_alloc(n + 1, 1);
	memcpy(buf, s, p - s);
	strcpy(buf + (p - s), p + 8);
	return buf;
    }
    else if (frame == R_BaseEnv) {
	return "base";
    }
    else {
	return "";
    }
}

static int package_code(const char *name)
{
    if (strcmp(name, "base") == 0)
	return PKG_BASE;
    if (strcmp(name, "stats") == 0)
	return PKG_STATS;
    return PKG_NONE;
}


/*
 * Compiler contexts
 */

typedef struct {
    int toplevel;
    int tailcall;
    int needRETURNJMP;
    CEnv *env;
    int optimize;
    int suppressAll;
    int suppressNoSuperAssignVar;
    SEXP suppressUndefined;	/* TRUE, FALSE, a character vector or NULL */
    SEXP call;
    int hasLoop;
    int loopLabel, endLabel, gotoOK;
} Cntxt;

static SEXP compiler_option(const char *name, SEXP options);

static Cntxt make_toplevel_context(CEnv *cenv, SEXP options)
{
    Cntxt c;
    c.toplevel = TRUE;
    c.tailcall = TRUE;
    c.needRETURNJMP = FALSE;
    c.env = cenv;
    c.optimize = Rf_asInteger(compiler_option("optimize", options));
    if (c.optimize == NA_INTEGER)
	c.optimize = 2;
    c.suppressAll = Rf_asLogical(compiler_option("suppressAll", options)) == TRUE;
    c.suppressNoSuperAssignVar =
	Rf_asLogical(compiler_option("suppressNoSuperAssignVar", options)) == TRUE;
    c.suppressUndefined = compiler_option("suppressUndefined", options);
    c.call = R_NilValue;
    c.hasLoop = FALSE;
    c.loopLabel = c.endLabel = -1;
    c.gotoOK = FALSE;
    return c;
}

static R_INLINE Cntxt make_call_context(const Cntxt *cntxt, SEXP call)
{
    Cntxt c = *cntxt;
    c.call = call;
    return c;
}

static R_INLINE Cntxt make_promise_context(const Cntxt *cntxt)
{
    Cntxt c = *cntxt;
    c.toplevel = FALSE;
    c.tailcall = TRUE;
    c.needRETURNJMP = TRUE;
    if (c.hasLoop)
	c.gotoOK = FALSE;
    return c;
}

static R_INLINE Cntxt make_nontailcall_context(const Cntxt *cntxt)
{
    Cntxt c = *cntxt;
    c.tailcall = FALSE;
    return c;
}

static R_INLINE Cntxt make_arg_context(const Cntxt *cntxt)
{
    Cntxt c = *cntxt;
    c.toplevel = FALSE;
    c.tailcall = FALSE;
    if (c.hasLoop)
	c.gotoOK = FALSE;
    return c;
}

static R_INLINE Cntxt make_novalue_context(const Cntxt *cntxt)
{
    Cntxt c = *cntxt;
    c.tailcall = FALSE;
    return c;
}

static R_INLINE Cntxt make_loop_context(const Cntxt *cntxt, int loopLabel,
					int endLabel)
{
    Cntxt c = make_novalue_context(cntxt);
    c.hasLoop = TRUE;
    c.loopLabel = loopLabel;
    c.endLabel = endLabel;
    c.gotoOK = TRUE;
    return c;
}

static void find_locals_list(SEXP *elist, int n, Cntxt *cntxt, SymSet *out);

/* make.functionContext: the environment is augmented with the formals
   and the local variables of the body, and the context reset */
static Cntxt make_function_context(const Cntxt *cntxt, SEXP forms, SEXP body)
{
    Cntxt fcntxt = *cntxt;
    fcntxt.env = add_cenv_frame(cntxt->env);
    /* a function literal built by code may have anything as its
       formals (names(forms) and c(forms, body) in the R compiler) */
    int nforms;
    if (TYPEOF(forms) == LISTSXP || TYPEOF(forms) == LANGSXP) {
	nforms = lang_length(forms);
	for (SEXP f = forms; f != R_NilValue; f = CDR(f))
	    if (TAG(f) != R_NilValue)
		add_cenv_var(fcntxt.env, TAG(f));
    }
    else if (TYPEOF(forms) == VECSXP) {
	nforms = (int) XLENGTH(forms);
	SEXP names = Rf_getAttrib(forms, R_NamesSymbol);
	if (TYPEOF(names) == STRSXP)
	    for (int i = 0; i < nforms; i++)
		if (CHAR(STRING_ELT(names, i))[0] != '\0')
		    add_cenv_var(fcntxt.env,
				 Rf_installTrChar(STRING_ELT(names, i)));
    }
    else
	nforms = 0;

    /* findLocalsList(c(forms, body), cntxt) */
    int n = nforms + 1;
    SEXP *elist = (SEXP *) R_alloc(n, sizeof(SEXP));
    int i = 0;
    if (TYPEOF(forms) == LISTSXP || TYPEOF(forms) == LANGSXP)
	for (SEXP f = forms; f != R_NilValue; f = CDR(f))
	    elist[i++] = CAR(f);
    else if (TYPEOF(forms) == VECSXP)
	for (; i < nforms; i++)
	    elist[i] = VECTOR_ELT(forms, i);
    elist[i] = body;
    find_locals_list(elist, n, &fcntxt, &fcntxt.env->vars);

    Cntxt ncntxt = make_toplevel_context(fcntxt.env, R_NilValue);
    ncntxt.optimize = cntxt->optimize;
    ncntxt.suppressAll = cntxt->suppressAll;
    ncntxt.suppressNoSuperAssignVar = cntxt->suppressNoSuperAssignVar;
    ncntxt.suppressUndefined = cntxt->suppressUndefined;
    return ncntxt;
}


/*
 * Compiler options
 */

static SEXP compiler_options_env(void)
{
    /* the compiler package's option environment, when the package is
       loaded; its defaults are used otherwise */
    SEXP ns = Rf_findVarInFrame3(R_NamespaceRegistry, s_compiler, TRUE);
    if (TYPEOF(ns) != ENVSXP)
	return NULL;
    SEXP env = Rf_findVarInFrame3(ns, s_compilerOptions, TRUE);
    if (TYPEOF(env) == PROMSXP) {
	PROTECT(env);
	env = Rf_eval(env, R_EmptyEnv);
	UNPROTECT(1);
    }
    return TYPEOF(env) == ENVSXP ? env : NULL;
}

/* the R_COMPILER_* environment variables, read as the compiler package's
   .onLoad() reads them; -1 when unset or invalid */
static int env_option_logical(const char *var)
{
    const char *val = getenv(var);
    if (val == NULL)
	return -1;
    static const char *yes[] = { "1", "TRUE", "true", "True", "yes", "Yes", NULL };
    static const char *no[] = { "0", "FALSE", "false", "False", "no", "No", NULL };
    for (int i = 0; yes[i]; i++)
	if (strcmp(val, yes[i]) == 0)
	    return TRUE;
    for (int i = 0; no[i]; i++)
	if (strcmp(val, no[i]) == 0)
	    return FALSE;
    return -1;
}

static int env_option_optimize(void)
{
    const char *val = getenv("R_COMPILER_OPTIMIZE");
    if (val == NULL || val[0] == '\0')
	return -1;
    char *end;
    long lev = strtol(val, &end, 10);
    if (*end != '\0' || lev < 0 || lev > 3)
	return -1;
    return (int) lev;
}

static SEXP compiler_option(const char *name, SEXP options)
{
    if (TYPEOF(options) == VECSXP) {
	SEXP names = Rf_getAttrib(options, R_NamesSymbol);
	if (TYPEOF(names) == STRSXP)
	    for (R_xlen_t i = 0; i < XLENGTH(names); i++)
		if (strcmp(CHAR(STRING_ELT(names, i)), name) == 0)
		    return VECTOR_ELT(options, i);
    }
    SEXP env = compiler_options_env();
    if (env != NULL) {
	SEXP val = Rf_findVarInFrame3(env, Rf_install(name), TRUE);
	if (val != R_UnboundValue) {
	    if (TYPEOF(val) == PROMSXP) {
		PROTECT(val);
		val = Rf_eval(val, R_EmptyEnv);
		UNPROTECT(1);
	    }
	    return val;
	}
    }
    /* the package's defaults, and the environment variables its
       .onLoad() applies */
    if (strcmp(name, "optimize") == 0) {
	int lev = env_option_optimize();
	return Rf_ScalarInteger(lev < 0 ? 2 : lev);
    }
    else if (strcmp(name, "suppressAll") == 0) {
	int v = env_option_logical("R_COMPILER_SUPPRESS_ALL");
	return v == FALSE ? R_FalseValue : R_TrueValue;
    }
    else if (strcmp(name, "suppressNoSuperAssignVar") == 0) {
	int v = env_option_logical("R_COMPILER_SUPPRESS_NO_SUPER_ASSIGN_VAR");
	return v == TRUE ? R_TrueValue : R_FalseValue;
    }
    else if (strcmp(name, "suppressUndefined") == 0) {
	int v = env_option_logical("R_COMPILER_SUPPRESS_UNDEFINED");
	if (v == TRUE)
	    return R_TrueValue;
	if (v == FALSE)
	    return R_FalseValue;
	static SEXP dflt = NULL;
	if (dflt == NULL) {
	    dflt = Rf_allocVector(STRSXP, 4);
	    R_PreserveObject(dflt);
	    SET_STRING_ELT(dflt, 0, Rf_mkChar(".Generic"));
	    SET_STRING_ELT(dflt, 1, Rf_mkChar(".Method"));
	    SET_STRING_ELT(dflt, 2, Rf_mkChar(".Random.seed"));
	    SET_STRING_ELT(dflt, 3, Rf_mkChar(".self"));
	}
	return dflt;
    }
    return R_NilValue;
}


/*
 * Source references
 */

/* extractSrcref(sref, idx), idx 1-based */
static SEXP extract_srcref(SEXP sref, int idx)
{
    if (TYPEOF(sref) == VECSXP) {
	if (XLENGTH(sref) >= idx)
	    return VECTOR_ELT(sref, idx - 1);
    }
    else if (TYPEOF(sref) == LISTSXP) {
	if (lang_length(sref) >= idx)
	    return lang_elt(sref, idx - 1);
    }
    else if (TYPEOF(sref) == INTSXP && XLENGTH(sref) >= 6) {
	return sref;
    }
    return R_NilValue;
}

static R_INLINE SEXP srcref_attr(SEXP e)
{
    if (e == R_NilValue || ATTRIB(e) == R_NilValue)
	return R_NilValue;
    return Rf_getAttrib(e, s_srcref);
}

/* getExprSrcref(expr) */
static R_INLINE SEXP expr_srcref(SEXP e)
{
    return extract_srcref(srcref_attr(e), 1);
}


/*
 * Code buffers (make.codeBuf)
 *
 * The code is accumulated as integers; a word that refers to a label
 * holds the label number, a word that refers to a list of labels (the
 * switch instruction) the index of the list, with the kind of each word
 * recorded alongside. patch_labels() resolves them as in patchlabels().
 * The constant pool is a generic vector, with a hash table over the
 * constants so that putconst() need not scan the pool.
 */

#define WK_INT 0
#define WK_LABEL 1
#define WK_LABELLIST 2

typedef struct {
    int *code;
    unsigned char *kind;
    int *exprbuf;
    int *srcrefbuf;
    int ncode, codecap;

    int exprTracking, srcrefTracking;
    SEXP curExpr, curSrcref;

    SEXP consts;
    PROTECT_INDEX cpi;
    int nconst, constcap;
    unsigned *chash;	/* hash of each constant */
    int *cnext;		/* chain links, -1 ends */
    int *cheads;	/* bucket heads, -1 empty */
    int nbuckets;

    int *labelpos;
    int nlabels, labelcap;

    int **lists;
    int *listlen;
    int nlists, listcap;
} CodeBuf;

typedef struct {
    SEXP expr, srcref;
} Loc;

static int *grow_ints(int *p, int oldn, int newn)
{
    int *q = (int *) R_alloc(newn, sizeof(int));
    if (oldn > 0)
	memcpy(q, p, oldn * sizeof(int));
    return q;
}

static R_INLINE unsigned hash_mix(unsigned h, unsigned v)
{
    return (h ^ v) * 0x9e3779b1u;
}

/* consistent with the num.eq comparison of identical(): 0 and -0 agree,
   as do all NaN payloads other than NA */
static R_INLINE unsigned hash_double(double d)
{
    if (d == 0) {
	return 0;
    }
    else if (R_IsNA(d)) {
	return 1;
    }
    else if (ISNAN(d)) {
	return 2;
    }
    else {
	uint64_t bits;
	memcpy(&bits, &d, sizeof bits);
	return (unsigned) (bits ^ (bits >> 32));
    }
}

static R_INLINE unsigned hash_charsxp(SEXP s)
{
    /* Seql() compares translated strings: strings in different
       encodings may be equal, so only an ASCII prefix is hashed, and
       the length only for a string that is ASCII throughout */
    if (s == NA_STRING)
	return 1;
    const char *p = CHAR(s);
    unsigned h = 7;
    int i;
    for (i = 0; i < 32 && p[i]; i++) {
	if ((unsigned char) p[i] >= 0x80)
	    return h;
	h = hash_mix(h, (unsigned char) p[i]);
    }
    if (p[i] == '\0')
	h = hash_mix(h, (unsigned) i + 1);
    return h;
}

#define CONST_HASH_WIDTH 8
#define CONST_HASH_MAX_STRINGS 64

/* Constants that R_compute_identical(x, y, 16) considers identical must
   hash alike: attributes, argument tags and NaN payloads are left out. */
static unsigned const_hash(SEXP x, int depth)
{
    unsigned h = hash_mix(2166136261u, (unsigned) TYPEOF(x));
    if (OBJECT(x))
	h = hash_mix(h, 1);

    switch (TYPEOF(x)) {
    case SYMSXP:
    case ENVSXP:
    case WEAKREFSXP:
	h = hash_mix(h, ptr_hash(x));
	break;
    case LGLSXP: {
	R_xlen_t n = XLENGTH(x);
	h = hash_mix(h, (unsigned) n);
	for (R_xlen_t i = 0; i < n && i < CONST_HASH_WIDTH; i++)
	    h = hash_mix(h, (unsigned) LOGICAL_ELT(x, i));
	break;
    }
    case INTSXP: {
	R_xlen_t n = XLENGTH(x);
	h = hash_mix(h, (unsigned) n);
	for (R_xlen_t i = 0; i < n && i < CONST_HASH_WIDTH; i++)
	    h = hash_mix(h, (unsigned) INTEGER_ELT(x, i));
	break;
    }
    case REALSXP: {
	R_xlen_t n = XLENGTH(x);
	h = hash_mix(h, (unsigned) n);
	for (R_xlen_t i = 0; i < n && i < CONST_HASH_WIDTH; i++)
	    h = hash_mix(h, hash_double(REAL_ELT(x, i)));
	break;
    }
    case CPLXSXP: {
	R_xlen_t n = XLENGTH(x);
	h = hash_mix(h, (unsigned) n);
	for (R_xlen_t i = 0; i < n && i < CONST_HASH_WIDTH; i++) {
	    Rcomplex z = COMPLEX_ELT(x, i);
	    h = hash_mix(h, hash_double(z.r));
	    h = hash_mix(h, hash_double(z.i));
	}
	break;
    }
    case STRSXP: {
	/* STRING_ELT() on a deferred string (as.character() of a long
	   sequence) expands the whole vector, so a long character vector
	   is hashed by its length alone */
	R_xlen_t n = XLENGTH(x);
	h = hash_mix(h, (unsigned) n);
	if (n <= CONST_HASH_MAX_STRINGS)
	    for (R_xlen_t i = 0; i < n && i < CONST_HASH_WIDTH; i++)
		h = hash_mix(h, hash_charsxp(STRING_ELT(x, i)));
	break;
    }
    case RAWSXP: {
	R_xlen_t n = XLENGTH(x);
	h = hash_mix(h, (unsigned) n);
	for (R_xlen_t i = 0; i < n && i < CONST_HASH_WIDTH; i++)
	    h = hash_mix(h, RAW_ELT(x, i));
	break;
    }
    case VECSXP:
    case EXPRSXP: {
	R_xlen_t n = XLENGTH(x);
	h = hash_mix(h, (unsigned) n);
	if (depth > 0)
	    for (R_xlen_t i = 0; i < n && i < CONST_HASH_WIDTH; i++)
		h = hash_mix(h, const_hash(VECTOR_ELT(x, i), depth - 1));
	break;
    }
    case LANGSXP:
    case LISTSXP: {
	int n = 0;
	for (SEXP p = x; p != R_NilValue && n < CONST_HASH_WIDTH; p = CDR(p))
	    n++;
	h = hash_mix(h, (unsigned) n);
	if (depth > 0) {
	    int i = 0;
	    for (SEXP p = x; p != R_NilValue && i < CONST_HASH_WIDTH;
		 p = CDR(p), i++)
		h = hash_mix(h, const_hash(CAR(p), depth - 1));
	}
	break;
    }
    case BCODESXP:
	if (depth > 0) {
	    h = hash_mix(h, const_hash(CAR(x), depth - 1));
	    h = hash_mix(h, const_hash(CDR(x), depth - 1));
	}
	break;
    case CLOSXP:
	if (depth > 0)
	    h = hash_mix(h, const_hash(FORMALS(x), depth - 1));
	h = hash_mix(h, ptr_hash(CLOENV(x)));
	break;
    default:
	break;
    }

    h ^= h >> 16;
    h *= 0x85ebca6bu;
    h ^= h >> 13;
    h *= 0xc2b2ae35u;
    h ^= h >> 16;
    return h;
}

#define CONST_HASH_DEPTH 3

static void rebuild_const_index(CodeBuf *cb)
{
    int nb = 32;
    while (nb < 2 * cb->constcap)
	nb *= 2;
    cb->nbuckets = nb;
    cb->cheads = (int *) R_alloc(nb, sizeof(int));
    for (int i = 0; i < nb; i++)
	cb->cheads[i] = -1;
    for (int i = 0; i < cb->nconst; i++) {
	unsigned b = cb->chash[i] & (unsigned) (nb - 1);
	cb->cnext[i] = cb->cheads[b];
	cb->cheads[b] = i;
    }
}

/* cb$putconst(x): the index of x in the constant pool, adding it if no
   identical constant is there */
static int putconst(CodeBuf *cb, SEXP x)
{
    unsigned h = const_hash(x, CONST_HASH_DEPTH);
    unsigned b = h & (unsigned) (cb->nbuckets - 1);
    for (int j = cb->cheads[b]; j >= 0; j = cb->cnext[j]) {
	SEXP y = VECTOR_ELT(cb->consts, j);
	/* 16 - take closure environments into account, this is necessary
	        as closures (closure literals) can get into the AST when
	        the AST is generated by a program (e.g. distr package) */
	if (x == y || (cb->chash[j] == h && R_compute_identical(x, y, 16)))
	    return j;
    }

    if (cb->nconst == cb->constcap) {
	int ncap = 2 * cb->constcap;
	PROTECT(x);
	SEXP nconsts = Rf_allocVector(VECSXP, ncap);
	for (int i = 0; i < cb->nconst; i++)
	    SET_VECTOR_ELT(nconsts, i, VECTOR_ELT(cb->consts, i));
	REPROTECT(cb->consts = nconsts, cb->cpi);
	UNPROTECT(1);
	cb->constcap = ncap;
	unsigned *nh = (unsigned *) R_alloc(ncap, sizeof(unsigned));
	memcpy(nh, cb->chash, cb->nconst * sizeof(unsigned));
	cb->chash = nh;
	cb->cnext = grow_ints(cb->cnext, cb->nconst, ncap);
	rebuild_const_index(cb);
	b = h & (unsigned) (cb->nbuckets - 1);
    }
    int i = cb->nconst++;
    SET_VECTOR_ELT(cb->consts, i, x);
    cb->chash[i] = h;
    cb->cnext[i] = cb->cheads[b];
    cb->cheads[b] = i;
    return i;
}

static void ensure_code(CodeBuf *cb, int n)
{
    if (cb->ncode + n <= cb->codecap)
	return;
    int ncap = cb->codecap;
    while (cb->ncode + n > ncap)
	ncap *= 2;
    cb->code = grow_ints(cb->code, cb->ncode, ncap);
    unsigned char *nk = (unsigned char *) R_alloc(ncap, 1);
    memcpy(nk, cb->kind, cb->ncode);
    cb->kind = nk;
    if (cb->exprTracking)
	cb->exprbuf = grow_ints(cb->exprbuf, cb->ncode, ncap);
    if (cb->srcrefTracking)
	cb->srcrefbuf = grow_ints(cb->srcrefbuf, cb->ncode, ncap);
    cb->codecap = ncap;
}

/* cb$putcode(...): append words and record the current location */
static void putcode(CodeBuf *cb, int n, const int *words,
		    const unsigned char *kinds)
{
    ensure_code(cb, n);
    int start = cb->ncode;
    for (int i = 0; i < n; i++) {
	cb->code[start + i] = words[i];
	cb->kind[start + i] = kinds == NULL ? WK_INT : kinds[i];
    }
    if (cb->exprTracking) {
	int ei = putconst(cb, cb->curExpr);
	for (int i = 0; i < n; i++)
	    cb->exprbuf[start + i] = ei;
    }
    if (cb->srcrefTracking) {
	int si = putconst(cb, cb->curSrcref);
	for (int i = 0; i < n; i++)
	    cb->srcrefbuf[start + i] = si;
    }
    cb->ncode += n;
}

static R_INLINE void put0(CodeBuf *cb, int op)
{
    putcode(cb, 1, &op, NULL);
}

static R_INLINE void put1(CodeBuf *cb, int op, int a)
{
    int w[2] = { op, a };
    putcode(cb, 2, w, NULL);
}

static R_INLINE void put2(CodeBuf *cb, int op, int a, int b)
{
    int w[3] = { op, a, b };
    putcode(cb, 3, w, NULL);
}


static R_INLINE void putL(CodeBuf *cb, int op, int label)
{
    int w[2] = { op, label };
    unsigned char k[2] = { WK_INT, WK_LABEL };
    putcode(cb, 2, w, k);
}

static R_INLINE void put1L(CodeBuf *cb, int op, int a, int label)
{
    int w[3] = { op, a, label };
    unsigned char k[3] = { WK_INT, WK_INT, WK_LABEL };
    putcode(cb, 3, w, k);
}

static R_INLINE void put2L(CodeBuf *cb, int op, int a, int b, int label)
{
    int w[4] = { op, a, b, label };
    unsigned char k[4] = { WK_INT, WK_INT, WK_INT, WK_LABEL };
    putcode(cb, 4, w, k);
}

static int makelabel(CodeBuf *cb)
{
    if (cb->nlabels == cb->labelcap) {
	int ncap = 2 * cb->labelcap;
	cb->labelpos = grow_ints(cb->labelpos, cb->nlabels, ncap);
	cb->labelcap = ncap;
    }
    cb->labelpos[cb->nlabels] = -1;
    return cb->nlabels++;
}

static R_INLINE void putlabel(CodeBuf *cb, int label)
{
    cb->labelpos[label] = cb->ncode;
}

/* register a list of labels for a switch instruction */
static int make_labellist(CodeBuf *cb, const int *labels, int n)
{
    if (cb->nlists == cb->listcap) {
	int ncap = cb->listcap == 0 ? 4 : 2 * cb->listcap;
	int **nl = (int **) R_alloc(ncap, sizeof(int *));
	if (cb->nlists > 0)
	    memcpy(nl, cb->lists, cb->nlists * sizeof(int *));
	cb->lists = nl;
	cb->listlen = grow_ints(cb->listlen, cb->nlists, ncap);
	cb->listcap = ncap;
    }
    int *copy = (int *) R_alloc(n > 0 ? n : 1, sizeof(int));
    memcpy(copy, labels, n * sizeof(int));
    cb->lists[cb->nlists] = copy;
    cb->listlen[cb->nlists] = n;
    return cb->nlists++;
}

/* cb$setcurexpr(expr): also update the srcref if expr has one */
static R_INLINE void setcurexpr(CodeBuf *cb, SEXP expr)
{
    if (cb->exprTracking)
	cb->curExpr = expr;
    if (cb->srcrefTracking) {
	SEXP sref = expr_srcref(expr);
	if (sref != R_NilValue)
	    cb->curSrcref = sref;
    }
}

/* cb$setcurloc(expr, sref) */
static R_INLINE void setcurloc(CodeBuf *cb, SEXP expr, SEXP sref)
{
    if (cb->exprTracking)
	cb->curExpr = expr;
    if (cb->srcrefTracking)
	cb->curSrcref = sref;
}

static R_INLINE Loc savecurloc(CodeBuf *cb)
{
    Loc loc = { cb->curExpr, cb->curSrcref };
    return loc;
}

static R_INLINE void restorecurloc(CodeBuf *cb, Loc loc)
{
    if (cb->exprTracking)
	cb->curExpr = loc.expr;
    if (cb->srcrefTracking)
	cb->curSrcref = loc.srcref;
}

/* make.codeBuf(expr, loc); the constant pool is protected until
   codebuf_code() or discard_codebuf() */
static void make_codebuf(CodeBuf *cb, SEXP expr, const Loc *loc)
{
    cb->exprTracking = TRUE;
    cb->srcrefTracking = TRUE;
    if (loc == NULL) {
	cb->curExpr = expr;
	cb->curSrcref = expr_srcref(expr);
    }
    else {
	cb->curExpr = loc->expr;
	cb->curSrcref = loc->srcref;
    }
    if (cb->curSrcref == R_NilValue)
	/* when the top-level srcref is null, we speculate there will be
	   no source references within the compiled expressions either;
	   disabling the tracking makes the resulting constant pool
	   smaller */
	cb->srcrefTracking = FALSE;
    if (! cb->srcrefTracking)
	cb->curSrcref = R_NilValue;

    cb->codecap = 64;
    cb->code = (int *) R_alloc(cb->codecap, sizeof(int));
    cb->kind = (unsigned char *) R_alloc(cb->codecap, 1);
    cb->exprbuf = (int *) R_alloc(cb->codecap, sizeof(int));
    cb->srcrefbuf = cb->srcrefTracking ?
	(int *) R_alloc(cb->codecap, sizeof(int)) : NULL;
    cb->code[0] = bc_version;
    cb->kind[0] = WK_INT;
    cb->exprbuf[0] = NA_INTEGER;
    if (cb->srcrefTracking)
	cb->srcrefbuf[0] = NA_INTEGER;
    cb->ncode = 1;

    cb->constcap = 32;
    cb->nconst = 0;
    PROTECT_WITH_INDEX(cb->consts = Rf_allocVector(VECSXP, cb->constcap),
		       &cb->cpi);
    cb->chash = (unsigned *) R_alloc(cb->constcap, sizeof(unsigned));
    cb->cnext = (int *) R_alloc(cb->constcap, sizeof(int));
    rebuild_const_index(cb);

    cb->labelcap = 16;
    cb->labelpos = (int *) R_alloc(cb->labelcap, sizeof(int));
    cb->nlabels = 0;
    cb->lists = NULL;
    cb->listlen = NULL;
    cb->nlists = cb->listcap = 0;

    putconst(cb, expr); /* insert expression as first constant */
}

/* cb$patchlabels() */
static void patch_labels(CodeBuf *cb, Cntxt *cntxt)
{
    for (int i = 1; i < cb->ncode; i++) {
	if (cb->kind[i] == WK_LABEL) {
	    int pos = cb->labelpos[cb->code[i]];
	    if (pos < 0)
		Rf_errorcall(cntxt->call,
			     _("no offset recorded for label \"L%d\""),
			     cb->code[i] + 1);
	    cb->code[i] = pos;
	}
	else if (cb->kind[i] == WK_LABELLIST) {
	    int li = cb->code[i];
	    int n = cb->listlen[li];
	    SEXP off = PROTECT(Rf_allocVector(INTSXP, n));
	    for (int j = 0; j < n; j++) {
		int pos = cb->labelpos[cb->lists[li][j]];
		if (pos < 0)
		    Rf_errorcall(cntxt->call,
				 _("no offset recorded for label \"L%d\""),
				 cb->lists[li][j] + 1);
		INTEGER(off)[j] = pos;
	    }
	    cb->code[i] = putconst(cb, off);
	    UNPROTECT(1);
	}
	cb->kind[i] = WK_INT;
    }
}

static void commit_index(CodeBuf *cb, const int *buf, const char *cls)
{
    SEXP idx = PROTECT(Rf_allocVector(INTSXP, cb->ncode));
    memcpy(INTEGER(idx), buf, cb->ncode * sizeof(int));
    SEXP klass = PROTECT(Rf_mkString(cls));
    Rf_setAttrib(idx, R_ClassSymbol, klass);
    putconst(cb, idx);
    UNPROTECT(2);
}

/* codeBufCode(cb, cntxt): patch labels, commit the location indices and
   build the byte code object; releases the buffer's protection */
static SEXP codebuf_code(CodeBuf *cb, Cntxt *cntxt)
{
    patch_labels(cb, cntxt);
    if (cb->exprTracking)
	commit_index(cb, cb->exprbuf, "expressionsIndex");
    if (cb->srcrefTracking)
	commit_index(cb, cb->srcrefbuf, "srcrefsIndex");

    SEXP bytes = PROTECT(Rf_allocVector(INTSXP, cb->ncode));
    memcpy(INTEGER(bytes), cb->code, cb->ncode * sizeof(int));
    SEXP consts = PROTECT(Rf_allocVector(VECSXP, cb->nconst));
    for (int i = 0; i < cb->nconst; i++)
	SET_VECTOR_ELT(consts, i, VECTOR_ELT(cb->consts, i));
    SEXP ans = bc_mkcode(bytes, consts);
    UNPROTECT(3); /* bytes, consts, cb->consts */
    return ans;
}


/*
 * Constant folding
 */

#define MAX_CONST_SIZE 10

/* The R compiler's tests on a constant, such as
   mode(e) %in% constModes && length(e) <= 10, call generics that
   dispatch on an object, with `&&` combining the results: evaluate the
   whole predicate in R, so that its three-valued logic and its checks
   on the operands apply, and treat NA as if() would. */
static int eval_object_test(SEXP call)
{
    PROTECT(call);
    SEXP val = PROTECT(Rf_eval(call, R_BaseNamespace));
    int ans = Rf_asLogical(val);
    if (ans == NA_LOGICAL)
	Rf_error(_("missing value where TRUE/FALSE needed"));
    UNPROTECT(2);
    return ans;
}

static SEXP s_andand, s_length, s_isna, s_not, s_le, s_eq;

static void init_test_symbols(void)
{
    if (s_andand == NULL) {
	s_andand = Rf_install("&&");
	s_length = Rf_install("length");
	s_isna = Rf_install("is.na");
	s_not = Rf_install("!");
	s_le = Rf_install("<=");
	s_eq = Rf_install("==");
    }
}

/* TRUE && length(e) <= MAX_CONST_SIZE for an object */
static int const_size_test(SEXP e)
{
    if (! OBJECT(e))
	return XLENGTH(e) <= MAX_CONST_SIZE;
    init_test_symbols();
    SEXP len = PROTECT(Rf_lang2(s_length, e));
    SEXP test = PROTECT(Rf_lang3(s_le, len, Rf_ScalarReal(MAX_CONST_SIZE)));
    SEXP call = Rf_lang3(s_andand, R_TrueValue, test);
    UNPROTECT(2);
    return eval_object_test(call);
}

/* length(ct) == 1 && !is.na(ct) for an object (the if() handler) */
static int const_scalar_test(SEXP ct)
{
    if (! OBJECT(ct))
	return XLENGTH(ct) == 1 && LOGICAL(ct)[0] != NA_LOGICAL;
    init_test_symbols();
    SEXP len = PROTECT(Rf_lang2(s_length, ct));
    SEXP t1 = PROTECT(Rf_lang3(s_eq, len, Rf_ScalarReal(1)));
    SEXP isna = PROTECT(Rf_lang2(s_isna, ct));
    SEXP t2 = PROTECT(Rf_lang2(s_not, isna));
    SEXP call = Rf_lang3(s_andand, t1, t2);
    UNPROTECT(4);
    return eval_object_test(call);
}

/* checkConst(e): a constant that may be folded into the code */
static int check_const(SEXP e)
{
    switch (TYPEOF(e)) {
    case NILSXP:
	return TRUE;
    case LGLSXP:
    case INTSXP:
    case REALSXP:
    case CPLXSXP:
    case STRSXP:
	return const_size_test(e);
    default:
	return FALSE;
    }
}

static int is_base_var(SEXP sym, Cntxt *cntxt);

/* constantFoldSym: *value is set when TRUE is returned */
static int constant_fold_sym(SEXP sym, Cntxt *cntxt, SEXP *value)
{
    if ((sym_flags(sym) & SI_CONSTNAME) && is_base_var(sym, cntxt)) {
	SEXP val = Rf_findVarInFrame3(R_BaseNamespace, sym, TRUE);
	if (TYPEOF(val) == PROMSXP) {
	    PROTECT(val);
	    val = Rf_eval(val, R_EmptyEnv);
	    UNPROTECT(1);
	}
	if (check_const(val)) {
	    *value = val;
	    return TRUE;
	}
    }
    return FALSE;
}

/* getFoldFun */
static SEXP get_fold_fun(SEXP sym, Cntxt *cntxt)
{
    if ((sym_flags(sym) & SI_FOLDFUN) && is_base_var(sym, cntxt)) {
	SEXP val = Rf_findVarInFrame3(R_BaseNamespace, sym, TRUE);
	if (TYPEOF(val) == PROMSXP) {
	    PROTECT(val);
	    val = Rf_eval(val, R_EmptyEnv);
	    UNPROTECT(1);
	}
	if (Rf_isFunction(val))
	    return val;
    }
    return NULL;
}

typedef struct {
    SEXP call;
    int failed;
    int isConst;
} FoldData;

/* checkConst(do.call(ffun, args)): the R compiler evaluates the folded
   call in a frame of the compiler namespace, from which S3 methods in
   the global environment and on the search path are visible; the base
   namespace sees the same. The length of an object result is
   dispatched too, so the check runs under the same handlers. */
static SEXP fold_body(void *data)
{
    FoldData *fd = (FoldData *) data;
    SEXP val = PROTECT(Rf_eval(fd->call, R_BaseNamespace));
    fd->isConst = check_const(val);
    UNPROTECT(1);
    return val;
}

static SEXP fold_handler(SEXP cond, void *data)
{
    FoldData *fd = (FoldData *) data;
    fd->failed = TRUE;
    return R_NilValue;
}

static int constant_fold(SEXP e, Cntxt *cntxt, SEXP *value);

/* constantFoldCall */
static int constant_fold_call(SEXP e, Cntxt *cntxt, SEXP *value)
{
    R_CheckStack();
    SEXP fun = CAR(e);
    if (TYPEOF(fun) != SYMSXP)
	return FALSE;
    SEXP ffun = get_fold_fun(fun, cntxt);
    if (ffun == NULL)
	return FALSE;

    /* every argument must fold to a constant */
    SEXP args = PROTECT(copy_call(CDR(e)));
    for (SEXP p = args; p != R_NilValue; p = CDR(p)) {
	bc_tick();
	if (CAR(p) == R_MissingArg) {
	    UNPROTECT(1);
	    return FALSE;
	}
	SEXP val;
	if (! constant_fold(CAR(p), cntxt, &val)) {
	    UNPROTECT(1);
	    return FALSE;
	}
	SETCAR(p, val);
    }
    /* all(modes %in% constModes): a check of the types only */
    for (SEXP p = args; p != R_NilValue; p = CDR(p))
	switch (TYPEOF(CAR(p))) {
	case NILSXP: case LGLSXP: case INTSXP: case REALSXP: case CPLXSXP:
	case STRSXP:
	    break;
	default:
	    UNPROTECT(1);
	    return FALSE;
	}

    /* tryCatch(checkConst(do.call(ffun, args)),
                error = function(e) NULL, warning = function(w) NULL) */
    FoldData fd;
    fd.call = PROTECT(LCONS(ffun, args));
    fd.failed = FALSE;
    fd.isConst = FALSE;
    SEXP val = R_tryCatch(fold_body, &fd, foldConditions, fold_handler, &fd,
			  NULL, NULL);
    UNPROTECT(2);
    if (fd.failed || ! fd.isConst)
	return FALSE;
    *value = val;
    return TRUE;
}

/* constantFold(e, cntxt): TRUE with *value set if e folds to a constant */
static int constant_fold(SEXP e, Cntxt *cntxt, SEXP *value)
{
    switch (TYPEOF(e)) {
    case LANGSXP:
	return constant_fold_call(e, cntxt, value);
    case SYMSXP:
	return constant_fold_sym(e, cntxt, value);
    case PROMSXP:
	Rf_errorcall(cntxt->call, _("cannot constant fold literal promises"));
    case BCODESXP:
	Rf_errorcall(cntxt->call,
		     _("cannot constant fold literal bytecode objects"));
    default:
	if (check_const(e)) {
	    *value = e;
	    return TRUE;
	}
	return FALSE;
    }
}


/*
 * Inlining information (getInlineInfo, isBaseVar)
 */

typedef struct {
    int found;
    SEXP frame;
    int ftype;
    int package;
    int guard;
} InlineInfo;

static InlineInfo get_inline_info(SEXP sym, Cntxt *cntxt, int guardOK)
{
    InlineInfo ii = { FALSE, NULL, FT_LOCAL, PKG_NONE, FALSE };
    int optimize = cntxt->optimize;
    if (optimize <= 0 || (sym_flags(sym) & SI_NOINLINE))
	return ii;
    VarInfo info = find_cenv_var(sym, cntxt->env);
    if (! info.found)
	return ii;
    ii.frame = info.frame;
    ii.ftype = info.ftype;
    if (info.ftype == FT_NAMESPACE) {
	SEXP frame = info.frame;
	if (! R_IsNamespaceEnv(frame)) {
	    /* should be the import frame of the current topenv */
	    SEXP top = Rf_topenv(R_NilValue, cenv_base(cntxt->env)->env);
	    if (! R_IsNamespaceEnv(top) || frame != ENCLOS(top))
		Rf_errorcall(cntxt->call, _("bad namespace import frame"));
	    frame = top;
	}
	ii.found = TRUE;
	ii.package = package_code(ns_name(find_home_ns(sym, frame)));
	ii.guard = FALSE;
    }
    else if (info.ftype == FT_GLOBAL &&
	     (optimize >= 3 ||
	      (optimize >= 2 && (sym_flags(sym) & SI_LANGFUN)))) {
	ii.found = TRUE;
	ii.package = package_code(pack_frame_name(info.frame));
	ii.guard = FALSE;
    }
    else if (guardOK && info.ftype == FT_GLOBAL &&
	     strcmp(pack_frame_name(info.frame), "base") == 0) {
	ii.found = TRUE;
	ii.package = PKG_BASE;
	ii.guard = TRUE;
    }
    return ii;
}

static int is_base_var(SEXP sym, Cntxt *cntxt)
{
    InlineInfo ii = get_inline_info(sym, cntxt, FALSE);
    return ii.found && (ii.frame == R_BaseNamespace || ii.frame == R_BaseEnv);
}


/*
 * Finding possible local variables (findLocals)
 */

/* getAssignedVar(e, cntxt) */
static SEXP get_assigned_var(SEXP e, Cntxt *cntxt)
{
    if (CDR(e) == R_NilValue)
	Rf_errorcall(cntxt->call, _("bad assignment: %s"), "<call>");
    SEXP v = CADR(e);
    if (v == R_MissingArg)
	Rf_errorcall(cntxt->call, _("bad assignment: %s"), "<call>");
    if (TYPEOF(v) == SYMSXP)
	return v;
    if (TYPEOF(v) == STRSXP) {
	if (XLENGTH(v) != 1 || STRING_ELT(v, 0) == NA_STRING ||
	    CHAR(STRING_ELT(v, 0))[0] == '\0')
	    Rf_errorcall(cntxt->call, _("bad assignment: %s"), "<call>");
	return Rf_installTrChar(STRING_ELT(v, 0));
    }
    while (TYPEOF(v) == LANGSXP) {
	if (lang_length(v) < 2)
	    Rf_errorcall(cntxt->call, _("bad assignment: %s"), "<call>");
	v = CADR(v);
	if (v == R_MissingArg)
	    Rf_errorcall(cntxt->call, _("bad assignment: %s"), "<call>");
    }
    if (TYPEOF(v) != SYMSXP)
	Rf_errorcall(cntxt->call, _("bad assignment: %s"), "<call>");
    return v;
}

typedef struct {
    SEXP *items;
    int n, cap;
} Worklist;

static void worklist_push(Worklist *w, SEXP e)
{
    if (TYPEOF(e) != LANGSXP)
	return;
    if (w->n == w->cap) {
	int ncap = w->cap == 0 ? 64 : 2 * w->cap;
	SEXP *ni = (SEXP *) R_alloc(ncap, sizeof(SEXP));
	if (w->n > 0)
	    memcpy(ni, w->items, w->n * sizeof(SEXP));
	w->items = ni;
	w->cap = ncap;
    }
    w->items[w->n++] = e;
}

static void worklist_push_list(Worklist *w, SEXP p)
{
    for (; p != R_NilValue; p = CDR(p))
	worklist_push(w, CAR(p));
}

/* findLocals1 for one expression, pushing the subexpressions to visit */
static void find_locals1(SEXP e, const SEXP *shadowed, int nshadowed,
			 Cntxt *cntxt, SymSet *vars, Worklist *todo)
{
    if (TYPEOF(e) != LANGSXP)
	return;
    SEXP fun = CAR(e);
    SEXP fsym = name_as_symbol(fun);
    if (fsym == NULL || (TYPEOF(fun) == STRSXP && XLENGTH(fun) != 1)) {
	if (TYPEOF(fun) == SYMSXP || TYPEOF(fun) == STRSXP) {
	    ;
	}
	else {
	    worklist_push_list(todo, e);
	    return;
	}
    }
    if (fsym == NULL) {
	/* a character function name that is not a single string */
	worklist_push_list(todo, CDR(e));
	return;
    }
    const char *v = CHAR(PRINTNAME(fsym));
    int isShadowed = FALSE;
    for (int i = 0; i < nshadowed; i++)
	if (shadowed[i] == fsym)
	    isShadowed = TRUE;

    if (strcmp(v, "=") == 0 || strcmp(v, "<-") == 0) {
	symset_add(vars, get_assigned_var(e, cntxt));
	worklist_push_list(todo, CDR(e));
    }
    else if (strcmp(v, "for") == 0) {
	if (CDR(e) != R_NilValue) {
	    SEXP var = CADR(e);
	    SEXP vsym = name_as_symbol(var);
	    if (vsym != NULL)
		symset_add(vars, vsym);
	    else if (var != R_MissingArg && TYPEOF(var) == LANGSXP)
		/* as.character(e[2]) deparses the element */
		symset_add(vars, Rf_install(CHAR(STRING_ELT(
		    Rf_deparse1line(var, FALSE), 0))));
	    worklist_push(todo, CAR(e));
	    worklist_push_list(todo, CDDR(e));
	}
    }
    else if (strcmp(v, "delayedAssign") == 0 || strcmp(v, "assign") == 0) {
	if (lang_length(e) == 3 && TYPEOF(CADR(e)) == STRSXP &&
	    XLENGTH(CADR(e)) == 1) {
	    symset_add(vars, Rf_installTrChar(STRING_ELT(CADR(e), 0)));
	    worklist_push(todo, CADDR(e));
	}
    }
    else if (strcmp(v, "function") == 0 || strcmp(v, "~") == 0) {
	;
    }
    else if (strcmp(v, "local") == 0) {
	if (! (! isShadowed && lang_length(e) == 2))
	    worklist_push_list(todo, CDR(e));
    }
    else if (strcmp(v, "expression") == 0 || strcmp(v, "quote") == 0) {
	if (isShadowed)
	    worklist_push_list(todo, CDR(e));
    }
    else {
	worklist_push_list(todo, CDR(e));
    }
}

/* findLocalsList1(elist, shadowed, cntxt) */
static void find_locals_list1(SEXP *elist, int n, const SEXP *shadowed,
			      int nshadowed, Cntxt *cntxt, SymSet *vars)
{
    Worklist todo = { NULL, 0, 0 };
    for (int i = 0; i < n; i++)
	worklist_push(&todo, elist[i]);
    while (todo.n > 0) {
	SEXP e = todo.items[--todo.n];
	bc_tick();
	find_locals1(e, shadowed, nshadowed, cntxt, vars, &todo);
    }
}

static void notify_assign_syntactic_fun(SEXP *funs, int n, Cntxt *cntxt);

/* findLocalsList(elist, cntxt): adds the local variables to out */
static void find_locals_list(SEXP *elist, int n, Cntxt *cntxt, SymSet *out)
{
    SEXP initialShadowedFuns[3] = {
	Rf_install("expression"), s_local, Rf_install("quote")
    };
    SEXP shadowed[3];
    int nshadowed = 0;
    for (int i = 0; i < 3; i++)
	if (! is_base_var(initialShadowedFuns[i], cntxt))
	    shadowed[nshadowed++] = initialShadowedFuns[i];
    SEXP sf[3];
    int nsf = 3;
    memcpy(sf, initialShadowedFuns, sizeof sf);
    for (;;) {
	SymSet vals;
	symset_init(&vals);
	find_locals_list1(elist, n, sf, nsf, cntxt, &vals);
	SEXP nsfv[3];
	int nnsf = 0;
	for (int i = 0; i < nshadowed; i++)
	    nsfv[nnsf++] = shadowed[i];
	for (int i = 0; i < nsf; i++)
	    if (symset_has(&vals, sf[i])) {
		int dup = FALSE;
		for (int j = 0; j < nnsf; j++)
		    if (nsfv[j] == sf[i])
			dup = TRUE;
		if (! dup)
		    nsfv[nnsf++] = sf[i];
	    }
	int last_nsf = nsf;
	memcpy(sf, nsfv, nnsf * sizeof(SEXP));
	nsf = nnsf;
	if (last_nsf == nsf) {
	    SEXP rdsf[5];
	    int nrdsf = 0;
	    static const char *specialSyntaxFuns[] = {
		"<-", "=", "for", "function", "~" /* C locale order */
	    };
	    for (int i = 0; i < 5; i++) {
		SEXP s = Rf_install(specialSyntaxFuns[i]);
		if (symset_has(&vals, s))
		    rdsf[nrdsf++] = s;
	    }
	    if (nrdsf > 0)
		notify_assign_syntactic_fun(rdsf, nrdsf, cntxt);
	    for (int i = 0; i < vals.size; i++)
		if (vals.tab[i] != NULL)
		    symset_add(out, vals.tab[i]);
	    return;
	}
    }
}


/*
 * Scans of expressions: mayCallBrowser, checkSkipLoopCntxt
 */

static int may_call_browser_list(SEXP elist, Cntxt *cntxt);

static int may_call_browser(SEXP e, Cntxt *cntxt)
{
    if (TYPEOF(e) != LANGSXP)
	return FALSE;
    R_CheckStack();
    bc_tick();
    SEXP fun = CAR(e);
    if (TYPEOF(fun) == SYMSXP) {
	if (fun == s_browser)
	    return TRUE; /* not checking isBaseVar to err on the positive */
	else if (fun == R_FunctionSymbol && is_base_var(fun, cntxt))
	    return FALSE;
	else
	    return may_call_browser_list(CDR(e), cntxt);
    }
    else {
	return may_call_browser_list(e, cntxt);
    }
}

static int may_call_browser_list(SEXP elist, Cntxt *cntxt)
{
    for (SEXP p = elist; p != R_NilValue; p = CDR(p))
	if (CAR(p) != R_MissingArg && may_call_browser(CAR(p), cntxt))
	    return TRUE;
    return FALSE;
}

static int check_skip_loop_cntxt_list(SEXP elist, Cntxt *cntxt, int breakOK);

static int check_skip_loop_cntxt(SEXP e, Cntxt *cntxt, int breakOK)
{
    if (TYPEOF(e) != LANGSXP)
	return TRUE;
    R_CheckStack();
    bc_tick();
    SEXP fun = CAR(e);
    if (TYPEOF(fun) == SYMSXP) {
	unsigned flags = sym_flags(fun);
	if (! breakOK && (flags & SI_BREAKNEXT))
	    return FALSE;
	else if ((flags & SI_LOOPSTOP) && is_base_var(fun, cntxt))
	    return TRUE;
	else if ((flags & SI_LOOPTOP) && is_base_var(fun, cntxt))
	    return check_skip_loop_cntxt_list(CDR(e), cntxt, breakOK);
	else if (flags & SI_EVALFUN)
	    return FALSE;
	else
	    return check_skip_loop_cntxt_list(CDR(e), cntxt, FALSE);
    }
    else {
	return check_skip_loop_cntxt_list(e, cntxt, FALSE);
    }
}

static int check_skip_loop_cntxt_list(SEXP elist, Cntxt *cntxt, int breakOK)
{
    for (SEXP p = elist; p != R_NilValue; p = CDR(p))
	if (CAR(p) != R_MissingArg &&
	    ! check_skip_loop_cntxt(CAR(p), cntxt, breakOK))
	    return FALSE;
    return TRUE;
}


/*
 * Compiler notes (the notify* functions). The R compiler prints these
 * with cat(); they are suppressed by default.
 */

static void note(const char *msg, CodeBuf *cb)
{
    SEXP sref = cb == NULL ? R_NilValue : cb->curSrcref;
    if (sref != R_NilValue && TYPEOF(sref) == INTSXP && XLENGTH(sref) >= 1) {
	const char *fname = "";
	SEXP srcfile = Rf_getAttrib(sref, s_srcfile);
	if (TYPEOF(srcfile) == ENVSXP) {
	    SEXP fn = Rf_findVarInFrame3(srcfile, s_filename, TRUE);
	    if (TYPEOF(fn) == STRSXP && XLENGTH(fn) >= 1) {
		fname = CHAR(STRING_ELT(fn, 0));
		const char *slash = strrchr(fname, '/');
		if (slash != NULL)
		    fname = slash + 1;
	    }
	}
	Rprintf("Note: %s at %s:%d \n", msg, fname, INTEGER(sref)[0]);
    }
    else {
	Rprintf("Note: %s \n", msg);
    }
}

static int suppress_undef(SEXP sym, Cntxt *cntxt)
{
    if (cntxt->suppressAll)
	return TRUE;
    SEXP suppress = cntxt->suppressUndefined;
    if (suppress == R_NilValue)
	return FALSE;
    if (TYPEOF(suppress) == LGLSXP && XLENGTH(suppress) == 1)
	return LOGICAL(suppress)[0] == TRUE;
    if (TYPEOF(suppress) == STRSXP) {
	const char *name = CHAR(PRINTNAME(sym));
	for (R_xlen_t i = 0; i < XLENGTH(suppress); i++)
	    if (strcmp(CHAR(STRING_ELT(suppress, i)), name) == 0)
		return TRUE;
    }
    return FALSE;
}

static void notify_undef_fun(SEXP fun, Cntxt *cntxt, CodeBuf *cb)
{
    if (! suppress_undef(fun, cntxt)) {
	char buf[512];
	snprintf(buf, sizeof buf,
		 _("no visible global function definition for '%s'"),
		 CHAR(PRINTNAME(fun)));
	note(buf, cb);
    }
}

static void notify_undef_var(SEXP var, Cntxt *cntxt, CodeBuf *cb)
{
    if (! suppress_undef(var, cntxt)) {
	char buf[512];
	snprintf(buf, sizeof buf,
		 _("no visible binding for global variable '%s'"),
		 CHAR(PRINTNAME(var)));
	note(buf, cb);
    }
}

static void notify_no_super_assign_var(SEXP sym, Cntxt *cntxt, CodeBuf *cb)
{
    if (! cntxt->suppressAll && ! cntxt->suppressNoSuperAssignVar) {
	char buf[512];
	snprintf(buf, sizeof buf,
		 _("no visible binding for '<<-' assignment to '%s'"),
		 CHAR(PRINTNAME(sym)));
	note(buf, cb);
    }
}

static void notify_wrong_arg_count(const char *fun, Cntxt *cntxt, CodeBuf *cb)
{
    if (! cntxt->suppressAll) {
	char buf[512];
	snprintf(buf, sizeof buf, _("wrong number of arguments to '%s'"), fun);
	note(buf, cb);
    }
}

static void notify_wrong_dots_use(const char *var, Cntxt *cntxt, CodeBuf *cb)
{
    if (! cntxt->suppressAll) {
	char buf[512];
	snprintf(buf, sizeof buf, "%s may be used in an incorrect context", var);
	note(buf, cb);
    }
}

static void notify_wrong_break_next(const char *fun, Cntxt *cntxt, CodeBuf *cb)
{
    if (! cntxt->suppressAll) {
	char buf[512];
	snprintf(buf, sizeof buf, "%s used in wrong context: no loop is visible",
		 fun);
	note(buf, cb);
    }
}

static void notify_bad_assign_fun(Cntxt *cntxt, CodeBuf *cb)
{
    if (! cntxt->suppressAll)
	note(_("invalid function in complex assignment"), cb);
}

static void notify_multiple_switch_defaults(Cntxt *cntxt, CodeBuf *cb)
{
    if (! cntxt->suppressAll)
	note(_("more than one default provided in switch() call"), cb);
}

static void notify_no_switch_cases(Cntxt *cntxt, CodeBuf *cb)
{
    if (! cntxt->suppressAll)
	note(_("'switch' with no alternatives"), cb);
}

static void notify_assign_syntactic_fun(SEXP *funs, int n, Cntxt *cntxt)
{
    if (! cntxt->suppressAll) {
	char buf[1024];
	snprintf(buf, sizeof buf, "%s ",
		 n == 1 ? _("local assignment to syntactic function: ") :
		 _("local assignments to syntactic functions: "));
	for (int i = 0; i < n; i++) {
	    strncat(buf, CHAR(PRINTNAME(funs[i])), sizeof buf - strlen(buf) - 1);
	    if (i < n - 1)
		strncat(buf, ", ", sizeof buf - strlen(buf) - 1);
	}
	note(buf, NULL);
    }
}

/* checkCall(def, call, signal): with the notes enabled, the call is
   matched against the definition by R's match.call() */
static const char *checkcall_source =
    "function(def, call) {\n"
    "    if (typeof(def) %in% c('builtin', 'special')) def <- args(def)\n"
    "    if (typeof(def) != 'closure') return(NA)\n"
    "    msg <- tryCatch({ match.call(def, call); NULL },\n"
    "                    error = function(e) conditionMessage(e))\n"
    "    if (is.null(msg)) TRUE\n"
    "    else sprintf(\"possible error in '%s': %s\", deparse(call, 20)[1], msg)\n"
    "}\n";

static void check_call_note(SEXP def, SEXP call, Cntxt *cntxt, CodeBuf *cb)
{
    if (any_dots(call))
	return;
    if (checkCallFun == NULL) {
	checkCallFun = R_ParseEvalString(checkcall_source, R_BaseEnv);
	R_PreserveObject(checkCallFun);
    }
    SEXP qcall = PROTECT(Rf_lang2(R_QuoteSymbol, call));
    SEXP e = PROTECT(Rf_lang3(checkCallFun, def, qcall));
    SEXP res = PROTECT(Rf_eval(e, R_BaseEnv));
    if (TYPEOF(res) == STRSXP && XLENGTH(res) == 1)
	note(CHAR(STRING_ELT(res, 0)), cb);
    UNPROTECT(3);
}


/*
 * Compiler top level (cmp, cmpConst, cmpSym, cmpCall, ...)
 */

static void cmp(SEXP e, CodeBuf *cb, Cntxt *cntxt, int missingOK, int setloc);
static void cmp_call(SEXP call, CodeBuf *cb, Cntxt *cntxt, int inlineOK);
static SEXP gen_code(SEXP e, Cntxt *cntxt, const Loc *loc);
static int try_inline(SEXP e, CodeBuf *cb, Cntxt *cntxt);
static int cmp_builtin(SEXP e, CodeBuf *cb, Cntxt *cntxt, int internal);
static int cmp_special(SEXP e, CodeBuf *cb, Cntxt *cntxt);
static void cmp_builtin_args(SEXP args, CodeBuf *cb, Cntxt *cntxt, int missingOK);
static void cmp_call_args(SEXP args, CodeBuf *cb, Cntxt *cntxt, int nse);

static R_INLINE int identical_true(SEXP x)
{
    return TYPEOF(x) == LGLSXP && XLENGTH(x) == 1 && LOGICAL(x)[0] == TRUE &&
	ATTRIB(x) == R_NilValue && ! OBJECT(x) && ! IS_S4_OBJECT(x);
}

static R_INLINE int identical_false(SEXP x)
{
    return TYPEOF(x) == LGLSXP && XLENGTH(x) == 1 && LOGICAL(x)[0] == FALSE &&
	ATTRIB(x) == R_NilValue && ! OBJECT(x) && ! IS_S4_OBJECT(x);
}

/* cmpConst(val, cb, cntxt) */
static void cmp_const(SEXP val, CodeBuf *cb, Cntxt *cntxt)
{
    if (val == R_NilValue) {
	put0(cb, LDNULL_OP);
    }
    else if (identical_true(val)) {
	put0(cb, LDTRUE_OP);
    }
    else if (identical_false(val)) {
	put0(cb, LDFALSE_OP);
    }
    else {
	int ci = putconst(cb, val);
	put1(cb, LDCONST_OP, ci);
    }
    if (cntxt->tailcall)
	put0(cb, RETURN_OP);
}

/* cmpSym(sym, cb, cntxt, missingOK) */
static void cmp_sym(SEXP sym, CodeBuf *cb, Cntxt *cntxt, int missingOK)
{
    if (sym == R_DotsSymbol) {
	notify_wrong_dots_use("...", cntxt, cb);
	put0(cb, DOTSERR_OP);
    }
    else if (is_ddsym(sym)) {
	if (! cntxt->suppressAll && ! find_loc_var(R_DotsSymbol, cntxt->env))
	    notify_wrong_dots_use(CHAR(PRINTNAME(sym)), cntxt, cb);
	int ci = putconst(cb, sym);
	put1(cb, missingOK ? DDVAL_MISSOK_OP : DDVAL_OP, ci);
	if (cntxt->tailcall)
	    put0(cb, RETURN_OP);
    }
    else {
	if (! cntxt->suppressAll && ! find_var(sym, cntxt->env))
	    notify_undef_var(sym, cntxt, cb);
	int ci = putconst(cb, sym);
	put1(cb, missingOK ? GETVAR_MISSOK_OP : GETVAR_OP, ci);
	if (cntxt->tailcall)
	    put0(cb, RETURN_OP);
    }
}

/* cmp(e, cb, cntxt, missingOK, setloc) */
static void cmp(SEXP e, CodeBuf *cb, Cntxt *cntxt, int missingOK, int setloc)
{
    R_CheckStack();
    bc_tick();
    Loc sloc;
    if (setloc) {
	sloc = savecurloc(cb);
	setcurexpr(cb, e);
    }
    SEXP val;
    if (! constant_fold(e, cntxt, &val)) {
	switch (TYPEOF(e)) {
	case LANGSXP:
	    cmp_call(e, cb, cntxt, TRUE);
	    break;
	case SYMSXP:
	    cmp_sym(e, cb, cntxt, missingOK);
	    break;
	case BCODESXP:
	    Rf_errorcall(cntxt->call, _("cannot compile byte code literals in code"));
	case PROMSXP:
	    Rf_errorcall(cntxt->call, _("cannot compile promise literals in code"));
	default:
	    cmp_const(e, cb, cntxt);
	}
    }
    else {
	PROTECT(val);
	cmp_const(val, cb, cntxt);
	UNPROTECT(1);
    }
    if (setloc)
	restorecurloc(cb, sloc);
}

/* cmpTag(n, cb) */
static R_INLINE void cmp_tag(SEXP tag, CodeBuf *cb)
{
    if (tag != R_NilValue && CHAR(PRINTNAME(tag))[0] != '\0') {
	int ci = putconst(cb, tag);
	put1(cb, SETTAG_OP, ci);
    }
}

/* cmpConstArg(a, cb, cntxt) */
static void cmp_const_arg(SEXP a, CodeBuf *cb)
{
    if (a == R_NilValue) {
	put0(cb, PUSHNULLARG_OP);
    }
    else if (identical_true(a)) {
	put0(cb, PUSHTRUEARG_OP);
    }
    else if (identical_false(a)) {
	put0(cb, PUSHFALSEARG_OP);
    }
    else {
	int ci = putconst(cb, a);
	put1(cb, PUSHCONSTARG_OP, ci);
    }
}

/* cmpCallArgs(args, cb, cntxt, nse) */
static void cmp_call_args(SEXP args, CodeBuf *cb, Cntxt *cntxt, int nse)
{
    Cntxt pcntxt = make_promise_context(cntxt);
    for (SEXP p = args; p != R_NilValue; p = CDR(p)) {
	bc_tick();
	SEXP a = CAR(p);
	SEXP n = TAG(p);
	if (a == R_MissingArg) {
	    put0(cb, DOMISSING_OP);
	    cmp_tag(n, cb);
	}
	else if (a == R_DotsSymbol) {
	    if (! cntxt->suppressAll && ! find_loc_var(R_DotsSymbol, cntxt->env))
		notify_wrong_dots_use("...", cntxt, cb);
	    put0(cb, DODOTS_OP);
	}
	else if (TYPEOF(a) == BCODESXP) {
	    Rf_errorcall(cntxt->call, _("cannot compile byte code literals in code"));
	}
	else if (TYPEOF(a) == PROMSXP) {
	    Rf_errorcall(cntxt->call, _("cannot compile promise literals in code"));
	}
	else {
	    if (TYPEOF(a) == SYMSXP || TYPEOF(a) == LANGSXP) {
		int ci;
		if (nse) {
		    ci = putconst(cb, a);
		}
		else {
		    Loc loc = savecurloc(cb);
		    SEXP code = PROTECT(gen_code(a, &pcntxt, &loc));
		    ci = putconst(cb, code);
		    UNPROTECT(1);
		}
		put1(cb, MAKEPROM_OP, ci);
	    }
	    else {
		cmp_const_arg(a, cb);
	    }
	    cmp_tag(n, cb);
	}
    }
}

/* cmpCallSymFun(fun, args, call, cb, cntxt) */
static void cmp_call_sym_fun(SEXP fun, SEXP args, SEXP call, CodeBuf *cb,
			     Cntxt *cntxt)
{
    int ci = putconst(cb, fun);
    put1(cb, GETFUN_OP, ci);
    int nse = (sym_flags(fun) & SI_NSE) != 0;
    cmp_call_args(args, cb, cntxt, nse);
    ci = putconst(cb, call);
    put1(cb, CALL_OP, ci);
    if (cntxt->tailcall)
	put0(cb, RETURN_OP);
}

/* cmpCallExprFun(fun, args, call, cb, cntxt) */
static void cmp_call_expr_fun(SEXP fun, SEXP args, SEXP call, CodeBuf *cb,
			      Cntxt *cntxt)
{
    Cntxt ncntxt = make_nontailcall_context(cntxt);
    cmp(fun, cb, &ncntxt, FALSE, TRUE);
    put0(cb, CHECKFUN_OP);
    cmp_call_args(args, cb, cntxt, FALSE);
    int ci = putconst(cb, call);
    put1(cb, CALL_OP, ci);
    if (cntxt->tailcall)
	put0(cb, RETURN_OP);
}

/* cmpCall(call, cb, cntxt, inlineOK) */
static void cmp_call(SEXP call, CodeBuf *cb, Cntxt *ocntxt, int inlineOK)
{
    Loc sloc = savecurloc(cb);
    setcurexpr(cb, call);
    Cntxt ccntxt = make_call_context(ocntxt, call);
    Cntxt *cntxt = &ccntxt;
    SEXP fun = CAR(call);
    SEXP args = CDR(call);
    if (TYPEOF(fun) == SYMSXP) {
	if (! (inlineOK && try_inline(call, cb, cntxt))) {
	    if (! cntxt->suppressAll) {
		/* the notes: a local function, an undefined function,
		   or a call that does not match the definition */
		if (! find_loc_var(fun, cntxt->env)) {
		    SEXP def = find_fun_def(fun, cntxt->env);
		    if (def == NULL) {
			notify_undef_fun(fun, cntxt, cb);
		    }
		    else {
			PROTECT(def);
			check_call_note(def, call, cntxt, cb);
			UNPROTECT(1);
		    }
		}
	    }
	    cmp_call_sym_fun(fun, args, call, cb, cntxt);
	}
    }
    else {
	/* this hack is needed for now because of the way the parser
	   handles break() and next() calls */
	if (TYPEOF(fun) == LANGSXP && TYPEOF(CAR(fun)) == SYMSXP &&
	    (CAR(fun) == s_break || CAR(fun) == s_next)) {
	    cmp(fun, cb, cntxt, FALSE, TRUE);
	    return;
	}
	cmp_call_expr_fun(fun, args, call, cb, cntxt);
    }
    restorecurloc(cb, sloc);
}

/* genCode(e, cntxt, loc) */
static SEXP gen_code(SEXP e, Cntxt *cntxt, const Loc *loc)
{
    /* the buffers of a nested code object are released once it is
       built, rather than at the end of the whole compilation */
    const void *vmax = vmaxget();
    CodeBuf cb;
    make_codebuf(&cb, e, loc);
    cmp(e, &cb, cntxt, FALSE, FALSE);
    SEXP ans = codebuf_code(&cb, cntxt);
    vmaxset(vmax);
    return ans;
}


/*
 * Inlining
 */

static int call_handler(int h, int op, SEXP e, CodeBuf *cb, Cntxt *cntxt);

/* tryInline(e, cb, cntxt) */
static int try_inline(SEXP e, CodeBuf *cb, Cntxt *cntxt)
{
    SEXP name = CAR(e);
    /* the table entry is copied: get_inline_info() may run R code, and a
       nested compilation can grow the table */
    SymInfo *si = symtab_lookup(name, FALSE);
    if (si == NULL || si->handler == H_NONE)
	return FALSE; /* no handler for any package */
    int handler = si->handler, handlerPkg = si->handlerPkg, op = si->op;
    InlineInfo info = get_inline_info(name, cntxt, TRUE);
    if (! info.found)
	return FALSE;
    if (handlerPkg != info.package)
	return FALSE;
    if (info.guard) {
	int tailcall = cntxt->tailcall;
	Cntxt gcntxt = *cntxt;
	if (tailcall)
	    gcntxt.tailcall = FALSE;
	int expridx = putconst(cb, e);
	int endlabel = makelabel(cb);
	put1L(cb, BASEGUARD_OP, expridx, endlabel);
	if (! call_handler(handler, op, e, cb, &gcntxt))
	    cmp_call(e, cb, &gcntxt, FALSE);
	putlabel(cb, endlabel);
	if (tailcall)
	    put0(cb, RETURN_OP);
	return TRUE;
    }
    else {
	return call_handler(handler, op, e, cb, cntxt);
    }
}

static int try_setter_inline(SEXP afun, SEXP place, SEXP origplace, SEXP call,
			     CodeBuf *cb, Cntxt *cntxt);
static int try_getter_inline(SEXP call, CodeBuf *cb, Cntxt *cntxt);


/*
 * Inline handlers for some SPECIAL functions
 */

static int h_function(SEXP e, CodeBuf *cb, Cntxt *cntxt)
{
    if (lang_length(e) < 3)
	Rf_errorcall(cntxt->call, _("subscript out of bounds"));
    SEXP forms = CADR(e);
    SEXP body = CADDR(e);
    SEXP sref = lang_length(e) > 3 ? CADDDR(e) : R_NilValue;
    Cntxt ncntxt = make_function_context(cntxt, forms, body);
    if (may_call_browser(body, cntxt))
	return FALSE;
    Loc loc = savecurloc(cb);
    SEXP cbody = PROTECT(gen_code(body, &ncntxt, &loc));
    SEXP cl = PROTECT(Rf_allocVector(VECSXP, 3));
    SET_VECTOR_ELT(cl, 0, forms);
    SET_VECTOR_ELT(cl, 1, cbody);
    SET_VECTOR_ELT(cl, 2, sref);
    int ci = putconst(cb, cl);
    UNPROTECT(2);
    put1(cb, MAKECLOSURE_OP, ci);
    if (cntxt->tailcall)
	put0(cb, RETURN_OP);
    return TRUE;
}

static int h_brace(SEXP e, CodeBuf *cb, Cntxt *cntxt)
{
    int n = lang_length(e);
    if (n == 1) {
	cmp(R_NilValue, cb, cntxt, FALSE, TRUE);
    }
    else {
	Loc sloc = savecurloc(cb);
	SEXP bsrefs = srcref_attr(e);
	SEXP p = CDR(e);
	if (n > 2) {
	    Cntxt ncntxt = make_novalue_context(cntxt);
	    for (int i = 2; i <= n - 1; i++, p = CDR(p)) {
		SEXP subexp = CAR(p);
		setcurloc(cb, subexp, extract_srcref(bsrefs, i));
		cmp(subexp, cb, &ncntxt, FALSE, FALSE);
		put0(cb, POP_OP);
	    }
	}
	SEXP subexp = CAR(p);
	setcurloc(cb, subexp, extract_srcref(bsrefs, n));
	cmp(subexp, cb, cntxt, FALSE, FALSE);
	restorecurloc(cb, sloc);
    }
    return TRUE;
}

static int h_if(SEXP e, CodeBuf *cb, Cntxt *cntxt)
{
    int n = lang_length(e);
    if (n < 3)
	Rf_errorcall(cntxt->call, _("subscript out of bounds"));
    SEXP test = CADR(e);
    SEXP then_expr = CADDR(e);
    int have_else = n == 4;
    SEXP else_expr = have_else ? CADDDR(e) : R_NilValue;
    SEXP ct;
    if (constant_fold(test, cntxt, &ct) && TYPEOF(ct) == LGLSXP &&
	const_scalar_test(ct)) {
	/* if (ct$value): the evaluator's own checks on the value */
	if (XLENGTH(ct) != 1)
	    Rf_errorcall(cntxt->call, _("the condition has length > 1"));
	if (LOGICAL(ct)[0] == NA_LOGICAL)
	    Rf_errorcall(cntxt->call,
			 _("missing value where TRUE/FALSE needed"));
	if (LOGICAL(ct)[0]) {
	    cmp(then_expr, cb, cntxt, FALSE, TRUE);
	}
	else if (have_else) {
	    cmp(else_expr, cb, cntxt, FALSE, TRUE);
	}
	else if (cntxt->tailcall) {
	    put0(cb, LDNULL_OP);
	    put0(cb, INVISIBLE_OP);
	    put0(cb, RETURN_OP);
	}
	else {
	    put0(cb, LDNULL_OP);
	}
	return TRUE;
    }
    Cntxt ncntxt = make_nontailcall_context(cntxt);
    cmp(test, cb, &ncntxt, FALSE, TRUE);
    int callidx = putconst(cb, e);
    int else_label = makelabel(cb);
    put1L(cb, BRIFNOT_OP, callidx, else_label);
    cmp(then_expr, cb, cntxt, FALSE, TRUE);
    if (cntxt->tailcall) {
	putlabel(cb, else_label);
	if (have_else) {
	    cmp(else_expr, cb, cntxt, FALSE, TRUE);
	}
	else {
	    put0(cb, LDNULL_OP);
	    put0(cb, INVISIBLE_OP);
	    put0(cb, RETURN_OP);
	}
    }
    else {
	int end_label = makelabel(cb);
	putL(cb, GOTO_OP, end_label);
	putlabel(cb, else_label);
	if (have_else)
	    cmp(else_expr, cb, cntxt, FALSE, TRUE);
	else
	    put0(cb, LDNULL_OP);
	putlabel(cb, end_label);
    }
    return TRUE;
}

static int h_andor(SEXP e, CodeBuf *cb, Cntxt *cntxt, int op1, int op2)
{
    if (lang_length(e) < 3)
	Rf_errorcall(cntxt->call, _("subscript out of bounds"));
    Cntxt ncntxt = make_arg_context(cntxt);
    int callidx = putconst(cb, e);
    int label = makelabel(cb);
    cmp(CADR(e), cb, &ncntxt, FALSE, TRUE);
    put1L(cb, op1, callidx, label);
    cmp(CADDR(e), cb, &ncntxt, FALSE, TRUE);
    put1(cb, op2, callidx);
    putlabel(cb, label);
    if (cntxt->tailcall)
	put0(cb, RETURN_OP);
    return TRUE;
}


/*
 * Inline handlers for loops
 */

static int h_break_next(SEXP e, CodeBuf *cb, Cntxt *cntxt, int isBreak)
{
    if (! cntxt->hasLoop) {
	notify_wrong_break_next(isBreak ? "break" : "next", cntxt, cb);
	return cmp_special(e, cb, cntxt);
    }
    else if (cntxt->gotoOK) {
	putL(cb, GOTO_OP, isBreak ? cntxt->endLabel : cntxt->loopLabel);
	return TRUE;
    }
    else {
	return cmp_special(e, cb, cntxt);
    }
}

static void cmp_repeat_body(SEXP body, CodeBuf *cb, Cntxt *cntxt)
{
    int loop_label = makelabel(cb);
    int end_label = makelabel(cb);
    putlabel(cb, loop_label);
    Cntxt lcntxt = make_loop_context(cntxt, loop_label, end_label);
    cmp(body, cb, &lcntxt, FALSE, TRUE);
    put0(cb, POP_OP);
    putL(cb, GOTO_OP, loop_label);
    putlabel(cb, end_label);
}

static int h_repeat(SEXP e, CodeBuf *cb, Cntxt *cntxt)
{
    if (lang_length(e) < 2)
	Rf_errorcall(cntxt->call, _("subscript out of bounds"));
    SEXP body = CADR(e);
    if (check_skip_loop_cntxt(body, cntxt, TRUE)) {
	cmp_repeat_body(body, cb, cntxt);
    }
    else {
	Cntxt ncntxt = *cntxt;
	ncntxt.needRETURNJMP = TRUE;
	int ljmpend_label = makelabel(cb);
	put1L(cb, STARTLOOPCNTXT_OP, 0, ljmpend_label);
	cmp_repeat_body(body, cb, &ncntxt);
	putlabel(cb, ljmpend_label);
	put1(cb, ENDLOOPCNTXT_OP, 0);
    }
    put0(cb, LDNULL_OP);
    if (cntxt->tailcall) {
	put0(cb, INVISIBLE_OP);
	put0(cb, RETURN_OP);
    }
    return TRUE;
}

static void cmp_while_body(SEXP call, SEXP cond, SEXP body, CodeBuf *cb,
			   Cntxt *cntxt)
{
    int loop_label = makelabel(cb);
    int end_label = makelabel(cb);
    putlabel(cb, loop_label);
    Cntxt lcntxt = make_loop_context(cntxt, loop_label, end_label);
    cmp(cond, cb, &lcntxt, FALSE, TRUE);
    int callidx = putconst(cb, call);
    put1L(cb, BRIFNOT_OP, callidx, end_label);
    cmp(body, cb, &lcntxt, FALSE, TRUE);
    put0(cb, POP_OP);
    putL(cb, GOTO_OP, loop_label);
    putlabel(cb, end_label);
}

static int h_while(SEXP e, CodeBuf *cb, Cntxt *cntxt)
{
    if (lang_length(e) < 3)
	Rf_errorcall(cntxt->call, _("subscript out of bounds"));
    SEXP cond = CADR(e);
    SEXP body = CADDR(e);
    if (check_skip_loop_cntxt(cond, cntxt, TRUE) &&
	check_skip_loop_cntxt(body, cntxt, TRUE))
	cmp_while_body(e, cond, body, cb, cntxt);
    else {
	Cntxt ncntxt = *cntxt;
	ncntxt.needRETURNJMP = TRUE;
	int ljmpend_label = makelabel(cb);
	put1L(cb, STARTLOOPCNTXT_OP, 0, ljmpend_label);
	cmp_while_body(e, cond, body, cb, &ncntxt);
	putlabel(cb, ljmpend_label);
	put1(cb, ENDLOOPCNTXT_OP, 0);
    }
    put0(cb, LDNULL_OP);
    if (cntxt->tailcall) {
	put0(cb, INVISIBLE_OP);
	put0(cb, RETURN_OP);
    }
    return TRUE;
}

/* cmpForBody(callidx, body, ci, cb, cntxt); ci < 0 for NULL */
static void cmp_for_body(int callidx, SEXP body, int ci, CodeBuf *cb,
			 Cntxt *cntxt)
{
    int body_label = makelabel(cb);
    int loop_label = makelabel(cb);
    int end_label = makelabel(cb);
    if (ci < 0)
	putL(cb, GOTO_OP, loop_label);
    else
	put2L(cb, STARTFOR_OP, callidx, ci, loop_label);
    putlabel(cb, body_label);
    Cntxt lcntxt = make_loop_context(cntxt, loop_label, end_label);
    cmp(body, cb, &lcntxt, FALSE, TRUE);
    put0(cb, POP_OP);
    putlabel(cb, loop_label);
    putL(cb, STEPFOR_OP, body_label);
    putlabel(cb, end_label);
}

static int h_for(SEXP e, CodeBuf *cb, Cntxt *cntxt)
{
    if (lang_length(e) < 4)
	Rf_errorcall(cntxt->call, _("subscript out of bounds"));
    SEXP sym = CADR(e);
    SEXP seq = CADDR(e);
    SEXP body = CADDDR(e);
    if (TYPEOF(sym) != SYMSXP)
	/* not worth warning here since the parser should not allow this */
	return FALSE;
    Cntxt ncntxt = make_nontailcall_context(cntxt);
    cmp(seq, cb, &ncntxt, FALSE, TRUE);
    int ci = putconst(cb, sym);
    int callidx = putconst(cb, e);
    if (check_skip_loop_cntxt(body, cntxt, TRUE)) {
	cmp_for_body(callidx, body, ci, cb, cntxt);
    }
    else {
	Cntxt lcntxt = *cntxt;
	lcntxt.needRETURNJMP = TRUE;
	int ctxt_label = makelabel(cb);
	put2L(cb, STARTFOR_OP, callidx, ci, ctxt_label);
	putlabel(cb, ctxt_label);
	int ljmpend_label = makelabel(cb);
	put1L(cb, STARTLOOPCNTXT_OP, 1, ljmpend_label);
	cmp_for_body(-1, body, -1, cb, &lcntxt);
	putlabel(cb, ljmpend_label);
	put1(cb, ENDLOOPCNTXT_OP, 1);
    }
    put0(cb, ENDFOR_OP);
    if (cntxt->tailcall) {
	put0(cb, INVISIBLE_OP);
	put0(cb, RETURN_OP);
    }
    return TRUE;
}


/*
 * Inline handlers for one and two argument primitives
 */

static int cmp_prim1(SEXP e, CodeBuf *cb, int op, Cntxt *cntxt)
{
    if (dots_or_missing(CDR(e))) {
	return cmp_builtin(e, cb, cntxt, FALSE);
    }
    else if (lang_length(e) != 2) {
	notify_wrong_arg_count(name_string(CAR(e)), cntxt, cb);
	return cmp_builtin(e, cb, cntxt, FALSE);
    }
    else {
	Cntxt ncntxt = make_nontailcall_context(cntxt);
	cmp(CADR(e), cb, &ncntxt, FALSE, TRUE);
	int ci = putconst(cb, e);
	put1(cb, op, ci);
	if (cntxt->tailcall)
	    put0(cb, RETURN_OP);
	return TRUE;
    }
}

/* checkNeedsInc() in cmp.R always returns FALSE, so no INCLNK/DECLNK
   instructions are generated around the second operand */
static int cmp_prim2(SEXP e, CodeBuf *cb, int op, Cntxt *cntxt)
{
    if (dots_or_missing(CDR(e))) {
	return cmp_builtin(e, cb, cntxt, FALSE);
    }
    else if (lang_length(e) != 3) {
	notify_wrong_arg_count(name_string(CAR(e)), cntxt, cb);
	return cmp_builtin(e, cb, cntxt, FALSE);
    }
    else {
	Cntxt ncntxt = make_nontailcall_context(cntxt);
	cmp(CADR(e), cb, &ncntxt, FALSE, TRUE);
	Cntxt acntxt = make_arg_context(cntxt);
	cmp(CADDR(e), cb, &acntxt, FALSE, TRUE);
	int ci = putconst(cb, e);
	put1(cb, op, ci);
	if (cntxt->tailcall)
	    put0(cb, RETURN_OP);
	return TRUE;
    }
}

static int h_log(SEXP e, CodeBuf *cb, Cntxt *cntxt)
{
    int n = lang_length(e);
    if (dots_or_missing(e) || has_tags(e) || n < 2 || n > 3)
	return cmp_special(e, cb, cntxt);
    int ci = putconst(cb, e);
    Cntxt ncntxt = make_nontailcall_context(cntxt);
    cmp(CADR(e), cb, &ncntxt, FALSE, TRUE);
    if (n == 2) {
	put1(cb, LOG_OP, ci);
    }
    else {
	Cntxt acntxt = make_arg_context(cntxt);
	cmp(CADDR(e), cb, &acntxt, FALSE, TRUE);
	put1(cb, LOGBASE_OP, ci);
    }
    if (cntxt->tailcall)
	put0(cb, RETURN_OP);
    return TRUE;
}

static int cmp_math1(SEXP e, CodeBuf *cb, int idx, Cntxt *cntxt)
{
    if (dots_or_missing(CDR(e))) {
	return cmp_builtin(e, cb, cntxt, FALSE);
    }
    else if (lang_length(e) != 2) {
	notify_wrong_arg_count(name_string(CAR(e)), cntxt, cb);
	return cmp_builtin(e, cb, cntxt, FALSE);
    }
    else {
	Cntxt ncntxt = make_nontailcall_context(cntxt);
	cmp(CADR(e), cb, &ncntxt, FALSE, TRUE);
	int ci = putconst(cb, e);
	put2(cb, MATH1_OP, ci, idx);
	if (cntxt->tailcall)
	    put0(cb, RETURN_OP);
	return TRUE;
    }
}

static int h_paren(SEXP e, CodeBuf *cb, Cntxt *cntxt)
{
    if (any_dots(e))
	return cmp_builtin(e, cb, cntxt, FALSE); /* punt */
    else if (lang_length(e) != 2) {
	notify_wrong_arg_count("(", cntxt, cb);
	return cmp_builtin(e, cb, cntxt, FALSE); /* punt */
    }
    else if (cntxt->tailcall) {
	Cntxt ncntxt = make_nontailcall_context(cntxt);
	cmp(CADR(e), cb, &ncntxt, FALSE, TRUE);
	put0(cb, VISIBLE_OP);
	put0(cb, RETURN_OP);
	return TRUE;
    }
    else {
	cmp(CADR(e), cb, cntxt, FALSE, TRUE);
	return TRUE;
    }
}


/*
 * Inline handlers for general BUILTIN and SPECIAL functions
 */

static int cmp_builtin(SEXP e, CodeBuf *cb, Cntxt *cntxt, int internal)
{
    SEXP fun = CAR(e);
    SEXP args = CDR(e);
    if (dots_or_missing(args))
	return FALSE;
    int ci = putconst(cb, fun);
    put1(cb, internal ? GETINTLBUILTIN_OP : GETBUILTIN_OP, ci);
    cmp_builtin_args(args, cb, cntxt, FALSE);
    ci = putconst(cb, e);
    put1(cb, CALLBUILTIN_OP, ci);
    if (cntxt->tailcall)
	put0(cb, RETURN_OP);
    return TRUE;
}

static void cmp_builtin_args(SEXP args, CodeBuf *cb, Cntxt *cntxt, int missingOK)
{
    Cntxt ncntxt = make_arg_context(cntxt);
    for (SEXP p = args; p != R_NilValue; p = CDR(p)) {
	bc_tick();
	SEXP a = CAR(p);
	SEXP n = TAG(p);
	if (a == R_MissingArg) {
	    if (missingOK) {
		put0(cb, DOMISSING_OP);
		cmp_tag(n, cb);
	    }
	    else {
		Rf_errorcall(cntxt->call, _("missing arguments are not allowed"));
	    }
	}
	else if (TYPEOF(a) == BCODESXP) {
	    Rf_errorcall(cntxt->call, _("cannot compile byte code literals in code"));
	}
	else if (TYPEOF(a) == PROMSXP) {
	    Rf_errorcall(cntxt->call, _("cannot compile promise literals in code"));
	}
	else {
	    if (TYPEOF(a) == SYMSXP) {
		SEXP ca;
		if (! constant_fold(a, cntxt, &ca)) {
		    cmp_sym(a, cb, &ncntxt, missingOK);
		    put0(cb, PUSHARG_OP);
		}
		else {
		    PROTECT(ca);
		    cmp_const_arg(ca, cb);
		    UNPROTECT(1);
		}
	    }
	    else if (TYPEOF(a) == LANGSXP) {
		cmp(a, cb, &ncntxt, FALSE, TRUE);
		put0(cb, PUSHARG_OP);
	    }
	    else {
		cmp_const_arg(a, cb);
	    }
	    cmp_tag(n, cb);
	}
    }
}

static int cmp_special(SEXP e, CodeBuf *cb, Cntxt *cntxt)
{
    int ci = putconst(cb, e);
    put1(cb, CALLSPECIAL_OP, ci);
    if (cntxt->tailcall)
	put0(cb, RETURN_OP);
    return TRUE;
}

static int cmp_dot_internal_call(SEXP e, CodeBuf *cb, Cntxt *cntxt)
{
    if (CDR(e) == R_NilValue)
	Rf_errorcall(cntxt->call, _("subscript out of bounds"));
    SEXP ee = CADR(e);
    if (TYPEOF(ee) != LANGSXP)
	Rf_errorcall(cntxt->call, _("object of type '%s' is not subsettable"),
		     Rf_type2char(TYPEOF(ee)));
    SEXP sym = CAR(ee);
    if (TYPEOF(sym) != SYMSXP)
	Rf_errorcall(cntxt->call, _("invalid symbol"));
    if (is_builtin_internal(sym))
	return cmp_builtin(ee, cb, cntxt, TRUE);
    else
	return cmp_special(e, cb, cntxt);
}


/*
 * Inline handlers for subsetting and related operators
 */

static int cmp_dispatch(int start_op, int dflt_op, SEXP e, CodeBuf *cb,
			Cntxt *cntxt, int missingOK)
{
    if ((missingOK && any_dots(e)) || (! missingOK && dots_or_missing(e)) ||
	lang_length(e) == 1)
	return cmp_special(e, cb, cntxt); /* punt */
    int ne = lang_length(e);
    SEXP oe = CADR(e);
    if (oe == R_MissingArg)
	return cmp_special(e, cb, cntxt); /* punt */
    Cntxt ncntxt = make_arg_context(cntxt);
    cmp(oe, cb, &ncntxt, FALSE, TRUE);
    int ci = putconst(cb, e);
    int end_label = makelabel(cb);
    put1L(cb, start_op, ci, end_label);
    if (ne > 2)
	cmp_builtin_args(CDDR(e), cb, cntxt, missingOK);
    put0(cb, dflt_op);
    putlabel(cb, end_label);
    if (cntxt->tailcall)
	put0(cb, RETURN_OP);
    return TRUE;
}

/* `$`: a character member name is converted to a symbol */
static SEXP dollar_member(SEXP x)
{
    if (TYPEOF(x) == STRSXP && XLENGTH(x) == 1 &&
	STRING_ELT(x, 0) != NA_STRING && CHAR(STRING_ELT(x, 0))[0] != '\0')
	return Rf_installTrChar(STRING_ELT(x, 0));
    return x;
}

static int h_dollar(SEXP e, CodeBuf *cb, Cntxt *cntxt)
{
    if (any_dots(e) || lang_length(e) != 3)
	return cmp_special(e, cb, cntxt);
    SEXP sym = dollar_member(CADDR(e));
    if (TYPEOF(sym) == SYMSXP) {
	Cntxt ncntxt = make_arg_context(cntxt);
	cmp(CADR(e), cb, &ncntxt, FALSE, TRUE);
	int ci = putconst(cb, e);
	int csi = putconst(cb, sym);
	put2(cb, DOLLAR_OP, ci, csi);
	if (cntxt->tailcall)
	    put0(cb, RETURN_OP);
	return TRUE;
    }
    else {
	return cmp_special(e, cb, cntxt);
    }
}


/*
 * Inline handler for local() and return() functions
 */

static int h_local(SEXP e, CodeBuf *cb, Cntxt *cntxt)
{
    if (lang_length(e) == 2) {
	/* (function() <body>)() */
	SEXP fn = PROTECT(Rf_lang4(R_FunctionSymbol, R_NilValue, CADR(e),
				   R_NilValue));
	SEXP ee = PROTECT(Rf_lang1(fn));
	cmp(ee, cb, cntxt, FALSE, TRUE);
	UNPROTECT(2);
	return TRUE;
    }
    else {
	return FALSE;
    }
}

static int h_return(SEXP e, CodeBuf *cb, Cntxt *cntxt)
{
    if (dots_or_missing(e) || lang_length(e) > 2)
	return cmp_special(e, cb, cntxt); /* punt for now */
    SEXP val = lang_length(e) == 1 ? R_NilValue : CADR(e);
    Cntxt ncntxt = make_nontailcall_context(cntxt);
    cmp(val, cb, &ncntxt, FALSE, TRUE);
    put0(cb, cntxt->needRETURNJMP ? RETURNJMP_OP : RETURN_OP);
    return TRUE;
}


/*
 * Inline handlers for the family of is.xyz primitives
 */

static int cmp_is(int op, SEXP e, CodeBuf *cb, Cntxt *cntxt)
{
    if (any_dots(e) || lang_length(e) != 2)
	return cmp_builtin(e, cb, cntxt, FALSE);
    Cntxt s = make_arg_context(cntxt);
    cmp(CADR(e), cb, &s, FALSE, TRUE);
    put0(cb, op);
    if (cntxt->tailcall)
	put0(cb, RETURN_OP);
    return TRUE;
}


/*
 * Inline handlers for some .Internal functions (cmpSimpleInternal)
 */

/* simpleFormals(def) */
static int simple_formals(SEXP def)
{
    for (SEXP f = FORMALS(def); f != R_NilValue; f = CDR(f)) {
	if (TAG(f) == R_DotsSymbol)
	    return FALSE;
	SEXP d = CAR(f);
	if (d != R_MissingArg)
	    switch (TYPEOF(d)) {
	    case SYMSXP: case LANGSXP: case PROMSXP: case BCODESXP:
		return FALSE;
	    default:
		break;
	    }
    }
    return TRUE;
}

static int is_formal_name(SEXP forms, SEXP sym)
{
    for (SEXP f = forms; f != R_NilValue; f = CDR(f))
	if (TAG(f) == sym)
	    return TRUE;
    return FALSE;
}

/* simpleArgs(icall, fnames) */
static int simple_args(SEXP icall, SEXP forms)
{
    for (SEXP p = CDR(icall); p != R_NilValue; p = CDR(p)) {
	SEXP a = CAR(p);
	if (a == R_MissingArg)
	    return FALSE;
	switch (TYPEOF(a)) {
	case SYMSXP:
	    if (! is_formal_name(forms, a))
		return FALSE;
	    break;
	case LANGSXP: case PROMSXP: case BCODESXP:
	    return FALSE;
	default:
	    break;
	}
    }
    return TRUE;
}

/* the .Internal call in the body of a simple internal, or NULL */
static SEXP simple_internal_call(SEXP def, Cntxt *cntxt)
{
    if (TYPEOF(def) != CLOSXP || ! simple_formals(def))
	return NULL;
    SEXP b = R_ClosureExpr(def);
    if (TYPEOF(b) == LANGSXP && lang_length(b) == 2 && CAR(b) == R_BraceSymbol)
	b = CADR(b);
    if (TYPEOF(b) == LANGSXP && CAR(b) == s_dotInternal) {
	if (CDR(b) == R_NilValue)
	    Rf_errorcall(cntxt->call, _("subscript out of bounds"));
	SEXP icall = CADR(b);
	if (TYPEOF(icall) != LANGSXP)
	    Rf_errorcall(cntxt->call,
			 _("object of type '%s' is not subsettable"),
			 Rf_type2char(TYPEOF(icall)));
	SEXP ifun = CAR(icall);
	if (TYPEOF(ifun) == SYMSXP && is_builtin_internal(ifun) &&
	    simple_args(icall, FORMALS(def)))
	    return icall;
    }
    return NULL;
}

typedef struct {
    SEXP call, argument, formal;
} PartialMatchData;

static SEXP signal_partial_match_body(void *data)
{
    PartialMatchData *pd = (PartialMatchData *) data;
#ifdef BCCOMPILE_STANDALONE
    Rf_warningcall(pd->call, _("partial argument match of '%s' to '%s'"),
		   CHAR(PRINTNAME(pd->argument)), CHAR(PRINTNAME(pd->formal)));
#else
    SEXP cond = PROTECT(R_makePartialArgumentMatchWarningCondition(
			    pd->call, pd->argument, pd->formal));
    R_signalWarningCondition(cond);
    UNPROTECT(1);
#endif
    return R_NilValue;
}

static SEXP signal_partial_match_handler(SEXP cond, void *data)
{
    return R_TrueValue;
}

/* signal the partial argument match warning of match.call(); TRUE when
   it was turned into an error */
static int signal_partial_match(SEXP call, SEXP argument, SEXP formal)
{
    PartialMatchData pd = { call, argument, formal };
    SEXP val = R_tryCatchError(signal_partial_match_body, &pd,
			       signal_partial_match_handler, &pd);
    return val == R_TrueValue;
}

/* Match the supplied arguments of call to the formals of def as
   match.call() does for formals without `...`. Returns FALSE when
   match.call() would signal an error. matched[i] receives the i-th
   formal's supplied argument or NULL. */
static int match_simple_args(SEXP forms, SEXP call, SEXP *matched)
{
    /* match.call() warns about partial matches when the option
       warnPartialMatchArgs is set; if the warning becomes an error
       (warn >= 2, unless a handler muffles it) checkCall() turns that
       into not inlining the call */
    int warnPartial = Rf_asLogical(Rf_GetOption1(Rf_install("warnPartialMatchArgs"))) == TRUE;

    int nf = lang_length(forms);
    int ns = lang_length(CDR(call));
    SEXP *supplied = (SEXP *) R_alloc(ns > 0 ? ns : 1, sizeof(SEXP));
    SEXP *stags = (SEXP *) R_alloc(ns > 0 ? ns : 1, sizeof(SEXP));
    int *sused = (int *) R_alloc(ns > 0 ? ns : 1, sizeof(int));
    int *fused = (int *) R_alloc(nf > 0 ? nf : 1, sizeof(int));
    int i = 0;
    for (SEXP p = CDR(call); p != R_NilValue; p = CDR(p), i++) {
	supplied[i] = CAR(p);
	stags[i] = TAG(p);
	sused[i] = 0;
    }
    SEXP *ftags = (SEXP *) R_alloc(nf > 0 ? nf : 1, sizeof(SEXP));
    i = 0;
    for (SEXP f = forms; f != R_NilValue; f = CDR(f), i++) {
	ftags[i] = TAG(f);
	fused[i] = 0;
	matched[i] = NULL;
    }

    /* exact matches by tag */
    for (int fi = 0; fi < nf; fi++) {
	const char *fname = CHAR(PRINTNAME(ftags[fi]));
	for (int si = 0; si < ns; si++) {
	    if (stags[si] == R_NilValue)
		continue;
	    if (strcmp(CHAR(PRINTNAME(stags[si])), fname) == 0) {
		if (fused[fi] == 2)
		    return FALSE; /* matched by multiple actual arguments */
		fused[fi] = 2;
		sused[si] = 2;
		matched[fi] = supplied[si];
	    }
	}
    }
    /* partial matches */
    for (int fi = 0; fi < nf; fi++) {
	if (fused[fi])
	    continue;
	const char *fname = CHAR(PRINTNAME(ftags[fi]));
	for (int si = 0; si < ns; si++) {
	    if (sused[si] == 2 || stags[si] == R_NilValue)
		continue;
	    const char *tag = CHAR(PRINTNAME(stags[si]));
	    if (tag[0] != '\0' && strncmp(fname, tag, strlen(tag)) == 0) {
		if (sused[si])
		    return FALSE; /* matches multiple formal arguments */
		if (fused[fi] == 1)
		    return FALSE; /* matched by multiple actual arguments */
		if (warnPartial &&
		    signal_partial_match(call, stags[si], ftags[fi]))
		    return FALSE; /* the warning became an error */
		sused[si] = 1;
		fused[fi] = 1;
		matched[fi] = supplied[si];
	    }
	}
    }
    /* positional matches */
    int fi = 0;
    for (int si = 0; si < ns; si++) {
	if (sused[si])
	    continue;
	if (stags[si] != R_NilValue && CHAR(PRINTNAME(stags[si]))[0] != '\0')
	    return FALSE; /* unused argument */
	while (fi < nf && fused[fi])
	    fi++;
	if (fi == nf)
	    return FALSE; /* unused argument */
	fused[fi] = 1;
	matched[fi] = supplied[si];
	fi++;
    }
    return TRUE;
}

static int cmp_simple_internal(SEXP e, CodeBuf *cb, Cntxt *cntxt)
{
    if (any_dots(e))
	return FALSE;
    SEXP def = find_fun_def(CAR(e), cntxt->env);
    /* checkCall(def, e, NULL) */
    if (def == NULL || TYPEOF(def) == BUILTINSXP || TYPEOF(def) == SPECIALSXP ||
	TYPEOF(def) != CLOSXP)
	/* checkCall() returns NA for these and the R code errors on it */
	Rf_errorcall(cntxt->call,
		     _("missing value where TRUE/FALSE needed"));
    PROTECT(def);
    SEXP forms = FORMALS(def);
    int nf = lang_length(forms);
    SEXP *matched = (SEXP *) R_alloc(nf > 0 ? nf : 1, sizeof(SEXP));
    if (! match_simple_args(forms, e, matched)) {
	UNPROTECT(1);
	return FALSE;
    }

    /* inlineSimpleInternalCall(e, def) */
    if (dots_or_missing(e)) {
	UNPROTECT(1);
	return FALSE;
    }
    SEXP icall = simple_internal_call(def, cntxt);
    if (icall == NULL) {
	UNPROTECT(1);
	return FALSE;
    }
    SEXP args = PROTECT(copy_call(CDR(icall)));
    for (SEXP p = args; p != R_NilValue; p = CDR(p)) {
	SEXP a = CAR(p);
	if (TYPEOF(a) == SYMSXP) {
	    SEXP val = R_NilValue;
	    int i = 0;
	    for (SEXP f = forms; f != R_NilValue; f = CDR(f), i++)
		if (TAG(f) == a) {
		    val = matched[i] != NULL ? matched[i] : CAR(f);
		    break;
		}
	    SETCAR(p, val);
	}
    }
    SEXP ncall = PROTECT(Rf_lang2(s_dotInternal, LCONS(CAR(icall), args)));
    int ans = cmp_dot_internal_call(ncall, cb, cntxt);
    UNPROTECT(3);
    return ans;
}


/*
 * Inline handler for .Call
 */

#define DOTCALL_MAX 16 /* should match DOTCALL_MAX in eval.c */

static int h_dotcall(SEXP e, CodeBuf *cb, Cntxt *cntxt)
{
    int n = lang_length(e);
    if (dots_or_missing(CDR(e)) || has_tags(e) || n < 2 || n > DOTCALL_MAX + 2)
	return cmp_builtin(e, cb, cntxt, FALSE); /* punt */
    Cntxt ncntxt = make_nontailcall_context(cntxt);
    cmp(CADR(e), cb, &ncntxt, FALSE, TRUE);
    int nargs = n - 2;
    if (nargs > 0) {
	Cntxt acntxt = make_arg_context(cntxt);
	for (SEXP p = CDDR(e); p != R_NilValue; p = CDR(p))
	    cmp(CAR(p), cb, &acntxt, FALSE, TRUE);
    }
    int ci = putconst(cb, e);
    put2(cb, DOTCALL_OP, ci, nargs);
    if (cntxt->tailcall)
	put0(cb, RETURN_OP);
    return TRUE;
}


/*
 * Inline handlers to control warnings
 */

static int good_multicolon_arg(SEXP a)
{
    return TYPEOF(a) == SYMSXP || (TYPEOF(a) == STRSXP && XLENGTH(a) == 1);
}

static int cmp_multicolon(SEXP e, CodeBuf *cb, Cntxt *cntxt)
{
    if (! dots_or_missing(e) && lang_length(e) == 3) {
	SEXP fun = CAR(e);
	SEXP x = CADR(e);
	SEXP y = CADDR(e);
	if (good_multicolon_arg(x) && good_multicolon_arg(y)) {
	    SEXP xs = PROTECT(Rf_ScalarString(TYPEOF(x) == SYMSXP ?
					      PRINTNAME(x) : STRING_ELT(x, 0)));
	    SEXP ys = PROTECT(Rf_ScalarString(TYPEOF(y) == SYMSXP ?
					      PRINTNAME(y) : STRING_ELT(y, 0)));
	    SEXP args = PROTECT(Rf_list2(xs, ys));
	    cmp_call_sym_fun(fun, args, e, cb, cntxt);
	    UNPROTECT(3);
	    return TRUE;
	}
	else {
	    return FALSE;
	}
    }
    else {
	return FALSE;
    }
}

static int h_suppress_undef(SEXP e, CodeBuf *cb, Cntxt *cntxt)
{
    Cntxt ncntxt = *cntxt;
    ncntxt.suppressUndefined = R_TrueValue;
    cmp_call_sym_fun(CAR(e), CDR(e), e, cb, &ncntxt);
    return TRUE;
}


/*
 * Assignment expressions
 */

static int check_assign(SEXP e, Cntxt *cntxt, CodeBuf *cb)
{
    if (lang_length(e) != 3)
	return FALSE;
    SEXP place = CADR(e);
    if (TYPEOF(place) == SYMSXP ||
	(TYPEOF(place) == STRSXP && XLENGTH(place) == 1))
	return TRUE;
    while (TYPEOF(place) == LANGSXP) {
	SEXP fun = CAR(place);
	if (TYPEOF(fun) != SYMSXP &&
	    ! (TYPEOF(fun) == LANGSXP && lang_length(fun) == 3 &&
	       TYPEOF(CAR(fun)) == SYMSXP &&
	       (CAR(fun) == R_DoubleColonSymbol ||
		CAR(fun) == R_TripleColonSymbol))) {
	    notify_bad_assign_fun(cntxt, cb);
	    return FALSE;
	}
	if (CDR(place) == R_NilValue)
	    Rf_errorcall(cntxt->call, _("subscript out of bounds"));
	place = CADR(place);
    }
    return TYPEOF(place) == SYMSXP;
}

static int cmp_symbol_assign(SEXP symbol, SEXP value, int superAssign,
			     CodeBuf *cb, Cntxt *cntxt)
{
    Cntxt ncntxt = make_nontailcall_context(cntxt);
    cmp(value, cb, &ncntxt, FALSE, TRUE);
    int ci = putconst(cb, symbol);
    put1(cb, superAssign ? SETVAR2_OP : SETVAR_OP, ci);
    if (cntxt->tailcall) {
	put0(cb, INVISIBLE_OP);
	put0(cb, RETURN_OP);
    }
    return TRUE;
}

/* getAssignFun(fun): `fun<-`, or NULL */
static SEXP get_assign_fun(SEXP fun)
{
    if (TYPEOF(fun) == SYMSXP) {
	const char *name = CHAR(PRINTNAME(fun));
	size_t n = strlen(name);
	char *buf = R_alloc(n + 3, 1);
	strcpy(buf, name);
	strcpy(buf + n, "<-");
	return Rf_install(buf);
    }
    /* check for and handle foo::bar(x) <- y assignments here */
    if (TYPEOF(fun) == LANGSXP && lang_length(fun) == 3) {
	const char *head = name_string(CAR(fun));
	if ((TYPEOF(CAR(fun)) == SYMSXP || TYPEOF(CAR(fun)) == STRSXP) &&
	    (strcmp(head, "::") == 0 || strcmp(head, ":::") == 0) &&
	    TYPEOF(CADR(fun)) == SYMSXP && TYPEOF(CADDR(fun)) == SYMSXP) {
	    SEXP afun = PROTECT(copy_call(fun));
	    SETCAR(CDDR(afun), get_assign_fun(CADDR(fun)));
	    UNPROTECT(1);
	    return afun;
	}
    }
    return NULL;
}

/* as.call(c(afun, as.list(place[-1]), list(value = vexpr))) */
static SEXP make_assign_call(SEXP afun, SEXP place, SEXP vexpr)
{
    int n = lang_length(place); /* afun + args of place + value */
    SEXP ans = PROTECT(Rf_allocList(n + 1));
    SET_TYPEOF(ans, LANGSXP);
    SETCAR(ans, afun);
    SEXP p = CDR(ans);
    for (SEXP q = CDR(place); q != R_NilValue; q = CDR(q), p = CDR(p)) {
	SETCAR(p, CAR(q));
	SET_TAG(p, TAG(q));
    }
    SETCAR(p, vexpr);
    SET_TAG(p, s_value);
    UNPROTECT(1);
    return ans;
}

static void cmp_setter_call(SEXP place, SEXP origplace, SEXP vexpr, CodeBuf *cb,
			    Cntxt *cntxt)
{
    SEXP afun = get_assign_fun(CAR(place));
    PROTECT(afun == NULL ? R_NilValue : afun);
    SEXP acall = PROTECT(make_assign_call(afun == NULL ? R_NilValue : afun,
					  place, vexpr));
    if (CDR(acall) != R_NilValue)
	SETCAR(CDR(acall), s_tmp);
    Cntxt ncntxt = make_call_context(cntxt, acall);
    Loc sloc = savecurloc(cb);
    SEXP cexpr = PROTECT(make_assign_call(afun == NULL ? R_NilValue : afun,
					  origplace, vexpr));
    setcurexpr(cb, cexpr);
    if (afun == NULL) {
	Rf_errorcall(cntxt->call, _("invalid function in complex assignment"));
    }
    else if (TYPEOF(afun) == SYMSXP) {
	if (! try_setter_inline(afun, place, origplace, acall, cb, &ncntxt)) {
	    int ci = putconst(cb, afun);
	    put1(cb, GETFUN_OP, ci);
	    put0(cb, PUSHNULLARG_OP);
	    cmp_call_args(CDDR(place), cb, &ncntxt, FALSE);
	    int cci = putconst(cb, acall);
	    int cvi = putconst(cb, vexpr);
	    put2(cb, SETTER_CALL_OP, cci, cvi);
	}
    }
    else {
	cmp(afun, cb, &ncntxt, FALSE, TRUE);
	put0(cb, CHECKFUN_OP);
	put0(cb, PUSHNULLARG_OP);
	cmp_call_args(CDDR(place), cb, &ncntxt, FALSE);
	int cci = putconst(cb, acall);
	int cvi = putconst(cb, vexpr);
	put2(cb, SETTER_CALL_OP, cci, cvi);
    }
    restorecurloc(cb, sloc);
    UNPROTECT(3);
}

static void cmp_getter_call(SEXP place, SEXP origplace, CodeBuf *cb,
			    Cntxt *cntxt)
{
    Cntxt ncntxt = make_call_context(cntxt, place);
    Loc sloc = savecurloc(cb);
    setcurexpr(cb, origplace);
    SEXP fun = CAR(place);
    if (TYPEOF(fun) == SYMSXP) {
	if (! try_getter_inline(place, cb, &ncntxt)) {
	    int ci = putconst(cb, fun);
	    put1(cb, GETFUN_OP, ci);
	    put0(cb, PUSHNULLARG_OP);
	    cmp_call_args(CDDR(place), cb, &ncntxt, FALSE);
	    int cci = putconst(cb, place);
	    put1(cb, GETTER_CALL_OP, cci);
	    put0(cb, SWAP_OP);
	}
    }
    else {
	cmp(fun, cb, &ncntxt, FALSE, TRUE);
	put0(cb, CHECKFUN_OP);
	put0(cb, PUSHNULLARG_OP);
	cmp_call_args(CDDR(place), cb, &ncntxt, FALSE);
	int cci = putconst(cb, place);
	put1(cb, GETTER_CALL_OP, cci);
	put0(cb, SWAP_OP);
    }
    restorecurloc(cb, sloc);
}

static int cmp_complex_assign(SEXP symbol, SEXP lhs, SEXP value, int superAssign,
			      CodeBuf *cb, Cntxt *cntxt)
{
    int startOP, endOP;
    if (superAssign) {
	startOP = STARTASSIGN2_OP;
	endOP = ENDASSIGN2_OP;
    }
    else {
	if (! cntxt->suppressAll && ! find_var(symbol, cntxt->env))
	    notify_undef_var(symbol, cntxt, cb);
	startOP = STARTASSIGN_OP;
	endOP = ENDASSIGN_OP;
    }
    if (! cntxt->toplevel)
	put0(cb, INCLNKSTK_OP);
    Cntxt vcntxt = cntxt->toplevel ? make_nontailcall_context(cntxt) :
	make_arg_context(cntxt);
    cmp(value, cb, &vcntxt, FALSE, TRUE);
    int csi = putconst(cb, symbol);
    put1(cb, startOP, csi);

    Cntxt ncntxt = make_arg_context(cntxt);

    /* flattenPlace(lhs, cntxt) */
    int nplaces = 0;
    for (SEXP place = lhs; TYPEOF(place) == LANGSXP; place = CADR(place)) {
	if (lang_length(place) < 2)
	    Rf_errorcall(cntxt->call, _("bad assignment 1"));
	nplaces++;
    }
    SEXP places = PROTECT(Rf_allocVector(VECSXP, nplaces));
    SEXP origplaces = PROTECT(Rf_allocVector(VECSXP, nplaces));
    {
	SEXP place = lhs;
	for (int i = 0; i < nplaces; i++, place = CADR(place)) {
	    SET_VECTOR_ELT(origplaces, i, place);
	    SET_VECTOR_ELT(places, i, set_lang_elt(place, 1, s_tmp));
	}
	if (TYPEOF(place) != SYMSXP)
	    Rf_errorcall(cntxt->call, _("bad assignment 2"));
    }

    for (int i = nplaces - 1; i >= 1; i--)
	cmp_getter_call(VECTOR_ELT(places, i), VECTOR_ELT(origplaces, i), cb,
			&ncntxt);
    cmp_setter_call(VECTOR_ELT(places, 0), VECTOR_ELT(origplaces, 0), value, cb,
		    &ncntxt);
    for (int i = 1; i < nplaces; i++)
	cmp_setter_call(VECTOR_ELT(places, i), VECTOR_ELT(origplaces, i), s_vtmp,
			cb, &ncntxt);
    UNPROTECT(2);

    put1(cb, endOP, csi);
    if (! cntxt->toplevel)
	put0(cb, DECLNKSTK_OP);
    if (cntxt->tailcall) {
	put0(cb, INVISIBLE_OP);
	put0(cb, RETURN_OP);
    }
    return TRUE;
}

static int cmp_assign(SEXP e, CodeBuf *cb, Cntxt *cntxt)
{
    if (! check_assign(e, cntxt, cb))
	return cmp_special(e, cb, cntxt);
    int superAssign = CAR(e) == Rf_install("<<-");
    SEXP lhs = CADR(e);
    SEXP value = CADDR(e);
    SEXP symbol = get_assigned_var(e, cntxt);
    if (superAssign && ! cntxt->suppressAll && ! find_var(symbol, cntxt->env))
	notify_no_super_assign_var(symbol, cntxt, cb);
    if (TYPEOF(lhs) == SYMSXP || TYPEOF(lhs) == STRSXP)
	return cmp_symbol_assign(symbol, value, superAssign, cb, cntxt);
    else if (TYPEOF(lhs) == LANGSXP)
	return cmp_complex_assign(symbol, lhs, value, superAssign, cb, cntxt);
    else
	return cmp_special(e, cb, cntxt); /* punt for now */
}

static int cmp_setter_dispatch(int start_op, int dflt_op, SEXP place, SEXP call,
			       CodeBuf *cb, Cntxt *cntxt)
{
    if (any_dots(place))
	return FALSE; /* punt */
    int ci = putconst(cb, call);
    int end_label = makelabel(cb);
    put1L(cb, start_op, ci, end_label);
    if (lang_length(place) > 2)
	cmp_builtin_args(CDDR(place), cb, cntxt, TRUE);
    put0(cb, dflt_op);
    putlabel(cb, end_label);
    return TRUE;
}

static int cmp_getter_dispatch(int start_op, int dflt_op, SEXP call,
			       CodeBuf *cb, Cntxt *cntxt)
{
    if (any_dots(call))
	return FALSE; /* punt */
    int ci = putconst(cb, call);
    int end_label = makelabel(cb);
    put0(cb, DUP2ND_OP);
    put1L(cb, start_op, ci, end_label);
    if (lang_length(call) > 2)
	cmp_builtin_args(CDDR(call), cb, cntxt, TRUE);
    put0(cb, dflt_op);
    putlabel(cb, end_label);
    put0(cb, SWAP_OP);
    return TRUE;
}

/* a member name for `$<-`/`$` getters and setters: as.name(sym) */
static SEXP member_symbol(SEXP sym, Cntxt *cntxt)
{
    if (TYPEOF(sym) == STRSXP) {
	if (XLENGTH(sym) == 0)
	    Rf_errorcall(cntxt->call, _("invalid type/length (symbol/0)"));
	if (STRING_ELT(sym, 0) == NA_STRING)
	    return Rf_install("NA");
	if (CHAR(STRING_ELT(sym, 0))[0] == '\0')
	    Rf_errorcall(cntxt->call,
			 _("attempt to use zero-length variable name"));
	return Rf_installTrChar(STRING_ELT(sym, 0));
    }
    return sym;
}

static int s_dollar(SEXP place, SEXP call, CodeBuf *cb, Cntxt *cntxt)
{
    if (any_dots(place) || lang_length(place) != 3)
	return FALSE;
    SEXP sym = member_symbol(CADDR(place), cntxt);
    if (TYPEOF(sym) == SYMSXP) {
	int ci = putconst(cb, call);
	int csi = putconst(cb, sym);
	put2(cb, DOLLARGETS_OP, ci, csi);
	return TRUE;
    }
    return FALSE;
}

static int s_at(SEXP place, SEXP origplace, SEXP acall, CodeBuf *cb,
		Cntxt *cntxt)
{
    if (! dots_or_missing(place) && lang_length(place) == 3 &&
	TYPEOF(CADDR(place)) == SYMSXP) {
	SEXP slot = PROTECT(Rf_ScalarString(PRINTNAME(CADDR(place))));
	SEXP nplace = PROTECT(set_lang_elt(place, 2, slot));
	SEXP vexpr = lang_elt(acall, lang_length(acall) - 1);
	cmp_setter_call(nplace, origplace, vexpr, cb, cntxt);
	UNPROTECT(2);
	return TRUE;
    }
    return FALSE;
}

/* the default instruction for 1, 2 or n indices; *rank set when the
   instruction takes the number of indices */
static int subset_dflt_op(int nidx, int op1, int op2, int opn, int *rank)
{
    if (nidx == 1) {
	*rank = FALSE;
	return op1;
    }
    else if (nidx == 2) {
	*rank = FALSE;
	return op2;
    }
    else {
	*rank = TRUE;
	return opn;
    }
}

/* cmpIndices(indices, cb, cntxt); checkNeedsInc() is always FALSE */
static void cmp_indices(SEXP indices, CodeBuf *cb, Cntxt *cntxt)
{
    for (SEXP p = indices; p != R_NilValue; p = CDR(p))
	cmp(CAR(p), cb, cntxt, TRUE, TRUE);
}

static int cmp_subassign_dispatch(int start_op, int dflt_op, int rank,
				  SEXP place, SEXP call, CodeBuf *cb,
				  Cntxt *cntxt)
{
    int ci = putconst(cb, call);
    int label = makelabel(cb);
    put1L(cb, start_op, ci, label);
    SEXP indices = CDDR(place);
    cmp_indices(indices, cb, cntxt);
    if (rank)
	put2(cb, dflt_op, ci, lang_length(indices));
    else
	put1(cb, dflt_op, ci);
    putlabel(cb, label);
    return TRUE;
}

static int s_subassign(SEXP place, SEXP call, CodeBuf *cb, Cntxt *cntxt,
		       int which2)
{
    if (dots_or_missing(place) || has_tags(place) || lang_length(place) < 3)
	return which2 ?
	    cmp_setter_dispatch(STARTSUBASSIGN2_OP, DFLTSUBASSIGN2_OP, place,
				call, cb, cntxt) :
	    cmp_setter_dispatch(STARTSUBASSIGN_OP, DFLTSUBASSIGN_OP, place,
				call, cb, cntxt);
    int nidx = lang_length(place) - 2;
    int rank;
    int dflt_op = which2 ?
	subset_dflt_op(nidx, VECSUBASSIGN2_OP, MATSUBASSIGN2_OP,
		       SUBASSIGN2_N_OP, &rank) :
	subset_dflt_op(nidx, VECSUBASSIGN_OP, MATSUBASSIGN_OP,
		       SUBASSIGN_N_OP, &rank);
    return cmp_subassign_dispatch(which2 ? STARTSUBASSIGN2_N_OP :
				  STARTSUBASSIGN_N_OP,
				  dflt_op, rank, place, call, cb, cntxt);
}

static int try_setter_inline(SEXP afun, SEXP place, SEXP origplace, SEXP call,
			     CodeBuf *cb, Cntxt *cntxt)
{
    SymInfo *si = symtab_lookup(afun, FALSE);
    if (si == NULL || si->setter == S_NONE)
	return FALSE;
    int setter = si->setter; /* see try_inline() */
    InlineInfo info = get_inline_info(afun, cntxt, FALSE);
    if (! info.found || info.package != PKG_BASE)
	return FALSE;
    switch (setter) {
    case S_DOLLAR:
	return s_dollar(place, call, cb, cntxt);
    case S_AT:
	return s_at(place, origplace, call, cb, cntxt);
    case S_SUBASSIGN:
	return s_subassign(place, call, cb, cntxt, FALSE);
    case S_SUBASSIGN2:
	return s_subassign(place, call, cb, cntxt, TRUE);
    default:
	return FALSE;
    }
}

static int g_dollar(SEXP call, CodeBuf *cb, Cntxt *cntxt)
{
    if (any_dots(call) || lang_length(call) != 3)
	return FALSE;
    SEXP sym = member_symbol(CADDR(call), cntxt);
    if (TYPEOF(sym) == SYMSXP) {
	int ci = putconst(cb, call);
	int csi = putconst(cb, sym);
	put0(cb, DUP2ND_OP);
	put2(cb, DOLLAR_OP, ci, csi);
	put0(cb, SWAP_OP);
	return TRUE;
    }
    return FALSE;
}

static int cmp_subset_getter_dispatch(int start_op, int dflt_op, int rank,
				      SEXP call, CodeBuf *cb, Cntxt *cntxt)
{
    int ci = putconst(cb, call);
    int end_label = makelabel(cb);
    put0(cb, DUP2ND_OP);
    put1L(cb, start_op, ci, end_label);
    SEXP indices = CDDR(call);
    cmp_indices(indices, cb, cntxt);
    if (rank)
	put2(cb, dflt_op, ci, lang_length(indices));
    else
	put1(cb, dflt_op, ci);
    putlabel(cb, end_label);
    put0(cb, SWAP_OP);
    return TRUE;
}

static int g_subset(SEXP call, CodeBuf *cb, Cntxt *cntxt, int which2)
{
    if (dots_or_missing(call) || has_tags(call) || lang_length(call) < 3)
	return which2 ?
	    cmp_getter_dispatch(STARTSUBSET2_OP, DFLTSUBSET2_OP, call, cb, cntxt) :
	    cmp_getter_dispatch(STARTSUBSET_OP, DFLTSUBSET_OP, call, cb, cntxt);
    int nidx = lang_length(call) - 2;
    int rank;
    int dflt_op = which2 ?
	subset_dflt_op(nidx, VECSUBSET2_OP, MATSUBSET2_OP, SUBSET2_N_OP, &rank) :
	subset_dflt_op(nidx, VECSUBSET_OP, MATSUBSET_OP, SUBSET_N_OP, &rank);
    return cmp_subset_getter_dispatch(which2 ? STARTSUBSET2_N_OP :
				      STARTSUBSET_N_OP,
				      dflt_op, rank, call, cb, cntxt);
}

static int try_getter_inline(SEXP call, CodeBuf *cb, Cntxt *cntxt)
{
    SEXP name = CAR(call);
    SymInfo *si = symtab_lookup(name, FALSE);
    if (si == NULL || si->getter == G_NONE)
	return FALSE;
    int getter = si->getter; /* see try_inline() */
    InlineInfo info = get_inline_info(name, cntxt, FALSE);
    if (! info.found || info.package != PKG_BASE)
	return FALSE;
    switch (getter) {
    case G_DOLLAR:
	return g_dollar(call, cb, cntxt);
    case G_SUBSET:
	return g_subset(call, cb, cntxt, FALSE);
    case G_SUBSET2:
	return g_subset(call, cb, cntxt, TRUE);
    default:
	return FALSE;
    }
}


/*
 * Improved subset handling: inline handlers for `[` and `[[`
 */

static int cmp_subset_dispatch(int start_op, int dflt_op, int rank, SEXP e,
			       CodeBuf *cb, Cntxt *cntxt)
{
    SEXP oe = CADR(e);
    if (oe == R_MissingArg)
	Rf_errorcall(cntxt->call, _("cannot compile this expression"));
    Cntxt ncntxt = make_arg_context(cntxt);
    int ci = putconst(cb, e);
    int label = makelabel(cb);
    cmp(oe, cb, &ncntxt, FALSE, TRUE);
    put1L(cb, start_op, ci, label);
    SEXP indices = CDDR(e);
    cmp_indices(indices, cb, &ncntxt);
    if (rank)
	put2(cb, dflt_op, ci, lang_length(indices));
    else
	put1(cb, dflt_op, ci);
    putlabel(cb, label);
    if (cntxt->tailcall)
	put0(cb, RETURN_OP);
    return TRUE;
}

static int h_subset(SEXP e, CodeBuf *cb, Cntxt *cntxt, int which2)
{
    if (dots_or_missing(e) || has_tags(e) || lang_length(e) < 3)
	return which2 ?
	    cmp_dispatch(STARTSUBSET2_OP, DFLTSUBSET2_OP, e, cb, cntxt, TRUE) :
	    cmp_dispatch(STARTSUBSET_OP, DFLTSUBSET_OP, e, cb, cntxt, TRUE);
    int nidx = lang_length(e) - 2;
    int rank;
    int dflt_op = which2 ?
	subset_dflt_op(nidx, VECSUBSET2_OP, MATSUBSET2_OP, SUBSET2_N_OP, &rank) :
	subset_dflt_op(nidx, VECSUBSET_OP, MATSUBSET_OP, SUBSET_N_OP, &rank);
    return cmp_subset_dispatch(which2 ? STARTSUBSET2_N_OP : STARTSUBSET_N_OP,
			       dflt_op, rank, e, cb, cntxt);
}


/*
 * Inline handler for switch
 */

/* The alternative names of a switch are the CHARSXPs of the tags, so
   equal names are the same pointer. A small open addressing table maps
   each distinct name to the first alternative with it. */
typedef struct {
    SEXP *keys;
    int *first;
    int size;
} NameIndex;

static void name_index_init(NameIndex *ni, int n)
{
    ni->size = 16;
    while (ni->size < 2 * (n + 1))
	ni->size *= 2;
    ni->keys = (SEXP *) R_alloc(ni->size, sizeof(SEXP));
    ni->first = (int *) R_alloc(ni->size, sizeof(int));
    memset(ni->keys, 0, ni->size * sizeof(SEXP));
}

/* returns the first index recorded for the name, or -1 and records i */
static int name_index_add(NameIndex *ni, SEXP name, int i)
{
    unsigned h = ptr_hash(name) & (ni->size - 1);
    while (ni->keys[h] != NULL) {
	if (ni->keys[h] == name)
	    return ni->first[h];
	h = (h + 1) & (ni->size - 1);
    }
    ni->keys[h] = name;
    ni->first[h] = i;
    return -1;
}

static int h_switch(SEXP e, CodeBuf *cb, Cntxt *cntxt)
{
    if (lang_length(e) < 2 || any_dots(e))
	return cmp_special(e, cb, cntxt);

    SEXP expr = CADR(e);
    SEXP cases = CDDR(e);
    int n = lang_length(cases);
    if (n == 0)
	notify_no_switch_cases(cntxt, cb);

    int *miss = (int *) R_alloc(n + 1, sizeof(int));
    SEXP *nm = (SEXP *) R_alloc(n + 1, sizeof(SEXP)); /* CHARSXPs or NULL */
    int haveNames = has_tags(cases);
    {
	int i = 0;
	for (SEXP p = cases; p != R_NilValue; p = CDR(p), i++) {
	    bc_tick();
	    miss[i] = CAR(p) == R_MissingArg;
	    nm[i] = TAG(p) == R_NilValue ? R_BlankString : PRINTNAME(TAG(p));
	}
    }
    /* allow for corner cases like switch(x, 1) which always returns 1
       if x is a character scalar */
    if (! haveNames && n == 1)
	haveNames = TRUE;

    int haveCharDflt = FALSE;
    if (haveNames) {
	int ndflt = 0;
	for (int i = 0; i < n; i++)
	    if (CHAR(nm[i])[0] == '\0')
		ndflt++;
	if (ndflt > 1) {
	    notify_multiple_switch_defaults(cntxt, cb);
	    /* punt back to interpreted version for now to get runtime
	       error message for multiple defaults */
	    cmp_special(e, cb, cntxt);
	    return TRUE;
	}
	haveCharDflt = ndflt > 0;
    }

    int anyMiss = FALSE;
    for (int i = 0; i < n; i++)
	if (miss[i])
	    anyMiss = TRUE;
    int missLabel = anyMiss ? makelabel(cb) : -1;
    int dfltLabel = makelabel(cb);
    int *labels = (int *) R_alloc(n + 1, sizeof(int));
    for (int i = 0; i < n; i++)
	labels[i] = miss[i] ? missLabel : makelabel(cb);
    labels[n] = dfltLabel;
    int endLabel = cntxt->tailcall ? -1 : makelabel(cb);

    /* the map from names to labels for a character switch: each
       distinct name (unique(nm[nm != ""])) acts through the first
       non-empty alternative at or after its first occurrence
       (findActionIndex) */
    SEXP unm = R_NilValue;
    int *nlabels = NULL;
    int nunm = 0;
    if (haveNames) {
	int *nextAction = (int *) R_alloc(n + 1, sizeof(int));
	nextAction[n] = n;
	for (int i = n - 1; i >= 0; i--)
	    nextAction[i] = miss[i] ? nextAction[i + 1] : i;
	NameIndex ni;
	name_index_init(&ni, n);
	SEXP *unames = (SEXP *) R_alloc(n + 1, sizeof(SEXP));
	int *ufirst = (int *) R_alloc(n + 1, sizeof(int));
	for (int i = 0; i < n; i++) {
	    bc_tick();
	    if (CHAR(nm[i])[0] == '\0')
		continue;
	    if (name_index_add(&ni, nm[i], i) < 0) {
		unames[nunm] = nm[i];
		ufirst[nunm] = i;
		nunm++;
	    }
	}
	if (haveCharDflt) {
	    unames[nunm] = R_BlankString;
	    int i = 0;
	    while (CHAR(nm[i])[0] != '\0')
		i++;
	    ufirst[nunm] = i;
	    nunm++;
	}
	nlabels = (int *) R_alloc(nunm + 1, sizeof(int));
	for (int j = 0; j < nunm; j++)
	    nlabels[j] = labels[nextAction[ufirst[j]]];
	/* if there is no unnamed case to act as a default for a
	   character switch then the numeric default becomes the
	   character default as well */
	if (! haveCharDflt) {
	    unames[nunm] = R_BlankString;
	    nlabels[nunm] = dfltLabel;
	    nunm++;
	}
	unm = PROTECT(Rf_allocVector(STRSXP, nunm));
	for (int j = 0; j < nunm; j++)
	    SET_STRING_ELT(unm, j, unames[j]);
    }
    else {
	PROTECT(unm);
    }

    /* compile the EXPR argument */
    Cntxt ncntxt = make_nontailcall_context(cntxt);
    cmp(expr, cb, &ncntxt, FALSE, TRUE);

    /* emit the SWITCH instruction */
    int cei = putconst(cb, e);
    if (haveNames) {
	int cni = putconst(cb, unm);
	int w[5] = { SWITCH_OP, cei, cni, make_labellist(cb, nlabels, nunm),
		     make_labellist(cb, labels, n + 1) };
	unsigned char k[5] = { WK_INT, WK_INT, WK_INT, WK_LABELLIST,
			       WK_LABELLIST };
	putcode(cb, 5, w, k);
    }
    else {
	int cni = putconst(cb, R_NilValue);
	int w[5] = { SWITCH_OP, cei, cni, cni, make_labellist(cb, labels, n + 1) };
	unsigned char k[5] = { WK_INT, WK_INT, WK_INT, WK_INT, WK_LABELLIST };
	putcode(cb, 5, w, k);
    }
    UNPROTECT(1); /* unm */

    /* emit code to signal an error if a numeric switch hits an empty
       alternative */
    if (anyMiss) {
	putlabel(cb, missLabel);
	cmp(emptySwitchCall, cb, cntxt, FALSE, TRUE);
    }

    /* emit code for the default case */
    putlabel(cb, dfltLabel);
    put0(cb, LDNULL_OP);
    if (cntxt->tailcall) {
	put0(cb, INVISIBLE_OP);
	put0(cb, RETURN_OP);
    }
    else {
	putL(cb, GOTO_OP, endLabel);
    }

    /* emit code for the non-empty alternatives */
    {
	int i = 0;
	for (SEXP p = cases; p != R_NilValue; p = CDR(p), i++)
	    if (! miss[i]) {
		putlabel(cb, labels[i]);
		cmp(CAR(p), cb, cntxt, FALSE, TRUE);
		if (! cntxt->tailcall)
		    putL(cb, GOTO_OP, endLabel);
	    }
    }
    if (! cntxt->tailcall)
	putlabel(cb, endLabel);
    return TRUE;
}


/*
 * Handler dispatch
 */

static int call_handler(int h, int op, SEXP e, CodeBuf *cb, Cntxt *cntxt)
{
    switch (h) {
    case H_FUNCTION: return h_function(e, cb, cntxt);
    case H_BRACE: return h_brace(e, cb, cntxt);
    case H_IF: return h_if(e, cb, cntxt);
    case H_AND: return h_andor(e, cb, cntxt, AND1ST_OP, AND2ND_OP);
    case H_OR: return h_andor(e, cb, cntxt, OR1ST_OP, OR2ND_OP);
    case H_ASSIGN: return cmp_assign(e, cb, cntxt);
    case H_BREAK: return h_break_next(e, cb, cntxt, TRUE);
    case H_NEXT: return h_break_next(e, cb, cntxt, FALSE);
    case H_REPEAT: return h_repeat(e, cb, cntxt);
    case H_WHILE: return h_while(e, cb, cntxt);
    case H_FOR: return h_for(e, cb, cntxt);
    case H_PLUS:
	return lang_length(e) == 3 ? cmp_prim2(e, cb, ADD_OP, cntxt) :
	    cmp_prim1(e, cb, UPLUS_OP, cntxt);
    case H_MINUS:
	return lang_length(e) == 3 ? cmp_prim2(e, cb, SUB_OP, cntxt) :
	    cmp_prim1(e, cb, UMINUS_OP, cntxt);
    case H_PRIM2: return cmp_prim2(e, cb, op, cntxt);
    case H_PRIM1: return cmp_prim1(e, cb, op, cntxt);
    case H_LOG: return h_log(e, cb, cntxt);
    case H_MATH1: return cmp_math1(e, cb, op, cntxt);
    case H_PAREN: return h_paren(e, cb, cntxt);
    case H_BUILTIN: return cmp_builtin(e, cb, cntxt, FALSE);
    case H_SPECIAL: return cmp_special(e, cb, cntxt);
    case H_DOTINTERNAL: return cmp_dot_internal_call(e, cb, cntxt);
    case H_DOLLAR: return h_dollar(e, cb, cntxt);
    case H_LOCAL: return h_local(e, cb, cntxt);
    case H_RETURN: return h_return(e, cb, cntxt);
    case H_IS: return cmp_is(op, e, cb, cntxt);
    case H_SIMPLEINTERNAL: return cmp_simple_internal(e, cb, cntxt);
    case H_SWITCH: return h_switch(e, cb, cntxt);
    case H_DOTCALL: return h_dotcall(e, cb, cntxt);
    case H_MULTICOLON: return cmp_multicolon(e, cb, cntxt);
    case H_SUPPRESSUNDEF: return h_suppress_undef(e, cb, cntxt);
    case H_SUBSET: return h_subset(e, cb, cntxt, FALSE);
    case H_SUBSET2: return h_subset(e, cb, cntxt, TRUE);
    default: return FALSE;
    }
}


/*
 * Compiler interface
 */

/* compile(e, env, options, srcref) */
attribute_hidden SEXP R_bcCompile(SEXP e, SEXP env, SEXP options, SEXP srcref)
{
    bc_init();
    const void *vmax = vmaxget();
    CEnv *cenv = make_cenv(env);
    Cntxt cntxt = make_toplevel_context(cenv, options);
    /* R code run while folding constants may reset the compiler options
       and free the old values */
    PROTECT(cntxt.suppressUndefined);
    find_locals_list(&e, 1, &cntxt, &cenv->vars);
    SEXP ans;
    if (may_call_browser(e, &cntxt)) {
	/* NOTE: compilation will be attempted repeatedly */
	ans = e;
    }
    else if (srcref == R_NilValue) {
	ans = gen_code(e, &cntxt, NULL);
    }
    else {
	Loc loc = { e, srcref };
	ans = gen_code(e, &cntxt, &loc);
    }
    UNPROTECT(1);
    vmaxset(vmax);
    return ans;
}

/* cmpfun(f, options) */
attribute_hidden SEXP R_bcCmpfun(SEXP f, SEXP options)
{
    bc_init();
    switch (TYPEOF(f)) {
    case CLOSXP:
	break;
    case BUILTINSXP:
    case SPECIALSXP:
	return f;
    default:
	Rf_error(_("cannot compile a non-function"));
    }
    const void *vmax = vmaxget();
    SEXP forms = FORMALS(f);
    SEXP body = R_ClosureExpr(f);
    SEXP env = CLOENV(f);
    Cntxt cntxt = make_toplevel_context(make_cenv(env), options);
    PROTECT(cntxt.suppressUndefined); /* see R_bcCompile */
    Cntxt ncntxt = make_function_context(&cntxt, forms, body);
    if (may_call_browser(body, &ncntxt)) {
	UNPROTECT(1);
	vmaxset(vmax);
	return f;
    }
    SEXP b;
    if (TYPEOF(body) != LANGSXP || CAR(body) != R_BraceSymbol) {
	Loc loc = { body, expr_srcref(f) };
	b = gen_code(body, &ncntxt, &loc);
    }
    else {
	b = gen_code(body, &ncntxt, NULL);
    }
    PROTECT(b);
    SEXP val = PROTECT(R_mkClosure(forms, b, env));
    if (ATTRIB(f) != R_NilValue)
	SHALLOW_DUPLICATE_ATTRIB(val, f);
    if (IS_S4_OBJECT(f))
	SET_S4_OBJECT(val);
    UNPROTECT(3);
    vmaxset(vmax);
    return val;
}

#ifdef BCCOMPILE_STANDALONE

SEXP C_bcCmpfun(SEXP f, SEXP options)
{
    return R_bcCmpfun(f, options);
}

SEXP C_bcCompile(SEXP e, SEXP env, SEXP options, SEXP srcref)
{
    return R_bcCompile(e, env, options, srcref);
}

#else

attribute_hidden SEXP do_bccmpfun(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    checkArity(op, args);
    return R_bcCmpfun(CAR(args), CADR(args));
}

attribute_hidden SEXP do_bccompile(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    checkArity(op, args);
    SEXP env = CADR(args);
    if (TYPEOF(env) != ENVSXP) /* the message the R implementation gives */
	Rf_error(_("argument is not an environment"));
    return R_bcCompile(CAR(args), env, CADDR(args), CADDDR(args));
}

/*
 * The JIT interface: tryCmpfun() and tryCompile(). A compilation error
 * leaves the function or expression uncompiled and, unless the compiler
 * notes are suppressed, prints the error as the R compiler does.
 */

typedef struct {
    SEXP fun, expr, env, srcref;
} TryData;

static SEXP try_cmpfun_body(void *data)
{
    TryData *td = (TryData *) data;
    return R_bcCmpfun(td->fun, R_NilValue);
}

static SEXP try_compile_body(void *data)
{
    TryData *td = (TryData *) data;
    return R_bcCompile(td->expr, td->env, R_NilValue, td->srcref);
}

static SEXP try_handler(SEXP cond, void *data)
{
    bc_init();
    if (Rf_asLogical(compiler_option("suppressAll", R_NilValue)) != TRUE) {
	SEXP msg = R_NilValue, call = R_NilValue;
	if (TYPEOF(cond) == VECSXP && XLENGTH(cond) >= 2) {
	    msg = VECTOR_ELT(cond, 0);
	    call = VECTOR_ELT(cond, 1);
	}
	const char *m = TYPEOF(msg) == STRSXP && XLENGTH(msg) > 0 ?
	    Rf_translateChar(STRING_ELT(msg, 0)) : "";
	if (call != R_NilValue) {
	    SEXP dc = PROTECT(Rf_deparse1line(call, FALSE));
	    Rprintf("%s %s at %s \n", _("Error: compilation failed - "), m,
		    CHAR(STRING_ELT(dc, 0)));
	    UNPROTECT(1);
	}
	else {
	    Rprintf("%s %s at NULL \n", _("Error: compilation failed - "), m);
	}
    }
    return R_NilValue;
}

/* tryCmpfun(fun): fun itself when the compilation fails */
attribute_hidden SEXP R_bcTryCmpfun(SEXP fun)
{
    TryData td = { fun, R_NilValue, R_NilValue, R_NilValue };
    SEXP val = R_tryCatchError(try_cmpfun_body, &td, try_handler, &td);
    return val == R_NilValue ? fun : val;
}

/* tryCompile(expr, env, NULL, srcref): expr itself when the compilation
   fails */
attribute_hidden SEXP R_bcTryCompile(SEXP expr, SEXP env, SEXP srcref)
{
    TryData td = { R_NilValue, expr, env, srcref };
    SEXP val = R_tryCatchError(try_compile_body, &td, try_handler, &td);
    return val == R_NilValue ? expr : val;
}

#endif
