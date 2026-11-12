library(compiler)

options(keep.source=TRUE)

## very minimal
x <- 2
stopifnot(eval(compile(quote(x + 1))) == 3)

## simple code generation
checkCode <- function(expr, code, optimize = 2) {
    v <- compile(expr, options = list(optimize = optimize))
    d <- .Internal(disassemble(v))[[2]][-1]
    dd <- as.integer(eval(substitute(code), getNamespace("compiler")))
    identical(d, dd)
}
x <- 2
stopifnot(checkCode(quote(x + 1),
                    c(GETVAR.OP, 1L,
                      LDCONST.OP, 2L,
                      ADD.OP, 0L,
                      RETURN.OP)))
f <- function(x) x
checkCode(quote({f(1); f(2)}),
          c(GETFUN.OP, 1L,
            PUSHCONSTARG.OP, 3L,
            CALL.OP, 4L,
            POP.OP,
            GETFUN.OP, 1L,
            PUSHCONSTARG.OP, 6L,
            CALL.OP, 7L,
            RETURN.OP))


## names and ... args
f <- function(...) list(...)
stopifnot(identical(f(1, 2), cmpfun(f)(1, 2)))

f <- function(...) list(x = ...)
stopifnot(identical(f(1, 2), cmpfun(f)(1, 2)))


## substitute and argument constant folding
f <- function(x) substitute(x)
g <- function() f(1 + 2)
v1 <- g()
f <- cmpfun(f)
g <- cmpfun(g)
v2 <- g()
stopifnot(identical(v1, v2))


## simple loops
sr <- function(x) {
    n <- length(x)
    i <- 1
    s <- 0
    repeat {
        if (i > n) break
        s <- s + x[i]
        i <- i + 1
    }
    s
}
sw <- function(x) {
    n <- length(x)
    i <- 1
    s <- 0
    while (i <= n) {
        s <- s + x[i]
        i <- i + 1
    }
    s
}
sf <- function(x) {
    s <- 0
    for (y in x)
        s <- s + y
    s
}
src <- cmpfun(sr)
swc <- cmpfun(sw)
sfc <- cmpfun(sf)
x <- 1 : 5
stopifnot(src(x) == sr(x))
stopifnot(swc(x) == sw(x))
stopifnot(sfc(x) == sf(x))


## Check that the handlers have been associated with the correct package
h <- ls(compiler:::inlineHandlers, all.names = TRUE)
p <- sub("package:", "", sapply(h, find))
pp <- sapply(h, function(n) get(n, compiler:::inlineHandlers)$package)
stopifnot(identical(p, pp))


## Check assumption about simple .Internals
## These are .External calls now.
## stopifnot(all(sapply(compiler:::safeStatsInternals,
##                      function(f)
##                      compiler:::is.simpleInternal(get(f, "package:stats")))))

stopifnot(all(sapply(compiler:::safeBaseInternals,
                     function(f)
                     compiler:::is.simpleInternal(get(f, "package:base")))))


## Type predicates on boxed values, immediate scalars, and compact sequences.
local({
    oldJIT <- enableJIT(0)
    ns <- asNamespace("compiler")
    opcodes <- c(is.null = "ISNULL.OP", is.logical = "ISLOGICAL.OP",
                 is.integer = "ISINTEGER.OP", is.double = "ISDOUBLE.OP",
                 is.complex = "ISCOMPLEX.OP", is.character = "ISCHARACTER.OP",
                 is.symbol = "ISSYMBOL.OP", is.object = "ISOBJECT.OP")
    for (predicate in names(opcodes)) {
        f <- eval(call("function", as.pairlist(alist(x = )),
                       call(predicate, quote(x))))
        code <- .Internal(disassemble(.Internal(bodyCode(cmpfun(f)))))
        ops <- compiler:::bcDecode(code[[2L]])
        op <- as.name(opcodes[[predicate]])
        stopifnot(sum(vapply(ops, identical, NA, op)) == 1L)
    }

    boxed <- list(NULL, logical(), FALSE, TRUE, NA, 0L, NA_integer_, 1,
                  NA_real_, NaN, Inf, 1i, "x", quote(x), list(1), as.raw(0),
                  factor("x"), structure(1L, class = "foo"), c(a = 1), 1:100)
    operands <- list(
        list(quote(x), boxed),
        list(quote(x + 0L), list(0L, 1L, -1L, NA_integer_)),
        list(quote(x + 0), list(0, 1, -1, NA_real_, NaN, Inf, -Inf)),
        list(quote(x < 0), list(-1, 0, 1, NA_real_)),
        list(quote(if (x) NA else TRUE), list(FALSE, TRUE)),
        list(quote(if (x) NA_integer_ else 1L), list(FALSE, TRUE)),
        list(quote(seq_len(x)), list(0L, 1L, 3L)),
        list(quote(seq_along(x)), list(NULL, 1, 1:3)),
        list(quote(1L:x), list(1L, 3L)))
    ## Consumers of the result P, which may be an immediate logical.
    contexts <- expression(P, if (P) 1 else 2, !P, P && TRUE, P || FALSE,
                           { r <- P; r }, c(P, NA))
    for (predicate in names(opcodes)) {
        for (operand in operands) {
            test <- call(predicate, operand[[1L]])
            for (context in contexts) {
                body <- do.call(substitute, list(context, list(P = test)))
                f <- eval(call("function", as.pairlist(alist(x = )), body))
                cf <- cmpfun(f)
                for (x in operand[[2L]])
                    stopifnot(identical(withVisible(cf(x)), withVisible(f(x))))
            }
        }
        f <- eval(substitute(function(x) {
            r <- logical()
            for (i in x)
                r <- c(r, P(i))
            r
        }, list(P = as.name(predicate))))
        cf <- cmpfun(f)
        for (x in list(1:3, c(1, 2), c(TRUE, NA), integer(), list(1L, "a")))
            stopifnot(identical(cf(x), f(x)))
    }

    ## The compiler leaves is.numeric() to the builtin, so reach the
    ## ISNUMERIC opcode by retargeting compiled is.double() code.
    for (operand in operands) {
        f <- eval(call("function", as.pairlist(alist(x = )),
                       call("is.double", operand[[1L]])))
        g <- eval(call("function", as.pairlist(alist(x = )),
                       call("is.numeric", operand[[1L]])))
        code <- .Internal(disassemble(.Internal(bodyCode(cmpfun(f)))))
        ops <- compiler:::bcDecode(code[[2L]])
        i <- which(vapply(ops, identical, NA, as.name("ISDOUBLE.OP")))
        stopifnot(length(i) == 1L)
        code[[2L]][i] <- as.integer(ns$ISNUMERIC.OP)
        cg <- .Internal(bcClose(formals(f),
                                .Internal(mkCode(code[[2L]], code[[3L]])),
                                environment(f)))
        for (x in operand[[2L]])
            stopifnot(identical(withVisible(cg(x)), withVisible(g(x))))
    }
    invisible(enableJIT(oldJIT))
})
