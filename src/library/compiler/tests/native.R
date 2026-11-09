## The C implementation of the compiler (src/main/bccompile.c) must
## generate exactly the byte code the R implementation generates: same
## instructions and the same constant pool, including the nested code
## objects for promises and closures and the expression and source
## reference indices.

library(compiler)

cmpfunC <- function(f, options = NULL) .Internal(bcCmpfun(f, options))
compileC <- function(e, env = .GlobalEnv, options = NULL, srcref = NULL)
    .Internal(bcCompile(e, env, options, srcref))

## compare two byte code objects, descending into nested ones
bcEqual <- function(a, b) {
    if (typeof(a) == "bytecode" && typeof(b) == "bytecode") {
        da <- .Internal(disassemble(a))
        db <- .Internal(disassemble(b))
        if (!identical(da[[2]], db[[2]]))
            return(FALSE)
        ca <- da[[3]]
        cb <- db[[3]]
        if (length(ca) != length(cb))
            return(FALSE)
        for (i in seq_along(ca))
            if (!bcEqual(ca[[i]], cb[[i]]))
                return(FALSE)
        return(TRUE)
    }
    if (typeof(a) == "list" && typeof(b) == "list" &&
        is.null(attributes(a)) && is.null(attributes(b)) &&
        length(a) == length(b)) {
        for (i in seq_along(a))
            if (!bcEqual(a[[i]], b[[i]]))
                return(FALSE)
        return(TRUE)
    }
    identical(a, b, ignore.bytecode = FALSE, ignore.environment = FALSE,
              ignore.srcref = FALSE)
}

closEqual <- function(a, b) {
    if (typeof(a) != "closure" || typeof(b) != "closure")
        return(identical(a, b))
    identical(formals(a), formals(b)) &&
        identical(attributes(a), attributes(b)) &&
        identical(environment(a), environment(b)) &&
        bcEqual(.Internal(bodyCode(a)), .Internal(bodyCode(b)))
}

checkFun <- function(f, options = NULL) {
    r <- tryCatch(cmpfun(f, options), error = function(e) e)
    c <- tryCatch(cmpfunC(f, options), error = function(e) e)
    if (inherits(r, "error") || inherits(c, "error"))
        inherits(r, "error") && inherits(c, "error") &&
            conditionMessage(r) == conditionMessage(c)
    else
        closEqual(r, c)
}

checkExpr <- function(e, env, srcref = NULL, options = NULL) {
    r <- tryCatch(compile(e, env, options, srcref), error = function(err) err)
    c <- tryCatch(compileC(e, env, options, srcref), error = function(err) err)
    if (inherits(r, "error") || inherits(c, "error"))
        inherits(r, "error") && inherits(c, "error")
    else if (typeof(r) == "bytecode")
        bcEqual(r, c)
    else
        identical(r, c)
}

## every closure in the base packages, with the default options
for (ns in c("base", "stats", "utils", "methods", "tools", "compiler")) {
    env <- asNamespace(ns)
    for (nm in ls(env, all.names = TRUE)) {
        f <- get(nm, env)
        if (typeof(f) == "closure" && !checkFun(f))
            stop("different code for ", ns, "::", nm)
    }
}

## code with source references, compiled as expressions (as cmpfile()
## does) and as functions, at every optimization level and with the
## compiler notes enabled
src <- "
f1 <- function(x, y) {
    z <- x + y * 2
    if (z > 3) z else -z
}
f2 <- function(x) {
    for (i in seq_along(x)) x[i] <- x[i]^2
    while (length(x) > 10) x <- x[-1]
    repeat {
        if (sum(x) > 100) break
        x <- x * 2
    }
    x
}
f3 <- function(x, n) switch(n, a = , b = x$a, c = x[[2]], 1, stop('bad'))
f4 <- function(d, i) {
    d$a[i] <- 3
    names(d)[1] <- 'z'
    d@slot <- 1
    attr(d, 'x')[[1]] <- 2
    d[[1]][i, 2] <- 4
    d[1, 2, 3] <- 5
    base::names(d) <- 'q'
    d
}
f5 <- function(...) list(..1, ...)
f6 <- function(x) nchar(x, 'bytes') + inherits(x, 'foo') + is.vector(x)
f7 <- function(x) local({ y <- x; (y) })
f8 <- function(x) is.null(x) || is.character(x) && !is.name(x)
f9 <- function(x) log(x, 2) + log(x) + sqrt(exp(x)) + stats::dnorm(x, 1, 2)
f10 <- function(x) .Call(C_foo, x, 1L)
f11 <- function(x) { x[1, ] <- 2; x[, 2] <- 3; x[[1]][[2]] <- 4; x }
f12 <- function(x) with(x, a + b) + -(-1) + 2 * pi + seq_len(3)[2]
f13 <- function(x) if (x) 1
f14 <- function(x) { quote(a + b); expression(y); ~ z; x }
f15 <- function(x, quote) quote(x <- 1)
f16 <- function(f) f(1)(2)
f17 <- function(x) x[[1]]$b@c[2]
f18 <- function() { g <- function(y) y + 1; g(2) }
f19 <- function(x) { x$y <- NULL; x[['z']] <- 1; x }
f20 <- function(x) return(x)
"
exprs <- parse(text = src, keep.source = TRUE)
srefs <- attr(exprs, "srcref")
env <- new.env()
for (i in seq_along(exprs)) {
    stopifnot(checkExpr(exprs[[i]], env, srefs[[i]]),
              checkExpr(exprs[[i]], env))
    eval(exprs[[i]], env)
}
options <- list(NULL, list(optimize = 0), list(optimize = 1),
                list(optimize = 3), list(suppressAll = FALSE),
                list(suppressAll = FALSE, suppressUndefined = FALSE))
for (nm in ls(env)) {
    f <- get(nm, env)
    for (opt in options)
        if (!checkFun(f, opt))
            stop("different code for ", nm, " with options ",
                 deparse(opt, nlines = 1))
}

## a deeply nested expression leaves both compilers with an error
## (the C compiler checks the stack in its recursive scans)
e <- quote(x)
for (i in 1:100000) e <- call("identity", e)
deep <- function(x) NULL
body(deep) <- e
stopifnot(inherits(tryCatch(cmpfun(deep), error = function(e) e), "error"),
          inherits(tryCatch(cmpfunC(deep), error = function(e) e), "error"))

## constant folding dispatches S3 methods visible from the global
## environment, as the R compiler's do.call() does
Ops.foo <- function(e1, e2) 99
s3fold <- function() NULL
body(s3fold) <- call("+", structure(1, class = "foo"), 2)
stopifnot(checkFun(s3fold), cmpfunC(s3fold)() == 99)
## the size test on the folded constant dispatches length() as well, so
## a method making the constant large prevents folding in both compilers
## and the state the method reads is observed at run time
length.foo <- function(x) 100L
counter <- 0
Ops.foo <- function(e1, e2) counter <<- counter + 1
stopifnot(checkFun(s3fold))
cf <- cmpfunC(s3fold)
invisible(cf()); invisible(cf())
stopifnot(counter == 2)
rm(Ops.foo, length.foo)

## partial argument matching in an inlined .Internal call honors
## warnPartialMatchArgs as match.call() does: a warning, or with
## warn = 2 an error that leaves the call uninlined
pm <- function(x) nchar(x, t = "bytes")
op <- options(warnPartialMatchArgs = TRUE, warn = 2)
stopifnot(checkFun(pm))
## a muffled warning is not an error, so the call is inlined
stopifnot(suppressWarnings(checkFun(pm)))
options(warn = 0)
stopifnot(suppressWarnings(checkFun(pm)))
options(op)

## an ALTREP constant is hashed without being expanded (the R compiler
## expands it in c(forms, body), so only the small one is compared)
stopifnot(checkFun(as.function(list(1:1000000))))
big <- as.function(list(1:1000000000))
stopifnot(system.time(cmpfunC(big))[["elapsed"]] < 5)
## nor is a deferred string expanded by hashing it
stopifnot(checkFun(as.function(list(as.character(1:100000)))))
bigs <- as.function(list(as.call(list(quote(length), as.character(1:1000000000)))))
stopifnot(system.time(cmpfunC(bigs))[["elapsed"]] < 5)
