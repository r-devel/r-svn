library(compiler)

## The dispatch-capable opcode carries the original call.
hasNumericOpcode <- function(f) {
    invisible(capture.output(code <- disassemble(cmpfun(f))))
    any(vapply(code[[2L]], identical, logical(1), as.name("ISNUMERIC2.OP")))
}
stopifnot(hasNumericOpcode(function(x) is.numeric(x)),
          hasNumericOpcode(function(x) is.numeric(x = x)),
          !hasNumericOpcode(function(...) is.numeric(...)),
          !hasNumericOpcode(function(x) is.numeric(y = x)),
          !hasNumericOpcode(function() is.numeric()),
          !hasNumericOpcode(function() is.numeric(,)))
## R 4.6.1 through R-devel r90881 used builtin calls for is.numeric().

## Compare interpreted and compiled results, including visibility.
values <- list(NULL, TRUE, NA, 1L, NA_integer_, 1, NA_real_, NaN, Inf,
               1i, "1", raw(1), list(1), quote(x), new.env(), function() 1,
               integer(), double(), c(1, 2), 1:1000000,
               structure(1, names = "a"), structure(1L, dim = c(1L, 1L)),
               factor("a"), as.Date("2020-01-01"),
               as.POSIXct("2020-01-01", tz = "UTC"),
               as.difftime(1, units = "secs"), structure(1, class = "noMethod"))
for (level in 1:3) {
    f <- function(x) is.numeric(x)
    g <- cmpfun(f, options = list(optimize = level))
    for (x in values)
        stopifnot(identical(withVisible(f(x)), withVisible(g(x))))
}
## R 4.6.1 through R-devel r90881 used builtin calls for these inputs.

## Immediate stack values and partial compact-sequence representations.
for (expr in expression(is.numeric(x + 1), is.numeric(x + 1L),
                        is.numeric(x > 0), is.numeric(invisible(x)),
                        is.numeric(1:x), is.numeric(seq_len(x)),
                        is.numeric(seq_along(x)))) {
    f <- eval(call("function", as.pairlist(alist(x = )), expr))
    g <- cmpfun(f)
    for (x in list(1L, 2, NA_integer_, NA_real_, NaN, Inf)) {
        if (grepl("seq|:", deparse(expr)) && !identical(x, 1L) && !identical(x, 2))
            next
        stopifnot(identical(withVisible(f(x)), withVisible(g(x))))
    }
}
## R 4.6.1 through R-devel r90881 boxed these operands for builtin calls.

## Preserve argument names, call information, and arbitrary method results.
local({
    is.numeric.numericTest <- function(x) {
        invisible(list(call = match.call(), expr = substitute(x),
                       generic = .Generic, method = .Method, value = unclass(x)))
    }
    x <- structure(1, class = "numericTest")
    for (f in list(function(x) is.numeric(x), function(x) is.numeric(x = x))) {
        g <- cmpfun(f)
        stopifnot(identical(withVisible(f(x)), withVisible(g(x))))
    }
})
## R 4.6.1 through R-devel r90881 dispatched only through builtin calls.

## NextMethod and evaluation of a side-effecting argument exactly once.
local({
    is.numeric.numericFirst <- function(x) NextMethod()
    is.numeric.numericLast <- function(x) FALSE
    count <- 0L
    make <- function() {
        count <<- count + 1L
        structure(1, class = c("numericFirst", "numericLast"))
    }
    f <- cmpfun(function() is.numeric(make()))
    stopifnot(identical(f(), FALSE), count == 1L)
})
## R 4.6.1 through R-devel r90881 dispatched only through builtin calls.

## Missing arguments, wrong names, and ... retain builtin errors and matching.
messageOf <- function(f, ...) tryCatch(f(...), error = conditionMessage)
for (f in list(function(x) is.numeric(x), function(x) is.numeric(y = x),
               function(...) is.numeric(...), function() is.numeric(),
               function() is.numeric(,), function(x) is.numeric(x, x))) {
    g <- cmpfun(f)
    stopifnot(identical(messageOf(f), messageOf(g)))
    if (length(formals(f)))
        stopifnot(identical(messageOf(f, 1), messageOf(g, 1)))
}
## R 4.6.1 through R-devel r90881 used builtin or ordinary calls here.

## A local replacement and the optimize=2 base binding guard still work.
local({
    is.numeric <- function(x) "local"
    f <- function(x) is.numeric(x)
    stopifnot(identical(cmpfun(f)(1), "local"))
})
## R 4.6.1 through R-devel r90881 had no dedicated numeric opcode emission.
local({
    e <- new.env(parent = globalenv())
    f <- eval(quote(function(x) is.numeric(x)), e)
    g <- cmpfun(f, options = list(optimize = 2))
    e$is.numeric <- function(x) "replacement"
    stopifnot(identical(g(1), "replacement"))
})
## R 4.6.1 through R-devel r90881 guarded a builtin call instead.

## S4 dispatch must continue to use the primitive's formal methods.
if (requireNamespace("methods", quietly = TRUE)) {
    methods::setClass("numericOpcodeTest", slots = c(value = "numeric"))
    methods::setMethod("is.numeric", "numericOpcodeTest",
                       function(x) invisible(FALSE))
    x <- methods::new("numericOpcodeTest", value = 1)
    f <- function(x) is.numeric(x)
    g <- cmpfun(f)
    stopifnot(identical(withVisible(f(x)), withVisible(g(x))))
    ## R 4.6.1 through R-devel r90881 dispatched only through builtin calls.
    methods::removeMethod("is.numeric", "numericOpcodeTest")
    methods::removeClass("numericOpcodeTest")
}

## Serialization retains the dispatch-capable instruction and its call operand.
f <- cmpfun(function(x) is.numeric(x))
g <- unserialize(serialize(f, NULL))
stopifnot(identical(g(1), TRUE), identical(g(as.Date("2020-01-01")), FALSE))
## R 4.6.1 through R-devel r90881 had no ISNUMERIC2 instruction to serialize.
