## Compile time must grow linearly with the size of the code. A body of
## many statements, a call with many arguments, a long chain of binary
## operators and a switch() with many alternatives each took time
## quadratic in their size (constant folding redone at every nesting
## level, a linear scan of the constant pool per instruction, positional
## indexing of pairlists). Compare the time for 4n with the time for n,
## both the smallest of three measurements over the same number of
## repetitions, chosen so that the n measurement takes at least 0.2 s:
## linear growth gives a ratio near 4, quadratic growth near 16. The
## bound of 8 catches a restored quadratic path with room for noise on a
## busy machine; the chain is kept short because the depth of the
## compiler's recursion on it has a cost of its own.
library(compiler)
opts <- list(suppressAll = TRUE)

timeReps <- function(f, x, k) {
    gc()
    system.time(for (i in seq_len(k)) f(x))[["elapsed"]]
}
minTime <- function(f, x, k)
    min(timeReps(f, x, k), timeReps(f, x, k), timeReps(f, x, k))
check <- function(what, f, x1, x4) {
    k <- 1L
    repeat {
        t1 <- timeReps(f, x1, k)
        if (t1 >= 0.2)
            break
        k <- 2L * k
    }
    t1 <- min(t1, minTime(f, x1, k))
    t4 <- minTime(f, x4, k)
    cat(sprintf("%-12s n: %6.2f s   4n: %6.2f s   (%d repetitions)\n",
                what, t1, t4, k))
    stopifnot(t4 < 8 * t1)
}

mkflat <- function(n) {
    stmts <- sprintf("v%d <- f(w%d, %d, g(u%d))", seq_len(n), seq_len(n),
                     seq_len(n), seq_len(n))
    src <- sprintf("function(w) {\n%s\n}", paste(stmts, collapse = "\n"))
    eval(parse(text = src, keep.source = FALSE)[[1]])
}
mkchain <- function(n)
    parse(text = paste(sprintf("x%d", seq_len(n)), collapse = " + "),
          keep.source = FALSE)[[1]]
mkcall <- function(n)
    parse(text = sprintf("f(%s)", paste(sprintf("x%d", seq_len(n)),
                                        collapse = ", ")),
          keep.source = FALSE)[[1]]
mkswitch <- function(n) {
    cases <- paste(sprintf("k%d = %d", seq_len(n), seq_len(n)),
                   collapse = ", ")
    eval(parse(text = sprintf("function(x) switch(x, %s, 0)", cases),
               keep.source = FALSE)[[1]])
}

cmpClosure <- function(f) cmpfun(f, opts)
cmpExpr <- function(x) compile(x, new.env(), opts)

check("statements", cmpClosure, mkflat(1000), mkflat(4000))
check("chain", cmpExpr, mkchain(25), mkchain(100))
check("arguments", cmpExpr, mkcall(2000), mkcall(8000))
check("switch", cmpClosure, mkswitch(1000), mkswitch(4000))

sw <- cmpfun(mkswitch(4000), opts)
stopifnot(sw("k2000") == 2000, sw("k4000") == 4000, sw("zz") == 0)

## A left-nested operator chain used to overflow the node stack at about
## 800 terms (the compiler recursed for every term).
long <- eval(parse(text = sprintf("function(x) %s",
                                  paste(c("x", rep("1", 3000)), collapse = " + ")),
                   keep.source = FALSE)[[1]])
stopifnot(cmpfun(long, opts)(0) == 3000)
