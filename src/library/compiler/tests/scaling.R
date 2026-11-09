## Compile time must grow linearly with the size of the code. A body of
## many statements, a call with many arguments, a long chain of binary
## operators and a switch() with many alternatives each took time
## quadratic in their size (constant folding redone at every nesting
## level, a linear scan of the constant pool per instruction, positional
## indexing of pairlists). Compare the time for 4n with the time for n:
## linear growth gives a ratio near 4, quadratic growth near 16. Very
## short times are floored so that the check is robust on fast machines.
library(compiler)
opts <- list(suppressAll = TRUE)

tm <- function(expr) {
    gc()
    system.time(expr)[["elapsed"]]
}
check <- function(what, t1, t4) {
    cat(sprintf("%-12s n: %6.2f s   4n: %6.2f s\n", what, t1, t4))
    stopifnot(t4 < 8 * max(t1, 0.1))
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

f1 <- mkflat(1000); f4 <- mkflat(4000)
check("statements", tm(cmpfun(f1, opts)), tm(cmpfun(f4, opts)))

c1 <- mkchain(100); c4 <- mkchain(400)
check("chain", tm(compile(c1, new.env(), opts)),
      tm(compile(c4, new.env(), opts)))

a1 <- mkcall(2000); a4 <- mkcall(8000)
check("arguments", tm(compile(a1, new.env(), opts)),
      tm(compile(a4, new.env(), opts)))

s1 <- mkswitch(1000); s4 <- mkswitch(4000)
check("switch", tm(cmpfun(s1, opts)), tm(sw <- cmpfun(s4, opts)))
stopifnot(sw("k2000") == 2000, sw("k4000") == 4000, sw("zz") == 0)
