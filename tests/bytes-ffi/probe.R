## Does an ordinary package -- one that has never heard of BYTESXP --
## fail safely when handed one?  This is the claim the whole design
## rests on, so it is tested against real compiled package code rather
## than argued.
##
##   ../../build/bin/R CMD SHLIB pkg.c && ../../build/bin/Rscript probe.R

dyn.load("pkg.so")
le <- function(h, w) rev(as.raw(strtoi(substring(h, seq(1, 2*w-1, 2), seq(2, 2*w, 2)), 16L)))
mk <- function(k, w, ...) as.bytes(as.raw(unlist(lapply(c(...), le, w = w))), w, k)

u <- mk("unsigned", 8L, "0000000000000001", "0000000000000002")
o <- as.bytes(as.raw(1:16), 8L)

fails <- 0L
chk <- function(fn, x, want, why) {
    got <- tryCatch(list(ok = TRUE, v = .Call(fn, x)),
                    error = function(e) list(ok = FALSE, v = conditionMessage(e)))
    good <- if (want == "error") !got$ok else got$ok && identical(got$v, want)
    if (!good) fails <<- fails + 1L
    cat(sprintf("  %-14s %-9s %s   %s\n", fn, if (got$ok) "returns" else "errors",
                if (good) "ok  " else "FAIL", why))
}

cat("a uint64 vector handed to code that knows nothing about it:\n")
chk("typed_switch",  u, "error", "its own default: branch catches the unknown type")
chk("assume_real",   u, "error", "REAL() type-checks and names the type")
chk("via_isinteger", u, FALSE,   "isInteger() is honest, so the safe branch is taken")
chk("just_length",   u, 2L,      "length is meaningful and allowed")
chk("via_dataptr",   u, "error", "the untyped escape hatch is guarded too")

cat("\nthe same, for an opaque vector:\n")
chk("typed_switch",  o, "error", "unknown type")
chk("assume_real",   o, "error", "REAL() type-checks")
chk("via_isinteger", o, FALSE,   "safe branch")

cat("\nunchanged for ordinary vectors:\n")
chk("typed_switch",  42L, 42L, "integer still works")
chk("just_length",   42L, 1L,  "integer still works")

cat(sprintf("\n%d failure(s)\n", fails))
if (fails) quit(status = 1L)
