## Does an ordinary package -- one that has never heard of BYTESXP --
## fail safely when handed one?  This is the claim the whole design
## rests on, so it is tested against real compiled package code rather
## than argued.
##
## Run by "make test-BytesFFI" in the tests directory, or by hand:
##   ../../build/bin/R CMD SHLIB pkg.c && ../../build/bin/Rscript probe.R

dyn.load(paste0("pkg", .Platform$dynlib.ext))
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

## The other direction: a package that *wants* to produce and consume
## these vectors, using only what Rinternals.h declares.  This is the
## door ingest packages come through, so it is checked the same way.
cat("\nthe opt-in API, used by a package that knows the type:\n")

made <- .Call("make_uint64", 4L)
chk2 <- function(got, want, why) {
    good <- identical(got, want)
    if (!good) fails <<- fails + 1L
    cat(sprintf("  %-24s %s   %s\n", "", if (good) "ok  " else "FAIL", why))
}
chk2(typeof(made), "uint64", "R_allocBytesVector() makes a real uint64 vector")
chk2(length(made), 4L, "length is the element count, not the byte count")
chk2(is.na(made), c(TRUE, FALSE, FALSE, FALSE),
     "R_bytesSetNA() marks an element missing")
chk2(as.character(made)[2:4],
     c("4611686018427387905", "4611686018427387906", "4611686018427387907"),
     "the payload copied in reads back at full precision")
chk2(.Call("describe", made), c(4L, 8L, 1L, 1L),
     "width, kind and NA-ness are readable from C")
chk2(.Call("sum_uint64", made), sum(as.numeric(as.character(made)[2:4])),
     "R_bytesIsNA() lets a consumer skip missing values")
chk2(.Call("first_byte_of_each", made), c(255L, 1L, 2L, 3L),
     "BYTES_RO() reaches the payload, NA included")
chk2(identical(made[2:4], .Call("make_uint64", 4L)[2:4]), TRUE,
     "two calls agree")

cat("\nthe opt-in accessors type-check, like INTEGER() and RAW():\n")
chk("width_of_anything", 42L,  "error", "R_bytesWidth() refuses an integer vector")
chk("bytes_of_anything", 42L,  "error", "BYTES_RO() refuses an integer vector")
chk("width_of_anything", made, 8L, "and works on the real thing")

cat(sprintf("\n%d failure(s)\n", fails))
if (fails) quit(status = 1L)
