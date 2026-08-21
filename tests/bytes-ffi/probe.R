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

## An ALTREP object is written as its serialized state and never as its
## own elements, so a 'bytes' vector reachable only through that state
## has to be found there.  The header goes out before the first item
## and a connection cannot be rewound, so a writer that missed it would
## discover the type mid-stream, with the file already truncated.
cat("\nan ALTREP class whose serialized state holds a 'bytes' vector:\n")
invisible(.Call("init_altrep"))
alt <- .Call("make_altrep_with_bytes", 1:3)
streamVersion <- function(r) readBin(r[3:6], "integer", 1L, endian = "big")
chk2(typeof(alt), "integer", "its own type says nothing about its state")
chk2(streamVersion(serialize(alt, NULL)), 4L,
     "the version is raised for what the state carries")
chk2(unserialize(serialize(alt, NULL))[1:3], 1:3, "and the object round trips")
chk2(inherits(tryCatch(serialize(alt, NULL, version = 3), error = identity),
	      "error"), TRUE,
     "a version too low is refused, not discovered mid-stream")
local({
    f <- tempfile()
    on.exit(unlink(f))
    writeLines("previous contents", f)
    prior <- file.size(f)
    tryCatch(saveRDS(alt, f, version = 3), error = function(e) NULL)
    chk2(file.size(f), prior, "so the file it refuses to write is left alone")
})
chk2(streamVersion(serialize(1:1000, NULL)), 3L,
     "an ALTREP with no 'bytes' vector is still version 3")

## isVector() covers BYTESXP, so R_duplicateAsResizable() would take one
## although R_allocResizableVector() has no way to name a width and a
## kind and cannot make one.  Both halves have to say the same thing.
cat("\nthe resizable-vector API:\n")
chk2(inherits(tryCatch(.Call("resizable", made), error = identity), "error"),
     TRUE, "R_duplicateAsResizable() refuses what it cannot allocate")
chk2(is.integer(.Call("resizable", c(1L, 2L, 3L))), TRUE,
     "and still takes an ordinary vector")

cat(sprintf("\n%d failure(s)\n", fails))
if (fails) quit(status = 1L)
