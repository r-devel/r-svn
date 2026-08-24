## Does an ordinary package -- one that has never heard of ALTSXP --
## fail safely when handed one?  This is the claim the whole design
## rests on, so it is tested against real compiled package code rather
## than argued.
##
## Run by "make test-XIntsFFI" in the tests directory, or by hand:
##   ../../build/bin/R CMD SHLIB pkg.c && ../../build/bin/Rscript probe.R

dyn.load(paste0("pkg", .Platform$dynlib.ext))
le <- function(h, w) rev(as.raw(strtoi(substring(h, seq(1, 2*w-1, 2), seq(2, 2*w, 2)), 16L)))
mk <- function(k, w, ...) as.xinteger(as.raw(unlist(lapply(c(...), le, w = w))), w, k)

u <- mk("unsigned", 8L, "0000000000000001", "0000000000000002")

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
chk2(typeof(made), "alt", "R_allocXIntVector() makes a genuine ALTSXP vector")
chk2(storage.mode(made), "uint64", "and its storage mode records uint64")
chk2(length(made), 4L, "length is the element count, not the byte count")
chk2(is.na(made), c(TRUE, FALSE, FALSE, FALSE),
     "R_xintSetNA() marks an element missing")
chk2(as.character(made)[2:4],
     c("4611686018427387905", "4611686018427387906", "4611686018427387907"),
     "the payload copied in reads back at full precision")
chk2(.Call("describe", made), c(4L, 8L, 1L, 1L),
     "width, kind and NA-ness are readable from C")
chk2(.Call("xinteger_altrep_metadata", made), TRUE,
     "the per-instance NA policy lives in standard ALTREP data2")
chk2(.Call("xinteger_altrep_metadata",
           xinteger(1L, 8L, "unsigned", na = FALSE)), FALSE,
     "data2 also records vectors with no reserved sentinel")
chk2(.Call("sum_uint64", made), sum(as.numeric(as.character(made)[2:4])),
     "R_xintIsNA() lets a consumer skip missing values")
chk2(.Call("first_byte_of_each", made), c(255L, 1L, 2L, 3L),
     "XINTEGER_RO() reaches the payload, NA included")
chk2(identical(made[2:4], .Call("make_uint64", 4L)[2:4]), TRUE,
     "two calls agree")

cat("\nthe opt-in accessors type-check, like INTEGER() and RAW():\n")
chk("width_of_anything", 42L,  "error", "R_xintWidth() refuses an integer vector")
chk("xinteger_of_anything", 42L,  "error", "XINTEGER_RO() refuses an integer vector")
chk("width_of_anything", made, 8L, "and works on the real thing")

## Existing ALTREP class/state serialization can carry an ALTSXP object
## without a new stream version.
cat("\nan ALTREP class whose serialized state holds an 'xinteger' vector:\n")
invisible(.Call("init_altrep"))
alt <- .Call("make_altrep_with_xint", 1:3)
streamVersion <- function(r) readBin(r[3:6], "integer", 1L, endian = "big")
chk2(typeof(alt), "integer", "its own type says nothing about its state")
invisible(.Call("reset_serialized_state_calls"))
serialized <- serialize(alt, NULL)
chk2(streamVersion(serialized), 3L,
     "the existing ALTREP stream version carries the state")
chk2(.Call("get_serialized_state_calls"), 1L,
     "its serialized-state method is called exactly once")
chk2(unserialize(serialized)[1:3], 1:3, "and the object round trips")
chk2(unserialize(serialize(alt, NULL, version = 3))[1:3], 1:3,
     "an explicitly requested version 3 round trips")
plain <- .Call("make_altrep_plain", 1:3)
invisible(.Call("reset_serialized_state_calls"))
chk2(streamVersion(serialize(plain, NULL)), 3L,
     "a package ALTREP also stays on version 3")
chk2(.Call("get_serialized_state_calls"), 1L,
     "and only the write called its method")
chk2(streamVersion(serialize(1:1000, NULL)), 3L,
     "while a compact sequence is read and left at version 3")

cat("\ntwo-sided ALTSXP semantic dispatch:\n")
pair <- .Call("make_dispatch_pair")
left <- pair[[1L]]; right <- pair[[2L]]
chk2(c(typeof(left), typeof(right)), c("alt", "alt"),
     "package classes use the opaque SEXPTYPE")
chk2(c(class(left), class(right)), c("dispatch_left", "dispatch_right"),
     "their registered ALTREP names are their implicit classes")
chk2(c(is.xinteger(left), is.xinteger(right)), c(FALSE, FALSE),
     "they are not mistaken for the built-in integer classes")
invisible(.Call("reset_binary_dispatch_calls"))
chk2(left + right, 2L,
     "the right class handles after the left class declines")
chk2(.Call("get_binary_dispatch_calls"), 2L,
     "each distinct class received one opportunity")
invisible(.Call("reset_binary_dispatch_calls"))
chk2(right + right, 1L, "a shared class handles from the left")
chk2(.Call("get_binary_dispatch_calls"), 1L,
     "a shared class is not called twice")
chk2(1.5 + right, 2L,
     "an ALTSXP method gets first refusal over ordinary real promotion")
chk2(c(left == right, right == left), c(TRUE, FALSE),
     "Compare fallback preserves operand order and identifies the receiver")
chk2(-right, 12L, "Unary_op handles opaque unary arithmetic")
chk2(as.integer(right), 1L, "Coerce owns conversion out of the opaque type")
chk2(format(right), "opaque:1", "Format supplies the display representation")
chk2(capture.output(print(right)), "[1] opaque:1",
     "the default printer also consumes Format")
chk2(sum(right), 100L, "Summary receives the base summary operation")
chk2(c(right, right), 2L, "Combine owns concatenation for an opaque class")
hashed <- .Call("make_dispatch_right", as.raw(c(1L, 1L, 2L)))
chk2(duplicated(hashed), c(FALSE, TRUE, FALSE),
     "Hash and canonical Elt support base hashing without private metadata")

## isVector() covers ALTSXP, so R_duplicateAsResizable() would take one
## although R_allocResizableVector() has no way to name a width and a
## kind and cannot make one.  Both halves have to say the same thing.
cat("\nthe resizable-vector API:\n")
chk2(inherits(tryCatch(.Call("resizable", made), error = identity), "error"),
     TRUE, "R_duplicateAsResizable() refuses what it cannot allocate")
chk2(is.integer(.Call("resizable", c(1L, 2L, 3L))), TRUE,
     "and still takes an ordinary vector")

## R_xintTypeSupported(): the question a column reader has to ask
## before it commits, since the allocator answers with an R error.
cat("\nasking before allocating:\n")
kinds <- c(unsigned = 1L, signed = 2L)
supported <- function(w, k) .Call("type_supported", w, k)
chk2(supported(8L, kinds[["signed"]]), TRUE, "width 8 signed is allocatable")
chk2(supported(8L, kinds[["unsigned"]]), TRUE, "width 8 unsigned is allocatable")
chk2(supported(16L, kinds[["unsigned"]]), FALSE, "only 64-bit values are prototyped")
chk2(supported(3L, kinds[["unsigned"]]), FALSE, "a width outside the set is not")
chk2(supported(256L, kinds[["unsigned"]]), FALSE, "width 256 is not")
chk2(supported(0L, kinds[["signed"]]), FALSE, "width 0 is not")
chk2(supported(-1L, kinds[["signed"]]), FALSE, "a negative width is not")
chk2(supported(8L, 0L), FALSE, "the removed kind code is not allocatable")
chk2(supported(8L, 3L), FALSE, "and neither is an unknown kind")

## the predicate and the allocator must never disagree: whatever it
## admits must allocate, and whatever it refuses must error
local({
    ok <- TRUE
    for (w in c(-1L, 0L, 1L, 2L, 3L, 4L, 7L, 8L, 16L, 17L, 255L, 256L, 1000L))
        for (k in c(-1L, 0L, 1L, 2L, 3L)) {
            said <- supported(w, k)
            did <- tryCatch(.Call("alloc_succeeds", w, k),
                            error = function(e) FALSE)
            if (!identical(said, did)) ok <- FALSE
        }
    chk2(ok, TRUE, "predicate agrees with the allocator at every pair")
})

cat(sprintf("\n%d failure(s)\n", fails))
if (fails) quit(status = 1L)
