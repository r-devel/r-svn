## Focused public-behavior gauntlet for the ALTSXP int64/uint64 prototype.

fails <- 0L
ok <- function(label, cond) {
    good <- isTRUE(tryCatch(cond, error = function(e) FALSE))
    if (!good) fails <<- fails + 1L
    cat(sprintf("%-48s %s\n", label, if (good) "ok" else "FAIL"))
}

i <- as.int64(c("-2", "0", "3", NA))
u <- as.uint64(c("0", "9007199254740993", "18446744073709551614", NA))

ok("typeof is the opaque structural type",
   identical(c(typeof(i), typeof(u)), c("alt", "alt")))
ok("implicit classes name the semantics",
   identical(c(class(i), class(u)), c("int64", "uint64")))
ok("storage modes name the semantics",
   identical(c(storage.mode(i), storage.mode(u)), c("int64", "uint64")))
ok("mode remains numeric", identical(c(mode(i), mode(u)), c("numeric", "numeric")))
ok("only built-in integer ALTSXP classes satisfy is.xinteger",
   is.xinteger(i) && is.xinteger(u))
ok("width and kinds", xintegerWidth(i) == 8L && xintegerWidth(u) == 8L &&
   xintegerKind(i) == "signed" && xintegerKind(u) == "unsigned")
ok("unsupported widths are rejected",
   inherits(tryCatch(xinteger(1L, 4L), error = identity), "error"))

ok("decimal text is exact above 2^53",
   identical(as.character(u), c("0", "9007199254740993",
                                "18446744073709551614", NA)))
ok("raw payload round trips",
   identical(suppressWarnings(as.xinteger(xintegerRaw(u), 8L, "unsigned")), u))
ok("subsetting retains the class",
   identical(storage.mode(u[2:3]), "uint64") &&
   identical(as.character(u[2:3]), as.character(u)[2:3]))
ok("replacement retains exact values", {
    z <- u; z[1] <- as.uint64("17"); identical(as.character(z[1]), "17")
})
ok("concatenation retains the class",
   storage.mode(c(u, u)) == "uint64" && length(c(u, u)) == 8L)
ok("matrix construction retains the class", {
    m <- matrix(u, 2L); storage.mode(m) == "uint64" && identical(dim(m), c(2L, 2L))
})

ok("signed arithmetic retains exact type",
   identical(as.character(i[1:3] + as.int64("5")), c("3", "5", "8")))
ok("multiplication and integer division",
   identical(as.character(as.int64(c("7", "-7")) * 3L), c("21", "-21")) &&
   identical(as.character(as.int64(c("7", "-7")) %/% 3L), c("2", "-3")))
ok("modulo follows R floor-division semantics",
   identical(as.character(as.int64(c("7", "-7")) %% 3L), c("1", "2")))
ok("overflow uses the reserved sentinel", {
    z <- suppressWarnings(as.int64("9223372036854775807") + 1L); is.na(z)
})
ok("no-sentinel vectors reject overflow", {
    z <- as.uint64("18446744073709551615", na = FALSE)
    inherits(tryCatch(z + 1L, error = identity), "error")
})
ok("ordinary division follows real promotion",
   identical(as.int64("7") / 2L, 3.5))
ok("mixed real arithmetic follows real promotion",
   identical(as.int64("7") + 0.5, 7.5))

ok("comparison is exact above 2^53",
   as.uint64("9007199254740993") > as.uint64("9007199254740992"))
ok("sorting uses integer value order",
   identical(as.character(sort(u[1:3])),
             c("0", "9007199254740993", "18446744073709551614")))
ok("hashing and equality agree", {
    z <- c(u[1:3], u[2]);
    identical(as.character(unique(z)), as.character(u[1:3])) &&
    identical(match(z, u[1:3]), c(1L, 2L, 3L, 2L))
})
ok("formatting is exact",
   identical(trimws(format(u[1:3])), as.character(u[1:3])))
ok("summaries retain or deliberately promote", {
    z <- as.int64(c("1", "2", "4"))
    identical(as.character(sum(z)), "7") &&
    identical(mean(z), 7/3) && identical(prod(z), 8)
})

streamVersion <- function(r) readBin(r[3:6], "integer", 1L, endian = "big")
wire <- serialize(u, NULL)
ok("ALTREP state uses stream version 3", streamVersion(wire) == 3L)
ok("serialization round trip", identical(unserialize(wire), u))

cat(sprintf("\n%d failure(s)\n", fails))
if (fails) quit(status = 1L)
