SC <- "/private/tmp/claude-502/-Users-kevin-r-devel-r-svn/82235406-213e-4234-8b48-5f2668136107/scratchpad"
fails <- 0L
chk <- function(l, c) { if (!isTRUE(c)) fails <<- fails + 1L
                        cat(sprintf("%-30s %s\n", l, if (isTRUE(c)) "ok" else "FAIL")) }
rd <- function(f) { p <- file.path(SC, f); readBin(p, "raw", n = file.size(p)) }
## our NA prints as "NA"; compare as character so exact 128-bit values survive
str_ <- function(v) { s <- as.character(v); s[is.na(v)] <- "NA"; s }

for (spec in list(list("u8",8L,"unsigned"), list("i8",8L,"signed"),
                  list("i16",16L,"signed"), list("u4",4L,"unsigned"),
                  list("i1",1L,"signed"), list("u2",2L,"unsigned"))) {
    tag <- spec[[1]]; w <- spec[[2]]; k <- spec[[3]]
    a <- as.bytes(rd(paste0("ar_",tag,"_a.bin")), w, k)
    b <- as.bytes(rd(paste0("ar_",tag,"_b.bin")), w, k)
    exp <- read.delim(file.path(SC, paste0("ar_",tag,"_exp.txt")),
                      header = FALSE, colClasses = "character", na.strings = character())
    cat(sprintf("\n-- %s (width %d, %s), %d pairs --\n", tag, w, k, length(a)))
    got <- suppressWarnings(list(a + b, a - b, a * b, a %/% b, a %% b))
    for (j in seq_along(got))
        chk(c("+","-","*","%/%","%%")[j], identical(str_(got[[j]]), exp[[j]]))
    if (k == "signed")
        chk("unary -", identical(str_(suppressWarnings(-a)), exp[[6]]))
}

cat("\n-- coercion --\n")
u <- as.bytes(rd("ar_u8_a.bin"), 8L, "unsigned")
chk("as.numeric is finite",   all(is.finite(suppressWarnings(as.numeric(u)))))
chk("as.integer in range",    { i <- suppressWarnings(as.integer(u))
                                all(is.na(i) | (i >= -.Machine$integer.max &
                                                i <= .Machine$integer.max)) })
small <- as.bytes(as.raw(c(rev(c(0,0,0,0,0,0,0,42)), rev(c(0,0,0,0,0,0,0,7)))), 8L, "signed")
chk("as.integer exact small", identical(as.integer(small), c(42L, 7L)))
chk("as.numeric exact small", identical(as.numeric(small), c(42, 7)))
chk("as.integer NA",          is.na(as.integer(bytesNA(1L, 8L, "signed"))))
chk("opaque coercion errors", inherits(tryCatch(as.integer(as.bytes(as.raw(1:8), 8L)),
                                                error = identity), "error"))
cat(sprintf("\n%d failure(s)\n", fails))
if (fails) quit(status = 1L)
