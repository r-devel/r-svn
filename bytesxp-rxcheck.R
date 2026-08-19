SC <- "/private/tmp/claude-502/-Users-kevin-r-devel-r-svn/82235406-213e-4234-8b48-5f2668136107/scratchpad"
fails <- 0L
chk <- function(l, c) { if (!isTRUE(c)) fails <<- fails + 1L
                        cat(sprintf("%-38s %s\n", l, if (isTRUE(c)) "ok" else "FAIL")) }
rd <- function(f) { p <- file.path(SC, f); readBin(p, "raw", n = file.size(p)) }
for (spec in list(list("u8",8L,"unsigned"), list("i8",8L,"signed"), list("i16",16L,"signed"),
                  list("u4",4L,"unsigned"), list("i1",1L,"signed"), list("u2",2L,"unsigned"))) {
    tag <- spec[[1]]; w <- spec[[2]]; k <- spec[[3]]
    x <- as.bytes(rd(paste0("rx_",tag,".bin")), w, k)
    asc  <- as.integer(readLines(file.path(SC, paste0("rx_",tag,"_asc.txt"))))
    desc <- as.integer(readLines(file.path(SC, paste0("rx_",tag,"_desc.txt"))))
    cat(sprintf("\n-- %s (width %d, %s), n = %d, heavy ties --\n", tag, w, k, length(x)))
    chk("order ascending (stable ties)",  identical(order(x), asc))
    chk("order decreasing (stable ties)", identical(order(x, decreasing=TRUE), desc))
    chk("sort matches order",             identical(sort(x), x[asc]))
    chk("sort decreasing matches order",  identical(sort(x, decreasing=TRUE), x[desc]))
    chk("sorted really is sorted",        !is.unsorted(sort(x)))
    ## NAs must still land per na.last, on both sides
    xn <- c(x[1:50], bytesNA(3L, w, k), x[51:100])
    chk("NA last by default",             all(is.na(xn[order(xn)][101:103])))
    chk("NA first when asked",            all(is.na(xn[order(xn, na.last=FALSE)][1:3])))
    chk("sort drops NA",                  length(sort(xn)) == 100L)
}
cat(sprintf("\n%d failure(s)\n", fails))
if (fails) quit(status = 1L)
