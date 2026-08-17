# Gauntlet for the wide (64-bit) integer vector prototype.
# Each probe reports: ok / ERR, the expression, and the value or message.
# The pattern of failures is the point: it catalogs which base operations
# need wide-awareness and which fail loudly via the guarded accessors.

w  <- function(x) .Internal(as.wideint(x))
iw <- function(x) .Internal(is.wideint(x))

probe <- function(desc, expr) {
    wmsg <- NULL
    out <- tryCatch(
        withCallingHandlers(
            eval(expr),
            warning = function(cnd) {
                wmsg <<- conditionMessage(cnd)
                invokeRestart("muffleWarning")
            }
        ),
        error = function(e) structure(conditionMessage(e), class = "probe_error")
    )

    if (inherits(out, "probe_error")) {
        status <- "ERR"
        text <- unclass(out)
    } else {
        status <- "ok "
        text <- tryCatch(
            paste(utils::capture.output(print(out)), collapse = " / "),
            error = function(e) paste("<print failed:", conditionMessage(e), ">")
        )
    }

    if (!is.null(wmsg))
        text <- paste0(text, "  [warn: ", wmsg, "]")
    if (nchar(text) > 110)
        text <- paste0(substr(text, 1, 110), "...")
    cat(sprintf("%s | %-42s | %s\n", status, desc, text))
}

section <- function(name) cat("\n#### ", name, "\n")

big  <- w("5000000000")                       # 5e9 > 2^31
huge <- w("1152921504606846976")              # 2^60
xs   <- w(c("5000000000", "6000000000", "7000000000"))
mix  <- w(c("1", NA, "5000000000"))
small <- w(c("1", "2", "3"))                  # wide but 32-bit-representable

section("creation and identity")
probe("iw(big)", quote(iw(big)))
probe("typeof(big)", quote(typeof(big)))
probe("class(big)", quote(class(big)))
probe("is.integer(big)", quote(is.integer(big)))
probe("is.numeric(big)", quote(is.numeric(big)))
probe("length(xs)", quote(length(xs)))
probe("w(1:3) from narrow", quote(w(1:3)))
probe("w(2^40) from double", quote(w(2^40)))
probe("w(1.5) fractional errors", quote(w(1.5)))
probe("is.na(mix)", quote(is.na(mix)))
probe("storage.mode(big)", quote(storage.mode(big)))

section("printing")
probe("print(big)", quote(print(big)))
probe("print(xs)", quote(print(xs)))
probe("print(mix)", quote(print(mix)))
probe("print(named wide)", quote(print(setNames(xs, c("a", "b", "c")))))
probe("str(xs)", quote(str(xs)))
probe("format(big)", quote(format(big)))
probe("cat(big)", quote(cat(big, "\n")))
probe("deparse(big)", quote(deparse(big)))
probe("paste(big)", quote(paste(big)))
probe("sprintf %d", quote(sprintf("%d", big)))

section("coercion")
probe("as.numeric(big)", quote(as.numeric(big)))
probe("as.numeric(huge) warns >2^53", quote(as.numeric(huge)))
probe("as.character(big)", quote(as.character(big)))
probe("as.logical(mix)", quote(as.logical(mix)))
probe("as.integer(small in range)", quote(as.integer(small)))
probe("as.integer(big) identity (wide)", quote(iw(as.integer(big))))
probe("as.vector(xs, 'list')", quote(as.vector(xs, "list")))
probe("as.double round trip", quote(w(as.numeric(big)) == big))

section("arithmetic: narrow overflow promotion")
probe(".Machine$integer.max + 1L", quote(2147483647L + 1L))
probe("iw(.Machine int.max + 1L)", quote(iw(2147483647L + 1L)))
probe("narrow * narrow overflow", quote(100000L * 100000L))
probe("narrow - narrow underflow", quote(-2147483647L - 100L))
probe("no-overflow stays narrow", quote(iw(1L + 1L)))

section("arithmetic: wide operands")
probe("big + 1L", quote(big + 1L))
probe("big * 2L", quote(big * 2L))
probe("-big", quote(-big))
probe("big - big", quote(big - big))
probe("big / 2L (double)", quote(big / 2L))
probe("big %/% 3L", quote(big %/% 3L))
probe("big %% 3L", quote(big %% 3L))
probe("big ^ 2L (double)", quote(big ^ 2L))
probe("big + 0.5 (double)", quote(big + 0.5))
probe("huge + 0.5 warns precision", quote(huge + 0.5))
probe("huge * huge overflows to NA", quote(huge * huge))
probe("xs + xs vectorized", quote(xs + xs))
probe("xs + 1:3 recycle narrow", quote(xs + 1:3))
probe("wide + TRUE", quote(big + TRUE))
probe("sum via + on NA", quote(mix + 1L))

section("comparison")
probe("big > 1L", quote(big > 1L))
probe("big == w('5000000000')", quote(big == w("5000000000")))
probe("big < 6e9", quote(big < 6e9))
probe("huge == 2^60 exact", quote(huge == 2^60))
probe("huge+1 == 2^60 not equal", quote((huge + w("1")) == 2^60))
probe("xs >= 6e9 vectorized", quote(xs >= 6e9))
probe("mix > 0L propagates NA", quote(mix > 0L))
probe("if (big > 0L)", quote(if (big > 0L) "yes" else "no"))

section("subsetting")
probe("xs[2]", quote(xs[2]))
probe("xs[c(1,3)]", quote(xs[c(1, 3)]))
probe("xs[-1]", quote(xs[-1]))
probe("xs[c(TRUE,FALSE,TRUE)]", quote(xs[c(TRUE, FALSE, TRUE)]))
probe("xs[[2]]", quote(xs[[2]]))
probe("rev(xs)", quote(rev(xs)))
probe("head(xs, 2)", quote(head(xs, 2)))
probe("xs[10] out of bounds -> NA", quote(xs[10]))

section("subassignment")
probe("xs[1] <- w('9000000000')", quote({ y <- xs; y[1] <- w("9000000000"); y }))
probe("xs[1] <- 1L", quote({ y <- xs; y[1] <- 1L; y }))
probe("xs[1] <- 1.0", quote({ y <- xs; y[1] <- 1.0; y }))
probe("narrow[1] <- wide promotes?", quote({ y <- 1:3; y[1] <- big; y }))
probe("xs[[2]] <- w('123')", quote({ y <- xs; y[[2]] <- w("123"); y }))

section("combination")
probe("c(xs, xs)", quote(c(xs, xs)))
probe("c(big, 1L)", quote(c(big, 1L)))
probe("c(big, 1.5)", quote(c(big, 1.5)))
probe("c(big, 'a')", quote(c(big, "a")))
probe("unlist(list(big, big))", quote(unlist(list(big, big))))
probe("rep(big, 3)", quote(rep(big, 3)))
probe("rbind(xs, xs)", quote(rbind(xs, xs)))

section("summaries and sorting")
probe("sum(xs)", quote(sum(xs)))
probe("mean(xs)", quote(mean(xs)))
probe("min(xs) / max(xs)", quote(c(min(xs), max(xs))))
probe("range(xs)", quote(range(xs)))
probe("prod(xs)", quote(prod(xs)))
probe("sort(rev(xs))", quote(sort(rev(xs))))
probe("order(rev(xs))", quote(order(rev(xs))))
probe("unique(c(xs, xs))", quote(unique(c(xs, xs))))
probe("duplicated(c(xs, xs))", quote(duplicated(c(xs, xs))))
probe("match(big, xs)", quote(match(big, xs)))
probe("big %in% xs", quote(big %in% xs))
probe("which.max(xs)", quote(which.max(xs)))
probe("which(xs > 5e9)", quote(which(xs > 5e9)))
probe("cumsum(xs)", quote(cumsum(xs)))
probe("abs(-big)", quote(abs(-big)))
probe("sqrt(big) (double)", quote(sqrt(big)))
probe("bitwAnd(big, big)", quote(bitwAnd(big, big)))

section("structures")
probe("names(xs) <- letters[1:3]", quote({ y <- xs; names(y) <- c("a", "b", "c"); names(y) }))
probe("matrix(xs, nrow = 1)", quote(matrix(xs, nrow = 1)))
probe("dim<- on wide", quote({ y <- c(xs, xs); dim(y) <- c(2, 3); y }))
probe("list(big)", quote(list(big)))
probe("data.frame(x = xs)", quote(data.frame(x = xs)))
probe("identical(big, w('5000000000'))", quote(identical(big, w("5000000000"))))
probe("identical(big, 5e9)", quote(identical(big, 5e9)))
probe("all.equal(big, w('5000000000'))", quote(all.equal(big, w("5000000000"))))

section("control flow and apply")
probe("for (v in xs) typeof(v)", quote({ r <- NULL; for (v in xs) r <- c(r, typeof(v)); r }))
probe("for value survives wide", quote({ r <- NULL; for (v in xs) r <- c(r, v > 4e9); r }))
probe("sapply(xs, function(x) x + 1L)", quote(sapply(xs, function(x) x + 1L)))
probe("lapply(xs, identity)", quote(lapply(xs, identity)))
probe("vapply numeric", quote(vapply(xs, as.numeric, numeric(1))))
probe("Map(`+`, xs, xs)", quote(Map(`+`, xs, xs)))
probe("do.call('+', list(big, 1L))", quote(do.call("+", list(big, 1L))))

section("bytecode consistency")
probe("compiled overflow promotes?", quote({
    f <- function(x) x + 1L
    fc <- compiler::cmpfun(f)
    r <- fc(2147483647L)
    c(iw(r), r == w("2147483648"))
}))
probe("compiled wide arithmetic", quote({
    g <- function(x) x * 2L
    gc <- compiler::cmpfun(g)
    gc(big)
}))
probe("compiled loop over wide", quote({
    h <- function(v) { s <- 0; for (x in v) s <- s + as.numeric(x); s }
    hc <- compiler::cmpfun(h)
    hc(xs)
}))

section("serialization (guarded)")
probe("serialize(big) errors", quote(serialize(big, NULL)))
probe("narrow serialize unaffected", quote(length(serialize(1:3, NULL))))

section("seq and misc")
probe("seq_along(xs)", quote(seq_along(xs)))
probe("big:(big) colon", quote(big:big))
probe("xtfrm(xs)", quote(xtfrm(xs)))
probe("tabulate-ish table(small)", quote(table(small)))
probe("outer(small, small)", quote(outer(small, small)))
probe("wide in switch/eval", quote(eval(quote(big + big))))

cat("\ndone.\n")
