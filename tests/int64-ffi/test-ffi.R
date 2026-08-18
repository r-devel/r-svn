dyn.load(paste0("pkgA", .Platform$dynlib.ext))
dyn.load(paste0("pkgB", .Platform$dynlib.ext))

x   <- as.int64(c(1, 2, 3))
big <- as.int64("5000000000")

## .Call default: int64 narrowed to integer (typecode 13)
stopifnot(.Call("a_typecode", x) == 13L)
stopifnot(.Call("a_first", x) == 1L)

## unrepresentable values error at the boundary, naming routine + arg
e <- tryCatch(.Call("a_first", big), error = conditionMessage)
stopifnot(is.character(e), grepl("32-bit integer range", e),
          grepl("a_first", e))

## non-int64 arguments are untouched
stopifnot(.Call("a_typecode", 1:3) == 13L)
stopifnot(.Call("a_typecode", 1.5) == 14L)

## opt-in package receives INT64SXP (typecode 11) and can read wide values
stopifnot(.Call("b_typecode", x) == 11L)
stopifnot(.Call("b_first64", big) == 5e9)

## .C always narrows (no opt-in mechanism there)
r <- .C("a_csum", x, 3L, out = double(1))
stopifnot(r$out == 6, is.integer(r[[1L]]))
e2 <- tryCatch(.C("a_csum", big, 1L, out = double(1)), error = conditionMessage)
stopifnot(is.character(e2), grepl("32-bit integer range", e2))

cat("FFI boundary tests OK\n")
