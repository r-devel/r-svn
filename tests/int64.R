### Tests for int64 support

capture_warning <- function(expr)
{
    warning <- NULL
    value <- withCallingHandlers(expr,
                                 warning = function(w) {
                                     warning <<- conditionMessage(w)
                                     invokeRestart("muffleWarning")
                                 })
    list(value = value, warning = warning)
}

is_try_error <- function(expr)
{
    inherits(try(expr, silent = TRUE), "try-error")
}


## Integer literals promote to int64 when they exceed the int32 range.
literal_overflow <- capture_warning(eval(parse(text = "9223372036854775808L")))
integer_overflow <- capture_warning(as.integer("2147483648"))
stopifnot(
    typeof(2147483647L) == "integer",
    typeof(2147483648L) == "int64",
    typeof(9223372036854775807) == "double",
    identical(2147483648L, as.int64("2147483648")),
    identical(integer_overflow$value, NA_integer_),
    grepl("integer range", integer_overflow$warning, fixed = TRUE),
    typeof(literal_overflow$value) == "int64",
    is.na(literal_overflow$value),
    grepl("int64 range", literal_overflow$warning, fixed = TRUE)
)

## as.int64 accepts exact decimal, scientific, and hexadecimal spellings.
inexact_decimal <- capture_warning(as.int64("1.00000000000000001"))
min_int64_from_real <- capture_warning(as.int64(-9223372036854775808))
fractional_literal <- capture_warning(eval(parse(text = "9007199254740993.1L")))
stopifnot(
    identical(as.int64("1.0"), as.int64("1")),
    identical(as.int64("1e3"), as.int64("1000")),
    identical(as.int64("0x1p3"), as.int64("8")),
    identical(as.int64("0x10"), as.int64("16")),
    is.na(inexact_decimal$value),
    grepl("int64 range", inexact_decimal$warning, fixed = TRUE),
    is.na(min_int64_from_real$value),
    is.character(min_int64_from_real$warning),
    grepl("int64 range", min_int64_from_real$warning, fixed = TRUE),
    typeof(fractional_literal$value) == "double",
    grepl("contains decimal; using numeric value",
          fractional_literal$warning, fixed = TRUE),
    identical(as.int64("0x1eeeeeeeeeeeeeee"),
              as.int64("2228981575573237486")),
    identical(0x1eeeeeeeeeeeeeeeL, as.int64("2228981575573237486")),
    identical(0x7fffffffffffffffL, as.int64("9223372036854775807"))
)

## Integer arithmetic stays integer until int64 is needed.
int64_multiply_overflow <- capture_warning(as.int64("4611686018427387904") *
                                           as.int64("2"))
stopifnot(
    typeof(1L + 2L) == "integer",
    typeof(2147483647L + 1L) == "int64",
    identical(2147483647L + 1L, as.int64("2147483648")),
    typeof(c(1L, 2L) + 1L) == "integer",
    typeof(c(1L, 2L) - 1L) == "integer",
    typeof(c(1L, 2L) * 2L) == "integer",
    typeof(c(1L, 2L) %% 2L) == "integer",
    typeof(c(1L, 2L) %/% 2L) == "integer",
    typeof(int64_multiply_overflow$value) == "int64",
    is.na(int64_multiply_overflow$value),
    grepl("int64 overflow", int64_multiply_overflow$warning, fixed = TRUE)
)

## Atomic vectors promote logical and integer values to int64, but not doubles.
stopifnot(
    typeof(c(TRUE, as.int64("2"))) == "int64",
    typeof(c(1L, as.int64("2"))) == "int64",
    typeof(c(as.int64("2"), 2)) == "double"
)

## Comparisons preserve precision across integer, double, complex, and raw values.
big_int64 <- as.int64("9007199254740993")
big_double <- 9007199254740992
big_complex <- 9007199254740992+0i
stopifnot(
    identical(as.int64("9223372036854775807") > as.int64("1"), TRUE),
    identical(as.int64("-1") < as.int64("1"), TRUE),
    identical(as.int64("9223372036854775807") < as.int64("-1"), FALSE),
    identical(as.int64("256") == as.raw(0), FALSE),
    identical(as.raw(255) < as.int64("256"), TRUE),
    identical(as.int64("2") > 1L, TRUE),
    identical(big_int64 == big_double, FALSE),
    identical(big_int64 > big_double, TRUE),
    identical(big_double < big_int64, TRUE),
    identical(big_int64 == big_complex, FALSE),
    identical(big_int64 != big_complex, TRUE),
    identical(as.int64("9007199254740992") == big_complex, TRUE),
    identical(as.int64("2") == 2+1i, FALSE),
    identical(as.int64("2") != 2+1i, TRUE),
    is.na(as.int64(NA) == as.int64(NA)),
    identical(!as.int64(c("0", "2", NA)), c(TRUE, FALSE, NA))
)

## cbind, rbind, as.vector, names, and unlist preserve int64 values.
named_int64 <- as.int64("1")
names(named_int64) <- "a"
named_pair <- setNames(as.int64(c("1", "2")), c("a", "b"))
int64_cbind <- cbind(a = as.int64("1"))
int64_rbind <- rbind(a = as.int64("1"))
real_cbind <- cbind(as.int64("2147483648"), 1.5)
real_rbind <- rbind(as.int64("2147483648"), 1.5)
list_cbind <- cbind(list(1), as.int64(c("2", "3")))
stopifnot(
    typeof(int64_cbind) == "int64",
    identical(dim(int64_cbind), c(1L, 1L)),
    identical(unname(int64_cbind[1, 1]), as.int64("1")),
    identical(as.vector(int64_cbind, "int64"), as.int64("1")),
    typeof(int64_rbind) == "int64",
    identical(dim(int64_rbind), c(1L, 1L)),
    identical(unname(int64_rbind[1, 1]), as.int64("1")),
    identical(as.vector(int64_rbind, "int64"), as.int64("1")),
    typeof(real_cbind) == "double",
    identical(unname(real_cbind[1, 1]), 2147483648),
    typeof(real_rbind) == "double",
    identical(unname(real_rbind[1, 1]), 2147483648),
    typeof(list_cbind) == "list",
    identical(dim(list_cbind), c(2L, 2L)),
    identical(list_cbind[[3]], as.int64("2")),
    identical(list_cbind[[4]], as.int64("3")),
    identical(as.vector(named_int64, "int64"), as.int64("1")),
    identical(as.vector(named_int64, "any"), as.int64("1")),
    identical(names(c(named_pair)), c("a", "b")),
    identical(names(unlist(list(x = named_pair))), c("x.a", "x.b"))
)

## Formatting and source round trips print int64 values without precision loss.
large_int64 <- as.int64("9007199254740993")
named_for_deparse <- as.int64("1")
names(named_for_deparse) <- "a"
old_prompt <- getOption("prompt")
on.exit(options(prompt = old_prompt), add = TRUE)
options(prompt = large_int64)
stopifnot(
    identical(format(as.int64(c("1", "2147483648", NA)), trim = TRUE),
              c("1", "2147483648", "NA")),
    identical(format.info(as.int64(c("1", "2147483648", NA))), 10L),
    identical(capture.output(cat(as.int64(c("1", NA)), sep = "\n")),
              c("1", "NA")),
    identical(capture.output(write.table(data.frame(x = as.int64(NA)),
                                         row.names = FALSE, na = "")),
              c("\"x\"", "")),
    is.character(capture.output(data.frame(x = as.int64(c("1", "2"))))),
    identical(formatC(as.int64("1")), "1"),
    identical(formatC(large_int64), "9007199254740993"),
    identical(getOption("prompt"), "9007199254740993"),
    identical(deparse(2147483648L), "2147483648L"),
    identical(eval(parse(text = deparse(2147483648L))), 2147483648L),
    identical(9007199254740993e0L, as.int64("9007199254740993")),
    identical(9223372036854775807e0L, as.int64("9223372036854775807")),
    identical(eval(parse(text = deparse(as.int64("1")))), as.int64("1")),
    identical(eval(parse(text = deparse(as.int64(c("1", "2147483648", NA))))),
              as.int64(c("1", "2147483648", NA))),
    identical(eval(parse(text = deparse(named_for_deparse))),
              named_for_deparse),
    is.character(capture.output(matrix(as.int64("1"), 1L))),
    identical(capture.output(as.int64(character(0))),
              "as.int64(character(0))"),
    identical(eval(parse(text = capture.output(as.int64(character(0))))),
              as.int64(character(0))),
    grepl("^ int64 ", capture.output(str(as.int64(1)))[1L])
)

## int64 values can be used as ordinary scalar subscripts and sizes.
dim_matrix <- matrix(1:4, 2L)
subscript_matrix <- matrix(as.int64(c("2", "2")), ncol = 2L)
subscript_assignment <- dim_matrix
subscript_assignment[subscript_matrix] <- 9L
stopifnot(
    identical((1:4)[as.int64("3")], 3L),
    identical((1:3)[as.int64("-9007199254740993")], 1:3),
    identical((1:4)[[as.int64("3")]], 3L),
    identical(length(vector("list", as.int64("3"))), 3L),
    identical(rep(1L, times = as.int64("3")), rep(1L, 3L)),
    identical(as.int64(c("1", "2"))[[2]], as.int64("2")),
    identical(dim_matrix[as.int64("1"), ], c(1L, 3L)),
    identical(dim_matrix[, as.int64("2")], c(3L, 4L)),
    identical(dim_matrix[subscript_matrix], 4L),
    identical(subscript_assignment, matrix(c(1L, 2L, 3L, 9L), 2L)),
    is_try_error(vector("list", as.int64("-2147483649"))),
    is_try_error(dim_matrix[[as.int64("4294967297"), 1L]]),
    .Machine$sizeof.pointer != 4L || {
        z <- list(1L)
        is_try_error(z[[as.int64("4294967297")]] <- 2L)
    }
)

## Subassignment preserves int64 values in vectors, lists, pairlists, and arrays.
stopifnot(
    {
        z <- as.int64("1")
        z[1] <- as.int64("2")
        identical(z, as.int64("2"))
    },
    {
        z <- list(1L)
        z[[1]] <- as.int64("9007199254740993")
        identical(z[[1]], as.int64("9007199254740993"))
    },
    {
        z <- pairlist(pairlist(1L))
        z[[as.int64(c("1", "1"))]] <- 2L
        identical(z, pairlist(pairlist(2L)))
    },
    {
        z <- pairlist(pairlist(1L, 2L))
        z[[as.int64(c("1", "2"))]] <- NULL
        identical(z, pairlist(pairlist(1L)))
    },
    {
        z <- expression(1L)
        z[[1]] <- as.int64("9007199254740993")
        identical(z[[1]], as.int64("9007199254740993"))
    },
    {
        z <- matrix(as.int64("1"), 1L)
        z[1, 1] <- as.int64("2")
        identical(z, matrix(as.int64("2"), 1L))
    },
    {
        z <- array(as.int64("1"), c(1L, 1L, 1L))
        z[1, 1, 1] <- as.int64("2")
        identical(z, array(as.int64("2"), c(1L, 1L, 1L)))
    },
    {
        z <- as.int64("1")
        length(z) <- 3L
        identical(z, c(as.int64("1"), as.int64(NA), as.int64(NA)))
    },
    {
        z <- as.int64("1")
        z[3] <- as.int64("2")
        identical(z, as.int64(c("1", NA, "2")))
    },
    {
        z <- 1:3
        tsp(z) <- as.int64(c("1", "3", "1"))
        identical(tsp(z), c(1, 3, 1))
    }
)

## Iteration and grouping APIs keep int64 values intact.
for_values <- local({
    out <- vector("int64", 0)
    for (i in as.int64(c("1", "2"))) out <- c(out, i)
    out
})
for_cmp <- compiler::cmpfun(function(x) {
    out <- vector("int64", length(x))
    j <- 0L
    for (i in x) {
        j <- j + 1L
        out[j] <- i
    }
    out
})
group <- gl(2, 1)
split_values <- split(as.int64(c("1", "2")), group)
tapply_values <- tapply(as.int64(c("1", "2")), group, identity,
                        simplify = FALSE)
stopifnot(
    identical(for_values, as.int64(c("1", "2"))),
    identical(for_cmp(as.int64(c("1", "2"))), as.int64(c("1", "2"))),
    identical(split_values, list("1" = as.int64("1"), "2" = as.int64("2"))),
    identical(unname(c(tapply_values)), unname(split_values)),
    identical(ave(as.int64(c("1", "2")), group, FUN = identity),
              as.int64(c("1", "2"))),
    identical(aggregate(as.int64(c("1", "2")), list(g = group), identity),
              data.frame(g = group, x = as.int64(c("1", "2"))))
)

## S3 and S4 dispatch treat int64 as a numeric vector type.
stopifnot(local({
    f <- function(x) UseMethod("f")
    f.numeric <- function(x) "numeric"
    identical(f(as.int64("1")), "numeric")
}))
s4_dispatch <- local({
    library(methods)
    f <- setGeneric("i64_s4_f", function(x) standardGeneric("i64_s4_f"))
    on.exit(removeGeneric("i64_s4_f"), add = TRUE)
    setMethod(f, "numeric", function(x) "numeric")
    g <- setGeneric("i64_s4_g", function(x) standardGeneric("i64_s4_g"))
    on.exit(removeGeneric("i64_s4_g"), add = TRUE)
    setMethod(g, "double", function(x) "double")
    A <- setClass("i64_s4_A", slots = c(x = "int64"))
    on.exit(removeClass("i64_s4_A"), add = TRUE)

    x <- as.int64("1")
    y <- x
    as(y, "double") <- 2
    list(is_numeric = is(x, "numeric"),
         is_double = is(x, "double"),
         dispatch = i64_s4_f(x),
         dispatch_double = i64_s4_g(x),
         as_double = as(x, "double"),
         as_int64 = list(character = as("2147483648", "int64"),
                         logical = as(TRUE, "int64"),
                         integer = as(1L, "int64")),
         double_replace = y,
         valid_slot = validObject(A(x = x)))
})
stopifnot(
    identical(s4_dispatch,
              list(is_numeric = TRUE,
                   is_double = TRUE,
                   dispatch = "numeric",
                   dispatch_double = "double",
                   as_double = 1,
                   as_int64 = list(character = as.int64("2147483648"),
                                   logical = as.int64(TRUE),
                                   integer = as.int64(1L)),
                   double_replace = as.int64("2"),
                   valid_slot = TRUE))
)

## Assigning implicit classes coerces to their corresponding storage type.
stopifnot(local({
    expected <- as.int64("9007199254740993")
    x <- expected
    class(x) <- "numeric"
    typeof(x) == "int64" &&
        identical(x, expected) &&
        identical(class(x), "int64")
}))
stopifnot(local({
    x <- 1
    class(x) <- "int64"
    typeof(x) == "int64" &&
        identical(x, as.int64("1")) &&
        identical(class(x), "int64")
}))

## all.equal and model APIs compare int64 values through the public numeric path.
model_data <- data.frame(x = as.int64(1:3), y = 1:3)
model_frame <- stats::model.frame(y ~ x, data = model_data)
model_fit <- stats::lm(y ~ x, data = model_data)
stopifnot(
    isTRUE(all.equal(as.int64("1"), as.int64("1"))),
    isTRUE({
        x <- as.int64("1")
        class(x) <- "foo"
        all.equal(x, unclass(x), check.attributes = FALSE,
                  check.class = FALSE)
    }),
    is.character(all.equal(as.int64("1"), as.int64("2"))),
    is.character(all.equal(as.int64("1"), as.int64(c("1", "2")))),
    is.character(all.equal(as.int64("9007199254740993"),
                           as.int64("9007199254740992"))),
    identical(typeof(model_frame$x), "int64"),
    isTRUE(all.equal(unname(stats::coef(model_fit)), c(0, 1)))
)

## Stats and graphics APIs accept int64 values at their numeric boundaries.
nlm_start <- stats::nlm(function(x) sum(x * x), as.int64("1"))
nlm_typsize <- stats::nlm(function(x) sum(x * x), 1,
                          typsize = as.int64("1"))
convolved <- stats::convolve(as.int64(c("1", "2", "3")),
                             as.int64(c("1", "2", "3")))
deriv_expr <- stats::deriv(~ x + 2147483648L, "x")
deriv_value <- eval(deriv_expr, list(x = 1))
stopifnot(
    identical(stats::D(2147483648L, "x"), 0),
    is.expression(deriv_expr),
    identical(c(attr(deriv_value, "gradient")), 1),
    abs(nlm_start$estimate) < 1e-4,
    abs(nlm_typsize$estimate) < 1e-4,
    identical(stats::dnorm(as.int64("1")), stats::dnorm(1)),
    isTRUE(all.equal(convolved, stats::convolve(1:3, 1:3))),
    local({
        grDevices::pdf(NULL)
        on.exit(grDevices::dev.off())
        graphics::plot.new()
        graphics::plot.window(xlim = as.int64(c("1", "2")), ylim = 0:1,
                              xaxs = "i", yaxs = "i")
        identical(graphics::par("usr"), c(1, 2, 0, 1))
    }),
    local({
        grDevices::pdf(NULL)
        on.exit(grDevices::dev.off())
        graphics::plot.new()
        graphics::text(0.5, 0.5, "x", adj = as.int64("1"))
        TRUE
    })
)

## RDS and save files use the version that can represent int64 values.
rds_path <- tempfile()
saveRDS(as.int64("1"), rds_path)
rds_info <- infoRDS(rds_path)
unlink(rds_path)

save_path <- tempfile()
saved_value <- as.int64("1")
save(saved_value, file = save_path, compress = FALSE)
save_con <- file(save_path, "rb")
save_magic <- readChar(save_con, 5L, useBytes = TRUE)
close(save_con)
save_env <- new.env(parent = emptyenv())
load(save_path, envir = save_env)
unlink(save_path)
stopifnot(
    identical(unserialize(serialize(as.int64(c("1", NA)), NULL)),
              as.int64(c("1", NA))),
    identical(rds_info$version, 4L),
    identical(rds_info$min_reader_version, rds_info$writer_version),
    identical(save_magic, "RDX4\n"),
    identical(save_env$saved_value, as.int64("1")),
    is_try_error(serialize(as.int64("1"), NULL, version = 3)),
    is.raw(try(serialize(as.pairlist(rep.int(list(1L), 100000L)), NULL),
               silent = TRUE))
)

## Serialization refhooks see int64-containing reference objects once.
refhook_count <- 0L
refhook_count_raw <- serialize(methods::getClass("ANY")@versionKey, NULL,
                               refhook = function(x) {
                                   refhook_count <<- refhook_count + 1L
                                   "id"
                               })
refhook_ptr <- methods::getClass("ANY")@versionKey
attr(refhook_ptr, "x") <- as.int64("1")
refhook_v3_raw <- try(serialize(refhook_ptr, NULL, version = 3,
                                refhook = function(x) "id"),
                      silent = TRUE)

refhook_once_env <- new.env(parent = emptyenv())
refhook_once_env$x <- as.int64("1")
refhook_once_count <- 0L
refhook_once_raw <- serialize(refhook_once_env, NULL,
                              refhook = function(x) {
                                  refhook_once_count <<-
                                      refhook_once_count + 1L
                                  if (refhook_once_count == 1L) "id" else NULL
                              })
refhook_once_con <- rawConnection(refhook_once_raw)
refhook_once_info <- infoRDS(refhook_once_con)
close(refhook_once_con)

refhook_decline_env <- new.env(parent = emptyenv())
refhook_decline_env$x <- as.int64("1")
refhook_decline_raw <- serialize(refhook_decline_env, NULL,
                                 refhook = function(x) NULL)
refhook_decline_out <- unserialize(refhook_decline_raw)

refhook_rds <- tempfile()
saveRDS(refhook_ptr, refhook_rds, refhook = function(x) "id")
refhook_rds_info <- infoRDS(refhook_rds)
unlink(refhook_rds)
stopifnot(
    is.raw(refhook_count_raw),
    identical(refhook_count, 1L),
    inherits(refhook_v3_raw, "try-error"),
    is.raw(refhook_once_raw),
    identical(refhook_once_count, 1L),
    identical(refhook_once_info$version, 4L),
    is.raw(refhook_decline_raw),
    identical(refhook_decline_out$x, as.int64("1")),
    identical(refhook_rds_info$version, 4L)
)

## Object bookkeeping records int64 vectors and serialized bindings.
scalar_binding_raw <- compiler::cmpfun(function() {
    for (i in 1L) return(is.raw(serialize(environment(), NULL)))
})()
binding_rds_info <- compiler::cmpfun(function() {
    x <- as.int64("1")
    for (i in 1L) {
        path <- tempfile()
        on.exit(unlink(path), add = TRUE)
        saveRDS(environment(), path)
        return(infoRDS(path))
    }
})()
object_size <- utils::object.size(as.int64("1"))
int64_element_size <- utils::object.size(vector("int64", 2)) -
    utils::object.size(vector("int64", 1))
gc_ok <- {
    x <- vector("int64", 1e6)
    rm(x)
    invisible(gc())
    TRUE
}
stopifnot(
    inherits(object_size, "object_size"),
    as.numeric(object_size) > 0,
    identical(as.numeric(int64_element_size), 8),
    identical(scalar_binding_raw, TRUE),
    identical(binding_rds_info$version, 4L),
    identical(gc_ok, TRUE),
    is.integer(memory.profile()["int64"]),
    !is.na(memory.profile()["int64"])
)

## match, duplicated, and unique use exact int64 equality.
big_int64 <- as.int64("9007199254740993")
big_double <- 9007199254740992
big_complex <- 9007199254740992+0i
stopifnot(
    identical(match(as.int64("1"), as.int64("1")), 1L),
    identical(match(big_int64, big_double, nomatch = 0L), 0L),
    identical(match(big_double, big_int64, nomatch = 0L), 0L),
    identical(match(big_double, as.int64("9007199254740992"), nomatch = 0L),
              1L),
    identical(match(big_int64, big_complex, nomatch = 0L), 0L),
    identical(match(as.int64("9007199254740992"), big_complex,
                    nomatch = 0L), 1L),
    identical(match(big_complex, as.int64("9007199254740992"),
                    nomatch = 0L), 1L),
    identical(match(as.int64(NA), NA_complex_, nomatch = 0L), 1L),
    identical(match(big_double, as.int64("9007199254740992"),
                    incomparables = big_int64, nomatch = 0L), 1L),
    identical(match(as.int64("2"), c(4611686018427387904, 2),
                    nomatch = 0L), 2L),
    identical(duplicated(as.int64(c("1", "1", NA, NA))),
              c(FALSE, TRUE, FALSE, TRUE)),
    identical(unique(as.int64(c("1", "1", "2"))), as.int64(c("1", "2")))
)

## seq and colon return int64 results when endpoints require it.
stopifnot(
    identical(seq(as.int64("3")), 1:3),
    identical(seq(as.int64("9007199254740993"), length.out = 2L),
              as.int64(c("9007199254740993", "9007199254740994"))),
    identical(seq(as.int64("9007199254740993"),
                  as.int64("9007199254740994"),
                  by = as.int64("1")),
              as.int64(c("9007199254740993", "9007199254740994"))),
    identical(seq.int(as.int64("3")), 1:3),
    identical(9223372036854775806L:9223372036854775807L,
              as.int64(c("9223372036854775806", "9223372036854775807"))),
    identical(seq.int(9223372036854775806L, 9223372036854775807L),
              as.int64(c("9223372036854775806", "9223372036854775807"))),
    identical(seq.int(9223372036854775806L, 9223372036854775807L,
                      by = 1L),
              as.int64(c("9223372036854775806", "9223372036854775807"))),
    identical(seq.int(9223372036854775806L, 9223372036854775807L,
                      by = 1),
              as.int64(c("9223372036854775806", "9223372036854775807"))),
    identical(9007199254740992:9007199254740993L,
              as.int64(c("9007199254740992", "9007199254740993"))),
    identical(9007199254740993L:9007199254740992,
              as.int64(c("9007199254740993", "9007199254740992"))),
    identical(seq.int(as.int64("9007199254740993"), length.out = 2L),
              as.int64(c("9007199254740993", "9007199254740994"))),
    identical(seq(TRUE, as.int64("9007199254740993"), length.out = 2L),
              as.int64(c("1", "9007199254740993"))),
    is_try_error(as.int64(character(0)) : 1L),
    is_try_error(as.int64("1") : integer())
)

## sort and order support shell sorting and reject radix sorting explicitly.
radix_unsupported <- function(expr)
{
    err <- try(expr, silent = TRUE)
    inherits(err, "try-error") &&
        grepl("method = \"radix\" is not supported for int64",
              conditionMessage(attr(err, "condition")), fixed = TRUE)
}
stopifnot(
    identical(is.unsorted(as.int64(c("1", "2"))), FALSE),
    identical(is.unsorted(as.int64(c("2", "1"))), TRUE),
    identical(sort(as.int64(c("2", "1", NA))), as.int64(c("1", "2"))),
    identical(sort(as.int64(c("2", "1", NA)), na.last = TRUE),
              as.int64(c("1", "2", NA))),
    identical(order(as.int64(c("2", "1", NA)), na.last = TRUE),
              c(2L, 1L, 3L)),
    identical(sort.list(as.int64(c("2", "1", NA)), na.last = TRUE),
              c(2L, 1L, 3L)),
    radix_unsupported(sort(as.int64(c("2", "1")), method = "radix")),
    radix_unsupported(order(as.int64(c("2", "1")), method = "radix")),
    radix_unsupported(sort.list(as.int64(c("2", "1")), method = "radix"))
)

## Summary functions compute exact int64 reductions where possible.
stopifnot(
    identical(sum(as.int64(c("1", "2"))), 3),
    identical(sum(as.int64(c("9007199254740993", "-9007199254740992"))),
              1),
    identical(sum(as.int64("9007199254740993"),
                  as.int64("-9007199254740992")), 1),
    identical(min(as.int64(c("1", "2"))), as.int64("1")),
    identical(max(as.int64(c("1", "2"))), as.int64("2")),
    identical(prod(as.int64(c("2", "3"))), 6),
    identical(mean(as.int64(c("1", "3"))), 2),
    identical(mean(as.int64(c("9007199254740993", "-9007199254740991"))),
              1),
    identical(min(as.int64(c("9223372036854775807",
                             "9223372036854775806"))),
              as.int64("9223372036854775806")),
    identical(max(as.int64(c("9223372036854775807",
                             "9223372036854775806"))),
              as.int64("9223372036854775807")),
    identical(cumsum(as.int64(c("1", "2"))), as.int64(c("1", "3"))),
    identical(cummin(as.int64(c("9007199254740993", "9007199254740992"))),
              as.int64(c("9007199254740993", "9007199254740992"))),
    identical(cummax(as.int64(c("9007199254740992", "9007199254740993"))),
              as.int64(c("9007199254740992", "9007199254740993"))),
    identical(min(as.int64(NA), 1.5), NA_real_),
    identical(max(as.int64(NA), 1.5), NA_real_),
    identical(min(5L, as.int64(integer())), as.int64("5")),
    identical(max(5L, as.int64(integer())), as.int64("5")),
    identical(colSums(matrix(as.int64(c("1", "2")), 1L)), c(1, 2)),
    identical(rowSums(matrix(as.int64(c("1", "2")), 1L)), 3),
    identical(colSums(matrix(as.int64(c("9007199254740993",
                                        "-9007199254740992")), 2L)), 1),
    identical(rowSums(matrix(as.int64(c("9007199254740993",
                                        "-9007199254740992")), 1L)), 1),
    identical(colMeans(matrix(as.int64(c("9007199254740993",
                                         "-9007199254740991")), 2L)), 1),
    identical(rowMeans(matrix(as.int64(c("9007199254740993",
                                         "-9007199254740991")), 1L)), 1)
)

## Missingness, extrema, and elementwise math match integer-vector behavior.
stopifnot(
    identical(is.finite(as.int64("1")), TRUE),
    identical(is.finite(as.int64(NA)), FALSE),
    identical(is.infinite(as.int64("1")), FALSE),
    identical(is.infinite(as.int64(NA)), FALSE),
    identical(is.na(list(as.int64(NA))), TRUE),
    identical(is.nan(c(as.int64("1"), as.int64(NA))), c(FALSE, FALSE)),
    identical(which.min(as.int64(c("2", "1"))), 2L),
    identical(which.max(as.int64(c("2", "1"))), 1L),
    identical(pmin(as.int64("2"), as.int64("1")), as.int64("1")),
    identical(pmax(as.int64("2"), as.int64("1")), as.int64("2")),
    identical(abs(as.int64(c("-2", "0", NA))), as.int64(c("2", "0", NA)))
)

## Matrix and array constructors preserve int64 type and shape.
array_print_matches_int <- local({
    old <- options(max.print = 12L)
    on.exit(options(old))
    i64 <- capture.output(print(array(as.int64(as.character(1:20)),
                                      c(2L, 5L, 2L))))
    int <- capture.output(print(array(1:20, c(2L, 5L, 2L))))
    identical(i64, int)
})
split_array <- asplit(array(as.int64(as.character(1:4)), c(2L, 2L)), 1L)
stopifnot(
    identical(matrix(as.int64("1"), 1L),
              structure(as.int64("1"), dim = c(1L, 1L))),
    identical(diag(as.int64(c("1", "9007199254740993"))),
              matrix(as.int64(c("1", "0", "0", "9007199254740993")),
                     2L)),
    identical(t(matrix(as.int64(c("1", "2", "3", "4")), 2L)),
              matrix(as.int64(c("1", "3", "2", "4")), 2L)),
    identical(array(as.int64("1"), 3L),
              structure(rep(as.int64("1"), 3L), dim = 3L)),
    identical(array(as.int64(NA), 3L),
              structure(rep(as.int64(NA), 3L), dim = 3L)),
    identical(split_array,
              structure(list(structure(as.int64(c("1", "3")), dim = 2L),
                             structure(as.int64(c("2", "4")), dim = 2L)),
                        dim = 2L)),
    identical(aperm(array(as.int64(as.character(1:8)), c(2L, 2L, 2L)),
                    c(2L, 1L, 3L)),
              array(as.int64(c("1", "3", "2", "4", "5", "7", "6", "8")),
                    c(2L, 2L, 2L))),
    array_print_matches_int
)

## rowsum, complete.cases, and vapply work with int64 data and groups.
stopifnot(
    identical(complete.cases(data.frame(x = as.int64(c("1", NA)))),
              c(TRUE, FALSE)),
    identical(rowsum(as.int64(c("1", "2", NA)), c(1L, 1L, 2L)),
              matrix(as.int64(c("3", NA)),
                     dimnames = list(c("1", "2"), NULL))),
    identical(rowsum(as.int64(c("1", "2", NA)), c(1L, 1L, 2L),
                     na.rm = TRUE),
              matrix(as.int64(c("3", "0")),
                     dimnames = list(c("1", "2"), NULL))),
    identical(rowsum(data.frame(x = as.int64(c("1", "2", NA))),
                     c(1L, 1L, 2L)),
              data.frame(x = as.int64(c("3", NA)),
                         row.names = c("1", "2"))),
    identical(rowsum(1:3, as.int64(c("1", "1", "2"))),
              matrix(c(3L, 3L), dimnames = list(c("1", "2"), NULL))),
    identical(rowsum(data.frame(x = 1:3), as.int64(c("1", "1", "2"))),
              data.frame(x = c(3L, 3L), row.names = c("1", "2"))),
    identical(vapply(1:2, function(i) as.int64(i), as.int64(0)),
              as.int64(c("1", "2"))),
    identical(vapply(1:2, function(i) as.int64(c(i, i + 2L)),
                     as.int64(c(0, 0))),
              matrix(as.int64(c("1", "3", "2", "4")), 2L)),
    identical(vapply(1:2, identity, as.int64(0)), as.int64(c("1", "2"))),
    identical(vapply(c(TRUE, FALSE), identity, as.int64(0)),
              as.int64(c("1", "0"))),
    identical(vapply(1:2, function(i) as.int64(i), 0), c(1, 2)),
    identical(vapply(1:2, function(i) as.int64(i), 0i), c(1+0i, 2+0i))
)
