###---- ALL tests here should return  TRUE !
###
###---- "Real" Arithmetic; Numerics etc  -->  ./arith-true.R

### mode checking, etc.
is.recursive(expression(1+3, 2/sqrt(pi)))# fix PR#9

## sum():
all(1:12 == cumsum(rep(1,12)))
x <- rnorm(127); sx <- sum(x);	abs((sum(rev(x)) -sx)) < 1e-12 * abs(sx)

## seq():
typeof(1:4) == "integer" #-- fails for 0.2, 0.3,.., 0.9

## Check parsing with L suffix for integer literals.
typeof(1L) == "integer"
typeof(1000L) == "integer"
typeof(1e3L) == "integer"
typeof(1e-3L) == "double" # gives warning
1.L # gives warning
inherits(try(parse(text = "12iL"), silent=TRUE), "try-error") # gives syntax error

## int64 promotion keeps explicit int32 APIs strict.
int64_warning <- NULL
warned_int64 <- withCallingHandlers(eval(parse(text = "9223372036854775808L")),
                                    warning = function(w) {
                                        int64_warning <<- conditionMessage(w)
                                        invokeRestart("muffleWarning")
                                    })
as_integer_warning <- NULL
strict_integer <- withCallingHandlers(as.integer("2147483648"),
                                      warning = function(w) {
                                          as_integer_warning <<- conditionMessage(w)
                                          invokeRestart("muffleWarning")
                                      })
stopifnot(
    typeof(2147483647L) == "integer",
    typeof(2147483648L) == "int64",
    typeof(9223372036854775807) == "double",
    identical(2147483648L, as.int64("2147483648")),
    identical(strict_integer, NA_integer_),
    grepl("integer range", as_integer_warning, fixed = TRUE),
    typeof(1L + 2L) == "integer",
    typeof(2147483647L + 1L) == "int64",
    identical(2147483647L + 1L, as.int64("2147483648")),
    typeof(c(TRUE, as.int64("2"))) == "int64",
    typeof(c(1L, as.int64("2"))) == "int64",
    typeof(c(as.int64("2"), 2)) == "double",
    identical((1:4)[as.int64("3")], 3L),
    identical((1:3)[as.int64("-9007199254740993")], 1:3),
    identical((1:4)[[as.int64("3")]], 3L),
    identical(length(vector("list", as.int64("3"))), 3L),
    identical(rep(1L, times = as.int64("3")), rep(1L, 3L)),
    typeof(warned_int64) == "int64",
    is.na(warned_int64),
    grepl("int64 range", int64_warning, fixed = TRUE),
    identical(as.int64("1.0"), as.int64("1")),
    identical(as.int64("1e3"), as.int64("1000")),
    identical(as.int64("0x10"), as.int64("16"))
)

i64_named <- as.int64("1")
names(i64_named) <- "a"
i64_cbind <- cbind(a = as.int64("1"))
i64_rbind <- rbind(a = as.int64("1"))
i64_real_cbind <- cbind(as.int64("2147483648"), 1.5)
i64_real_rbind <- rbind(as.int64("2147483648"), 1.5)
i64_list_cbind <- cbind(list(1), as.int64(c("2", "3")))
i64_dim_matrix <- matrix(1:4, 2L)
i64_subscript_matrix <- matrix(as.int64(c("2", "2")), ncol = 2L)
i64_subscript_assignment <- i64_dim_matrix
i64_subscript_assignment[i64_subscript_matrix] <- 9L
i64_big <- as.int64("9007199254740993")
i64_class_numeric <- i64_big
class(i64_class_numeric) <- "numeric"
i64_class_from_double <- 1
class(i64_class_from_double) <- "int64"
i64_big_rounded <- 9007199254740992
i64_real_collision_table <- c(4611686018427387904, 2)
min_int64_warning <- NULL
min_int64_from_real <- withCallingHandlers(as.int64(-9223372036854775808),
                                           warning = function(w) {
                                               min_int64_warning <<- conditionMessage(w)
                                               invokeRestart("muffleWarning")
                                           })
fractional_int64_warning <- NULL
fractional_int64_fallback <- withCallingHandlers(eval(parse(text = "9007199254740993.1L")),
                                                 warning = function(w) {
                                                     fractional_int64_warning <<- conditionMessage(w)
                                                     invokeRestart("muffleWarning")
                                                 })
i64_for_values <- local({
    out <- vector("int64", 0)
    for (i in as.int64(c("1", "2"))) out <- c(out, i)
    out
})
i64_for_cmp <- compiler::cmpfun(function(x) {
    out <- vector("int64", length(x))
    j <- 0L
    for (i in x) {
        j <- j + 1L
        out[j] <- i
    }
    out
})
i64_for_bc_values <- i64_for_cmp(as.int64(c("1", "2")))
i64_object_size <- utils::object.size(as.int64("1"))
i64_group <- gl(2, 1)
i64_split <- split(as.int64(c("1", "2")), i64_group)
i64_tapply <- tapply(as.int64(c("1", "2")), i64_group, identity,
                     simplify = FALSE)
i64_ave <- ave(as.int64(c("1", "2")), i64_group, FUN = identity)
i64_aggregate <- aggregate(as.int64(c("1", "2")), list(g = i64_group),
                           identity)
i64_s3 <- local({
    f <- function(x) UseMethod("f")
    f.numeric <- function(x) "numeric"
    f(as.int64("1"))
})
i64_model_data <- data.frame(x = as.int64(1:3), y = 1:3)
i64_model_frame <- stats::model.frame(y ~ x, data = i64_model_data)
i64_lm <- stats::lm(y ~ x, data = i64_model_data)
i64_formatC <- formatC(as.int64("1"))
i64_rds <- tempfile()
saveRDS(as.int64("1"), i64_rds)
i64_rds_info <- infoRDS(i64_rds)
unlink(i64_rds)
i64_refhook_ptr <- methods::getClass("ANY")@versionKey
attr(i64_refhook_ptr, "x") <- as.int64("1")
i64_refhook_v3_raw <- try(serialize(i64_refhook_ptr, NULL, version = 3,
                                    refhook = function(x) "id"),
                          silent = TRUE)
i64_refhook_rds <- tempfile()
saveRDS(i64_refhook_ptr, i64_refhook_rds, refhook = function(x) "id")
i64_refhook_rds_info <- infoRDS(i64_refhook_rds)
unlink(i64_refhook_rds)
i64_scalar_binding_raw <- compiler::cmpfun(function() {
    for (i in 1L) return(is.raw(serialize(environment(), NULL)))
})()
i64_binding_rds_info <- compiler::cmpfun(function() {
    x <- as.int64("1")
    for (i in 1L) {
        path <- tempfile()
        on.exit(unlink(path), add = TRUE)
        saveRDS(environment(), path)
        return(infoRDS(path))
    }
})()
i64_save <- tempfile()
i64_saved_value <- as.int64("1")
save(i64_saved_value, file = i64_save, compress = FALSE)
i64_save_con <- file(i64_save, "rb")
i64_save_magic <- readChar(i64_save_con, 5L, useBytes = TRUE)
close(i64_save_con)
i64_save_env <- new.env(parent = emptyenv())
load(i64_save, envir = i64_save_env)
unlink(i64_save)
i64_mul_warning <- NULL
i64_mul_overflow <- withCallingHandlers(as.int64("4611686018427387904") *
                                            as.int64("2"),
                                        warning = function(w) {
                                            i64_mul_warning <<- conditionMessage(w)
                                            invokeRestart("muffleWarning")
                                        })
i64_gc_ok <- {
    x <- vector("int64", 1e6)
    rm(x)
    invisible(gc())
    TRUE
}
stopifnot(
    identical(as.int64("9223372036854775807") > as.int64("1"), TRUE),
    identical(as.int64("-1") < as.int64("1"), TRUE),
    identical(as.int64("9223372036854775807") < as.int64("-1"), FALSE),
    identical(as.int64("256") == as.raw(0), FALSE),
    identical(as.raw(255) < as.int64("256"), TRUE),
    identical(as.int64("2") > 1L, TRUE),
    identical(i64_big == i64_big_rounded, FALSE),
    identical(i64_big > i64_big_rounded, TRUE),
    identical(i64_big_rounded < i64_big, TRUE),
    is.na(as.int64(NA) == as.int64(NA)),
    identical(!as.int64(c("0", "2", NA)), c(TRUE, FALSE, NA)),
    typeof(i64_cbind) == "int64",
    identical(dim(i64_cbind), c(1L, 1L)),
    identical(unname(i64_cbind[1, 1]), as.int64("1")),
    identical(as.vector(i64_cbind, "int64"), as.int64("1")),
    typeof(i64_rbind) == "int64",
    identical(dim(i64_rbind), c(1L, 1L)),
    identical(unname(i64_rbind[1, 1]), as.int64("1")),
    identical(as.vector(i64_rbind, "int64"), as.int64("1")),
    typeof(i64_real_cbind) == "double",
    identical(unname(i64_real_cbind[1, 1]), 2147483648),
    typeof(i64_real_rbind) == "double",
    identical(unname(i64_real_rbind[1, 1]), 2147483648),
    identical(as.vector(i64_named, "int64"), as.int64("1")),
    identical(as.vector(i64_named, "any"), as.int64("1")),
    identical(format(as.int64(c("1", "2147483648", NA)), trim = TRUE),
              c("1", "2147483648", "NA")),
    identical(format.info(as.int64(c("1", "2147483648", NA))), 10L),
    identical(capture.output(cat(as.int64(c("1", NA)), sep = "\n")),
              c("1", "NA")),
    identical(capture.output(write.table(data.frame(x = as.int64(NA)),
                                         row.names = FALSE, na = "")),
              c("\"x\"", "")),
    is.character(capture.output(data.frame(x = as.int64(c("1", "2"))))),
    identical(unserialize(serialize(as.int64(c("1", NA)), NULL)), as.int64(c("1", NA))),
    is.raw(try(serialize(as.pairlist(rep.int(list(1L), 100000L)), NULL),
               silent = TRUE)),
    identical(as.int64(c("1", "2"))[[2]], as.int64("2")),
    inherits(i64_object_size, "object_size"),
    as.numeric(i64_object_size) > 0,
    identical(i64_split, list("1" = as.int64("1"), "2" = as.int64("2"))),
    identical(unname(c(i64_tapply)), unname(i64_split)),
    identical(i64_ave, as.int64(c("1", "2"))),
    identical(i64_aggregate,
              data.frame(g = i64_group, x = as.int64(c("1", "2")))),
    identical(i64_s3, "numeric"),
    typeof(i64_class_numeric) == "int64",
    identical(i64_class_numeric, i64_big),
    identical(class(i64_class_numeric), "int64"),
    typeof(i64_class_from_double) == "int64",
    identical(i64_class_from_double, as.int64("1")),
    identical(class(i64_class_from_double), "int64"),
    identical(typeof(i64_model_frame$x), "int64"),
    isTRUE(all.equal(unname(stats::coef(i64_lm)), c(0, 1))),
    identical(i64_formatC, "1"),
    identical(i64_rds_info$version, 4L),
    identical(i64_rds_info$min_reader_version,
              i64_rds_info$writer_version),
    is.raw(i64_refhook_v3_raw),
    identical(i64_refhook_rds_info$version, 3L),
    identical(i64_scalar_binding_raw, TRUE),
    identical(i64_binding_rds_info$version, 4L),
    identical(i64_save_magic, "RDX4\n"),
    identical(i64_save_env$i64_saved_value, as.int64("1")),
    inherits(try(serialize(as.int64("1"), NULL, version = 3),
                 silent = TRUE), "try-error"),
    typeof(i64_mul_overflow) == "int64",
    is.na(i64_mul_overflow),
    grepl("int64 overflow", i64_mul_warning, fixed = TRUE),
    identical(i64_gc_ok, TRUE),
    identical(i64_for_values, as.int64(c("1", "2"))),
    identical(i64_for_bc_values, as.int64(c("1", "2"))),
    isTRUE(all.equal(as.int64("1"), as.int64("1"))),
    is.character(all.equal(as.int64("1"), as.int64("2"))),
    is.character(all.equal(as.int64("1"), as.int64(c("1", "2")))),
    is.character(all.equal(i64_big, as.int64("9007199254740992"))),
    identical(match(as.int64("1"), as.int64("1")), 1L),
    identical(match(i64_big, i64_big_rounded, nomatch = 0L), 0L),
    identical(match(i64_big_rounded, i64_big, nomatch = 0L), 0L),
    identical(match(i64_big_rounded, as.int64("9007199254740992"), nomatch = 0L), 1L),
    identical(match(i64_big_rounded, as.int64("9007199254740992"),
                    incomparables = i64_big, nomatch = 0L), 1L),
    identical(match(as.int64("2"), i64_real_collision_table, nomatch = 0L), 2L),
    identical(duplicated(as.int64(c("1", "1", NA, NA))), c(FALSE, TRUE, FALSE, TRUE)),
    identical(unique(as.int64(c("1", "1", "2"))), as.int64(c("1", "2"))),
    identical(seq(as.int64("3")), 1:3),
    identical(seq(as.int64("9007199254740993"), length.out = 2L),
              as.int64(c("9007199254740993", "9007199254740994"))),
    identical(seq(as.int64("9007199254740993"), as.int64("9007199254740994"),
                  by = as.int64("1")),
              as.int64(c("9007199254740993", "9007199254740994"))),
    identical(seq.int(as.int64("3")), 1:3),
    identical(9223372036854775806L:9223372036854775807L,
              as.int64(c("9223372036854775806", "9223372036854775807"))),
    identical(seq.int(9223372036854775806L, 9223372036854775807L),
              as.int64(c("9223372036854775806", "9223372036854775807"))),
    identical(seq.int(9223372036854775806L, 9223372036854775807L, by = 1L),
              as.int64(c("9223372036854775806", "9223372036854775807"))),
    identical(seq.int(9223372036854775806L, 9223372036854775807L, by = 1),
              as.int64(c("9223372036854775806", "9223372036854775807"))),
    identical(9007199254740992:9007199254740993L,
              as.int64(c("9007199254740992", "9007199254740993"))),
    identical(9007199254740993L:9007199254740992,
              as.int64(c("9007199254740993", "9007199254740992"))),
    identical(seq.int(as.int64("9007199254740993"), length.out = 2L),
              as.int64(c("9007199254740993", "9007199254740994"))),
    inherits(try(as.int64(character(0)) : 1L, silent = TRUE), "try-error"),
    inherits(try(as.int64("1") : integer(), silent = TRUE), "try-error"),
    local({
        grDevices::pdf(NULL)
        on.exit(grDevices::dev.off())
        graphics::plot.new()
        graphics::plot.window(xlim = as.int64(c("1", "2")), ylim = 0:1,
                              xaxs = "i", yaxs = "i")
        identical(graphics::par("usr"), c(1, 2, 0, 1))
    }),
    identical(i64_dim_matrix[i64_subscript_matrix], 4L),
    identical(i64_subscript_assignment, matrix(c(1L, 2L, 3L, 9L), 2L)),
    identical(complete.cases(data.frame(x = as.int64(c("1", NA)))), c(TRUE, FALSE)),
    typeof(c(1L, 2L) + 1L) == "integer",
    typeof(c(1L, 2L) - 1L) == "integer",
    typeof(c(1L, 2L) * 2L) == "integer",
    typeof(c(1L, 2L) %% 2L) == "integer",
    typeof(c(1L, 2L) %/% 2L) == "integer",
    typeof(i64_list_cbind) == "list",
    identical(dim(i64_list_cbind), c(2L, 2L)),
    identical(i64_list_cbind[[3]], as.int64("2")),
    identical(i64_list_cbind[[4]], as.int64("3")),
    is.na(min_int64_from_real),
    is.character(min_int64_warning),
    grepl("int64 range", min_int64_warning, fixed = TRUE),
    { z <- as.int64("1"); z[1] <- as.int64("2"); identical(z, as.int64("2")) },
    { z <- list(1L); z[[1]] <- as.int64("9007199254740993"); identical(z[[1]], as.int64("9007199254740993")) },
    { z <- expression(1L); z[[1]] <- as.int64("9007199254740993"); identical(z[[1]], as.int64("9007199254740993")) },
    { z <- matrix(as.int64("1"), 1L); z[1, 1] <- as.int64("2"); identical(z, matrix(as.int64("2"), 1L)) },
    { z <- array(as.int64("1"), c(1L, 1L, 1L)); z[1, 1, 1] <- as.int64("2"); identical(z, array(as.int64("2"), c(1L, 1L, 1L))) },
    identical(sum(as.int64(c("1", "2"))), 3),
    .Machine$sizeof.longdouble <= 8L ||
        identical(sum(as.int64(c("9007199254740993", "-9007199254740992"))), 1),
    .Machine$sizeof.longdouble <= 8L ||
        identical(sum(as.int64("9007199254740993"), as.int64("-9007199254740992")), 1),
    identical(min(as.int64(c("1", "2"))), as.int64("1")),
    identical(max(as.int64(c("1", "2"))), as.int64("2")),
    identical(prod(as.int64(c("2", "3"))), 6),
    identical(mean(as.int64(c("1", "3"))), 2),
    .Machine$sizeof.longdouble <= 8L ||
        identical(mean(as.int64(c("9007199254740993", "-9007199254740991"))), 1),
    identical(min(as.int64(c("9223372036854775807", "9223372036854775806"))),
              as.int64("9223372036854775806")),
    identical(max(as.int64(c("9223372036854775807", "9223372036854775806"))),
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
    .Machine$sizeof.longdouble <= 8L ||
        identical(colSums(matrix(as.int64(c("9007199254740993", "-9007199254740992")), 2L)), 1),
    .Machine$sizeof.longdouble <= 8L ||
        identical(rowSums(matrix(as.int64(c("9007199254740993", "-9007199254740992")), 1L)), 1),
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
    identical(abs(as.int64(c("-2", "0", NA))), as.int64(c("2", "0", NA))),
    identical(is.unsorted(as.int64(c("1", "2"))), FALSE),
    identical(is.unsorted(as.int64(c("2", "1"))), TRUE),
    identical(sort(as.int64(c("2", "1", NA))), as.int64(c("1", "2"))),
    identical(sort(as.int64(c("2", "1", NA)), na.last = TRUE), as.int64(c("1", "2", NA))),
    identical(order(as.int64(c("2", "1", NA)), na.last = TRUE), c(2L, 1L, 3L)),
    identical(sort.list(as.int64(c("2", "1", NA)), na.last = TRUE), c(2L, 1L, 3L)),
    identical(deparse(2147483648L), "2147483648L"),
    identical(eval(parse(text = deparse(2147483648L))), 2147483648L),
    identical(9007199254740993e0L, as.int64("9007199254740993")),
    identical(9223372036854775807e0L, as.int64("9223372036854775807")),
    typeof(fractional_int64_fallback) == "double",
    grepl("contains decimal; using numeric value", fractional_int64_warning, fixed = TRUE),
    identical(eval(parse(text = deparse(as.int64("1")))), as.int64("1")),
    identical(eval(parse(text = deparse(as.int64(c("1", "2147483648", NA))))), as.int64(c("1", "2147483648", NA))),
    identical(eval(parse(text = deparse(i64_named))), i64_named),
    is.integer(memory.profile()["int64"]),
    !is.na(memory.profile()["int64"]),
    identical(matrix(as.int64("1"), 1L), structure(as.int64("1"), dim = c(1L, 1L))),
    identical(t(matrix(as.int64(c("1", "2", "3", "4")), 2L)),
              matrix(as.int64(c("1", "3", "2", "4")), 2L)),
    identical(array(as.int64("1"), 3L), structure(rep(as.int64("1"), 3L), dim = 3L)),
    identical(array(as.int64(NA), 3L), structure(rep(as.int64(NA), 3L), dim = 3L)),
    identical(aperm(array(as.int64(as.character(1:8)), c(2L, 2L, 2L)), c(2L, 1L, 3L)),
              array(as.int64(c("1", "3", "2", "4", "5", "7", "6", "8")), c(2L, 2L, 2L))),
    { z <- as.int64("1"); length(z) <- 3L; identical(z, c(as.int64("1"), as.int64(NA), as.int64(NA))) },
    { z <- as.int64("1"); z[3] <- as.int64("2"); identical(z, as.int64(c("1", NA, "2"))) },
    { z <- 1:3; tsp(z) <- as.int64(c("1", "3", "1")); identical(tsp(z), c(1, 3, 1)) },
    identical(rowsum(as.int64(c("1", "2", NA)), c(1L, 1L, 2L)),
              matrix(as.int64(c("3", NA)), dimnames = list(c("1", "2"), NULL))),
    identical(rowsum(as.int64(c("1", "2", NA)), c(1L, 1L, 2L), na.rm = TRUE),
              matrix(as.int64(c("3", "0")), dimnames = list(c("1", "2"), NULL))),
    identical(rowsum(data.frame(x = as.int64(c("1", "2", NA))), c(1L, 1L, 2L)),
              data.frame(x = as.int64(c("3", NA)), row.names = c("1", "2"))),
    identical(vapply(1:2, function(i) as.int64(i), as.int64(0)), as.int64(c("1", "2"))),
    identical(vapply(1:2, function(i) as.int64(c(i, i + 2L)), as.int64(c(0, 0))),
              matrix(as.int64(c("1", "3", "2", "4")), 2L)),
    identical(vapply(1:2, identity, as.int64(0)), as.int64(c("1", "2"))),
    identical(vapply(c(TRUE, FALSE), identity, as.int64(0)), as.int64(c("1", "0"))),
    identical(vapply(1:2, function(i) as.int64(i), 0), c(1, 2)),
    identical(vapply(1:2, function(i) as.int64(i), 0i), c(1+0i, 2+0i)),
    is.character(capture.output(matrix(as.int64("1"), 1L))),
    inherits(try(vector("list", as.int64("-2147483649")), silent = TRUE), "try-error"),
    inherits(try(i64_dim_matrix[[as.int64("4294967297"), 1L]], silent = TRUE), "try-error"),
    .Machine$sizeof.pointer != 4L || {
        z <- list(1L)
        inherits(try(z[[as.int64("4294967297")]] <- 2L, silent = TRUE), "try-error")
    },
    identical(0x1eeeeeeeeeeeeeeeL, as.int64("2228981575573237486")),
    identical(0x7fffffffffffffffL, as.int64("9223372036854775807"))
)
stopifnot(
    identical(i64_dim_matrix[as.int64("1"), ], c(1L, 3L)),
    identical(i64_dim_matrix[, as.int64("2")], c(3L, 4L))
)

all((0:6) == pi + ((-pi):pi))
all((0:7) == (pi+seq(-pi,pi, length=8))*7/(2*pi))

1 == as.integer(is.na(c(pi,NA)[2]))
1 == as.integer(is.nan(0/0))

## rev():
cc <- c(1:10,10:1) ;		all(cc == rev(cc))

## dim[names]():
all(names(c(a=pi, b=1, d=1:4)) == c("a","b", paste("d", 1:4, sep="")))
##P names(c(a=pi, b=1, d=1:4))
ncb <- dimnames(cbind(a=1, yy=1:3))[[2]]
(!is.null(ncb)) && all(ncb == c("a","yy"))

all(cbind(a=1:2, b=1:3, c=1:6) == t(rbind(a=1:2, b=1:3, c=1:6)))
##P rbind(a=1:2, b=1:3, c=1:6)
all(dim(cbind(cbind(I=1,x=1:4), c(a=pi))) == 4:3)# fails in S+

a <- b <- 1:3
all(dimnames(cbind(a, b))[[2]] == c("a","b"))

## rbind PR#338
all(dim(m <- rbind(1:2, diag(2))) == 3:2)
all(m == c(1,1,0, 2,0,1))

## factor():
is.factor(factor(integer()))
all(levels(ordered(rev(gl(3,4)))) == 1:3)# coercion to char
all(levels(factor(factor(9:1)[3:5])) == 5:7)
## crossing bug PR#40
is.factor(ff <- gl(2,3) : gl(3,2)) && length(ff) == 6
all(levels(ff) == t(outer(1:2, 1:3, paste, sep=":")))
## from PR#5
ll <- c("A","B"); ff <- factor(ll); f0 <- ff[, drop=TRUE]
all(f0 == ff) && all(levels(ff) == ll) && is.factor(ff) && is.factor(f0)

### data.frame s :

## from lists [bug PR#100]
x <- NULL
x$x1 <- 1:10
x$x2 <- 0:9
all(dim(dx <- as.data.frame(x)) == c(10,2))

## Logicals: (S is wrong)
l1 <- c(TRUE,FALSE,TRUE)
(! as.logical(as.data.frame(FALSE)[,1]))
all(l1 == as.logical(as.data.frame(l1)[,1]))

## empty data.frames :
x <- data.frame(a=1:3)
x30 <- x[, -1] # was not even possible in S-PLUS

all(dim(x30) == c(3,0))
x01 <- x[-(1:3), , drop = FALSE]
x00 <- x01[,-1]
all(dim(x01) == 0:1)
all(dim(x00) == 0)
all(dim(x) == dim(rbind(x, x01)))
## bugs up to 1.2.3 :
all(dim(x30) == dim(m30 <- as.matrix(x30)))
all(dim(x01) == dim(m01 <- as.matrix(x01)))
all(dim(x30) == dim(as.data.frame(m30)))
all(dim(x01) == dim(as.data.frame(m01)))
all(dim(x01) == dim(   data.frame(m01)))
all(dim(x30) == dim(   data.frame(m30)))
all(dim(x)   == dim(cbind(x, x30)))
## up to 1.4.0 :
all(dim(x30) == dim( data.matrix(x30)))
all(dim(x00) == dim( data.matrix(x00)))

m0 <- matrix(pi, 0,3)
a302 <- array("", dim=c(3,0,2))
identical(apply(m0, 1, dim), NULL)
identical(apply(m0, 2, dim), NULL)
identical(apply(m0, 1,length),  integer(0))
identical(apply(m0, 2,length),  integer(3))
identical(apply(a302, 1, mode), rep("character",3))
## NO (maybe later?):
## identical(apply(a302, 2, mode), rep("character",0))
is.character(aa <- apply(a302, 2, mode)) && length(aa) == 0
identical(apply(a302, 3, mode), rep("character",2))
identical(apply(a302, 3, length),integer(2))
identical(apply(a302, 3, dim), matrix(as.integer(c(3,0)), 2 ,2))
identical(apply(a302, 1, dim), matrix(as.integer(c(0,2)), 2 ,3))
identical(apply(array(dim=3), 1,length), rep(1:1, 3))
identical(apply(array(dim=0), 1,length), rep(1:1, 0))# = integer(0)


### Subsetting

## bug PR#425
x <- matrix(1:4, 2, 2, dimnames=list(c("abc","ab"), c("cde","cd")))
y <- as.data.frame(x)
all(x["ab",] == c(2,4))
all(y["ab",] == c(2,4))

## from bug PR#447
x <- 1:2 ; x[c("2","2")] <- 4
all.equal(x, c(1:2, "2" = 4))

## stretching
l2 <- list(a=1, b=2)
l2["cc"] <- pi
l2[["d"]] <- 4
l2 $ e <- 55
all.equal(l2, list(a = 1, b = 2, cc = pi, d = 4, e = 55), tolerance = 0)
all.equal(l2["d"], list(d = 4))
l2$d == 4 && l2$d == l2[["d"]]

## bug in R <= 1.1
f1 <- y1 ~ x1
f2 <- y2 ~ x2
f2[2] <- f1[2]
deparse(f2) == "y1 ~ x2"

m <- cbind(a=1:2,b=c(R=10,S=11))
all(sapply(dimnames(m), length) == c(2,2))
## [[ for matrix:
m[[1,2]] == m[[3]] && m[[3]] == m[3] && m[3] == m[1,2]

## bug in R <= 1.1.1 : unclass(*) didn't drop the class!
## to be robust to S4 methods DON'T test for null class
## The test for attr(,"class") is valid, if essentially useless
d1 <- rbind(data.frame(a=1, b = I(TRUE)), new = c(7, "N"))
is.null(attr(unclass(d1$b), "class"))

## bugs in R 1.2.0
format(as.POSIXct(relR120 <- "2000-12-15 11:24:40")) == relR120
format(as.POSIXct(substr(relR120,1,10))) == substr(relR120,1,10)

## rank() with NAs (and ties)
x <- c(3:1,6,4,3,NA,5,0,NA)
rx <-  rank(x)
all(rx == c(4.5, 3:2, 8, 6, 4.5, 9, 7, 1, 10))
rxK <- rank(x, na.last = "keep")
all(rx [rx <= 8]    == na.omit(rxK))
all(rank(x, na.last = NA) == na.omit(rxK))

## as.list.function() instead of *.default():
identical(as.list(as.list),
	  alist(x = , ... = , UseMethod("as.list")))

## startsWith() / endsWith()  assertions
t1 <- c("Foobar", "bla bla", "something", "another", "blu", "brown",
        "blau blüht der Enzian")# non-ASCII
t2 <- c("some text", "any text")
t3 <- c("Martin", "Zürich", "Mächler")

all(endsWith(t1, "")); all(startsWith(t1, ""))
all(endsWith(t2, "")); all(startsWith(t2, ""))
all(endsWith(t3, "")); all(startsWith(t3, ""))
all(endsWith(t2, "text"))
all(endsWith(t2, " text"))
identical(startsWith(t1, "b" ), c(FALSE, TRUE, FALSE, FALSE, TRUE,  TRUE, TRUE))
identical(startsWith(t1, "bl"), c(FALSE, TRUE, FALSE, FALSE, TRUE, FALSE, TRUE))
identical(startsWith(t1, "bla"),c(FALSE, TRUE, FALSE, FALSE,FALSE, FALSE, TRUE))
identical(  endsWith(t1, "n"),  c(FALSE,FALSE, FALSE, FALSE,FALSE,  TRUE, TRUE))
identical(  endsWith(t1, "an"), c(FALSE,FALSE, FALSE, FALSE,FALSE, FALSE, TRUE))
##
identical(startsWith(t3, "M" ), c( TRUE, FALSE, TRUE))
identical(startsWith(t3, "Ma"), c( TRUE, FALSE, FALSE))
identical(startsWith(t3, "Mä"), c(FALSE, FALSE, TRUE))
