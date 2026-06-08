### Tests for int64 support

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
    identical(as.int64("0x1p3"), as.int64("8")),
    identical(as.int64("0x10"), as.int64("16"))
)

i64_named <- as.int64("1")
names(i64_named) <- "a"
i64_named_pair <- setNames(as.int64(c("1", "2")), c("a", "b"))
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
i64_big_exact_complex <- 9007199254740992+0i
i64_hex_big <- as.int64("0x1eeeeeeeeeeeeeee")
i64_inexact_decimal_warning <- NULL
i64_inexact_decimal <- withCallingHandlers(as.int64("1.00000000000000001"),
                                           warning = function(w) {
                                               i64_inexact_decimal_warning <<-
                                                   conditionMessage(w)
                                               invokeRestart("muffleWarning")
                                           })
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
i64_nlm_p <- stats::nlm(function(x) sum(x * x), as.int64("1"))
i64_nlm_typsize <- stats::nlm(function(x) sum(x * x), 1,
                              typsize = as.int64("1"))
i64_dnorm <- stats::dnorm(as.int64("1"))
i64_convolve <- stats::convolve(as.int64(c("1", "2", "3")),
                                as.int64(c("1", "2", "3")))
i64_deriv <- stats::deriv(~ x + 2147483648L, "x")
i64_deriv_value <- eval(i64_deriv, list(x = 1))
i64_s3 <- local({
    f <- function(x) UseMethod("f")
    f.numeric <- function(x) "numeric"
    f(as.int64("1"))
})
i64_s4 <- local({
    library(methods)
    f <- setGeneric("i64_s4_f", function(x) standardGeneric("i64_s4_f"))
    on.exit(removeGeneric("i64_s4_f"), add = TRUE)
    setMethod(f, "numeric", function(x) "numeric")
    A <- setClass("i64_s4_A", slots = c(x = "int64"))
    on.exit(removeClass("i64_s4_A"), add = TRUE)
    x <- as.int64("1")
    list(is_numeric = is(x, "numeric"),
         dispatch = i64_s4_f(x),
         valid_slot = validObject(A(x = x)))
})
i64_model_data <- data.frame(x = as.int64(1:3), y = 1:3)
i64_model_frame <- stats::model.frame(y ~ x, data = i64_model_data)
i64_lm <- stats::lm(y ~ x, data = i64_model_data)
i64_formatC <- formatC(as.int64("1"))
i64_big_formatC <- formatC(as.int64("9007199254740993"))
i64_prompt <- local({
    old <- getOption("prompt")
    on.exit(options(prompt = old))
    options(prompt = as.int64("9007199254740993"))
    getOption("prompt")
})
i64_rds <- tempfile()
saveRDS(as.int64("1"), i64_rds)
i64_rds_info <- infoRDS(i64_rds)
unlink(i64_rds)
i64_refhook_count <- 0L
i64_refhook_count_raw <- serialize(methods::getClass("ANY")@versionKey, NULL,
                                   refhook = function(x) {
                                       i64_refhook_count <<-
                                           i64_refhook_count + 1L
                                       "id"
                                   })
i64_refhook_ptr <- methods::getClass("ANY")@versionKey
attr(i64_refhook_ptr, "x") <- as.int64("1")
i64_refhook_v3_raw <- try(serialize(i64_refhook_ptr, NULL, version = 3,
                                    refhook = function(x) "id"),
                          silent = TRUE)
i64_refhook_once_env <- new.env(parent = emptyenv())
i64_refhook_once_env$x <- as.int64("1")
i64_refhook_once_count <- 0L
i64_refhook_once_raw <- serialize(i64_refhook_once_env, NULL,
                                  refhook = function(x) {
                                      i64_refhook_once_count <<-
                                          i64_refhook_once_count + 1L
                                      if (i64_refhook_once_count == 1L)
                                          "id"
                                      else
                                          NULL
                                  })
i64_refhook_once_con <- rawConnection(i64_refhook_once_raw)
i64_refhook_once_info <- infoRDS(i64_refhook_once_con)
close(i64_refhook_once_con)
i64_refhook_decline_env <- new.env(parent = emptyenv())
i64_refhook_decline_env$x <- as.int64("1")
i64_refhook_decline_raw <- serialize(i64_refhook_decline_env, NULL,
                                     refhook = function(x) NULL)
i64_refhook_decline_out <- unserialize(i64_refhook_decline_raw)
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
i64_array_print_matches_int <- local({
    old <- options(max.print = 12L)
    on.exit(options(old))
    i64 <- capture.output(print(array(as.int64(as.character(1:20)),
                                      c(2L, 5L, 2L))))
    int <- capture.output(print(array(1:20, c(2L, 5L, 2L))))
    identical(i64, int)
})
i64_asplit <- asplit(array(as.int64(as.character(1:4)), c(2L, 2L)), 1L)
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
    identical(i64_big == i64_big_exact_complex, FALSE),
    identical(i64_big != i64_big_exact_complex, TRUE),
    identical(as.int64("9007199254740992") == i64_big_exact_complex, TRUE),
    identical(as.int64("2") == 2+1i, FALSE),
    identical(as.int64("2") != 2+1i, TRUE),
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
    identical(names(c(i64_named_pair)), c("a", "b")),
    identical(names(unlist(list(x = i64_named_pair))), c("x.a", "x.b")),
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
    identical(i64_s4, list(is_numeric = TRUE, dispatch = "numeric",
                           valid_slot = TRUE)),
    typeof(i64_class_numeric) == "int64",
    identical(i64_class_numeric, i64_big),
    identical(class(i64_class_numeric), "int64"),
    typeof(i64_class_from_double) == "int64",
    identical(i64_class_from_double, as.int64("1")),
    identical(class(i64_class_from_double), "int64"),
    identical(typeof(i64_model_frame$x), "int64"),
    isTRUE(all.equal(unname(stats::coef(i64_lm)), c(0, 1))),
    identical(i64_formatC, "1"),
    identical(i64_big_formatC, "9007199254740993"),
    identical(i64_prompt, "9007199254740993"),
    identical(i64_rds_info$version, 4L),
    identical(i64_rds_info$min_reader_version,
              i64_rds_info$writer_version),
    is.raw(i64_refhook_count_raw),
    identical(i64_refhook_count, 1L),
    inherits(i64_refhook_v3_raw, "try-error"),
    is.raw(i64_refhook_once_raw),
    identical(i64_refhook_once_count, 1L),
    identical(i64_refhook_once_info$version, 4L),
    is.raw(i64_refhook_decline_raw),
    identical(i64_refhook_decline_out$x, as.int64("1")),
    identical(i64_refhook_rds_info$version, 4L),
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
    identical(match(i64_big, i64_big_exact_complex, nomatch = 0L), 0L),
    identical(match(as.int64("9007199254740992"), i64_big_exact_complex, nomatch = 0L), 1L),
    identical(match(i64_big_exact_complex, as.int64("9007199254740992"), nomatch = 0L), 1L),
    identical(match(as.int64(NA), NA_complex_, nomatch = 0L), 1L),
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
    identical(seq(TRUE, as.int64("9007199254740993"), length.out = 2L),
              as.int64(c("1", "9007199254740993"))),
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
    identical(stats::D(2147483648L, "x"), 0),
    is.expression(i64_deriv),
    identical(c(attr(i64_deriv_value, "gradient")), 1),
    abs(i64_nlm_p$estimate) < 1e-4,
    abs(i64_nlm_typsize$estimate) < 1e-4,
    identical(i64_dnorm, stats::dnorm(1)),
    isTRUE(all.equal(i64_convolve, stats::convolve(1:3, 1:3))),
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
    { z <- pairlist(pairlist(1L)); z[[as.int64(c("1", "1"))]] <- 2L; identical(z, pairlist(pairlist(2L))) },
    { z <- pairlist(pairlist(1L, 2L)); z[[as.int64(c("1", "2"))]] <- NULL; identical(z, pairlist(pairlist(1L))) },
    { z <- expression(1L); z[[1]] <- as.int64("9007199254740993"); identical(z[[1]], as.int64("9007199254740993")) },
    { z <- matrix(as.int64("1"), 1L); z[1, 1] <- as.int64("2"); identical(z, matrix(as.int64("2"), 1L)) },
    { z <- array(as.int64("1"), c(1L, 1L, 1L)); z[1, 1, 1] <- as.int64("2"); identical(z, array(as.int64("2"), c(1L, 1L, 1L))) },
    identical(sum(as.int64(c("1", "2"))), 3),
    identical(sum(as.int64(c("9007199254740993", "-9007199254740992"))), 1),
    identical(sum(as.int64("9007199254740993"), as.int64("-9007199254740992")), 1),
    identical(min(as.int64(c("1", "2"))), as.int64("1")),
    identical(max(as.int64(c("1", "2"))), as.int64("2")),
    identical(prod(as.int64(c("2", "3"))), 6),
    identical(mean(as.int64(c("1", "3"))), 2),
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
    identical(colSums(matrix(as.int64(c("9007199254740993", "-9007199254740992")), 2L)), 1),
    identical(rowSums(matrix(as.int64(c("9007199254740993", "-9007199254740992")), 1L)), 1),
    identical(colMeans(matrix(as.int64(c("9007199254740993", "-9007199254740991")), 2L)), 1),
    identical(rowMeans(matrix(as.int64(c("9007199254740993", "-9007199254740991")), 1L)), 1),
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
    identical(diag(as.int64(c("1", "9007199254740993"))),
              matrix(as.int64(c("1", "0", "0", "9007199254740993")), 2L)),
    identical(t(matrix(as.int64(c("1", "2", "3", "4")), 2L)),
              matrix(as.int64(c("1", "3", "2", "4")), 2L)),
    identical(array(as.int64("1"), 3L), structure(rep(as.int64("1"), 3L), dim = 3L)),
    identical(array(as.int64(NA), 3L), structure(rep(as.int64(NA), 3L), dim = 3L)),
    identical(i64_asplit,
              structure(list(structure(as.int64(c("1", "3")), dim = 2L),
                             structure(as.int64(c("2", "4")), dim = 2L)),
                        dim = 2L)),
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
    identical(rowsum(1:3, as.int64(c("1", "1", "2"))),
              matrix(c(3L, 3L), dimnames = list(c("1", "2"), NULL))),
    identical(rowsum(data.frame(x = 1:3), as.int64(c("1", "1", "2"))),
              data.frame(x = c(3L, 3L), row.names = c("1", "2"))),
    identical(vapply(1:2, function(i) as.int64(i), as.int64(0)), as.int64(c("1", "2"))),
    identical(vapply(1:2, function(i) as.int64(c(i, i + 2L)), as.int64(c(0, 0))),
              matrix(as.int64(c("1", "3", "2", "4")), 2L)),
    identical(vapply(1:2, identity, as.int64(0)), as.int64(c("1", "2"))),
    identical(vapply(c(TRUE, FALSE), identity, as.int64(0)), as.int64(c("1", "0"))),
    identical(vapply(1:2, function(i) as.int64(i), 0), c(1, 2)),
    identical(vapply(1:2, function(i) as.int64(i), 0i), c(1+0i, 2+0i)),
    is.character(capture.output(matrix(as.int64("1"), 1L))),
    identical(capture.output(as.int64(character(0))), "as.int64(character(0))"),
    identical(eval(parse(text = capture.output(as.int64(character(0))))), as.int64(character(0))),
    grepl("^ int64 ", capture.output(str(as.int64(1)))[1L]),
    inherits(try(vector("list", as.int64("-2147483649")), silent = TRUE), "try-error"),
    inherits(try(i64_dim_matrix[[as.int64("4294967297"), 1L]], silent = TRUE), "try-error"),
    .Machine$sizeof.pointer != 4L || {
        z <- list(1L)
        inherits(try(z[[as.int64("4294967297")]] <- 2L, silent = TRUE), "try-error")
    },
    is.na(i64_inexact_decimal),
    grepl("int64 range", i64_inexact_decimal_warning, fixed = TRUE),
    identical(i64_hex_big, as.int64("2228981575573237486")),
    identical(0x1eeeeeeeeeeeeeeeL, as.int64("2228981575573237486")),
    identical(0x7fffffffffffffffL, as.int64("9223372036854775807")),
    i64_array_print_matches_int
)
stopifnot(
    identical(i64_dim_matrix[as.int64("1"), ], c(1L, 3L)),
    identical(i64_dim_matrix[, as.int64("2")], c(3L, 4L))
)
