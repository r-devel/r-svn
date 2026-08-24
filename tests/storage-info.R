## Tests for .storage_info().

info <- .storage_info(as.uint64(1))
stopifnot(identical(info,
    list(typeof = "alt", storage_mode = "uint64", element_size = 8L,
	 signed = FALSE, nullable = TRUE)))

info <- .storage_info(as.int64(1, na = FALSE))
stopifnot(identical(info,
    list(typeof = "alt", storage_mode = "int64", element_size = 8L,
	 signed = TRUE, nullable = FALSE)))

stopifnot(
    identical(.storage_info(1L),
	list(typeof = "integer", storage_mode = "integer", element_size = 4L,
	     signed = TRUE, nullable = TRUE)),
    identical(.storage_info(raw())$signed, FALSE),
    identical(.storage_info(raw())$nullable, FALSE),
    identical(.storage_info(double())$signed, NA),
    identical(.storage_info(character())$nullable, TRUE),
    identical(.storage_info(list(NULL))$nullable, FALSE),
    identical(.storage_info(list(NA))$nullable, FALSE), anyNA(list(NA)),
    !anyNA(list(NULL)), is.null(list(NULL)[[1L]]),
    identical(.storage_info(structure(1L, class = "some_class")),
	      .storage_info(1L)),
    inherits(tryCatch(.storage_info(environment()), error = identity),
	     "error"))
