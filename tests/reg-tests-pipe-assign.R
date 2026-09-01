#### Regression tests for pipe assignment operator <|>

## Basic usage with simple variables
x <- 5
x <|> sqrt()
stopifnot(identical(x, sqrt(5)))

x <- 16
x <|> sqrt()
stopifnot(identical(x, 4))

## Subscripting with [
x <- c(1, 4, 9, 16)
x[2] <|> sqrt()
stopifnot(identical(x, c(1, 2, 9, 16)))

x <- c(1, 4, 9)
x[c(1,3)] <|> sqrt()
stopifnot(identical(x, c(1, 4, 3)))

## Subscripting with [[
x <- list(1, 4, 9)
x[[2]] <|> sqrt()
stopifnot(identical(x, list(1, 2, 9)))

## Field extraction with $
x <- list(a = 4, b = 9)
x$a <|> sqrt()
stopifnot(identical(x$a, 2))
stopifnot(identical(x$b, 9))

## Names replacement function
x <- c(a=1, b=2, c=3)
names(x) <|> toupper()
stopifnot(identical(names(x), c("A", "B", "C")))

## Attributes
x <- 1:5
attr(x, "test") <- "hello"
attr(x, "test") <|> toupper()
stopifnot(identical(attr(x, "test"), "HELLO"))

## Multiple arguments (data-first function)
x <- c(3, 1, 2)
x <|> sort(decreasing = TRUE)
stopifnot(identical(x, c(3, 2, 1)))

x <- c(3, 1, 2)
x <|> sort(decreasing = TRUE)
stopifnot(identical(x, c(3, 2, 1)))

## Placeholder in named argument
x <- "hello"
x <|> sub("h", "H", x = _)
stopifnot(identical(x, "Hello"))

x <- "hello"
x <|> gsub("l", "L", x = _)
stopifnot(identical(x, "heLLo"))

## Error cases
## RHS not a function call
tryCatch(
    eval(parse(text = "x <|> 5")),
    error = function(e) {
        msg <- conditionMessage(e)
        if (!grepl("requires a function call", msg))
            stop("Expected error about RHS not being function call")
    }
)

## Multiple placeholders (should error)
tryCatch(
    eval(parse(text = "x <|> f(a = _, b = _)")),
    error = function(e) {
        msg <- conditionMessage(e)
        if (!grepl("only appear once", msg))
            stop("Expected error about multiple placeholders")
    }
)

## Placeholder not named (should error)
tryCatch(
    eval(parse(text = "x <|> f(_)")),
    error = function(e) {
        msg <- conditionMessage(e)
        if (!grepl("named argument", msg))
            stop("Expected error about placeholder not being named")
    }
)

## Chaining should error (non-associative)
tryCatch(
    eval(parse(text = "x <|> f() <|> g()")),
    error = function(e) {
        msg <- conditionMessage(e)
        # Parser error expected
        TRUE
    }
)

cat("All pipe assignment tests passed!\n")
