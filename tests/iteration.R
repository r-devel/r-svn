
## test `for` loops and `as.iterable` methods

## These tests pass with both compiler::enableJIT(0) and compiler::enableJIT(3)

sequence_generator_factory <- function(start, end, step = 1L) {
  if (missing(end)) {
    end <- start
    start <- 1L
  }
  stopifnot(length(start) == 1L,
            length(end) == 1L,
            length(step) == 1L)

  function() {
    if (start > end)
      return(NULL) # iterator exhausted

    on.exit(start <<- start + step)
    start
  }
}


x <- structure(list(), class = 'foo')
as.iterable.foo <- function(x) c("a", "b", "c")

## pass to `for` an object with an `as.iterable()` method
observed <- textConnection(NULL, "w", local = TRUE)
for (elem in x) {
  writeLines(elem, observed)
}
stopifnot(
  identical(elem, "c"),
  identical(textConnectionValue(observed), c("a", "b", "c"))
)

observed <- textConnection(NULL, "w", local = TRUE)

## pass to `for` a closure
for (elem in sequence_generator_factory(4)) {
  writeLines(paste("elem =", elem), observed)
}


stopifnot(
  identical(elem, 4L),
  identical(
    textConnectionValue(observed),
    c("elem = 1", "elem = 2", "elem = 3", "elem = 4")
))

observed <- textConnection(NULL, "w", local = TRUE)

## pass to `for` an object with an `as.iterable()` method that returns a closure
as.iterable.foo <- function(x) {
  sequence_generator_factory(11, 13)
}

for(elem in x) {
  writeLines(paste("elem =", elem), observed)
}

stopifnot(
  identical(elem, 13L),
  identical(
    textConnectionValue(observed),
    c("elem = 11", "elem = 12", "elem = 13")
))
