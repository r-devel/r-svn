
## test `for` loops and `as.iterable` methods

## These tests pass with both compiler::enableJIT(0) and compiler::enableJIT(3)

sequence_generator_factory <- function(start, end, step = 1L) {
  if(missing(end)) {
    end <- start
    start <- 1L
  }
  stopifnot(length(start) == 1L,
            length(end) == 1L,
            length(stop) == 1L)

  function(){
    if(start > end) return(NULL)
    on.exit(start <<- start + step)
    start
  }
}


x <- structure(list(), class = 'foo')
as.iterable.foo <- function(x) 1:3

## pass an object with an `as.iterable` method to `for`
counter <- 0L
for(i in x){
  counter <- counter + 1L
  print(i)
}
stopifnot(i == 3L, counter == 3L)


## pass a function to `for`
for(i in sequence_generator_factory(4))
  cat("i = ", i, "\n")

stopifnot(i == 4L)

## pass to `for` an object with an `as.iterable` method that returns a function
as.iterable.foo <- function(x) {
  sequence_generator_factory(11, 13)
}

for(i in x)
  cat("i = ", i, "\n")

stopifnot(i == 13)
