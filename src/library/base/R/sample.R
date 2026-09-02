#  File src/library/base/R/sample.R
#  Part of the R package, https://www.R-project.org
#
#  Copyright (C) 1995-2022 The R Core Team
#
#  This program is free software; you can redistribute it and/or modify
#  it under the terms of the GNU General Public License as published by
#  the Free Software Foundation; either version 2 of the License, or
#  (at your option) any later version.
#
#  This program is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.
#
#  A copy of the GNU General Public License is available at
#  https://www.R-project.org/Licenses/

sample <- function(x, size, replace = FALSE, prob = NULL,
                   method = c("sequential", "marginal", "poisson"))
{
    if(length(x) == 1L && is.numeric(x) && is.finite(x) && x >= 1) {
	if(missing(size)) size <- x
	sample.int(x, size, replace, prob, method = method)
    } else {
	if(missing(size)) size <- length(x)
	x[sample.int(length(x), size, replace, prob, method = method)]
    }
}

sample.int <- function(n, size = n, replace = FALSE, prob = NULL,
  useHash = (n > 1e7 && !replace && is.null(prob) && size <= n/2),
  method = c("sequential", "marginal", "poisson"))
{
  stopifnot(length(n) == 1L)
  if (replace || is.null(prob)) {
    size <- size %||% n
    if (useHash) {
      ## will work with size > n/2 but may be slow.
      stopifnot(is.null(prob), !replace)
      return(.Internal(sample2(n, size)))
    } 
    return(.Internal(sample(n, size, replace, prob)))
  }
  ## sampling without replacement and with specified probability weights
  size <- size %||% sum(prob)
  if (length(prob) != n) {
    stop("incorrect number of probabilities")
  }
  method <- match.arg(method)
  switch(
    method,
    sequential = .Internal(sample(n, size, replace, prob)),
    marginal = sample.pps(n, size, prob),
    poisson = { ## shuffle if length > 1
      rval <- which(stats::runif(n) <= prob/sum(prob) * size)
      if (length(rval) < 2) rval else sample(rval) 
    }
  )
}


sample.pps <- function(n, size, prob, tolerance = sqrt(.Machine$double.eps)) {

    inclusion_probs <- function(a, size) {
        a <- as.double(a)
        size <- as.integer(round(size))
        b <- a < 0
        if (any(b)) {
            warning("there are ", sum(b), " negative value(s) shifted to zero")
            a[b] <- 0
        }
        .Internal(inclusion_probs(a, size))
    }
    
    up_brewer <- function(pi_k, eps = sqrt(.Machine$double.eps)) {
        if (anyNA(pi_k))
            stop("there are missing values in the pi_k vector")
        pi_k <- as.double(pi_k)
        eps <- as.double(eps)
        .Internal(up_brewer(pi_k, eps))
    }
    
  sum_prob <- sum(prob)
  sums_to_one <- isTRUE(all.equal(sum_prob, 1, tolerance = tolerance))
  sums_to_int <- 
    isTRUE(all.equal(sum_prob, round(sum_prob), tolerance = tolerance))
  if (is.null(size)) {
    if(!sums_to_int)
      stop("sum(prob) must be an integer")
    size <- round(sum_prob)
  } else {
    size_is_sum <- isTRUE(all.equal(size, sum(prob), tolerance = tolerance))
    size_is_int <- isTRUE(all.equal(size, round(size), tolerance = tolerance))
    if (!size_is_int)
        stop("size must be NULL or an integer")
    if (size>n)
        stop("cannot take a sample larger than the population when 'replace = FALSE'")
    if (sums_to_one && !size_is_sum) {
      warning("rescaling prob, which changes inclusion probabilities")
      prob <- inclusion_probs(prob * size, size)
    } else if (sums_to_int && !size_is_sum) {
      warning("sum(prob) is not equal to size or 1, rescaling")
      prob <- inclusion_probs(prob/sum_prob * size, size)
    }
  }
  up_brewer(prob)
}
