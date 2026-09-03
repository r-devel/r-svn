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
    if(!replace && !is.null(prob)) {
	method <- match.arg(method)
	if((missing(size) || is.null(size)) && method != "sequential")
	    stop(gettextf("'size' must be specified for method = \"%s\"",
			  method), domain = NA)
    }

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
    if (useHash) {
      ## will work with size > n/2 but may be slow.
      stopifnot(is.null(prob), !replace)
      return(.Internal(sample2(n, size)))
    } 
    return(.Internal(sample(n, size, replace, prob)))
  }
  ## sampling without replacement and with specified probability weights
  if (length(prob) != n) {
    stop("incorrect number of probabilities")
  }
  method <- match.arg(method)
  if ((missing(size) || is.null(size)) && method != "sequential")
    stop(gettextf("'size' must be specified for method = \"%s\"",
                  method), domain = NA)
  switch(
    method,
    sequential = .Internal(sample(n, size, replace, prob)),
    marginal = sample.pps(n, size, prob),
    poisson = { ## shuffle if length > 1
      pik <- .Internal(inclusion_probs(prob, size))
      rval <- which(stats::runif(n) <= pik)
      if (length(rval) < 2) rval else sample(rval) 
    }
  )
}


sample.pps <- function(n, size, prob) {

  if (missing(size) || is.null(size))
    stop("'size' must be specified")
    
    up_brewer <- function(pi_k, eps = sqrt(.Machine$double.eps)) {
        if (anyNA(pi_k))
            stop("there are missing values in the pi_k vector")
        pi_k <- as.double(pi_k)
        eps <- as.double(eps)
        .Internal(up_brewer(pi_k, eps))
    }

  if (length(prob) != n)
      stop("incorrect number of probabilities")

  pik <- .Internal(inclusion_probs(prob, size))
  up_brewer(pik)
}
