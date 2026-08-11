#  File src/library/base/R/aperm.R
#  Part of the R package, https://www.R-project.org
#
#  Copyright (C) 1995-2012 The R Core Team
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

aperm <- function(a, perm, ...) UseMethod("aperm")

aperm.default <- function (a, perm = NULL, resize = TRUE, ...)
     .Internal(aperm(a, perm, resize))

aperm.table <- function(a, perm = NULL, resize = TRUE, keep.class = TRUE, ...)
{
     r <- aperm.default(a, perm, resize=resize)
     if(!keep.class) class(r) <- NULL
     r
}

aperm.matrix <- function(a, perm = NULL, resize = TRUE, ...) 
{
     if (length(perm) == 0L)
          perm <- c(2L, 1L)
     if (length(perm) != 2L)
         stop(gettextf("'perm' is of wrong length %d (!= 2)", length(perm)))
     if (is.character(perm)) {
         if (is.null(dna <- dimnames(a)) || is.null(dnna <- names(dna)))
             stop("'a' does not have named dimnames")
         perm <- match(perm, dnna)
     }
     # the following keeps compatibility with the C code for the default method
     perm <- as.integer(perm)
     if (any(perm < 1L | perm > 2L))
         stop("value out of range in 'perm'")
     if (all(perm == 1L))
         stop("invalid 'perm' argument")
     if (identical(perm, c(1L, 2L)))
         return(a)

     ta <- t(a)
     if (!resize) {
         dim(ta) <- dim(a)
     }
     ta
}     