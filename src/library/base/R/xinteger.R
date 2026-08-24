#  File src/library/base/R/xinteger.R
#  Part of the R package, https://www.R-project.org
#
#  Copyright (C) 2026 The R Core Team
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

## The built-in signed and unsigned 64-bit ALTSXP classes.

## kind = "unsigned"  : unsigned integers of 8*width bits
## kind = "signed"    : two's complement integers of 8*width bits
## Elements are stored in native order, so reading them from
## an external source is a plain copy, and order by value, so sorting
## is the same on every platform.

## na = FALSE declines to reserve a value for NA, so every bit pattern
## of the width is a legitimate value.  The price is that operations
## which would produce NA -- an out-of-range subscript, a join miss,
## arithmetic overflow -- become errors rather than missing values.

xinteger <- function(length = 0L, width,
                  kind = c("unsigned", "signed"), na = TRUE)
    .Internal(xinteger(length, width, match.arg(kind), na))

## x may be raw (reinterpreted in native byte order), character (parsed
## as decimal), or integer/logical (narrowed, as in arithmetic).
as.xinteger <- function(x, width,
                     kind = c("unsigned", "signed"), na = TRUE)
    .Internal(as.xinteger(x, width, match.arg(kind), na))

xintegerHasNA <- function(x) .Internal(xintegerHasNA(x))

xintegerRaw <- function(x) .Internal(xintegerRaw(x))

xintegerKind <- function(x) .Internal(xintegerKind(x))

xintegerWidth <- function(x) .Internal(xintegerWidth(x))

is.xinteger <- function(x) .Internal(is.xinteger(x))

as.int64  <- function(x, na = TRUE) as.xinteger(x,  8L, "signed",   na)
as.uint64 <- function(x, na = TRUE) as.xinteger(x,  8L, "unsigned", na)

## The detailed 'xinteger' storage modes, which name a width and a kind:
## the R-level screen readBin() and `mode<-` need before handing a name
## over, in one place so the two cannot drift.
##
## An exact list rather than a pattern, because the widths are a closed
## set (see XINT_WIDTH_OK in src/include/Defn.h) and it is the same set
## R_xintTypeFromName() accepts.  A name of the same shape but another
## width -- "int24" -- is therefore not one of these, and keeps whatever
## meaning it had before this type existed rather than becoming an error
## in readBin().
.XIntTypeNames <- c("int64", "uint64")

.isXIntTypeName <- function(s) s %in% .XIntTypeNames
