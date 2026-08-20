#  File src/library/base/R/bytes.R
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

## Vectors of fixed-width opaque data.  length() counts elements; each
## element is `width` bytes and is only ever compared or hashed as a
## block of bytes, never interpreted as a number.

## kind = "opaque"   : byte strings; lexicographic order, hex display
## kind = "unsigned"  : unsigned integers of 8*width bits
## kind = "signed"    : two's complement integers of 8*width bits
## The numeric kinds store bytes in native order, so reading them from
## an external source is a plain copy, and order by value, so sorting
## is the same on every platform.

## na = FALSE declines to reserve a value for NA, so every bit pattern
## of the width is a legitimate value.  The price is that operations
## which would produce NA -- an out-of-range subscript, a join miss,
## arithmetic overflow -- become errors rather than missing values.

bytes <- function(length = 0L, width = 1L,
                  kind = c("opaque", "unsigned", "signed"), na = TRUE)
    .Internal(bytes(length, width, match.arg(kind), na))

## x may be raw (reinterpreted verbatim, so the numeric kinds take
## native byte order and ingest is a plain copy), character (parsed:
## decimal for the numeric kinds, hex for opaque -- the inverse of
## as.character), or integer/logical (narrowed, as in arithmetic).
as.bytes <- function(x, width = 1L,
                     kind = c("opaque", "unsigned", "signed"), na = TRUE)
    .Internal(as.bytes(x, width, match.arg(kind), na))

bytesHasNA <- function(x) .Internal(bytesHasNA(x))

bytesRaw <- function(x) .Internal(bytesRaw(x))

bytesKind <- function(x) .Internal(bytesKind(x))

bytesWidth <- function(x) .Internal(bytesWidth(x))

is.bytes <- function(x) .Internal(is.bytes(x))
