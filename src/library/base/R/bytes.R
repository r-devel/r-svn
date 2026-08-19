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

bytes <- function(length = 0L, width = 1L,
                  kind = c("opaque", "unsigned", "signed"))
    .Internal(bytes(length, width, match.arg(kind)))

as.bytes <- function(x, width = 1L,
                     kind = c("opaque", "unsigned", "signed"))
    .Internal(as.bytes(x, width, match.arg(kind)))

bytesNA <- function(length = 1L, width = 1L,
                    kind = c("opaque", "unsigned", "signed"))
    .Internal(bytesNA(length, width, match.arg(kind)))

bytesRaw <- function(x) .Internal(bytesRaw(x))

bytesKind <- function(x) .Internal(bytesKind(x))

bytesWidth <- function(x) .Internal(bytesWidth(x))

is.bytes <- function(x) .Internal(is.bytes(x))
