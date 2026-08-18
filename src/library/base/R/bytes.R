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

bytes <- function(length = 0L, width = 1L)
    .Internal(bytes(length, width))

as.bytes <- function(x, width = 1L)
    .Internal(as.bytes(x, width))

bytesRaw <- function(x) .Internal(bytesRaw(x))

bytesWidth <- function(x) .Internal(bytesWidth(x))

is.bytes <- function(x) typeof(x) == "bytes"
