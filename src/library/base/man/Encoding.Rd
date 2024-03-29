% File src/library/base/man/Encoding.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2023 R Core Team
% Distributed under GPL 2 or later

\name{Encoding}
\alias{Encoding}
\alias{Encoding<-}
\alias{enc2native}
\alias{enc2utf8}
\concept{encoding}
\title{Read or Set the Declared Encodings for a Character Vector}
\description{
  Read or set the declared encodings for a character vector.
}
\usage{
Encoding(x)

Encoding(x) <- value

enc2native(x)
enc2utf8(x)
}
\arguments{
  \item{x}{A character vector.}
  \item{value}{A character vector of positive length.}
}
\details{
  Character strings in \R can be declared to be encoded in
  \code{"latin1"} or \code{"UTF-8"} or as \code{"bytes"}.  These
  declarations can be read by \code{Encoding}, which will return a
  character vector of values \code{"latin1"}, \code{"UTF-8"}
  \code{"bytes"} or \code{"unknown"}, or set, when \code{value} is
  recycled as needed and other values are silently treated as
  \code{"unknown"}.  ASCII strings will never be marked with a declared
  encoding, since their representation is the same in all supported
  encodings.  Strings marked as \code{"bytes"} are intended to be
  non-ASCII strings which should be manipulated as bytes, and never
  converted to a character encoding (so writing them to a text file is
  supported only by \code{writeLines(useBytes = TRUE)}).
  % non-bug report PR#16327

  \code{enc2native} and \code{enc2utf8} convert elements of character
  vectors to the native encoding or UTF-8 respectively, taking any
  marked encoding into account.  They are \link{primitive} functions,
  designed to do minimal copying.

  There are other ways for character strings to acquire a declared
  encoding apart from explicitly setting it (and these have changed as
  \R has evolved).  The parser marks strings containing \samp{\\u} or
  \samp{\\U} escapes.  Functions \code{\link{scan}},
  \code{\link{read.table}}, \code{\link{readLines}}, and
  \code{\link{parse}} have an \code{encoding} argument that is used to
  declare encodings, \code{\link{iconv}} declares encodings from its
  \code{to} argument, and console input in suitable locales is also
  declared.  \code{\link{intToUtf8}} declares its output as
  \code{"UTF-8"}, and output text connections (see
  \code{\link{textConnection}}) are marked if running in a
  suitable locale.  Under some circumstances (see its help page)
  \code{\link{source}(encoding=)} will mark encodings of character
  strings it outputs.

  Most character manipulation functions will set the encoding on output
  strings if it was declared on the corresponding input.  These include
  \code{\link{chartr}}, \code{\link{strsplit}(useBytes = FALSE)},
  \code{\link{tolower}} and \code{\link{toupper}} as well as
  \code{\link{sub}(useBytes = FALSE)} and \code{\link{gsub}(useBytes =
  FALSE)}.  Note that such functions do not \emph{preserve} the
  encoding, but if they know the input encoding and that the string has
  been successfully re-encoded (to the current encoding or UTF-8), they
  mark the output.

  \code{\link{substr}} does preserve the encoding, and
  \code{\link{chartr}}, \code{\link{tolower}} and \code{\link{toupper}}
  preserve UTF-8 encoding on systems with Unicode wide characters.  With
  their \code{fixed} and \code{perl} options, \code{\link{strsplit}},
  \code{\link{sub}} and \code{gsub} will give a marked UTF-8 result if
  any of the inputs are UTF-8.

 %% \code{\link{strsplit}}, \code{\link{sub}} 
 %%  and \code{gsub} will mark their result \code{"bytes"} if it is non-ASCII
 %%  and any of the inputs are marked \code{"bytes"} or \code{useBytes = TRUE}
 %% was used.

  \code{\link{paste}} and \code{\link{sprintf}} return elements marked
  as bytes if any of the corresponding inputs is marked as bytes, and
  otherwise marked as UTF-8 if any of the inputs is marked as UTF-8.

  \code{\link{match}}, \code{\link{pmatch}}, \code{\link{charmatch}},
  \code{\link{duplicated}} and \code{\link{unique}} all match in UTF-8
  if any of the elements are marked as UTF-8.

  Changing the current encoding from a running R session may lead to
  confusion (see \code{\link{Sys.setlocale}}).

  There is some ambiguity as to what is meant by a \sQuote{Latin-1}
  locale, since some OSes (notably Windows) make use of character
  positions undefined (or used for control characters) in the ISO 8859-1
  character set.  How such characters are interpreted is
  system-dependent but as from \R 3.5.0 they are if possible interpreted
  as per Windows codepage 1252 (which Microsoft calls \sQuote{Windows
  Latin 1 (ANSI)}) when converting to e.g.\sspace{}UTF-8.
}
\value{
  A character vector.

  For \code{enc2utf8} encodings are always marked: they are for
  \code{enc2native} in UTF-8 and Latin-1 locales.
}
\examples{
## x is intended to be in latin1
x. <- x <- "fran\xE7ais"
Encoding(x.) # "unknown" (UTF-8 loc.) | "latin1" (8859-1/CP-1252 loc.) | ....
Encoding(x) <- "latin1"
x
xx <- iconv(x, "latin1", "UTF-8")
Encoding(c(x., x, xx))
c(x, xx)
xb <- xx; Encoding(xb) <- "bytes"
xb # will be encoded in hex
cat("x = ", x, ", xx = ", xx, ", xb = ", xb, "\n", sep = "")
(Ex <- Encoding(c(x.,x,xx,xb)))
stopifnot(identical(Ex, c(Encoding(x.), Encoding(x),
                          Encoding(xx), Encoding(xb))))
}
\keyword{utilities}
\keyword{character}
