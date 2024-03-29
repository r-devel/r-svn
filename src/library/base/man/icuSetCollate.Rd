% File src/library/base/man/icuSetCollate.Rd
% Part of the R package, https://www.R-project.org
% Copyright 2008-2022 R Core Team
% Distributed under GPL 2 or later

\name{icuSetCollate}
\alias{icuSetCollate}
\alias{icuGetCollate}
\alias{R_ICU_LOCALE}

\title{ Setup Collation by ICU }
\description{
  Controls the way collation is done by ICU (an optional part of the \R
  build).
}
\usage{
icuSetCollate(...)

icuGetCollate(type = c("actual", "valid"))
}
\arguments{
  \item{\dots}{named arguments, see \sQuote{Details}.}
  \item{type}{a character string: either the \code{"actual"} locale
    in use for collation, or the most specific locale which would be
    \code{"valid"}.  Can be abbreviated.}
}
\details{
  Optionally, \R can be built to collate character strings by ICU
  (\url{https://icu.unicode.org/}).  For such systems,
  \code{icuSetCollate} can be used to tune the way collation is done.
  On other builds calling this function does nothing, with a warning.

  Possible arguments are
  \describe{
    \item{\code{locale}:}{A character string such as \code{"da_DK"}
      giving the language and country whose collation rules are to be
      used.  If present, this should be the first argument.}
    \item{\code{case_first}:}{\code{"upper"}, \code{"lower"} or
      \code{"default"}, asking for upper- or lower-case characters to be
      sorted first.  The default is usually lower-case first, but not in
      all languages (not under the default settings for Danish, for example).}
    \item{\code{alternate_handling}:}{Controls the handling of
      \sQuote{variable} characters (mainly punctuation and symbols).
      Possible values are \code{"non_ignorable"} (primary strength) and
      \code{"shifted"} (quaternary strength).}
    \item{\code{strength}:}{Which components should be used?  Possible
      values \code{"primary"}, \code{"secondary"}, \code{"tertiary"}
      (default), \code{"quaternary"} and \code{"identical"}. }
    \item{\code{french_collation}:}{In a French locale the way accents
      affect collation is from right to left, whereas in most other locales
      it is from left to right.  Possible values \code{"on"}, \code{"off"}
      and \code{"default"}.}
    \item{\code{normalization}:}{Should strings be normalized?  Possible values
      are \code{"on"} and \code{"off"} (default).  This affects the
      collation of composite characters.}
    \item{\code{case_level}:}{An additional level between secondary and
      tertiary, used to distinguish large and small Japanese Kana
      characters. Possible values \code{"on"} and \code{"off"} (default).}
    \item{\code{hiragana_quaternary}:}{Possible values \code{"on"} (sort
      Hiragana first at quaternary level) and \code{"off"}.}
  }
  Only the first three are likely to be of interest except to those with a
  detailed understanding of collation and specialized requirements.

  Some special values are accepted for \code{locale}:
  \describe{
    \item{\code{"none"}:}{ICU is not used for collation: the OS's
      collation services are used instead.}
    \item{\code{"ASCII"}:}{ICU is not used for collation: the C function
      \code{strcmp} is used instead, which should sort byte-by-byte in
      (unsigned) numerical order.}
    \item{\code{"default"}:}{
      obtains the locale from the OS as is done at the start of the
      session (except on Windows).  If environment variable
      \env{R_ICU_LOCALE} is set to a non-empty value, its value is used
      rather than consulting the OS, unless environment variable
      \env{LC_ALL} is set to 'C' (or unset but \env{LC_COLLATE} is set to
      'C'). 
    }
    \item{\code{""}, \code{"root"}:}{
      the \sQuote{root} collation: see
      \url{https://www.unicode.org/reports/tr35/tr35-collation.html#Root_Collation}.
    }
  }
  For the specifications of \sQuote{real} ICU locales, see
  \url{https://unicode-org.github.io/icu/userguide/locale/}.  Note that ICU does not
  report that a locale is not supported, but falls back to its idea of
  \sQuote{best fit} (which could be rather different and is reported by
  \code{icuGetCollate("actual")}, often \code{"root"}).  Most English
  locales fall back to \code{"root"} as although e.g.\sspace{}\code{"en_GB"} is
  a valid locale (at least on some platforms), it contains no special
  rules for collation.  Note that \code{"C"} is not a supported ICU locale
  and hence \env{R_ICU_LOCALE} should never be set to \code{"C"}.
  
  Some examples are \code{case_level = "on", strength = "primary"} to ignore
  accent differences and \code{alternate_handling = "shifted"} to ignore
  space and punctuation characters.
  
  Initially ICU will not be used for collation if the OS is set to use the
  \code{C} locale for collation and \env{R_ICU_LOCALE} is not set.  Once
  this function is called with a value for \code{locale}, ICU will be used
  until it is called again with \code{locale = "none"}.  ICU will not be
  used once \code{Sys.setlocale} is called with a \code{"C"} value for
  \code{LC_ALL} or \code{LC_COLLATE}, even if \env{R_ICU_LOCALE} is set. 
  ICU will be used again honoring \env{R_ICU_LOCALE} once
  \code{Sys.setlocale} is called to set a different collation order. 
  Environment variables \env{LC_ALL} (or \env{LC_COLLATE}) take precedence
  over \env{R_ICU_LOCALE} if and only if they are set to 'C'.  Due to the
  interaction with other ways of setting the collation order,
  \env{R_ICU_LOCALE} should be used with care and only when needed.

  All customizations are reset to the default for the locale if
  \code{locale} is specified: the collation engine is reset if the
  OS collation locate category is changed by \code{\link{Sys.setlocale}}.
}
\value{
  For \code{icuGetCollate}, a character string describing the ICU locale
  in use (which may be reported as \code{"ICU not in use"}).  The
  \sQuote{actual} locale may be simpler than the requested locale: for
  example \code{"da"} rather than \code{"da_DK"}: English locales are
  likely to report \code{"root"}.
}
\note{
  Except on Windows, ICU is used by default wherever it is available.
  As it works internally in UTF-8, it will be most efficient in UTF-8
  locales.

  On Windows, \R is normally built including ICU, but it will only be
  used if environment variable \env{R_ICU_LOCALE} had been set when \R
  is started or after \code{icuSetCollate} is called to select the
  locale (as ICU and Windows differ in their idea of locale names).
  Note that \code{icuSetCollate(locale = "default")} should work
  reasonably well, but finds the system default ignoring environment
  variables such as \env{LC_COLLATE}.
}
\seealso{
  \link{Comparison}, \code{\link{sort}}.

  \code{\link{capabilities}} for whether ICU is available;
  \code{\link{extSoftVersion}} for its version.

  The ICU user guide chapter on collation
  (\url{https://unicode-org.github.io/icu/userguide/collation/}).
}
\examples{\donttest{
## These examples depend on having ICU available, and on the locale.
## As we don't know the current settings, we can only reset to the default.
if(capabilities("ICU")) withAutoprint({
    icuGetCollate()
    icuGetCollate("valid")
    x <- c("Aarhus", "aarhus", "safe", "test", "Zoo")
    sort(x)
    icuSetCollate(case_first = "upper"); sort(x)
    icuSetCollate(case_first = "lower"); sort(x)

    ## Danish collates upper-case-first and with 'aa' as a single letter
    icuSetCollate(locale = "da_DK", case_first = "default"); sort(x) 
    ## Estonian collates Z between S and T
    icuSetCollate(locale = "et_EE"); sort(x)
    icuSetCollate(locale = "default"); icuGetCollate("valid")
})
}}
\keyword{ utilities }
