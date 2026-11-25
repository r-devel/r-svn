% File src/library/utils/man/update.packages.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2014 R Core Team
% Distributed under GPL 2 or later

\name{contrib.url}
\alias{contrib.url}
\title{Find Appropriate Paths in CRAN-like Repositories}
\description{
  \code{contrib.url} adds the appropriate type-specific path within a
  repository to each URL in \code{repos}.
}
\usage{
contrib.url(repos, type = getOption("pkgType"))
}
\arguments{
  \item{repos}{character vector, the base URL(s) of the repositories
    to use.  Can also declare which package type(s) each repository
    provides: see \sQuote{Details}.
  }
  \item{type}{character string, indicating which type of packages: see
    \code{\link{install.packages}}.
  }
}
\details{
  If \code{type = "both"} this will use the source repository.

  Not all repositories provide packages of all types: repositories not
  providing packages of the requested type are dropped from the result.
  Which type(s) a repository provides can be declared via an optional
  \code{"pkgType"} attribute on \code{repos}, a character vector of the
  same length with elements \code{"both"}, \code{"source"} or
  \code{"binary"} (elements without a declared type are treated as
  \code{"both"}, so a plain character vector behaves as before):
\preformatted{  options(repos = structure(
      c(myrepo = "https://myrepo.example.com", CRAN = "https://cran.r-project.org"),
      pkgType = c("both", "source")))}
}
\value{
  A character vector of the repository URLs providing packages of the
  resolved \code{type}, with the type-specific path appended to each.
}
\seealso{
  \code{\link{setRepositories}} to set \code{\link{options}("repos")},
  the most common value used for argument \code{repos}.

  \code{\link{available.packages}}, \code{\link{download.packages}},
  \code{\link{install.packages}}.

  Section \manual{R-admin}{Setting up a package repository}.
}
\keyword{utilities}
