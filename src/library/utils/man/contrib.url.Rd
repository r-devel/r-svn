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
    to use.  Names can declare which package type(s) a repository
    provides: see \sQuote{Details}.
  }
  \item{type}{character string, indicating which type of packages: see
    \code{\link{install.packages}}.
  }
}
\details{
  If \code{type = "both"} this will use the source repository.

  Not all repositories provide packages of all types.  A repository
  whose name in \code{repos} ends in \code{".bin"} or \code{".binary"}
  is declared to only provide binary packages, and one whose name ends
  in \code{".src"} or \code{".source"} to only provide source packages.
  Repositories not providing packages of the requested \code{type} are
  dropped from the result.  For example, with
\preformatted{  options(repos = c(CRAN.source = "https://cran.r-project.org",
                    myrepo = "https://myrepo.example.com"))}
  CRAN will only be used when looking for source packages.
}
\value{
  A character vector of the repository URLs providing packages of the
  requested \code{type}, with the type-specific path appended to each.
}
\seealso{
  \code{\link{setRepositories}} to set \code{\link{options}("repos")},
  the most common value used for argument \code{repos}.

  \code{\link{available.packages}}, \code{\link{download.packages}},
  \code{\link{install.packages}}.

  Section \manual{R-admin}{Setting up a package repository}.
}
\keyword{utilities}
