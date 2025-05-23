% File src/library/datasets/man/Indometh.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2011 R Core Team
% Distributed under GPL 2 or later

\name{Indometh}
\docType{data}
\alias{Indometh}
\title{Pharmacokinetics of Indomethacin}
\description{
  The \code{Indometh} data frame has 66 rows and 3 columns of data on
  the pharmacokinetics of indometacin (or, older spelling,
  \sQuote{indomethacin}).
}
\usage{Indometh}
\format{
  An object of class
  \code{c("nfnGroupedData", "nfGroupedData", "groupedData", "data.frame")}
  containing the following columns:
 \describe{
    \item{\code{Subject}}{
      an ordered factor with containing the subject codes.  The
      ordering is according to increasing maximum response.
    }
    \item{\code{time}}{
      a numeric vector of times at which blood samples were drawn (hr).
    }
    \item{\code{conc}}{
      a numeric vector of plasma concentrations of indometacin (\abbr{mcg}/ml).
    }
  }
}
\details{
  Each of the six subjects were given an intravenous injection of
  indometacin.

  This dataset was originally part of package \CRANpkg{nlme}, and that has
  methods (including for \code{[}, \code{as.data.frame}, \code{plot} and
  \code{print}) for its grouped-data classes.
}
\source{
  Kwan, Breault, Umbenhauer, McMahon and Duggan (1976)
  Kinetics of Indomethacin absorption, elimination, and
    enterohepatic circulation in man.
  \emph{Journal of Pharmacokinetics and Biopharmaceutics} \bold{4},
  255--280.

  Davidian, M. and Giltinan, D. M. (1995)
  \emph{Nonlinear Models for Repeated Measurement Data},
  Chapman & Hall (section 5.2.4, p.\sspace{}129)

  Pinheiro, J. C. and Bates, D. M. (2000) \emph{Mixed-effects Models in
    S and S-PLUS}, Springer.
}
\seealso{
  \code{\link{SSbiexp}} for models fitted to this dataset.
}
\keyword{datasets}
