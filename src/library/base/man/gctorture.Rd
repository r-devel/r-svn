% File src/library/base/man/gctorture.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2014 R Core Team
% Distributed under GPL 2 or later

\name{gctorture}
\alias{gctorture}
\alias{gctorture2}
\alias{R_GCTORTURE}
\alias{R_GCTORTURE_WAIT}
\alias{R_GCTORTURE_INHIBIT_RELEASE}
\title{Torture Garbage Collector}
\usage{
gctorture(on = TRUE)
gctorture2(step, wait = step, inhibit_release = FALSE)
}
\arguments{
  \item{on}{logical; turning it on/off.}
  \item{step}{integer; run \abbr{GC} every \code{step} allocations; \code{step
      = 0} turns the \abbr{GC} torture off.}
  \item{wait}{integer; number of allocations to wait before starting
    \abbr{GC} torture.}
  \item{inhibit_release}{logical; do not release free objects for
    re-use: use with caution.}
}
\description{
  Provokes garbage collection on (nearly) every memory allocation.
  Intended to ferret out memory protection bugs.  Also makes \R run
  \emph{very} slowly, unfortunately.
}
\details{
  Calling \code{gctorture(TRUE)} instructs the memory manager to force a
  full \abbr{GC} on every allocation. \code{gctorture2} provides a more refined
  interface that allows the start of the \abbr{GC} torture to be deferred and
  also gives the option of running a \abbr{GC} only every \code{step}
  allocations.

  The third argument to \code{gctorture2} is only used if R has been
  configured with a strict write barrier enabled.  When this is the case
  all garbage collections are full collections, and the memory manager
  marks free nodes and enables checks in many situations that signal an
  error when a free node is used.  This can help greatly in isolating
  unprotected values in C code.  It does not detect the case where a
  node becomes free and is reallocated.  The \code{inhibit_release}
  argument can be used to prevent such reallocation.  This will cause
  memory to grow and should be used with caution and in conjunction with
  operating system facilities to monitor and limit process memory use.

  \code{gctorture2} can also be invoked via environment variables at the
  start of the \R session.  \env{R_GCTORTURE} corresponds to the
  \code{step} argument, \env{R_GCTORTURE_WAIT} to \code{wait}, and
  \env{R_GCTORTURE_INHIBIT_RELEASE} to \code{inhibit_release}.
}
\value{
  Previous value of first argument.
}
\author{Peter Dalgaard and Luke Tierney}
\keyword{environment}

