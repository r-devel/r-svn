require("tools")

(ud4 <- undoc("stats4"))
stopifnot(sapply(ud4, length) == 0)

## PR#18960: undoc(dir = ) for package with \packageFOO macros
pkg <- tempfile("testpkg")
dir.create(file.path(pkg, "man"), recursive = TRUE)
dir.create(file.path(pkg, "R"))

write.dcf(file = file.path(pkg, "DESCRIPTION"),
  list(Package = "testpkg",
       Title = "Test Package Title",
       Version = "1.0",
       Description = "Test package description.",
       Author = "Test Author",
       Maintainer = "Test Maintainer <tm@example.com>",
       License = "GPL-2"))

f_pkg_rd <- file.path(pkg, "man", "testpkg-package.Rd")
cat(file = f_pkg_rd, r"(
\name{testpkg-package}
\title{\packageTitle{testpkg}}
\description{\packageDescription{testpkg}}
\author{\packageAuthor{testpkg}}
\keyword{package}
)")
cat(file = file.path(pkg, "R", "foo.R"),
    "foo <- function() 1\n")

stopifnot(identical(undoc(dir = pkg)[["code objects"]], "foo"))
cat(file = f_pkg_rd, r"(
\name{testpkg-package}
\alias{foo}
\title{\packageTitle{testpkg}}
\description{\packageDescription{testpkg}}
\author{\packageAuthor{testpkg}}
\keyword{package}
)")
stopifnot(lengths(undoc(dir = pkg)) == 0)

unlink(pkg, recursive = TRUE)
