% File src/library/utils/man/INSTALL.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2025 R Core Team
% Distributed under GPL 2 or later

\name{INSTALL}
\alias{INSTALL}
\alias{R_INSTALL_TAR}
\alias{R_INSTALL_STAGED}

\title{Install Add-on Packages}
\description{Utility for installing add-on packages.}
\usage{
\special{R CMD INSTALL [options] [-l lib] pkgs}
}
\arguments{
  \item{pkgs}{a space-separated list with the path names of the packages to be
    installed.  See \sQuote{Details}.}
  \item{lib}{the path name of the \R library tree to install to.  Also
    accepted in the form \samp{--library=lib}.  Paths including spaces should
    be quoted, using the conventions for the shell in use.}
  \item{options}{a space-separated list of options through which in
    particular the process for building the help files can be controlled.
    Use \command{R CMD INSTALL --help} for the full current list of options.}
}
\details{
  This will stop at the first error, so if you want all the \code{pkgs}
  to be tried, call this via a shell loop.

  If used as \command{R CMD INSTALL pkgs} without explicitly specifying
  \code{lib}, packages are installed into the library tree rooted at the
  first directory in the library path which would be used by \R run in
  the current environment.

  To install into the library tree \code{\var{lib}}, use
  \command{R CMD INSTALL -l \var{lib} \var{pkgs}}.
  This prepends \code{lib} to the library path for
  duration of the install, so required packages in the installation
  directory will be found (and used in preference to those in other
  libraries).
#ifdef windows

  It is possible that environment variable \env{TMPDIR}
  will need to be set (although \command{R CMD} tries to set it suitably
  otherwise): use forward slashes and no spaces for the path to a
  writable directory.
#endif

  Both \code{lib} and the elements of \code{pkgs} may be absolute or
  relative path names of directories.  \code{pkgs} may also contain
  names of package archive files: these are then extracted to a
  temporary directory.  These are tarballs containing a single
  directory, optionally compressed by \command{gzip}, \command{bzip2},
  \command{xz} or \command{compress}.
#ifdef unix
  Finally, binary package archive files (as created by
  \command{R CMD INSTALL --build}) can be supplied.
#endif
#ifdef windows
  Finally, zipped binary packages (as created by
  \command{R CMD INSTALL --build}) can be supplied.
#endif

  Tarballs are by default unpackaged by the internal \code{\link{untar}}
  function: if needed an external \command{tar} command can be specified
  by the environment variable \env{R_INSTALL_TAR}: please ensure that it
  can handle the type of compression used on the tarball.  (This is
  sometimes needed for tarballs containing invalid or unsupported
  sections, and can be faster on very large tarballs.   Setting
  \env{R_INSTALL_TAR} to \samp{tar.exe} has been needed to overcome
  permissions issues on some Windows systems.)

  The package sources can be cleaned up prior to installation by
  \option{--preclean} or after by \option{--clean}: cleaning is
  essential if the sources are to be used with more than one
  architecture or platform.
#ifdef unix

  Some package sources contain a \file{configure} script that can be
  passed arguments or variables via the option \option{--configure-args}
  and \option{--configure-vars}, respectively, if necessary.  The latter
  is useful in particular if libraries or header files needed for the
  package are in non-system directories.  In this case, one can use the
  configure variables \code{LIBS} and \code{CPPFLAGS} to specify these
  locations (and set these via \option{--configure-vars}), see section
  \sQuote{Configuration variables} in
  \sQuote{R Installation and Administration} for more information.
  (If these are used more than
  once on the command line they are concatenated.)  The configure
  mechanism can be bypassed using the option \option{--no-configure}.
#endif

  If the attempt to install the package fails, leftovers are removed.
  If the package was already installed, the old version is restored.
  This happens either if a command encounters an error or if the
  install is interrupted from the keyboard: after cleaning up the script
  terminates.

  For details of the locking which is done, see the section
  \sQuote{Locking} in the help for \code{\link{install.packages}}.

#ifdef windows

  Option \option{--build} can be used to zip up the installed package
  for distribution.
#endif
#ifdef unix

  Option \option{--build} can be used to tar up the installed package
  for distribution as a binary package (as used on macOS).  This is done
  by \code{utils::tar} unless environment variable \env{R_INSTALL_TAR}
  is set.
#endif

  By default a package is installed with static HTML help pages if and
  only if \R was: use options \option{--html} and \option{--no-html} to
  override this.

  Packages are not by default installed keeping the source formatting
  (see the \code{keep.source} argument to \code{\link{source}}): this
  can be enabled by the option \option{--with-keep.source} or by setting
  environment variable \env{R_KEEP_PKG_SOURCE} to \code{yes}.

  Specifying the \option{--install-tests} option copies the contents
  of the \file{tests} directory into the package installation. If the
  \env{R_ALWAYS_INSTALL_TESTS} environment variable is set to a true
  value, the tests will be installed even if \option{--install-tests} is
  omitted.

  Use \command{R CMD INSTALL --help} for concise usage information,
  including all the available options.
}

\note{
  The options do not have to precede \samp{pkgs} on the command line,
  although it will be more legible if they do.  All the options are
  processed before any packages, and where options have conflicting
  effects the last one will win.
#ifdef unix

  Some parts of the operation of \code{INSTALL} depend on the \R
  temporary directory (see \code{\link{tempdir}}, usually under
  \file{/tmp}) having both write and execution access to the account
  running \R.  This is usually the case, but if \file{/tmp} has been
  mounted as \code{noexec}, environment variable \env{TMPDIR} may need
  to be set to a directory from which execution is allowed.

  For most recent builds of \R the internal \code{untar} will handle
  tarballs compressed by \command{zstd}: most (but not all) external
  \command{tar} commands will handle \command{ztd} compression and
  others such as \command{lz4}.
#endif
}

\section{Sub-architectures}{
  An \R installation can support more than one sub-architecture:
  currently this is most commonly used for 32- and 64-bit builds on
  Windows.
  
  For such installations, the default behaviour is to try to install
  source packages for all installed sub-architectures unless the package
  has a configure script or a \file{src/Makefile} (or
  \file{src/Makefile.win} on Windows), when only compiled code for the
  sub-architecture running \command{R CMD INSTALL} is installed.

  To install a source package with compiled code only for the
  sub-architecture used by \command{R CMD INSTALL}, use
  \option{--no-multiarch}.  To install just the compiled code for
  another sub-architecture, use \option{--libs-only}.

  There are two ways to install for all available sub-architectures.  If
  the configure script is known to work for both Windows architectures,
  use flag \option{--force-biarch} (and packages can specify this
  \emph{via} a \samp{Biarch: yes} field in their \code{DESCRIPTION} files).
  Second, a single tarball can be installed with
\preformatted{R CMD INSTALL --merge-multiarch mypkg_version.tar.gz}
}

\section{Staged installation}{
  The default way to install source packages changed in \R 3.6.0, so
  packages are first installed to a temporary location and then (if
  successful) moved to the destination library directory.  Some older
  packages were written in ways that assume direct installation to the
  destination library.

  Staged installation can currently be overridden by having a line
  \samp{StagedInstall: no} in the package's \file{DESCRIPTION} file,
  \emph{via} flag \option{--no-staged-install} or by setting environment
  variable \env{R_INSTALL_STAGED} to a false value
  (e.g.\sspace{}\samp{false} or \samp{no}).

  Staged installation requires either \option{--pkglock} or
  \option{--lock}, one of which is used by default.
}

\seealso{
  \code{\link{REMOVE}};
  \code{\link{.libPaths}} for information on using several library trees;
  \code{\link{install.packages}} for \R-level installation of packages;
  \code{\link{update.packages}} for automatic update of packages using
  the Internet or a local repository.

  The chapter on \sQuote{Add-on packages} in
  \sQuote{R Installation and Administration}
  and the chapter on \sQuote{Creating R packages} in
  \sQuote{Writing R Extensions}
#ifdef windows
  (on the Help menu in \code{RGui}),
#endif
  \emph{via} \code{\link{RShowDoc}} or in the \file{doc/manual}
  subdirectory of the \R source tree.
}
\keyword{utilities}
