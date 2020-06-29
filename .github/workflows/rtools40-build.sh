#!/bin/sh
# Minimal script to build R for Windows from SVN.
# You must run this script inside the rtools40 shell.
# This builds and checks a single architecture (no manuals or installer)

# Run script safely and emit some verbose output
set -e
set -x

# Absolute path to this script
srcdir=$(dirname $(realpath $0))

# Put pdflatex on the path (needed only for CMD check)
export PATH="$PATH:/c/progra~1/git/bin:/c/progra~1/MiKTeX/miktex/bin/x64"
echo "PATH: $PATH"
pdflatex --version
texindex --version
texi2any --version
make --version
perl --version

# Extra steps to prepare SVN build (rather than official tarball)
cd "$(cygpath ${GITHUB_WORKSPACE})"
sed -i.bak 's|$(GIT) svn info|./.github/workflows/svn-info.sh|' src/include/Makefile.win
curl -sSL https://curl.haxx.se/ca/cacert.pem > etc/curl-ca-bundle.crt
./tools/rsync-recommended
./.github/workflows/svn-info.sh

# Install system libs
pacman -Syu --noconfirm
pacman -S --needed --noconfirm mingw-w64-{i686,x86_64}-{gcc,gcc-fortran,icu,libtiff,libjpeg,libpng,pcre2,xz,bzip2,zlib,cairo,tk,curl}

# Create the TCL bundle required by tcltk package
mkdir -p Tcl/{bin,bin64,lib,lib64}
${srcdir}/create-tcltk-bundle.sh

# Temporary patches to prevent R from using the old toolchain or binary 
# packages from CRAN compiled with the old toolchain.
#patch -Np1 -i "${srcdir}/rtools40.patch" 
#sed -i 's|PLATFORM_PKGTYPE|NONE|' src/main/Makefile.win

# Build just the core pieces (no manuals or installer)
TEXINDEX=$(cygpath -m $(which texindex))
cd "src/gnuwin32"
sed -e "s|@win@|${WIN}|" -e "s|@texindex@|${TEXINDEX}|" -e "s|@home32@||" "${srcdir}/MkRules.local.in" > MkRules.local
cat MkRules.local
make all cairodevices recommended vignettes manuals

# Optional: run checks
make check-all
