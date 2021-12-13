#!/bin/sh
# Minimal script to build R for Windows from SVN.
# You must run this script inside the rtools40 shell.
# This builds and checks a single architecture (no manuals or installer)

# Run script safely and emit some verbose output
set -e
set -x

# Absolute path to this script
scripts=$(dirname $(realpath $0))
sources=$(cygpath ${GITHUB_WORKSPACE})

# Put pdflatex on the path (needed only for CMD check)
export PATH="$PATH:/c/progra~1/git/bin:/c/progra~1/MiKTeX/miktex/bin/x64"
echo "PATH: $PATH"
pdflatex --version
texindex --version
texi2any --version
make --version
perl --version

# Extra steps to prepare SVN build (rather than official tarball)
cd "${sources}"
sed -i.bak 's|$(GIT) svn info|./.github/workflows/svn-info.sh|' src/include/Makefile.win
curl -sSL https://curl.se/ca/cacert.pem > etc/curl-ca-bundle.crt
./tools/rsync-recommended
./.github/workflows/svn-info.sh

# Install system libs
pacman -Syu --noconfirm
pacman -S --needed --noconfirm mingw-w64-ucrt-x86_64-{gcc,gcc-fortran,icu,libtiff,libjpeg,libpng,pcre2,xz,bzip2,zlib,cairo,tk,curl,libwebp}

# Create the TCL bundle required by tcltk package
mkdir -p Tcl/{bin,lib}
${scripts}/create-tcltk-bundle.sh

# Build 64 bit + docs and installers
cd "${sources}/src/gnuwin32"
TEXINDEX=$(cygpath -m $(which texindex))  
sed -e "s|@texindex@|${TEXINDEX}|" "${scripts}/MkRules.local.in" > MkRules.local
make distribution
