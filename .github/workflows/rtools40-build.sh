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
export PATH="/ucrt64/bin:$PATH:$HOME/AppData/Local/Programs/MiKTeX/miktex/bin/x64:/c/progra~1/MiKTeX/miktex/bin/x64:/c/progra~1/MiKTeX 2.9/miktex/bin/x64"
echo "PATH: $PATH"
pdflatex --version
texindex --version
texi2any --version
make --version
perl --version
gcc --version

# Extra steps to prepare SVN build (rather than official tarball)
cd "$(cygpath ${GITHUB_WORKSPACE})"
sed -i.bak 's|$(GIT) svn info|./.github/workflows/svn-info.sh|' src/include/Makefile.win
curl -sSL https://curl.se/ca/cacert.pem > etc/curl-ca-bundle.crt
./tools/rsync-recommended
./.github/workflows/svn-info.sh

# Install system libs
pacman -Sy --noconfirm
pacman -S --needed --noconfirm mingw-w64-ucrt-x86_64-{gcc,gcc-fortran,icu,libtiff,libjpeg,libpng,pcre2,xz,bzip2,zlib,cairo,tk,curl,libwebp}

# Create the TCL bundle required by tcltk package
mkdir -p Tcl/{bin,lib}
${srcdir}/create-tcltk-bundle.sh

# Build just the core pieces (no manuals or installer)
TEXINDEX=$(cygpath -m $(which texindex))
cd "src/gnuwin32"
sed -e "s|@texindex@|${TEXINDEX}|" "${srcdir}/MkRules.local.in" > MkRules.local
cat MkRules.local
make all cairodevices recommended vignettes manuals

# Optional: run checks
make check-all
