#!/bin/sh
# Run script safely and emit some verbose output
set -e
set -x

# Put pdflatex on the path (needed only for CMD check)
export PATH="/c/x86_64-w64-mingw32.static.posix/bin:$PATH:/c/Users/$USER/AppData/Roaming/TinyTeX/bin/windows:$HOME/AppData/Local/Programs/MiKTeX/miktex/bin/x64:/c/progra~1/MiKTeX/miktex/bin/x64:/c/progra~1/MiKTeX 2.9/miktex/bin/x64"
echo "PATH: $PATH"
pdflatex --version
texindex --version
texi2any --version
make --version
perl --version
gcc --version

# Extra steps to prepare SVN build (rather than official tarball)
cd "$(cygpath ${GITHUB_WORKSPACE})"
sed -i.bak 's|$(GIT) svn info|./.github/scripts/svn-info.sh|' src/include/Makefile.win
curl -sSL https://curl.se/ca/cacert.pem > etc/curl-ca-bundle.crt
./tools/rsync-recommended
./.github/scripts/svn-info.sh

# Download the TCL bundle required by tcltk package
curl -OL https://github.com/r-windows/rtools-chocolatey/releases/download/6104/tcltk-6104-6025.zip
unzip tcltk-6104-6025.zip

# Add custom flags to MkRules.local
cp .github/scripts/MkRules.local src/gnuwin32/
cd src/gnuwin32

# Build just the core pieces (no installer)
make all cairodevices recommended vignettes manuals

# Run checks
make check-all
