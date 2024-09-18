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
#curl -OL https://cran.r-project.org/bin/windows/Rtools/rtools43/files/tcltk-5493-5412.zip
curl -OL https://github.com/r-windows/rtools-chocolatey/releases/download/5948/Tcl-5948-5877.zip
unzip Tcl-5948-5877.zip

# Add custom flags to MkRules.local
cp .github/scripts/MkRules.local src/gnuwin32/
cd src/gnuwin32

# Build just the installer
make distribution
