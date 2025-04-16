#!/bin/sh
# Run script safely and emit some verbose output
set -e
set -x

# Put pdflatex on the path (needed only for CMD check)
if [ "$( uname -s | grep ARM)" ]; then
export PATH="/c/aarch64-w64-mingw32.static.posix/bin:$PATH:/c/Users/$USER/AppData/Roaming/TinyTeX/bin/windows:$HOME/AppData/Local/Programs/MiKTeX/miktex/bin/x64:/c/progra~1/MiKTeX/miktex/bin/x64:/c/progra~1/MiKTeX 2.9/miktex/bin/x64"
tcltk="https://github.com/r-windows/rtools-chocolatey/releases/download/6536/tcltk-aarch64-6536-6492.zip"
echo "USE_LLVM = 1" >> src/gnuwin32/MkRules.local
clang --version
else
export PATH="/c/x86_64-w64-mingw32.static.posix/bin:$PATH:/c/Users/$USER/AppData/Roaming/TinyTeX/bin/windows:$HOME/AppData/Local/Programs/MiKTeX/miktex/bin/x64:/c/progra~1/MiKTeX/miktex/bin/x64:/c/progra~1/MiKTeX 2.9/miktex/bin/x64"
tcltk="https://github.com/r-windows/rtools-chocolatey/releases/download/6536/tcltk-6536-6492.zip"
gcc --version
fi

echo "PATH: $PATH"
pdflatex --version
texindex --version
texi2any --version
make --version
perl --version

# Extra steps to prepare SVN build (rather than official tarball)
cd "$(cygpath ${GITHUB_WORKSPACE})"
sed -i.bak 's|$(GIT) svn info|./.github/scripts/svn-info.sh|' src/include/Makefile.win
curl -sSL https://curl.se/ca/cacert.pem > etc/curl-ca-bundle.crt
#./tools/rsync-recommended
./.github/scripts/wget-recommended.sh
./.github/scripts/svn-info.sh

# Download the TCL bundle required by tcltk package
curl -o tcltk.zip -L $tcltk
unzip -q tcltk.zip

# Add custom flags to MkRules.local
#cp .github/scripts/MkRules.local src/gnuwin32/
cd src/gnuwin32

# Build just the installer
make distribution
