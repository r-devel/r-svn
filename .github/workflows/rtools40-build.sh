#!/bin/sh
# Minimal script to build R for Windows from SVN.
# You must run this script inside the rtools40 shell.
# This builds and checks a single architecture (no manuals or installer)

# Run script safely and emit some verbose output
set -e
set -x

# Absolute path to this script
srcdir=$(dirname $(realpath $0))

# Substitute some ucrt64 specific inputs
if [ "$toolchain" = "ucrt64" ]; then
sed -i 's|/mingw|/ucrt|g' "${srcdir}/MkRules.local.in"
sed -i 's|/mingw64|/ucrt64|g' "${srcdir}/create-tcltk-bundle.sh"
sed -i 's|,x86_64|,ucrt-x86_64|g' "${srcdir}/create-tcltk-bundle.sh"
fi

# Temp fix for older rtools40 installations
if ! grep -Fq "ucrt64" /etc/pacman.conf; then
echo "[ucrt64]" >> /etc/pacman.conf
echo "Server = https://cran.r-project.org/bin/windows/Rtools/4.0/ucrt64/" >> /etc/pacman.conf
echo "SigLevel = Never" >> /etc/pacman.conf
fi

# Get architecture
case ${toolchain} in
  mingw32)
    _arch=i686
    WIN=32
  ;;
  mingw64)
    _arch=x86_64
    WIN=64
  ;;
  ucrt64)
    _arch=ucrt-x86_64
    WIN=64
  ;;
esac

# Put pdflatex on the path (needed only for CMD check)
export PATH="$PATH:$HOME/AppData/Local/Programs/MiKTeX/miktex/bin/x64:/c/progra~1/MiKTeX/miktex/bin/x64:/c/progra~1/MiKTeX 2.9/miktex/bin/x64"
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
pacman -Sy --noconfirm
pacman -S --needed --noconfirm mingw-w64-${_arch}-{gcc,gcc-fortran,icu,libtiff,libjpeg,libpng,pcre2,xz,bzip2,zlib,cairo,tk,curl}

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
