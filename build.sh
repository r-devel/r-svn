#!/bin/sh
export R_TEXI2DVICMD=emulation
export CPPFLAGS="-I/opt/homebrew/include"
export PDFLATEX="${PWD}/.github/scripts/dummy"
export PATH="/opt/homebrew/opt/texinfo/bin:$PATH"
sed -i.bak 's|$(GIT) svn info|./.github/scripts/svn-info.sh|' Makefile.in
CC=clang ./configure --disable-java --without-cairo --without-tcltk --without-x --without-recommended-packages --with-aqua --with-lapack --with-blas --enable-R-shlib SED=/usr/bin/sed
make -j4



