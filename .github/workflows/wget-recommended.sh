#!/usr/bin/env bash
# This is a replacement for the tools/rsync-recommended script to not overload the cran master server.
version=`cut -f1 -d' ' VERSION`
wget https://cran.rstudio.com/src/contrib/${version}/Recommended/ --no-parent -r -nv -A gz
cp -f cran.rstudio.com/src/contrib/${version}/Recommended/*.tar.gz src/library/Recommended
cd src/library/Recommended
echo "Creating links"
for i in *.tar.gz ; do
ln -s $i ${i//_*/}.tgz
done
