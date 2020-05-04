#/bin/sh
set -e
OUTPUT=$(mktemp -d)
FILES=$(pacman -Sp mingw-w64-{i686,x86_64}-{tcl,tk,bwidget,tktable} --cache=".")
for FILE in $FILES
do
    curl -OLs $FILE
	echo "    Extracting: $(basename $FILE)"
	tar xf $(basename $FILE) -C ${OUTPUT}
	unlink $(basename $FILE)
done
rm -f $(find ${OUTPUT} -name *.a)

# Copy to 'Tcl' directory
rm -Rf Tcl && mkdir Tcl
mkdir -p Tcl/{bin,bin64}
cp ${OUTPUT}/mingw32/bin/*.dll Tcl/bin/
cp ${OUTPUT}/mingw64/bin/*.dll Tcl/bin64/
cp -r ${OUTPUT}/mingw32/lib Tcl/
cp -r ${OUTPUT}/mingw64/lib Tcl/bin64/

# Cleanup
rm -Rf ${OUTPUT}
