#/bin/sh
set -e
OUTPUT=$(mktemp -d)
FILES=$(pacman -Sp mingw-w64-ucrt-x86_64-{tcl,tk,bwidget,tktable} --cache=".")
for FILE in $FILES
do
    curl -OLs $FILE
	echo "    Extracting: $(basename $FILE)"
	tar xf $(basename $FILE) -C ${OUTPUT}
	unlink $(basename $FILE)
done
rm -f $(find ${OUTPUT} -name *.a)

# Copy to 'Tcl' directory
rm -Rf Tcl
mkdir -p Tcl/bin
cp ${OUTPUT}/ucrt64/bin/*.dll Tcl/bin/
cp -r ${OUTPUT}/ucrt64/lib Tcl/

# Cleanup
rm -Rf ${OUTPUT}
