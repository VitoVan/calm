echo "Copy all libraries (.so / .dll / .dylib) ..."
rm -rf ./lib
mkdir ./lib
./calm sbcl --load ./s/dev/all/load-calm.lisp --load ./s/dev/all/copy-lib.lisp
