echo "Copy all libraries (.so / .dll / .dylib) ..."
rm -rf ./lib
mkdir ./lib
./calm sbcl --load ./sh/all/load-calm.lisp --load ./sh/all/copy-lib.lisp
