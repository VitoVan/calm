echo "Copy libraries with canvas.lisp (.so / .dll / .dylib) ..."
cd ./lib
../calm sbcl \
        --load ./sh/all/load-calm.lisp \
        --load "${APP_DIR}/canvas.lisp" \
        --load ./sh/all/copy-lib.lisp
