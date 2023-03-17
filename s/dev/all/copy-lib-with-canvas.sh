echo "Copy libraries with canvas.lisp (.so / .dll / .dylib) ..."
cd ./lib
../calm sbcl \
        --load ./s/dev/all/load-calm.lisp \
        --load "${CALM_APP_DIR}/canvas.lisp" \
        --load ./s/dev/all/copy-lib.lisp
