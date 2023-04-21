######
# compile canvas.lisp to canvas.js
######

if [[ -z "${CALM_HOME}" ]]; then
    echo "CALM_HOME not set."
    exit 42
fi

if [[ -z "${CALM_APP_DIR}" ]]; then
    echo "CALM_APP_DIR not set."
    exit 42
fi

cd ${CALM_HOME}

check_result() {
    if [ $? -eq 0 ]; then
        echo "SEEMS GOOD."
    else
        echo "BAD THING HAPPENED."
        exit 42
    fi
    echo "PREVIOUS DIR: " $(pwd)
}

mkdir -p build/web/

check_result

if [ ! -d "./build/web/jscl/" ]; then
    cd ./build/web/
    git clone https://github.com/jscl-project/jscl.git
    cd ./build/web/jscl/
    git checkout 0c21063a66f5043e6aadbae940a612db6ed0c539
    cd ../../
fi

check_result

sbcl --load ./build/web/jscl/jscl.lisp \
     --eval '(jscl:bootstrap)' \
     --eval '(pushnew :jscl *features*)' \
     --eval '(jscl:compile-application (list "src/web/package.lisp" "src/web/cairo.lisp" "src/c.lisp" "src/calm.lisp"  "src/config.lisp" "src/events.lisp" "build/web/canvas.lisp" "src/web/post.lisp") "build/web/canvas.js")' \
     --eval '(quit)'

check_result

npx uglifyjs ./build/web/jscl/jscl.js -c -m -o ./build/web/jscl.js

# npx google-closure-compiler --js file.js --js_output_file file.out.js

check_result

cp ./build/app.ico ./build/web/favicon.ico
