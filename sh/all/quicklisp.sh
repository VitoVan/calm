echo "Install Quicklisp ..."
if ! [ -d quicklisp ]; then
    if ! [ -f quicklisp.lisp ]; then
        set -x
        curl -o ./quicklisp.lisp -L https://beta.quicklisp.org/quicklisp.lisp
        set +x
    fi
    ./calm sbcl \
           --load quicklisp.lisp \
           --load ./sh/all/install-quicklisp.lisp
fi
