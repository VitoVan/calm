echo "Install Quicklisp ..."
if ! [ -d quicklisp ]; then
    if ! [ -f quicklisp.lisp ]; then
        curl -o ./quicklisp.lisp -L https://beta.quicklisp.org/quicklisp.lisp
    fi
    ./calm sbcl \
           --load quicklisp.lisp \
           --load ./sh/all/install-quicklisp.lisp
fi
