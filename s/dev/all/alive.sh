echo "Downloading alive-lsp ..."
if ! [ -d quicklisp/local-projects/alive-lsp ]; then
    git clone --depth 1 --branch v0.2.7 https://github.com/nobody-famous/alive-lsp.git quicklisp/local-projects/alive-lsp
    ./calm sbcl --load ./s/dev/all/load-calm.lisp --load ./s/dev/all/install-alive.lisp
fi
