echo "Install Quicklisp ..."
if ! [ -d quicklisp ]; then
    if ! [ -f quicklisp.lisp ]; then
        set -x
        curl -o ./quicklisp.lisp -L https://beta.quicklisp.org/quicklisp.lisp
        set +x
    fi
    ./calm sbcl \
           --load quicklisp.lisp \
           --load ./s/dev/all/install-quicklisp.lisp
    cd quicklisp/local-projects/
    git clone --depth 1 --recursive https://github.com/lem-project/lem.git
    cd lem && git reset --hard 2b9b464de73e24cff9d0c34cceb9635c2eaed7c6 && cd ..
    git clone --depth 1 https://github.com/sharplispers/log4cl.git
    cd log4cl && git reset --hard fe3da517147d023029782ced7cd989ba24f1e62d && cd ..
fi
