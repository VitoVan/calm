# to use the CALM binary in pwd
export PATH=$(pwd):$PATH

mkdir /tmp/leca
cp -R {lem,calm,build,entry.lisp,src,lib,s,quicklisp,README.md,calm.asd,sbcl,LICENSE} /tmp/leca
cd /tmp && GZIP=-9 tar -czvf ./leca.tgz ./leca && cd - && mv /tmp/leca.tgz .

