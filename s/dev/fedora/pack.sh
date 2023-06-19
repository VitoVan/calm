# to use the CALM binary in pwd
export PATH=$(pwd):$PATH

mkdir /tmp/calm
cp -R {calm,build,entry.lisp,src,lib,s,quicklisp,README.md,calm.asd,sbcl,LICENSE} /tmp/calm
cd /tmp && GZIP=-9 tar -czvf ./calm.tgz ./calm && cd - && mv /tmp/calm.tgz .

