# to use the CALM binary in pwd
export PATH=$(pwd):$PATH

mkdir /tmp/leca
cp -R {lem.exe,calm.exe,calmNoConsole.exe,entry.lisp,build,src,lib,s,quicklisp,README.md,calm.asd,sbcl,images,LICENSE} /tmp/leca
cd /tmp && zip -r -9 ./leca.zip ./leca && cd - && mv /tmp/leca.zip .
