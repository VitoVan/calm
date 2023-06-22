# to use the CALM binary in pwd
export PATH=$(pwd):$PATH

mkdir /tmp/calm
cp -R {calm.exe,calmNoConsole.exe,entry.lisp,build,src,lib,s,quicklisp,README.md,calm.asd,sbcl,images,LICENSE} /tmp/calm
cd /tmp && zip -r -9 ./calm.zip ./calm && cd - && mv /tmp/calm.zip .
