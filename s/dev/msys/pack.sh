# to use the CALM binary in pwd
export PATH=$(pwd):$PATH

zip -r ./calm.zip calm.exe calmNoConsole.exe entry.lisp build src lib s quicklisp README.md calm.asd sbcl images LICENSE

#
# sample application
#

cd docs/src/examples/panic/

#
# publish
#

# default installation path of NSIS (makensis)
export PATH=$PATH:"/c/Program Files (x86)/NSIS/"

export APP_ICON=${CALM_HOME}/build/app.ico
export APP_NAME=Hello

unset CALM_APP_DIR
unset CALM_HOME
../../../../calm publish

mv ./*-Installer.exe ../../../../Hello.exe
