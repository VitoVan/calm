# to use the CALM binary in pwd
export PATH=$(pwd):$PATH

zip -r ./calm.zip calm.exe calmNoConsole.exe entry.lisp build src lib s quicklisp README.md calm.asd sbcl images LICENSE

#
# sample application
#

cd docs/examples/circles/

#
# this is needed if the current working directory has been changed
# while building new APP,
# otherwise CALM will inherit the previous CALM_APP_DIR
# instead of using the new `uiop:getcwd'
#
unset CALM_APP_DIR

#
# dist-with-canvas
#

../../../calm dist-with-canvas

tar -czvf ./dist-with-canvas.tgz dist-with-canvas

mv ./dist-with-canvas.tgz ../../../dist-with-canvas.windows.tgz

#
# publish
#

# default installation path of NSIS (makensis)
export PATH=$PATH:"/c/Program Files (x86)/NSIS/"

export APP_ICON=${CALM_HOME}/build/app.ico
export APP_NAME=Hello

../../../calm publish

mv ./*-Installer.exe ../../../Hello.exe
