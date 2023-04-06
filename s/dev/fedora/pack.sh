# to use the CALM binary in pwd
export PATH=$(pwd):$PATH

tar -czvf ./calm.tgz build calm entry.lisp src lib s quicklisp README.md calm.asd sbcl images LICENSE

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

mv ./dist-with-canvas.tgz ../../../dist-with-canvas.linux.tgz

#
# publish
#

export APP_NAME=Hello
export APP_ICON="${CALM_HOME}/build/app.png"

../../../calm publish

mv ./*.AppImage ../../../
