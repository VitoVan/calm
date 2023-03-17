# to use the CALM binary in pwd
export PATH=$(pwd):$PATH

tar -czvf ./calm.tgz build calm entry.lisp src lib s quicklisp README.md calm.asd sbcl images LICENSE

#
# sample application
#

cd docs/examples/circles/

#
# publish
#

export APP_NAME=Hello
export APP_ICON="${CALM_HOME}/build/app.png"

unset CALM_APP_DIR
unset CALM_HOME
../../../calm publish

mv ./*.AppImage ../../../
