# to use the CALM binary in pwd
export PATH=$(pwd):$PATH

if [[ -z "${CI_MATRIX_OS}" ]]; then
    export DMG_SUFIX=""
else
    export DMG_SUFIX=".${CI_MATRIX_OS}"
fi

export APP_ICON=./build/calm.icns
export APP_VERSION=$(grep :version calm.asd | awk -F \" '{print $2}')

mkdir calm-dist
cp -R {calm,build,entry.lisp,src,lib,s,quicklisp,README.md,calm.asd,sbcl,images,LICENSE} ./calm-dist/
export APP_NAME=Calm
export BUNDLE_ID=com.vitovan.calm
export DIST_DIR=./calm-dist/

./calm make-bundle
# remove macos bundle for CALM itself
# to be able to load canvas.lisp outside of CALM_HOME
rm Calm.app/Contents/MacOS/.calm-app-macos-bundle

export DMG_ICON=./build/calm-dmg.icns
./calm make-dmg

mv Calm.dmg calm${DMG_SUFIX}.dmg

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

export DIST_DIR=./dist-with-canvas/
export APP_NAME=Hello
../../../calm make-bundle

tar -czvf ./dist-with-canvas.tgz Hello.app

mv ./dist-with-canvas.tgz ../../../dist-with-canvas${DMG_SUFIX}.tgz

#
# publish
#

export APP_ICON=${CALM_HOME}/build/app.icns
export DMG_ICON=${CALM_HOME}/build/app-dmg.icns
export APP_NAME=Hello
export BUNDLE_ID=com.vitovan.hellocalm
export DMG_NAME="hello${DMG_SUFIX}"

../../../calm publish

mv *.dmg ../../../Hello${DMG_SUFIX}.dmg
