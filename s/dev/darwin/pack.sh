# to use the CALM binary in pwd
export PATH=$(pwd):$PATH

if [[ -z "${CI_MATRIX_OS}" ]]; then
    export DMG_SUFIX=""
elif [[ "macos-12" == "${CI_MATRIX_OS}" ]]; then
    export DMG_SUFIX=""
else
    export DMG_SUFIX=".${CI_MATRIX_OS}"
fi

export APP_ICON=./build/calm.icns
export APP_VERSION=0.0.35

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

cd docs/src/examples/panic/

#
# publish
#

export APP_ICON=${CALM_HOME}/build/app.icns
export DMG_ICON=${CALM_HOME}/build/app-dmg.icns
export APP_NAME=Hello
export BUNDLE_ID=com.vitovan.hellocalm
export DMG_NAME="hello${DMG_SUFIX}"

unset CALM_APP_DIR
unset CALM_HOME
../../../../calm publish

mv *.dmg ../../../../Hello${DMG_SUFIX}.dmg
