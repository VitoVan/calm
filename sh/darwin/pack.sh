if [[ -z "${CI_MATRIX_OS}" ]]; then
    export DMG_SUFIX=""
elif [[ "macos-12" == "${CI_MATRIX_OS}" ]]; then
    export DMG_SUFIX=""
else
    export DMG_SUFIX="-${CI_MATRIX_OS}"
fi

export APP_ICON=./build/calm.icns
export APP_VERSION=0.0.20

mkdir calm-dist
cp -R {calm,calm.c,build,entry.lisp,src,lib,sh,quicklisp,README.md,calm.asd,sbcl,images,LICENSE} ./calm-dist/
export APP_NAME=Calm
export DIST_DIR=./calm-dist
./calm sh darwin bundle
rm Calm.app/Contents/MacOS/.please_load_calm_canvas_from_here
export DMG_NAME="calm${DMG_SUFIX}"
export DMG_ICON=./build/calm-dmg.icns
./calm sh darwin dmg

export APP_ICON=./build/app.icns
export DMG_ICON=./build/app-dmg.icns

cd .github/workflows/
../../calm dist
cd ../../
export APP_NAME=Hello
export DIST_DIR=./.github/workflows/dist
./calm sh darwin bundle
export DMG_NAME="hello${DMG_SUFIX}$"
./calm sh darwin dmg

cd .github/workflows/
../../calm dist-with-canvas
cd ../../
export APP_NAME="Hello Canvas"
export DIST_DIR=./.github/workflows/dist-with-canvas
./calm sh darwin bundle
export DMG_NAME="hello-canvas${DMG_SUFIX}"
./calm sh darwin dmg
