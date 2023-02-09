export APP_ICON=./build/app.icns
export APP_VERSION=0.0.19

mkdir calm-dist
cp -R {calm,calm.c,build,entry.lisp,src,lib,sh,quicklisp,README.md,calm.asd,sbcl,images,LICENSE} ./calm-dist/
export APP_NAME=Calm
export DMG_NAME=calm
export DIST_DIR=./calm-dist
./calm sh darwin bundle
rm Calm.app/Contents/MacOS/.please_load_calm_canvas_from_here
./calm sh darwin dmg

cd .github/workflows/
../../calm dist
cd ../../
export APP_NAME=Hello
export DMG_NAME=hello
export DIST_DIR=./.github/workflows/dist
./calm sh darwin bundle
./calm sh darwin dmg

cd .github/workflows/
../../calm dist-with-canvas
cd ../../
export APP_NAME="Hello Canvas"
export DMG_NAME=hello-canvas
export DIST_DIR=./.github/workflows/dist-with-canvas
./calm sh darwin bundle
./calm sh darwin dmg
