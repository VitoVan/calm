mkdir calm-dist
cp -R {calm,calm.c,build,entry.lisp,src,lib,sh,quicklisp,README.md,calm.asd,sbcl,images,LICENSE} ./calm-dist/
APP_ICON=./build/calm.icns APP_NAME=Calm APP_VERSION=0.0.15 DIST_DIR=./calm-dist ./calm sh darwin bundle
rm Calm.app/Contents/MacOS/.please_load_calm_canvas_from_here
zip -r -9 calm-macOS.zip Calm.app

cd .github/workflows/
../../calm dist
cd ../../
APP_ICON=./build/app.icns APP_NAME=Hello APP_VERSION=0.0.1 DIST_DIR=./.github/workflows/dist ./calm sh darwin bundle
zip -r -9 calm-app-macOS.zip Hello.app


cd .github/workflows/
../../calm dist-with-canvas
cd ../../
APP_ICON=./build/app.icns APP_NAME=HelloCanvas APP_VERSION=0.0.1 DIST_DIR=./.github/workflows/dist-with-canvas ./calm sh darwin bundle
zip -r -9 calm-app-with-canvas-macOS.zip HelloCanvas.app
