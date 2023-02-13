tar -czvf ./calm.tgz build calm calm.c entry.lisp src lib sh quicklisp README.md calm.asd sbcl images LICENSE

cd .github/workflows/

../../calm dist
chmod +x ./dist/calm
export DIST_DIR=./dist
export APP_NAME=Hello
export APP_ICON="${CALM_DIR}/build/app.png"
../../calm sh fedora appimage
mv ./*.AppImage ../../

../../calm dist-with-canvas
chmod +x ./dist-with-canvas/calm
mv ./dist-with-canvas ./hello-canvas
tar -czvf ../../hello-canvas.tgz ./hello-canvas/
