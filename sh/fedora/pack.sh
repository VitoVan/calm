zip -r -9 ./calm-Linux.zip build calm calm.c entry.lisp src lib sh quicklisp README.md calm.asd sbcl images LICENSE

cd .github/workflows/

../../calm dist
chmod +x ./dist/calm
mv ./dist ./calm-app
zip -r -9 ../../calm-app-Linux.zip ./calm-app

../../calm dist-with-canvas
chmod +x ./dist-with-canvas/calm
mv ./dist-with-canvas ./calm-app-with-canvas
zip -r -9 ../../calm-app-with-canvas-Linux.zip ./calm-app-with-canvas
