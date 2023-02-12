zip -r ./calm.zip calm.exe calmNoConsole.exe calm.c entry.lisp build src lib sh quicklisp README.md calm.asd sbcl images LICENSE

cd .github/workflows/

../../calm dist
chmod +x ./dist/calm
export DIST_DIR=./dist
export APP_NAME=Hello
../../calm sh msys installer
mv ./*-Installer.exe ../../

../../calm dist-with-canvas
chmod +x ./dist-with-canvas/calm
mv ./dist-with-canvas ./calm-dist
zip -r ../../hello-canvas.zip ./calm-dist/
