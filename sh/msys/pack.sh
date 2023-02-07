echo "Setting icons ..."
./calm sh msys icon

zip -r ./calm.zip calm.exe calmNoConsole.exe calm.c entry.lisp src lib sh quicklisp README.md calm.asd sbcl images LICENSE

cd .github/workflows/

../../calm dist
chmod +x ./dist/calm
mv ./dist ./hello
zip -r ../../hello.zip ./hello/

../../calm dist-with-canvas
chmod +x ./dist-with-canvas/calm
mv ./dist-with-canvas ./hello-canvas
zip -r ../../hello-canvas.zip ./hello-canvas/
