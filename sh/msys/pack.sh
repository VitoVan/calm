zip -r -9 ./calm-Windows.zip build.sh calm calm.exe calmConsole.exe calm.c build.bat entry.lisp src lib sh quicklisp README.md calm.asd sbcl images LICENSE

cd .github/workflows/

../../calm dist
chmod +x ./dist/calm
mv ./dist ./calm-app
zip -r -9 ../../calm-app-Windows.zip ./calm-app

../../calm dist-with-canvas
chmod +x ./dist-with-canvas/calm
mv ./dist-with-canvas ./calm-app-with-canvas
zip -r -9 ../../calm-app-with-canvas-Windows.zip ./calm-app-with-canvas
