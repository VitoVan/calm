zip -r -9 ./calm-$(uname | cut -d '_' -f 1).zip build.sh calm calm.exe calm.c build.bat entry.lisp src lib sh quicklisp README.md calm.asd sbcl images LICENSE

cd .github/workflows/

../../calm dist
mv ./dist ./calm-app
zip -r -9 ../../calm-app-$(uname | cut -d '_' -f 1).zip ./calm-app

../../calm dist-with-canvas
mv ./dist-with-canvas ./calm-app-with-canvas
zip -r -9 ../../calm-app-with-canvas-$(uname | cut -d '_' -f 1).zip ./calm-app-with-canvas
