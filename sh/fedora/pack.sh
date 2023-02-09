tar -czvf ./calm.tgz build calm calm.c entry.lisp src lib sh quicklisp README.md calm.asd sbcl images LICENSE

cd .github/workflows/

../../calm dist
chmod +x ./dist/calm
mv ./dist ./hello
tar -czvf ../../hello.tgz ./hello/

../../calm dist-with-canvas
chmod +x ./dist-with-canvas/calm
mv ./dist-with-canvas ./hello-canvas
tar -czvf ../../hello-canvas.tgz ./hello-canvas/
