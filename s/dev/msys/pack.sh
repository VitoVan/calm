# to use the CALM binary in pwd
export PATH=$(pwd):$PATH

zip -r ./calm.zip calm.exe calmNoConsole.exe entry.lisp build src lib s quicklisp README.md calm.asd sbcl images LICENSE

