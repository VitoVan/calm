# to use the CALM binary in pwd
export PATH=$(pwd):$PATH

tar -czvf ./calm.tgz build calm entry.lisp src lib s quicklisp README.md calm.asd sbcl images LICENSE

