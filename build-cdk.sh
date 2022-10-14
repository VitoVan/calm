#!/bin/bash

echo "Building macOS CDK ..."

if [ ! -f "./install_root-macOS.zip" ]; then
    curl -OL https://github.com/VitoVan/sbcl-with-compression/releases/download/test-2.2.9-09/install_root-macOS.zip
fi

unzip install_root-macOS.zip

mv ./install_root ./cdk

curl -o ./cdk/quicklisp.lisp -L https://beta.quicklisp.org/quicklisp.lisp
./sbcl --load ./cdk/quicklisp.lisp --eval '(quicklisp-quickstart:install :proxy "http://127.0.0.1:1087/" :path "./cdk/quicklisp/")' --eval '(quit)'

echo "Building launcher"
./sbcl --load launcher.lisp

echo "Starting CALM"
./calm

echo "Building Windows CDK ..."

echo "Building Linux CDK ..."
