#!/bin/bash

if [ ! -f "./calm.asd" ]; then
    echo "This file should be ran in the root directory of CALM."
    echo "sh ./scripts/build-cdk.sh"
    exit 42
fi

echo "Building macOS CDK ..."

echo "Downloading pre-built SBCL (--with-fancy --with-compression) ..."

if [ ! -f "./install_root-macOS.zip" ]; then
    curl -OL https://github.com/VitoVan/sbcl-with-compression/releases/download/test-2.2.9-09/install_root-macOS.zip
fi

unzip install_root-macOS.zip

mv ./install_root ./cdk

echo "Installing Quicklisp ..."

curl -o ./cdk/quicklisp.lisp -L https://beta.quicklisp.org/quicklisp.lisp
./sbcl --load ./cdk/quicklisp.lisp --eval '(quicklisp-quickstart:install :proxy "http://127.0.0.1:1087/" :path "./cdk/quicklisp/")' --eval '(quit)'

echo "Building launcher ..."
./sbcl --load launcher.lisp

echo "Starting CALM"
CI=true ./calm

echo "DONE."

echo "Building Windows CDK ..."

echo "Building Linux CDK ..."
