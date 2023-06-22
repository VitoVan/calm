#!/bin/bash
set -x

if [[ ! -v CALM_VERSION ]]; then
    echo "CALM_VERSION is not set"
    echo "e.g."
    echo "export CALM_VERSION=1.0.0"
    exit 42
fi

curl -o calm.zip -L https://github.com/VitoVan/calm/archive/refs/heads/${CALM_VERSION}.zip
unzip calm.zip -d /tmp/calm

rm -rf ./doc
cp -R /tmp/calm/doc ./doc
cd doc
sbcl --load md-to-html.lisp

git status

