#!/bin/bash
set -x

if [[ ! -v CALM_VERSION ]]; then
    echo "CALM_VERSION is not set"
    echo "e.g."
    echo "export CALM_VERSION=1.0.0"
    exit 42
fi

curl -o /tmp/calm.zip -L https://github.com/VitoVan/calm/archive/refs/heads/${CALM_VERSION}.zip
unzip calm.zip -d /tmp/calm

rm -rf ./docs
cp -R /tmp/calm/calm-${CALM_VERSION}/docs ./docs
cp /tmp/calm/calm-${CALM_VERSION}/README.md ./
cd docs
sbcl --load md-to-html.lisp
rm README.md

cd ..
git status

git add .
git commit -m "Deploy docs to gh-pages $(date +%s)"
git push --set-upstream origin gh-pages


