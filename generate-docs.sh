#!/bin/bash
set -x

if [[ ! -v CALM_VERSION ]]; then
    echo "CALM_VERSION is not set"
    echo "e.g."
    echo "export CALM_VERSION=1.0.0"
    exit 42
fi

rm /tmp/calm.zip
rm -rf /tmp/calm

git clone --single-branch --depth 1 --branch ${CALM_VERSION} https://github.com/VitoVan/calm.git /tmp/calm

rm -rf ./docs
cp -R /tmp/calm/docs ./docs

cp /tmp/calm/build/calm.ico ./favicon.ico

cp /tmp/calm/README.md ./
cp /tmp/calm/README_JA.md ./

sbcl --non-interactive --load md-to-html.lisp

rm README.md
rm README_JA.md

git status

git add .
git commit -m "Deploy docs for ${CALM_VERSION} to gh-pages $(date +%s)"
git push --set-upstream origin gh-pages
