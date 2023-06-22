#!/bin/bash
set -x

if [[ ! -v CALM_EXAMPLE_VERSION ]]; then
    echo "CALM_EXAMPLE_VERSION is not set"
    echo "e.g."
    echo "export CALM_EXAMPLE_VERSION=examples-0.1.1"
    exit 42
fi

rm -rf ./*/${CALM_EXAMPLE_VERSION}

mkdir ${CALM_EXAMPLE_VERSION}
cd ${CALM_EXAMPLE_VERSION}

curl -LO https://github.com/VitoVan/calm/releases/download/${CALM_EXAMPLE_VERSION}/Circles-web.zip
unzip Circles-web.zip
mv web circles

curl -LO https://github.com/VitoVan/calm/releases/download/${CALM_EXAMPLE_VERSION}/Fan-web.zip
unzip Fan-web.zip
mv web fan

curl -LO https://github.com/VitoVan/calm/releases/download/${CALM_EXAMPLE_VERSION}/Meditator-web.zip
unzip Meditator-web.zip
mv web meditator

curl -LO https://github.com/VitoVan/calm/releases/download/${CALM_EXAMPLE_VERSION}/Mondrian-web.zip
unzip Mondrian-web.zip
mv web mondrian

rm *.zip

ls **/*.js | xargs -I _ uglifyjs _ -c -m -o _

cd ..
git status

git add .
git commit -m "Deploy examples to gh-pages $(date +%s)"
git push --set-upstream origin gh-pages
