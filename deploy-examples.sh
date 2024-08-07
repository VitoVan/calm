#!/bin/bash
set -x

if [[ ! -v CALM_VERSION ]]; then
    echo "CALM_VERSION is not set"
    echo "e.g."
    echo "export CALM_VERSION=examples-0.1.1"
    exit 42
fi

rm -rf ./*/${CALM_VERSION}

mkdir ${CALM_VERSION}
cd ${CALM_VERSION}

curl -LO https://github.com/VitoVan/calm/releases/download/${CALM_VERSION}/Circles-web.zip
unzip Circles-web.zip
mv web circles

curl -LO https://github.com/VitoVan/calm/releases/download/${CALM_VERSION}/Fan-web.zip
unzip Fan-web.zip
mv web fan

curl -LO https://github.com/VitoVan/calm/releases/download/${CALM_VERSION}/Meditator-web.zip
unzip Meditator-web.zip
mv web meditator

curl -LO https://github.com/VitoVan/calm/releases/download/${CALM_VERSION}/Mondrian-web.zip
unzip Mondrian-web.zip
mv web mondrian

rm *.zip

ls **/*.js | xargs -I _ uglifyjs _ -c -m -o _

cd ..
git status

git diff

read -p "Do you want to proceed? (yes/no) " yn

case $yn in
        yes ) echo ok, we will proceed;;
        no ) echo exiting...;
                exit;;
        * ) echo invalid response;
                exit 1;;
esac

git add .
git commit -m "Deploy examples for ${CALM_VERSION} to gh-pages $(date +%s)"
git push --set-upstream origin gh-pages