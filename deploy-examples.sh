# Delete the ref to avoid keeping history.
git update-ref -d refs/heads/gh-pages
rm -rf ./*/

curl -LO https://github.com/VitoVan/calm/releases/download/examples-0.0.42/Circles-web.zip
unzip Circles-web.zip
mv web circles

curl -LO https://github.com/VitoVan/calm/releases/download/examples-0.0.42/Fan-web.zip
unzip Fan-web.zip
mv web fan

curl -LO https://github.com/VitoVan/calm/releases/download/examples-0.0.42/Meditator-web.zip
unzip Meditator-web.zip
mv web meditator

curl -LO https://github.com/VitoVan/calm/releases/download/examples-0.0.42/Mondrian-web.zip
unzip Mondrian-web.zip
mv web mondrian

rm *.zip

git add .
git commit -m "Deploy examples to gh-pages"
git push --force --set-upstream origin gh-pages

