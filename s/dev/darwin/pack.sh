# to use the CALM binary in pwd
export PATH=$(pwd):$PATH

if [[ -z "${CI_MATRIX_OS}" ]]; then
    export DMG_SUFIX=""
else
    export DMG_SUFIX=".${CI_MATRIX_OS}"
fi

export APP_ICON=./build/calm.icns
export APP_VERSION=$(grep :version calm.asd | awk -F \" '{print $2}')

mkdir calm-dist
cp -R {lem,calm,build,entry.lisp,src,lib,s,quicklisp,README.md,calm.asd,sbcl,LICENSE} ./calm-dist/
export APP_NAME=Leca
export BUNDLE_ID=com.vitovan.leca
export DIST_DIR=./calm-dist/

./calm make-bundle

sed -i '' 's|<string>calm</string>|<string>lem</string>|' Leca.app/Contents/Info.plist

export DMG_ICON=./build/calm-dmg.icns
./calm make-dmg

mv Leca.dmg leca${DMG_SUFIX}.dmg
