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
cp -R {calm,build,entry.lisp,src,lib,s,quicklisp,README.md,calm.asd,sbcl,LICENSE} ./calm-dist/
export APP_NAME=Calm
export BUNDLE_ID=com.vitovan.calm
export DIST_DIR=./calm-dist/

./calm make-bundle

export DMG_ICON=./build/calm-dmg.icns
./calm make-dmg

mv Calm.dmg calm${DMG_SUFIX}.dmg
