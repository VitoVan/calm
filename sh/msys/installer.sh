set -x

cd "$APP_DIR"

if [[ -z "${APP_NAME}" ]]; then
    echo "Please set the env: APP_NAME"
    exit 42
fi

if [[ -z "${DIST_DIR}" ]]; then
    echo "Please set the env: DIST_DIR"
    exit 42
else
    rm -rf calm-install-root
    cp -r "${DIST_DIR}" calm-install-root
fi

export PATH=$PATH:"/c/Program Files (x86)/NSIS/"

if ! command -v makensis &> /dev/null; then
    echo "makensis is not ready, please install NSIS and set your PATH env"
fi

mkdir -p calm-install-root-assets

echo "generate config ..."
sed "s/__APP_NAME__/${APP_NAME}/g" "$CALM_DIR/sh/msys/installer.nsi" > ./installer.nsi

echo "copy resources ..."
if [[ -z "${INSTALLER_ASSETS_DIR}" ]]; then
    echo "using default installer assets (ico, bmp) ..."
    cp "$CALM_DIR/build/app-installer.ico" ./calm-install-root-assets/
    cp "$CALM_DIR/build/app-uninstaller.ico" ./calm-install-root-assets/
    cp "$CALM_DIR/build/installer-header.bmp" ./calm-install-root-assets/
    cp "$CALM_DIR/build/installer-page.bmp" ./calm-install-root-assets/
else
    echo "using custom installer assets (ico, bmp) ..."
    cp "${INSTALLER_ASSETS_DIR}/app-installer.ico" ./calm-install-root-assets/
    cp "${INSTALLER_ASSETS_DIR}/app-uninstaller.ico" ./calm-install-root-assets/
    cp "${INSTALLER_ASSETS_DIR}/installer-header.bmp" ./calm-install-root-assets/
    cp "${INSTALLER_ASSETS_DIR}/installer-page.bmp" ./calm-install-root-assets/
fi

echo "make installer ..."
makensis -V4 installer.nsi

echo "clean up ..."
rm ./installer.nsi
rm -r calm-install-root
rm -r calm-install-root-assets
