if [[ -z "${DIST_DIR}" ]]; then
    echo "Please set the env: DIST_DIR"
    exit 42
fi

if [[ -z "${APP_NAME}" ]]; then
    echo "Please set the env: APP_NAME"
    exit 42
fi

if [[ -z "${APP_ICON}" ]]; then
    echo "Please set the env: APP_ICON"
    exit 42
fi

cd "${APP_DIR}"

APP_IMAGE_TOOL="${CALM_DIR}/sh/fedora/appimagetool"
APP_IMAGE_APPRUN="${CALM_DIR}/sh/fedora/AppRun"

if [ ! -f "${APP_IMAGE_TOOL}" ]; then
    echo "appimagetool is not ready, downloading ..."
    set -x
    curl -o "${APP_IMAGE_TOOL}" -L https://github.com/AppImage/AppImageKit/releases/download/13/appimagetool-x86_64.AppImage
    chmod +x "${APP_IMAGE_TOOL}"
    chmod +x "${APP_IMAGE_APPRUN}"
    set +x
fi

env
set -x

export LINUX_APP_DIR="${APP_NAME}.AppDir"
mkdir -p ${LINUX_APP_DIR}

cp "${APP_IMAGE_APPRUN}" ${LINUX_APP_DIR}/
cp "${APP_ICON}" ${LINUX_APP_DIR}/icon.png

cat > "${LINUX_APP_DIR}/${APP_NAME}.desktop" <<EOF
[Desktop Entry]
Name=${APP_NAME}
Exec=calm
Icon=icon
Type=Application
Categories=Utility;
EOF

mkdir -p ${LINUX_APP_DIR}/usr/bin

# default app window icon
mkdir -p ${DIST_DIR}/build
cp ${CALM_DIR}/build/app.png ${DIST_DIR}/build/

cp -r ${DIST_DIR}/* $LINUX_APP_DIR/usr/bin/


if [[ -z "$CI" ]]; then
    "${APP_IMAGE_TOOL}" "${LINUX_APP_DIR}" "${APP_NAME}".AppImage
else
    # inside docker
    "${APP_IMAGE_TOOL}" --appimage-extract-and-run "${LINUX_APP_DIR}" "${APP_NAME}".AppImage
fi



rm -r ${LINUX_APP_DIR}
