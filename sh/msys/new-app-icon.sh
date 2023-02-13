if [[ -z "${APP_ICON}" ]]; then
    echo "Please set the env: APP_ICON"
    exit 42
fi

RCEDIT="${CALM_DIR}/sh/msys/rcedit.exe"
if [ ! -f "$RCEDIT" ]; then
    echo "RCEDIT is not ready, downloading ..."
    set -x
    curl -o "$RCEDIT" -L https://github.com/electron/rcedit/releases/download/v1.1.1/rcedit-x64.exe
    set +x
fi

cd "${APP_DIR}"

"$RCEDIT" "${CALM_DIR}/sbcl/bin/sbcl.exe" --set-icon "${APP_ICON}"
"$RCEDIT" "${CALM_DIR}/calmNoConsole.exe" --set-icon "${APP_ICON}"
