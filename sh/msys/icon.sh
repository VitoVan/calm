if [[ -z "${CALM_DIR}" ]]; then
    echo "Please set the env: CALM_DIR"
    exit 42
fi

RCEDIT="$CALM_DIR/sh/msys/rcedit-x64.exe"

if [ ! -f "$RCEDIT" ]; then
    curl -o "$RCEDIT" -L https://github.com/electron/rcedit/releases/download/v1.1.1/rcedit-x64.exe
fi

"$RCEDIT" "$CALM_DIR/sbcl/bin/sbcl.exe" --set-icon "$CALM_DIR/build/app.ico"
"$RCEDIT" "$CALM_DIR/calmNoConsole.exe" --set-icon "$CALM_DIR/build/calm.ico"

rm "$RCEDIT"
