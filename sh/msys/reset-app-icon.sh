RCEDIT="./sh/msys/rcedit.exe"
if [ ! -f "$RCEDIT" ]; then
    echo "RCEDIT is not ready, downloading ..."
    set -x
    curl -o "$RCEDIT" -L https://github.com/electron/rcedit/releases/download/v1.1.1/rcedit-x64.exe
    set +x
fi

"$RCEDIT" "./sbcl/bin/sbcl.exe" --set-icon "./build/app.ico"
"$RCEDIT" "./calmNoConsole.exe" --set-icon "./build/app.ico"

