if [[ -z "${APP_NAME}" ]]; then
    echo "Please set the env: APP_NAME"
    exit 42
fi

if [[ -z "${DMG_NAME}" ]]; then
    echo "Please set the env: DMG_NAME"
    exit 42
fi

if [[ -z "${DMG_ICON}" ]]; then
    echo "Please set the env: DMG_ICON"
    exit 42
fi

if ! command -v create-dmg &> /dev/null; then
    echo "create-dmg is not ready, downloading ..."
    brew install create-dmg
fi

cd "$APP_DIR"

create-dmg --hdiutil-verbose --volname "$APP_NAME - CALM" \
           --volicon "$DMG_ICON" \
           --window-pos 200 120 \
           --window-size 800 280 \
           --icon-size 100 \
           --icon "$APP_NAME.app" 200 90 \
           --hide-extension "$APP_NAME.app" \
           --app-drop-link 600 85 \
           "$DMG_NAME.dmg" "$APP_NAME.app/"
