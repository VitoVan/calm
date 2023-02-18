#
# "xxx cannot be opened because the developer cannot be verified",
# your macOS user is likely to receive many warnings like this when they received a zipped dist folder of a CALM application.
# Except signing your application (https://support.apple.com/guide/security/app-code-signing-process-sec3ad8e6e53/web)
# you could also bundle all these dylibs and binaries into 1 macOS application,
# by this way, your macos user will only receive 1 warning and can easily get rid of it (if they trust you).
#
# This script is for creating macOS application bundle
# Example usage:
#
# APP_ICON=./build/app.icns APP_NAME=Hello APP_VERSION=0.0.1 DIST_DIR=./dist ./calm sh darwin bundle
#

if [[ -z "${APP_NAME}" ]]; then
    echo "Please set the env: APP_NAME"
    exit 42
fi

if [[ -z "${APP_ID}" ]]; then
    echo "Please set the env: APP_ID, e.g. com.jack.coolapp"
    exit 42
fi

if [[ -z "${APP_VERSION}" ]]; then
    echo "Please set the env: APP_VERSION"
    exit 42
fi

if [[ -z "${APP_ICON}" ]]; then
    echo "Please set the env: APP_ICON"
    exit 42
fi

if [[ -z "${DIST_DIR}" ]]; then
    echo "Please set the env: DIST_DIR"
    exit 42
fi


cd "$APP_DIR"

#
# https://notes.alinpanaitiu.com/Making%20macOS%20apps%20uninstallable
# the following trick makes the app "uninstallable"
#
mkdir "$APP_NAME.app/Contents/_MASReceipt"
touch "$APP_NAME.app/Contents/_MASReceipt/receipt"

mkdir -p "$APP_NAME.app/Contents/Resources"

cat > "$APP_NAME.app/Contents/Info.plist" <<EOF
<?xml version="1.0" encoding="UTF-8"?>
<!DOCTYPE plist PUBLIC "-//Apple Computer//DTD PLIST 1.0//EN" "http://www.apple.com/DTDs/PropertyList-1.0.dtd">
<plist version="1.0">
<dict>
  <key>CFBundleGetInfoString</key>
  <string>$APP_NAME</string>
  <key>CFBundleExecutable</key>
  <string>calm</string>
  <key>CFBundleIdentifier</key>
  <string>$APP_ID</string>
  <key>CFBundleName</key>
  <string>$APP_NAME</string>
  <key>CFBundleIconFile</key>
  <string>icon.icns</string>
  <key>CFBundleShortVersionString</key>
  <string>$APP_VERSION</string>
  <key>CFBundleInfoDictionaryVersion</key>
  <string>6.0</string>
  <key>CFBundlePackageType</key>
  <string>APPL</string>
  <key>IFMajorVersion</key>
  <integer>0</integer>
  <key>IFMinorVersion</key>
  <integer>1</integer>
  <key>NSHighResolutionCapable</key><true/>
  <key>NSSupportsAutomaticGraphicsSwitching</key><true/>
</dict>
</plist>
EOF

cp -R "${DIST_DIR}" "${APP_NAME}.app/Contents/MacOS"
chmod +x "${APP_NAME}.app/Contents/MacOS/calm"
cp "${APP_ICON}" "${APP_NAME}.app/Contents/Resources/icon.icns"
touch "${APP_NAME}.app/Contents/MacOS/.please_load_calm_canvas_from_here"
