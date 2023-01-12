# APP_ICON=./build/app.icns APP_NAME=Hello APP_VERSION=0.0.1 DIST_DIR=./dist ./calm sh darwin bundle

mkdir -p "$APP_NAME.app/Contents/Resources"

cat > "$APP_NAME.app/Contents/Info.plist" <<END
<?xml version="1.0" encoding="UTF-8"?>
<!DOCTYPE plist PUBLIC "-//Apple Computer//DTD PLIST 1.0//EN" "http://www.apple.com/DTDs/PropertyList-1.0.dtd">
<plist version="1.0">
<dict>
  <key>CFBundleGetInfoString</key>
  <string>$APP_NAME</string>
  <key>CFBundleExecutable</key>
  <string>calm</string>
  <key>CFBundleIdentifier</key>
  <string>com.vitovan.calm</string>
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
END

cp -R "${DIST_DIR}" "${APP_NAME}.app/Contents/MacOS"
chmod +x "${APP_NAME}.app/Contents/MacOS/calm"
cp "${APP_ICON}" "${APP_NAME}.app/Contents/Resources/icon.icns"
touch "${APP_NAME}.app/Contents/MacOS/.please_load_calm_canvas_from_here"
