#!/bin/bash
#
#   This file is licensed under the MIT License,
#   albeit it is included in the project CALM.
#
#   Some code of this file is copied from `macappshell`:
#   https://github.com/Xeoncross/macappshell
#
#   MIT License
#
#   Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:
#
#   The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.
#
#   THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
#
#

# debugging
if [ -n "$DEBUGGING" ]; then
    set -x
    env
fi


DEFAULT_DIR=$(pwd)

# Mac OSX .app builder

function die {
        echo "ERROR: $1" > /dev/null 1>&2
        exit 1
}

if [ "$#" -ne 2 ]; then
        die "Usage: `basename $0` AppNameHere icon-file.svg"
fi

APPNAME=$1
ICONNAME=$2

if [ ! -f $ICONNAME ]; then
        die "Image file for icon not found"
fi

mkdir -p "$APPNAME.app/Contents/"{MacOS,Resources}

cat > "$APPNAME.app/Contents/Info.plist" <<END
<?xml version="1.0" encoding="UTF-8"?>
<!DOCTYPE plist PUBLIC "-//Apple Computer//DTD PLIST 1.0//EN" "http://www.apple.com/DTDs/PropertyList-1.0.dtd">
<plist version="1.0">
<dict>
  <key>CFBundleGetInfoString</key>
  <string>$APPNAME</string>
  <key>CFBundleExecutable</key>
  <string>calm</string>
  <key>CFBundleIdentifier</key>
  <string>com.example.www</string>
  <key>CFBundleName</key>
  <string>$APPNAME</string>
  <key>CFBundleIconFile</key>
  <string>icon.icns</string>
  <key>CFBundleShortVersionString</key>
  <string>0.01</string>
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

cp $ICONNAME "$APPNAME.app/Contents/Resources/"
cd "$APPNAME.app/Contents/Resources/"

fileName="$(basename $ICONNAME)"
postfix=${fileName##*.}

if [[ $postfix == 'icns' ]]; then
    mv $fileName "icon.icns"
elif [[ $postfix == 'svg' ]]; then
    qlmanage -z -t -s 1024 -o ./ "$fileName"
    fileName=${fileName}.png
else
    echo $fileName

    mkdir icon.iconset

    sips -z 16 16 "$fileName" --out icon.iconset/icon_16x16.png
    sips -z 32 32 "$fileName" --out icon.iconset/icon_16x16@2x.png
    cp icon.iconset/icon_16x16@2x.png icon.iconset/icon_32x32.png
    sips -z 64 64 "$fileName" --out icon.iconset/icon_32x32@2x.png
    sips -z 128 128 "$fileName" --out icon.iconset/icon_128x128.png
    sips -z 256 256 "$fileName" --out icon.iconset/icon_128x128@2x.png
    cp icon.iconset/icon_128x128@2x.png icon.iconset/icon_256x256.png
    sips -z 512 512 "$fileName" --out icon.iconset/icon_256x256@2x.png
    cp icon.iconset/icon_256x256@2x.png icon.iconset/icon_512x512.png
    sips -z 1024 1024 "$fileName" --out icon.iconset/icon_512x512@2x.png

    # Create .icns file
    iconutil -c icns icon.iconset

    # Cleanup
    rm -R icon.iconset
    rm $fileName
fi


cd "$DEFAULT_DIR"

cp -r ./dist/* "$APPNAME.app/Contents/MacOS/"

if [ ! -d "./resources" ]; then
    echo "Directory 'resources' does not exist ... skipping ..."
else
    mkdir "$APPNAME.app/Contents/MacOS/resources/"
    cp -r resources/* "$APPNAME.app/Contents/MacOS/resources/"
fi

if ! command -v create-dmg &> /dev/null; then
    echo "create-dmg is not ready, downloading ..."
    brew install create-dmg
fi

rm "$APPNAME.dmg"

create-dmg --hdiutil-verbose --volname "CALM Application" \
           --volicon "$CALM_DIR/scripts/calm-cartridge.icns" \
           --window-pos 200 120 \
           --window-size 800 280 \
           --icon-size 100 \
           --icon "$APPNAME.app" 200 90 \
           --hide-extension "$APPNAME.app" \
           --app-drop-link 600 85 \
           "$APPNAME.dmg" "$APPNAME.app/"

echo "DONE."
