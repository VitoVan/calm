export CALM_BUILDING=1

env

build_fedora () {
    if ! [ -f calm ]; then
        echo "build launcher ..."
        sudo dnf install gcc -y
        gcc src/calm.c -o calm
    fi

    ./calm s dev fedora deps.sh
    ./calm s dev fedora sbcl.sh
    ./calm s dev all quicklisp.sh
    ./calm s dev all copy-lib.sh
    ./calm s dev fedora config-lib.sh

    # prepare appimage-tool
    APPIMAGETOOL="./s/usr/linux/appimagetool.AppImage"
    if [ ! -f "$APPIMAGETOOL" ]; then
        set -x
        curl -o "$APPIMAGETOOL" -L https://github.com/AppImage/AppImageKit/releases/download/13/appimagetool-x86_64.AppImage
        chmod +x "$APPIMAGETOOL"
        set +x
    fi

    ./calm s dev fedora pack.sh
    echo "DONE"
}

build_darwin () {
    echo "build launcher ..."
    brew install gcc
    gcc src/calm.c -o calm

    ./calm s dev darwin deps.sh
    ./calm s dev darwin sbcl.sh
    ./calm s dev all quicklisp.sh
    ./calm s dev all copy-lib.sh
    ./calm s dev darwin config-lib.sh
    ./calm s dev darwin pack.sh
    echo "DONE"
}

build_msys () {
    ##        The following should be ran on MSVC Shell
    #        echo "build launcher ..."
    #        cl /Fe:calmGUI src\calm.c /link /SUBSYSTEM:WINDOWS /ENTRY:mainCRTStartup
    #        cl /Fe:calm src\calm.c /link /SUBSYSTEM:CONSOLE

    ./calm s dev msys deps.sh
    ./calm s dev msys sbcl.sh
    ./calm s dev all quicklisp.sh
    ./calm s dev all copy-lib.sh
    ./calm s dev msys config-lib.sh

    echo "setting icons ..."
    RCEDIT="./s/usr/windows/rcedit.exe"
    if [ ! -f "$RCEDIT" ]; then
        set -x
        curl -o "$RCEDIT" -L https://github.com/electron/rcedit/releases/download/v1.1.1/rcedit-x64.exe
        set +x
    fi
    "$RCEDIT" "./sbcl/bin/sbcl.exe" --set-icon "./build/app.ico"
    #
    # calmNoConsole.exe will be copied into dist folder,
    # so let's make its icon the same as the app icon
    #
    "$RCEDIT" "./calmNoConsole.exe" --set-icon "./build/app.ico"
    "$RCEDIT" "./calm.exe" --set-icon "./build/calm.ico"

    echo "packing ..."
    ./calm s dev msys pack.sh

}

if [[ "$OSTYPE" == "linux-gnu"* ]]; then
    export DISTRO="$(awk -F= '/^NAME/{print $2}' /etc/os-release | sed 's/"//g')"
    if [[ "$DISTRO" == "Fedora"* ]]; then
        build_fedora
    else
        echo "Sorry, I only managed to build this on Fedora."
        echo "If you don't mind, docker could fulfil this task:"
        echo "    docker run -v $PWD:/calm -w /calm fedora bash build/build.sh"
        exit 42
    fi
elif [[ "$OSTYPE" == "darwin"* ]]; then
    build_darwin
elif [[ "$OSTYPE" == "msys" ]]; then
    build_msys
else
    echo "Unsupported platform, please try Fedora Linux / macOS / MSYS2"
    exit 42
fi

if ./calm test; then
    echo "DONE."
#    ./calm s dev all clean.sh
    exit 0
else
    echo "Failed!"
#    ./calm s dev all clean.sh
    exit 42
fi
