export CALM_BUILDING=1

env

build_fedora () {
    if ! [ -f calm ]; then
        echo "build launcher ..."
        sudo dnf install gcc -y
        gcc calm.c -o calm
    fi

    ./calm sh fedora deps
    ./calm sh fedora sbcl
    ./calm sh all quicklisp
    ./calm sh all copy-lib
    ./calm sh fedora config-lib
    ./calm sh fedora pack
    echo "DONE"
}

build_darwin () {
    echo "build launcher ..."
    brew install gcc
    gcc calm.c -o calm

    ./calm sh darwin deps
    ./calm sh darwin sbcl
    ./calm sh all quicklisp
    ./calm sh all copy-lib
    ./calm sh darwin config-lib
    ./calm sh darwin pack
    echo "DONE"
}

build_msys () {
    ##        The following should be ran on MSVC Shell
    #        echo "build launcher ..."
    #        cl /Fe:calmGUI calm.c /link /SUBSYSTEM:WINDOWS /ENTRY:mainCRTStartup
    #        cl /Fe:calm calm.c /link /SUBSYSTEM:CONSOLE

    ./calm sh msys deps
    ./calm sh msys sbcl
    ./calm sh all quicklisp
    ./calm sh all copy-lib
    ./calm sh msys config-lib

    echo "setting icons ..."
    RCEDIT="./sh/msys/rcedit.exe"
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
    rm "$RCEDIT"

    echo "packing ..."
    ./calm sh msys pack

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
    ./calm sh all clean
    exit 0
else
    echo "Failed!"
    ./calm sh all clean
    exit 42
fi
