export CALM_BUILDING=1

build_fedora () {
    echo "build launcher ..."
    sudo dnf install gcc -y
    gcc calm.c -o calm

    ./calm sh fedora deps
    ./calm sh fedora sbcl
    ./calm sh all quicklisp
    ./calm sh all copy-lib
    ./calm sh fedora config-lib
    ./calm sh all pack
    ./calm sh all clean
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
    ./calm sh all pack
    ./calm sh all clean
    echo "DONE"
}

build_msys () {
    ##        The following should be ran on MSVC Shell
    #        echo "build launcher ..."
    #        cl /Fe:calmGUI calm.c /link /SUBSYSTEM:WINDOWS /ENTRY:mainCRTStartup
    #        cl /Fe:calm calm.c /link /SUBSYSTEM:CONSOLE

    ./calm.exe sh msys deps
    ./calm.exe sh msys sbcl
    ./calm.exe sh all quicklisp
    ./calm.exe sh all copy-lib
    ./calm.exe sh msys config-lib
    ./calm.exe sh all pack
    ./calm.exe sh all clean
    echo "DONE"
}

if [[ "$OSTYPE" == "linux-gnu"* ]]; then
    export DISTRO="$(awk -F= '/^NAME/{print $2}' /etc/os-release | sed 's/"//g')"
    if [[ "$DISTRO" == "Fedora"* ]]; then
        build_fedora
    else
        echo "Sorry, I only managed to build this on Fedora"
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
