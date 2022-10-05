#!/bin/bash

# debugging
# set -x

echo "CALM Installer v0.0.1"

export CALM_BRANCH=main

install_quicklisp () {
    # test Quicklisp
    sbcl --noinform  --disable-debugger --eval "(format t \"~A\" (find-package 'quicklisp-client))" --eval "(quit)" | grep QUICKLISP

    if [ $? -eq 0 ]; then
        echo "Quicklisp already installed."
    else
        echo "Install Quicklisp ..."
        curl -O https://beta.quicklisp.org/quicklisp.lisp

        sbcl --disable-debugger \
             --load ./quicklisp.lisp \
             --eval "(quicklisp-quickstart:install)" \
             --eval "(quit)"

        echo '(let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname)))) (when (probe-file quicklisp-init) (load quicklisp-init)))' > ~/.sbclrc
        rm ./quicklisp.lisp
    fi
}

install_calm () {
    # for github workflow mostly
    if [ -d .git ]; then
        if [[ $(git remote get-url --all origin) == *"VitoVan/calm"* ]]; then
            echo .git;
            export CALM_BRANCH=$(git describe --tags --abbrev=0)
        fi
    fi

    git clone  --depth 1 --branch $CALM_BRANCH https://github.com/VitoVan/calm.git ~/calm
    echo 'export PATH="$PATH:$HOME/calm/"' >> ~/.bash_profile
    export PATH="$PATH:$HOME/calm/"

    realpath ~/calm/
    ls -lah ~/calm/
    echo $PATH

    echo "Building calm core ..."
    calm core

    cd ~/calm/
    ls -lah .

    if test -f "calm.core"; then
        echo "CALM installed successfully."
        exit 0
    else
        echo "ERR."
        exit 42
    fi

}

# https://stackoverflow.com/questions/394230/how-to-detect-the-os-from-a-bash-script
if [[ "$OSTYPE" == "linux-gnu"* ]]; then
    # ...
    export DISTRO="$(awk -F= '/^NAME/{print $2}' /etc/os-release | sed 's/"//g')"
    echo "linux-$DISTRO"

    echo "Installing dependencies ..."
    if [[ "$DISTRO" == "Ubuntu"* ]]; then
        if ! command -v sbcl &> /dev/null; then
            echo "Installing SBCL ..."
            sudo apt install sbcl -y
        fi
        sudo apt install zip git libsdl2-2.0-0 libsdl2-mixer-2.0-0  libcairo2 -y
    elif [[ "$DISTRO" == "Fedora"* ]]; then
        if ! command -v sbcl &> /dev/null; then
            echo "Installing SBCL ..."
            sudo dnf install sbcl -y
        fi
        sudo dnf install zip git SDL2 SDL2_mixer cairo -y
    else
        echo "Unsupported DISTRO. Please install dependencies by yourself and modify this script."
        exit 42
    fi

    install_quicklisp
    install_calm

elif [[ "$OSTYPE" == "darwin"* ]]; then
    # macOS
    if ! command -v brew &> /dev/null
    then
        echo "Homebrew could not be found, Installing Homebrew ..."
        /bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"
    fi

    if ! command -v sbcl &> /dev/null; then
        echo "Installing SBCL ..."
        brew install sbcl
    fi
    echo "Installing dependencies ..."
    brew install git coreutils sdl2 sdl2_mixer cairo

    # link them, in case of they were unlinked before
    brew link sdl2 sdl2_mixer cairo

    install_quicklisp
    install_calm

elif [[ "$OSTYPE" == "cygwin" ]]; then
    # POSIX compatibility layer and Linux environment emulation for Windows
    echo "Cygwin. Please use MSYS2"
elif [[ "$OSTYPE" == "msys" ]]; then

    # Windows / MSYS2

    if ! command -v sbcl &> /dev/null; then
        echo "SBCL not ready, try setting ENV ..."

        if test -f "/c/program files/steel bank common lisp/sbcl.exe"; then
            echo 'export PATH="$PATH:/c/program files/steel bank common lisp/"' >> ~/.bash_profile
            echo 'export SBCL_HOME="/c/program files/steel bank common lisp/"' >> ~/.bash_profile
        elif test -f "/c/program files/sbcl/bin/sbcl.exe"; then
            echo 'export PATH=$PATH:"/c/Program Files/sbcl/bin/"' >> ~/.bash_profile
            echo 'export SBCL_HOME="/c/Program Files/sbcl/lib/sbcl/"' >> ~/.bash_profile
        else
            echo "Can't find SBCL, please make sure it is installed correctly."
            exit 42
        fi
        source ~/.bash_profile
        echo $PATH
    fi

    echo "Installing dependencies ..."
    echo $PATH

    pacman -S --noconfirm --needed git zip \
           mingw64/mingw-w64-x86_64-zstd \
           mingw64/mingw-w64-x86_64-SDL2 \
           mingw64/mingw-w64-x86_64-SDL2_mixer \
           mingw64/mingw-w64-x86_64-cairo

    install_quicklisp

    install_calm

    echo "Now, you can start using CALM in a new MSYS2 Terminal."

elif [[ "$OSTYPE" == "win32" ]]; then
    # I'm not sure this could happen.
    echo "WIN32. Please use MSYS2."
elif [[ "$OSTYPE" == "freebsd"* ]]; then
    # ...
    echo "FREEBSD. Not supported yet"
else
    # Unknown.
    echo "???. Unknown OS. Not supported"
fi
