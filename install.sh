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

# https://stackoverflow.com/questions/394230/how-to-detect-the-os-from-a-bash-script
if [[ "$OSTYPE" == "linux-gnu"* ]]; then
    # ...
    export DISTRO="$(awk -F= '/^NAME/{print $2}' /etc/os-release | sed 's/"//g')"
    echo "linux-$DISTRO"

    echo "Installing dependencies ..."
    if [[ "$DISTRO" == "Ubuntu"* ]]; then
        sudo apt install sbcl git libsdl2-2.0-0 libsdl2-mixer-2.0-0  libcairo2 -y
    elif [[ "$DISTRO" == "Fedora"* ]]; then
        sudo dnf install sbcl git SDL2 SDL2_mixer cairo -y
    else
        echo "Unsupported DISTRO. Please install dependencies by yourself and modify this script."
        exit 42
    fi

    install_quicklisp

elif [[ "$OSTYPE" == "darwin"* ]]; then
    # macOS
    if ! command -v brew &> /dev/null
    then
        echo "Homebrew could not be found, Installing Homebrew ..."
        /bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"
    fi

    echo "Installing dependencies ..."
    brew install sbcl git coreutils sdl2 sdl2_mixer cairo

    # link them, in case of they were unlinked before
    brew link sdl2 sdl2_mixer cairo

    install_quicklisp
elif [[ "$OSTYPE" == "cygwin" ]]; then
    # POSIX compatibility layer and Linux environment emulation for Windows
    echo "Cygwin. Please use MSYS2"
elif [[ "$OSTYPE" == "msys" ]]; then
    # Windows / MSYS2
    echo "Installing dependencies ..."
    pacman -S --noconfirm --needed git \
           mingw64/mingw-w64-x86_64-SDL2 \
           mingw64/mingw-w64-x86_64-SDL2_mixer \
           mingw64/mingw-w64-x86_64-cairo

    choco install sbcl
    echo 'export PATH="/c/program files/steel bank common lisp/:$PATH"' >> ~/.bashrc
    source ~/.bashrc

    install_quicklisp

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
