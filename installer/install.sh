echo "CALM Installer v0.0.1"

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
    git clone https://github.com/VitoVan/calm.git ~/calm
    echo 'export PATH="$PATH:~/calm/"' >> ~/.bash_profile
    source ~/.bash_profile

    export PATH="$PATH:~/calm/"

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
        sudo apt install sbcl git libsdl2-2.0-0 libsdl2-mixer-2.0-0  libcairo2 -y
    elif [[ "$DISTRO" == "Fedora"* ]]; then
        sudo dnf install sbcl git SDL2 SDL2_mixer cairo -y
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

    echo "Installing dependencies ..."
    brew install sbcl git coreutils sdl2 sdl2_mixer cairo

    # link them, in case of they were unlinked before
    brew link sdl2 sdl2_mixer cairo

    install_quicklisp
    install_calm

elif [[ "$OSTYPE" == "cygwin" ]]; then
    # POSIX compatibility layer and Linux environment emulation for Windows
    echo "Cygwin. Please use MSYS2"
elif [[ "$OSTYPE" == "msys" ]]; then

    # Windows / MSYS2
    echo "Installing dependencies ..."
    echo $PATH

    pacman -S --noconfirm --needed git p7zip \
           mingw64/mingw-w64-x86_64-SDL2 \
           mingw64/mingw-w64-x86_64-SDL2_mixer \
           mingw64/mingw-w64-x86_64-cairo


    ls -lah "/c/program files/steel bank common lisp/"
    echo 'export PATH="$PATH:/c/program files/steel bank common lisp/"' >> ~/.bash_profile
    echo 'export SBCL_HOME="/c/program files/steel bank common lisp/"' >> ~/.bash_profile

    source ~/.bash_profile

    export PATH="$PATH:/c/program files/steel bank common lisp/"
    export SBCL_HOME="/c/program files/steel bank common lisp/"

    echo $PATH

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
