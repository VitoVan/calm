# Hacking CALM

## With Pre-built Binaries

### Step 1: Fork CALM

https://github.com/VitoVan/calm/fork

### Step 2: Install CALM binary

https://github.com/VitoVan/calm#-installation

### Step 3: Init Git Repository

```bash
# cd to your downloaded CALM binary directory
# e.g. on macOS:
cd /Applications/Calm.app/Contents/MacOS

# init git
git init .

# add your fork to remote
git remote add origin git@github.com:<your-name>/calm.git

# fetch
git fetch origin --depth=1

# reset hard to origin/main
git reset --hard origin/main
```

Now you can modify, push and create pull requests!

### My Personal Favour

Personally, I would clone the repository somewhere else and make some symbol links into the directory of the downloaded binary, such as:

```bash
cd /Applications/Calm.app/Contents/MacOS

rm -rf ./build
rm -rf ./s
rm -rf ./src

ln -s ~/github/VitoVan/calm/build build
ln -s ~/github/VitoVan/calm/s s
ln -s ~/github/VitoVan/calm/src src
```

This won't cover all the source code, but normally it's enough.

## Run From Source

0. Preparation

    - Linux

      Sit tight.

    - macOS

      You need a package manager, I suggest [Homebrew](https://brew.sh/).

      [MacPorts](https://www.macports.org/) should work, but I haven't tried it.

    - Windows
      You need a package manager, I suggest [MSYS2](https://www.msys2.org/).

      I hate to ask Windows users to use Bash, but I haven't found any other options.

1. Install SBCL & Quicklisp

    SBCL binaries can be found [here](https://www.sbcl.org/platform-table.html). It should also has been included in your package manager. You should be able to install it easily.

    - Linux (Fedora)
      ```bash
      sudo dnf install sbcl -y
      ```
    - macOS
      ```bash
      brew install sbcl
      ```

    - Windows
      ```bat
      winget install sbcl
      ```

    After you have installed SBCL, it should be accessible in your terminal.
    > On Windows (MSYS2), you may need to set the PATH environment manually, like this:
    > ```bash
    > echo 'export PATH="/c/program files/steel bank common lisp/:$PATH"' >> ~/.bashrc
    > source ~/.bashrc
    > ```


    Now download and install Quicklisp:

    ```bash
    curl -O https://beta.quicklisp.org/quicklisp.lisp
    sbcl --non-interactive --load quicklisp.lisp \
         --eval "(quicklisp-quickstart:install)" \
         --eval "(ql-util:without-prompting (ql:add-to-init-file))"
    ```

2. Install dependencies

    All you need is: [libzstd](https://github.com/facebook/zstd), [SDL2](https://wiki.libsdl.org/SDL2/FrontPage), [SDL2_mixer](https://github.com/libsdl-org/SDL_mixer), [SDL2_image](https://github.com/libsdl-org/SDL_image) and [Cairo](https://www.cairographics.org/).

    > libzstd is required by the recent version of SBCL, and will be installed by your package manager automatically if you installed SBCL.

    Here I will provide some sample commands, hope this would be helpful:

    - Linux (Fedora)
      ```bash
      sudo dnf install libzstd SDL2 SDL2_mixer SDL2_image cairo -y
      ```
    - macOS
      ```bash
      brew install zstd sdl2 sdl2_mixer sdl2_image cairo
      ```
    - Windows (MSYS2)
      > The code below definitely need to be adjusted for ARM devices, since they are installing x86\_64 version libs. Packages can be found [here](https://packages.msys2.org/queue).
      ```bash
      pacman -S --noconfirm --needed git \
       mingw64/mingw-w64-x86_64-zstd \
       mingw64/mingw-w64-x86_64-SDL2 \
       mingw64/mingw-w64-x86_64-SDL2_mixer \
       mingw64/mingw-w64-x86_64-SDL2_image \
       mingw64/mingw-w64-x86_64-cairo
      ```

3. Clone CALM & make a temporary launcher

    ```bash
    # clone the source code
    git clone https://github.com/VitoVan/calm.git ~/calm
    cd ~/calm

    # make a temporary launcher
    cat > "./calm" <<EOF
      cd "$HOME/calm"
      export CALM_APP_DIR=$(pwd)
      export CALM_HOME="$HOME/calm/"
      export CALM_CMD=show
      sbcl --load entry.lisp show
    EOF

    chmod +x ./calm

    # add the launcher to PATH env
    echo 'export PATH="$HOME/calm/:$PATH"' >> ~/.bashrc

    # Windows (MSYS2) user may need the following line:
    # echo 'export PATH="/mingw64/bin/:$PATH"' >> ~/.bashrc

    # activate the ENV change
    source ~/.bashrc

    # give it a try
    calm
    ```

Congrats! You are all set!

Can't wait to see what you are going to build!

# Building CALM

Currently CALM was built on github actions, you can find the workflow [here](https://github.com/VitoVan/calm/blob/main/.github/workflows/calm.yml).

If you have interests to build CALM and encountered any problem, please let me know.

I would be so glad to help.
