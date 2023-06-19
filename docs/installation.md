# Installation

If you don't mind to use pre-built binaries, the installation of CALM is just to [download and extract](https://github.com/VitoVan/calm#pre-built-binary).

### Setting Up CALM

You have donwloaded it, right?

Now let's set up the environment for the ease of use.

- Linux
  ```bash
  tar xvf calm.tgz --directory=$HOME/
  echo 'export PATH="$HOME/calm/:$PATH"'>> ~/.bashrc
  ```
- macOS
  1. Open the downloaded DMG file

  2. Drage the .app to the Applications folder

  3. Eject the DMG and run Calm.app

     **It may take a while** for the first run.

     > On macOS, it's very likely the system will stop you from running the software, since it's not verified by Apple. If you have problem to run it, please check [this](https://support.apple.com/HT202491).

  4. Add command `calm` to the PATH environment

      ```bash
      echo 'export PATH="/Applications/Calm.app/Contents/MacOS/:$PATH"'>> ~/.bashrc
      ```


- Windows
  1. Right click to Extract All

  2. Move the extracted folder to `C:\calm`

  3. Add `C:\calm` to your [PATH environment variable](https://helpdeskgeek.com/windows-10/add-windows-path-environment-variable/)

     > If you are using MSYS2 or Git Bash, run the following code:
     >
       > ```bash
      > echo 'export PATH="/c/calm/:$PATH"' >> ~/.bashrc
      > ```


Congrats! You are all set!

Can't wait to see what you are going to build!

## Run From Source

For the one who hates pre-built binary, or who has the unsupported system / device.

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

## Build CALM

Running CALM from source is an expedient way to have a taste, it can load `canvas.lisp`, but don't deal with the third party dependencies.

To build CALM is to perform the following tasks:

- Collect the dependencies
- Build a launcher (from `calm.c`)

Those strenuous low-tech work are initiated by `build.sh`, and carried by those in the `s/dev/` directory.

To build CALM on a not yet supported platform, the following files definitely need to be investigated:

```bash
s/dev/
├── all
│   ├── clean.sh
│   ├── copy-lib-with-canvas.sh
│   ├── copy-lib.lisp
│   ├── copy-lib.sh
│   ├── install-quicklisp.lisp
│   ├── load-calm.lisp
│   └── quicklisp.sh
├── darwin
│   ├── config-lib.sh
│   ├── deps.sh
│   ├── pack.sh
│   └── sbcl.sh
├── fedora
│   ├── config-lib.sh
│   ├── deps.sh
│   ├── pack.sh
│   └── sbcl.sh
└── msys
    ├── config-lib.sh
    ├── deps.sh
    ├── pack.sh
    └── sbcl.sh
```

I will leave this here for now, since there might be very few people want to do this kind of work.

If you have interests to build CALM and encountered any problem, please let me know.

I would be so glad to help.
