# <img style="vertical-align:middle;margin-right:10px;" width="100" alt="Calm" src="docs/images/calm.png"> Canvas And Lisp Magic

Calm down and draw something, in Lisp.

[![platform support](https://img.shields.io/badge/Platform-Linux%20%7C%20macOS%20%7C%20Windows-blue.svg)](#installation) [![CI](https://github.com/VitoVan/calm/actions/workflows/calm.yml/badge.svg)](https://github.com/VitoVan/calm/actions/workflows/calm.yml) [![GitHub all releases](https://img.shields.io/github/downloads/vitovan/calm/total?color=green&label=Downloads&style=plastic)](https://github.com/VitoVan/calm/releases/latest)

## Hello World

Find whatever directory, create a file: **canvas.lisp**

```lisp
(in-package #:calm)
(defparameter *color-list* '((0.83 0.82 0.84) (0.89 0.12 0.17) (0.94 0.87 0.47) (0 0.35 0.59)))
(defun draw ()
  (dotimes (i 7)
    (apply #'c:set-source-rgb (nth (if (>= i 4) (- i 4) i) *color-list*))
    (c:arc (+ 60 (* (- (/ *calm-window-width* 5) 40) i)) 70 50 0 (* 2 pi))
    (c:fill-path)))
```

Launch your terminal, cd to that directory, enter the command:

```bash
calm
```

[![Hello World](docs/examples/circles/canvas.png)](#hello-world)

## More Examples

<p align="center">
    <a title="Check the code for Fan" href="./docs/examples/fan"><img width="250" alt="Fan" src="./docs/examples/fan/canvas.png"></a>
    <a title="Check the code for Mondrian" href="./docs/examples/mondrian"><img width="250" alt="Mondrian" src="./docs/examples/mondrian/canvas.png"></a>
    <a title="Check the code for Meditator" href="./docs/examples/meditator"><img width="250" alt="Meditator" src="./docs/examples/meditator/canvas.png"></a>
</p>


The example pack can be downloaded [here](https://github.com/VitoVan/calm/releases/latest/download/examples.zip).

For more examples, please check this list: [Made with CALM](https://github.com/VitoVan/made-with-calm).

## Installation

### Pre-built Binary

1. **Download the [latest release](https://github.com/VitoVan/calm/releases/latest)** for your platform

2. Extract the content

3. Add the extracted folder into your PATH environment

   for macOS, add `/path/to/Calm.app/Contents/MacOS/` instead

In case anything went wrong, here is an [Installation Guide](docs/installation.md).

### From the Source Code

All the binaries are built with [Github Action](.github/workflows/calm.yml), the available environments are limited. Currently they only support x86_64 CPU. If you are using something not supported, or you are one of those good old Lispers, feel free to [Run from Source](docs/installation.md#run-from-source), or [Build CALM](docs/installation.md#build-calm).

## <img style="vertical-align:middle;margin-right:5px;" width="50" alt="Calm Application" src="./build/app.png"> Distribution

Distribution of CALM Application is just one command.

Launch your terminal, cd to the directory where the file **canvas.lisp** exists, enter the command:

```bash
calm publish
```

This command will generate different packages on different platforms:

**Linux: AppImage**

![Linux AppImage](./docs/images/linux-appimage.png)

>  **Note**
>
>  You may not expect the fancy window icon on Wayland, I don't know why.

**macOS: Application Bundle**

![macOS Application DMG](./docs/images/macos-dmg.png)

>**Note**
>
>_You don't need to read this if you don't care about what I am going to install on your machine_
>
>DMG creation is powered by [create-dmg](https://github.com/create-dmg/create-dmg), will be installed by `brew install create-dmg` if it were not present.
>
>So if you don't have create-dmg, this will install create-dmg for you.
>
>And, if you don't have [Homebrew](https://brew.sh/), this will also install Homebrew for you.
>
>The binary detection was done by `command -v create-dmg` and `command -v brew`.

**Windows: Installer**

![Windows Installer](./docs/images/windows-installer.png)

> **Note**
>
> _You don't need to read this if you don't care about what I am going to install on your machine_
>
> Installer creation is powered by [NSIS](https://nsis.sourceforge.io/), will be installed by `winget install nsis` if it were not present.
>
> So if you don't have NSIS (i.e., `makensis`) under your PATH, this will install NSIS for you.
>
> And, if you don't have [winget](https://github.com/microsoft/winget-cli) under your PATH, this will also install winget for you.
>
> The binary detection was done by `where makensis` and `where winget`.

### Customization

If you want to change the generated application name, icon, etc, please use the following command:

```bash
calm publish-with-options
```

You will be asked for all the customization options and the default value will also be provided.

### Share Your Code

Share your work with other Lispers.

Launch your terminal, cd to the directory where the file **canvas.lisp** exists, enter the command:

```bash
calm share
```

Your canvas related content will be packed and uploaded to [transfer.sh](https://transfer.sh/), you can share them with a link:

```bash
  ____      _      _       __  __
 / ___|    / \    | |     |  \/  |
| |       / _ \   | |     | |\/| |
| |___   / ___ \  | |___  | |  | |
 \____| /_/   \_\ |_____| |_|  |_|

CALM: 0.0.39, SBCL: 2.3.1

CALM Archive Created: calm-share-3888034151.tar.gz

Generating Sharing Link:
/usr/local/opt/curl/bin/curl
EXECUTING CMD: curl -s --upload-file ./calm-share-3888034150.tar.gz https://transfer.sh/
https://transfer.sh/Bf3BtK/calm-share-3888034151.tar.gz
```

If any of your friend don't know how to use this file, please don't mock them.

## Environment Variables

Setf-able:

- `CALM_HOST_LISP`

  With `CALM_HOST_LISP=ecl calm` to use [ECL](https://ecl.common-lisp.dev/) on your machine.

  Of course, `CALM_HOST_LISP=sbcl calm` will also work.

  Setting this variable means you are going to take care of all the dependencies (including Quicklisp).

- `SDL_VIDEO_ALLOW_SCREENSAVER=1`

  Allow screensaver, check [Why does SDL disable my screensaver by default?](https://wiki.libsdl.org/SDL2/FAQUsingSDL#why_does_sdl_disable_my_screensaver_by_default).

  You can use `(setf (uiop:getenv "SDL_VIDEO_ALLOW_SCREENSAVER") "1")` to set it.

Read-only:

- `CALM_HOME`

  This variable holds the path of the directory where calm binary exists

- `CALM_APP_DIR`

  This variable holds the path of the directory where calm was started up (aka `pwd`)

## Tutorials

Not yet, but here are some links might be helpful:

- SDL2
    - https://wiki.libsdl.org/SDL2
    - https://github.com/lispgames/cl-sdl2

- Cairo
  - https://www.cairographics.org/
  - https://github.com/rpav/cl-cairo2
    in `(c:fill-path)`, the `c` is a nickname for `cl-cairo2`

- CALM
  - https://github.com/VitoVan/calm/blob/main/src/config.lisp
  - https://github.com/VitoVan/calm/blob/main/src/utils.lisp

- Development Tools
    - https://slime.common-lisp.dev/
    - https://lispcookbook.github.io/cl-cookbook/vscode-alive.html

- Common Lisp
  - https://lispcookbook.github.io/cl-cookbook/
  - http://www.lispworks.com/documentation/HyperSpec/Front/
  - https://lisp-lang.org/books/
