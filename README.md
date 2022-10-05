# Canvas And Lisp Magic

[![Don't Panic](./images/dont-panic.png)](https://github.com/VitoVan/calm)

Calm down and draw something, in Lisp.

[![platform support](https://img.shields.io/badge/Platform-Linux%20%7C%20macOS%20%7C%20Windows-blue.svg)](#installation) [![CI](https://github.com/VitoVan/calm/actions/workflows/main.yml/badge.svg)](https://github.com/VitoVan/calm/actions/workflows/main.yml)

## Hello World

Find whatever directory, create a file: **canvas.lisp**

```lisp
(in-package #:calm)
(defparameter *color-list* '((0.83 0.82 0.84) (0.89 0.12 0.17) (0.94 0.87 0.47) (0 0.35 0.59)))
(defun draw ()
  (dotimes (i 7)
    (apply #'c:set-source-rgb (nth (if (>= i 4) (- i 4) i) *color-list*))
    (c:arc (+ 60 (* (- (/ *calm-width* 5) 40) i)) 70 50 0 (* 2 pi))
    (c:fill-path))
  (setf *calm-redraw* nil))
```

Launch your terminal, cd to that directory, enter the command:

```bash
calm
```

[![Hello World](./images/hello-world.png)](#hello-world)

## More Examples

<p align="center">
    <a title="Check the code for Fan" href="https://github.com/calm2d/fan"><img width="250" alt="Fan" src="./images/fan.png"></a>
    <a title="Check the code for Mondrian" href="https://github.com/calm2d/mondrian"><img width="250" alt="Mondrian" src="./images/mondrian.png"></a>
    <a title="Check the code for Meditator" href="https://github.com/calm2d/meditator"><img width="250" alt="Meditator" src="./images/meditator.png"></a>
</p>

## Installation

### Auto Install

#### *nix (Fedora | Ubuntu | macOS)

```bash
/bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/VitoVan/calm/main/scripts/install.sh)"
```

Paste that in a macOS Terminal or Linux shell prompt.

#### Windows

```powershell
Set-ExecutionPolicy Bypass -Scope Process -Force; [System.Net.ServicePointManager]::SecurityProtocol = [System.Net.ServicePointManager]::SecurityProtocol -bor 3072; iex ((New-Object System.Net.WebClient).DownloadString('https://raw.githubusercontent.com/VitoVan/calm/main/scripts/install.ps1'))
```

Paste that in a Windows PowerShell.

### Manual Install

#### 1. Install dependencies

- [SBCL](https://www.sbcl.org)
- [Quicklisp](https://quicklisp.org)
- [SDL2](https://www.libsdl.org)
- [SDL2_mixer](https://www.libsdl.org/projects/mixer)
- [Cairo](https://www.cairographics.org)
- [Git](https://git-scm.com)

For Windows platform, it is only tested under the [MSYS2](https://www.msys2.org) [terminal](https://www.msys2.org/docs/terminals/).

#### 2. Clone & Setup CALM

```bash
# Clone
git clone https://github.com/VitoVan/calm.git ~/calm
# Config PATH
echo 'export PATH="$PATH:~/calm/"' >> ~/.bash_profile
source ~/.bash_profile
# Build core
calm core
# Start CALM
calm
```

DONE.

## Distribution

### Standard Distribution

To distribute your applications to non-wizard users,

to provide a portable application package for those who fears the dark of the terminal.

Launch your terminal, cd to the directory where the file **canvas.lisp** exists, enter the command:

```bash
calm dist
```

You will get a directory `dist` containing all the dependencies and your final binary.

Before sending it to your friends, please make sure you have put all your resources (.wav, .png, .mp3, etc.) into that directory.

Typically, they will be able to enjoy your application.

If not, tell them to [extract the zip file](https://www.wikihow.com/Unzip-a-File) and run the file `calm` in the extracted folder.

### Canvas Distribution

For those who loves the source, for the maximum configurability.

```bash
calm dist --with-canvas
```

Mostly identical to the standard distribution, but with `canvas.lisp` included.

This gives the end user ability to modify your application.

It is very convenient when your friend has a bad taste in color.

> **Note**
> 1. Don't use Quicklisp in your `canvas.lisp`
> 2. Put everything related to Quicklisp and CFFI into `before_canvas.lisp`, it will be loaded before dumping your binary.

### Expedient Distribution

Your friends are using a Linux / macOS / Windows, but you don't have the corresponding OS.

Let's say it's Windows.

You could also distribute expediently by:

1. [downloading the latest](https://github.com/VitoVan/calm/releases) `calm-hello-windows-with-canvas.zip`
2. extract it
3. replace the `canvas.lisp` file with yours
4. zip it and send it to your friends

> **Warning**
> 1. Don't use Quicklisp in your `canvas.lisp`, that won't work.
> 2. Don't load any other foreign libraries except the already loaded ones (SDL2, Cairo, etc.)

## Environment Variables

Setf-able:

- `NO_SWANK=1`

  Disable [Swank](https://www.cliki.net/SWANK), it was enabled by default at port 4242

- `SWANK_PORT=4343`

  Change Swank port to 4343, the default port was 4242

- `NO_CORE=1`

  Disable calm [core](https://www.sbcl.org/manual/#Saving-a-Core-Image), it was enabled by default to reduce the startup time

- `NO_SCREENSAVER=1`

  Disable screensaver, in case you want to keep the screen on.


Read-only:

- `CALM_DIR`

  This variable holds the path of the directory where calm binary exists

- `APP_DIR`

  This variable holds the path of the directory where calm was started up (aka `pwd`)
