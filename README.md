# Canvas And Lisp Magic

[![Don't Panic](./images/dont-panic.png)](https://github.com/VitoVan/calm)

Calm down and draw something, in Lisp.

[![platform support](https://img.shields.io/badge/Platform-Linux%20%7C%20macOS%20%7C%20Windows-blue.svg)](#installation) [![CI](https://github.com/VitoVan/calm/actions/workflows/calm.yml/badge.svg)](https://github.com/VitoVan/calm/actions/workflows/main.yml)

## Hello World

Find whatever directory, create a file: **canvas.lisp**

```lisp
(in-package #:calm)
(defparameter *color-list* '((0.83 0.82 0.84) (0.89 0.12 0.17) (0.94 0.87 0.47) (0 0.35 0.59)))
(defun draw ()
  (dotimes (i 7)
    (apply #'c:set-source-rgb (nth (if (>= i 4) (- i 4) i) *color-list*))
    (c:arc (+ 60 (* (- (/ *calm-width* 5) 40) i)) 70 50 0 (* 2 pi))
    (c:fill-path)))
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
curl -o ~/calm.zip "https://github.com/VitoVan/calm/releases/download/0.0.13/calm-(uname | cut -d '_' -f 1).zip"
unzip ~/calm.zip ~/calm && rm ~/calm.zip
echo 'export PATH="$PATH:$HOME/calm/"' >> ~/.bash_profile
source ~/.bash_profile
```

Paste that in a macOS Terminal or Linux shell prompt.

#### Windows

```powershell
Invoke-WebRequest -Uri "https://github.com/VitoVan/calm/releases/download/0.0.13/calm-Windows.zip" -OutFile "C:\calm.zip"
Expand-Archive C:\calm.zip -DestinationPath C:\calm
setx /M PATH "%PATH%;C:\calm"
refreshenv
```

Paste that in a Windows PowerShell.

### Manual Install

1. Download the latest [release](https://github.com/VitoVan/calm/releases)
2. Unzip it
3. Add the extracted folder into your PATH environment.

## Distribution (CALM Application)

### Standard Distribution

To distribute your applications to non-wizard users, to provide a portable application package for those who fears the dark of the terminal.

Launch your terminal, cd to the directory where the file **canvas.lisp** exists, enter the command:

```bash
calm dist
```

You will get a directory `dist` containing all the dependencies and your final binary, now you should put all your resource files (.wav, .png, .mp3, etc.) into that directory.

Now, zip it and send it to your friend! Normally, they will be able to enjoy your application. If not, tell them to [unzip](https://www.wikihow.com/Unzip-a-File) and run the file `calm` or double click `calm.exe`.

### Canvas Distribution

For those who love the source, for the maximum configurability.

```bash
calm dist-with-canvas
```

Mostly identical to the standard distribution, but with `canvas.lisp` included.

This gives the end user ability to modify your application.

It is very convenient when your friend has a bad taste in color.

### Expedient Distribution

Your friend is using Linux / macOS / Windows, but you don't have the corresponding OS.

Let's say it's Windows.

You could also distribute expediently:

1. [download the latest](https://github.com/VitoVan/calm/releases) `calm-app-with-canvas-MINGW64.zip `
2. extract it
3. replace the `canvas.lisp` file with yours
4. zip it and send it to your friend

> **Note**
> 1. Don't use Quicklisp, normally it won't work.
> 2. CFFI might work, but foreign libraries (except SDL2, Cairo, etc.) are not guaranteed to exist.

## Environment Variables

Setf-able:

- `SDL_VIDEO_ALLOW_SCREENSAVER=1`

  Allow screensaver, check [Why does SDL disable my screensaver by default?](https://wiki.libsdl.org/SDL2/FAQUsingSDL#why_does_sdl_disable_my_screensaver_by_default)

- `CALM_EVAL='(format t "hello")'`

  Some lisp code eval-ed after loading your `canvas.lisp` and before start

Read-only:

- `CALM_DIR`

  This variable holds the path of the directory where calm binary exists

- `APP_DIR`

  This variable holds the path of the directory where calm was started up (aka `pwd`)
