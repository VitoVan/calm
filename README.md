# Canvas And Lisp Magic

![dont-panic](./dont-panic.png)

Calm down and draw, in Lisp.

![platform support](https://img.shields.io/badge/Platform-Linux%20%7C%20macOS%20%7C%20Windows-blue.svg) [![CI](https://github.com/VitoVan/calm/actions/workflows/main.yml/badge.svg)](https://github.com/VitoVan/calm/actions/workflows/main.yml)

## Auto Install

### *nix (Fedora | Ubuntu | macOS)

```bash
/bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/VitoVan/calm/main/installer/install.sh)"
```

Paste that in a macOS Terminal or Linux shell prompt.

### Windows

```powershell
Set-ExecutionPolicy Bypass -Scope Process -Force; [System.Net.ServicePointManager]::SecurityProtocol = [System.Net.ServicePointManager]::SecurityProtocol -bor 3072; iex ((New-Object System.Net.WebClient).DownloadString('https://raw.githubusercontent.com/VitoVan/calm/main/installer/install.ps1'))
```

Paste that in a Windows PowerShell.

## Manual Install

### 1. Install dependencies

- [SBCL](https://www.sbcl.org)
- [Quicklisp](https://quicklisp.org)
- [SDL2](https://www.libsdl.org)
- [SDL2_mixer](https://www.libsdl.org/projects/mixer)
- [Cairo](https://www.cairographics.org)
- [Git](https://git-scm.com)

For Windows platform, it is only tested under the [MSYS2](https://www.msys2.org) environment.

### 2. Clone & Setup CALM

```bash
# Clone
git clone https://github.com/VitoVan/calm.git ~/calm
# Config PATH
echo 'export PATH="$PATH:~/calm/"' >> ~/.bash_profile
source ~/.bash_profile
# Build cache
calm cache
# Start CALM
calm
```

DONE.
