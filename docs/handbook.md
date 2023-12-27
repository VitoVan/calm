# CALM Handbook

Copyright &copy; 2022-2023 The CALM Contributors

## Abstract

Welcome to CALM! CALM is short for **C**anvas **A**ided **L**isp **M**agic, you can use it to create canvas-based applications with Lisp and distribute them on Linux, macOS, Windows, and the web.

This handbook covers the installation and common tasks could be achieved via CALM. This book is the result of ongoing work by the CALM contributors. Some sections might be outdated. Those interested in helping to update and expand this document could [edit this file directly](https://github.com/VitoVan/calm/edit/main/docs/handbook.md) and send a pull request.

The latest version of this book is available from the [CALM web site](https://vitovan.com/calm). Previous versions can be obtained from the [Releases page](https://github.com/VitoVan/calm/releases).

This book is heavily influenced by the [FreeBSD Handbook](https://docs.freebsd.org/en/books/handbook/book/).

## Preface

### Intended Audience

The CALM newcomer will find that the first part of this book guides the user through the CALM installation process and gently introduces some basics of the Lisp programming language. To work through this part, all you need is the curiosity to explore and the willingness to learn new concepts as they are presented.

Once you have traveled this far, the rest, far larger parts of the Handbook is a comprehensive reference to all manner of topics interest to CALM developers. Some of these chapters may recommend that you do some prior reading, and this is noted in the synopsis at the beginning of each chapter.

### Organization of This Book

This book is split into four logically distinct sections.

The first part, *Getting Started*, covers the installation and basics of CALM and Lisp. It is expected that the reader will follow these chapters in sequence. It also can be safely skipped or skimmed by the experienced programmer.

The second part, *Drawing Graphics*, covers some frequently used drawing features of CALM. This part, and all subsequent parts, can be read out of order. Each chapter begins with a succinct synopsis that describes what the chapter covers and what the reader is expected to already know. This is meant to allow the casual reader to skip around to find chapters of interest. The third part, *OS Interaction*, covers operating system related tasks, such as file selection and window management. The fourth part, *GUI Programming*, covers the possibilities and examples of using CALM as a GUI framework.

### Conventions Used in This Book

To provide a consistent and easy to read text, several conventions are followed throughout the book.

#### Typographic Conventions

- *Italic*

  An *italic* font is used for filenames, URLs, emphasized text, and the first usage of technical terms

- `Monospace`

  A `monospaced` font is used for error messages, commands, environment variables, variables, and code fragments.

- **Bold**

  A **bold** font is used for applications, commands, and keys.

#### User Input

Keys are shown in <kbd>square</kbd> to stand out from other text. Key combinations that are meant to be typed simultaneously are show with `+` between the keys, such as:

<kbd>Ctrl</kbd>+<kbd>Alt</kbd>+<kbd>Del</kbd>

Meaning the user should type the <kbd>Ctrl</kbd>, <kbd>Alt</kbd> and <kbd>Del</kbd> keys at the same time.

Keys that are meant to be typed in sequence will be separated with commas, for example:

<kbd>Ctrl</kbd>+<kbd>X</kbd>, <kbd>Ctrl</kbd>+<kbd>S</kbd>

Would mean that the user is expected to type the <kbd>Ctrl</kbd> and <kbd>X</kbd> keys simultaneously and then to type the <kbd>Ctrl</kbd> and <kbd>S</kbd> keys simultaneously.

#### Examples

Examples starting with `C:\>` indicate a [Windows Command](https://learn.microsoft.com/windows-server/administration/windows-commands/windows-commands). Unless otherwise noted, these commands may be executed from a "Command Prompt" window in Windows 10 or 11.

```cmd
C:\>logman -?
```

> Please note that the real command starts after the symbol `>`, the characters before `>` will be vary depending on your username and current directory path.

Examples starting with `%` indicate a command that must be invoked from a macOS terminal, and examples starting with `$` indicate a command that must be invoked from a Linux terminal. Unless otherwise noted, Bash syntax is used for setting environment variables and other commands.

On Linux:

```bash
$ vmstat
```

On macOS:

```bash
% vm_stat
```

#### Acknowledgments

The book you are holding represents the efforts of [CALM contributors](https://github.com/VitoVan/calm/graphs/contributors) around the world. You are welcome to be one of them. Whether send in fixes for typos, or submit complete chapters, all the contributions will be useful.

No companies have supported the development of CALM or this document, either by paying authors to work on it or paying for publication, etc. If you are running a company and have interests in doing so, please [submit an issue](https://github.com/VitoVan/calm/issues/new/).

## Part I: Getting Started

This part of the handbook is for developers who are new to CALM. These chapters:

- Introduce CALM.
- Guide readers through the installation process.
- Teach Lisp basics and fundamentals.
- Show how to create applications.
- Show how to distribute applications.

The number of forward references in the text have been kept to a minimum so that this section can be read from front to back with minimal page flipping.

### Chapter 1. Introduction

#### 1.1. Synopsis

Thank you for your interest in CALM! The following chapter covers various aspects of the CALM Project, such as its history, goals, development model, and so on.

After reading this chapter you will know:

- How CALM relates to SBCL, SDL2, Cairo, Pango and other software.
- The history and goals of the CALM Project.
- The basics of the CALM open-source development model.

#### 1.2. Welcome to CALM!

CALM is a Lisp development environment for modern Linux, macOS and Windows. It enables you to build and distribute canvas based applications as Linux AppImage, macOS Application Bundle, Windows Installer and Web Application. It provides a high performance Common Lisp compiler along with many useful libraries. Its particular strengths are:

- *Documentation Focused*, we place equal importance on documentation work as we do on Lisp code. Many excellent Lisp programmers feel reluctant to waste time on documenting their work, this sometimes ends up with the work being undervalued. Our focus on documentation in the CALM Project is not about being overrated. Instead, we aim to help you understand CALM and enjoy programming in Lisp easily.
- *Distribution on Multi-platforms* - applications generated by CALM work on Linux, macOS, Windows and the web. This enables you the ability of "Write once, run almost anywhere".
- *GPL-2.0-only license*, which grants you rights to freely modify and extend its source code and incorporate it in your commercial product without worrying about [Tivoization](https://wikipedia.org/wiki/Tivoization). The reason of selecting this particular license is to enable sharing, to have fun together.
- *High performance Common Lisp compiler* - SBCL, a high performance Common Lisp compiler which is being [actively maintained](https://www.sbcl.org/news.html). In quantum computing area, it is used by [rigetti](https://www.reddit.com/r/Common_Lisp/comments/7ifq92/lisp_at_the_frontier_of_computation_by_robert/), [D-wave](https://lispjobs.wordpress.com/2015/03/16/software-developer-for-quantum-processor-development-group-d-wave-systems-vancouver-british-columbia/) and [others](https://lisp-journey.gitlab.io/who/).
- *Based on Robust Engines* - CALM uses [Cairo](https://www.cairographics.org/) as its underlying graphic engine, and [Pango](https://www.pango.org/) as the text rendering engine. For window handling, it's [SDL2](https://libsdl.org/) doing the job. These libraries have been battle-tested for decades. Yes, they are by no means something new and fancy, but also there isn't anything new or fancy about the value of &pi; and the shape of &lambda; for more than decades.

CALM is intended to be a thin layer above SDL2, Cairo, Pango and some other third-party libraries. We haven't done any heavy work other than integrating them. Standing on the shoulders of giants, what we are doing is just paper wrapping.

Most of the time, we introduce the underlying APIs to you instead of providing yet another programming interface. By this, the knowledge gained here could be easily applied into many other Lisp frameworks or other programming languages.

##### 1.2.1. What Can CALM Do?

The applications to what CALM can produce are truly limited only by your own imagination. From generative art to sophisticated GUI program, desktop timer widget to full featured text editor; if it can be presented on a 2D surface then it is more than likely that you can implement it with CALM! CALM also benefits the [Quicklisp](https://www.quicklisp.org/) ecosystem where [over 1500 Lisp libraries](https://www.quicklisp.org/beta/releases.html) available for you to use.

But we are not encouraging you to do everything in CALM. As the name Canvas Aided Lisp Magic suggested, CALM helps you the most when creating canvas-based application. Here is a sampling of some of the applications in which people have built using CALM:

- Paintings - Fan, Mondrian and Meditator

    <img height="200" src="examples/fan/canvas.png"/><img height="200" src="examples/mondrian/canvas.png"/><img height="200" src="examples/meditator/canvas.png"/>

- Focalizzare - A Pomodoro Timer

  <img height="300" src="https://github.com/VitoVan/focalizzare/raw/main/images/25-minutes.png"/> <img height="300" src="https://github.com/VitoVan/focalizzare/raw/main/images/24-minutes.png"/>

- Score Board - A board to record scores

    <img height="300" src="https://github.com/VitoVan/scoreboard/raw/main/canvas.png"/>

- Pelúsica - A game: the survival of a musical ball

    <img height="300" src="https://github.com/VitoVan/pelusica/raw/main/images/pelusica.png"/><img height="300" src="https://github.com/VitoVan/pelusica/raw/main/images/pelusica-dark.png"/>

- The Maze and Lost Cat - A maze game

    <img height="300" src="https://github.com/VitoVan/lisp-game-jam-2023/raw/main/Spring/screenshots/spring-1.png"/><img height="300" src="https://github.com/VitoVan/lisp-game-jam-2023/raw/main/Spring/screenshots/spring-2.png"/>

For a complete list and the details of the above applications, please check the list of [Made with CALM](https://github.com/VitoVan/made-with-calm). Feel free to add your work there by [editing the list](https://github.com/VitoVan/made-with-calm/edit/main/README.md).

##### 1.2.2. Who Uses CALM?

CALM has been know for its multi-platform application publishing capabilities. Currently, no testimonials from companies or individuals other than the author of CALM can be found.

If you are using CALM, please consider submitting a testimonial. Not only will you be helping further awareness of CALM, but your testimonial will include links back to you and will be showcased in this CALM Handbook. Testimonials could be written by anybody who have built anything with CALM and include *why you use CALM*, *how you have benefited*, and, *why the reader should donate or contribute to the Project*. We hope you'll consider helping increase the awareness and evolution of CALM by writing a testimonial today! Submit [a new issue](https://github.com/VitoVan/calm/issues/new/) or contact us in the [chat room](https://discord.gg/xN6VeaMr2a).

#### 1.3. About the CALM Project

The following section provides some background information on the project, including a brief history, project goals, and the development model of the project.

##### 1.3.1 A Brief History of CALM

The CALM Project had its genesis in 2022, the original goal was to create a simple timer app in Lisp to [cure hemorrhoid](https://vitovan.com/jack.html), which of course failed the mission and the related post was [flagged](https://news.ycombinator.com/item?id=34868979) on Hacker News.

Since that time, CALM has made a series of releases each time improving the stability, speed and feature set of the previous version.

For now, long-term development projects continue to take place in the main branch.

##### 1.3.2. CALM Project Goals

The goals of the CALM Project are to provide an environment that may be used for canvas-based Lisp programming and without worrying about the software distribution.

Many of the CALM contributors have a significant investment in the code and documentation and would certainly not mind a little financial compensation now and then, but we are definitely not prepared to insist on it. We believe that our first and foremost "mission" is to provide code and documentation to any and all comers, and for whatever purpose, so that the code gets the widest possible use and provides the widest possible benefit. This is, I believe, one of the most fundamental goals of Free Software and one that we enthusiastically support.

The GNU General Public License, version 2 used by CALM comes with slightly more strings attached which enforces access of the source code, there might be concerns about the commercial use of CALM. Please relax and think of Linux which is also released under GPLv2, the things you built on Linux don't have to follow the license of Linux and don't have to be open-sourced. This should be the same with CALM.

##### 1.3.3. The CALM Development Model

The development of CALM is a very open and flexible process, being built from the contributions of people around the world, as can be seen from our [list of contributors](https://github.com/VitoVan/calm/graphs/contributors). We are constantly on the lookout for new volunteers, and those interested in becoming more closely involved should consult the article on [Contributing](CONTRIBUTING.md).

In short, we do not have a development model yet. And we are looking forward to build one with you. Let's have fun together!

### Chapter 2. Installing CALM

#### 2.1. Synopsis

CALM supports different operating systems including Linux, Windows and macOS. Depending on the platform and system version, different binaries can be [downloaded](https://github.com/VitoVan/calm#pre-built-binary) to install.

The rest of this chapter describes all the three platforms, explaining how to install and setup CALM. There may be minor differences between your system and what is shown here, so use this chapter as a general guide rather than as a set of literal instructions.

After reading this chapter, you will know:

- How to obtain CALM binaries and set it up for ease of use.
- How to troubleshoot a failed installation.
- How to upgrade a outdated version of CALM

#### 2.2. Minimum Requirements

##### 2.2.1 Hardware Requirements

The main component of CALM is SBCL, it requires on the order of [16Mb RAM](https://man.archlinux.org/man/extra/sbcl/sbcl.1.en#SYSTEM_REQUIREMENTS) to run on X86 systems. CALM itself requires approximately 60 Mb disk space, albeit the generated CALM applications are much more smaller (approximately 20~30 Mb). The other usages of hardware resources are depending on your code.

##### 2.2.2 Supported Operating Systems

Supported platforms are currently limited by [GitHub Actions runner images](https://github.com/actions/runner-images):

- Linux - GLIBC version >= 2.31
- macOS - version >= 11
- Windows - version >= 10

All the binaries were built on x86_64 architecture due to the [limitation](https://github.com/actions/runner-images/issues/5631) of GitHub [runners](https://github.com/actions/runner-images/issues/2187), if you are interested in providing CI runner resources for CALM, please [let us know](https://github.com/VitoVan/calm/issues/new/choose).

If your platforms are not supported, please refer to [Hacking CALM - Run from Source](hacking.md#run-from-source).

#### 2.3. Downloading and Extracting

Once it has been determined that the system meets the minimum requirements for installing CALM, the installation file should be downloaded and extracted.

The guide for extraction is shown below in the order of Linux, macOS and Windows. Feel free to skip the parts which are not related to your platform.

##### 2.3.1 Downloading

Please click one of the following buttons to download.

[![Linux Download](https://img.shields.io/badge/Linux-glibc%202.31+-FFD032.svg?logo=linux)](<https://github.com/VitoVan/calm/releases/latest/download/calm.tgz>) [![macOS Ventura Download](https://img.shields.io/badge/macOS-Ventura-black?logo=apple)](<https://github.com/VitoVan/calm/releases/latest/download/calm.macos-13.dmg>) [![Windows Download](https://img.shields.io/badge/Windows-Windows%2010/11-017fd5.svg?logo=windows)](<https://github.com/VitoVan/calm/releases/latest/download/calm.zip>) [![macOS Monterey Download](https://img.shields.io/badge/macOS-Monterey-white?logo=apple)](<https://github.com/VitoVan/calm/releases/latest/download/calm.macos-12.dmg>) [![macOS Big Sur Download](https://img.shields.io/badge/macOS-Big%20Sur-white?logo=apple)](<https://github.com/VitoVan/calm/releases/latest/download/calm.macos-11.dmg>)

You can save the downloaded file in any directory you want, just remember the location.

##### 2.3.2 Extracting on Linux

Many modern Linux distributions have included Archive Manager, File Manager or other facilities for you to extract archive files. But for the consistency across different Linux distributions, here we will use terminal to do this task.

1. Open Terminal

   ![Linux Terminal](handbook.assets/linux-terminal.png)

2. Change directory to *Downloads* (please change this if you placed the downloaded file to other directory)

   ```bash
   cd ~/Downloads
   ```

   ![Linux Terminal - Downloads](handbook.assets/linux-terminal-downloads.png)

3. Make sure the downloaded file exists

   ```bash
   ls calm.tgz
   ```

   ![Linux Terminal - CALM](handbook.assets/linux-terminal-calm-tgz.png)

4. Extract it

   ```bash
   tar xvf calm.tgz --directory=$HOME/
   ```

   ![Linux Terminal - Extract](handbook.assets/linux-terminal-extract.png)

5. Test the extracted CALM binary

   ```bash
   ~/calm/calm
   ```

   ![Linux CALM Starting](handbook.assets/linux-calm-starting.png)

   After a while (a few seconds or minutes depending on your hardware performance), the default CALM window should pop up:

   ![Linux Don't Panic](handbook.assets/linux-dont-panic.png)

##### 2.3.3 Extracting on macOS

Once you have downloaded the macOS binary, you can click the *Show downloads* button on Safari to see it:

![macOS Safari](handbook.assets/macos-safari.png)

If you are using a different browser, you could also find it from the downloads directory:

![macOS Downloads](handbook.assets/macos-dock-downloads.png)

Single click on the downloaded file, a window should pop up:

![macOS DMG Window](handbook.assets/macos-dmg-window.png)

Then drag the CALM icon in the left onto the *Applications* icon and release:

![macOS DMG Copying](handbook.assets/macos-dmg-copying.png)

After the copying finished, you can choose to right click and eject the CALM volume on your desktop.

Now open Terminal, you can click *Launchpad* and type in `Terminal` to search for it:

![macOS Terminal](handbook.assets/macos-terminal.png)

Type in the following commands and press <kbd>return</kbd>:

```bash
xattr -d com.apple.quarantine /Applications/Calm.app
```

![macOS Terminal - xattr](handbook.assets/macos-terminal-xattr.png)

> The above command will ask macOS to respect your choice of running CALM without interfering you with further annoyance.

Test the extracted CALM binary:

```bash
/Applications/Calm.app/Contents/MacOS/calm
```

![macOS CALM Start](handbook.assets/macos-calm-start.png)

![macOS CALM Starting](handbook.assets/macos-calm-starting.png)

After a while (a few seconds or minutes depending on your hardware performance), the default CALM window should pop up:

![macOS Don't Panic](handbook.assets/macos-dont-panic.png)

##### 2.3.3 Extracting on Windows

Once you have downloaded the Windows binary, you can press <kbd>Ctrl</kbd>+<kbd>J</kbd> to show the downloads list if you are using the Edge web browser:

![Windows Edge](handbook.assets/windows-edge.png)

Please click *Open file* or find any other way to open it:

![Windows Open ZIP](handbook.assets/windows-open-zip.png)

Then click the *Extract all* button, you may find it a little different on Windows 10, it doesn't matter.

![Windows ZIP Extract 01](handbook.assets/windows-zip-extract-1.png)

In the popped up window, modify the destination to:

```bash
C:\Users\vito\
```

> Please replace `vito` with your own username, you don't need to actually type it, it would be already there, all we need to do is removing the trailing part `Downloads\calm`.

![Windows ZIP Extract - 02](handbook.assets/windows-zip-extract-2.png)

Then click *Extract*:

![Windows ZIP Extract - 03](handbook.assets/windows-zip-extract-3.png)

Once the copying process finished, you will see a directory named *calm* shown to you:

![Windows ZIP Extract - 04](handbook.assets/windows-zip-extract-4.png)

Now, close the window and open Windows Command Prompt. You can find it by pressing <kbd>Win</kbd>+<kbd>R</kbd> and type `cmd` in it:

![Windows Run - CMD](handbook.assets/windows-run-cmd.png)

Click *OK*, you will get a Windows Command Prompt:

![Windows CMD Opened](handbook.assets/windows-cmd-opened.png)

Now, test the extracted CALM binary:

```CMD
calm\calm.exe
```

![Windows CALM Starting](handbook.assets/windows-calm-starting.png)

![Windows CALM Init](handbook.assets/windows-calm-init.png)

After a while (a few seconds or minutes depending on your hardware performance), the default CALM window should pop up:

![Windows Don't Panic](handbook.assets/windows-dont-panic.png)

#### 2.4. Setting Up CALM

Once you have downloaded and extracted CALM, you are ready to use it. The only inconvenient part is that each time you want to start CALM, you have type in the full path of it. Now let's make some arrangements to get rid of this inconvenience.

The guide for setting up is shown below in the order of Linux, macOS and Windows. Feel free to skip the parts which are not related to your platform.

##### 2.4.1 Setting Up on Linux

This guide will only consider Bash environment, for other shells, please refer to the corresponding manual.

Open terminal, make sure it is Bash shell we're using:

```bash
echo $SHELL
```

![Linux Check Shell](handbook.assets/linux-check-shell.png)

Add CALM directory to PATH environment variable, and activate it:

```bash
echo 'export PATH=$HOME/calm/:$PATH' >> ~/.bash_profile
source ~/.bash_profile
```

![Linux Set Environment](handbook.assets/linux-set-env.png)

Type `calm` and press <kbd>Enter</kbd>:

![Linux Terminal CALM](handbook.assets/linux-terminal-calm.png)

Now you can use command `calm` in your terminal without typing its full path!

##### 2.4.2 Setting Up on macOS

Open Terminal and check the current shell:

![macOS ZSH Shell](handbook.assets/macos-zsh-shell.png)

Modern macOS is using ZSH as the default shell, if your output is `/bin/zsh`, then type the following commands:

```zsh
echo 'export PATH=/Applications/Calm.app/Contents/MacOS/:$PATH' >> ~/.zshrc
source ~/.zshrc
```

![macOS Set Environment](handbook.assets/macos-set-env.png)

Here is the Bash version, in case you switched your default shell:

```bash
echo 'export PATH=/Applications/Calm.app/Contents/MacOS/:$PATH' >> ~/.bash_profile
source ~/.bash_profile
```

For other shells, please refer to the corresponding manual.

Type `calm` and press <kbd>return</kbd>:

![macOS Terminal CALM](handbook.assets/macos-terminal-calm.png)

Now you can use command `calm` in macOS without typing its full path!

##### 2.4.3 Setting Up on Windows

Open Windows Command Prompt by pressing <kbd>Win</kbd>+<kbd>R</kbd> and type `cmd` in it:

![Windows Run - CMD](handbook.assets/windows-run-cmd.png)

In the opened CMD window, type in the following command and press <kbd>Enter</kbd>:

```CMD
setx PATH "%HOMEDRIVE%%HOMEPATH%\calm\;%PATH%"
```

![Windows CMD Environment](handbook.assets/windows-cmd-env.png)

Now *close all CMD windows*, then reopen it and type in `calm`, press <kbd>Enter</kbd>:

![Windows CMD CALM](handbook.assets/windows-cmd-calm.png)

Now you can use command `calm` in Windows Command Prompt without typing its full path!

### Chapter 3. Lisp Basics

#### 3.1. Synopsis

This chapter covers the basic knowledge of the Lisp programming language. New Lisp programmers are encouraged to read through this chapter carefully. The Lisp related knowledge will be demonstrated with a CALM example application, albeit much of them are not specific to CALM.

After reading this chapter, you will know the following aspects of Lisp:

- Basic syntax special forms
- Variables and basic data types
- Lists
- Input / Output
- How to define and use functions

Here in this chapter. We won't emphasize the interactive feature of Lisp, since many languages today have the interactive development console which is similar to the Lisp REPL (Read-Eval-Print Loop), albeit they are totally different underlying.

We also won't introduce Macros the great and powerful. My knowledge is not enough to touch this subject and this book is not designed to teach the higher level magic.

#### 3.2 Preparation

CALM does not come with an editor for you to write code with, so we need to discuss about this.

This chapter describes:

- The different editors you could start with and continue to work with
- How to create CALM sample application and edit the code

##### 3.2.1 Editors for Lisp

To write Lisp code, we need an editor, any kind of editor could do this job.

From the default notepad.exe on Windows to the intimidating Emacs. In this book, we leave this choice to you, you could start from any default editor on your platform, use it and feel it, let the pain of insufficiency guide you to your perfect choice.

Here are some recommendations for editing Lisp code:

- [Visual Studio Code](https://code.visualstudio.com/) 

  optional extension: [Alive](https://marketplace.visualstudio.com/items?itemName=rheller.alive) ([configuration](https://github.com/VitoVan/calm/pull/169))

- [Emacs](https://www.gnu.org/software/emacs/)

  optional mode: [SLIME](https://slime.common-lisp.dev/)

- [Vim](https://www.vim.org/)

  optional mode: [Slimv](https://github.com/kovisoft/slimv)

All the editors above are able to edit Lisp code directly with good syntax highlight support, that all we need as a newcomer. If you want to be more powerful, some optional extensions or modes are also available, but they are not recommended at this stage, since that could complicate things.

##### 3.2.2 CALM Sample Application

Before creating your first CALM application, you need to decided where to put the source code. In this book, we will use the following path as the parent directory for all our CALM applications.

Windows:

```bat
%HOMEDRIVE%%HOMEPATH%\calm-projects
```

Linux / macOS:

```bash
$HOME/calm-projects
```

Please open your terminal, and follow the following steps to create your first CALM application:

1. Create the parent directory and change to it

   Linux / macOS: 

   ````bash
   mkdir $HOME/calm-projects
   cd $HOME/calm-projects
   ````

   Windows:

   ```bat
   mkdir %HOMEDRIVE%%HOMEPATH%\calm-projects
   cd %HOMEDRIVE%%HOMEPATH%\calm-projects
   ```

2. Create directory for our application and change to it

   ```bash
   mkdir hello
   cd hello
   ```

3. Generate sample CALM application

   ```bash
   calm hello
   ```

Now you have generated the sample CALM application, and you are ready to examine it or launch it.

To check the generated files:

Linux / macOS:

```bash
ls
```

Windows:

```bat
dir
```

To launch the sample application:

```bash
calm
```

You should have known this.

To open the current directory in your system file explorer:

Windows:

```bat
explorer .
```

macOS:

```bash
open .
```

Linux:

```bash
xdg-open .
# might not work on all Linux distributions
```

Now, use your favorite editor, open the file `canvas.lisp` in the current directory. 

### Chapter 4. Creating Applications

### Chapter 5. Distributing Applications

## Part II: Drawing Graphics

## Part III: OS Interaction

## Part IV: GUI Programming

## Colophon
