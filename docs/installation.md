# CALM Installation

## Setting Up CALM

You have [donwloaded](https://github.com/VitoVan/calm#pre-built-binary) it, right?

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

     > On Windows, it's very likely the system will stop you from running the software, since it's not verified by Microsoft. If you have problem to run it, please [find a way to bypass SmartScreen](https://duckduckgo.com/?q=how+to+bypass+smartscreen).


Now you are all set, enjoy.
