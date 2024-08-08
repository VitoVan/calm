# CALM インストール

## CALM の設定

もう[ダウンロード](https://github.com/VitoVan/calm#pre-built-binary)済みですよね？

では、使いやすい環境を整えてみましょう。

- Linux
  ```bash
  tar xvf calm.tgz --directory=$HOME/
  echo 'export PATH="$HOME/calm/:$PATH"'>> ~/.bashrc
  ```
- macOS
  1. ダウンロードした DMG ファイルを開きます

  2. .app をアプリケーションフォルダにドラッグします

  3. 私を信じて、Calm.appの隔離を解除してください。

      ```bash
      xattr -d com.apple.quarantine /Applications/Calm.app
      ```

  4. DMG を取り出し、Calm.app を実行する

     初回の実行には **時間がかかるかもしれません。**

  5. コマンド `calm` を PATH 環境に追加する

      ```bash
      echo 'export PATH="/Applications/Calm.app/Contents/MacOS/:$PATH"'>> ~/.bashrc
      ```


- Windows
  1. 右クリックしてすべて展開します

  2. 解凍したフォルダを C: に移動します

  3. [PATH 環境変数](https://helpdeskgeek.com/windows-10/add-windows-path-environment-variable/)に `C:¥calm` を追加します

     > MSYS2 または Git Bash を使用している場合は、以下のコードを実行してください:
     >
       > ```bash
      > echo 'export PATH="/c/calm/:$PATH"' >> ~/.bashrc
      > ```

     > Windows の場合、マイクロソフト社によって検証されていないため、システムがソフトウェアの実行を停止する確率が非常に高いです。実行に問題がある場合は、[SmartScreen をバイパスする方法を見つけてください](https://www.google.com/search?q=how+to+bypass+smartscreen)。


これで準備万端、お楽しみください。
