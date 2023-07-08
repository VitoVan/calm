# CALM をハックする

## ビルド済みバイナリを使用

### ステップ 1: CALM をフォークする

https://github.com/VitoVan/calm/fork

### ステップ 2: CALM バイナリのインストール

https://github.com/VitoVan/calm#-installation

### ステップ 3: Git リポジトリの初期化

```bash
# ダウンロードした CALM バイナリのディレクトリに cd する
# 例 macOS の場合:
cd /Applications/Calm.app/Contents/MacOS

# git の init
git init .

# フォークをリモートに追加
git remote add origin git@github.com:<your-name>/calm.git

# fetch
git fetch origin --depth=1

# hard を origin/main にリセット
git reset --hard origin/main
```

これで、修正、プッシュ、プルリクエストの作成ができるようになりましたた！

### 個人的なお気に入り

個人的には、リポジトリをどこか別の場所にクローンし、ダウンロードしたバイナリのディレクトリにシンボルリンクを作成します、それは次のものなどがあります:

```bash
cd /Applications/Calm.app/Contents/MacOS

rm -rf ./build
rm -rf ./s
rm -rf ./src

ln -s ~/github/VitoVan/calm/build build
ln -s ~/github/VitoVan/calm/s s
ln -s ~/github/VitoVan/calm/src src
```

これですべてのソースコードをカバーできるわけではないが、通常はこれで十分です。

## ソースからの実行

0. 準備

    - Linux

      じっと座る。

    - macOS

      パッケージマネージャーが必要です。[Homebrew](https://brew.sh/) をお勧めします。

      [MacPorts](https://www.macports.org/)は機能するはずですが、試してはいません。

    - Windows
      [MSYS2](https://www.msys2.org/) をお勧めします。

      Windows ユーザーに Bash を使えとは言いませんが、他の選択肢は見つかってはいません。

1. SBCL と Quicklisp のインストール

    SBCL のバイナリは[こちら](https://www.sbcl.org/platform-table.html)にあります。また、パッケージ・マネージャーにも含まれているはずです。簡単にインストールできるはずです。

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

    SBCL をインストールしたら、ターミナルからアクセスできるはずです。
    > Windows (MSYS2) では、次のように PATH 環境を手動で設定する必要があります:
    > ```bash
    > echo 'export PATH="/c/program files/steel bank common lisp/:$PATH"' >> ~/.bashrc
    > source ~/.bashrc
    > ```


    Quicklisp をダウンロードしてインストールする:

    ```bash
    curl -O https://beta.quicklisp.org/quicklisp.lisp
    sbcl --non-interactive --load quicklisp.lisp \
         --eval "(quicklisp-quickstart:install)" \
         --eval "(ql-util:without-prompting (ql:add-to-init-file))"
    ```

2. 依存関係のインストール

    必要なのは次のものだけです: [libzstd](https://github.com/facebook/zstd), [SDL2](https://wiki.libsdl.org/SDL2/FrontPage), [SDL2_mixer](https://github.com/libsdl-org/SDL_mixer), [SDL2_image](https://github.com/libsdl-org/SDL_image) そして [Cairo](https://www.cairographics.org/).

    > libzstd は最近のバージョンの SBCL に必要で、SBCL をインストールすればパッケージマネージャーによって自動的にインストールされます。

    ここでは、いくつかのコマンドのサンプルを提供します:

    - Linux (Fedora)
      ```bash
      sudo dnf install libzstd SDL2 SDL2_mixer SDL2_image cairo -y
      ```
    - macOS
      ```bash
      brew install zstd sdl2 sdl2_mixer sdl2_image cairo
      ```
    - Windows (MSYS2)
      > 以下のコードは、ARM デバイス用に調整する必要があります。パッケージは[ここ](https://packages.msys2.org/queue)にある。
      ```bash
      pacman -S --noconfirm --needed git \
       mingw64/mingw-w64-x86_64-zstd \
       mingw64/mingw-w64-x86_64-SDL2 \
       mingw64/mingw-w64-x86_64-SDL2_mixer \
       mingw64/mingw-w64-x86_64-SDL2_image \
       mingw64/mingw-w64-x86_64-cairo
      ```

3. CALM をクローンし、一時的なランチャーの作成

    ```bash
    # ソースコードのクローン
    git clone https://github.com/VitoVan/calm.git ~/calm
    cd ~/calm

    # 一時的なランチャーの作成
    cat > "./calm" <<EOF
      cd "$HOME/calm"
      export CALM_APP_DIR=$(pwd)
      export CALM_HOME="$HOME/calm/"
      export CALM_CMD=show
      sbcl --load entry.lisp show
    EOF

    chmod +x ./calm

    # ランチャーをPATH環境に追加
    echo 'export PATH="$HOME/calm/:$PATH"' >> ~/.bashrc

    # Windows (MSYS2) ユーザーは、以下の行が必要な場合があります:
    # echo 'export PATH="/mingw64/bin/:$PATH"' >> ~/.bashrc

    # ENV変更の有効化
    source ~/.bashrc

    # 試してみる
    calm
    ```

おめでとう！準備万端です！

何を作るのか楽しみです！

# CALM のビルド

現在、CALM は GitHub actions でビルドされています。ワークフローは[こちら](https://github.com/VitoVan/calm/blob/main/.github/workflows/calm.yml)にあります。

もし CALM のビルドに興味があり、何か問題が発生したら、私にお知らせください。

喜んでお手伝いします。
