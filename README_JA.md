# <img style="vertical-align:middle;margin-right:10px;" width="100" alt="Calm" src="docs/images/calm.png"> C A L M

[![CI](https://github.com/VitoVan/calm/actions/workflows/calm.yml/badge.svg)](https://github.com/VitoVan/calm/actions/workflows/calm.yml) [![GitHub all releases](https://img.shields.io/github/downloads/vitovan/calm/total?color=brightgreen&label=Downloads&style=flat)](#ビルド済みバイナリ) [![Discord](https://img.shields.io/discord/1124894601908584528?logo=discord&logoColor=white&label=Chat&labelColor=%235865f2&color=white)](https://discord.gg/xN6VeaMr2a)

**C**anvas **A**ided **L**isp **M**agic: Lisp でキャンバスベースのアプリケーションを作成し、Linux、macOS、Windows、Web 上で配布できます。

[English](README.md) | 日本語

## Hello, World!

任意のディレクトリを探し、ファイルを作成する: **canvas.lisp**

```lisp
(defparameter *color-list* '((0.83 0.82 0.84) (0.89 0.12 0.17) (0.94 0.87 0.47) (0 0.35 0.59)))
(defun draw ()
  (c:set-operator :darken)
  (dotimes (i 7)
    (c:arc (+ 72 (* (- (/ *calm-window-width* 5) 44) i)) 73 50 0 (* 2 pi))
    (apply #'c:set-source-rgb (nth (if (>= i 4) (- i 4) i) *color-list*))
    (c:fill-path)))
```

ターミナルを起動し、そのディレクトリに cd してコマンドを入力する:

```bash
calm
```

[![Hello World](docs/examples/circles/canvas.png)](#hello-world)

## 例

<p align="center">
    <a href="https://vitovan.com/calm/1.3.0/fan/calm.html"><img width="250" alt="Fan" src="./docs/examples/fan/canvas.png"></a>
    <a href="https://vitovan.com/calm/1.3.0/mondrian/calm.html"><img width="250" alt="Mondrian" src="./docs/examples/mondrian/canvas.png"></a>
    <a href="https://vitovan.com/calm/1.3.0/meditator/calm.html"><img width="250" alt="Meditator" src="./docs/examples/meditator/canvas.png"></a>
</p>

上記の例のソースファイルとバイナリは[こちら](https://github.com/VitoVan/calm/tree/main/docs/examples)と[こちら](https://github.com/VitoVan/calm/releases/tag/1.3.0)です。

CALM で作られた他のアプリケーションについては、[Made with CALM](https://github.com/VitoVan/made-with-calm) をご覧ください。

## <img style="vertical-align:middle;margin-right:5px;" width="50" alt="Calm Installer" src="./docs/images/calm.png"> インストール

### ビルド済みバイナリ

1. ダウンロード

   [![Linux Download](https://img.shields.io/badge/Linux-glibc%202.31+-FFD032.svg?logo=linux)](<https://github.com/VitoVan/calm/releases/latest/download/calm.tgz>) [![macOS Ventura Download](https://img.shields.io/badge/macOS-Ventura-black?logo=apple)](<https://github.com/VitoVan/calm/releases/latest/download/calm.macos-13.dmg>) [![Windows Download](https://img.shields.io/badge/Windows-Windows%2010/11-017fd5.svg?logo=windows)](<https://github.com/VitoVan/calm/releases/latest/download/calm.zip>) [![macOS Monterey Download](https://img.shields.io/badge/macOS-Monterey-white?logo=apple)](<https://github.com/VitoVan/calm/releases/latest/download/calm.macos-12.dmg>) [![macOS Big Sur Download](https://img.shields.io/badge/macOS-Big%20Sur-white?logo=apple)](<https://github.com/VitoVan/calm/releases/latest/download/calm.macos-11.dmg>)

2. 解凍

3. 解凍したディレクトリを PATH 環境に追加する

   macOS の場合は、代わりに `/path/to/Calm.app/Contents/MacOS/` を追加する

macOS と Windows ユーザーの場合、CALM を使うには [Windows SmartScreen](https://duckduckgo.com/?q=how+to+bypass+smartscreen) よりもスマートであるか、[macOS を飼いならす事](https://support.apple.com/guide/mac-help/open-a-mac-app-from-an-unidentified-developer-mh40616/mac)ができる必要があります。万が一何かあったときのために、ここに[インストールガイド](docs/installation_JA.md)があります。

### ソースからの実行

現在、サポートされているプラットフォームは [GitHub Actions runner images](https://github.com/actions/runner-images) によって制限されています。

もしあなたのプラットフォームがサポートされていない場合は、[ソースから実行](docs/hacking_JA.md#ソースからの実行)を使ってください。

## <img style="vertical-align:middle;margin-right:5px;" width="50" alt="Calm Application" src="./docs/images/app.png"> 配布

ターミナルを起動し、**canvas.lisp** ファイルが存在するディレクトリに cd し、次のコマンドを入力する:

```bash
calm publish
```

このコマンドは、プラットフォームによって異なるパッケージを生成する:

**Linux: AppImage**

![Linux AppImage](./docs/images/linux-appimage.png)

>  **注**
>
>  ファンシーウィンドウのアイコンは Wayland では表示されない、何故かは不明。

**macOS: Application Bundle**

![macOS Application DMG](./docs/images/macos-dmg.png)

>**注**
>
>DMG の作成は [create-dmg](https://github.com/create-dmg/create-dmg) によって行われ、もし存在しなければ `brew install create-dmg` によってインストールされます。そのため、create-dmg を持っていない場合は、これで create-dmg がインストールされます。
>
>また、[Homebrew](https://brew.sh/) を持っていない場合は Homebrew をインストールしてくれます。
>
>バイナリ検出は `command -v create-dmg` と `command -v brew` で行われています。

**Windows: Installer**

![Windows Installer](./docs/images/windows-installer.png)

> **注**
>
> インストーラの作成は [NSIS](https://nsis.sourceforge.io/) によって行われ、もし存在しなければ `winget install nsis` によってインストールされます。NSIS (つまり `makensis`) が PATH の下にない場合、NSIS がインストールされます。
>
> また、PATH の下に [winget](https://github.com/microsoft/winget-cli) がなければ、これで winget もインストールされます。
>
> バイナリ検出は `where makensis` と `where winget` によって行われています。

### Web へ

```bash
calm publish-web
```

このコマンドを実行すると、Lisp コードをコンパイルして Web ページを作成することができます。

詳しくは[コマンドリファレンス](#コマンドリファレンス)を参照してください。

# CALM - リファレンス

CALM 1.0.0 から、バージョン番号は [Semantic Versioning Specification](https://semver.org/spec/v2.0.0.html) に従います。これは、私がおかしくなることを心配することなく、落ち着いて CALM を使えることを意味します。というのも、私がおかしくなりそうなときはいつでも、[何かが変更される前に](https://semver.org/spec/v2.0.0.html#spec-item-7)お知らせしますし、[何か驚かせるようなことがあったら](https://semver.org/spec/v2.0.0.html#spec-item-8)メジャーバージョンを上げます。

Keep CALM and have fun.

## コマンドリファレンス

### `calm`

このコマンドはプロジェクトのディレクトリ内で実行する必要があり、**canvas.lisp** というファイルが存在するはずです。

このコマンドは **canvas.lisp** をロードし、関数 `draw` または `draw-forever` の指示に従ってウィンドウを表示します。**canvas.lisp** は普通の Lisp ソースファイルなので、好きなように記述してください。

CALM 関連の関数やパラメータについては、[API リファレンス](#api-リファレンス)を参照してください。

### `calm hello`

このコマンドは、デフォルトのディレクトリ構造でサンプルアプリケーションを作成します。最初にプロジェクトディレクトリを作成する必要があります:

```bash
mkdir my-cool-app
cd my-cool-app
calm hello
```

以下のファイルとディレクトリが作成されます:

```text
.
├── assets
├── canvas.lisp
└── fonts
    └── fonts.conf
```

**assets** ディレクトリと **fonts** ディレクトリに入れたファイルは、配布時にアプリケーションに同梱されます。お気に入りのフォントを **fonts** ディレクトリに置くと、アプリケーション内でそのフォントを使用できるようになります。

フォントの使い方については、[API リファレンス: テキストのレンダリング](#テキストのレンダリング)を参照してください。

### `calm publish`

このコマンドは以下を生成します:

- Linux AppImage
- DMG 内の macOS アプリケーションバンドル
- Windows アプリケーションインストーラー

実行しているプラットフォームによって異なります。

引数はとりませんが、いくつかのオプションは環境変数で設定できます。オプションの詳細については `calm publish-with-options` を確認してください。

### `calm publish-with-options`

このコマンドは `calm publish` と同じことをしますが、その代わりにカスタマイズ可能なすべてのオプション（デフォルト値が用意されています、心配しないでください）について、それぞれあなたの意見を聞きます:

| OS | ENV | 説明                                                  |
| ---------------- | -------------------- | ------------------------------------------------------------ |
| Linux            | APP_NAME             | AppImage のファイル名                                |
| Linux            | APP_ICON             | AppImage ファイルのアイコンと SDL2 Window、PNG ファイルの絶対パス |
| macOS            | APP_NAME             | Launchpad に表示される macOS アプリケーションバンドルの名 |
| macOS            | APP_ICON             | macOS アプリケーションアイコン、ICNS ファイルの絶対パス        |
| macOS            | BUNDLE_ID            | [CFBundleIdentifier](https://developer.apple.com/documentation/bundleresources/information_property_list/cfbundleidentifier)、例えば `com.vitovan.helloworld` |
| macOS            | APP_VERSION          | [CFBundleShortVersionString](https://developer.apple.com/documentation/bundleresources/information_property_list/cfbundleshortversionstring)、例えば: `10.14.1` |
| macOS            | DMG_ICON             | Apple Disk Image (DMG) のアイコン、ICNS ファイルの絶対パス |
| Windows          | APP_NAME             | Windows アプリケーション名は、*コントロールパネル > プログラムと機能*、*アプリケーションと機能*、およびデスクトップショートカットの名前として表示されます |
| Windows          | APP_ICON             | Windows EXE アイコン、ICO ファイルの絶対パス               |

対応する環境変数を設定した場合、オプションは聞かれません。また、`calm publish` コマンドを使っているときにこれらの環境変数を設定することもできます。

### `calm publish-web`

このコマンドは、インターネット上で提供するために必要なすべての素材を含む **web** ディレクトリを生成します。一般的な使い方は次のようになります:

```bash
cd my-cool-app
calm publish-web
```

以下のファイルが作成されます:

```text
web
├── calm.data
├── calm.html
├── calm.js
├── calm.wasm
├── canvas.js
├── favicon.ico
└── jscl.js
```

**calm.html** がエントリーポイントです。web ブラウザの制限のため、このファイルは次のような HTTP プロトコルでご覧ください:

```bash
cd web
python3 -m http.server 8000
```

次にブラウザで http://127.0.0.1:8000/calm.html を開きます。

注: デフォルトでは **fonts** と **assets** ディレクトリ内のファイルはパック *されません* 。以下の REBUILD_WASM_P オプションを確認してください。

### `calm publish-web-with-options`

このコマンドは `calm publish-web` 用であることを除けば、`calm publish-with-options` のように動作します。

| ENV | 説明                                                  |
| -------------------- | ------------------------------------------------------------ |
| LISP_FILES           | `(load "shape.lisp")` のようなコードは、JSCL が HTTP リクエストでそのファイルをロードしようとするため、ウェブアプリケーションに問題を引き起こす可能性があります。 <br/>**canvas.lisp** 以外のファイルをインクルードする必要がある場合は、JSCL をバイパスするようにコードを修正してください、例えば: 次のように、`#-jscl (load "shape.lisp")` を実行し、このオプションを設定する: `("/abs/path/to/canvas.lisp" "/abs/path/to/shape.lisp")` 。ENV を設定する場合は、二重引用符をエスケープすることを忘れないでください。 |
| REBUILD_WASM_P       | デフォルトでは、WebAssembly ファイルはローカルでビルドされず、[CALM Releases](https://github.com/VitoVan/calm/releases/) からダウンロードされます: **calm.tar** 。 このビルド済み WebAssembly バイナリは [OpenSan-Regular.ttf](https://github.com/googlefonts/opensans/raw/main/fonts/ttf/OpenSans-Regular.ttf) とバンドルされており、以下の [API リファレンス](#api-リファレンス)にある [Cairo および SDL2 のすべての API](https://github.com/VitoVan/calm/blob/main/s/usr/web/wasm.lisp#L61) を公開しています。<br/> 他の **fonts** や **assets** をバンドルする必要がある場合や、より多くの C API をウェブに公開する必要がある場合は、このオプションを "yes" に設定してください。 <br/>注意: WebAssembly のバイナリをビルドするには、たくさんの依存関係が必要になります。そのため、`docker` コマンドが自由に使えることを確認して下さい。<br/>デフォルト: "no" |

JSCL は CALM の Web 上でのバックボーンですので、JSCL の変更は CALM 自体の変更とみなされます。CALM の各バージョンで使用されている JSCL のコードベースは固定されていますので、CALM をアップデートしない限り変更されることはありません。安心してお使いください。

### API リファレンス

CALM は、[SDL2](https://wiki.libsdl.org/SDL2/FrontPage) や [Cairo](https://www.cairographics.org/)、その他のものの上の薄いレイヤーになることを意図しています。なので、CALM が提供する API の数はできるだけ少なくなるように意図しています。

#### 基礎

##### ファイル `canvas.lisp`

これは CALM アプリケーションのエントリーファイルです。通常、`draw` という関数が含まれています。

##### 関数 `draw`

これは CALM アプリケーションのエントリー関数で、アプリケーションが起動すると呼び出されます。表示されるキャンバスの描画関数を呼び出すことになっています:

```lisp
(defun draw ()
  (c:set-source-rgb 1 0 0)
  (c:arc 200 73 50 0 (* 2 pi))
  (c:fill-path))
```

この関数は受動的に呼び出されます。つまり、マウスの動き、キーのダウン、キーのアップ、マウスボタンのダウンなど、ユーザーによって何らかのイベントがトリガーされない限り、この関数は最初の呼び出しの後、再び呼び出されることはありません。

ユーザーの操作なしにキャンバスを更新し続けたい場合は、`draw-forever`を使うべきです。

注: `c:arc` のような関数は CALM が公開しているサードパーティの API です。詳しくは[キャンバスに描画](#キャンバスに描画)を参照してください。

##### 関数 `draw-forever`

この関数は `draw` 関数と同様に、CALM アプリケーションのエントリーポイントとしても機能します。`draw` と `draw-forever` の両方を定義することは、John Wick の犬を殺してしまうような深刻な結果をもたらすので、避けることが重要です。

この関数はユーザーの操作に関係なく、`*calm-delay*` ミリ秒ごとに呼び出されます。

##### 変数 `*calm-delay*`

この変数は、CALM がキャンバスをリフレッシュする前に何ミリ秒待つかをコントロールします

デフォルト: 42

この変数はデスクトッププラットフォームでのみ機能します。ウェブプラットフォームでは `*calm-fps*` をチェックしてください。

##### 変数 `*calm-redraw*`

この変数はこの後のキャンバスをリフレッシュするかどうかを制御します。

通常はこの変数に触れる必要はありません。しかし、`draw-forever` を使用していて、リフレッシュのプロセスを手動でコントロールしたい場合、この変数は役に立ちます。例えば:

```lisp
(defparameter *game-started* nil)

(defun on-keyup (key)
  (when (c:keq key :SCANCODE-SPACE)
    (setf *game-started* (not *game-started*))))

(defun draw-forever ()
  (format t "drawing canvas...~%")
  (c:set-source-rgb (/ 12 255) (/ 55 255) (/ 132 255))
  (c:paint)
  (c:set-source-rgb 1 1 1)
  (c:move-to 70 90)
  (c:select-font-family "Arial" :normal :normal)
  (c:set-font-size 60)
  (c:show-text (format nil "Press SPACE: ~A" (write-to-string (mod (c:get-ticks) 9))))
  (setf *calm-redraw* *game-started*))
```

注: この変数は、ユーザーイベントがトリガーされるたびに `T` に設定されます。

##### 変数 `*calm-fps*`

この変数は CALM がキャンバスを更新する前に何ミリ秒待つかを制御します。0 を設定すると、ブラウザの `requestAnimationFrame` メカニズムを使用してキャンバスを更新します。

デフォルト: 42

この変数はウェブ上でのみ動作します。デスクトッププラットフォームでは `*calm-delay*` をチェックしてください。

#### キャンバスに描画

CALM での描画は、[Cairo](https://www.cairographics.org/) を使って実現できます。

どのように描画するかについては、[Cairo チュートリアル](https://www.cairographics.org/tutorial/)と [Cairo API](https://www.cairographics.org/manual/) を読んでください。

たとえば:

```c
cairo_set_line_width (cr, 0.1);
cairo_set_source_rgb (cr, 0, 0, 0);
cairo_rectangle (cr, 0.25, 0.25, 0.5, 0.5);
cairo_stroke (cr);
```

は以下と同じです

```lisp
(c:set-line-width 0.1)
(c:set-source-rgb 0 0 0)
(c:rectangle 0.25 0.25 0.5 0.5)
(c:stroke)
```

[cl-cairo2 によってエクスポートされる](https://github.com/rpav/cl-cairo2/blob/master/src/package.lisp#L7-L142) シンボルはすべて、`c:` のような接頭辞でアクセスできるようにすべきです、例えば： `c:arc` 。ウェブ上では、アクセス可能なシンボルは [cairo.lisp](https://github.com/VitoVan/calm/blob/main/src/web/cairo.lisp#L266-L405) によって制限されています。

Cairo は CALM の主要な描画機能ですので、Cairo 関連のシンボルの変更は CALM 自体の変更とみなされます。安心してお使いください。

##### 関数 `c:rrectangle`

丸みのある長方形を描きます。

```lisp
(defun draw ()
  (c:set-source-rgb 0 0 1)
  (c:rrectangle 20 20 100 100 :radius 8) ;; <---- ここ
  (c:fill-path))
```

##### 関数 `c:show-png`

png ファイルを表示します。

```lisp
(defun draw ()
  (c:show-png "assets/calm.png" 20 20 100 100))
```

この関数は必要に応じて png を引き伸ばします。

#### テキストのレンダリング

##### 関数 `c:select-font-family`

この関数は`c:show-text`で使用するフォントを選択します。

```lisp
(c:select-font-family "Open Sans" :normal :normal)
```

次の 3 つの引数を取ります: `family`、`slant`、`weight` です。詳細な例については、`c:show-text` を参照して下さい。

カスタムフォントをインストールせずに使用するには、**canvas.lisp**ファイルからの相対パスで、**fonts**ディレクトリに置くだけです。

##### 関数 `c:show-text`

この関数は単純なテキストを表示します。

```lisp
(defun draw ()
  (c:move-to 30 100)
  (c:set-font-size 84)
  (c:select-font-family "Open Sans" :italic :bold)
  (c:show-text "DON'T PANIC"))
```

##### 関数 `c:show-markup`

この関数は [Pango Markup](https://docs.gtk.org/Pango/pango_markup.html) を表示します。

```lisp
(defun draw ()
  (c:move-to 20 10)
  (c:set-font-size 84)
  (c:show-markup "This is <span fgcolor='#245791' weight='bold' face='Open Sans'>SICK</span>"))
```

`c:show-markup` と `c:show-text` では座標系が若干異なるので、`c:show-markup` と `c:show-text` を切り替える場合には少し位置を調整する必要があるかもしれないです。

この関数は以下の理由によりウェブには公開されていません:

1. Pango はマルチスレッドを必要とし、そのためには余計な [HTTP HEADERS](https://web.dev/coop-coep/) を設定する必要がある。
2. Pango を WebAssembly にコンパイルすると、読み込みにかかる時間とデータが増えます。
3. Pango を使用する場合、余分なフォントがバンドルされることが多く、ロードされるデータが増えます。

というわけで、実装は簡単ですが、デフォルトで Pango をインクルードするのは良いアイデアとは思えません。

#### サウンドの再生

##### 関数 `c:play-wav`

wavファイルを再生します。

もし `c:play-wav` が以前に呼ばれ、前の wav ファイルがまだ再生されていた場合、その音はマージされます。

```lisp
(c:play-wav "assets/ouch.ogg" :loops 0 :channel -1)
```

`:loops` を -1 に設定すると、"無限に" （～65000 回）再生します

`:channel` を -1 に設定すると、最初に空いているチャンネルで再生します

同時に再生できるファイルの最大数は `*calm-audio-numchans*` 変数に制限されます。

##### 変数 `*calm-audio-numchans*`

同時に再生される wav ファイルの最大数です。

デフォルト: 8

##### 関数 `c:volume-wav`

`c:play-wav` の音量を設定します。

```lisp
(c:volume-wav 128 :channel -1)
```

値は 0 （無音）から 128 の間でなければなりません。

`:channel` を -1 に設定すると、すべてのチャンネルを意味します。

##### 関数 `c:halt-wav`

あるチャンネル、あるいはすべてのチャンネルを停止します。

```lisp
(c:halt-wav :channel -1)
```

`:channel` を -1 に設定すると、すべてのチャンネルを意味します。

##### 関数 `c:play-music`

MP3、Ogg、WAV の音楽ファイルを再生します。

他のタイプのファイルでも動作するかもしれませんが、CALM が保証するものではありません。

```lisp
(c:play-music "assets/bgm.ogg" :loops 0)
```

もし `c:play-music` が以前に呼ばれ、前の音楽がまだ再生されていた場合、それは停止され、最新の音楽が再生され始めます。

##### 関数 `c:volume-music`

`c:play-music` の音量を設定します。

```lisp
(c:volume-music 128)
```

値は 0 （無音）から 128 の間でなければなりません。

##### 関数 `c:halt-music`

音楽の再生を停止します。

##### 関数 `c:play-audio`

音声ファイルを再生します。この機能は [HTMLAudioElement](https://developer.mozilla.org/en-US/docs/Web/API/HTMLAudioElement) を利用するため、ウェブ上でのみ利用可能です。

```lisp
(c:play-audio "assets/meow.ogg" :loop-audio-p nil :volume 1)
```

`:volume` は 0 から 1 の間でなければなりません。

##### 関数 `c:halt-audio`

特定のオーディオファイルまたはすべてのオーディオファイルの再生を停止します。

```lisp
(c:halt-audio "assets/purr.ogg")
```

引数なしで呼び出すと、再生中のすべてのオーディオを停止します。


#### 内部の状態

##### 変数 `*calm-state-mouse-x*`

##### 変数 `*calm-state-mouse-y*`

##### 変数 `*calm-state-mouse-up*`

##### 変数 `*calm-state-mouse-down*`

##### 変数 `*calm-state-mouse-just-clicked*`

##### 変数 `*calm-state-finger-x*`

##### 変数 `*calm-state-finger-y*`

##### 変数 `*calm-state-finger-just-tapped*`

上記の変数はマウスと指（モバイルウェブブラウザのようなタッチデバイス）の状態を保持し、読み取り専用です。`(setf *calm-state-mouse-x* 20)` の結果は漂白剤を飲むことと同じです。

##### 関数 `c:get-ticks`

これは単なる [SDL_GetTicks](https://wiki.libsdl.org/SDL2/SDL_GetTicks) です。

#### イベントのコールバック

これらのコールバックは、あなたが定義すべき関数です。定義しておけば、対応するイベントがトリガーされたときに呼び出されます。

##### コールバック `on-keydown`

##### コールバック `on-keyup`

あなたはこれらのコールバックが何をするのか知っているが、知らないのは、その本来あるべき引数でしょう。詳しい例については `c:keq` を確認して下さい。

##### 関数 `c:keq`

この関数は、最初の引数を無限の SDL2 スキャンコードと比較し、マッチするものがあれば `T` を返します。

```lisp
(defun on-keyup (key) ;; 悪い vimer のためのキーアップハンドラー
  (cond
    ((c:keq key :scancode-left :scancode-h)
     (format t "move left~%"))
    ((c:keq key :scancode-right :scancode-l)
     (format t "move right~%"))
    ((c:keq key :scancode-up :scancode-k)
     (format t "move up~%"))
    ((c:keq key :scancode-down :scancode-j)
     (format t "move down~%"))
    (t (format t "I don't know what to do~%"))))
```

SDL2 スキャンコード: https://wiki.libsdl.org/SDL2/SDL_Scancode

##### コールバック `on-mousewheel`

```lisp
(defun on-mousewheel (x y direction)
  ;; コードをここに
  )
```

##### コールバック `on-mousemotion`

```lisp
(defun internal-on-mousemotion (&key x y)
  ;; コードをここに
  )
```

##### コールバック `on-mousebuttonup`

##### コールバック `on-mousebuttondown`

```lisp
(defun on-mousebuttonup (&key button x y clicks)
  ;; コードをここに
  )
(defun on-mousebuttondown (&key button x y clicks)
  ;; コードをここに
  )
```

##### コールバック `on-fingermotion`

```lisp
(defun on-fingermotion (&key x  y dx dy pressure finger-id)
  ;; コードをここに
  )
```

##### コールバック `on-fingerup`

##### コールバック `on-fingerdown`

```lisp
(defun on-fingerup (&key x  y dx dy pressure finger-id)
  ;; コードをここに
  )
(defun on-fingerdown (&key x  y dx dy pressure finger-id)
  ;; コードをここに
  )
```

##### コールバック `on-windowresized`

```lisp
(defun on-windowresized (width height)
  ;; コードをここに
  )
```

##### コールバック `on-windowenter`

##### コールバック `on-windowleave`

これら 2 つのコールバックは引数を取りません:

```lisp
(defun on-windowenter ()
  ;; コードをここに
  )
```

#### ウィンドウ関連

##### 変数 `*calm-window*`

この変数は読み取り専用で、作成された [SDL_Window](https://wiki.libsdl.org/SDL2/SDL_Window) のインスタンスを保持します。

この変数を使用すると、次のような SDL2 ウィンドウ関連のあらゆる関数を使用できます:

```lisp
;; ウィンドウの位置を取得
(sdl2:get-window-position *calm-window*)

;; ウィンドウを常に上に表示
(sdl2-ffi.functions:sdl-set-window-always-on-top
 *calm-window*
 sdl2-ffi:+true+)
```

##### 変数 `*calm-window-x*`

##### 変数 `*calm-window-y*`

CALM アプリケーションのウィンドウの初期位置（x、y）。

デフォルト: `:centered`

##### 変数 `*calm-window-flags*`

[SDL_WindowFlags](https://wiki.libsdl.org/SDL2/SDL_WindowFlags) のリスト。

この値は次のように設定できます: `(setf *calm-window-flags* '(:shown :allow-highdpi :resizable))`

デフォルト: `'(:shown :allow-highdpi)`

##### 変数 `*calm-window-width*`

##### 変数 `*calm-window-height*`

##### 変数 `*calm-window-title*`

あなたはこれらの変数が何をするか知っているはずです。

そうでないなら、[5 ドルを私に渡して](https://github.com/sponsors/VitoVan/)、そしてもっとよく考えて下さい。

## チュートリアル

OKOK、これを書くよ、これを書くよ、ちょっと待ってて。

待っている間、以下の便利なリンクをチェックできますよ:

- SDL2
    - https://wiki.libsdl.org/SDL2
    - https://github.com/lispgames/cl-sdl2
- Cairo
  - https://www.cairographics.org/

  - https://github.com/rpav/cl-cairo2

- 開発ツール
    - https://slime.common-lisp.dev/
    - https://lispcookbook.github.io/cl-cookbook/vscode-alive.html
- Common Lisp
  - https://lispcookbook.github.io/cl-cookbook/
  - http://www.lispworks.com/documentation/HyperSpec/Front/
  - https://lisp-lang.org/books/

## ライセンス

[ソースコード](https://github.com/VitoVan/calm)は GPL-2.0-only でリリースされています。
