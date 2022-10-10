#!/bin/bash

# debugging
if [ -n "$DEBUGGING" ]; then
    set -x
fi

echo "CALM Distributor v0.0.7"

export SBCL_VERSION=$(sbcl --version | cut -c 6-10)

export SBCL_COMPRESSION_LEVEL=9

if [[ $SBCL_VERSION > "2.2.5" ]]; then
    export SBCL_COMPRESSION_LEVEL=22
fi

export SBCL_COMPRESSION_ARG=""

echo "Detecting SBCL compression level ..."
sbcl --eval "(if (find :sb-core-compression *features*) (uiop:quit 0) (uiop:quit 42))"
if [ $? -eq 0 ]; then
    export SBCL_COMPRESSION_ARG=":compression $SBCL_COMPRESSION_LEVEL"
fi

export SBCL_APPLICATION_TYPE_ARG=""

copy_deps () {
    echo "Copy dependencies ..."
    CMD="sbcl --load '$CALM_DIR/calm.asd' --eval '(ql:quickload :calm)'"

    if test -f "$BEFORE_CANVAS_FILE"; then
        CMD="$CMD --load '$BEFORE_CANVAS_FILE'"
    fi

    CMD="$CMD --load '$CANVAS_FILE' --load '$CALM_DIR/scripts/copy-foreign-libraries.lisp' --eval '(uiop:quit)'"
    eval $CMD
}

dump_binary () {
    CMD="sbcl --load '$CALM_DIR/calm.asd' --eval '(ql:quickload :calm)'"
    CALM_START="calm-start"

    if test -f "$BEFORE_CANVAS_FILE"; then
        CMD="$CMD --load '$BEFORE_CANVAS_FILE'"
    fi

    if [ -z "$CALM_DIST_WITH_CANVAS" ]; then
        echo "Dump binary ..."
        CMD="$CMD --load '$CANVAS_FILE' "
    else
        echo "Dump binary with canvas ..."
        cp "$APP_DIR/canvas.lisp" ./
        CALM_START="calm-load-and-start"
    fi
    CMD="$CMD --eval \"(sb-ext:save-lisp-and-die \\\"calm-bin\\\" $SBCL_COMPRESSION_ARG $SBCL_APPLICATION_TYPE_ARG :executable t :toplevel #'calm:$CALM_START)\""
    eval $CMD
}

dist_linux () {
    if [[ "$DISTRO" == "Fedora"* ]]; then
        echo "Distributing CALM into directory './dist' ..."
    else
        echo "'calm dist' and 'calm dist --with-canvas' only work on Fedora"
        echo "Please use docker instead (if you don't mind):"
        echo "docker run -v $PWD:/app --rm vitovan/calm bash -c 'calm dist'"
        exit 42
    fi

    echo "Prepare & Switch to dist folder ..."
    rm -rf ./dist
    mkdir ./dist
    cp "$CALM_DIR/calm" ./dist/
    cd ./dist

    copy_deps

    if [[ $SBCL_VERSION > "2.2.5" ]]; then
        echo "Copy libzstd ..."
        cp /usr/lib64/libzstd.so* ./
    fi

    chmod +x *.so*

    # copy all the DLLs required by SDL2 & cairo
    ldd ./*.so* | grep '=> /lib64' | awk '{print $3}' | sort | uniq | xargs -I _ cp _ .

    # something should not be there
    rm -f libc.so*
    rm -f libstdc++.so*

    dump_binary
    echo "Please run the file \"calm\"." > ./how-to-run-this-app.txt
    echo "If you are using the terminal, cd to this directory, and type:" >> ./how-to-run-this-app.txt
    echo "./calm" >> ./how-to-run-this-app.txt

    chmod +x calm*

    cd ..
    ls -lah ./dist
    echo "=========================="
    echo "Please copy all your resource files into 'dist' folder before distributing."
    echo "=========================="
}

dist_darwin () {
    # link all dependencies, in case of they were unlinked before
    brew link sdl2 sdl2_mixer cairo

    echo "Prepare & Switch to dist folder ..."
    rm -rf ./dist
    mkdir ./dist
    cp "$CALM_DIR/calm" ./dist/
    cd ./dist

    copy_deps

    if [[ $SBCL_VERSION > "2.2.5" ]]; then
        echo "Copy libzstd ..."
        cp /usr/local/lib/libzstd.dylib ./
    fi

    echo "Copy every dependency's dependency's dependency's ..."
    # copy all dependencies (`gcp` stands for `GNU cp` in GNU coreutils)
    # this should be ran for many times until no more dylib need to be copied
    # loop 42 times to make sure every dependency's dependency's dependency's ... dependencies are all copied
    for i in {1..42}
    do
        otool -L *.dylib | grep /usr/local | awk '{print $1}' | xargs -I _ cp -n _ .
    done

    chmod +w *.dylib

    echo "Configure dep tree ..."

    # set LC_RPATH
    # make all of them able to load dependencies from the @loader_path
    for f in *.dylib; do install_name_tool -add_rpath @loader_path/. $f; done

    # change LC_ID_DYLIB
    for f in *.dylib; do install_name_tool -id @rpath/`basename $f` $f; done

    # change LC_LOAD_DYLIB
    # make all of them load dependencies from the @rpath
    for f in *.dylib
    do
        for p in $(otool -L $f | grep /usr/local | awk '{print $1}')
        do
            install_name_tool -change $p @rpath/`basename $p` $f
        done
    done

    # fix libSDL2.dylib
    rm libSDL2.dylib
    ln -s `find libSDL2-*.dylib` libSDL2.dylib

    echo "Unlink the dependencies ..."
    brew unlink sdl2 sdl2_mixer cairo


    dump_binary

    echo "Please double click the file \"calm\"." > ./how-to-run-this-app.txt

    echo "Relink the dependencies ..."
    brew link sdl2 sdl2_mixer cairo

    cd ..
    ls -lah ./dist
    echo "=========================="
    echo "Please copy all your resource files into 'dist' folder before distributing."
    echo "=========================="
}

dist_msys () {
    # Windows / MSYS2

    #
    # export SBCL_APPLICATION_TYPE_ARG=":application-type :gui"

    echo "Prepare & Switch to dist folder ..."
    rm -rf ./dist
    mkdir ./dist
    cd ./dist

    copy_deps

    if [[ $SBCL_VERSION > "2.2.5" ]]; then
        echo "Copy libzstd ..."
        cp /mingw64/bin/libzstd.dll ./
    fi

    # copy all the DLLs required by *.dll
    ldd *.dll  | grep mingw | awk '{print $3}' | xargs -I _ cp _ .

    dump_binary

    mv calm-bin calm.exe

    echo "Please double click \"calm.exe\"." > ./how-to-run-this-app.txt

    cd ..
    ls -lah ./dist
    echo "=========================="
    echo "Please copy all your resource files into 'dist' folder before distributing."
    echo "=========================="
}

make_appimage () {
    echo "Making AppImage ..."

    if [ ! -d "./dist" ]; then
        echo "Directory 'dist' does not exist, please run 'calm dist' first"
        echo "Quitting ..."
        exit 42
    else
        mkdir calm.AppDir
        cp dist/* calm.AppDir/

        if [ ! -d "./resources" ]; then
            echo "Directory 'resources' does not exist ... skipping ..."
        else
            mkdir calm.AppDir/resources/
            cp -r ./resources/* calm.AppDir/resources/*
        fi

        mv "$CALM_DIR/scripts/calm.svg" calm.AppDir/
        mv "$CALM_DIR/scripts/calm.desktop" calm.AppDir/
        mv "$CALM_DIR/scripts/AppRun" calm.AppDir/

        chmod +x calm.AppDir/AppRun

        if [ ! -f "$CALM_DIR/scripts/appimagetool" ]; then
            echo "Downloading appimagetool ..."
            curl -o "$CALM_DIR/scripts/appimagetool" -L \
                 https://github.com/AppImage/AppImageKit/releases/download/continuous/appimagetool-x86_64.AppImage
            chmod +x "$CALM_DIR/scripts/appimagetool"
        fi
        "$CALM_DIR/scripts/appimagetool" calm.AppDir calm-app.AppImage
        ls -lah .
    fi
}

make_standalone_exe_with_warp () {
    echo "Making stand-alone EXE ..."

    if [ ! -d "./dist" ]; then
        echo "Directory 'dist' does not exist, please run 'calm dist' first"
        echo "Quitting ..."
        exit 42
    else

        WARP_PACKER="$CALM_DIR/scripts/warp-packer.exe"

        if [ ! -f "$WARP_PACKER" ]; then
            echo "Downloading warp-packer ..."
            curl -o "$WARP_PACKER" -L \
             https://github.com/dgiagio/warp/releases/download/v0.3.0/windows-x64.warp-packer.exe
        fi

        if [ ! -d "./resources" ]; then
            echo "Directory 'resources' does not exist ... skipping ..."
        else
            mkdir ./dist/resources/
            cp -r resources/* ./dist/resources/*
        fi

        "$WARP_PACKER" --arch windows-x64 --input_dir ./dist --exec calm.exe --output calm.exe

        ls -lah .
    fi
}

make_standalone_exe () {
    echo "Making stand-alone EXE ..."

    if [ ! -d "./dist" ]; then
        echo "Directory 'dist' does not exist, please run 'calm dist' first"
        echo "Quitting ..."
        exit 42
    else

        CALM_ZIPPER="$CALM_DIR/scripts/calm-zipper.exe"

        if [ ! -f "$CALM_ZIPPER" ]; then
            echo "Downloading calm-zipper ..."
            curl -o "$CALM_ZIPPER" -L \
                 https://github.com/VitoVan/calm/releases/latest/download/calm-zipper.exe
        fi

        if [ ! -d "./resources" ]; then
            echo "Directory 'resources' does not exist ... skipping ..."
        else
            mkdir ./dist/resources/
            cp -r resources/* ./dist/resources/*
        fi

        "$CALM_ZIPPER" ./dist

        ls -lah .
    fi
}

cd "$APP_DIR"

# https://stackoverflow.com/questions/394230/how-to-detect-the-os-from-a-bash-script
if [[ "$OSTYPE" == "linux-gnu"* ]]; then
    export DISTRO="$(awk -F= '/^NAME/{print $2}' /etc/os-release | sed 's/"//g')"
    echo "linux-$DISTRO"

    if [ -z "$CALM_DIST_FANCY_APP" ]; then
        dist_linux
    else
        make_appimage
    fi

elif [[ "$OSTYPE" == "darwin"* ]]; then

    dist_darwin

elif [[ "$OSTYPE" == "cygwin" ]]; then

    echo "Cygwin. Please use MSYS2."

elif [[ "$OSTYPE" == "msys" ]]; then


    if [ -z "$CALM_DIST_FANCY_APP" ]; then
        dist_msys
    else
        make_standalone_exe
    fi


elif [[ "$OSTYPE" == "win32" ]]; then
    # I'm not sure this could happen.
    echo "WIN32. Please use MSYS2, please let me know if you want:"
    echo "https://github.com/VitoVan/calm/issues/new"
elif [[ "$OSTYPE" == "freebsd"* ]]; then
    # ...
    echo "FREEBSD. Not supported yet, please let me know if you want:"
    echo "https://github.com/VitoVan/calm/issues/new"
else
    # Unknown.
    echo "???. Unknown OS. Not supported yet, please let me know if you want:"
    echo "https://github.com/VitoVan/calm/issues/new"
fi
