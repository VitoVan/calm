#!/bin/bash

# debugging
# set -x

echo "CALM Distributor v0.0.1"

export SBCL_VERSION=$(sbcl --version | cut -c 6-10)

export SBCL_COMPRESSION_LEVEL=9

if [[ $SBCL_VERSION > "2.2.5" ]]; then
    export SBCL_COMPRESSION_LEVEL=22
fi

export SBCL_COMPRESSION_ARG=""

sbcl --eval "(if (find :sb-core-compression *features*) (uiop:quit 0) (uiop:quit 42))"

if [ $? -eq 0 ]; then
    export SBCL_COMPRESSION_ARG=":compression $SBCL_COMPRESSION_LEVEL"
fi


# https://stackoverflow.com/questions/394230/how-to-detect-the-os-from-a-bash-script
if [[ "$OSTYPE" == "linux-gnu"* ]]; then
    # Linux

    export DISTRO="$(awk -F= '/^NAME/{print $2}' /etc/os-release | sed 's/"//g')"
    echo "linux-$DISTRO"

    if [[ "$DISTRO" == "Fedora"* ]]; then

        cd $APP_DIR

        echo "Prepare & Switch to dist folder ..."
        rm -rf ./dist
        mkdir ./dist
        cp $CALM_DIR/calm ./dist
        cd ./dist

        echo "Copy dependencies ..."
        sbcl --noinform \
             --load "$CALM_DIR/calm.asd" \
             --eval "(ql:quickload 'calm)" \
             --load "$CANVAS_FILE" \
             --load "$CALM_DIR/scripts/copy-foreign-libraries.lisp" \
             --eval '(uiop:quit)'

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

        echo "Dump binary ..."

        sbcl --disable-debugger \
             --load "$CALM_DIR/calm.asd" \
             --eval "(ql:quickload 'calm)" \
             --load "$CANVAS_FILE" \
             --eval "(sb-ext:save-lisp-and-die \"calm-dist\" $SBCL_COMPRESSION_ARG :executable t :toplevel #'calm:calm-start)"

        chmod +x calm*

        echo "DONE."
        ls -lah .
        cd ..
        mv dist calm-app
        zip -r -9 calm-app-linux.zip ./calm-app
        pwd
        echo "=========================="
        echo "Please copy all your resource files into 'dist' folder before distributing."
        echo "=========================="
    else
        echo "Sorry, I only made this work on Fedora. You could try using docker."
        exit 42
    fi


elif [[ "$OSTYPE" == "darwin"* ]]; then
    # macOS

    cd $APP_DIR

    # link all dependencies, in case of they were unlinked before
    brew link sdl2 sdl2_mixer cairo

    echo "Prepare & Switch to dist folder ..."
    rm -rf ./dist
    mkdir ./dist
    cp $CALM_DIR/calm ./dist
    cd ./dist

    echo "Copy dependencies ..."
    sbcl --noinform \
         --load "$CALM_DIR/calm.asd" \
         --eval "(ql:quickload 'calm)" \
         --load "$CANVAS_FILE" \
         --load "$CALM_DIR/scripts/copy-foreign-libraries.lisp" \
         --eval '(uiop:quit)'

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

    echo "Dump binary ..."

    sbcl --disable-debugger \
         --load "$CALM_DIR/calm.asd" \
         --eval "(ql:quickload 'calm)" \
         --load "$CANVAS_FILE" \
         --eval "(sb-ext:save-lisp-and-die \"calm-dist\" $SBCL_COMPRESSION_ARG :executable t :toplevel #'calm:calm-start)"

    echo "Relink the dependencies ..."
    brew link sdl2 sdl2_mixer cairo

    echo "DONE."
    ls -lah .
    cd ..
    mv dist calm-app
    zip -r -9 calm-app-macos.zip ./calm-app
    pwd
    echo "=========================="
    echo "Please copy all your resource files into 'dist' folder before distributing."
    echo "=========================="
elif [[ "$OSTYPE" == "cygwin" ]]; then
    # POSIX compatibility layer and Linux environment emulation for Windows
    echo "Cygwin. Please use MSYS2"
elif [[ "$OSTYPE" == "msys" ]]; then
    # Windows / MSYS2

    cd $APP_DIR

    echo "Prepare & Switch to dist folder ..."
    rm -rf ./dist
    mkdir ./dist
    cd ./dist

    echo "Copy dependencies ..."
    sbcl --noinform \
         --load "$CALM_DIR/calm.asd" \
         --eval "(ql:quickload 'calm)" \
         --load "$CANVAS_FILE" \
         --load "$CALM_DIR/scripts/copy-foreign-libraries.lisp" \
         --eval '(uiop:quit)'

    ls -lah /mingw64/bin/

    if [[ $SBCL_VERSION > "2.2.5" ]]; then
        echo "Copy libzstd ..."
        cp /mingw64/bin/libzstd.dll ./
    fi

    # copy all the DLLs required by *.dll
    ldd *.dll  | grep mingw | awk '{print $3}' | xargs -I _ cp _ .

    echo "Dump binary ..."

    sbcl --disable-debugger \
         --load "$CALM_DIR/calm.asd" \
         --eval "(ql:quickload 'calm)" \
         --load "$CANVAS_FILE" \
         --eval "(sb-ext:save-lisp-and-die \"calm.exe\" :application-type :gui $SBCL_COMPRESSION_ARG :executable t :toplevel #'calm:calm-start)"

    echo "DONE."
    ls -lah .
    cd ..
    mv dist calm-app
    zip -r -9 calm-app-windows.zip ./calm-app
    pwd
    echo "=========================="
    echo "Please copy all your resource files into 'dist' folder before distributing."
    echo "=========================="

elif [[ "$OSTYPE" == "win32" ]]; then
    # I'm not sure this can happen.
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
