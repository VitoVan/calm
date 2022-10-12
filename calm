#!/bin/bash

# debugging
if [ -n "$DEBUGGING" ]; then
    set -x
    env
fi

export APP_DIR=$(pwd)/

# https://stackoverflow.com/questions/59895/how-can-i-get-the-source-directory-of-a-bash-script-from-within-the-script-itsel
SOURCE="${BASH_SOURCE[0]:-$0}";
while [ -L "$SOURCE" ]; do # resolve $SOURCE until the file is no longer a symlink
    DIR="$( cd -P "$( dirname -- "$SOURCE"; )" &> /dev/null && pwd 2> /dev/null; )";
    SOURCE="$( readlink -- "$SOURCE"; )";
    [[ $SOURCE != /* ]] && SOURCE="${DIR}/${SOURCE}"; # if $SOURCE was a relative symlink, we need to resolve it relative to the path where the symlink file was located
done
export CALM_DIR="$( cd -P "$( dirname -- "$SOURCE"; )" &> /dev/null && pwd 2> /dev/null; )/";


cd "$CALM_DIR"

if [ -f env ]; then
    export $(grep -v '^#' env | xargs)
fi

if [ -z "$NO_SCREENSAVER" ]; then
    export SDL_VIDEO_ALLOW_SCREENSAVER=1
fi

if [ -n "$DIST_MODE" ]; then
    echo "Entering DIST_MODE ..."
    export APP_DIR=$CALM_DIR
fi

echo "CALM_DIR: $CALM_DIR"
echo "APP_DIR: $APP_DIR"

export CANVAS_FILE=$APP_DIR/canvas.lisp
export BEFORE_CANVAS_FILE=$APP_DIR/before_canvas.lisp

WITH_SWANK=""
if [ -z "$NO_SWANK" ]; then
    if [ -z "$SWANK_PORT" ]; then
        export SWANK_PORT=4242
    fi
    WITH_SWANK="--eval '(swank:create-server :port $SWANK_PORT)'"
fi

calm_start () {
    CMD="sbcl"
    # if calm.asd exists, then start CALM in normal mode
    if [ -f "calm.asd" ]; then
        if [ -z "$NO_CORE" ]; then
            WITH_CORE="--core calm.core"
            if [ ! -f "calm.core" ]; then
                echo "Calm core does not exist, building now ..."
                calm core
            fi
        fi
        CMD="$CMD $WITH_CORE --load 'calm.asd' --eval '(ql:quickload :calm)' $WITH_SWANK --eval '(uiop:chdir \"$APP_DIR\")'"
        if test -f "$CANVAS_FILE"; then
            if test -f "$BEFORE_CANVAS_FILE"; then
                CMD="$CMD --load '$BEFORE_CANVAS_FILE'"
            fi
            CMD="$CMD --load '$CANVAS_FILE'"
        fi
        CMD="$CMD --eval '(calm:calm-start)'"
        eval $CMD
    else # if calm.asd does not exist, then start CALM in distribution mode (dumped binary)
        if [[ "$OSTYPE" == "linux-gnu"* ]]; then
            LD_LIBRARY_PATH=./ ./calm-bin
        elif [[ "$OSTYPE" == "darwin"* ]]; then
            DYLD_FALLBACK_LIBRARY_PATH=./ ./calm-bin
        else
            echo "Something went wrong, please report to:"
            echo "https://github.com/VitoVan/calm/issues/new"
        fi
    fi
}


if [ "$1" == "core" ]; then
    echo "Deleting old core binary ... "
    rm -f ./calm.core
    echo "Dumping core ..."
    sbcl --load "calm.asd" --eval "(ql:quickload 'calm)" --eval '(save-lisp-and-die "calm.core")'
    echo "DONE."
    exit 0
elif [ "$1" == "dist" ]; then
    echo "Building portable application package ... "

    # are we going to distribute canvas.lisp and load it when running?
    if [[ $2 == "--with-canvas" ]]; then
        export CALM_DIST_WITH_CANVAS=1
    elif [[ $2 == "--fancy-app" ]]; then
        export CALM_DIST_FANCY_APP=1
        if [ -z "$3" ]; then
            export CALM_DIST_FANCY_APP_NAME="CalmApp"
        else
            export CALM_DIST_FANCY_APP_NAME=$3
        fi
    fi

    . ./scripts/dist.sh
    echo "Collecting dependencies ..."
    echo "DONE."
    exit 0
elif [ -z "$1" ]; then
    calm_start
else
    echo "Unrecognised ARG: $1"
fi
