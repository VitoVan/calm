#!/bin/bash

export APP_DIR="$(pwd)"

# https://stackoverflow.com/questions/59895/how-can-i-get-the-source-directory-of-a-bash-script-from-within-the-script-itsel
SOURCE="${BASH_SOURCE[0]:-$0}";
while [ -L "$SOURCE" ]; do # resolve $SOURCE until the file is no longer a symlink
    DIR="$( cd -P "$( dirname -- "$SOURCE"; )" &> /dev/null && pwd 2> /dev/null; )";
    SOURCE="$( readlink -- "$SOURCE"; )";
    [[ $SOURCE != /* ]] && SOURCE="${DIR}/${SOURCE}"; # if $SOURCE was a relative symlink, we need to resolve it relative to the path where the symlink file was located
done
export CALM_DIR="$( cd -P "$( dirname -- "$SOURCE"; )" &> /dev/null && pwd 2> /dev/null; )/";


# cd "$CALM_DIR"

if [[ "$OSTYPE" == "linux-gnu"* ]]; then
    export LD_LIBRARY_PATH="$CALM_DIR/cdk/lib/calm"
elif [[ "$OSTYPE" == "darwin"* ]]; then
    export DYLD_FALLBACK_LIBRARY_PATH="$CALM_DIR/cdk/lib/calm"
else
    echo "Something went wrong, please report to:"
    echo "https://github.com/VitoVan/calm/issues/new"
fi

export SBCL_BIN="$CALM_DIR/cdk/libexec/bin/sbcl"
export SBCL_HOME="$CALM_DIR/cdk/lib/sbcl/"
export SBCL_CORE="$SBCL_HOME/sbcl.core"
export SBCL_USERINIT="$CALM_DIR/.sbclrc"

if [ ! -f "./launcher" ]; then
    "$SBCL_BIN" --core "$SBCL_CORE" --userinit "$SBCL_USERINIT" --load launcher.lisp
fi

eval "./launcher $@"
