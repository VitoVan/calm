#!/bin/bash

# https://stackoverflow.com/questions/59895/how-can-i-get-the-source-directory-of-a-bash-script-from-within-the-script-itsel
SOURCE="${BASH_SOURCE[0]:-$0}";
while [ -L "$SOURCE" ]; do # resolve $SOURCE until the file is no longer a symlink
    DIR="$( cd -P "$( dirname -- "$SOURCE"; )" &> /dev/null && pwd 2> /dev/null; )";
    SOURCE="$( readlink -- "$SOURCE"; )";
    [[ $SOURCE != /* ]] && SOURCE="${DIR}/${SOURCE}"; # if $SOURCE was a relative symlink, we need to resolve it relative to the path where the symlink file was located
done
export DIR="$( cd -P "$( dirname -- "$SOURCE"; )" &> /dev/null && pwd 2> /dev/null; )/";

cd "$DIR"

if [[ "$OSTYPE" == "linux-gnu"* ]]; then
    export LD_LIBRARY_PATH=./cdk/lib/calm
elif [[ "$OSTYPE" == "darwin"* ]]; then
    export DYLD_FALLBACK_LIBRARY_PATH=./cdk/lib/calm
else
    echo "Something went wrong, please report to:"
    echo "https://github.com/VitoVan/calm/issues/new"
fi

if [ ! -f "./cdk/libexec/bin/sbcl" ]; then
    echo "CDK not ready, please build with:"
    echo "sbcl --load scripts/build-cdk.lisp"
fi

export SBCL_HOME="$DIR/cdk/lib/sbcl"

export SBCL_SOURCE_ROOT="$DIR/cdk/share/sbcl/src"

export CORE="$SBCL_HOME/sbcl.core"

exec "./cdk/libexec/bin/sbcl" --core "$CORE" --userinit ./.sbclrc  "$@"
