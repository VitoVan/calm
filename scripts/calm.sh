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

export SBCL_HOME=./cdk/lib/sbcl/

if [ ! -f "./launcher" ]; then
   ./sbcl --load launcher.lisp
fi

eval "./launcher $@"
