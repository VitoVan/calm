echo "Config all libraries (.dll) ..."
cd ./lib

cp /mingw64/bin/libzstd.dll ./

# copy all the DLLs required by *.dll
ldd *.dll  | grep mingw | awk '{print $3}' | xargs -I _ cp _ .
