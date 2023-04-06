echo "config all libraries (.dll) ..."
cd ./lib

cp /mingw64/bin/libzstd.dll ./
cp /mingw64/bin/libpangocairo*.dll ./

# copy all the DLLs required by *.dll
ldd *.dll  | grep mingw | awk '{print $3}' | xargs -I _ cp _ .

# copy all typelibs
cp -R /mingw64/lib/girepository-1.0/*.typelib ./
