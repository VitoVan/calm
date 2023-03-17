echo "config all libraries (.dylib) ..."
cd ./lib

cp /usr/local/lib/libzstd.dylib ./

# copy all dependencies
# this should be ran for many times until no more dylib need to be copied
# loop 42 times to make sure every dependency's dependency's dependency's ... dependencies are all copied
for i in {1..42}
do
    otool -L *.dylib | grep /usr/local | awk '{print $1}' | xargs -I _ cp -n _ .
done

chmod +w *.dylib

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
