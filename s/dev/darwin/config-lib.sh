# debug
set -x

echo "config all libraries (.dylib) ..."
cd ./lib

cp $(brew --prefix)/lib/libzstd.dylib ./
cp $(brew --prefix)/lib/libpangocairo*.dylib ./

# copy all dependencies
# this should be ran for many times until no more dylib need to be copied
# loop 42 times to make sure every dependency's dependency's dependency's ... dependencies are all copied
for i in {1..42}
do
    otool -L *.dylib | grep $(brew --prefix) | awk '{print $1}' | xargs -I _ cp -n _ .
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
    for p in $(otool -L $f | grep $(brew --prefix) | awk '{print $1}')
    do
        install_name_tool -change $p @rpath/`basename $p` $f
    done
done

rm libSDL2.dylib
ln -s libSDL2-2.0.0.dylib libSDL2.dylib

rm libSDL2_mixer.dylib
ln -s libSDL2_mixer-2.0.0.dylib libSDL2_mixer.dylib

rm libSDL2_image.dylib
ln -s libSDL2_image-2.0.0.dylib libSDL2_image.dylib

rm libcairo.dylib
ln -s libcairo.2.dylib libcairo.dylib

rm libpangocairo-1.0.dylib
ln -s libpangocairo-1.0.0.dylib libpangocairo-1.0.dylib

rm libgobject-2.0.dylib
ln -s libgobject-2.0.0.dylib libgobject-2.0.dylib

rm libgirepository-1.0.dylib
ln -s libgirepository-1.0.1.dylib libgirepository-1.0.dylib

rm libzstd.dylib
ln -s libzstd.1.dylib libzstd.dylib

ls -lah .

# copy all typelibs
cp -L -R $(brew --prefix)/lib/girepository-1.0/*.typelib ./
