echo "config all libraries (.so) ..."
cd ./lib
cp /usr/lib64/libzstd.so*
chmod +x *.so*
ldd ./*.so* | grep '=> /lib64' | awk '{print $3}' | sort | uniq | xargs -I _ cp _ .
rm -f libc.so*
rm -f libm.so*
rm -f libstdc++.so*
rm -f libdl.so*
rm -f librt.so*
rm -f libpthread.so*
cd ..
