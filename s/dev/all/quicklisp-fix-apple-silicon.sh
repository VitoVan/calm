# https://github.com/lispgames/cl-sdl2/issues/154#issuecomment-1280030566
# this path is made on 2024-08-06
# should be removed when the following PR merged:
#   https://github.com/lispgames/cl-sdl2/pull/167
#   https://github.com/lispgames/cl-sdl2-image/pull/11
#   https://github.com/Failproofshark/cl-sdl2-ttf/pull/28
# and this issue closed:
#   https://github.com/lispgames/cl-sdl2-mixer/issues/12


# Get the system architecture
arch=$(uname -m)

# Check if the architecture is arm64
if [ "$arch" == "arm64" ]; then
    echo "This macOS system is ARM64. Applying SDL2 patch"
    cd ./quicklisp/local-projects

    git clone https://github.com/cffi/cffi.git
    cd cffi
    git reset --hard 13f21d5272d56e759d64895f670b428a63a16f01
    cd ..

    git clone https://github.com/rpav/cl-autowrap.git
    cd cl-autowrap
    git reset --hard 4bba9e37b59cd191dea150a89aef7245a40b1c9d
    cd ..

    git clone https://github.com/ellisvelo/cl-sdl2.git
    cd cl-sdl2
    git reset --hard fe8a1638dcadae3ea1e5627897cad3aa99f95635
    cd ..

    git clone https://github.com/ellisvelo/cl-sdl2-mixer.git
    cd cl-sdl2-mixer
    git reset --hard 8e20cbc06ab61413bdd0183da1d02bbb52fd7332
    cd ..

    git clone https://github.com/ellisvelo/cl-sdl2-image.git
    cd cl-sdl2-image
    git reset --hard 8734b0e24de9ca390c9f763d9d7cd501546d17d4
    cd ..
else
    echo "This macOS system is not ARM64. No need to apply patch."
    exit 0
fi
