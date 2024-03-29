echo "Install system dependencies ..."
sudo dnf install -y \
     findutils \
     git \
     file \
     zip \
     unzip \
     gcc \
     libzstd \
     SDL2 \
     SDL2_mixer \
     SDL2_image \
     cairo \
     pango \
     fontconfig \
     gobject-introspection
