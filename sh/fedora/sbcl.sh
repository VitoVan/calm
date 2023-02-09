echo "Install SBCL binary (with compression feature) ..."
if ! [ -d sbcl ]; then
    if ! [ -f install_root.zip ]; then
        curl -o install_root.zip -L https://github.com/VitoVan/calm/releases/download/sbcl-2.3.1/install_root-ubuntu-18.04.zip
    fi
    rm -rf ./install_root
    rm -f calm.core
    unzip install_root.zip
    mv ./install_root ./sbcl
fi
