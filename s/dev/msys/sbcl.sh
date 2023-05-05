echo "Install SBCL binary (with compression feature) ..."
if ! [ -d sbcl ]; then
    if ! [ -f install_root.zip ]; then
        set -x
        curl -o install_root.zip -L https://github.com/VitoVan/calm/releases/download/sbcl-2.3.4/install_root-windows-2022.zip
        set +x
    fi
    rm -rf ./install_root
    rm -f calm.core
    unzip install_root.zip
    mv ./install_root ./sbcl
fi
