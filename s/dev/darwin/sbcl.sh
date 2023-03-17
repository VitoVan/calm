echo "Install SBCL binary (with compression feature) ..."
if ! [ -d sbcl ]; then
    if ! [ -f install_root.zip ]; then
        set -x
        if [[ -z "${CI_MATRIX_OS}" ]]; then
            curl -o install_root.zip -L https://github.com/VitoVan/calm/releases/download/sbcl-2.3.1/install_root-macos-12.zip
        else
            curl -o install_root.zip -L https://github.com/VitoVan/calm/releases/download/sbcl-2.3.1/install_root-${CI_MATRIX_OS}.zip
        fi
        set +x
    fi
    rm -rf ./install_root
    rm -f calm.core
    unzip install_root.zip
    mv ./install_root ./sbcl
fi
