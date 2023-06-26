# without this, zenity will fail from our bundled .so files
unset LD_LIBRARY_PATH

if [ "$XDG_CURRENT_DESKTOP" = "KDE" ]; then
    if command -v kdialog &> /dev/null
    then
        kdialog --icon ${CALM_HOME}/build/calm.png --title 'Initialising CALM' --passivepopup 'This may take a few minutes.' 60
        exit
    fi
fi

if [ "$XDG_CURRENT_DESKTOP" = "GNOME" ]; then
    if command -v zenity &> /dev/null
    then
        if zenity --help=notification &> /dev/null
        then
            zenity --notification --text="Initialising CALM... \nThis may take a few minutes." --hint=image_path:${CALM_HOME}/build/calm.png
            exit
        fi
    fi
fi

if command -v zenity &> /dev/null
then
    zenity --timeout=60 --info --text="Initialising CALM... \n\nThis may take a few minutes."  --no-wrap --ok-label="OK, I will wait"
    exit
fi

if command -v gxmessage &> /dev/null
then
    echo -e "Initialising CALM... \n\nThis may take a few minutes." | gxmessage -timeout 60 -center -buttons "OK\, I will wait" -file -
    exit
fi

if command -v xmessage &> /dev/null
then
    echo -e "Initialising CALM... \n\nThis may take a few minutes." | xmessage -timeout 60 -center -buttons "OK\, I will wait" -file -
    exit
fi
