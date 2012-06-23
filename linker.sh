#!/bin/sh

# Linker for my dotfiles
# Checks if a file exists and does interactive rm. If user says no
# the script will still try to link, but failes because the file
# already exists. 

#read -p 'Will overwrite existing config files. Continue? (y/n) '
#[ $REPLY != "y" ] && exit 1


rm -i ~/.bashrc && ln -s $PWD/bashrc ~/.bashrc

# Only desktop (turing, but mathom may be different)
if  [ `hostname` = 'turing' ]; then 
    echo "turing: xorg.conf.."
    if [ -f "/etc/X11/xorg.conf" ]; then
        echo "   Note: Moving old /etc/X11/xorg.conf to ~/xorg.conf-$(date +"%d%m%y")"
        sudo mv -i /etc/X11/xorg.conf ~/xorg.conf-$(date +"%d%m%y")
    fi
    sudo ln -s $PWD/xorg.conf-turing /etc/X11/xorg.conf
fi


if hash X 2>&-; then
    echo "Found X..."
    [ -f '~/.Xresources' ] && rm -i ~/.Xresources
    ln -s $PWD/Xresources ~/.Xresources

    [ -f '~/.xsession' ] && rm -i ~/.xsession
    ln -s $PWD/xsession ~/.xsession
fi

if hash xmonad 2>&-; then
    echo "Found XMonad..."
    if [ -d '~/.xmonad/' ] then
        [ -f '~/.xmonad/xmonad.hs' ] && rm -i .xmonad/xmonad.hs
    else
        mkdir ~/.xmonad/
    fi
    
    ln -s $PWD/xmonad.hs ~/.xmonad/xmonad.hs
    if [ -f $PWD/xmobarrc-`hostname` ]; then
        ln -s $PWD/xmobarrc-`hostname` ~/.xmobarrc
    else
        # Assume the mathom config as the default
        ln -s $PWD/mathom-turing ~/.xmobarrc
    fi
fi


if hash screen 2>&-; then
    echo "Found GNU Screen..."
    [ -f '~/.screenrc' ] && rm -i screenrc
    ln -s $PWD/screenrc ~/.screenrc
    # Makes alt act like meta. Originally for irssi in xterm.. I think.
    [ -f '~/.inputrc' ] && rm -i .inputrc
    ln -s $PWD/inputrc ~/.inputrc
fi
