#!/bin/sh

read -p 'Will overwrite existing config files. Continue? (y/n)'
[ $REPLY != [yY] ] && exit 1

# Linker for my dotfiles
ln -s $PWD/bashrc ~/.bashrc

# Only desktop
if  [ `hostname` = 'turding' ]; then 
    echo "turing: xorg.conf.."
    if [ -f "/etc/X11/xorg.conf" ]; then
        echo "   Note: Moving old /etc/X11/xorg.conf to ~/xorg.conf-$(date +"%d%m%y")"
        sudo mv -i /etc/X11/xorg.conf ~/xorg.conf-$(date +"%d%m%y")
    fi
    sudo ln -s $PWD/xorg.conf-turing /etc/X11/xorg.conf
fi


if hash X 2>&-; then
    echo "Found X..."
    ln -s $PWD/Xresources ~/.Xresources
    ln -s $PWD/xsession ~/.xsession
fi

if hash xmonad 2>&-; then
    echo "Found XMonad..."
    ln -s $PWD/xmonad.hs ~/.xmonad/xmonad.hs
    if [ -f $PWD/xmobarrc-`hostname` ]; then
        ln -s $PWD/xmobarrc-`hostname` ~/.xmobarrc
    fi
fi


if hash screen 2>&-; then
    echo "Found GNU Screen..."
    mv ~/.screenrc /tmp
    ln -s $PWD/screenrc ~/.screenrc
    # Makes alt act like meta. Originally for irssi in xterm.. I think. 
    ln -s $PWD/inputrc ~/.inputrc
fi
